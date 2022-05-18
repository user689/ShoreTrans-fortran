!> @brief main logic of the program
!! @details contains the main functionalities to translate \n
!! the profile and calculate the error of the translation \n
!! only main loop is used outside of this module.
module st_translate_profile
#ifndef STANDALONE
    use constants
    use SharedVariables
#endif
    use st_defaults
    use st_helper
    use st_slump, only: slump_profile


    implicit none
    INTEGER :: XI ! profile translation
    public :: translate_profile, XI



    private
    contains

    !> @brief main loop of the program
    !! @details main optimization loop of the program \n
    !! uses the ridders method to find the minimum of the function \n
    !! convergence should be reached in a max of O(sqrt(n)) steps. \n
    !! where n is the number of points in the profile \n
    !! @return the new profile
    subroutine translate_profile()
        implicit none
        real(kind=8), dimension(n_pts) :: z_low, z_upp, z_m
        integer :: i, xm, x_upp, x_low, xi_est
        real(kind=8) :: dv_m, dv_upp, dv_low, dv_est, x_curr, f_sq
        character(len=charlen) :: msg

        allocate(z_final(n_pts)) ! allocate space for the final profile
        dv_est = nanr;
        ! upper and lower bounds
        x_upp = min(n_pts - doc_index -1, doc_index - 2 - toe_crest_index)
        call get_profile(z_final, x_upp)
        dv_upp = dv
        z_upp = z_final
        x_low = - min( toe_crest_index-1, n_pts-doc_index-1)
        call get_profile(z_final, x_low)
        dv_low = dv
        z_low = z_final
        xi_est = bruun_estimate() ! bruun estimate
        if(xi_est.GT.x_upp.OR.xi_est.LT.x_low) then
            call logger(3, 'XIest changed from:' // adj(num2str(xi_est)))
            xi_est = nint(0.5d0 * (x_upp + x_low))
            call logger(3, 'to: ' // adj(num2str(xi_est)) )
        end if
        if (sign(1.d0, dv_upp * dv_low) .lt. 0.d0) then
            call logger(3,'I |   xlow   |   xupp   |   xest   |    ' //&
                          'dvlow |    dvupp |    dv')
            do i=1,max_iter
                xm = nint(0.5d0 * (x_upp + x_low)) ! update estimate
                if (i == 1) xm =xi_est  ! better initial guess
                call get_profile(z_final, xm)
                dv_m = dv ! get the function value at the new estimate
                z_m = z_final
                f_sq = sqrt(dv_m * dv_m - (dv_upp * dv_low))
                if (eql(f_sq, 0.d0)) then
                    xi_est = xm
                    dv_est = dv_m
                    exit ! converged to exact minimum
                end if
                ! apply the false position method
                x_curr = (xm - x_low) * dv_m / f_sq
                x_curr= xm + sign(1.d0, dv_low - dv_upp) * x_curr
                ! round into the new interval (faster convergence)
                xi_est = NINT(x_curr + 0.5 * SIGN(1.d0, xm - x_curr))
                call get_profile(z_final, xi_est) ! apply the new estimate
                dv_est = dv
                write (msg, '(I2,A,I8,A,I8,A,I8,A,F8.2,A,F8.2,A,F8.2)')&
                & i, ' | ', x_low, ' | ', x_upp, ' | ', xi_est, ' | ', &
                & dv_low, ' | ', dv_upp, ' | ', dv_est
                call logger(3, adj(msg))

                ! convergence checks
                if (eql(abs(dv_est), 0.d0)) then
                    exit ! converged to minimum
                else if (abs(x_low - x_upp) .lt. 2) then
                    if(abs(dv_low) .gt. abs(dv_upp)) then
                        xi_est = x_upp
                        dv_est = dv_upp
                        z_final = z_upp
                    else
                        xi_est = x_low
                        dv_est = dv_low
                        z_final = z_low
                    end if
                    exit ! found solution
                end if

                ! update the bounds
                if (sign(1.d0, dv_m * dv_est) .lt. 0) then
                   IF(xm .LT. xi_est) THEN
                    x_low = xm; dv_low = dv_m
                    z_low = z_m; x_upp = xi_est
                    dv_upp = dv_est; z_upp = z_final
                   ELSE
                    x_low = xi_est; dv_low = dv_est
                    z_low = z_final; x_upp = xm
                    dv_upp = dv_m; z_upp = z_m
                   END IF
                else if (sign(1.d0, dv_low * dv_est) .lt. 0) then
                    x_upp = xi_est
                    dv_upp = dv_est
                    z_low = z_final
                else if (sign(1.d0, dv_upp * dv_est) .lt. 0) then
                    x_low = xi_est
                    dv_low = dv_est
                    z_upp = z_final
                else
                    ! this should never happen
                    call logger(0, 'Unkown error in main_loop (ST_TRANSLATE_PROFILE)')
                end if
            end do
            if (i .eq. max_iter) then
                call logger(1, 'Maximum number of iterations reached')
                call logger(1, 'Solution may not be the real minimum')
            end if
        else if (eql(abs(dv_upp), 0.d0)) then
            ! update upper bound
            xi_est = x_upp
            dv_est = dv_upp
            z_final = z_upp
        else if (eql(abs(dv_low), 0.d0)) then
            ! update lower bound
            xi_est = x_low
            dv_est = dv_low
            z_final = z_low
        else
            ! no solution can be found
            ! TODO: think how can we make this work for all situations
            ! e.g: no DoC (lagoons), incomplete profiles
            call logger(0, 'No solution can be found')
            call logger(0, 'Please check the profile')
            STOP
        end if
        xi = xi_est
        dv = dv_est
        call logger(2, 'Final xi: ' // adj(num2str(xi)))
        call logger(2, 'Final dv (error): ' // adj(num2str(dv)))
    end subroutine translate_profile


    !> @brief Estimate the shoreline recession
    !! @details Estimate the shoreline recession using the
    !! Bruun method xi = - ds * (W/h)
    !! where ds is the sea level rise and W and h are the
    !! width and height of the active profile
    !! it is used as an initial guess for the main loop
    !! @return the estimate of the shoreline recession/progression
    function bruun_estimate() result(xi_est)
        integer :: xi_est
        real(kind=8) :: x_est
        h = toe_crest - doc
        w = x(doc_index) - x(toe_crest_index)
        ! xi is the one calculate from bruun rule
        ! plus any additional (sources/sinks)
        x_est = (-ds * w / h) + (dv_input / h)! calculate the estimate
        xi_est = nint(x_est / dx) ! round to nearest integer
        ! catch any xi values that are too large
        if (- xi_est .ge. toe_crest_index) then
            xi_est = 1 - toe_crest_index
            call logger(1, "xi is too large, setting to"// &
                num2str(1 - toe_crest_index ))
        end if
        ! log calculated values
        call logger (3, 'ds = '//adj(num2str(ds)))
        call logger (3, 'h = '//adj(num2str(h)))
        call logger (3, 'w = '//adj(num2str(w)))
        call logger (-1, 'Initial estimate (bruun) xi = ' &
        // adj(num2str(xi_est * dx)) // ' (m)(' // adj(num2str(xi_est)) &
        // ')')
    end function bruun_estimate

    !> @brief smooth the profile at the base of the active
    !! zone
    !! @details an interpolation is made from the
    !! base of the active profile (XD1, ZD1) to a point onshore/offshore
    !! of the profile
    !! a minimum interpolation distance of 10% the width is assumed
    !! @param[in] xi_tmp the shoreline recession/progression
    !! @param[inout] z_out the profile to be smoothed
    !! @return the smoothed profile
    subroutine smooth_profile(z_out, xi_tmp)
        integer, intent(in) :: xi_tmp
        real(kind=8), allocatable, intent(inout) :: z_out(:)
        integer :: st_min, start_ind, end_ind ! smoothing profile
        ! smoothing the profile
        if (xi_tmp .le. 0) then
            st_min = nint(w * 0.1) ! minimum smoothing window
            start_ind = min((doc_index -1 + xi_tmp), &
                            (doc_index - st_min)) ! min smoothing window
            end_ind = doc_index
        else ! xi > 0
            start_ind = doc_index - 1
            end_ind = doc_index + xi_tmp
        end if
        z_out(start_ind+1:end_ind) =  interp1(x(start_ind), x(end_ind),&
                                    z_out(start_ind), z(end_ind), &
                                         x(start_ind+1:end_ind))
    end subroutine smooth_profile

    !> @brief reset the elevation between the toe_crest and
    !! the end of the profile
    !! @details if rollover is off, prevent elevation increase between
    !! toe_crest and the end of the profile
    !! this allows for accretion between the wall and the crest
    !! also maintains the crest of the profile in case the profile
    !! is marching offshore
    subroutine reset_elevation(z_tmp, xi_tmp)
        real(kind=8), allocatable, intent(inout) :: z_tmp(:)
        integer, intent(in) :: xi_tmp ! current xi value
        where(z_tmp .gt. z .and. x .le. x(toe_crest_index)) z_tmp = z

        ! As the profile marches offshore,
        ! this maintains the crest of the barrier
        if(xi_tmp > 0) then
            z_tmp(toe_crest_index +1 : toe_crest_index+xi_tmp) = &
                                                toe_crest + ds
        end if
    end subroutine reset_elevation

    !> @brief translate the profile
    !! @details translate the profile by xi
    !! then calculate the difference in volume between the
    !! original profile and the translated profile
    !! @param[in] xi_tmp the shoreline recession/progression
    !! @param[inout] z_out the profile to be translated
    !! @return the translated profile
    subroutine get_profile(z1, xi_tmp)
        implicit none
        real(kind=8), allocatable, intent(out) :: z1(:) ! translated profile
        integer, intent(in) :: xi_tmp ! index of current profile
        integer, dimension(:), allocatable :: active_ind ! active indices
        integer :: active_size, i ! active size and loop index
        real(kind=8) :: v0, v1 ! volumes of current and translated profiles

        active_size = doc_index - 1 - toe_crest_index - xi_tmp
        if (active_size .le. 0) then
            call logger(0, 'get_profile: active_size <= 0'// &
                          ' can not translate profile')
            stop
        end if

        allocate(active_ind(active_size)) ! active zone
        allocate(z1(n_pts)) ! same dimension as the two arrays

        ! raise the profile by SLR
        z1 = z
        z1(toe_crest_index:doc_index-1) = z1(toe_crest_index:doc_index-1)&
                                         + ds
        ! active zone is the zone that is translated
        active_ind = (/(i, i=(toe_crest_index+xi_tmp),(doc_index-1))/)
        z1(active_ind) = z1(active_ind - xi_tmp) ! move profile to the right
        call reset_elevation(z1, xi_tmp) ! reset elevation at the end of the profile
        call smooth_profile(z1, xi_tmp) ! interpolate at the end of the profile
        ! slump profile (erosion of dunes)
        call slump_profile(z1)
        ! calculate volume difference
        v0 = trapz(x(1:doc2_index), z(1:doc2_index) - doc2)
        v1 = trapz(x(1:doc2_index), z1(1:doc2_index) - doc2)
        dv = v1 - v0 - dv_input ! volume difference (error)
    end subroutine get_profile
end module st_translate_profile