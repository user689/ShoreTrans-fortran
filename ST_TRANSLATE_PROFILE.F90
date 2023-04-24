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
    use st_slump
    use st_wall_volume


    implicit none
    integer :: xi
    public :: translate_profile, xi



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
        integer :: i,j, xm, x_upp, x_low, xi_est, x_prev
        real(kind=8) :: dv_m, dv_upp, dv_low, dv_est, x_curr, f_sq, dv_prev
        character(len=charlen) :: msg

        allocate(z_final(n_pts)) ! allocate space for the final profile
        x_prev = nani
        ! upper and lower bounds
        x_upp = min(n_pts - doc_index -1, doc_index - 2 - toe_crest_index)
        call get_profile(z_final, x_upp)
        dv_upp = dv
        z_upp = z_final
        call get_profile(z_final, x_upp -1)
        write(msg, '(E8.1)') dv_upp/ (dv_upp - dv)
        call logger(3, adj(msg))

        x_low = - min( toe_crest_index-1, n_pts-doc_index-1)

        if (wall%switch.eq.1) x_low = max(x_low, 1 - wall%index)
        call get_profile(z_final, x_low)
        dv_low = dv
        z_low = z_final
        xi_est = bruun_estimate() ! bruun estimate
        if (xi_est.lt.x_low.or.xi_est.gt.x_upp) then ! check in case bruun estimate is outside of interval
            xi_est = nint(0.5d0 * (x_upp + x_low))
        end if
        write(msg, '(A,I8,A,E8.1)') 'xupp = ', x_upp, ' dv_upp =', dv_upp
        call logger(3, adj(msg))
        write(msg, '(A,I8,A,E8.1)') 'xlow = ', x_low, ' dv_low =', dv_low
        call logger(3, adj(msg))

        if (sign(1.d0, dv_upp * dv_low) .lt. 0.d0) then
            call logger(3,'I |   xlow   |   xupp   |   xest   |    ' //&
                          'dvlow |   dvupp  |    dv    |')
            outer: do i=1,max_iter
                xm = nint(0.5d0 * (x_upp + x_low)) ! update estimate
                if (i == 1) xm =xi_est  ! better initial guess
                call get_profile(z_final, xm)
                dv_m = dv ! get the function value at the new estimate
                z_m = z_final
                if (x_prev == nani) then
                    x_prev = xm
                    dv_prev = dv_m
                end if

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
                write (msg, '(I2,A,I8,A,I8,A,I8,A,1PE8.1,A,E8.1,A,E8.1,A)') &
                i, ' | ', x_low, ' | ', x_upp, ' | ', xi_est, ' | ', &
                dv_low, ' | ', dv_upp, ' | ', dv_est, ' |'
                call logger(3, adj(msg))


                if(abs(dv_prev) .lt. abs(dv_est)) then
                    ! function is diverging !!
                    do j=1, max_iter
                        if (abs(xi_est - x_prev) .lt. 2) then
                            xi_est = x_prev
                            exit outer
                        end if
                        xi_est = nint((xi_est + x_prev)*0.5d0)
                        call get_profile(z_final, xi_est)
                        dv_est = dv
                        if(abs(dv_est) .lt. abs(dv_prev)) exit
                    end do                      
                end if
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

                x_prev = xi_est
                dv_prev = dv_est
            end do outer
            if (i .eq. max_iter) then
                call logger(1, 'Maximum number of iterations reached')
                call logger(1, 'Solution may not be the real minimum')
            end if
        else if (eql(dv_upp, 0.d0)) then
            ! update upper bound
            xi_est = x_upp
            dv_est = dv_upp
            z_final = z_upp
        else if (eql(dv_low, 0.d0)) then
            ! update lower bound
            xi_est = x_low
            dv_est = dv_low
            z_final = z_low
        else
            ! no solution can be found
            ! TODO: think how can we make this work for all situations
            ! e.g: no DoC (lagoons), incomplete profiles
            call logger(0, 'No solution can be found. Please check the profile')
            STOP
        end if
        xi = xi_est
        call get_profile(z_final, xi_est)
        call logger(2, 'Final xi: ' // adj(num2str(xi *dx)) // ' m ('// adj(num2str(xi)) //')')
        call logger(2, 'Final dv (error): ' // adj(num2str(dv)))
    end subroutine translate_profile


    ! !> @brief use the bisection method to reevaluate the estimate
    ! !! @details fall back for when the ridders method diverges 
    ! !! when there are multiple roots.
    ! !! added 19/04/2023
    ! subroutine bisection(xa, xb, dva, dvb, rootx, rootdv)
    !     real(kind=8), intent(in) :: xa, xb, dva, dvb ! [a,b] interval
    !     real(kind=8), intent(out) :: rootx, rootdv
    !     real(kind=8) :: dvb, xc,dvc
    !     integer :: i
    !     call logger(3,'I |   xa   |   xb   |  dva |   dvb  |')
    !     do i=1:max_iter
    !         write (msg, '(I2,A,I8,A,I8,A,1PE8.1,A,E8.1,A)') &
    !         i, ' | ', xa, ' | ', xb, ' | ', dva, ' | ', dva, ' | '
    !         if (sign(1.d0, dvb * dva) .lt. 0.d0) then
    !             ! solution can be found between the two values
    !             xc = nint((xa + xb)/2)
    !             call get_profile(z_final, xc)
    !             dvc = dv
    !             if(eql(dvc,0.d0)) then
    !                 rootx = xc
    !                 rootdv = dvc
    !                 return
    !             elseif(sign(1.d0, dva * dvc) .lt. 0.d0)
    !                 xb = xc
    !                 dvb = dvc
    !             else
    !                 if(abs(dvc).ge.abs(dva)) then
    !                     rootx = xa
    !                     rootdv = dva
    !                     return
    !                 else
    !                     xa = xc
    !                     dva = dvc
    !                 end if
    !             end if
    !         else
    !             xb = nint((xb+xa)/2)
    !             call get_profile(z_final, xb)
    !             dvb = dv
    !         end if

    !         if (abs(xa - xb) .lt. 2) then
    !             if(abs(dvb) .gt. abs(dva)) then
    !                 rootx = xa
    !                 rootdv = dva
    !             else
    !                 rootx = xb
    !                 rootdv = dvb
    !             end if
    !             return
    !         end if
    !         end do
    ! end subroutine bisection

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
        call logger (3, 'Initial estimate (bruun) xi = ' &
        // adj(num2str(xi_est * dx)) // ' m (' // adj(num2str(xi_est)) &
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
    subroutine smooth_profile(z_out, xi_tmp, z_nowall)
        integer, intent(in) :: xi_tmp
        real(kind=8), dimension(n_pts), intent(inout) :: z_out, z_nowall
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

        ! additional checks to remove errors
        if (start_ind .le. 0) then 
            start_ind =0;
            call logger(1, 'The smoothing points are beyond the edge of the profile. '// &
            'Begin smoothing from the start of the profile. ')

        else if(end_ind .ge. n_pts) then
            end_ind = n_pts
            call logger(1, 'The smoothing points are beyond the edge of the profile. '// &
            'End smoothing at the end of the profile.')
        end if
        z_out(start_ind+1:end_ind) =  interp1(x(start_ind), x(end_ind),&
                                    z_out(start_ind), z(end_ind), &
                                         x(start_ind+1:end_ind))
        z_nowall(start_ind+1:end_ind) = z_out(start_ind+1:end_ind)

    end subroutine smooth_profile

    !> @brief reset the elevation between the toe_crest and
    !! the end of the profile
    !! @details if rollover is off, prevent elevation increase between
    !! toe_crest and the end of the profile
    !! this allows for accretion between the wall and the crest
    !! also maintains the crest of the profile in case the profile
    !! is marching offshore
    subroutine reset_elevation(z_tmp, xi_tmp)
        real(kind=8), dimension(n_pts), intent(inout) :: z_tmp
        integer, intent(in) :: xi_tmp ! current xi value
        if (rollover .eq. 0) then
            where(z_tmp .gt. z .and. x .le. x(toe_crest_index)) z_tmp = z
        else if(rollover .eq. 2) then
            where(z_tmp .gt. z .and. x .lt. x(toe_crest_index)) z_tmp = toe_crest
        end if 

        ! As the profile marches offshore,
        ! this maintains the crest of the barrier
        if(xi_tmp > 0) then
            z_tmp(toe_crest_index +1 : toe_crest_index+xi_tmp) = &
                                                toe_crest + ds
        end if
    end subroutine reset_elevation

    subroutine rollover_profile(z_tmp, xi_tmp)
        real(kind=8), dimension(n_pts) , intent(inout) :: z_tmp ! current z
        real(kind=8), dimension(n_pts) :: z_noWash
        integer, intent(in) :: xi_tmp ! current xi value
        real(kind=8) :: z_back, z_step, z_step_n
        integer :: ind_wash

        z_noWash = z_tmp
        ind_wash = toe_crest_index + xi_tmp
        z_back = z_tmp(ind_wash)
        z_step = dx * tan(pi/180 * roll_backSlope )
        z_step_n = z_step
        do
            ind_wash = ind_wash - 1
            if(ind_wash .le. 0) exit ! check for edge cases
            z_tmp(ind_wash) = z_back - z_step_n
            z_step_n = z_step_n + z_step
            ! if overwash has dipped below existing profile, bring it back up
            if (z_tmp(ind_wash) .le. z_noWash(ind_wash)) then
                z_tmp(ind_wash) = z_noWash(ind_wash)
                exit
            end if
    
        end do
        
    end subroutine rollover_profile 

    ! raise profile that is below the rock profile
    subroutine raise_rock(z_tmp)
        real(kind=8), dimension(n_pts), intent(inout) :: z_tmp
        if(rock.eq. 0) return
        where (z_tmp .le. z_rock) z_tmp = z_rock
    end subroutine raise_rock

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
        z_nowall = z1 ! for wall calculation
        ! active zone is the zone that is translated
        active_ind = (/(i, i=(toe_crest_index+xi_tmp),(doc_index-1))/)
        z1(active_ind) = z1(active_ind - xi_tmp) ! move profile to the right

        if (wall%switch.eq.1.and. .not.wall%overwash) then
            z_nowall(active_ind) = z1(active_ind)
            where(x.le.x(wall%index))  z1 = z
        end if
        call raise_rock(z1) ! reset profile above rock profile
        call reset_elevation(z1, xi_tmp) ! reset elevation at the end of the profile
        call smooth_profile(z1, xi_tmp, z_nowall) ! interpolate at the end of the profile
        ! slump profile (erosion of dunes)
        if (rollover .eq. 0) then
            call slump_profile(z1, xi_tmp)
        else if (rollover .gt. 0) then ! 1 or 2
            call rollover_profile(z1, xi_tmp)
        end if
        call redistribute_volume(z1, z_nowall, xi_tmp)
        call raise_rock(z1) ! last check for rock
        ! calculate volume difference
        v0 = trapz(x(1:doc2_index), z0_rock(1:doc2_index) - doc2)
        v1 = trapz(x(1:doc2_index), z1(1:doc2_index) - doc2)
        dv = v1 - v0 - dv_input ! volume difference (error)
    end subroutine get_profile
end module st_translate_profile