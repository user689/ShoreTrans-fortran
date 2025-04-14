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
   real(kind=8), allocatable :: dv_tmp(:)
   public :: translate_profile, xi



   private
contains

   subroutine translate_profile()

      integer :: i, x_upp, x_low, xi_est, step, x_tmp
      real(kind=8) :: grad_f, dv_est, dv_plus, dv_minus
      character(len=charlen) :: msg
      allocate(z_final(n_pts))
      ! get the upper and lower bounds

      if(xi_test.ne.nani) then
         call logger(2, 'Using xi=' // adj(num2str(xi_test)) // ' grid points')
         xi = xi_test
         call get_profile(z_final, xi)
         call logger(2, 'Final volume error (dv): ' // adj(num2str(dv)) // ' m3')
         return
      end if
      x_upp = min(n_pts - doc_index -1, doc_index - 2 - toe_crest_index)
      x_low = - min( toe_crest_index-1, n_pts-doc_index-1)
      if(x_low .gt. x_upp) then
         call logger(4, 'Switching bounds as x_low > x_upp')
         x_tmp = x_low
         x_low = x_upp
         x_upp = x_tmp
      end if
      write(msg, '(A,I8,A,I8,A)') 'Bounds: x_upp = ', x_upp, ' grid points, x_low = ', x_low, ' grid points'
      call logger(3, adj(msg))
      if (wall%switch.eq.1) x_low = max(x_low, 1 - wall%index)
      allocate(dv_tmp(x_low:x_upp))
      dv_tmp = nanr
      xi_est = bruun_estimate() ! calculate the bruun rule estimate

      if (xi_est.lt.min(x_low, x_upp).or.xi_est.gt.max(x_upp, x_low)) then
         ! check in case bruun estimate is outside of interval
         call logger(3, 'Xi_est outside interval bounds, resetting to midpoint')
         xi_est = nint(0.5d0 * (x_upp + x_low))
      end if
      do i=1,max_iter
         write(msg, '(A,I8,A,I8,A)') 'Iteration ', i, ': xi_est = ', xi_est, ' grid points'
         call logger(4, adj(msg))
         if(xi_est .lt. min(x_low, x_upp) ) then
         xi_est = min(x_low, x_upp) - sign(2, min(x_low, x_upp));
         write(msg, '(A,I8,A)') 'Adjusted xi_est (lower bound): ', xi_est, ' grid points'
         call logger(4, adj(msg))
         else if(xi_est .ge. max(x_upp,x_low)) then
         xi_est = max(x_low, x_upp) -sign(2, max(x_low, x_upp));
         write(msg, '(A,I8,A)') 'Adjusted xi_est (upper bound): ', xi_est, ' grid points'
         call logger(4, adj(msg))
         end if

         if (abs(dv_tmp(xi_est) - nanr) > 1.0d-8) then ! we already have a value xi_est
         ! Find the minimum value among already calculated points
         xi_est = minloc(abs(dv_tmp), 1, abs(dv_tmp - nanr) > 1.0d-8) + x_low - 1
         call logger(4, 'Found existing minimum volume error, exiting iteration')
         exit
         end if

         dv_est = evaluate_f(xi_est)
         if (eql(dv_est,0.d0)) then
            call logger(4, 'Current volume error is zero, solution found')
            exit
         endif

         dv_plus = evaluate_f(xi_est +1)
         if (eql(dv_plus,0.d0)) then
            xi_est = xi_est +1
            call logger(4, 'Found zero volume error at xi_est+1, solution found')
            exit
         end if

         dv_minus = evaluate_f(xi_est -1)
         if (eql(dv_minus,0.d0)) then
            xi_est = xi_est -1
            call logger(4, 'Found zero volume error at xi_est-1, solution found')
            exit
         end if

         ! calculate the gradient at this point
         grad_f = (dv_plus - dv_minus) /2.d0
         write(msg, '(A,E12.5,A)') 'Volume error gradient = ', grad_f, ' m3/grid point'
         call logger(4, adj(msg))
         if (eql(grad_f,0.d0)) then
            call logger(4, 'Zero gradient detected, no further improvement possible')
            exit
         endif

         ! calculate the new estimate
         step = nint(dv_est/grad_f)
         if (step.eq.0) then
            call logger(4, 'Step size is zero, no further improvement possible')
            exit
         endif
         xi_est = xi_est - step
      end do
      xi =xi_est
      call get_profile(z_final, xi_est)
      write(msg, '(A,F10.4,A,I8,A)') 'Final xi: ', xi*dx, ' m (', xi, ' grid points)'
      call logger(2, adj(msg))
      call logger(2, 'Final volume error (dv): ' // adj(num2str(dv)) // ' m3')
   end subroutine translate_profile


   function evaluate_f(xi_est)
      integer, intent(in) :: xi_est
      real(kind=8) :: evaluate_f
      character(len=charlen) :: msg
      if (eql( dv_tmp(xi_est), nanr)) then
         call get_profile(z_final, xi_est)
         dv_tmp(xi_est) = dv
         evaluate_f = dv
      else
         evaluate_f = dv_tmp(xi_est)
      end if
      write(msg, *) 'xi = ', xi_est, 'error = ', evaluate_f
      call logger(3, adj(msg) )
   end function evaluate_f

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
