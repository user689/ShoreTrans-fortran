module st_error_checking
#ifndef STANDALONE
   use Constants
   use SharedVariables
#endif
   use st_defaults
   use st_helper

   implicit none
   public :: check_errors
   private
contains


   subroutine check_errors
      ! check for switches (should be 0 or 1)
      call assert(rock .eq. 0 .or. rock .eq. 1, &
         "rock must be 0 or 1", 0) ! rock switch
      call assert((slump%switch .eq. 0) .or. (slump%switch .eq. 1), &
         'slump must be 0 or 1', 0) ! slump switch

      call assert((wall%switch .eq. 0) .or. (wall%switch .eq. 1), &
         'wall switch must be 0 or 1', 0) ! wall switch

      ! slumping parameters
      if (slump%switch .eq. 1) then
         ! check if slope is between -180 and 180
         call assert(slump%slope.ge.-180 .and. slump%slope .le. 180,&
            'dune slope must be between -180 and 180', 0)
         ! check if slump cap is positive
         call assert(slump%cap.ge.0,'slump cap set to a negative' //&
            ' value') !only warning
      end if

      ! wall parameters
      if (wall%switch .eq. 1) then
         call assert(.not. eql(wall%x, nanr) .or. .not. eql(wall%level,nanr), &
            'Wall switch is turned on' //&
            ' but none of wall_x or wall_level are set.', 0)
      end if

      call assert(rollover.le.2.and.rollover.ge.0, 'rollover must be 0,1 or 2' , 0)
   end subroutine check_errors


   !> @brief assert that a condition is true
   !! param[in] var variable to check
   !! param[in] msg message to print if var is false
   !! param[in] priority priority of the message (0: error, 1: warning, 2: info, 3: extra)
   subroutine assert(var, msg, priority)
      logical, intent(in) :: var
      integer, intent(in), optional :: priority
      character(len=*), intent(in) :: msg
      if (present(priority)) then
         if (.not. var) then
            call logger(priority, adj(msg))
            if(priority .eq. 0) stop ! stop if error
         end if
      else
         if (.not. var) then
            call logger(1, adj(msg)) ! defaults to warning
         end if
      end if
   end subroutine assert
end module st_error_checking