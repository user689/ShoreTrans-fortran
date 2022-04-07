!> @brief
!! handles common error checking (before running the program)
!> @details
!! validates the input parameters, checks for errors, and stops the program if necessary
module st_error_checking

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

        ! slumping parameters
        if (slump%switch .eq. 1) then
            ! check if slope is between -180 and 180
            call assert(slump%slope.ge.-180 .and. slump%slope .le. 180,&
                    'dune slope must be between -180 and 180', 0)
            ! check if slump cap is positive
            call assert(slump%cap.ge.0,'slump cap set to a negative' //&
                                        ' value') !only warning
        end if

    end subroutine check_errors



end module st_error_checking