!> @brief applies slump to cliff and dunes to avoid vertical sand cliff
!! @details
!! erodes part of the cliff/dune to avoid vertical sand cliff \n
!! can be turned off by setting slump to 0
module st_slump
#ifndef STANDALONE
    use Constants
    use SharedVariables
#endif
    use st_defaults
    use st_helper

    implicit none

    public :: slump_profile


    private
    contains

    !> @brief slumps cliff/dune
    !! @param[inout] z_temp profile to be slumped
    !! @param[in] cap height above which no slump occurs
    !! @param[in] slope slope of the profile to slump to
    !! @return the slumped profile
    subroutine slump_profile(z_temp, XI)
        real(kind=8), dimension(:), intent(inout) :: z_temp
        integer,  intent(in) :: XI
        character(len=charlen) :: msg
        real(kind=8), allocatable, dimension(:) :: dune_angles(:), &
                                             dx1(:), dz1(:)
        integer, allocatable, dimension(:) :: dune_indices(:), &
                                                dune_n(:)
        integer :: i, s, ind1, ind2, start_index
        integer :: pts, dune_offset
        real(kind=8) :: dune_angle
        logical :: isprint ! print message only once

        if (slump%switch == 0) return ! no slumping
        s= size(z_temp)

        allocate(dune_angles(s),dx1(s),dz1(s)) ! temporary arrays
        allocate(dune_indices(s))
        ! find sand cliff location
        dx1 = dx
        dx1(1) = 0.d0
        dz1 = 0.d0
        dz1(2:s) = z_temp(1:s-1) - z_temp(2:s)
        dune_angles = atan2(dz1, dx1) *180/pi! angles of dune
        dune_indices = 0

        where((dune_angles > slump%slope) .and. (z_temp < slump%cap))
            dune_indices(:) = 1
        end where
        allocate(dune_n(sum(dune_indices) + 1))
        dune_n = pack([(i,i=1,s)],dune_indices == 1)
        isprint = .TRUE.
        do i=1,sum(dune_indices) ! loop over dunes
            pts = 0
            ind2 = dune_n(i)
            ind1 = ind2 - 1
            dune_angle = dune_angles(ind2)
            dune_offset = 0
            do while (dune_angle > slump%slope)
                pts = pts + 1
                dune_offset = ind1 - pts
                if (dune_offset .LE. 0) then
                    ! can't go back any further
                    if(isprint) then
                        write(msg, '(A,I0,A)') '[xi=', XI,'] Ran out of' //&
                        'profile to slump dune. Try increasing the onshore length'
                        call logger (1, adj(msg))
                        isprint = .FALSE.
                    end if
                    dune_offset = 1
                    pts = ind1 - 1
                    exit
                end if
                dune_angle = atan2( (z_temp(dune_offset) - z_temp(ind2)), &
                                 (x(ind2) - x(dune_offset)) ) * 180/pi
                ! dune_angle = atan2d(z_temp(dune_offset) - z_temp(ind2),&
                                    ! x(ind2) - x(dune_offset))
            end do

            ! interpolate section of profile to be slopped
            start_index = ind1 - pts
            z_temp(start_index:ind2) = interp1(x(start_index), x(ind2),&
                                                z_temp(start_index),&
                                                z_temp(ind2), &
                                                x(start_index:ind2))


            if(rock.eq.1) then
                where (z_temp .le. z_rock) z_temp = z_rock
            end if

        end do
    end subroutine slump_profile

end  module st_slump