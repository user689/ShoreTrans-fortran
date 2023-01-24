!> @brief initialization module
!! @details
!! module to set the initial values of variables \n
!! checks if some variables are not initialized and sets them to default values
!! also reads the cross shore profile

module st_initialization

#ifndef STANDALONE
    use constants
    use SharedVariables
#endif
    use st_defaults
    use st_helper
    implicit none

    public :: setup_shoretrans
    private

    contains
    !> @brief initialization subroutine
    !> @details
    !> subroutine to set the initial values of variables \n
    !> checks if some variables are not initialized and sets them to default values
    !> also reads the cross shore profile
    subroutine setup_shoretrans

        implicit none

        integer, allocatable, dimension(:) :: tmp_array

        ! check for errors
        call check_errors()

        ! verbose output
        if (verbose .EQ. -1) then
            ! delete the log file
            call logger(-10, 'Deleting log file') ! delete log file
        end if
        call logger(3, 'logging level set to ' // &
                    num2str(verbose))

        if(xshorefilename.NE.nans) call read_xshore() ! read the cross shore profile
        dx = X(2) - X(1)
        allocate(tmp_array(n_pts))
        ! defaults for the toe/crest values
        ! if not set, then the default values are the
        ! maximum of the profile

        if (eql(toe_crest , nanr) .and.     &
            (toe_crest_index.EQ.nani)) then
            toe_crest = maxval(z)
            tmp_array = 1
            where(z .ge. toe_crest) tmp_array = 0
            toe_crest_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1)+2

            call logger(1, 'none of the toe/crest values were set, '// &
                       'using the maximum of the profile')
            call logger(3, 'setting toe/crest value: ' // &
                       adj(num2str(toe_crest)) // ' at index: ' // &
                       adj(num2str(toe_crest_index)))
        end if

        ! ! if only one option is not set,
        ! then use it to set the other option
        if (toe_crest_index.EQ.nani) then
            tmp_array = 1
            where(z .ge. toe_crest) tmp_array = 0
            toe_crest_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1)+1
            call logger(3, 'setting toe/crest index: ' // &
                       adj(num2str(toe_crest_index)))
        else if (eql(toe_crest,nanr)) then
            toe_crest = z(toe_crest_index)
            call logger(3, 'setting toe/crest value: ' // &
                       adj(num2str(toe_crest)))
        end if

        ! Depth of closure (1 and 2)
        ! set up doc_index (upper depth of closure)
        if (eql(doc , nanr)) then
            doc = minval(z)
            call logger(1, 'Depth of closure not set, using doc=: ' &
                       // adj(num2str(doc)))
        else
            call logger(3, 'Depth of closure set to: ' // &
                       adj(num2str(doc)))
        end if
        tmp_array = 1
        where(z .ge. doc) tmp_array = 0
        doc_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1) + 2
        if(doc_index.gt.size(z)) then
            call logger(1, 'DOC level not reached using min(z)')
            doc_index = size(z)
        end if

        call logger(3, 'Depth of closure index set to: ' // &
                   adj(num2str(doc_index)))

        if (eql(doc2 , nanr)) then
            doc2 = doc - 0.1d0
            call logger(1, 'Depth of closure 2 not set, using doc2= ' &
                       // adj(num2str(doc2)))
        else
            call logger(3, 'Depth of closure 2 set to: ' // &
                       adj(num2str(doc2)))
        end if
        tmp_array = 1
        where(z .ge. doc2) tmp_array = 0
        doc2_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1) + 2
        if(doc2_index.gt.size(z)) then
            call logger(1, 'DOC2 index not reached, using min(z)')
            doc2_index = size(z)
        end if
        call logger(3, 'Depth of closure 2 index set to: ' // &
                   adj(num2str(doc2_index)))
        ! sediment flux (volume), default is 0.0
        if (.not. eql(dv_input, 0.d0)) then ! volume change
            call logger(3, 'Sediment (budget/deficit) = ' // &
                       adj(num2str(dv_input)))
        end if

        if (rollover .EQ. 0) then
            ! slump (erosion of dunes) default is True, i.e. slump
            call logger(3, 'slumping set to: ' // &
                    adj(num2str(slump%switch)) // ' with slope: ' // &
                    adj(num2str(slump%slope)) // ' and cap: ' // &
                    adj(num2str(slump%cap)))
        else
            call logger(3, 'Rollover set to: '// adj(num2str(rollover)) // &
            ' with back slope angle (deg): ' // adj(num2str(roll_backSlope)) )

        end if

    end subroutine setup_shoretrans

    subroutine initialize_transect(x,z)
        real(kind=8), dimension(:), allocatable :: x, z
        real(kind=8):: dx_tmp ! current dx
        integer i
        dx_tmp = x_tmp(2) - x_tmp(1) ! assume uniform spacing
        ! check if we need to interpolate the cross-shore profile
        if (dx.le.0) then ! dx not set
            dx = dx_tmp
            allocate(x(n_pts), z(n_pts), z_rock(n_pts))
            x = x_tmp
            z = z_tmp
            z_rock = rock_tmp
        else
            ! dx should always be +ve
            n_pts = int((x_tmp(n_pts) - x_tmp(1)) / dx) +1
            allocate(x(n_pts), z(n_pts), z_rock(n_pts))
            do i=1,n_pts
                x(i) = x_tmp(1) + dx * (i-1)
            end do
            ! interpolate new profile
            z = interp1_vec(x_tmp, x, z_tmp)
            if (rock .ne. 1) then
                z_rock = z - 100.d0
            else
                z_rock = interp1_vec(x_tmp, x, rock_tmp)
            end if
        end if
        deallocate(x_tmp,z_tmp,rock_tmp)
    end subroutine initialize_transect

    subroutine read_xshore()
        implicit none
        integer :: n_tmp, ios, i
        character(charlen) :: line
    ! read cross-shore profile
    !> cross-shore filename
        xshorefilename = adj(dir_name)  // '/inputs/' // xshorefilename
        call logger(3, 'Reading cross-shore profile from ' // &
                   adj(xshorefilename))

        open(unit=fid, file=adj(xshorefilename), status='old'&
             , iostat=ios)
        ! if ios is not 0, an error occured
        if ( ios /= 0 ) then
            call logger(0, 'Error opening file ' // &
                       adj(xshorefilename))
            close(fid)
            stop
        end if
        ! skip comment lines
        do
            read(fid, '(A)', iostat=ios) line
            if (index(line, '!') == 0) exit ! skip comment lines
        end do

        ! we expect the first line to be the number of points
        read(line, *, iostat=ios) n_tmp
        allocate(x_tmp(n_tmp), z_tmp(n_tmp), rock_tmp(n_tmp))

        ! read the rest of the file
        if (rock .ne. 1) then ! no rock profile
            do i = 1, n_tmp
                read(fid, *, iostat=ios) x_tmp(i), z_tmp(i)
            end do
            rock_tmp = z_tmp - 100.
        else
            do i = 1, n_tmp
                read(fid, *, iostat=ios) x_tmp(i), z_tmp(i), rock_tmp(i)
            end do
        end if
        close(fid)
        n_pts = n_tmp
        call initialize_transect(x,z)
    end subroutine read_xshore

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

        call assert(rollover.le.2.and.rollover.ge.0, 'rollover must be 0,1 or 2' , 0) 
    end subroutine check_errors

end module st_initialization