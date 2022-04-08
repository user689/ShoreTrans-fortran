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

        allocate(tmp_array(n_pts))

        ! defaults for the toe/crest values
        ! if not set, then the default values are the
        ! maximum of the profile

        if (toe_crest.EQ.nanr .and.     &
            (toe_crest_index.EQ.nani)) then
            toe_crest = maxval(z)
            toe_crest_index =  maxloc(z, 1)
            call logger(1, 'none of the toe/crest values were set, '// &
                       'using the maximum of the profile')
            call logger(3, 'setting toe/crest value: ' // &
                       adj(num2str(toe_crest)) // ' at index: ' // &
                       adj(num2str(toe_crest_index)))
        end if

        ! ! if only one option is not set,
        ! then use it to set the other option
        print *, toe_crest
        if (toe_crest_index.EQ.nani) then
            tmp_array = 1
            where(z .ge. toe_crest) tmp_array = 0
            toe_crest_index = minloc(tmp_array, 1, back=.true.)
            call logger(3, 'setting toe/crest index: ' // &
                       adj(num2str(toe_crest_index)))
        else if (eql(toe_crest,nanr)) then
            toe_crest = z(toe_crest_index)
            call logger(3, 'setting toe/crest value: ' // &
                       adj(num2str(toe_crest)))
        end if

        ! sea level rise (default is 1.0)
        if (eql(ds, nanr)) then
            ds = 1.d0
            call logger(1, 'sea level rise not set, using' // &
                           ' default value: ' // &
                            adj(num2str(ds)))
        end if

        ! Depth of closure (1 and 2)
        ! set up dc_index (upper depth of closure)
        if (eql(dc , nanr)) then
            dc = - 10.d0
            call logger(1, 'Depth of closure not set, using dc=: ' &
                       // adj(num2str(dc)))
        else
            call logger(3, 'Depth of closure set to: ' // &
                       adj(num2str(dc)))
        end if
        tmp_array = 1
        where(z .ge. dc) tmp_array = 0
        dc_index = minloc(tmp_array, 1, back=.true.) + 1
        call logger(3, 'Depth of closure index set to: ' // &
                   adj(num2str(dc_index)))

        if (eql(dc2 , nanr)) then
            dc2 = dc - 0.1d0
            call logger(1, 'Depth of closure 2 not set, using dc2= ' &
                       // adj(num2str(dc2)))
        else
            call logger(3, 'Depth of closure 2 set to: ' // &
                       adj(num2str(dc2)))
        end if
        tmp_array = 1
        where(z .ge. dc2) tmp_array = 0
        dc2_index = minloc(tmp_array, 1, back=.true.) + 1
        call logger(3, 'Depth of closure 2 index set to: ' // &
                   adj(num2str(dc2_index)))

        ! sediment flux (volume), default is 0.0
        if (.not. eql(dv_input, 0.d0)) then ! volume change
            call logger(3, 'Sediment (budget/deficit) = ' // &
                       adj(num2str(dv_input)))
        end if

        ! slump (erosion of dunes) default is True, i.e. slump
        call logger(3, 'slumping set to: ' // &
                   adj(num2str(slump%switch)) // ' with slope: ' // &
                   adj(num2str(slump%slope)) // ' and cap: ' // &
                   adj(num2str(slump%cap)))
    end subroutine setup_shoretrans

    subroutine read_xshore()
        implicit none

        integer :: n_tmp, ios, i
        character(charlen) :: line
        real(kind=8):: dx_tmp
        real(kind=8), allocatable :: x_tmp(:), z_tmp(:), rock_tmp(:)

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
        dx_tmp = x_tmp(2) - x_tmp(1) ! assume uniform spacing
        ! check if we need to interpolate the cross-shore profile
        if (eql(dx, nanr)) then ! dx not set
            dx = dx_tmp
            n_pts = n_tmp
            allocate(x(n_pts), z(n_pts), z_rock(n_pts))
            x = x_tmp
            z = z_tmp
            z_rock = rock_tmp
        else ! dx was set
            if (dx .lt. 0) then
                n_pts = int(-(x_tmp(n_tmp) - x_tmp(1)) / dx) + 1
                allocate(x(n_pts), z(n_pts), z_rock(n_pts))
                call logger(3, 'Interpolating cross-shore profile ' // &
                'from ' // &
                adj(num2str(n_tmp)) // ' points to ' // &
                adj(num2str(n_pts)) // ' points')
                do i=1,n_pts
                    x(i) = x_tmp(n_tmp) + dx * (i-1)
                end do
            else if (abs(dx) .lt. eps*eps) then
                call logger(0, 'dx cannot be zero')
                stop
            else
                ! new size of x
                n_pts = int((x_tmp(n_tmp) - x_tmp(1)) / dx) +1
                allocate(x(n_pts), z(n_pts), z_rock(n_pts))
                call logger(3, 'Interpolating cross-shore profile ' // &
                           'from ' // &
                           adj(num2str(n_tmp)) // ' points to ' // &
                           adj(num2str(n_pts)) // ' points')
                do i=1,n_pts
                    x(i) = x_tmp(1) + dx * (i-1)
                end do
            end if
            ! interpolate new profile
            z = interp1_vec(x_tmp, x, z_tmp)
            if (rock .ne. 1) then
                z_rock = z - 100.d0
            else
                z_rock = interp1_vec(x_tmp, x, rock_tmp)
            end if
        end if
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

        end subroutine check_errors

end module st_initialization