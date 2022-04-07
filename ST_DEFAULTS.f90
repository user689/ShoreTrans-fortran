!> @brief
!! default variables used by the whole program
!> @details
!! contains the default values for the variables used by the whole program
!!
!! created: 2022-02-18
module st_defaults
    implicit none

    ! Constants
    real, parameter :: nanr = -999.0 !< not a number (real)
    integer, parameter :: nani = -999 !< not a number (integer)
    integer, parameter :: charlen = 256 !< default length of strings
    integer, parameter :: fid = 18 ! id of files (read)

    ! shoreline variables
    integer :: n_pts !< number of points in shoreline (size of x,z arrays)
    real, allocatable, dimension(:) :: x(:), z(:) !< cross shore x and z
    real, allocatable, dimension(:) ::  z_final(:) !< final z values
    !> general options
    integer :: max_iter = 100 !< maximum number of iterations
    real :: ds = nanr !< sea level rise (m)
    real, parameter :: eps = 1.0e-6 !< convergence criterion



    !> verbosity level
    !! 0 = only errors
    !! 1 = errors and warnings
    !! 2 = errors, warnings and info
    !! 3 = errors, warnings, extra info
    !! 4 = errors, warnings, extra info and debug
    !! -1 = no output
    integer :: verbose = 2

    character(charlen) :: xshorefilename !< name of file containing x and z

    ! options for cross-shore profile
    ! Note: convention used is for z is positive
    ! upwards (unlike lxshore)
    real :: dc = nanr !< (upper) depth of closure (m)
    integer :: dc_index = nani !< index of dc
    real :: dc2 = nanr  !< (lower) depth of closure (m)
    integer :: dc2_index = nani !< index of dc2
    ! specify either a z-value or an index (1-based)
    real :: toe_crest = nanr !< toe crest elevation (m)
    integer :: toe_crest_index = nani !< index of toe_crest

    ! internal variables
    character(charlen) :: dir_name !< directory of case to be run

    ! initial variables
    real :: h  !< height of active profile
    real :: w  !< width of active profile
    real :: dx = nanr !< cross-shore step size
    real :: dv !< volume difference between adjacent profiles
    real :: dv_input = 0.0 !< input volume change

    ! slump variables
    type slump_type
      integer :: switch  !< 0=no slump, 1=slump
      real :: slope  !< dunes get eroded, this is the final slope
      real :: cap   !< ignore slump above this depth.
                    !! prevents slump for high cliffs
    end type slump_type
    type(slump_type) :: slump = slump_type(1, 30., 100.) !< default values

    ! options for rock layer
    integer :: rock = 0 !< 0=no rock, 1=rock
    real, allocatable, dimension(:) :: z_rock(:) !< rock layer z-values
end module st_defaults