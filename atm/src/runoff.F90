module runoff_mod

use remap_runoff_mod, only : remap_runoff_class, remap_runoff_do
use remap_runoff_mod, only : remap_runoff_new, remap_runoff_del

implicit none
private 
public runoff_type

type runoff_type
    private
contains
    private
    procedure, public :: init
    procedure, public :: remap
end type runoff_type

type(remap_runoff_class) :: remap_runoff

! Conservatively redistribute runoff exceeding runoff_cap in specified regions.
! Regions specify grid points that will be checked for whether they exceed the cap;
! excess runoff from those grid points may be redistributed outside the specified region.
integer, parameter :: max_caps = 4 ! maximum number of runoff cap regions (increase if want more; also make the default arrays below match)
integer :: num_runoff_caps = 1 ! number of runoff cap regions to actually use
real(kind=dbl_kind), dimension(max_caps) :: runoff_caps = (/ 0.03, 0.0, 0.0, 0.0 /) ! kg/m^2/s  runoff cap applied in each region (0.0 = no cap)
! runoff cap is applied to all points between or including these index limits
integer, dimension(max_caps) :: runoff_caps_is = (/ 0, 0, 0, 0 /) ! starting i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_ie = (/ 1000000, -1, -1, -1 /) ! ending i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_js = (/ 0, 0, 0, 0 /) ! starting j index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_je = (/ 1000000, -1, -1, -1 /) ! ending j index for each runoff region (count from 1)


namelist /runoff_nml/ &
    remap_weights, & 
    num_runoff_caps, &
    runoff_caps, &
    runoff_caps_is, &
    runoff_caps_ie, &
    runoff_caps_js, &
    runoff_caps_je

contains

subroutine init(this, ice_grid)

    type(runoff_type), intent(inout) :: this
    type(ice_grid_type), intent(in) :: ice_grid

    ! Rean input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, runoff_nml)
    close(unit=99)
    num_runoff_caps = max(0, min(num_runoff_caps, max_caps))

    ! Initialise module level variables with details about the ice grid.
    if (trim(dataset) == 'jra55') then
      call remap_runoff_new(remap_runoff, 'rmp_jrar_to_cict_CONSERV.nc', &
                            ice_grid%lats, ice_grid%lons, ice_grid%mask, &
                            num_runoff_caps, runoff_caps, &
                            runoff_caps_is, runoff_caps_ie, &
                            runoff_caps_js, runoff_caps_je)
    else
      call remap_runoff_new(remap_runoff, 'rmp_corr_to_cict_CONSERV.nc', &
                            ice_grid%lats, ice_grid%lons, ice_grid%mask, &
                            num_runoff_caps, runoff_caps, &
                            runoff_caps_is, runoff_caps_ie, &
                            runoff_caps_js, runoff_caps_je)
    endif

    allocate(this%runoff_save())

end subroutine init

subroutine remap(this, input, output)

    type(runoff_type), intent(inout) :: this
    real, dimension(:, :), intent(in) :: input
    real, dimension(:, :), intent(inout) :: output

    ! Check whether runoff has changed before remapping.
    call remap_runoff_do(remap_runoff, input, output, this%mask)

end subroutine remap

end module runoff_mod

