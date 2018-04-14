module runoff_mod

use remap_runoff_mod, only : remap_runoff_class, remap_runoff_do
use remap_runoff_mod, only : remap_runoff_new, remap_runoff_del
use ice_grid_proxy_mod, only : ice_grid_proxy

implicit none
private
public runoff

type runoff
    private
contains
    private
    procedure, pass(self), public :: init
    procedure, pass(self), public :: remap
end type runoff

type(remap_runoff_class) :: remap_runoff

character(len=1024) :: remap_weights_file

! Conservatively redistribute runoff exceeding runoff_cap in specified regions.
! Regions specify grid points that will be checked for whether they exceed the cap;
! excess runoff from those grid points may be redistributed outside the specified region.
integer, parameter :: max_caps = 4 ! maximum number of runoff cap regions (increase if want more; also make the default arrays below match)

integer :: num_runoff_caps = 1 ! number of runoff cap regions to actually use
real, dimension(max_caps) :: runoff_caps = (/ 0.03, 0.0, 0.0, 0.0 /) ! kg/m^2/s  runoff cap applied in each region (0.0 = no cap)
! runoff cap is applied to all points between or including these index limits
integer, dimension(max_caps) :: runoff_caps_is = (/ 0, 0, 0, 0 /) ! starting i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_ie = (/ 1000000, -1, -1, -1 /) ! ending i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_js = (/ 0, 0, 0, 0 /) ! starting j index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_je = (/ 1000000, -1, -1, -1 /) ! ending j index for each runoff region (count from 1)

namelist /runoff_nml/ &
    remap_weights_file, &
    num_runoff_caps, &
    runoff_caps, &
    runoff_caps_is, &
    runoff_caps_ie, &
    runoff_caps_js, &
    runoff_caps_je

contains

subroutine init(self, target_grid)

    class(runoff), intent(inout) :: self
    class(ice_grid_proxy), intent(in) :: target_grid

    ! Rean input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, runoff_nml)
    close(unit=99)
    num_runoff_caps = max(0, min(num_runoff_caps, max_caps))

    call remap_runoff_new(remap_runoff, trim(remap_weights_file), &
                          target_grid%lats, target_grid%lons, target_grid%mask, &
                          num_runoff_caps, runoff_caps, &
                          runoff_caps_is, runoff_caps_ie, &
                          runoff_caps_js, runoff_caps_je)

end subroutine init

subroutine remap(self, input, output, output_mask)

    class(runoff), intent(inout) :: self
    real, dimension(:, :), intent(in) :: input
    real, dimension(:, :), intent(inout) :: output
    real, dimension(:, :), intent(in) :: output_mask

    ! Check whether runoff has changed before remapping.
    call remap_runoff_do(remap_runoff, input, output, output_mask)

end subroutine remap

end module runoff_mod

