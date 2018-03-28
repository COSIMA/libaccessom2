module field_mod

use netcdf, only : nf90_max_name
use datetime_module, only : datetime

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=256) :: filename
    character(len=nf90_max_name) :: ncname
    type(datetime) :: timestamp
    integer :: nx, ny
    integer :: dt
    integer :: oasis_varid
    integer :: oasis_partid
    integer :: nc_idx_guess
    real, dimension(:, :), allocatable :: data_array
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, filename, ncname, nx, ny, dt)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, filename, ncname
    integer, intent(in) :: nx, ny
    integer, optional, intent(in) :: dt

    self%name = name
    self%filename = filename
    self%ncname = ncname
    allocate(self%data_array(nx, ny))
    self%data_array(:, :) = HUGE(1.0)
    self%timestamp = datetime(HUGE(1))
    if (present(dt)) then
        self%dt = dt
    endif

    self%nc_idx_guess = 1

end subroutine

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
