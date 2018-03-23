module field_mod

use datetime_module, only : datetime

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=256) :: filename
    character(len=64) :: ncname
    type(datetime) :: timestamp
    integer :: nx, ny
    integer :: dt
    integer :: oasis_varid
    integer :: oasis_partid
    real, dimension(:, :), allocatable :: data_array
contains
    procedure, pass(self), public :: get_shape
endtype field

contains

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
