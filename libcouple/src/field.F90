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
    integer :: oasis_varid
    integer :: oasis_partid
    real, dimension(:, :), allocatable :: data_array
endtype field

endmodule field_mod

