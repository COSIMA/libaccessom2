module field_mod

use netcdf, only : nf90_max_name
use datetime_module, only : datetime

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=1024) :: filename_template
    type(datetime) :: timestamp
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: dt
    type(ncvar) :: data_var
    real, dimension(:, :), allocatable :: data_array
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, ncname, filename_template, filename)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, ncname
    character(len=*), intent(in) :: filename_template, filename

    self%name = name
    self%filename_template = filename_template
    self%timestamp = datetime(HUGE(1))

    self%ncvar%init(ncname, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt

end subroutine

subroutine field_update_data(self, filename, forcing_date)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    if (filename /= self%ncvar%filename) then
        self%refresh(filename)
    endif

    indx = self%ncvar%get_index_for_datetime(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvar%get_index_for_datetime(forcing_date, .true.)
    endif
    call assert(indx /= -1, &
                "Could not find forcing date "//forcing_date%isoformat())

    call self%ncvar%read_data(indx, self%data_array)
    fld%timestamp = forcing_date

end subroutine field_update_data

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
