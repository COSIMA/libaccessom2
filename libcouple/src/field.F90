module field_mod

use netcdf, only : nf90_max_name
use ncvar_mod, only : ncvar_type => ncvar
use datetime_module, only : datetime
use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=1024) :: filename_template
    type(datetime) :: timestamp
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: dt
    type(ncvar_type) :: ncvar
    real, dimension(:, :), allocatable :: data_array

    type(logger_type) :: logger
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: update_data => field_update_data
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, ncname, filename_template, filename, logger)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, ncname
    character(len=*), intent(in) :: filename_template, filename
    type(logger_type), intent(in) :: logger

    self%name = name
    self%filename_template = filename_template
    self%timestamp = datetime(HUGE(1))
    self%logger = logger

    call self%ncvar%init(ncname, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt

end subroutine

subroutine field_update_data(self, filename, forcing_date)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    integer :: indx

    if (trim(filename) /= trim(self%ncvar%filename)) then
        call self%ncvar%refresh(filename)
    endif

    indx = self%ncvar%get_index_for_datetime(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvar%get_index_for_datetime(forcing_date, .true.)
    endif
    call assert(indx /= -1, &
                "Could not find forcing date "//forcing_date%isoformat())

    call self%logger%write(LOG_DEBUG, 'field_update_data: file '//trim(filename))
    call self%logger%write(LOG_DEBUG, 'field_update_data: index ', indx)
    call self%ncvar%read_data(indx, self%data_array)
    self%timestamp = forcing_date

end subroutine field_update_data

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
