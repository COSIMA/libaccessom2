module coupling_field_mod

use netcdf, only : nf90_max_name
use ncvar_mod, only : ncvar_type => ncvar
use datetime_module, only : datetime
use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG

implicit none
private

type, public :: coupling_field
    character(len=64) :: name
    type(datetime) :: timestamp
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: dt
    character(len=9) :: calendar

    type(logger_type) :: logger
contains
    procedure, pass(self), public :: init => coupling_field_init
endtype coupling_field

contains

subroutine coupling_field_init(self, name, loggerin)
    class(coupling_field), intent(inout) :: self
    character(len=*), intent(in) :: name
    type(logger_type), intent(in) :: loggerin

    self%name = name
    self%timestamp = datetime(HUGE(1))
    self%logger = loggerin

end subroutine

endmodule coupling_field_mod
