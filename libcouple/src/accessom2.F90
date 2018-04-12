module accessom2_mod

!> Model-level configuration and synchronisation

use, intrinsic :: iso_c_binding, only: c_null_char
use datetime_module, only : datetime, c_strptime, tm2date, tm_struct, timedelta
use error_handler, only : assert

implicit none
private
public accessom2

type accessom2
    private

    character(len=6) :: model_name

    type(datetime) :: forcing_start_date, forcing_end_date
    integer, dimension(3) :: restart_period
    ! FIXME: this should be read from the forcing file.
    integer :: calendar

    ! These are internal
    type(datetime) :: exp_cur_date, forcing_cur_date
    type(datetime) :: run_start_date, run_end_date

contains

    private
    procedure, pass(self), public :: init => accessom2_init
    procedure, pass(self), public :: deinit => accessom2_deinit
endtype accessom2

contains

character(*), parameter :: restart_file = 'accessom2_restart_datetime.nml'
character(*), parameter :: config_file = 'accessom2.nml'

character(len=19) :: forcing_start_date, forcing_end_date
character(len=19) :: exp_cur_date, forcing_cur_date
character(len=9) :: calendar = 'gregorian'
integer, dimension(3) :: restart_period

namelist /date_manager_nml/ forcing_start_date, forcing_end_date, restart_period
namelist /do_not_edit_nml/ forcing_cur_date, exp_cur_date

subroutine accessom2_init(self, model_name)
    class(accessom2), intent(inout) :: self
    character(len=6), intent(in) :: model_name

    integer :: tmp_unit, rc
    type(tm_struct) :: ctime
    logical :: file_exists

    ! Read namelist which includes information about the forcing start and end date
    inquire(file=config_file, exist=file_exists)
    call assert(file_exists, 'Input accessom2.nml does not exist.')
    open(newunit=tmp_unit, file='accessom2.nml')
    read(tmp_unit, nml=date_manager_nml)
    close(tmp_unit)

    rc = c_strptime(forcing_start_date//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    call assert(rc /= 0, 'Bad forcing_start_date format in '//config_file)
    self%forcing_start_date = tm2date(ctime)

    rc = c_strptime(forcing_end_date//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    call assert(rc /= 0, 'Bad forcing_end_date format in '//config_file)
    self%forcing_end_date = tm2date(ctime)

    self%restart_period = restart_period

    ! Read in exp_cur_date and focing_cur_date from restart file.
    inquire(file=restart_file, exist=file_exists)
    if (file_exists) then
        open(newunit=tmp_unit, file=restart_file)
        read(tmp_unit, nml=do_not_edit_nml)
        close(tmp_unit)
        rc = c_strptime(forcing_cur_date//c_null_char, &
                        "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
        call assert(rc /= 0, 'Bad forcing_cur_date format in '//restart_file)
        self%forcing_cur_date = tm2date(ctime)

        rc = c_strptime(exp_cur_date//c_null_char, &
                        "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
        call assert(rc /= 0, 'Bad exp_cur_date format in '//restart_file)
        self%exp_cur_date = tm2date(ctime)
    else
        self%forcing_cur_date = self%forcing_start_date
        self%exp_cur_date = self%forcing_start_date
    endif

endsubroutine accessom2_init

subroutine accessom2_deinit(self, model_name)
endsubroutine accessom2_deinit

endmodule accessom2_mod


