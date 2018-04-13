
module accessom2_mod

use, intrinsic :: iso_c_binding, only: c_null_char
use datetime_module, only : datetime, c_strptime, tm2date, tm_struct, timedelta
use error_handler, only : assert

implicit none
private
public accessom2

type accessom2
    private

    character(len=6) :: model_name

    ! These are set by the user.
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
    procedure, pass(self), public :: progress_date => &
                                        accessom2_progress_date

    procedure, pass(self), public :: get_cur_forcing_date => &
                                        accessom2_get_cur_forcing_date
    procedure, pass(self), public :: get_cur_exp_date => &
                                        accessom2_get_cur_exp_date

    procedure, pass(self), public :: get_cur_forcing_date_str => &
                                        accessom2_get_cur_forcing_date_str
    procedure, pass(self), public :: get_cur_exp_date_str => &
                                        accessom2_get_cur_exp_date_str

    procedure, pass(self), public :: run_finished => accessom2_run_finished
    procedure, pass(self) :: calc_run_end_date
    procedure, pass(self), public :: get_total_runtime_in_seconds => &
                                     accessom2_get_total_runtime_in_seconds
    procedure, pass(self), public :: get_cur_runtime_in_seconds => &
                                     accessom2_get_cur_runtime_in_seconds
endtype accessom2

integer, parameter :: CALENDAR_NOLEAP = 1, CALENDAR_GREGORIAN = 2
character(*), parameter :: restart_file = 'accessom2_restart_datetime.nml'
character(*), parameter :: config_file = 'accessom2.nml'

character(len=19) :: forcing_start_date, forcing_end_date
character(len=19) :: exp_cur_date, forcing_cur_date
character(len=9) :: calendar = 'gregorian'
integer, dimension(3) :: restart_period

namelist /accessom2_nml/ forcing_start_date, forcing_end_date, restart_period, &
                            calendar
namelist /do_not_edit_nml/ forcing_cur_date, exp_cur_date

contains

subroutine accessom2_init(self, model_name)
    class(accessom2), intent(inout) :: self
    character(len=6), intent(in) :: model_name

    integer :: tmp_unit, rc
    type(tm_struct) :: ctime
    logical :: file_exists

    self%model_name = model_name

    ! Read namelist which includes information about the forcing start and end date
    inquire(file=config_file, exist=file_exists)
    call assert(file_exists, 'Input accessom2.nml does not exist.')
    open(newunit=tmp_unit, file='accessom2.nml')
    read(tmp_unit, nml=accessom2_nml)
    close(tmp_unit)

    if (trim(calendar) == 'noleap') then
        self%calendar = CALENDAR_NOLEAP
    else
        self%calendar = CALENDAR_GREGORIAN
    endif

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

    self%run_start_date = self%exp_cur_date
    self%run_end_date = self%calc_run_end_date()

endsubroutine accessom2_init

subroutine accessom2_sync_config(self, atm_intercomm, ice_intercomm, &
                                 ocean_intercomm)

endsubroutine accessom2_sync_config

function calc_run_end_date(self)
    class(accessom2), intent(inout) :: self

    type(datetime) :: calc_run_end_date
    integer :: year, month, day, hour, minute, second, i

    if (self%restart_period(3) > 0) then
        call assert(self%restart_period(1) == 0 .and. &
                    self%restart_period(2) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        day = self%exp_cur_date%getDay()
        hour = self%exp_cur_date%getHour()
        minute = self%exp_cur_date%getMinute()
        second = self%exp_cur_date%getSecond()

        second = second + self%restart_period(3)
        if (second > 0) then
            minute = minute + (second / 60)
            second = mod(second, 60)
        endif
        if (minute > 0) then
            hour = hour + (minute / 60)
            minute = mod(minute, 60)
        endif
        if (hour > 0) then
            day = day + (hour / 24)
            hour = mod(hour, 24)
        endif

        calc_run_end_date = datetime(self%exp_cur_date%getYear(), &
                                     self%exp_cur_date%getMonth(), 1, &
                                     hour, minute, second)
        do i=1, day-1
            if (calc_run_end_date%getMonth() == 2 .and. &
                calc_run_end_date%getDay() == 29 .and. &
                self%calendar == CALENDAR_NOLEAP) then
                calc_run_end_date = calc_run_end_date + timedelta(days=1)
            endif
            calc_run_end_date = calc_run_end_date + timedelta(days=1)
        enddo

    elseif (self%restart_period(2) > 0) then
        call assert(self%restart_period(1) == 0 .and. &
                    self%restart_period(3) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        year = self%exp_cur_date%getYear()
        month = self%exp_cur_date%getMonth() + self%restart_period(2)
        year = year + (month / 12)
        month = mod(month, 12)

        calc_run_end_date = datetime(year, month, &
                                     self%exp_cur_date%getDay(), &
                                     self%exp_cur_date%getHour(), &
                                     self%exp_cur_date%getMinute(), &
                                     self%exp_cur_date%getSecond())

    elseif (self%restart_period(1) > 0) then
        call assert(self%restart_period(2) == 0 .and. &
                    self%restart_period(3) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        year = self%exp_cur_date%getYear() + self%restart_period(1)
        calc_run_end_date = datetime(year, self%exp_cur_date%getMonth(), &
                                     self%exp_cur_date%getDay(), &
                                     self%exp_cur_date%getHour(), &
                                     self%exp_cur_date%getMinute(), &
                                     self%exp_cur_date%getSecond())
    endif

endfunction calc_run_end_date

!> Progress both the forcing and experiment dates
subroutine accessom2_progress_date(self, timestep)
    class(accessom2), intent(inout) :: self
    integer, intent(in) :: timestep

    ! Forcing date
    self%forcing_cur_date = self%forcing_cur_date + timedelta(seconds=timestep)
    if (self%forcing_cur_date%getMonth() == 2 .and. &
        self%forcing_cur_date%getMonth() == 29 .and. &
        self%calendar == CALENDAR_NOLEAP) then

        self%forcing_cur_date = self%forcing_cur_date + timedelta(days=1)
    endif

    if (self%forcing_cur_date >= self%run_end_date) then
        self%forcing_cur_date = self%forcing_start_date
    endif

    ! Experiment date
    self%exp_cur_date = self%exp_cur_date + timedelta(seconds=timestep)
    if (self%exp_cur_date%getMonth() == 2 .and. &
        self%exp_cur_date%getMonth() == 29 .and. &
        self%calendar == CALENDAR_NOLEAP) then

        self%exp_cur_date = self%exp_cur_date + timedelta(days=1)
    endif

    if (self%exp_cur_date > self%run_end_date) then
        self%exp_cur_date = self%run_end_date
    endif

endsubroutine accessom2_progress_date

function accessom2_get_cur_forcing_date(self)
    class(accessom2), intent(inout) :: self

    type(datetime) :: accessom2_get_cur_forcing_date

    accessom2_get_cur_forcing_date = self%forcing_cur_date

endfunction accessom2_get_cur_forcing_date

function accessom2_get_cur_forcing_date_str(self)
    class(accessom2), intent(inout) :: self

    character(len=19) :: accessom2_get_cur_forcing_date_str
    type(datetime) :: date

    date = self%get_cur_forcing_date()
    accessom2_get_cur_forcing_date_str = date%isoformat()

endfunction accessom2_get_cur_forcing_date_str

function accessom2_get_cur_exp_date(self)
    class(accessom2), intent(inout) :: self

    type(datetime) :: accessom2_get_cur_exp_date

    accessom2_get_cur_exp_date = self%exp_cur_date

endfunction accessom2_get_cur_exp_date

function accessom2_get_cur_exp_date_str(self)
    class(accessom2), intent(inout) :: self

    character(len=19) :: accessom2_get_cur_exp_date_str
    type(datetime) :: date

    date = self%get_cur_exp_date()
    accessom2_get_cur_exp_date_str = date%isoformat()

endfunction accessom2_get_cur_exp_date_str

function accessom2_get_total_runtime_in_seconds(self)
    class(accessom2), intent(inout) :: self

    integer :: accessom2_get_total_runtime_in_seconds

    type(timedelta) :: td

    td = self%run_end_date - self%run_start_date
    accessom2_get_total_runtime_in_seconds = td%total_seconds()

endfunction accessom2_get_total_runtime_in_seconds

function accessom2_get_cur_runtime_in_seconds(self)
    class(accessom2), intent(inout) :: self

    integer :: accessom2_get_cur_runtime_in_seconds

    type(timedelta) :: td

    td = self%exp_cur_date - self%run_start_date
    accessom2_get_cur_runtime_in_seconds = td%total_seconds()

endfunction accessom2_get_cur_runtime_in_seconds

function accessom2_run_finished(self)
    class(accessom2), intent(inout) :: self

    logical :: accessom2_run_finished

    if (self%exp_cur_date >= self%run_end_date) then
        accessom2_run_finished = .true.
    else
        accessom2_run_finished = .false.
    endif

endfunction accessom2_run_finished

subroutine accessom2_deinit(self)
    class(accessom2), intent(inout) :: self

    integer :: tmp_unit

    exp_cur_date = self%exp_cur_date%strftime('%Y-%m-%dT%H:%M:%S')
    forcing_cur_date = self%forcing_cur_date%strftime('%Y-%m-%dT%H:%M:%S')

    if (self%model_name == 'matmxx') then
        open(newunit=tmp_unit, file=restart_file)
        write(unit=tmp_unit, nml=do_not_edit_nml)
        close(tmp_unit)
    endif

end subroutine accessom2_deinit

end module accessom2_mod
