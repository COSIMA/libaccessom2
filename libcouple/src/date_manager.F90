
module date_manager_mod

use, intrinsic :: iso_c_binding, only: c_null_char
use datetime_module, only : datetime, c_strptime, tm2date, tm_struct, timedelta
use error_handler, only : assert

implicit none
private
public date_manager
public integer, parameter :: CALENDAR_NOLEAP = 1, CALENDAR_GREGORIAN = 2

type date_manager
    private

    ! These are set by the user.
    type(datetime) :: forcing_start_date
    type(datetime) :: forcing_end_date
    integer, dimension(3) :: restart_period
    integer :: timestep
    integer :: calendar

    ! These are internal
    type(datetime) :: exp_start_date
    type(datetime) :: exp_cur_date, forcing_cur_date
    type(datetime) :: run_end_date

    character(len=*), parameter :: restart_file = 'accessom2_restart_datetime.nml'
    character(len=*), parameter :: config_file = 'accessom2.nml'

contains
    private
    procedure, pass(self), public :: init => date_manager_init
    procedure, pass(self), public :: deinit => date_manager_deinit
    procedure, pass(self), public :: progress_forcing_date => &
                                        date_manager_progress_forcing_date
    procedure, pass(self), public :: progress_exp_date => &
                                        date_manager_progress_exp_date
    procedure, pass(self), public :: run_finished => &
                                        date_manager_run_finished
    procedure, pass(self), public :: get_cur_forcing_date => &
                                        date_manager_get_cur_forcing_date
    procedure, pass(self), public :: get_cur_exp_date => &
                                        date_manager_get_cur_exp_date
    procedure, pass(self), public :: run_finished => date_manager_run_finished
    procedure, pass(self) :: run_end_date => date_manager_run_end_date
endtype date_manager

character(len=19) :: forcing_start_date, forcing_end_date
character(len=19) :: exp_cur_date, forcing_cur_date
character(len=9) :: calendar
integer, dimension(3) :: restart_period

namelist /time_manager_nml/ forcing_start_date, forcing_end_date, calendar, restart_period
namelist /do_not_edit_nml/ current_forcing_date, current_exp_date

contains

subroutine date_manager_init(self, timestep, calendar)

    class(date_manager), intent(inout) :: self
    integer, intent(in) :: timestep, calendar

    self%timestep = timestep
    self%calendar = calendar

    ! Read namelist which includes information about the forcing start and end date
    inquire(file=self.config_file, exist=file_exists)
    call assert(file_exists, 'Input accessom2.nml does not exist.')
    open(newunit=tmp_unit, file='accessom2.nml')
    read(tmp_unit, nml=time_manager_nml)
    close(tmp_unit)

    rc = c_strptime(forcing_start_date//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    call assert(rc /= 0, 'Bad forcing_start_date format in '//self.config_file)
    self%forcing_start_date = tm2date(ctime)

    rc = c_strptime(forcing_end_date//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    call assert(rc /= 0, 'Bad forcing_end_date format in '//self.config_file)
    self%forcing_end_date = tm2date(ctime)

    self%restart_period = restart_period

    ! Read in exp_cur_date and focing_cur_date from restart file.
    inquire(file=self.restart_file, exist=file_exists)
    if (file_exists) then
        open(newunit=tmp_unit, file=self.restart_file)
        read(tmp_unit, nml=do_not_edit_nml)
        close(tmp_unit)
        rc = c_strptime(forcing_cur_date//c_null_char, &
                        "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
        call assert(rc /= 0, 'Bad forcing_cur_date format in '//self.restart_file)
        self%forcing_cur_date = tm2date(ctime)

        rc = c_strptime(exp_cur_date//c_null_char, &
                        "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
        call assert(rc /= 0, 'Bad exp_cur_date format in '//self.restart_file)
        self%exp_cur_date = tm2date(ctime)
    else
        self%forcing_cur_date = self%forcing_start_date
        self%exp_cur_date = self%forcing_start_date
    endif

    self%run_end_date = self%date_manager_run_end_date()

endsubroutine

function date_manager_run_end_date(self)
    class(date_manager), intent(inout) :: self

    type(datetime) :: date_manager_run_end_date
    integer :: year, month, day, hour, minute, second, i

    if (self%restart_period(3) > 0) then
        call assert(self%restart_period(1) == 0 .and. &
                    self%restart_period(2) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        day = self%cur_exp_date%getDay()
        hour = self%cur_exp_date%getHour()
        minute = self%cur_exp_date%getMinute()
        second = self%cur_exp_date%getSecond()

        second = second + job_runtime(3)
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

        date_manager_run_end_date = datetime(self%cur_exp_date%getYear(), &
                                             self%cur_exp_date%getMonth(), 1, &
                                             hour, minute, second)
        do i=1, day-1
            if (run_end_date%getMonth() == 2 .and. run_end_date%getDay() == 29 &
                .and. trim(self%calendar) == 'noleap')  then
                run_end_date = run_end_date + timedelta(days=1)
            endif
            run_end_date = run_end_date + timedelta(days=1)
        enddo

    elseif (run_period(2) > 0) then
        call assert(run_period(1) == 0 .and. run_period(3) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        year = self%cur_exp_date%getYear()
        month = self%cur_exp_date%getMonth() + run_period(2)
        year = year + (month / 12)
        month = mod(month, 12)

        date_manager_run_end_date = datetime(year, month, &
                                             self%cur_exp_date%getDay(), &
                                             self%cur_exp_date%getHour(), &
                                             self%cur_exp_date%getMinute(), &
                                             self%cur_exp_date%getSecond())

    elseif (run_period(1) > 0) then
        call assert(run_period(2) == 0 .and. run_period(3) == 0, &
                    'Job runtime must be only one of years, months, seconds')

        year = self%cur_exp_date%getYear() + run_period(1)
        date_manager_run_end_date = datetime(year, &
                                             self%cur_exp_date%getMonth(), &
                                             self%cur_exp_date%getDay(), &
                                             self%cur_exp_date%getHour(), &
                                             self%cur_exp_date%getMinute(), &
                                             self%cur_exp_date%getSecond())
    endif

endfunction date_manager_run_end_date

subroutine date_manager_progress_forcing_date(self)
    class(date_manager), intent(inout) :: self

    self%forcing_cur_date += timedelta(seconds=self.timestep)
    if self%forcing_cur_date%getMonth() == 2 .and. &
        self%forcing_cur_date%getMonth() == 29 .and. &
        trim(self%calendar) == 'noleap':

        self%forcing_cur_date += timedelta(days=1)

    if self%forcing_cur_date > self%run_end_date:
        self%forcing_cur_date = self%run_end_date

endsubroutine date_manager_progress_forcing_date

subroutine date_manager_progress_exp_date(self)
    class(date_manager), intent(inout) :: self

    self%exp_cur_date += timedelta(seconds=self.timestep)
    if self%exp_cur_date%getMonth() == 2 .and. &
        self%exp_cur_date%getMonth() == 29 .and. &
        trim(self%calendar) == 'noleap':

        self%exp_cur_date += timedelta(days=1)

    if self%exp_cur_date > self%run_end_date:
        self%exp_cur_date = self%run_end_date

endsubroutine date_manager_progress_exp_date

function date_manager_get_cur_forcing_date(self)
    class(date_manager), intent(inout) :: self

    type(datetime) :: date_manager_get_cur_forcing_date

    date_manager_get_cur_forcing_date = self%forcing_cur_date

endfunction date_manager_get_cur_forcing_date

function date_manager_get_cur_exp_date(self)
    class(date_manager), intent(inout) :: self

    type(datetime) :: date_manager_get_cur_exp_date

    date_manager_get_cur_exp_date = self%exp_cur_date

endfunction date_manager_get_cur_exp_date

function date_manager_run_finished(self)
    class(date_manager), intent(inout) :: self

    logical :: date_manager_run_finished

    if (self%exp_cur_date >= self%run_end_date) then
        date_manager_run_finished = .true.
    else:
        date_manager_run_finished = .false.

endfunction date_manager_run_finished


subroutine deinit(self)
    class(date_manager), intent(inout) :: self

    exp_cur_date = self%exp_cur_date%strftime('%Y-%m-%dT%H:%M:%S')
    forcing_cur_date = self%forcing_cur_date%strftime('%Y-%m-%dT%H:%M:%S')

    if (self%model_name == 'matmxx') then
        open(newunit=tmp_unit, file=self.restart_file)
        write(unit=tmp_unit, nml=do_not_edit_nml)
        close(tmp_unit)
    endif

end subroutine deinit


end module date_manager_mod
