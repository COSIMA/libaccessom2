
module date_manager_mod

use, intrinsic :: iso_c_binding, only: c_null_char
use datetime_module, only : datetime, c_strptime, tm2date, tm_struct, timedelta
use error_handler, only : assert

implicit none
private
public date_manager

type date_manager
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
    procedure, pass(self), public :: init => date_manager_init
    procedure, pass(self), public :: deinit => date_manager_deinit
    procedure, pass(self), public :: progress_date => &
                                        date_manager_progress_date

    procedure, pass(self), public :: get_cur_forcing_date => &
                                        date_manager_get_cur_forcing_date
    procedure, pass(self), public :: get_cur_exp_date => &
                                        date_manager_get_cur_exp_date

    procedure, pass(self), public :: get_cur_forcing_date_str => &
                                        date_manager_get_cur_forcing_date_str
    procedure, pass(self), public :: get_cur_exp_date_str => &
                                        date_manager_get_cur_exp_date_str

    procedure, pass(self), public :: run_finished => date_manager_run_finished
    procedure, pass(self) :: calc_run_end_date
    procedure, pass(self), public :: get_total_runtime_in_seconds => &
                                     date_manager_get_total_runtime_in_seconds
    procedure, pass(self), public :: get_cur_runtime_in_seconds => &
                                     date_manager_get_cur_runtime_in_seconds
endtype date_manager

contains

subroutine date_manager_init(self, model_name)
    class(date_manager), intent(inout) :: self
    character(len=6), intent(in) :: model_name

    self%model_name = model_name

    self%run_start_date = self%exp_cur_date
    self%run_end_date = self%calc_run_end_date()

endsubroutine date_manager_init

function calc_run_end_date(self)
    class(date_manager), intent(inout) :: self

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
subroutine date_manager_progress_date(self, timestep)
    class(date_manager), intent(inout) :: self
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

endsubroutine date_manager_progress_date

function date_manager_get_cur_forcing_date(self)
    class(date_manager), intent(inout) :: self

    type(datetime) :: date_manager_get_cur_forcing_date

    date_manager_get_cur_forcing_date = self%forcing_cur_date

endfunction date_manager_get_cur_forcing_date

function date_manager_get_cur_forcing_date_str(self)
    class(date_manager), intent(inout) :: self

    character(len=19) :: date_manager_get_cur_forcing_date_str
    type(datetime) :: date

    date = self%get_cur_forcing_date()
    date_manager_get_cur_forcing_date_str = date%isoformat()

endfunction date_manager_get_cur_forcing_date_str

function date_manager_get_cur_exp_date(self)
    class(date_manager), intent(inout) :: self

    type(datetime) :: date_manager_get_cur_exp_date

    date_manager_get_cur_exp_date = self%exp_cur_date

endfunction date_manager_get_cur_exp_date

function date_manager_get_cur_exp_date_str(self)
    class(date_manager), intent(inout) :: self

    character(len=19) :: date_manager_get_cur_exp_date_str
    type(datetime) :: date

    date = self%get_cur_exp_date()
    date_manager_get_cur_exp_date_str = date%isoformat()

endfunction date_manager_get_cur_exp_date_str

function date_manager_get_total_runtime_in_seconds(self)
    class(date_manager), intent(inout) :: self

    integer :: date_manager_get_total_runtime_in_seconds

    type(timedelta) :: td

    td = self%run_end_date - self%run_start_date
    date_manager_get_total_runtime_in_seconds = td%total_seconds()

endfunction date_manager_get_total_runtime_in_seconds

function date_manager_get_cur_runtime_in_seconds(self)
    class(date_manager), intent(inout) :: self

    integer :: date_manager_get_cur_runtime_in_seconds

    type(timedelta) :: td

    td = self%exp_cur_date - self%run_start_date
    date_manager_get_cur_runtime_in_seconds = td%total_seconds()

endfunction date_manager_get_cur_runtime_in_seconds

function date_manager_run_finished(self)
    class(date_manager), intent(inout) :: self

    logical :: date_manager_run_finished

    if (self%exp_cur_date >= self%run_end_date) then
        date_manager_run_finished = .true.
    else
        date_manager_run_finished = .false.
    endif

endfunction date_manager_run_finished

subroutine date_manager_deinit(self)
    class(date_manager), intent(inout) :: self

    integer :: tmp_unit

    exp_cur_date = self%exp_cur_date%strftime('%Y-%m-%dT%H:%M:%S')
    forcing_cur_date = self%forcing_cur_date%strftime('%Y-%m-%dT%H:%M:%S')

    if (self%model_name == 'matmxx') then
        open(newunit=tmp_unit, file=restart_file)
        write(unit=tmp_unit, nml=do_not_edit_nml)
        close(tmp_unit)
    endif

end subroutine date_manager_deinit

end module date_manager_mod
