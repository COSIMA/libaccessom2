module simple_timer_mod

use logger_mod, only : logger_type => logger, LOG_INFO
use error_handler, only : assert

implicit none
private
public simple_timer

type simple_timer
    private

    logical :: first_call
    logical :: enabled

    character(len=32) :: name

    real :: maxtime, mintime
    real :: mean, m2
    integer :: count
    integer :: starttime, endtime, count_rate

    type(logger_type), pointer :: logger

contains
    private
    procedure, pass(self), public :: init => simple_timer_init
    procedure, pass(self), public :: write_stats => simple_timer_write_stats
    procedure, pass(self), public :: start => simple_timer_start
    procedure, pass(self), public :: stop => simple_timer_stop
    procedure, pass(self) :: update_variance
    procedure, pass(self) :: finalise_variance
endtype simple_timer

contains

subroutine simple_timer_init(self, name, loggerin, enabled, include_first_call)
    class(simple_timer), intent(inout) :: self
    character(len=*), intent(in) :: name
    type(logger_type), target, intent(in) :: loggerin
    logical, optional, intent(in) :: enabled
    logical, optional, intent(in) :: include_first_call

    self%enabled = .true.
    if (present(enabled)) then
        if (.not. enabled) then
            self%enabled = .false.
        endif
    endif

    if (.not. self%enabled) then
        return
    endif

    self%name = trim(name)
    self%logger => loggerin

    self%first_call = .false.
    if (present(include_first_call)) then
        if (.not. include_first_call) then
            self%first_call = .true.
        endif
    endif

    self%starttime = 0
    self%endtime = 0
    self%maxtime = -1
    self%mintime = -1
    self%count = 0
    self%mean = 0
    self%m2 = 0
    call system_clock(COUNT_RATE=self%count_rate) 

endsubroutine simple_timer_init

subroutine simple_timer_start(self)
    class(simple_timer), intent(inout) :: self

    if (.not. self%enabled) then
        return
    endif

    if (.not. self%first_call) then
        call system_clock(self%starttime)
    endif

endsubroutine simple_timer_start

subroutine simple_timer_stop(self)
    class(simple_timer), intent(inout) :: self

    real :: time

    if (.not. self%enabled) then
        return
    endif

    if (.not. self%first_call) then
        call assert(self%starttime > 0, 'simple_timer: timer_start not called.')

        call system_clock(self%endtime)

        time = real(self%endtime - self%starttime)/self%count_rate
        if (self%maxtime == -1 .or. time > self%maxtime) then
            self%maxtime = time
        endif
        if (self%mintime == -1 .or. time < self%mintime) then
            self%mintime = time
        endif

        call self%update_variance(time)

    endif
    self%first_call = .false.

endsubroutine simple_timer_stop

subroutine simple_timer_write_stats(self)
    class(simple_timer), intent(inout) :: self

    real :: variance
    character(len=7) :: max_str, min_str, mean_str, variance_str, count_str

    if (.not. self%enabled) then
        return
    endif

    call self%finalise_variance(variance)

    write(min_str, '(F7.3)') self%mintime
    write(max_str, '(F7.3)') self%maxtime
    write(mean_str, '(F7.3)') self%mean
    write(variance_str, '(F7.3)') variance
    write(count_str, '(I7.7)') self%count

    call self%logger%write(LOG_INFO, '{ timer-'//trim(self%name)//' min: '//min_str//' }')
    call self%logger%write(LOG_INFO, '{ timer-'//trim(self%name)//' max: '//max_str//' }')
    call self%logger%write(LOG_INFO, '{ timer-'//trim(self%name)//' mean: '//mean_str//' }')
    call self%logger%write(LOG_INFO, '{ timer-'//trim(self%name)//' variance: '//variance_str//' }')
    call self%logger%write(LOG_INFO, '{ timer-'//trim(self%name)//' count: '//count_str//' }')

endsubroutine simple_timer_write_stats

subroutine update_variance(self, new_value)
    class(simple_timer), intent(inout) :: self
    real, intent(in) :: new_value

    real :: delta, delta2

    ! See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    self%count = self%count + 1
    delta = new_value - self%mean
    self%mean = self%mean + delta / self%count
    delta2 = new_value - self%mean
    self%M2 = self%M2 + delta*delta2

endsubroutine update_variance

subroutine finalise_variance(self, variance)
    class(simple_timer), intent(inout) :: self
    real, intent(out) :: variance

    if (self%count > 1) then
        variance = self%M2 / (self%count - 1)
    else
        variance = 0
    endif

endsubroutine finalise_variance

endmodule simple_timer_mod

