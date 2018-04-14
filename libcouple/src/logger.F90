module logger_mod

use mpi
use error_handler, only : assert
implicit none
private

integer, parameter, public :: LOG_DEBUG = 10
integer, parameter, public :: LOG_INFO = 20
integer, parameter, public :: LOG_WARNING = 30
integer, parameter, public :: LOG_ERROR = 40

type, public :: logger

    integer :: loglevel
    character(len=6) :: model_name
    character(len=1024) :: logfilename
    integer :: fp

contains
    procedure, pass(self), public :: init => logger_init
    procedure, pass(self), public :: deinit => logger_deinit
    procedure, pass(self), public :: write => logger_write
endtype logger

contains

subroutine logger_init(self, basename, logfiledir, loglevel)
    class(logger), intent(inout) :: self
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: logfiledir
    character(len=*), optional, intent(in) :: loglevel

    character(len=5) :: pe_str
    integer :: pe, err
    logical :: file_exists

    print*, loglevel
    if (present(loglevel)) then
        if (trim(loglevel) == 'DEBUG') then
            self%loglevel = LOG_DEBUG
        elseif (trim(loglevel) == 'INFO') then
            self%loglevel = LOG_INFO
        elseif (trim(loglevel) == 'WARNING') then
            self%loglevel = LOG_WARNING
        elseif (trim(loglevel) == 'ERROR') then
            self%loglevel = LOG_ERROR
        else
            call assert(.false., 'logger_init: bad log level')
        endif
    else
        self%loglevel = LOG_ERROR
    endif

    call MPI_Comm_rank(MPI_COMM_WORLD, pe, err)
    write(pe_str, '(I5.5)') pe

    self%logfilename = trim(basename)//'.pe'//pe_str//'.log'
    if (present(logfiledir)) then
        self%logfilename = trim(logfiledir)//'/'//trim(self%logfilename)
    endif

    inquire(file=trim(self%logfilename), exist=file_exists)
    open(newunit=self%fp, file=trim(self%logfilename))
endsubroutine

subroutine logger_write(self, loglevel, str, intnum)
    class(logger), intent(in) :: self
    integer, intent(in) :: loglevel
    character(len=*), intent(in) :: str
    integer, optional, intent(in) :: intnum

    character(len=10) :: intnum_str

    if (loglevel >= self%loglevel) then
        if (present(intnum)) then
            write(intnum_str, '(I10.10)') intnum
            write(self%fp, *) trim(str)//' '//intnum_str
        else
            write(self%fp, *) trim(str)
        endif
    endif

endsubroutine logger_write

subroutine logger_deinit(self)
    class(logger), intent(inout) :: self

    close(self%fp)

endsubroutine logger_deinit

endmodule logger_mod
