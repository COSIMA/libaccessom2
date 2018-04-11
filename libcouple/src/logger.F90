module logger_mod

use mpi
use error_handler, only : assert

implicit none

private

type, public :: logger

    character(len=6) :: model_name
    character(len=1024) :: logfilename
    integer :: fp

contains
    procedure, pass(self), public :: init => logger_init
    procedure, pass(self), public :: deinit => logger_deinit
    procedure, pass(self), public :: write => logger_write
endtype logger

contains

subroutine logger_init(self, basename, logfiledir)
    class(logger), intent(inout) :: self
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: logfiledir

    character(len=5) :: pe_str
    integer :: pe, err
    logical :: file_exists

    call MPI_Comm_rank(MPI_COMM_WORLD, pe, err)
    call assert(err == 0, 'logger cannot get PE')
    write(pe_str, '(I5.5)') pe

    self%logfilename = trim(basename)//'.pe'//pe_str//'.log'
    if (present(logfiledir)) then
        self%logfilename = trim(logfiledir)//'/'//trim(self%logfilename)
    endif

    inquire(file=trim(self%logfilename), exist=file_exists)
    call assert(.not. file_exists, &
                'Log file already exists: '//trim(self%logfilename))
    open(newunit=self%fp, file=trim(self%logfilename))
endsubroutine

subroutine logger_write(self, str)
    class(logger), intent(in) :: self
    character(len=*), intent(in) :: str

    write(self%fp, *) trim(str)

endsubroutine logger_write

subroutine logger_deinit(self)
    class(logger), intent(inout) :: self

    close(self%fp)

endsubroutine logger_deinit

endmodule logger_mod
