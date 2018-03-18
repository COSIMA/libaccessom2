module error_handler

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none

private
public assert

contains

subroutine assert(res, error_msg)

    logical, intent(in) :: res
    character(len=*), intent(in) :: error_msg

    if (.not. res) then
        write(stderr, error_msg)
        stop 1
    endif

end subroutine

end module
