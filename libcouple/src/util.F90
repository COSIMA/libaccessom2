module util_mod

use netcdf
use error_handler, only : assert
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
implicit none

contains

subroutine ncheck(status, error_str)

    integer, intent(in) :: status
    character(len=*), intent(in), optional :: error_str

    if (status /= nf90_noerr) then
        write(stdout, '(/a)') 'Error - from NetCDF library'
        if (present(error_str)) then
            write(stdout, '(a)') error_str
        endif
        write(stdout, '(a/)')   trim(nf90_strerror(status))
        stop
    end if

end subroutine ncheck

!> Replace all occurrences of 'pattern' with 'replace' in string.
! Based on: http://fortranwiki.org/fortran/show/String_Functions
function replace_text(string, pattern, replace)  result(outs)

    character(len=*), intent(in) :: string, pattern, replace
    character(len(string)) :: outs
    integer             :: i, nt, nr

    outs = string ; nt = len_trim(pattern) ; nr = len_trim(replace)
    do
        i = index(outs,pattern(:nt)) ; if (i == 0) exit
        outs = outs(:i-1) // replace(:nr) // outs(i+nt:)
    end do

end function replace_text

!> Return name of the first file that matches pattern
function first_file_matching_pattern(pattern)
    character(len=*), intent(in) :: pattern

    character(len=1024) :: first_file_matching_pattern
    character(len=1024) :: command
    integer :: exitstat, tmp_unit

    open(newunit=tmp_unit, file='tmpfile.txt')
    command = 'ls '//trim(pattern)//' > tmpfile.txt 2> /dev/null'

    exitstat = 1
    call execute_command_line(trim(command), exitstat=exitstat)
    if (exitstat == 0) then
        read(tmp_unit, '(A)') first_file_matching_pattern
    else
        first_file_matching_pattern = ''
    endif

    close(tmp_unit, status='delete')

endfunction

subroutine read_data(ncid, varid, varname, indx, dataout)

    integer, intent(in) :: ncid, varid, indx
    character(len=*), intent(in) :: varname
    real, dimension(:, :), intent(out) :: dataout

    integer, dimension(:), allocatable :: count, start
    integer :: ndims, nx, ny, time

    call get_var_dims(ncid, varid, ndims, nx, ny, time)
    call assert(ndims == 2 .or. ndims == 3 .or. ndims == 4, 'Unsupported number of dims')

    allocate(count(ndims), start(ndims))
    nx = size(dataout, 1)
    ny = size(dataout, 2)

    ! Get data, we select a specfic time-point of data to read
    if (ndims == 2) then
        start = (/ 1, 1 /)
        count = (/ nx, ny /)
    elseif (ndims == 3) then
        start = (/ 1, 1, indx /)
        count = (/ nx, ny, 1 /)
    else
        start = (/ 1, 1, 1, indx /)
        count = (/ nx, ny, 1, 1 /)
    end if
    call ncheck(nf90_get_var(ncid, varid, dataout, start=start, count=count), &
                'Get var '//trim(varname))
    deallocate(count, start)

end subroutine read_data

! Return the spatial and time dimensions of a field.
subroutine get_var_dims(ncid, varid, ndims, nx, ny, time)
    integer, intent(in) :: ncid, varid

    integer, intent(out) :: ndims, nx, ny, time

    integer, dimension(:), allocatable :: dimids
    integer :: i, len
    character(len=nf90_max_name) :: dimname

    ! Get dimensions used by this var.
    call ncheck(nf90_inquire_variable(ncid, varid, ndims=ndims), &
                'get_var_dims: Inquire ndims')
    allocate(dimids(ndims))
    call ncheck(nf90_inquire_variable(ncid, varid, dimids=dimids), &
                'get_var_dims: Inquire dimids')

    ! Only support dimension names: time, latitude, longitude for now.
    nx = 0
    ny = 0
    time = 0
    do i=1, ndims
      call ncheck(nf90_inquire_dimension(ncid, dimids(i), &
                                         name=dimname, len=len), &
                    'get_var_dims: Inquire dimension '//dimname)
      if (trim(dimname) == 'time' .or. trim(dimname) == 'AT') then
        time = len
      elseif (trim(dimname) == 'latitude' .or. trim(dimname) == 'AY' .or. &
                trim(dimname) == 'ny') then
        ny = len
      elseif (trim(dimname) == 'longitude' .or. trim(dimname) == 'AX' .or. &
                trim(dimname) == 'nx') then
        nx = len
      else
        call assert(.false., 'get_var_dims: Unsupported dimension name '//trim(dimname))
      endif
    enddo

    deallocate(dimids)

endsubroutine get_var_dims

end module util_mod
