module util_mod

use netcdf
use error_handler, only : assert
use datetime_module, only : datetime, timedelta, c_strptime, tm2date, tm_struct
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
      call ncheck(nf90_inquire_dimension(ncid, dimids(i), name=dimname, len=len), &
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
        call assert(.false., 'get_field_dims: Unsupported dimension name '//trim(dimname))
      endif
    enddo

    deallocate(dimids)

end subroutine get_var_dims

subroutine get_nc_start_date(ncid, varid, nc_start_date)

    integer, intent(in) :: ncid, varid
    type(datetime), intent(out) :: nc_start_date

    character(len=256) :: time_str
    type(tm_struct) :: ctime
    integer :: rc, idx

    ! Get start date
    call ncheck(nf90_get_att(ncid, varid, "units", time_str), &
                'get_nc_start_date: nf90_get_att: '//time_str)

    ! See whether it has the expected format
    idx = index(time_str, "days since")
    call assert(idx > 0, "Invald time format")

    time_str = replace_text(time_str, "days since ", "")
    ! See whether we have hours
    idx = index(time_str, ":")
    if (idx > 0) then
        rc = c_strptime(trim(time_str), "%Y-%m-%d %H:%M:%S"//char(0), ctime)
    else
        rc = c_strptime(trim(time_str)//" 00:00:00", "%Y-%m-%d %H:%M:%S"//char(0), ctime)
    endif
    call assert(rc /= 0, 'strptime in get_nc_start_date failed on '//time_str)
    nc_start_date = tm2date(ctime)

end subroutine get_nc_start_date

function get_var_dt(ncid)
    integer, intent(in) :: ncid
    integer :: get_var_dt

    integer :: num_times, varid, idx
    real, dimension(:), allocatable :: times
    character(len=256) :: time_str

    ! Calculate dt by looking at the time coordinate.
    ! FIXME: this assumes that there is only one time coordinate per file.
    call ncheck(nf90_inq_varid(ncid, 'time', varid), &
                'get_var_dt: Inquire time')

    call ncheck(nf90_inquire_dimension(ncid, varid, len = num_times))
    call ncheck(nf90_get_att(ncid, varid, "units", time_str))
    ! See whether it has the expected format
    idx = index(time_str, "days since")
    call assert(idx > 0, "Invald time format")
    call assert(num_times > 1, "Can't determine dt")

    allocate(times(num_times))
    call ncheck(nf90_get_var(ncid, varid, times), &
                 'get_var_dt: nf90_get_var')

    get_var_dt = (times(2) - times(1))*86400

endfunction get_var_dt

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

function timedelta_in_seconds(start_date, end_date, calendar)
    type(datetime), intent(in) :: start_date, end_date
    character(len=6), optional, intent(in) :: calendar

    integer :: timedelta_in_seconds
    type(timedelta) :: td

    td = end_date - start_date
    timedelta_in_seconds = td%total_seconds()

endfunction timedelta_in_seconds

end module util_mod
