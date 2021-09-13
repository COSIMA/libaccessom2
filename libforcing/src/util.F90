module util_mod

use netcdf
use datetime_module, only : datetime
use error_handler, only : assert
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none

integer, dimension(12), parameter, private :: DAYS_IN_MONTH = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

contains

subroutine ncheck(status, error_str)

    integer, intent(in) :: status
    character(len=*), intent(in) :: error_str

    if (status /= nf90_noerr) then
        write(stderr, '(/a)') 'Error - from NetCDF library'
        write(stderr, '(a)') error_str
        write(stderr, '(a/)')   trim(nf90_strerror(status))
        stop
    end if

end subroutine ncheck

!> Replace all occurrences of 'pattern' with 'replace' in string.
! Based on: http://fortranwiki.org/fortran/show/String_Functions
! BUG: endless loop if replace contains pattern
function replace_text(string, pattern, replace)  result(outs)

    character(len=*), intent(in) :: string, pattern, replace
    character(len(string)+100) :: outs ! provide 100 extra char - BUG: may be too few in some cases
    integer             :: i, nt, nr

    outs = string ; nt = len_trim(pattern) ; nr = len_trim(replace)
    do
        i = index(outs,pattern(:nt)) ; if (i == 0) exit
        outs = outs(:i-1) // replace(:nr) // outs(i+nt:)
    end do

end function replace_text


subroutine read_data(ncid, varid, varname, indx, dataout)

    integer, intent(in) :: ncid, varid, indx
    character(len=*), intent(in) :: varname
    real, dimension(:, :), intent(out) :: dataout

    integer, dimension(:), allocatable :: count, start
    real, dimension(1) :: scalar_dataout
    integer :: ndims, nx, ny, time

    call get_var_dims(ncid, varid, ndims, nx, ny, time)
    call assert(ndims == 1 .or. ndims == 2 .or. ndims == 3 .or. ndims == 4, &
                'Unsupported number of dims')

    allocate(count(ndims), start(ndims))
    nx = size(dataout, 1)
    ny = size(dataout, 2)

    ! Get data, we select a specfic time-point of data to read
    if (ndims == 1) then
        start = (/ indx /)
        count = (/ 1 /)

        call ncheck(nf90_get_var(ncid, varid, scalar_dataout, start=start, &
                                 count=count), &
                    'Get var '//trim(varname))
        dataout(:, :) = scalar_dataout(1)
    else
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
    endif

    deallocate(count, start)

end subroutine read_data

!> Try a number of different names to get the 'time' varid and dimid.
subroutine get_time_varid_and_dimid(ncid, dimid, varid, found)
    integer, intent(in) :: ncid
    integer, intent(out) :: dimid, varid
    logical, intent(out) :: found

    integer :: i, status
    character(len=4), dimension(4) :: names

    names(1) = 'time'
    names(2) = 'TIME'
    names(3) = 'AT'
    names(4) = 'Time'

    do i=1, 4
        status = nf90_inq_dimid(ncid, trim(names(i)), dimid)
        if (status == nf90_noerr) then
            exit
        endif
    enddo

    if (status == nf90_noerr) then
        status = nf90_inq_varid(ncid, trim(names(i)), varid)
        if (status == nf90_noerr) then
            found = .true.
        else
            found = .false.
        endif
    else
        found = .false.
    endif

end subroutine get_time_varid_and_dimid

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
      if (trim(dimname) == 'time' .or. trim(dimname) == 'AT' .or. &
          trim(dimname) == 'TIME' .or. trim(dimname) == 'Time') then
        time = len
      elseif (trim(dimname) == 'latitude' .or. trim(dimname) == 'AY' .or. &
                trim(dimname) == 'ny' .or. trim(dimname) == 'LAT' .or. &
                trim(dimname) == 'lat' .or. trim(dimname) == 'nj') then
        ny = len
      elseif (trim(dimname) == 'longitude' .or. trim(dimname) == 'AX' .or. &
                trim(dimname) == 'nx' .or. trim(dimname) == 'LON' .or. &
                trim(dimname) == 'lon' .or. trim(dimname) == 'ni') then
        nx = len
      else
        call assert(.false., 'get_var_dims: Unsupported dimension name '//trim(dimname))
      endif
    enddo

    deallocate(dimids)

endsubroutine get_var_dims

!> Search for a filename that contains year, month, start_day and end_day
! substrings. This is very specifically designed to handle the kinds
! of filenames used for JRA55 and ERA5 atmospheric forcings.

function filename_for_date(filename_template, date)
    character(len=*), intent(in) :: filename_template
    type(datetime), intent(in) :: date

    integer :: year, month, start_day, end_day
    character(len=1024) :: filename_for_date
    character(len=4) :: year_str, yearp1_str
    character(len=2) :: month_str, start_day_str, end_day_str

    year = date%getYear()
    month = date%getMonth()

    write(year_str, "(I4)") year
    write(yearp1_str, "(I4)") year+1
    write(month_str, "(I2)") month

    start_day = 1
    end_day = DAYS_IN_MONTH(month)
    write(start_day_str, "(I2)") start_day
    write(end_day_str, "(I2)") end_day

    filename_for_date = replace_text(filename_template, &
                                            "{{ year }}", year_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{year}}", year_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{ year+1 }}", yearp1_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{year+1}}", yearp1_str)

    filename_for_date = replace_text(filename_for_date, &
                                            "{{ month }}", month_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{month}}", month_str)

    filename_for_date = replace_text(filename_for_date, &
                                            "{{ start_day }}", start_day_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{start_day}}", start_day_str)

    filename_for_date = replace_text(filename_for_date, &
                                            "{{ end_day }}", end_day_str)
    filename_for_date = replace_text(filename_for_date, &
                                            "{{end_day}}", end_day_str)

endfunction filename_for_date


end module util_mod
