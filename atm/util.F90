
subroutine ncheck(status, error_str)

    implicit none

    integer(kind=int_kind), intent(in) :: status
    character(len=*), intent(in), optional :: error_str

    if (status /= nf90_noerr) then
      write(*,'(/a)')   'MATM: error - from NetCDF library'
    if (present(error_str)) then
        write(*,'(a)')   error_str
      endif
      write(*,'(a/)')   trim(nf90_strerror(status))
      stop
    end if

end subroutine ncheck

! Return the spatial and time dimensions of a field.
subroutine get_var_dims(ncid, varid, ndims, nx, ny, time)

    integer, intent(in) :: ncid, varid
    character(len=*), intent(in) :: varname
    integer, intent(out) :: ndims, nx, ny, time

    integer, dimension(:), allocatable :: dimids
    integer :: ndims, i, len
    character(len=nf90_max_name) :: dimname

    ! Get dimensions used by this var.
    call ncheck(nf90_inquire_variable(ncid, varid, ndims=ndims))
    allocate(dimids(ndims))
    call ncheck(nf90_inquire_variable(ncid, varid, dimids=dimids))

    ! Only support dimension names: time, latitude, longitude for now.
    nx = 0
    ny = 0
    time = 0
    do i = 1,ndims
      call ncheck(nf90_inquire_dimension(ncid, dimids(i), name=dimname, len=len))
      if (trim(dimname) == 'time' .or. trim(dimname) == 'AT') then
        time = len
      elseif (trim(dimname) == 'latitude' .or. trim(dimname) == 'AY') then
        ny = len
      elseif (trim(dimname) == 'longitude' .or. trim(dimname) == 'AX') then
        nx = len
      else
        stop 'MATM get_field_dims: Unsupported dimension name'
      endif
    enddo

    deallocate(dimids)
    call ncheck(nf90_close(ncid))

    if (nx == 0 .or. ny == 0 .or. time == 0) then
      stop "MATM get_field_dims: couldn't get all dimensions"
    endif

end subroutine get_var_dims

subroutine get_nc_start_date(ncid, varid, nc_start_date)

    integer, intent(in) :: ncid, varid
    type(datetime), intent(out) :: nc_start_date

    integer :: varid
    type(datetime) :: nc_start_date_w_hours
    character(len=256) :: time_str

    ! Get start date
    call ncheck(nf90_get_att(ncid, varid, "units", time_str)

    time_str = replace_text(time_str, "days since ", "")
    nc_start_date_w_hours = strptime(trim(time_str), "%Y-%m-%d %H:%M:%S")
    nc_start_date = strptime(trim(time_str), "%Y-%m-%d")

end subroutine get_nc_start_date


subroutine read_data(ncid, varid, varname, indx, dataout)

    integer, intent(in) :: ncid, varid, indx
    character(len=*), intent(in) :: varname
    real, dimension(:, :), intent(out) :: dataout

    integer, dimension(:), allocatable :: count, start
    integer :: ndims, nx, ny, time

    call get_var_dims(ncid, varid, ndims, nx, ny, time)

    allocate(count(ndims), start(ndims))
    nx = size(data, 1)
    ny = size(data, 2)

    ! Get data, we select a specfic time-point of data to read
    if (ndims == 3) then
        start = (/ 1, 1, indx /)
        count = (/ nx, ny, 1 /)
    else
        start = (/ 1, 1, 1, indx /)
        count = (/ nx, ny, 1, 1 /)
    end if
    call ncheck(nf90_get_var(ncid, varid, data, start=start, count=count), &
                'Get var '//trim(varname))
    deallocate(count, start)

end subroutine read_data

!> Replace all occurrences of 'pattern' with 'replace' in string.
! Based on: http://fortranwiki.org/fortran/show/String_Functions
function replace_text(string, pattern, replace)  result(outs)

	character(len=*), intent(in) :: s,text,rep
	character(len(string)) :: outs
	integer             :: i, nt, nr

	outs = string ; nt = len_trim(pattern) ; nr = len_trim(replace)
	do
	   i = index(outs,pattern(:nt)) ; if (i == 0) exit
	   outs = outs(:i-1) // replace(:nr) // outs(i+nt:)
	end do

end function replace_text


