module atm_forcing

use json_module
use dictionary
use variable
use datetime_module, only: datetime
use, intrinsic :: iso_fortran_env , only: error_unit

implicit none

public init_atm_forcing, get_atm_forcing

private

type(dict) :: filename_dict
type(dict) :: fieldname_dict
type(dict) :: fieldshape_dict
integer :: nc_index_guess

type(datetime) :: start_date

contains

!> Parse forcing file into a dictionary.
subroutine init_atm_forcing(json_fname, run_start_date)

    character(len=*), intent(in) :: json_fname

    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: root, inputs

    ! The run start date it needed to find information about the forcing.
    start_date = run_start_date

    call json%initialize()
    call json%load_file(filename=json_fname)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        return
    endif

    call core%initialize()
    call json%get(root)

    call core%get_child(root, "inputs", inputs)
    call core%traverse(inputs, traverse_inputs)

    call core%destroy()
    call json%destroy()

    nc_index_guess = 1

end subroutine init_atm_forcing

subroutine traverse_inputs(json, p, finished)

    class(json_core), intent(inout) :: json
    type(json_value), pointer, intent(in) :: p
    logical, intent(out) :: finished

    character(len=:), allocatable :: filename, fieldname, cname
    logical :: found
    integer :: ncid, varid, nx, ny, unused

    finished = .false.

    call json%get(p, "filename", filename, found)
    call json%get(p, "fieldname", fieldname, found)
    call json%get(p, "cname", cname, found)

    if (found) then
        filename_dict = filename_dict // (trim(cname) .kv. trim(filename))
        fieldname_dict = fieldname_dict // (trim(cname) .kv. trim(fieldname))

        ! Open each forcing file and get the shape of field.
        fname  = filename_for_year(filename, start_date.getYear())
        call ncheck(nf90_open(trim(fname), NF90_NOWRITE, ncid), &
                    'Opening '//trim(fname))
        call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
                    'Inquire: '//trim(fieldname))

        call get_var_dims(ncid, varid, unused, nx, ny, unused)
        fieldshape_dict = fieldshape_dict // (trim(cname) .kv. (/ nx, ny /))

        call ncheck(nf90_close(ncid), 'Closing '//trim(fname))
    endif

end subroutine traverse_inputs

!> Get information about an atmospherice forcing. 
subroutine get_atm_forcing_info(key, nx, ny)

    character(len=*), intent(in) :: key
    integer, intent(out) :: nx, ny

    integer, dimension(2) :: fieldshape

	call assign(fieldshape, fieldshape_dict, trim(key))
    nx = fieldshape(1)
    ny = fieldshape(2)

end subroutine get_atm_forcing_info(key, nx, ny)

subroutine get_atm_forcing(key, start_date, cur_date, period_in_years, data)

    character(len=*), intent(in) :: key
    type(datetime), intent(in) :: start_date, cur_date
    integer, intent(in) :: period_in_years
    real, dimension(:,:), allocatable, intent(inout) :: data

    character(len=256) :: filename, fieldname
    type(datetime) :: forcing_date
    integer :: forcing_year

    if (.not. (trim(key) .in. filename_dict)) then
        print*, 'key not found'
    endif

	call assign(filename, filename_dict, trim(key))
	call assign(fieldname, fieldname_dict, trim(key))

    ! Find the correct file. First select the year.
    forcing_year = mod(cur_date%getYear() - start_date%getYear(), period)
    filename = filename_for_year(filename, forcing_year)

    ! Find the correct timeslice in the file.
    call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
                'Opening '//trim(filename))

    forcing_date = datetime(forcing_year, cur_date%getMonth(), &
                            cur_date%getDay(), cur_date%getHour(), &
                            cur_date%getMinute(), cur_date%getSecond())
    indx = forcing_index(ncid, forcing_date, nc_index_guess)
    if (indx == -1) then
        print*, 'Could not find forcing index'
        stop
    endif

    ! Update the guess for next time.
    nc_index_guess = indx

    ! Get variable ID
    call ncheck(nf90_inq_varid(ncid, trim(varname), varid), &
                'Inquire: '//trim(varname))

    call read_data(ncid, varid, indx, varname, data)

    call ncheck(nf90_close(ncid), 'Closing '//trim(filename))

end subroutine get_atm_forcing


function filename_for_year(filename, year) result(new_filename)

    character(len=*), intent(in) :: filename
    integer, intent(in) :: year

    character(len=4) :: year_str

    write(year_str, "I4") year
	new_filename = replace_text(filename, "{{ year }}", year_str)
	new_filename = replace_text(filename, "{{year}}", year_str)

end function filename_for_year

!> Return the time index of a particular date.
! Starts looking from a guess index.
function forcing_index(ncid, target_date, guess)  result(indx)

    integer, intent(in) :: ncid, varid
    type(datetime), intent(in) :: target_date
    integer, intent(in) :: guess
    integer, intent(out) :: indx

    integer :: varid, num_times
    type(datetime) :: nc_start_date
    type(timedelta) :: td
    real, dimension(:), allocatable :: times

    ! Get time variable ID
    call ncheck(nf90_inq_varid(ncid, "time", varid), 
                'Inquire: '//trim(varname))

    call get_nc_start_date(ncid, varid, nc_start_date)

    call ncheck(nf90_inquire_dimension(ncid, varid, len = num_times))
    allocate(times(num_times))
    call ncheck(nf90_get_var(ncid, varid, times)

    ! First start searching using 'guess'
    do indx=guess, num_times
        td = timedelta(seconds=int(times(indx)*86400))
        if ((nc_start_date + td) == target_date) then
            return
        endif
    enddo

    ! Searching from the beginning
    do indx=1, num_times
        td = timedelta(seconds=int(times(indx)*86400))
        if ((nc_start_date + td) == target_date) then
            return
        endif
    enddo

    ! The index was not found.
    indx = -1

end function forcing_index

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

subroutine read_data(ncid, varid, varname, indx, data)

    integer, intent(in) :: ncid, varid, indx
    character(len=*), intent(in) :: varname
    real, dimension(:, :), allocatable, intent(out) :: data

    integer, dimension(:), allocatable :: count, start
    integer :: ndims, nx, ny, time

    call get_var_dims(ncid, varid, ndims, nx, ny, time)

    allocate(count(ndims), start(ndims))
    allocate(data(nx, ny))

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

end module atm_forcing
