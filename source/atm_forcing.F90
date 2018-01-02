module atm_forcing

use json_module
use dictionary
use variable
use datetime_module, only: datetime
use, intrinsic :: iso_fortran_env , only: error_unit

implicit none

public parse_forcing, get_forcing

private

type(dict) :: filename_dict
type(dict) :: fieldname_dict

contains

!> Parse forcing file into a dictionary.
subroutine parse_forcing(json_fname)

    character(len=*), intent(in) :: json_fname

    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: root, inputs

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

end subroutine parse_forcing

subroutine traverse_inputs(json, p, finished)

    class(json_core), intent(inout) :: json
    type(json_value), pointer, intent(in) :: p
    logical, intent(out) :: finished

    character(len=:), allocatable :: filename, fieldname, cname
    logical :: found

    finished = .false.

    call json%get(p, "filename", filename, found)
    call json%get(p, "fieldname", fieldname, found)
    call json%get(p, "cname", cname, found)

    if (found) then
        filename_dict = filename_dict // (trim(cname) .kv. trim(filename))
        fieldname_dict = fieldname_dict // (trim(cname) .kv. trim(fieldname))
    endif

end subroutine traverse_inputs


subroutine get_forcing(key, date, data)

    character(len=*), intent(in) :: key
    type(datetime), intent(in) :: date
    integer, intent(in) :: forcing_year
    real, dimension(:,:), intent(inout) :: data

    character(len=256) :: filename, fieldname, time_str
    type(datetime), intent(in) :: start_date, start_date_w_hours
    character(len=4) :: year

    if (.not. (trim(key) .in. filename_dict)) then
        print*, 'key not found'
    endif

	call assign(filename, filename_dict, trim(key))
	print*, 'val: ', trim(filename)

	call assign(fieldname, fieldname_dict, trim(key))
	print*, 'val: ', trim(fieldname)

    ! Find the correct file. First select the year.
	filename = replace_text(filename, "{{ year }}", forcing_year)
	filename = replace_text(filename, "{{year}}", forcing_year)

    ! Check that the file exists.

    ! Find the correct timeslice in the file.
    call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
                'Opening '//trim(filename))

    ! Get variable ID
    call ncheck(nf90_inq_varid(ncid, trim(varname), varid), &
                'Inquire: '//trim(varname))

    ! Get start date
    call ncheck(nf90_get_att(ncid, varid, "units", time_str)

    time_str = replace_text(time_str, "days since ", "")
    start_date_w_hours = strptime(trim(time_str), "%Y-%m-%d %H:%M:%S")
    start_date = strptime(trim(time_str), "%Y-%m-%d")

    indx = forcing_index(ncid, start_date, date, guess)

    call read_data()


end subroutine get_forcing

!> Take a netcdf time attribute and return a date.
subroutine get_start_date(time_str, date)

    character(len=*), intent(in) :: time_str
    type(datetime), intent(out) :: date

    date = strptime

end subroutine

!> Return the time index of a particular date.
! Starts looking from a guess index.
function forcing_index(ncid, start_date, target_date, guess)  result(indx)

    integer, intent(in) :: ncid, varid
    type(datetime), intent(in) :: start_date, target_date, tmp_date
    integer, intent(in) :: guess
    integer, intent(out) :: indx

    integer :: varid, num_times
    real, dimension(:), allocatable :: times

    ! Get variable ID
    call ncheck(nf90_inq_varid(ncid, "time", varid), 
                'Inquire: '//trim(varname))

    call ncheck(nf90_inquire_dimension(ncid, varid, len = num_times))
    allocate(times(num_times))
    call ncheck(nf90_get_var(ncid, varid, times)

    ! First start searching using 'guess'
    do indx=guess, num_times
        tmp_date = start_date + times(indx)
        if (tmp_date == target_date) then
            return
        endif
    enddo

    ! Searching from the beginning
    do indx=1, num_times
        tmp_date = start_date + times(indx)
        if (tmp_date == target_date) then
            return
        endif
    enddo

    ! The date was not found.
    indx = -1

end function forcing_index()


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
