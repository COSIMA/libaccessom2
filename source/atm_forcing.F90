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
    real, dimension(:,:), intent(inout) :: data

    character(len=256) :: filename, fieldname
    character(len=4) :: year

    if (.not. (trim(key) .in. filename_dict)) then
        print*, 'key not found'
    endif

	call assign(filename, filename_dict, trim(key))
	print*, 'val: ', trim(filename)

	call assign(fieldname, fieldname_dict, trim(key))
	print*, 'val: ', trim(fieldname)

    ! Find the correct file. First select the year.
    write(year,'(i4)') date%getYear()
	filename = replace_text(filename, "{{ year }}", year)
	filename = replace_text(filename, "{{year}}", year)

    ! Find the correct timeslice in the file.

end subroutine get_forcing

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

end module atm_forcing
