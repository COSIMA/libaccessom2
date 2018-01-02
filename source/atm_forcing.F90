module atm_forcing

use json_module
use dictionary
use variable
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


subroutine get_forcing(key, date_str, data)

    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: date_str
    real, dimension(:,:), intent(inout) :: data

    character(len=256) :: filename, fieldname

    if (.not. (trim(key) .in. filename_dict)) then
        print*, 'key not found'
    endif

	call assign(filename, filename_dict, trim(key))
	print*, 'val: ', trim(filename)

	call assign(fieldname, fieldname_dict, trim(key))
	print*, 'val: ', trim(fieldname)

end subroutine get_forcing


end module atm_forcing
