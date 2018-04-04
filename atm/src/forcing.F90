module forcing_mod

use error_handler, only : assert
use json_module
use json_kinds
use datetime_module, only : datetime, timedelta
use util_mod, only : ncheck, get_var_dims, replace_text, get_nc_start_date, get_var_dt, read_data
use util_mod, only : first_file_matching_pattern
use netcdf
use field_mod, only : field_type => field
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none

private

type, public :: forcing
    type(datetime) :: start_date
    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: inputs
contains
    procedure, pass(self), public :: init => forcing_init
    procedure, pass(self), public :: deinit => forcing_deinit
    procedure, pass(self), public :: init_fields => forcing_init_fields
    procedure, pass(self), public :: update_field => forcing_update_field
endtype forcing

contains

!> Open forcing file and find fields
subroutine forcing_init(self, config, start_date, nfields)

    class(forcing), intent(inout) :: self
    character(len=*), intent(in) :: config
    type(datetime), intent(in) :: start_date
    integer, intent(out) :: nfields

    type(json_value), pointer :: root

    self%start_date = start_date

    call self%json%initialize()
    call self%json%load_file(filename=trim(config))
    if (self%json%failed()) then
        call self%json%print_error_message(stderr)
        call assert(.false., 'forcing_init() failed')
    endif

    call self%core%initialize()
    call self%json%get(root)
    call self%core%get_child(root, "inputs", self%inputs)

    nfields = self%core%count(self%inputs)

endsubroutine forcing_init

!> Parse forcing file into a dictionary.
subroutine forcing_init_fields(self, fields, min_dt)

    class(forcing), intent(inout) :: self
    type(field_type), dimension(:), intent(inout) :: fields
    integer, intent(out) :: min_dt

    type(json_value), pointer :: fp
    integer :: ncid, varid
    integer :: nx, ny, ndims, time, i
    character(kind=CK, len=:), allocatable :: cname, fieldname, filename
    character(len=1024) :: fname
    logical :: found

    min_dt = huge(min_dt)
    do i=1, size(fields)
        call self%core%get_child(self%inputs, i, fp, found)
        call assert(found, "Input not found in forcing config.")

        call self%core%get(fp, "filename", filename, found)
        call assert(found, "Entry 'filename' not found in forcing config.")

        call self%core%get(fp, "fieldname", fieldname, found)
        call assert(found, "Entry 'fieldname' not found in forcing config.")

        call self%core%get(fp, "cname", cname, found)
        call assert(found, "Entry 'cname' not found in forcing config.")

        ! Get the shape of forcing fields
        fname  = filename_for_year(filename, self%start_date%getYear())
        call ncheck(nf90_open(trim(fname), NF90_NOWRITE, ncid), &
                    'Opening '//trim(fname))
        call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
                    'Inquire: '//trim(fieldname))

        call get_var_dims(ncid, varid, ndims, nx, ny, time)
        call assert(nx /= 0 .and. ny /= 0, 'Bad var dimensions')

        ! Initialise a new field object.
        call fields(i)%init(trim(cname), trim(filename), trim(fieldname), &
                            nx, ny, get_var_dt(ncid))

        if (fields(i)%dt < min_dt) then
            min_dt = fields(i)%dt
        endif

        call ncheck(nf90_close(ncid), 'Closing '//trim(fname))
    enddo

endsubroutine forcing_init_fields

subroutine forcing_update_field(self, forcing_date, fld, debug_output)

    class(forcing), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date
    type(field_type), intent(inout) :: fld
    logical, optional, intent(in) :: debug_output

    integer :: indx, ncid, varid
    character(len=1024) :: filename, varname
    real :: start_time, end_time

    ! Check whether any work needs to be done
    if (fld%timestamp == forcing_date) then
        return
    endif

    if (present(debug_output)) then
        if (debug_output) then
            print*, 'forcing_update_field at '//forcing_date%isoformat()
        endif
    endif

    filename = filename_for_year(fld%filename, forcing_date%getYear())
    call assert(trim(filename) /= '', "File not found: "//fld%filename)

    fld%update_data_from_file(filename, forcing_date)

endsubroutine forcing_update_field

function filename_for_year(filename, year)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: year
    character(len=1024) :: filename_for_year

    character(len=1024) :: with_year_replaced
    character(len=4) :: year_str

    write(year_str, "(I4)") year
    with_year_replaced = replace_text(filename, "{{ year }}", year_str)
    with_year_replaced = replace_text(with_year_replaced, "{{year}}", year_str)
    filename_for_year = first_file_matching_pattern(with_year_replaced)
    if (trim(filename_for_year) == '') then
        filename_for_year = with_year_replaced
    endif
endfunction filename_for_year

subroutine forcing_deinit(self)
    class(forcing), intent(inout) :: self

    call self%core%destroy()
    call self%json%destroy()

end subroutine forcing_deinit

endmodule forcing_mod
