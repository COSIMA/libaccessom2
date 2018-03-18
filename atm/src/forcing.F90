module forcing_mod

use error_handler, only : assert
use json_module
use json_kinds
use datetime_module, only : datetime, timedelta
use util_mod, only : ncheck, get_var_dims, replace_text, get_nc_start_date, read_data
use netcdf
use field_mod, only : field_type => field
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none

private

type, public :: forcing
    type(datetime) :: start_date
    integer :: period
    integer :: index_guess
    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: inputs
    integer :: nfields
contains
    procedure, pass(self), public :: init => forcing_init
    procedure, pass(self), public :: update => forcing_update
endtype forcing

contains

!> Open forcing file and find fields
subroutine forcing_init(self, config, start_date, period, nfields)

    class(forcing), intent(inout) :: self
    character(len=*), intent(in) :: config
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: period
    integer, intent(out) :: nfields

    type(json_value), pointer :: root
    logical :: found

    self%start_date = start_date
    self%period = period
    self%index_guess = 1

    call self%json%initialize()
    call self%json%load_file(filename=trim(config))
    if (self%json%failed()) then
        call self%json%print_error_message(stderr)
        call assert(.false., 'forcing_init() failed')
    endif

    call self%core%initialize()
    call self%json%get(root)
    call self%core%get_child(root, "inputs", self%inputs)

    self%nfields = self%core%count(inputs)
    nfields = self%nfields

endsubroutine forcing_init

!> Parse forcing file into a dictionary.
subroutine forcing_init_fields(self, fields)

    class(forcing), intent(inout) :: self
    type(field_type), dimension(:), intent(inout) :: fields

    type(json_value), pointer :: root, fp
    integer :: ncid, varid
    integer :: nx, ny, unused
    character(kind=CK, len=:), allocatable :: cname, fieldname, filename
    character(len=64) :: fname
    logical :: found

    do i=1, size(fields)
        call core%get_child(self%inputs, i, fp, found)
        call assert(found, "Input not found in forcing config.")

        call core%get(fp, "filename", filename, found)
        call assert(found, "Entry 'filename' not found in forcing config.")

        call core%get(fp, "fieldname", fieldname, found)
        call assert(found, "Entry 'fieldname' not found in forcing config.")

        call core%get(fp, "cname", cname, found)
        call assert(found, "Entry 'cname' not found in forcing config.")

        ! Get the shape of forcing fields
        fname  = filename_for_year(filename, start_date%getYear())
        call ncheck(nf90_open(trim(fname), NF90_NOWRITE, ncid), &
                    'Opening '//trim(fname))
        call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
                    'Inquire: '//trim(fieldname))

        call get_var_dims(ncid, varid, unused, nx, ny, unused)
        call ncheck(nf90_close(ncid), 'Closing '//trim(fname))

        ! Initialise a new field object.

        fields(i)%name = trim(cname)
        fields(i)%filename = trim(filename)
        fields(i)%ncname = trim(fieldname)
        fields(i)%nx = nx
        fields(i)%ny = ny
        allocate(fields(i)%data_array(nx, ny))
        fields%timestamp = self%start_date
    enddo
endsubroutine forcing_init_fields

subroutine forcing_update_fields(self, cur_date)

    class(forcing), intent(inout) :: self
    type(datetime), intent(in) :: cur_date

    integer :: year, indx, i, ncid, varid
    character(len=256) :: filename, varname
    type(datetime) :: forcing_date

    ! Check whether any work needs to be done
    if (self%fields(i)%timestamp == cur_date) then
        return
    endif

    do i=1, size(self%fields)
        ! Find the correct file based on the current year.
        year = mod(cur_date%getYear() - self%start_date%getYear(), self%period)
        filename = filename_for_year(self%fields(i)%filename, year)

        ! Find the correct timeslice in the file.
        call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
                    'Opening '//trim(filename))
        forcing_date = datetime(year, cur_date%getMonth(), &
                                cur_date%getDay(), cur_date%getHour(), &
                                cur_date%getMinute(), cur_date%getSecond())
        indx = forcing_index(ncid, forcing_date, self%index_guess)
        call assert(indx /= -1, "Could not find forcing index")

        ! Update the guess for next time.
        self%index_guess = indx

        ! Get variable ID
        call ncheck(nf90_inq_varid(ncid, self%fields(i)%ncname, varid), &
                    'Inquire: '//trim(varname))

        call read_data(ncid, varid, varname, indx, self%fields(i)%array)

        call ncheck(nf90_close(ncid), 'Closing '//trim(filename))

        self%fields(i)%timestamp = cur_date
    enddo

endsubroutine forcing_update

function filename_for_year(filename, year)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: year
    character(len=len(filename)) :: filename_for_year

    character(len=4) :: year_str

    write(year_str, "(I4)") year
    filename_for_year = replace_text(filename, "{{ year }}", year_str)
    filename_for_year = replace_text(filename, "{{year}}", year_str)
endfunction filename_for_year

!> Return the time index of a particular date.
! Starts looking from a guess index.
function forcing_index(ncid, target_date, guess)

    integer, intent(in) :: ncid
    type(datetime), intent(in) :: target_date
    integer, intent(in) :: guess
    integer :: forcing_index

    integer :: varid, num_times
    type(datetime) :: nc_start_date
    type(timedelta) :: td
    real, dimension(:), allocatable :: times

    ! Get time variable ID
    call ncheck(nf90_inq_varid(ncid, "time", varid), 'Inquire: time')

    call get_nc_start_date(ncid, varid, nc_start_date)

    call ncheck(nf90_inquire_dimension(ncid, varid, len = num_times))
    allocate(times(num_times))
    call ncheck(nf90_get_var(ncid, varid, times))

    ! First start searching using 'guess'
    do forcing_index=guess, num_times
        td = timedelta(seconds=int(times(forcing_index)*86400))
        if ((nc_start_date + td) == target_date) then
            return
        endif
    enddo

    ! Searching from the beginning
    do forcing_index=1, num_times
        td = timedelta(seconds=int(times(forcing_index)*86400))
        if ((nc_start_date + td) == target_date) then
            return
        endif
    enddo

    ! The index was not found.
    forcing_index = -1

endfunction forcing_index

subroutine forcing_deinit(self)
    class(forcing), intent(inout) :: self

    call self%core%destroy()
    call self%json%destroy()

end subroutine forcing_deinit()

endmodule forcing_mod
