module forcing_mod

use error_handler, only : assert
use json_module
use datetime_module, only: datetime
use field, only: init, update

implicit none

private

type forcing_field_type
    character(len=64) :: name
    character(len=64) :: ncname
    character(len=256) :: filename
    integer :: nx, ny
end type forcing_field_type

type, public forcing_type
    private
    type(datetime) :: start_date
    integer :: period
    class(forcing_field_type), dimension(:), allocatable :: fields
contains
    procedure, public :: init => forcing_init
    procedure, public :: update => forcing_update
    procedure, public :: num_fields
end type forcing_type

contains

!> Parse forcing file into a dictionary.
subroutine forcing_init(this, config, start_date, period)

    class(forcing_type), intent(inout) :: this
    character(len=*), intent(in) :: config
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: period

    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: root, inputs, fp
    integer :: i

    this%start_date = start_date
    this%period = period
    this%index_guess = 1

    call json%initialize()
    call json%load_file(filename=config)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        call assert(.false., 'forcing_init() failed')
    endif

    call core%initialize()
    call json%get(root)
    call core%get_child(root, "inputs", inputs)

    n_inputs = core%count(inputs)
    allocate(fields(n_inputs))

    do i, n_inputs
        call core%get_child(inputs, i, fp, found)
        call assert(found, "Input not found in forcing config.")

        call json%get(fp, "filename", filename, found)
        call assert(found, "Entry 'filename' not found in forcing config.")

        call json%get(fp, "fieldname", fieldname, found)
        call assert(found, "Entry 'fieldname' not found in forcing config.")

        call json%get(fp, "cname", cname, found)
        call assert(found, "Entry 'cname' not found in forcing config.")

        ! Get the shape of forcing fields
        fname  = filename_for_year(filename, start_date.getYear())
        call ncheck(nf90_open(trim(fname), NF90_NOWRITE, ncid), &
                    'Opening '//trim(fname))
        call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
                    'Inquire: '//trim(fieldname))

        call get_var_dims(ncid, varid, unused, nx, ny, unused)
        call ncheck(nf90_close(ncid), 'Closing '//trim(fname))

        ! Initialise a new field object.
        fields(i)%init(cname, filename, fieldname, nx, ny)
    enddo

    call core%destroy()
    call json%destroy()

end subroutine forcing_init

function field_get_filename(this)
end function field_get_filename

function filename_for_year(filename, year) result(new_filename)

    character(len=*), intent(in) :: filename
    integer, intent(in) :: year

    character(len=4) :: year_str

    write(year_str, "I4") year
	new_filename = replace_text(filename, "{{ year }}", year_str)
	new_filename = replace_text(filename, "{{year}}", year_str)

end function filename_for_year

subroutine forcing_update(this, cur_date)

    class(forcing_type), intent(inout) :: this
    type(datetime), intent(in) :: cur_date

    integer :: i

    do i, size(this%fields)

        ! Find the correct file based on the current year.
        year = mod(cur_date%getYear() - start_date%getYear(), this%period)
        filename = filename_for_year(this%fields(i)%get_filename(), year)

        ! Find the correct timeslice in the file.
        call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
                    'Opening '//trim(filename))
        forcing_date = datetime(year, cur_date%getMonth(), &
                                cur_date%getDay(), cur_date%getHour(), &
                                cur_date%getMinute(), cur_date%getSecond())
        indx = forcing_index(ncid, forcing_date, this%index_guess)
        call assert(indx /= -1, "Could not find forcing index")

        ! Update the guess for next time.
        this%index_guess = indx

        ! Get variable ID
        call ncheck(nf90_inq_varid(ncid, this%fields(i)%get_ncname()), varid), &
                    'Inquire: '//trim(varname))

        call read_data(ncid, varid, indx, varname, this%fields(i)%array)

        call ncheck(nf90_close(ncid), 'Closing '//trim(filename))
    enddo

end subroutine forcing_update

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
    call ncheck(nf90_inq_varid(ncid, "time", varid), 'Inquire: time')

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

function get_name()
end function

function get_shape()
end function

end module forcing_mod
