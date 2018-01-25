module forcing_mod

use error_handler, only : assert
use json_module
use datetime_module, only : datetime
use field_mod, only : field
use util, only : ncheck, get_var_dims, replace_text
use netcdf

implicit none

private

type field
    private
    character(len=64) :: name
    character(len=256) :: filename
    character(len=64) :: ncname
    type(datetime) :: timestamp
    integer :: nx, ny
    real, dimension(:, :), allocatable :: array
endtype field

type, public forcing
    private
    type(datetime) :: start_date
    integer :: period
    integer :: index_guess
    type(field), dimension(:), allocatable :: fields
contains
    procedure, pass(self), public :: init => forcing_init
    procedure, pass(self), public :: read_field => forcing_read_field
    procedure, pass(self), public :: get_name
    procedure, pass(self), public :: get_data
    procedure, pass(self), public :: get_num_fields
endtype forcing

contains

!> Parse forcing file into a dictionary.
subroutine forcing_init(self, config, start_date, period)

    class(forcing), intent(inout) :: self
    character(len=*), intent(in) :: config
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: period

    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: root, inputs, fp
    integer :: i

    self%start_date = start_date
    self%period = period
    self%index_guess = 1

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
    allocate(self%fields(n_inputs))

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

        self%fields(i)%name = trim(name)
        self%fields(i)%filename = trim(filename)
        self%fields(i)%ncname = trim(ncname)
        self%fields(i)%nx = nx
        self%fields(i)%ny = ny
        allocate(self%fields(i)%array(nx, ny))
        self%fields%timestamp = start_date
    enddo

    call core%destroy()
    call json%destroy()

endsubroutine forcing_init

subroutine forcing_update(self, cur_date)

    class(forcing), intent(in) :: self
    type(datetime), intent(in) :: cur_date

    integer :: year, indx
    character(len=256) :: filename

    ! Check whether any work needs to be done
    if (self%fields(i)%timestamp == cur_date) then
        return
    endif

    do i, size(self%fields)
        ! Find the correct file based on the current year.
        year = mod(cur_date%getYear() - start_date%getYear(), self%period)
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

        call read_data(ncid, varid, indx, varname, self%fields(i)%array)

        call ncheck(nf90_close(ncid), 'Closing '//trim(filename))

        self%fields(i)%timestamp = cur_date
    enddo

endsubroutine forcing_read_field

pure function integer get_num_fields(self)
    class(forcing_type), intent(in) :: self
    get_num_fields = size(self%fields)
endfunction

pure function character(len=64) get_name(self, idx)
    class(forcing), intent(in) :: self
    integer, intent(in) :: idx

    get_name = self%fields(idx)%name
endfunction

pure function get_data(self, idx) return(data)
    class(forcing), intent(in) :: self
    integer, intent(in) :: idx
    real, dimension(:, :), intent(out) :: data

    data = self%fields(idx)%array(:, :)
endfunction

pure function filename_for_year(filename, year) result(new_filename)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: year

    character(len=4) :: year_str

    write(year_str, "I4") year
	new_filename = replace_text(filename, "{{ year }}", year_str)
	new_filename = replace_text(filename, "{{year}}", year_str)
endfunction filename_for_year

!> Return the time index of a particular date.
! Starts looking from a guess index.
pure function integer forcing_index(ncid, target_date, guess)  result(indx)

    integer, intent(in) :: ncid, varid
    type(datetime), intent(in) :: target_date
    integer, intent(in) :: guess

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

endfunction forcing_index

endmodule forcing_mod
