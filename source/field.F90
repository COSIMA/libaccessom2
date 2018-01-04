
module field

use error_handler , only : assert
use datetime_module, only: datetime

implicit none

private
public init, update, set_id

type field_type
    private
    integer :: oasis_id
    character(len=64) :: name
    character(len=64) :: ncname
    character(len=256) :: filename
    type(datetime) :: timestamp
    real, dimension(:,:), allocatable :: array
contains
    public     
    procedure :: init => field_init
    procedure :: update => field_update
    procedure :: set_id => field_set_id
    procedure :: get_filename => field_get_filename
end type field_type

contains

subroutine field_init(this, name, filename, nc_name, nx, ny)

    class(field_type), intent(inout) :: this
    character(len=*), intent(in) :: name, filename, nc_name
    integer, intent(in) :: nx, ny

    this%name = trim(name)
    this%nc_name = trim(nc_name)
    this%filename = trim(filename)

    call assert(.not. allocated(this%array), 'Field data already allocated')
    allocate(this%array(nx, ny))

end subroutine field_init

subroutine field_update(this, date) 

    class(field_type), intent(inout) :: this
    type(datetime), intent(in) :: date

    ! Update the data. 
    this%timestamp = date

end subroutine field_update

subroutine field_set_id(this, id)

    class(field_type), intent(inout) :: this
    integer, intent(in) :: id

    this%id = id
 
end subroutine field_set_id

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
    call ncheck(nf90_inq_varid(ncid, "time", varid), &
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





end module field
