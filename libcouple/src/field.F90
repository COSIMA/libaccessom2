module field_mod

use netcdf, only : nf90_max_name
use datetime_module, only : datetime

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=256) :: filename
    character(len=nf90_max_name) :: ncname
    type(datetime) :: timestamp
    integer :: nx, ny
    integer :: dt
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: nc_idx_guess

    character(len=256) :: cur_filename
    real, dimension(:, :), allocatable :: data_array
    real, dimension(:,:), allocatable :: time_bnds
    real, dimension(:), allocatable :: times
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, filename, ncname, nx, ny, dt)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, filename, ncname
    integer, intent(in) :: nx, ny
    integer, optional, intent(in) :: dt

    self%name = name
    self%filename = filename
    self%ncname = ncname
    allocate(self%data_array(nx, ny))
    self%data_array(:, :) = HUGE(1.0)
    self%timestamp = datetime(HUGE(1))
    if (present(dt)) then
        self%dt = dt
    endif

    self%nc_idx_guess = 1

end subroutine

subroutine field_update_data_from_file(self, filename, forcing_date)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    ! Find the correct timeslice in the file.
    call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
                'Opening '//trim(filename))
    indx = fld%forcing_index(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        fld%nc_idx_guess = 1
        indx = forcing_index(ncid, forcing_date, fld%nc_idx_guess)
    endif

    call assert(indx /= -1, &
                "Could not find forcing date "//forcing_date%isoformat())
    ! Update the guess for next time.
    fld%nc_idx_guess = indx + 1

    ! Get data

    call cpu_time(start_time)
    call ncheck(nf90_inq_varid(ncid, fld%ncname, varid), &
                'Inquire: '//trim(varname))
    call read_data(ncid, varid, varname, indx, fld%data_array)
    call cpu_time(end_time)
    print*, 'forcing time read_data took ', end_time - start_time
    call ncheck(nf90_close(ncid), 'Closing '//trim(filename))

    fld%timestamp = forcing_date

end subroutine field_update_data_from_file


!> Return the time index of a particular date.
! Starts looking from a guess index.
function forcing_index(field, target_date, guess)
    type(field_type), intent(in) :: field
    type(datetime), intent(in) :: target_date
    integer, intent(in) :: guess
    integer :: forcing_index

    type(timedelta) :: td, td_before, td_after

    if (allocated(field%time_bnds)) then
        do forcing_index=guess, num_times
            td_before = timedelta(seconds=int(field%time_bnds(1, forcing_index)*86400))
            td_after = timedelta(seconds=int(field%time_bnds(2, forcing_index)*86400))
            if (target_date >= (nc_start_date + td_before) .and. &
                target_date < (nc_start_date + td_after)) then
                deallocate(time_bnds)
                return
            endif
        enddo
        deallocate(time_bnds)
    else
        do forcing_index=guess, num_times
            td = timedelta(seconds=int(field%times(forcing_index)*86400))
            if (target_date == (nc_start_date + td)) then
                return
            endif
        enddo
    endif

    ! The index was not found.
    forcing_index = -1

endfunction forcing_index


subroutine load_time_coords(self, ncid, filename)
    class(field), intent(inout) :: self
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: filename

    integer :: time_id, time_bnds_id, num_times, len
    integer :: status
    type(datetime) :: nc_start_date
    type(timedelta) :: td, td_before, td_after
    character(len=nf90_max_name) :: time_bnds_name, dimname
    integer, dimension(2) :: dimids
    real, dimension(:), allocatable :: times
    real, dimension(:, :), allocatable :: time_bnds

    real :: start_time, end_time

    call ncheck(nf90_inq_varid(ncid, "time", time_id), 'Inquire: time')
    call get_nc_start_date(ncid, time_id, nc_start_date)

    status = nf90_get_att(ncid, time_id, "bounds", time_bnds_name)
    if (status == nf90_noerr) then
        ! In this case find the time bounds index which the
        ! target date falls within.
        call cpu_time(start_time)
        call ncheck(nf90_inq_varid(ncid, trim(time_bnds_name), time_bnds_id), &
                    'Inquire varid: '//trim(time_bnds_name))
        call ncheck(nf90_inquire_variable(ncid, time_bnds_id, dimids=dimids), &
                    'Inquire dimids '//trim(time_bnds_name))
        call ncheck(nf90_inquire_dimension(ncid, dimids(1), name=dimname, len=len), &
                    'Inquire dimension 1 '//trim(time_bnds_name))
        call assert(len == 2, 'Unexpected length for dimension '//dimname)
        call ncheck(nf90_inquire_dimension(ncid, dimids(2), name=dimname, len=num_times), &
                    'Inquire dimension 2 '//trim(time_bnds_name))
        call cpu_time(end_time)
        print*, 'forcing inquire took ', end_time - start_time

        allocate(time_bnds(len, num_times))
        call cpu_time(start_time)
        call ncheck(nf90_get_var(ncid, time_bnds_id, time_bnds), &
                    'Get '//trim(time_bnds_name))
        call cpu_time(end_time)
        print*, 'forcing time read took ', end_time - start_time
    else
        ! In this case return index where there is an exact match between
        ! target_date and a date in the time coordinate.
        call ncheck(nf90_inquire_dimension(ncid, time_id, len=num_times))
        allocate(times(num_times))
        call ncheck(nf90_get_var(ncid, time_id, times))
    endif

end subroutine load_time_coords

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
