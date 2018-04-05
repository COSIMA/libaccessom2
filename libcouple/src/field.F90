module field_mod

use netcdf, only : nf90_max_name
use datetime_module, only : datetime

implicit none

private

type, public :: field
    character(len=64) :: name
    character(len=256) :: filename_template
    character(len=nf90_max_name) :: ncname
    type(datetime) :: timestamp
    integer :: nx, ny
    integer :: dt
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: nc_idx_guess

    ! Cache netCDF current info for this field. Cache needs to be updated
    ! whenever the filename changes.
    character(len=256) :: filename
    integer :: ncid
    type(datetime) :: nc_start_date
    real, dimension(:,:), allocatable :: time_bnds
    real, dimension(:), allocatable :: times
    real, dimension(:, :), allocatable :: data_array
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: update_data_from_file => field_update_data_from_file
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, filename_template, ncname, nx, ny, dt)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, filename_template, ncname
    integer, intent(in) :: nx, ny
    integer, optional, intent(in) :: dt

    self%name = name
    self%filename_template = filename_template
    self%ncname = ncname
    allocate(self%data_array(nx, ny))
    self%data_array(:, :) = HUGE(1.0)
    self%timestamp = datetime(HUGE(1))
    if (present(dt)) then
        self%dt = dt
    endif

    self%nc_idx_guess = 1
    self%nc_fp = -1
    self%filename = ''

end subroutine

subroutine field_update_data_from_file(self, filename, forcing_date)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    integer :: time_id, time_bnds_id

    integer :: time_id, time_bnds_id, num_times, len
    integer :: status
    type(timedelta) :: td, td_before, td_after
    character(len=nf90_max_name) :: time_bnds_name, dimname
    integer, dimension(2) :: dimids

    real :: start_time, end_time

    if (filename /= self%filename) then
        self%filename = filename

        if (self%ncid /= -1) then
            call ncheck(nf90_close(self%ncid), 'Closing '//trim(self%filename))
        endif
        if (allocated(self%time_bnds)) then
            deallocate(self%time_bnds)
        endif
        if (allocated(self%times)) then
            deallocate(self%times)
        endif

        call ncheck(nf90_open(trim(self%filename), NF90_NOWRITE, self%ncid), &
                    'Opening '//trim(self%filename))
        call ncheck(nf90_inq_varid(self%ncid, "time", time_id), 'Inquire: time')
        call get_nc_start_date(self%ncid, time_id, self%nc_start_date)

        status = nf90_get_att(self%ncid, time_id, "bounds", time_bnds_name)
        if (status == nf90_noerr) then
            call ncheck(nf90_inq_varid(ncid, trim(time_bnds_name), time_bnds_id), &
                        'Inquire varid: '//trim(time_bnds_name))
            call ncheck(nf90_inquire_variable(ncid, time_bnds_id, dimids=dimids), &
                        'Inquire dimids '//trim(time_bnds_name))
            call ncheck(nf90_inquire_dimension(ncid, dimids(1), name=dimname, len=len), &
                        'Inquire dimension 1 '//trim(time_bnds_name))
            call assert(len == 2, 'Unexpected length for dimension '//dimname)
            call ncheck(nf90_inquire_dimension(ncid, dimids(2), name=dimname, len=num_times), &
                        'Inquire dimension 2 '//trim(time_bnds_name))

            allocate(self%time_bnds(len, num_times))
            call ncheck(nf90_get_var(self%ncid, time_bnds_id, self%time_bnds), &
                        'Get '//trim(time_bnds_name))
        else
            call ncheck(nf90_inquire_dimension(ncid, time_id, len=num_times))
            allocate(self%times(num_times))
            call ncheck(nf90_get_var(ncid, time_id, times))
        endif
    endif

    indx = self%forcing_index(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        self%nc_idx_guess = 1
        indx = self%forcing_index(forcing_date)
    endif
    call assert(indx /= -1, &
                "Could not find forcing date "//forcing_date%isoformat())
    ! Update the guess for next time.
    self%nc_idx_guess = indx + 1

    ! Get data
    call ncheck(nf90_inq_varid(self%ncid, self%ncname, varid), &
                'Inquire: '//trim(varname))
    call read_data(self%ncid, varid, varname, indx, self%data_array)

    fld%timestamp = forcing_date

end subroutine field_update_data_from_file

!> Return the time index of a particular date.
function forcing_index(self, target_date)
    class(field), intent(in) :: field
    type(datetime), intent(in) :: target_date
    integer :: forcing_index

    type(timedelta) :: td, td_before, td_after

    if (allocated(field%time_bnds)) then
        do forcing_index=self%nc_idx_guess, num_times
            td_before = timedelta(seconds=int(self%time_bnds(1, forcing_index)*86400))
            td_after = timedelta(seconds=int(self%time_bnds(2, forcing_index)*86400))
            if (target_date >= (nc_start_date + td_before) .and. &
                target_date < (nc_start_date + td_after)) then
                return
            endif
        enddo
    else
        do forcing_index=self%nc_idx_guess, num_times
            td = timedelta(seconds=int(self%times(forcing_index)*86400))
            if (target_date == (nc_start_date + td)) then
                return
            endif
        enddo
    endif

    ! The index was not found.
    forcing_index = -1

endfunction forcing_index

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
