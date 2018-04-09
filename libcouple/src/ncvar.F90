module ncvar_mod

! Simple wrapper around a netCDF variable. Does things like caching the time
! coordinate so that it does not have to be continuously reread.

use util_mod, only : read_data, replace_text, ncheck, get_var_dims
use netcdf
use datetime_module, only : datetime, timedelta, c_strptime, tm2date, tm_struct
use error_handler, only : assert

implicit none

private

type, public :: ncvar
    character(len=nf90_max_name) :: name
    character(len=1024) :: filename
    integer :: varid, ncid
    type(datetime) :: start_date
    integer :: nx, ny
    integer :: dt

    integer :: idx_guess

    real, dimension(:,:), allocatable :: time_bnds
    real, dimension(:), allocatable :: times
contains
    procedure, pass(self), public :: init => ncvar_init
    procedure, pass(self), public :: deinit => ncvar_deinit
    procedure, pass(self), public :: refresh => ncvar_refresh
    procedure, pass(self), public :: read_data => ncvar_read_data
    procedure, pass(self) :: get_index_for_datetime
    procedure, pass(self) :: get_start_date
endtype ncvar

contains

subroutine ncvar_init(self, name, filename)
    class(ncvar), intent(inout) :: self
    character(len=*), intent(in) :: name, filename

    self%name = name
    self%ncid = -1
    call self%refresh(filename)

end subroutine

subroutine ncvar_refresh(self, filename)
    class(ncvar), intent(inout) :: self
    character(len=*), intent(in) :: filename

    character(len=nf90_max_name) :: time_bnds_name, dimname
    integer :: time_id, time_bnds_id, len, ndims, num_times
    integer, dimension(2) :: dimids
    integer :: status

    call assert(trim(self%filename) /= trim(filename), &
                'ncvar unnecessary refresh')
    call self%deinit()

    self%idx_guess = 1

    self%filename = filename
    ! Open, initialise ncid and varid
    call ncheck(nf90_open(trim(self%filename), NF90_NOWRITE, self%ncid), &
                'ncvar opening '//trim(self%filename))
    call ncheck(nf90_inq_varid(self%ncid, trim(self%name), self%varid), &
               'ncvar inquire: '//trim(self%name))

    ! Initialise dimensions
    call get_var_dims(self%ncid, self%varid, ndims, self%nx, self%ny, num_times)
    call assert(self%nx /= 0 .and. self%ny /= 0, 'ncvar bad dimensions')

    ! Initialise and cache time variable
    ! Warning: this assumes that there is only one time coordinate per file.
    call ncheck(nf90_inq_varid(self%ncid, "time", time_id), &
                'ncvar inquire: time')
    call ncheck(nf90_inquire_dimension(self%ncid, time_id, len=num_times))
    allocate(self%times(num_times))
    call ncheck(nf90_get_var(self%ncid, time_id, self%times))

    self%dt = int((self%times(2) - self%times(1))*86400)
    ! Initialise start date
    self%start_date = self%get_start_date(time_id)


    status = nf90_get_att(self%ncid, time_id, "bounds", time_bnds_name)
    if (status == nf90_noerr) then
        call ncheck(nf90_inq_varid(self%ncid, trim(time_bnds_name), &
                                   time_bnds_id), &
                    'ncvar inquire varid: '//trim(time_bnds_name))
        call ncheck(nf90_inquire_variable(self%ncid, time_bnds_id, &
                                          dimids=dimids), &
                    'ncvar inquire dimids '//trim(time_bnds_name))
        call ncheck(nf90_inquire_dimension(self%ncid, dimids(1), name=dimname, &
                                           len=len), &
                    'ncvar inquire dimension 1 '//trim(time_bnds_name))
        call assert(len == 2, 'Unexpected length for dimension '//dimname)
        call ncheck(nf90_inquire_dimension(self%ncid, dimids(2), &
                                           name=dimname, len=num_times), &
                    'ncvar inquire dimension 2 '//trim(time_bnds_name))

        allocate(self%time_bnds(len, num_times))
        call ncheck(nf90_get_var(self%ncid, time_bnds_id, self%time_bnds), &
                    'Get '//trim(time_bnds_name))
    endif

endsubroutine


function get_start_date(self, time_varid)
    class(ncvar), intent(in) :: self
    integer :: time_varid
    type(datetime) :: get_start_date

    character(len=256) :: time_str
    type(tm_struct) :: ctime
    integer :: rc, idx

    ! Get start date
    call ncheck(nf90_get_att(self%ncid, time_varid, "units", time_str), &
                'get_start_date: nf90_get_att: '//time_str)

    ! See whether it has the expected format
    idx = index(trim(time_str), "days since")
    call assert(idx > 0, "ncvar invalid time format")

    time_str = replace_text(time_str, "days since ", "")
    ! See whether we have hours
    idx = index(time_str, ":")
    if (idx > 0) then
        rc = c_strptime(trim(time_str), "%Y-%m-%d %H:%M:%S"//char(0), ctime)
    else
        rc = c_strptime(trim(time_str)//" 00:00:00", "%Y-%m-%d %H:%M:%S"//char(0), ctime)
    endif
    call assert(rc /= 0, 'strptime in get_start_date failed on '//time_str)
    get_start_date = tm2date(ctime)

endfunction get_start_date

!> Return the time index of a particular date.
function get_index_for_datetime(self, target_date, from_beginning)
    class(ncvar), intent(inout) :: self
    type(datetime), intent(in) :: target_date
    logical, optional, intent(in) :: from_beginning

    integer :: i, get_index_for_datetime

    type(timedelta) :: td, td_before, td_after

    if (present(from_beginning)) then
        if (from_beginning) then
            self%idx_guess = 1
        endif
    endif

    if (allocated(self%time_bnds)) then
        do i=self%idx_guess, size(self%time_bnds)
            td_before = timedelta(seconds=int(self%time_bnds(1, i)*86400))
            td_after = timedelta(seconds=int(self%time_bnds(2, i)*86400))
            if (target_date >= (self%start_date + td_before) .and. &
                target_date < (self%start_date + td_after)) then
                get_index_for_datetime = i
                return
            endif
        enddo
    else
        do i=self%idx_guess, size(self%times)
            td = timedelta(seconds=int(self%times(i)*86400))
            if (target_date == (self%start_date + td)) then
                get_index_for_datetime = i
                return
            endif
        enddo
    endif

    ! The index was not found.
    get_index_for_datetime = -1

endfunction get_index_for_datetime

subroutine ncvar_read_data(self, indx, dataout)
    class(ncvar), intent(inout) :: self
    integer, intent(in) :: indx
    real, dimension(:, :), intent(inout) :: dataout

    call read_data(self%ncid, self%varid, self%name, indx, dataout)

end subroutine ncvar_read_data

subroutine ncvar_deinit(self)
    class(ncvar), intent(inout) :: self

    if (allocated(self%time_bnds)) then
        deallocate(self%time_bnds)
    endif
    if (allocated(self%times)) then
        deallocate(self%times)
    endif
    if (self%ncid /= -1) then
        call ncheck(nf90_close(self%ncid), 'ncvar closing '//trim(self%filename))
    endif

endsubroutine ncvar_deinit

endmodule ncvar_mod
