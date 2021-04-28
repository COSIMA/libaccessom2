module ncvar_mod

! Simple wrapper around a netCDF variable. Does things like caching the time
! coordinate so that it does not have to be continuously reread.

use util_mod, only : read_data, replace_text, ncheck, get_var_dims
use util_mod, only : get_time_varid_and_dimid
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
    character(len=9) :: calendar

    integer :: idx_guess

    real, dimension(:,:), allocatable :: time_bnds
    real, dimension(:), allocatable :: times
contains
    procedure, pass(self), public :: init => ncvar_init
    procedure, pass(self), public :: deinit => ncvar_deinit
    procedure, pass(self), public :: refresh => ncvar_refresh
    procedure, pass(self), public :: read_data => ncvar_read_data
    procedure, pass(self) :: get_index_for_datetime
    procedure, pass(self) :: get_start_date_and_calendar
endtype ncvar

contains

subroutine ncvar_init(self, name, filename, &
                      expect_temporal_only, expect_spatial_only)
    class(ncvar), intent(inout) :: self
    character(len=*), intent(in) :: name, filename
    logical, intent(in), optional :: expect_temporal_only
    logical, intent(in), optional :: expect_spatial_only

    logical :: temporal_only, spatial_only

    temporal_only = .false.
    spatial_only = .false.
    if (present(expect_temporal_only)) then
        temporal_only = expect_temporal_only
    endif
    if (present(expect_spatial_only)) then
        spatial_only = expect_spatial_only
    endif

    self%name = name
    self%ncid = -1
    call self%refresh(filename, temporal_only, spatial_only)

end subroutine

subroutine ncvar_refresh(self, filename, &
                         expect_temporal_only, expect_spatial_only)
    class(ncvar), intent(inout) :: self
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: expect_temporal_only
    logical, intent(in), optional :: expect_spatial_only

    character(len=nf90_max_name) :: time_bnds_name, dimname
    integer :: time_varid, time_dimid, time_bnds_id, len, ndims, num_times
    integer, dimension(2) :: dimids
    integer :: status
    logical :: temporal_only, spatial_only
    logical :: found_time_varid

    temporal_only = .false.
    spatial_only = .false.
    if (present(expect_temporal_only)) then
        temporal_only = expect_temporal_only
    endif
    if (present(expect_spatial_only)) then
        spatial_only = expect_spatial_only
    endif

    call assert(trim(self%filename) /= trim(filename), &
                'ncvar unnecessary refresh')
    call self%deinit()

    self%idx_guess = 1

    self%filename = filename
    ! Open, initialise ncid and varid
    call ncheck(nf90_open(trim(self%filename), NF90_NOWRITE, self%ncid), &
                'ncvar opening '//trim(self%filename))
    call ncheck(nf90_inq_varid(self%ncid, trim(self%name), self%varid), &
               'ncvar inquire: '//trim(self%name)//' in '//trim(self%filename))

    ! Initialise dimensions
    call get_var_dims(self%ncid, self%varid, ndims, self%nx, self%ny, num_times)
    if (.not. temporal_only) then
        call assert(self%nx /= 0 .and. self%ny /= 0, &
                    'ncvar bad spatial dimensions')
    endif

    ! Initialise and cache time variable
    call get_time_varid_and_dimid(self%ncid, time_dimid, time_varid, &
                                  found_time_varid)
    if (.not. spatial_only) then
        call assert(found_time_varid, 'ncvar bad temporal dimensions')
    else
        ! Nothing more to do if there is not time variable
        return
    endif

    call ncheck(nf90_inquire_dimension(self%ncid, time_dimid, len=num_times), &
                'ncvar inquire_dimension time in: '//trim(self%filename))
    allocate(self%times(num_times))
    call ncheck(nf90_get_var(self%ncid, time_varid, self%times), &
                'ncvar get_var time in: '//trim(self%filename))

    self%dt = int((self%times(2) - self%times(1))*86400)
    ! Initialise start date and calendar
    call self%get_start_date_and_calendar(time_varid, self%start_date, self%calendar)

    status = nf90_get_att(self%ncid, time_varid, "bounds", time_bnds_name)
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

subroutine get_start_date_and_calendar(self, time_varid, start_date, calendar)
    class(ncvar), intent(in) :: self
    integer, intent(in) :: time_varid
    type(datetime), intent(out) :: start_date
    character(len=9), intent(out) :: calendar

    character(len=256) :: time_str
    type(tm_struct) :: ctime
    integer :: rc, idx

    ! Getcalendar
    call ncheck(nf90_get_att(self%ncid, time_varid, "calendar", calendar), &
                'get_start_date_and_calendar: nf90_get_att: '//calendar)
    if (trim(calendar) == 'NOLEAP') then
        calendar = 'noleap'
    endif
    call assert(trim(calendar) == 'noleap' .or. trim(calendar) == 'gregorian', &
                'get_start_date_and_calendar: unrecognized calendar type')

    ! Get start date
    call ncheck(nf90_get_att(self%ncid, time_varid, "units", time_str), &
                'get_start_date_and_calendar: nf90_get_att: '//time_str)


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
    call assert(rc /= 0, 'strptime in get_start_date_and_calendar failed on '//time_str)
    start_date = tm2date(ctime)

endsubroutine get_start_date_and_calendar

!> Return the time index of a particular date.
function get_index_for_datetime(self, target_date, from_beginning)
    class(ncvar), intent(inout) :: self
    type(datetime), intent(in) :: target_date
    logical, optional, intent(in) :: from_beginning

    integer :: i, get_index_for_datetime
    integer :: days, seconds

    type(timedelta) :: td, td_before, td_after

    if (present(from_beginning)) then
        if (from_beginning) then
            self%idx_guess = 1
        endif
    endif

    if (allocated(self%time_bnds)) then
        do i=self%idx_guess, size(self%time_bnds, 2)
            ! Must convert to days _and_ seconds rather than just days to avoid
            ! integer overflow.
            days = int(self%time_bnds(1, i))
            seconds = int((self%time_bnds(1, i) - days)*86400)
            td_before = timedelta(days=days, seconds=seconds)

            days = int(self%time_bnds(2, i))
            seconds = int((self%time_bnds(2, i) - days)*86400)
            td_after = timedelta(days=days, seconds=seconds)

            if (target_date >= (self%start_date + td_before) .and. &
                target_date < (self%start_date + td_after)) then
                get_index_for_datetime = i
                self%idx_guess = i
                return
            endif
        enddo
    else
        do i=self%idx_guess, size(self%times)
            days = int(self%times(i))
            seconds = int((self%times(i) - days)*86400)
            td = timedelta(days=days, seconds=seconds)
            if (target_date == (self%start_date + td)) then
                get_index_for_datetime = i
                self%idx_guess = i
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
