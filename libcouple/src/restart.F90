module restart_mod

use util_mod, only : get_nc_start_date, ncheck
use forcing_mod, only : field
use datetime_module, only : datetime
use netcdf

implicit none

private

type, public :: restart
    type(datetime) :: restart_date
    character(len=256) :: restart_file
contains
    procedure, pass(self), public :: init => restart_init
    procedure, pass(self), public :: get_cur_date => restart_get_cur_date
    procedure, pass(self), public :: write => restart_write
endtype restart

contains

subroutine restart_init(self, start_date, restart_file)

    class(restart), intent(inout) :: self
    type(datetime), intent(in) :: start_date
    character(len=*), intent(in) :: restart_file

    self%restart_date = start_date
    self%restart_file = trim(restart_file)

endsubroutine

!>
! Read restart file and get current date
type(datetime) function restart_get_cur_date(self) result(cur_date)

    class(restart), intent(in) :: self

    integer :: ncid, varid, status

    cur_date = self%restart_date

    ! If restart file exists then read date
    status = nf90_open(trim(self%restart_file), NF90_NOWRITE, ncid)
    if (status == nf90_noerr) then
        call ncheck(nf90_inq_varid(ncid, "time", varid), 'Inquire: time')
        call get_nc_start_date(ncid, varid, cur_date)
    endif

endfunction restart_get_cur_date

! Save the atmosphere <-> ice coupling fields. This does two things:
! 1) save the datetime of the next forcing.
! 2) save the values of the next forcing. In general these aren't
! needed between atm <-> ice unless a lag is introducted.
subroutine restart_write(self, cur_date, fields)

    class(restart), intent(inout) :: self
    type(datetime), intent(in) :: cur_date
    type(field), dimension(:), intent(in) :: fields

    integer :: ncid, time_dimid, time_varid, nx, ny, i
    integer, dimension(size(fields)) :: lat_dimid, lon_dimid, field_varid
    integer, dimension(3) :: dimids

    ! Create file, time dim and var, lat and lon dims.
    call ncheck(nf90_create(trim(self%restart_file), nf90_write, ncid), &
                'Creating: '//trim(self%restart_file))
    call ncheck(nf90_def_dim(ncid, 'time', nf90_unlimited, time_dimid), &
                'Defining time dim '//trim(self%restart_file))
    call ncheck(nf90_def_var(ncid, 'time', nf90_real, (/ time_dimid /), time_varid), &
                'Defining time var '//trim(self%restart_file))
    call ncheck(nf90_put_att(ncid, time_varid, 'units', &
                        'days since '//cur_date%strftime("%Y-%m-%d %H:%M:%S")), &
                'Adding attribute units to time '//trim(self%restart_file))

    do i=1, size(fields)
        nx = size(fields(i)%array, 1)
        ny = size(fields(i)%array, 2)

        call ncheck(nf90_def_dim(ncid, 'ny_'//trim(fields(i)%name), ny,  lat_dimid(i)), &
                    'Def dim '//'ny_'//trim(fields(i)%name))
        call ncheck(nf90_def_dim(ncid, 'nx_'//trim(fields(i)%name), nx,  lon_dimid(i)), &
                    'Def dim '//'nx_'//trim(fields(i)%name))

        dimids(:) = (/ lon_dimid(i), lat_dimid(i), time_dimid /)
        call ncheck(nf90_def_var(ncid, fields(i)%name, nf90_real, dimids, field_varid(i)), &
                    'Defining var '//trim(self%restart_file))


    enddo
    call ncheck(nf90_enddef(ncid), 'nf90_enddef for '//trim(self%restart_file))

    ! Load single time value
    call ncheck(nf90_put_var(ncid, time_varid, (/ 0 /)), &
                'nf90_put_var '//trim(self%restart_file))

    ! Load field values.
    do i=1, size(fields)
        call ncheck(nf90_put_var(ncid, field_varid(i), fields(i)%array))
    enddo

    call ncheck(nf90_close(ncid))

endsubroutine restart_write

endmodule restart_mod
