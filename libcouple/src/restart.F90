module restart_mod

! FIXME: rewrite using ncvar_mod

use util_mod, only : ncheck, read_data
use field_mod, only : field
use datetime_module, only : datetime
use netcdf

implicit none

private

type, public :: restart
    character(len=256) :: restart_file
contains
    procedure, pass(self), public :: init => restart_init
    procedure, pass(self), public :: get_date => restart_get_date
    procedure, pass(self), public :: write => restart_write
    procedure, pass(self), public :: read => restart_read
endtype restart

contains

subroutine restart_init(self, restart_file)

    class(restart), intent(inout) :: self
    character(len=*), intent(in) :: restart_file

    self%restart_file = trim(restart_file)

endsubroutine

! Save the atmosphere <-> ice coupling fields. This does two things:
! 1) save the datetime of the next forcing.
! 2) save the values of the next forcing. In general these aren't
! needed between atm <-> ice unless a lag is introducted.
subroutine restart_write(self, cur_date, fields)

    class(restart), intent(inout) :: self
    type(datetime), intent(in) :: cur_date
    type(field), dimension(:), intent(in) :: fields

    integer :: ncid, nx, ny, i
    integer :: time_dimid, lat_dimid, lon_dimid
    integer :: time_varid
    integer, dimension(size(fields)) :: field_varid
    integer, dimension(3) :: dimids

    if (size(fields) < 1) then
        return
    endif

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

    ! FIXME: this assumes that all fields have the same shape
    nx = size(fields(1)%data_array, 1)
    ny = size(fields(1)%data_array, 2)
    call ncheck(nf90_def_dim(ncid, 'ny', ny,  lat_dimid), 'Def dim ny')
    call ncheck(nf90_def_dim(ncid, 'nx', nx,  lon_dimid), 'Def dim nx')

    do i=1, size(fields)
        dimids(:) = (/ lon_dimid, lat_dimid, time_dimid /)
        call ncheck(nf90_def_var(ncid, trim(fields(i)%name), nf90_real, &
                    dimids, field_varid(i)), &
                    'Defining var '//trim(fields(i)%name)//' in '//trim(self%restart_file))
    enddo
    call ncheck(nf90_enddef(ncid), 'nf90_enddef for '//trim(self%restart_file))

    ! Load single time value
    call ncheck(nf90_put_var(ncid, time_varid, (/ 0 /)), &
                'nf90_put_var '//trim(self%restart_file))

    ! Load field values.
    do i=1, size(fields)
        call ncheck(nf90_put_var(ncid, field_varid(i), fields(i)%data_array), &
                    'restart_write: nf90_put_var '//trim(fields(i)%name))
    enddo

    call ncheck(nf90_close(ncid))

endsubroutine restart_write

!> Read in coupling field restart fields.
subroutine restart_read(self, fields)
    class(restart), intent(inout) :: self
    type(field), dimension(:), intent(inout) :: fields

    integer :: ncid, varid, i
    integer :: ndims, nx, ny, time

    call ncheck(nf90_open(trim(self%restart_file), NF90_NOWRITE, ncid), &
                'Opening '//trim(self%restart_file))

    do i=1, size(fields)
        call ncheck(nf90_inq_varid(ncid, trim(fields(i)%name), varid), &
                    'Inquire: '//trim(fields(i)%name)//' in '//trim(self%restart_file))
        call read_data(ncid, varid, trim(fields(i)%name), 1, fields(i)%data_array)
    enddo

    call ncheck(nf90_close(ncid), 'Closing '//self%restart_file)

end subroutine restart_read

endmodule restart_mod
