module restart_mod

implicit none

use util_mod, only : get_nc_start_date
use field_mod, only : field

private

type, public restart
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

    self%restart_date = start_date
    self%restart_file = restart_file

endsubroutine

!>
! Read restart file and get current date
type(datetime) function restart_get_cur_date(self) result(cur_date)

    class(restart), intent(in) :: self

    integer :: ncid, varid

    cur_date = self%start_date

    ! If restart file exists then read date
    status = nf90_open(trim(this%restart_file), NF90_NOWRITE, ncid)
    if (status == nf90_noerr) then
        call ncheck(nf90_inq_varid(ncid, "time", varid), 'Inquire: time')
        call get_nc_start_date(ncid, varid, cur_date)
    endif

end subroutine restart_get_cur_date


! Save the atmosphere <-> coupling fields. This will then be used as a restart
! for the ice model.
subroutine restart_write(self, cur_date, fields)

    class(restart), intent(inout) :: self
    type(field), dimension(:), intent(in) :: fields

    integer :: ncid, time_dimid, time_varid, nx, ny
    integer, dimension(size(fields)) :: lat_dimid, lon_dimid, field_varid
    integer, dimension(3) :: dimids

    ! Create file, time dim and var, lat and lon dims.
    call ncheck(nf90_create(trim(this%restart_file), nf90_write, ncid), &
                'Creating: '//trim(this%restart_file))
    call ncheck(nf90_def_dim(ncid, 'time', nf90_unlimited, time_dimid), &
                'Defining time dim '//trim(this%restart_file))
    call ncheck(nf90_def_var(ncid, 'time', nf90_real, (/ time_dimid /), time_varid), &
                'Defining time var '//trim(this%restart_file))
    call ncheck(nf90_put_att(ncid, time_varid, 'units',
                        'days since '//cur_date%strftime("%Y-%m-%d %H:%M:%S")), &
                'Adding attribute units to time '//trim(this%restart_file))

    do i=1, size(fields)
        nx = size(fields(i)%array, 1)
        ny = size(fields(i)%array, 2)

        call ncheck(nf90_def_dim(ncid, 'ny_'//trim(fields(i)%name, ny,  latdim_id(i)), &
                    'Def dim '//'ny_'//trim(fields(i)%name)
        call ncheck(nf90_def_dim(ncid, 'nx_'//trim(fields(i)%name, nx,  londim_id(i)), &
                    'Def dim '//'nx_'//trim(fields(i)%name)

        dimids(:) = (/ londim_id(i), latdim_id(i), time_dimid /)
        call ncheck(nf90_def_var(ncid, fields(i)%name, nf90_real, dimids, field_varid(i)), &
                    'Defining var '//trim(this%restart_file))


    enddo
    call ncheck(nf90_enddef(ncid), 'nf90_enddef for '//trim(this%restart_file))

    ! Load single time value
    call ncheck(nf90_put_var(ncid, time_varid, (/ 0 /)),
                'nf90_put_var '//trim(this%restart_file))

    ! Load field values.
    do i=1, size(fields)
        call ncheck(nf90_put_var(ncid, field_varid(i), fields(i)%array))
    enddo

    call ncheck(nf_close(ncid))

endsubroutine restart_write
 
endmodule restart_mod
