module restart_mod

implicit none

use 

private
public restart_type

type restart_type
    type(datetime) :: restart_date
    character(len=256) :: restart_file
end type restart_type

contains

subroutine restart_init(this, start_date, restart_file)

    class(restart_type), intent(inout) :: this

    this%restart_date = start_date
    this%restart_file = restart_file

end subroutine

!> 
! Read restart file and get current date
subroutine restart_get_curr_date(this, curr_date)

    class(restart_type), intent(inout) :: this
    type(datetime), intent(inout) :: curr_date

    integer :: ncid, varid

    curr_date = this%start_date

    ! If restart file exists then read date
    status = nf90_open(trim(this%restart_file), NF90_NOWRITE, ncid)
    if (status == nf90_noerr) then
        call ncheck(nf90_inq_varid(ncid, "time", varid), 'Inquire: time')
        call get_nc_start_date(ncid, varid, curr_date)
    endif

end subroutine restart_get_curr_time


! Save the atmosphere <-> coupling fields. This will then be used as a restart
! for the ice model.
subroutine restart_write(this, cur_date, fields)

    class(restart_type), intent(inout) :: this
    type(field_type), dimension(:), intent(in) :: fields

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

subroutine restart_write
 
end module restart_mod
