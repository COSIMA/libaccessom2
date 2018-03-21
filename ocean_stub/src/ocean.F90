
program ice

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use datetime_module, only : datetime, strptime
    use ice_grid_mod, only  : ice_grid_type => ice_grid
    use field_mod, only : ice_type => field
    use mod_oasis, only : OASIS_IN, OASIS_OUT

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128

    type(ice_grid_type) :: ice_grid
    type(coupler_type) :: coupler

    ! Namelist parameters
    integer, dimension(3) :: run_start_date, run_end_date
    integer :: dt = 1800
    integer, dimension(2) :: resolution
    type(field_type), allocatable :: fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_atm_field_names = ''
    integer :: num_coupling_fields

    namelist /ice_nml/ run_start_date, run_end_date, dt, resolution, &
                       from_atm_field_names

    type(datetime) :: cur_date, start_date, end_date
    logical :: file_exists

    ! Read namelist which includes information about the start and end date,
    ! model resolution and names and direction of coupling fields.
    inquire(file='ice.nml', exist=file_exists)
    call assert(file_exists, 'Input ice.nml does not exist.')
    open(newunit=tmp_unit, file='ice.nml')
    read(tmp_unit, nml=ice_nml)
    close(tmp_unit)

    start_date = strptime(run_start_date, '%Y-%m-%d %H:%M:%S')
    end_date = strptime(run_end_date, '%Y-%m-%d %H:%M:%S')

    call restart%init(start_date, 'a2i.nc')
    cur_date = restart%get_cur_date()

    ! Count the coupling fields
    num_coupling_fields = 0
    do i=1, MAX_FIELDS
        if (from_atm_field_names(i) /= '') then
            num_coupling_fields = num_coupling_fields + 1
            exit
        endif
    enddo

    allocate(fields(num_coupling_fields))

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('cicexx', start_date, num_coupling_fields)
    do i=1, num_coupling_fields
        allocate(field(i)%data_array(resolution))
        call coupler%init_field(field(i), OASIS_IN)
    enddo
    call coupler%init_end()

    ! Initialise grid and send details to peer.
    ! This will be used for regridding of runoff.
    call ice_grid%init('grid.nc', 'kmt.nc', resolution, &
                       coupler%get_peer_intercomm())
    call ice_grid%send()

    do
        ! Get fields from atmos
        do i=1, num_coupling_fields
            call coupler%get(fields(i), cur_date)
        enddo

        ! atm is blocked, unblock it. This prevents the atm from sending
        ! continuously and means that the next set of coupling fields
        ! will arrive while we're doing work.

        call coupler%sync()

        ! Do work

        cur_date = cur_date + timedelta(seconds=dt)
        if (cur_date == param%end_date) then
            exit
        elseif (cur_date > param%end_date) then
            assert(.false., 'Runtime not evenly divisible by timestep')
        endif
    enddo

    ! Write out restart.
    call restart%write(cur_date, fields)
    call coupler%deinit()
    do i=1, num_coupling_fields
        call coupler%deallocate(fields(i))
    enddo
    deallocate(fields)

end program
