
program ice

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use datetime_module, only : datetime, strptime, timedelta
    use ice_grid_mod, only  : ice_grid_type => ice_grid
    use field_mod, only : field_type => field
    use restart_mod, only : restart_type => restart
    use mod_oasis, only : OASIS_IN, OASIS_OUT

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128, &
                          MAX_FILE_NAME_LEN = 256

    type(ice_grid_type) :: ice_grid
    type(coupler_type) :: coupler
    type(restart_type) :: restart

    ! Namelist parameters
    character(len=19) :: start_date, end_date
    integer :: dt, i, tmp_unit
    integer, dimension(2) :: resolution
    type(field_type), dimension(:), allocatable :: fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_atm_field_names = ''
    character(len=MAX_FILE_NAME_LEN) :: ice_grid_file, ice_mask_file
    integer :: num_coupling_fields
    type(datetime) :: cur_date, run_start_date, run_end_date
    logical :: file_exists

    namelist /ice_nml/ start_date, end_date, dt, resolution, &
                       from_atm_field_names, ice_grid_file, ice_mask_file

    ! Read namelist which includes information about the start and end date,
    ! model resolution and names and direction of coupling fields.
    inquire(file='ice.nml', exist=file_exists)
    call assert(file_exists, 'Input ice.nml does not exist.')
    open(newunit=tmp_unit, file='ice.nml')
    read(tmp_unit, nml=ice_nml)
    close(tmp_unit)

    run_start_date = strptime(start_date, '%Y-%m-%d %H:%M:%S')
    run_end_date = strptime(end_date, '%Y-%m-%d %H:%M:%S')

    call restart%init(run_start_date, 'a2i.nc')
    cur_date = restart%get_cur_date()

    ! Count the coupling fields
    num_coupling_fields = 0
    do i=1, MAX_FIELDS
        if (from_atm_field_names(i) /= '') then
            num_coupling_fields = num_coupling_fields + 1
        else
            exit
        endif
    enddo

    allocate(fields(num_coupling_fields))

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('cicexx', run_start_date)

    ! Initialise grid and send details to peer.
    ! This will be used for regridding of runoff.
    call ice_grid%init(trim(ice_grid_file), trim(ice_mask_file), &
                       resolution, coupler%get_peer_intercomm())
    call ice_grid%send()

    do i=1, num_coupling_fields
        fields(i)%name = trim(from_atm_field_names(i))
        allocate(fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(fields(i), OASIS_IN)
    enddo
    call coupler%init_end()

    do
        ! Get fields from atmos
        do i=1, num_coupling_fields
            call coupler%get(fields(i), cur_date)
        enddo

        ! atm is blocked, unblock it. This prevents the atm from sending
        ! continuously and means that the next set of coupling fields
        ! will arrive while we're doing work.
        call coupler%sync('cicexx')

        ! Do work
        cur_date = cur_date + timedelta(seconds=dt)
        if (cur_date == run_end_date) then
            exit
        endif
        call assert(cur_date < run_end_date, 'ICE: current date after end date')
        call assert(cur_date >= run_start_date, 'ICE: current date before start date')
    enddo

    ! Write out restart.
    call restart%write(cur_date, fields)
    call coupler%deinit()
    do i=1, num_coupling_fields
        deallocate(fields(i)%data_array)
    enddo
    deallocate(fields)

end program
