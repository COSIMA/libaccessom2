
program ice

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use datetime_module, only : datetime, strptime, timedelta
    use ice_grid_mod, only  : ice_grid_type => ice_grid
    use field_mod, only : field_type => field
    use restart_mod, only : restart_type => restart
    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use accessom2_mod, only : accessom2_type => accessom2

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128, &
                          MAX_FILE_NAME_LEN = 256

    type(ice_grid_type) :: ice_grid
    type(accessom2_type) :: accessom2
    type(coupler_type) :: coupler
    type(restart_type) :: i2o_restart, o2i_restart

    ! Namelist parameters
    type(datetime) :: start_date, end_date, cur_date
    integer :: dt, i, tmp_unit
    integer, dimension(2) :: resolution
    type(field_type), dimension(:), allocatable :: from_atm_fields, &
        from_ocean_fields, to_ocean_fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_atm_field_names = '', from_ocean_field_names = '', &
        to_ocean_field_names = ''
    integer :: num_from_atm_fields, num_from_ocean_fields, num_to_ocean_fields
    character(len=MAX_FILE_NAME_LEN) :: ice_grid_file, ice_mask_file
    logical :: file_exists

    namelist /ice_nml/ dt, resolution, &
                       from_atm_field_names, from_ocean_field_names, &
                       to_ocean_field_names, ice_grid_file, ice_mask_file

    ! Read namelist which includes information
    ! model resolution and names and direction of coupling fields.
    inquire(file='ice.nml', exist=file_exists)
    call assert(file_exists, 'Input ice.nml does not exist.')
    open(newunit=tmp_unit, file='ice.nml')
    read(tmp_unit, nml=ice_nml)
    close(tmp_unit)

    ! Initialise our ACCESS-OM2 module needed for model-level housekeeping
    call accessom2%init('cicexx')
    start_date = accessom2%get_start_date()
    end_date = accessom2%get_end_date()
    cur_date = start_date

    ! Initialise coupler, this needs to be done before the ice grid is
    ! sent to the atmosphere.
    call coupler%init_begin('cicexx', start_date)

    ! Count and allocate the coupling fields
    num_from_atm_fields = 0
    num_from_ocean_fields = 0
    num_to_ocean_fields = 0
    do i=1, MAX_FIELDS
        if (from_atm_field_names(i) /= '') then
            num_from_atm_fields = num_from_atm_fields + 1
        endif
        if (from_ocean_field_names(i) /= '') then
            num_from_ocean_fields = num_from_ocean_fields + 1
        endif
        if (to_ocean_field_names(i) /= '') then
            num_to_ocean_fields = num_to_ocean_fields + 1
        endif
    enddo
    allocate(from_atm_fields(num_from_atm_fields))
    allocate(from_ocean_fields(num_from_ocean_fields))
    allocate(to_ocean_fields(num_to_ocean_fields))

    ! Initialise ice grid and send details to peer.
    ! This will be used for regridding of runoff.
    call ice_grid%init(trim(ice_grid_file), trim(ice_mask_file), &
                       resolution, coupler%get_peer_intercomm())
    call ice_grid%send()

    ! Set up coupling fields
    do i=1, size(from_atm_fields)
        from_atm_fields(i)%name = trim(from_atm_field_names(i))
        allocate(from_atm_fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(from_atm_fields(i), OASIS_IN)
    enddo
    do i=1, size(from_ocean_fields)
        from_ocean_fields(i)%name = trim(from_ocean_field_names(i))
        allocate(from_ocean_fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(from_ocean_fields(i), OASIS_IN)
    enddo
    do i=1, size(to_ocean_fields)
        to_ocean_fields(i)%name = trim(to_ocean_field_names(i))
        allocate(to_ocean_fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(to_ocean_fields(i), OASIS_OUT)
    enddo
    call coupler%init_end()

    ! Read in o2i and i2o coupling field restart files.
    call o2i_restart%init('o2i.nc')
    call o2i_restart%read(from_ocean_fields)
    call i2o_restart%init('i2o.nc')
    call i2o_restart%read(to_ocean_fields)

    ! Get from atmosphere
    do i=1, size(from_atm_fields)
        call coupler%get(from_atm_fields(i), cur_date)
    enddo
    ! Update atmospheric forcing halos - expensive operation.

    ! Note the structure of the following loop:
    ! --> send to ocean
    ! --> do time consuming work
    ! --> get from ocean
    ! No work is done between the 'get' and 'send' because this is the
    ! only time that the ocean is waiting on the ice (given that the ice
    ! runs slightly faster than the ocean).
    do
        ! Send to ocean - non-blocking
        do i=1, size(to_ocean_fields)
            call coupler%put(to_ocean_fields(i), cur_date)
        enddo

        ! Do work
        cur_date = cur_date + timedelta(seconds=dt)
        if (cur_date == end_date) then
            exit
        endif
        call assert(cur_date < end_date, 'ICE: current date after end date')
        call assert(cur_date >= start_date, 'ICE: current date before start date')

        ! Get from atmos - fast because atmos should have already sent.
        do i=1, size(from_atm_fields)
            call coupler%get(from_atm_fields(i), cur_date)
        enddo

        ! atm is blocked, unblock it. This prevents the atm from sending
        ! continuously and means that the next set of coupling fields
        ! will arrive while we're doing work.
        call coupler%atm_ice_sync()

        ! Update atmospheric forcing halos - expensive operation.

        ! Get from ocean - blocking on ocean. Important that ice runs faster
        ! that ocean and can receive immediately and quickly loop to send
        ! to the ocean. This will minimise the ocean wait time.
        do i=1, size(from_ocean_fields)
            call coupler%get(from_ocean_fields(i), cur_date)
        enddo
    enddo

    ! Write out restart.
    call i2o_restart%write(cur_date, to_ocean_fields)

    call accessom2%deinit(cur_date)
    call coupler%deinit(cur_date)

end program
