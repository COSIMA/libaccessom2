program atm

    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use forcing_mod, only : forcing_type => forcing
    use field_mod, only : field_type => field, FIELD_DOMAIN_LAND
    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use ice_grid_proxy_mod, only : ice_grid_type => ice_grid_proxy
    use runoff_mod, only : runoff_type => runoff
    use accessom2_mod, only : accessom2_type => accessom2
    use simple_timer_mod, only : simple_timer_type => simple_timer
    use logger_mod, only : LOG_INFO, LOG_DEBUG
    use yatm_version_mod, only : YATM_COMMIT_HASH

    implicit none

    integer, parameter :: MAX_FILE_NAME_LEN = 1024

    type(accessom2_type) :: accessom2
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(runoff_type) :: runoff
    type(field_type), dimension(:), allocatable :: fields
    integer, dimension(:), allocatable :: to_runoff_map
    type(field_type), dimension(2) :: runoff_fields ! Liquid (river) and solid (iceberg) runoff
    character(len=MAX_FILE_NAME_LEN) :: forcing_file, accessom2_config_dir
    character(len=9) :: calendar
    integer, dimension(2) :: ice_shape
    integer :: i, err, tmp_unit
    logical :: file_exists
    integer :: num_atm_to_ice_fields, dt, cur_runtime_in_seconds
    integer :: num_runoff_fields

    type(simple_timer_type) :: field_read_timer, ice_wait_timer
    type(simple_timer_type) :: init_runoff_timer, remap_runoff_timer
    type(simple_timer_type) :: coupler_put_timer

    namelist /atm_nml/ forcing_file, accessom2_config_dir

    print *, YATM_COMMIT_HASH

    ! Read input namelist
    forcing_file = 'forcing.json'
    accessom2_config_dir = '../'
    inquire(file='atm.nml', exist=file_exists)
    call assert(file_exists, 'Input atm.nml does not exist.')
    open(newunit=tmp_unit, file='atm.nml')
    read(tmp_unit, nml=atm_nml)
    close(tmp_unit)

    ! Initialise model-level init, config and sync/tracking module
    call accessom2%init('matmxx', config_dir=trim(accessom2_config_dir))
    call accessom2%print_version_info()

    ! Initialise the coupler.
    call coupler%init_begin('matmxx', accessom2%logger, &
                            config_dir=trim(accessom2_config_dir))

    ! Initialise ice grid proxy and get information about it,
    ! this is needed for local remapping.
    call ice_grid%init(coupler%ice_root)
    call ice_grid%recv()
    ice_shape = ice_grid%get_shape()

    ! Initialise forcing object, this reads config and
    ! tells us how man atm-to-ice fields there are.
    call forcing%init(forcing_file, accessom2%logger, num_atm_to_ice_fields)

    ! Tell libaccessom2 about any global configs/state
    call accessom2%set_calendar(calendar)
    call accessom2%set_atm_timestep(dt)
    call accessom2%set_cpl_field_counts(num_atm_to_ice_fields=num_atm_to_ice_fields)
    ! Synchronise accessom2 'state' (i.e. configuration) between all PEs of all models.
    call accessom2%sync_config(coupler)

    ! Initialise timers
    call field_read_timer%init('field_read', accessom2%logger, &
                               accessom2%simple_timers_enabled())
    call ice_wait_timer%init('ice_wait', accessom2%logger, &
                             accessom2%simple_timers_enabled())
    call init_runoff_timer%init('init_runoff', accessom2%logger, &
                                 accessom2%simple_timers_enabled())
    call remap_runoff_timer%init('remap_runoff', accessom2%logger, &
                                 accessom2%simple_timers_enabled())
    call coupler_put_timer%init('coupler_put', accessom2%logger, &
                                 accessom2%simple_timers_enabled())

    ! Initialise the runoff remapping object with ice grid information.
    call init_runoff_timer%start()
    call runoff%init(ice_grid)
    call init_runoff_timer%stop()

    ! Initialise forcing fields, involves reading details of each from disk,
    ! and allocating necessary memory.
    allocate(fields(num_atm_to_ice_fields))
    call forcing%init_fields(fields, accessom2%get_cur_forcing_date(), &
                             dt, calendar, num_land_fields)
    ! Create intermediate fields for runoff,
    ! these are a copy/variation of the forcing fields
    allocate(runoff_fields(num_land_fields))

    ! Create a little map to go from atm_to_ice field indices to
    ! runoff field indices, simplifies the code below
    allocate(to_runoff_map(num_atm_to_ice_fields))
    ri = 1
    do i=1, num_atm_to_ice_fields
        if (field(i)%domain == FIELD_DOMAIN_LAND) then
            to_runoff_map(i) = ri
            ri = ri + 1
        else
            to_runoff_map(i) = 0
        endif
    enddo

    ! Initialise coupling fields, runoff fields need special treatment.
    do i=1, num_atm_to_ice_fields
        if (to_runoff_map[i] /= 0)
            ri = to_runoff_map[i]
            runoff_fields(ri)%name = fields(i)%name
            runoff_fields(ri)%domain = fields(i)%domain
            runoff_fields(ri)%timestamp = fields(i)%timestamp
            allocate(runoff_fields(ri)%data_array(ice_shape(1), ice_shape(2)))
            call coupler%init_field(runoff_fields(ri), OASIS_OUT)
        else
            call coupler%init_field(fields(i), OASIS_OUT)
        endif
    enddo

    ! Finish coupler initialisation. Tell oasis how long the run is and the
    ! coupling timesteps.
    call coupler%init_end(accessom2%get_total_runtime_in_seconds(), &
                          accessom2%get_coupling_field_timesteps())

    do while (.not. accessom2%run_finished())

        cur_runtime_in_seconds = int(accessom2%get_cur_runtime_in_seconds())

        ! Send each forcing field
        do i=1, num_atm_to_ice_fields
            if (mod(cur_runtime_in_seconds, fields(i)%dt) == 0) then
                call field_read_timer%start()
                call forcing%update_field(fields(i), &
                                          accessom2%get_cur_forcing_date())
                call field_read_timer%stop()
                if (to_runoff_map(i) /= 0) then
                    ri = to_runoff_map(i)
                    call remap_runoff_timer%start()
                    call runoff%remap(fields(i)%data_array, &
                                      runoff_fields(ri)%data_array, ice_grid%mask)
                    call remap_runoff_timer%stop()
                endif
            endif

            call coupler_put_timer%start()
            if (index(fields(i)%name, 'runof') /= 0) then
                call coupler%put(runoff_field, cur_runtime_in_seconds, err)
            else
                call coupler%put(fields(i), cur_runtime_in_seconds, err)
            endif
            call coupler_put_timer%stop()
        enddo

        ! Block until we receive from ice. Ice will do a nonblocking send immediately
        ! after receiving the above fields. This prevents the atm from sending continuously.
        call ice_wait_timer%start()
        call accessom2%atm_ice_sync()
        call ice_wait_timer%stop()

        call accessom2%logger%write(LOG_INFO, '{ "cur_exp-datetime" :  "'//accessom2%get_cur_exp_date_str()//'" }')
        call accessom2%logger%write(LOG_INFO, '{ "cur_forcing-datetime" : "'//accessom2%get_cur_forcing_date_str()//'" }')
        call accessom2%logger%write(LOG_DEBUG, 'cur_runtime_in_seconds ', &
                                    int(accessom2%get_cur_runtime_in_seconds()))

        call accessom2%progress_date(dt)
    enddo

    call field_read_timer%write_stats()
    call ice_wait_timer%write_stats()
    call init_runoff_timer%write_stats()
    call remap_runoff_timer%write_stats()
    call coupler_put_timer%write_stats()

    call accessom2%logger%write(LOG_INFO, 'Run complete, calling deinit')

    call coupler%deinit()
    call accessom2%deinit(finalize=.true.)
    call forcing%deinit()

end program atm
