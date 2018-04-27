program atm

    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use forcing_mod, only : forcing_type => forcing
    use field_mod, only : field_type => field
    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use ice_grid_proxy_mod, only : ice_grid_type => ice_grid_proxy
    use runoff_mod, only : runoff_type => runoff
    use accessom2_mod, only : accessom2_type => accessom2
    use logger_mod, only : logger_type => logger, LOG_INFO

    implicit none

    integer, parameter :: MAX_FILE_NAME_LEN = 1024


    type(logger_type) :: logger
    type(accessom2_type) :: accessom2
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(runoff_type) :: runoff
    type(field_type), dimension(:), allocatable :: fields
    type(field_type) :: runoff_field
    character(len=MAX_FILE_NAME_LEN) :: forcing_file, accessom2_config_dir
    character(len=9) :: calendar
    integer, dimension(2) :: ice_shape
    integer :: i, err, tmp_unit
    logical :: file_exists
    integer :: num_coupling_fields, dt, cur_runtime_in_seconds

    namelist /atm_nml/ forcing_file, accessom2_config_dir

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
    ! Logger needs MPI_Init to have been called (above) and can now start
    call logger%init('matmxx', logfiledir='log', loglevel=accessom2%log_level)

    ! Initialise forcing object and fields, involves reading details of each
    ! field from disk.
    call forcing%init(forcing_file, logger, num_coupling_fields)
    allocate(fields(num_coupling_fields))
    call forcing%init_fields(fields, accessom2%get_cur_forcing_date(), dt, calendar)
    ! 'calendar' is a global config, tell accessom2 about it.
    call accessom2%set_calendar(calendar)

    ! Initialise the coupler. It needs to tell oasis how long the run is.
    call coupler%init_begin('matmxx', logger, config_dir=trim(accessom2_config_dir))
    ! Synchronise accessom2 'state' (i.e. configuration) between all models.
    call accessom2%sync_config(coupler%atm_intercomm, coupler%ice_intercomm, &
                               coupler%ocean_intercomm)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%ice_intercomm)
    call ice_grid%recv()

    ! Initialise the runoff remapping object with ice grid information.
    call runoff%init(ice_grid)
    ice_shape = ice_grid%get_shape()

    ! Initialise OASIS3-MCT fields. Runoff done seperately for now.
    do i=1, num_coupling_fields
        if (index(fields(i)%name, 'runof') /= 0) then
            call assert(.not. allocated(runoff_field%data_array), &
                        'Runoff already associated')
            runoff_field%name = fields(i)%name
            runoff_field%timestamp = fields(i)%timestamp
            allocate(runoff_field%data_array(ice_shape(1), ice_shape(2)))
            call coupler%init_field(runoff_field, OASIS_OUT)
        else
            call coupler%init_field(fields(i), OASIS_OUT)
        endif
    enddo
    call coupler%init_end(accessom2%get_total_runtime_in_seconds())

    do while (.not. accessom2%run_finished())

        cur_runtime_in_seconds = accessom2%get_cur_runtime_in_seconds()

        ! Send each forcing field
        do i=1, num_coupling_fields
            if (mod(cur_runtime_in_seconds, fields(i)%dt) == 0) then
                call forcing%update_field(fields(i), &
                                          accessom2%get_cur_forcing_date())
                if (index(fields(i)%name, 'runof') /= 0) then
                    call runoff%remap(fields(i)%data_array, &
                                      runoff_field%data_array, ice_grid%mask)
                endif
            endif

            if (index(fields(i)%name, 'runof') /= 0) then
                call coupler%put(runoff_field, cur_runtime_in_seconds, err)
            else
                call coupler%put(fields(i), cur_runtime_in_seconds, err)
            endif
        enddo

        ! Block until we receive from ice. Ice will do a nonblocking send immediately
        ! after receiving the above fields. This prevents the atm from sending continuously.
        call accessom2%atm_ice_sync()

        call accessom2%progress_date(dt)

        call logger%write(LOG_INFO, 'cur_exp_date '//accessom2%get_cur_exp_date_str())
        call logger%write(LOG_INFO, 'cur_forcing_date '//accessom2%get_cur_forcing_date_str())
        call logger%write(LOG_DEBUG, 'cur_runtime_in_seconds ', &
                            accessom2%get_cur_runtime_in_seconds())
    enddo

    call logger%write(LOG_INFO, 'Run complete, calling deinit')

    call coupler%deinit()
    call accessom2%deinit(finalize=.true.)
    call forcing%deinit()

end program atm
