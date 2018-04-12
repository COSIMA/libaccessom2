program atm

    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use forcing_mod, only : forcing_type => forcing
    use field_mod, only : field_type => field
    use coupler_mod, only : coupler_type => coupler
    use params_mod, only : params
    use error_handler, only : assert
    use ice_grid_proxy_mod, only : ice_grid_type => ice_grid_proxy
    use runoff_mod, only : runoff_type => runoff
    use date_manager_mod, only : date_manager_type => date_manager
    use logger_mod, only : logger_type => logger, LOG_INFO

    implicit none

    type(params) :: param
    type(logger_type) :: logger
    type(date_manager_type) :: date_manager
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(runoff_type) :: runoff
    type(field_type), dimension(:), allocatable :: fields
    type(field_type) :: runoff_field
    integer, dimension(2) :: ice_shape
    integer :: i, err
    integer :: num_coupling_fields, dt, cur_runtime_in_seconds

    ! Initialise run settings and logger
    call param%init()
    call logger%init('matmxx', logfiledir='log', loglevel=param%log_level)

    ! Initialise time manager
    call date_manager%init('matmxx')

    ! Initialise the coupler. It needs to tell oasis how long the run is.
    call coupler%init_begin('matmxx', &
                            date_manager%get_total_runtime_in_seconds(), logger)

    ! Initialise forcing object and fields, involves reading details of each
    ! field from disk.
    call forcing%init("forcing.json", date_manager%get_cur_forcing_date(), &
                      num_coupling_fields, logger)
    allocate(fields(num_coupling_fields))
    call forcing%init_fields(fields, dt)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%get_peer_intercomm())
    call ice_grid%recv()

    ! Initialise the runoff remapping object with ice grid information.
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
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
    call coupler%init_end()

    do while (.not. date_manager%run_finished())

        call logger%write(LOG_INFO, 'cur_exp_date '//date_manager%get_cur_exp_date_str())
        call logger%write(LOG_INFO, 'cur_forcing_date '//date_manager%get_cur_forcing_date_str())

        cur_runtime_in_seconds = date_manager%get_cur_runtime_in_seconds()

        ! Send each forcing field
        do i=1, num_coupling_fields
            if (mod(cur_runtime_in_seconds, fields(i)%dt) == 0) then
                call forcing%update_field(fields(i), &
                                          date_manager%get_cur_forcing_date())
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
        call coupler%atm_ice_sync()

        call date_manager%progress_date(dt)
    enddo

    call coupler%deinit(date_manager%get_cur_exp_date())
    call date_manager%deinit()
    call forcing%deinit()

end program atm
