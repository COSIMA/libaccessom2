program atm

    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use datetime_module, only : datetime, timedelta
    use forcing_mod, only : forcing_type => forcing
    use field_mod, only : field_type => field
    use coupler_mod, only : coupler_type => coupler
    use params_mod, only : params
    use error_handler, only : assert
    use ice_grid_proxy_mod, only : ice_grid_type => ice_grid_proxy
    use runoff_mod, only : runoff_type => runoff
    use restart_mod, only : restart_type => restart
    use util_mod, only : runtime_in_seconds

    implicit none

    type(params) :: param
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(datetime) :: cur_date
    type(restart_type) :: restart
    type(runoff_type) :: runoff
    type(field_type), dimension(:), allocatable :: fields
    type(field_type) :: runoff_field
    integer, dimension(2) :: ice_shape
    integer :: i, num_coupling_fields, min_dt, runtime

    ! Get run settings, including the start date from the prior restart file.
    call param%init()
    call restart%init(param%start_date, 'atm_restart.nc')
    cur_date = restart%get_date()

    ! Initialise forcing object and fields, involves reading details of each
    ! field from disk.
    call forcing%init("forcing.json", param%start_date, &
                      param%forcing_period_years, num_coupling_fields)
    allocate(fields(num_coupling_fields))
    call forcing%init_fields(fields, min_dt)

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('matmxx', param%start_date)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%get_peer_intercomm())
    call ice_grid%recv()

    ! Initialise the runoff remapping object with ice grid information.
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
    ice_shape = ice_grid%get_shape()

    ! Initialise OASIS3-MCT fields. Runoff done seperately for now.
    ! FIXME: should be able to remove special treatment of runoff field.
    do i=1, num_coupling_fields
        if (fields(i)%name == 'runoff') then
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

    do
        runtime = runtime_in_seconds(param%start_date, cur_date)

        ! Send each forcing field
        do i=1, num_coupling_fields
            if (mod(runtime, fields(i)%dt) == 0) then
                call forcing%update_field(cur_date, fields(i))
                if (fields(i)%name == 'runoff') then
                    call runoff%remap(fields(i)%data_array, runoff_field%data_array, ice_grid%mask)
                endif
            endif

            if (fields(i)%name == 'runoff') then
                call coupler%put(runoff_field, cur_date, param%debug_output)
            else
                call coupler%put(fields(i), cur_date, param%debug_output)
            endif
        enddo

        ! Block until we receive from ice. Ice will do a nonblocking send immediately
        ! after receiving the above fields. This prevents the atm from sending continuously.
        call coupler%sync('matmxx')

        ! Update current date
        cur_date = cur_date + timedelta(seconds=min_dt)
        if (cur_date == param%end_date) then
            exit
        endif
        call assert(cur_date < param%end_date, 'ATM: current date after end date')
        call assert(cur_date >= param%start_date, 'ATM: current date before start date')
    enddo

    call restart%write(cur_date, fields)
    call coupler%deinit()

    do i=1, num_coupling_fields
        deallocate(fields(i)%data_array)
    enddo
    deallocate(fields)

end program atm
