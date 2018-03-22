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

    implicit none

    type(params) :: param
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(datetime) :: cur_date
    type(restart_type) :: restart
    type(runoff_type) :: runoff
    real, dimension(:, :), allocatable :: runoff_work
    type(field_type), dimension(:), allocatable :: fields
    type(field_type) :: runoff_field
    integer, dimension(2) :: ice_shape
    integer :: i, num_coupling_fields

    call param%init()
    call restart%init(param%start_date, 'a2i.nc')
    cur_date = restart%get_cur_date()

    call forcing%init("forcing.json", param%start_date, &
                      param%forcing_period_years, num_coupling_fields)
    allocate(fields(num_coupling_fields))
    call forcing%init_fields(fields)

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('matmxx', param%start_date)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%get_peer_intercomm())
    call ice_grid%recv()
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
    ice_shape = ice_grid%get_shape()

    do i=1, num_coupling_fields
        call coupler%init_field(fields(i), OASIS_OUT)
        if (fields(i)%name == 'runoff') then
            call assert(.not. allocated(runoff_field%data_array), &
                        'Runoff already associated')
            runoff_field%name = fields(i)%name
            runoff_field%timestamp = fields(i)%timestamp
            allocate(runoff_field%data_array(ice_shape(1), ice_shape(2)))
        endif
    enddo
    call coupler%init_end()

    do
        ! Update all forcing fields
        call forcing%update_fields(cur_date, fields)

        ! Send each forcing field
        do i=1, num_coupling_fields
            if (fields(i)%name == 'runoff') then
                call runoff%remap(fields(i)%data_array, runoff_field%data_array, ice_grid%mask)
                call coupler%put(runoff_field, cur_date, param%debug_output)
            else
                call coupler%put(fields(i), cur_date, param%debug_output)
            endif
        enddo

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler%sync('matmxx')

        cur_date = cur_date + timedelta(seconds=param%dt)
        if (cur_date == param%end_date) then
            exit
        elseif (cur_date > param%end_date) then
            call assert(.false., 'Runtime not evenly divisible by timestep')
        endif
    enddo

    call restart%write(cur_date, fields)
    call coupler%deinit()

    do i=1, num_coupling_fields
        deallocate(fields(i)%data_array)
    enddo
    deallocate(fields)
    deallocate(runoff_work)

end program atm
