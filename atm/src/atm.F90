program atm

    use datetime_module, only : datetime, timedelta
    use forcing_mod, only : forcing_type => forcing
    use coupler_mod, only : coupler_type => coupler
    use params_mod, only : params
    use ice_grid_mod, only : ice_grid_type => ice_grid
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
    integer, dimension(2) :: ice_shape
    integer :: i

    call param%init()
    call restart%init(param%start_date, 'a2i.nc')
    cur_date = restart%get_cur_date()

    call forcing%init("atm_forcing.json", param%start_date, &
                      param%forcing_period_years)

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('matmxx', param%start_date, forcing%get_num_fields())
    do i=1, forcing%get_num_fields()
        call coupler%add_field(forcing%get_name(i), forcing%get_shape(i))
    enddo
    call coupler%init_end()

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%get_peer_intercomm())
    call ice_grid%recv()
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
    ice_shape = ice_grid%get_shape()
    allocate(runoff_work(ice_shape(1), ice_shape(2)))

    do
        ! Update all forcing fields
        call forcing%update(cur_date)

        ! Send each forcing field
        do i=1, forcing%get_num_fields()
            if (forcing%get_name(i) == 'runoff') then
                call runoff%remap(forcing%fields(i)%array, runoff_work, ice%mask)
                call coupler%put(forcing%get_name(i), runoff_work, &
                                 cur_date, param%debug_output)
            else
                call coupler%put(forcing%get_name(i), forcing%fields(i)%array, &
                                 cur_date, param%debug_output)
            endif
        enddo

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler%sync()

        cur_date = cur_date + timedelta(seconds=param%dt)
        if (cur_date == param%end_date) then
            exit
        elseif (cur_date > param%end_date) then
            assert(.false., 'Runtime not evenly divisible by timestep')
        endif
    enddo

    deallocate(runoff_work)
    call restart%write(cur_date, forcing%fields)
    call coupler%deinit()

end program atm
