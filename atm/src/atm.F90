program atm

    use forcing_mod, only : forcing_type
    use params_mod, only : params
    use coupler_mod, only : coupler
    use ice_grid_mod, only : ice_grid

    implicit none

    type(params) :: param
    type(coupler) :: couple
    type(atm_forcing) :: forcing
    type(ice_grid) :: ice
    type(datetime) :: cur_date
    character(len=64) :: field_name
    real, dimension(2) :: field_shape
    real, dimension(:, :), allocatable :: runoff_work

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
    call ice_grid%init(coupler%get_ice_intercomm())
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
    allocate(runoff_work(runoff%get_shape()(1), runoff%get_shape()(2)))

    do
        ! Update all forcing fields
        forcing%update(cur_date)

        ! Send each forcing field
        do i=1, forcing%get_num_fields()
            if (forcing%get_name(i) == 'runoff') then
                runoff%remap(forcing%get_data(i), runoff_work)
                coupler%put(forcing%get_name(i), runoff_work, &
                            cur_date, param%debug)
            else
                coupler%put(forcing%get_name(i), forcing%get_data(i), &
                            cur_date, param%debug)
            endif
        enddo

        cur_date = cur_date + timedelta(seconds=param%dt)

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler%sync()

        if (cur_date > param%end_date); exit
    enddo

    deallocate(runoff_work)
    call forcing%write_restart(cur_date)
    call coupler%deinit()

end program atm
