program atm

    use forcing_mod, only : forcing_type
    use params_mod, only : params_type
    use coupler_mod, only : coupler_type
    use ice_grid_mod, only : ice_grid_type

    implicit none

    type(params_type) :: params
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(field_type), dimension(:), allocatable :: fields

    call params%init()

    call forcing%init("atm_forcing.json", params%start_date, &
                      params%forcing_period_years)
    allocate(fields(forcing%get_num_fields()))
    do i=1, size(fields)
        call fields(i)%init(forcing%get_name(i), forcing%get_shape(i))
    enddo

    call coupler%init('matmxx', params%start_date, fields)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(ice_intercomm)
    call runoff%init(ice_grid)

    do
        do i=1, size(fields)
            forcing%get_data(fields(i)%get_name(), cur_date, work)
            fields(i)%update(cur_date, work)

            if (fields(i)%get_name() == 'runoff') then
                runoff%remap(work, runoff)
                fields(i)%update(cur_date, runoff)
            endif

            coupler%put(fields(i), cur_date)
        enddo

        cur_date = cur_date + timedelta(seconds=dt)

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler%sync()

        if (cur_date > end_date); exit
    enddo

    call coupler%write_restart()
    call coupler%deinit()


end program atm
