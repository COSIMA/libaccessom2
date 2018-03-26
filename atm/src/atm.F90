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
    use util_mod, only : runtime_in_seconds
    use accessom2_mod, only : accessom2_type => accessom2

    implicit none

    type(params) :: param
    type(accessom2_type) :: accessom2
    type(coupler_type) :: coupler
    type(forcing_type) :: forcing
    type(ice_grid_type) :: ice_grid
    type(datetime) :: start_date, end_date, cur_date
    type(runoff_type) :: runoff
    type(field_type), dimension(:), allocatable :: fields
    type(field_type) :: runoff_field
    integer, dimension(2) :: ice_shape
    integer :: i, num_coupling_fields, min_dt, runtime

    ! Initialise run settings
    call param%init()

    ! Initialise our ACCESS-OM2 module needed for model-level housekeeping
    call accessom2%init('matmxx')
    start_date = accessom2%get_start_date()
    end_date = accessom2%get_job_end_date()
    cur_date = start_date

    ! Initialise the coupler
    call coupler%init_begin('matmxx', start_date)

    ! Initialise forcing object and fields, involves reading details of each
    ! field from disk.
    call forcing%init("forcing.json", start_date,
                      param%forcing_period_years, num_coupling_fields)
    allocate(fields(num_coupling_fields))
    call forcing%init_fields(fields, min_dt)

    ! Get information about the ice grid needed for runoff remapping.
    call ice_grid%init(coupler%get_peer_intercomm())
    call ice_grid%recv()

    ! Initialise the runoff remapping object with ice grid information.
    call runoff%init(ice_grid, param%runoff_remap_weights_file)
    ice_shape = ice_grid%get_shape()

    ! Initialise OASIS3-MCT fields. Runoff done seperately for now.
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
        print*, 'ATM 0'
        runtime = runtime_in_seconds(start_date, cur_date)

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
        print*, 'ATM 1'

        ! Block until we receive from ice. Ice will do a nonblocking send immediately
        ! after receiving the above fields. This prevents the atm from sending continuously.
        call coupler%sync('matmxx')
        print*, 'ATM 2'

        ! Update current date
        cur_date = cur_date + timedelta(seconds=min_dt)
        if (cur_date == end_date) then
            exit
        endif
        call assert(cur_date < end_date, 'ATM: current date after end date')
        call assert(cur_date >= start_date, 'ATM: current date before start date')
    enddo

    call accessom2%deinit(cur_date)
    call coupler%deinit(cur_date)

end program atm
