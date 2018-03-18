
program ice

    use coupler_mod, only : coupler_type => coupler

    use coupler, only : coupler_init, coupler_init_done, coupler_get, coupler_put, &
                        coupler_add_field, coupler_destroy_field, coupler_close, &
                        couple_field_type, &
                        COUPLER_MAX_FIELDS, COUPLER_MAX_FIELD_NAME_LEN, COUPLER_IN, COUPLER_OUT
    use restarts, only : read_restart, write_restart
    use calendar, only : calendar_timediff, date_type, calendar_make_date
    use error_handler, only : assert

    implicit none

    type(coupler_type) :: coupler
    type(forcing_type) :: forcing

    ! Namelist parameters
    integer, dimension(3) :: run_start_date, run_end_date
    integer :: dt = 1800
    integer, dimension(2) :: resolution
    integer, dimension(2) :: subdomain_decomposition
    character(len=COUPLER_MAX_FIELD_NAME_LEN), dimension(COUPLER_MAX_FIELDS) :: &
        from_atm_field_names = '', to_atm_field_names = '', &
        from_ocn_field_names = '', to_ocn_field_names = ''

    namelist /ice_nml/ run_start_date, run_end_date, dt, resolution, &
                       from_atm_field_names

    type(couple_field_type), dimension(:), allocatable :: from_atm_fields, to_atm_fields, &
                                                          from_ocn_fields, to_ocn_fields
    type(date_type) :: start_date, end_date
    integer :: step, nsteps, i
    integer :: x_global_res, y_global_res, x_subdomains, y_subdomains
    integer :: runtime, curr_time
    integer :: tmp_unit, num_from_atm_fields, num_to_atm_fields, &
               num_from_ocn_fields, num_to_ocn_fields
    logical :: file_exists

    ! Read namelist which includes information about the start and end date,
    ! model resolution and names and direction of coupling fields. 
    inquire(file='ice.nml', exist=file_exists)
    call assert(file_exists, 'Input ice.nml does not exist.')
    open(newunit=tmp_unit, file='ice.nml')
    read(tmp_unit, nml=ice_nml)
    close(tmp_unit)

    ! Create date types.
    call calendar_make_date(run_start_date, start_date)
    call calendar_make_date(run_end_date, end_date)

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('icexx', start_date, forcing%get_num_fields())
    do i=1, forcing%get_num_fields()
        call coupler%add_field(forcing%get_name(i), forcing%get_shape(i))
    enddo
    call coupler%init_end()

    nsteps = runtime / dt
    curr_time = 0

    do step=1, nsteps

        ! Get fields from atmos
        call coupler%get(curr_time, from_atm_fields)

        ! Get fields from ocean
        call coupler_get(curr_time, from_ocn_fields)

        ! Do some calculation.

        ! After calculation time has passed.
        curr_time = curr_time + dt

        if (step /= nsteps) then
            ! Send fields to ocean.
            call coupler_put(curr_time, to_ocn_fields)
            ! Send fields to atm.
            call coupler_put(curr_time, to_atm_fields)
        else
            call write_restart('restart/ice_to_atm_restart.nc', to_atm_fields)
            call write_restart('restart/ice_to_ocn_restart.nc', to_ocn_fields)
        endif
    enddo

    ! Write out restart.
    call restart%write(cur_date, forcing%fields)
    call coupler%deinit()

end program
