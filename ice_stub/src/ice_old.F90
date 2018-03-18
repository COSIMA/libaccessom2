
program ice

use coupler, only : coupler_init, coupler_init_done, coupler_get, coupler_put, &
                    coupler_add_field, coupler_destroy_field, coupler_close, &
                    couple_field_type, &
                    COUPLER_MAX_FIELDS, COUPLER_MAX_FIELD_NAME_LEN, COUPLER_IN, COUPLER_OUT
use restarts, only : read_restart, write_restart
use calendar, only : calendar_timediff, date_type, calendar_make_date
use error_handler, only : assert

implicit none

! Namelist parameters
integer, dimension(3) :: run_start_date, run_end_date
integer :: dt = 1800
integer, dimension(2) :: resolution
integer, dimension(2) :: subdomain_decomposition
character(len=COUPLER_MAX_FIELD_NAME_LEN), dimension(COUPLER_MAX_FIELDS) :: &
    from_atm_field_names = '', to_atm_field_names = '', &
    from_ocn_field_names = '', to_ocn_field_names = ''

namelist /ice_nml/ run_start_date, run_end_date, dt, resolution, &
                   subdomain_decomposition, &
                   from_atm_field_names, to_atm_field_names, &
                   from_ocn_field_names, to_ocn_field_names

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

    x_global_res = resolution(1)
    y_global_res = resolution(2)
    x_subdomains = subdomain_decomposition(1)
    y_subdomains = subdomain_decomposition(2)
    
    ! Create date types.
    call calendar_make_date(run_start_date, start_date)
    call calendar_make_date(run_end_date, end_date)

    ! Count the coupling fields
    num_from_atm_fields = 0
    num_to_atm_fields = 0
    num_from_ocn_fields = 0
    num_to_ocn_fields = 0
    do i=1, COUPLER_MAX_FIELDS
        if (from_atm_field_names(i) /= '') then
            num_from_atm_fields = num_from_atm_fields + 1
        endif
        if (from_atm_field_names(i) /= '') then
            num_to_atm_fields = num_to_atm_fields + 1
        endif
        if (from_atm_field_names(i) /= '') then
            num_from_ocn_fields = num_from_ocn_fields + 1
        endif
        if (from_atm_field_names(i) /= '') then
            num_to_ocn_fields = num_to_ocn_fields + 1
        endif
    enddo

    ! Initialise the coupler. 
    call coupler_init('icexxx', x_global_res, y_global_res, x_subdomains, y_subdomains)

    ! Add all the fields. 
    allocate(from_atm_fields(num_from_atm_fields))
    allocate(to_atm_fields(num_to_atm_fields))
    allocate(from_ocn_fields(num_from_ocn_fields))
    allocate(to_ocn_fields(num_to_atm_fields))
    do i=1, num_from_atm_fields
        call coupler_add_field(from_atm_fields(i), from_atm_field_names(i), COUPLER_IN)
    enddo
    do i=1, num_to_atm_fields
        call coupler_add_field(to_atm_fields(i), to_atm_field_names(i), COUPLER_OUT)
    enddo
    do i=1, num_from_ocn_fields
        call coupler_add_field(from_ocn_fields(i), from_ocn_field_names(i), COUPLER_IN)
    enddo
    do i=1, num_to_ocn_fields
        call coupler_add_field(to_ocn_fields(i), to_ocn_field_names(i), COUPLER_OUT)
    enddo

    call coupler_init_done()

    ! Calculate time difference in seconds between start and end dates.
    call calendar_timediff(start_date, end_date, runtime)

    ! Read in restart? 

    nsteps = runtime / dt
    curr_time = 0

    ! Read in initial condition and send to ocean and atmosphere. 
    call read_restart('restart/ice_to_atm_restart.nc', to_atm_fields)
    call coupler_put(curr_time, to_atm_fields)
    call read_restart('restart/ice_to_ocn_restart.nc', to_ocn_fields)
    call coupler_put(curr_time, to_ocn_fields)

    do step=1, nsteps 

        ! Get fields from atmos
        call coupler_get(curr_time, from_atm_fields)

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

    ! Clean up coupler. 
    do i=1, num_from_atm_fields
        call coupler_destroy_field(from_atm_fields(i))
    enddo
    do i=1, num_to_atm_fields
        call coupler_destroy_field(to_atm_fields(i))
    enddo
    do i=1, num_from_ocn_fields
        call coupler_destroy_field(from_ocn_fields(i))
    enddo
    do i=1, num_to_ocn_fields
        call coupler_destroy_field(to_ocn_fields(i))
    enddo
    deallocate(from_atm_fields)
    deallocate(to_atm_fields)
    deallocate(from_ocn_fields)
    deallocate(to_ocn_fields)

    call coupler_close()

end program
