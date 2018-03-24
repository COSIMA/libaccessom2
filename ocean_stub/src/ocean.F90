
program ocean

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use datetime_module, only : datetime, strptime, timedelta
    use ice_grid_mod, only  : ice_grid_type => ice_grid
    use field_mod, only : field_type => field
    use restart_mod, only : restart_type => restart
    use mod_oasis, only : OASIS_IN, OASIS_OUT

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128, &
                          MAX_FILE_NAME_LEN = 256

    type(ice_grid_type) :: ice_grid
    type(coupler_type) :: coupler
    type(restart_type) :: restart

    ! Namelist parameters
    character(len=19) :: start_date, end_date
    integer :: dt, i, tmp_unit
    integer, dimension(2) :: resolution
    type(field_type), dimension(:), allocatable :: in_fields, out_fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_ice_field_names = '', to_ice_field_names
    integer :: num_from_ice_fields, num_to_ice_fields
    type(datetime) :: cur_date, run_start_date, run_end_date
    logical :: file_exists

    namelist /ocean_nml/ start_date, end_date, dt, resolution, &
                       from_ice_field_names, to_ice_field_names

    ! Read namelist which includes information about the start and end date,
    ! model resolution and names and direction of coupling fields.
    inquire(file='ocean.nml', exist=file_exists)
    call assert(file_exists, 'Input ocean.nml does not exist.')
    open(newunit=tmp_unit, file='ocean.nml')
    read(tmp_unit, nml=ocean_nml)
    close(tmp_unit)

    run_start_date = strptime(start_date, '%Y-%m-%d %H:%M:%S')
    run_end_date = strptime(end_date, '%Y-%m-%d %H:%M:%S')

    call restart%init('o2i.nc')
    cur_date = restart%get_date(run_start_date)

    ! Count and allocate the coupling fields
    num_from_ice_fields = 0
    num_to_ice_fields = 0
    do i=1, MAX_FIELDS
        if (from_ice_field_names(i) /= '') then
            num_from_ice_fields = num_from_ice_fields + 1
        endif
        if (to_ice_field_names(i) /= '') then
            num_to_ice_fields = num_to_ice_fields + 1
        endif
    enddo
    allocate(in_fields(num_from_ice_fields))
    allocate(out_fields(num_to_ice_fields))

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('oceanx', run_start_date)

    do i=1, num_from_ice_fields
        in_fields(i)%name = trim(from_ice_field_names(i))
        allocate(in_fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(in_fields(i), OASIS_IN)
    enddo
    do i=1, num_to_ice_fields
        out_fields(i)%name = trim(to_ice_field_names(i))
        allocate(out_fields(i)%data_array(resolution(1), resolution(2)))
        call coupler%init_field(out_fields(i), OASIS_OUT)
    enddo
    call coupler%init_end()

    do
        ! Get fields from ice
        do i=1, num_from_ice_fields
            call coupler%get(in_fields(i), cur_date)
        enddo

        ! Do work, i.e. use the in_fields and populate the out_fields

        ! Send fields to ice
        do i=1, num_to_ice_fields
            call coupler%put(out_fields(i), cur_date)
        enddo

        cur_date = cur_date + timedelta(seconds=dt)
        if (cur_date == run_end_date) then
            exit
        endif
        call assert(cur_date < run_end_date, 'ICE: current date after end date')
        call assert(cur_date >= run_start_date, 'ICE: current date before start date')
    enddo

    ! Write out restart.
    call restart%write(cur_date, out_fields)
    call coupler%deinit()

end program ocean
