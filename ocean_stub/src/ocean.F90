
program ocean

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use datetime_module, only : datetime, strptime, timedelta
    use field_mod, only : field_type => field
    use restart_mod, only : restart_type => restart
    use accessom2_mod, only : accessom2_type => accessom2
    use util_mod, only : timedelta_in_seconds
    use mod_oasis, only : OASIS_IN, OASIS_OUT

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128, &
                          MAX_FILE_NAME_LEN = 256

    type(coupler_type) :: coupler
    type(restart_type) :: restart
    type(accessom2_type) :: accessom2

    ! Namelist parameters
    type(datetime) :: cur_date, start_date, end_date
    integer :: dt, i, tmp_unit, err
    integer, dimension(2) :: resolution
    type(field_type), dimension(:), allocatable :: in_fields, out_fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_ice_field_names = '', to_ice_field_names = ''
    integer :: num_from_ice_fields, num_to_ice_fields
    logical :: file_exists

    namelist /ocean_nml/ dt, resolution, &
                       from_ice_field_names, to_ice_field_names

    ! Read namelist which model resolution and names and
    ! direction of coupling fields.
    inquire(file='ocean.nml', exist=file_exists)
    call assert(file_exists, 'Input ocean.nml does not exist.')
    open(newunit=tmp_unit, file='ocean.nml')
    read(tmp_unit, nml=ocean_nml)
    close(tmp_unit)

    ! Initialise our ACCESS-OM2 module needed for model-level housekeeping
    call accessom2%init('matmxx')
    start_date = accessom2%get_start_date()
    end_date = accessom2%get_end_date()
    cur_date = start_date

    ! Initialise coupler, adding coupling fields
    call coupler%init_begin('oceanx', start_date, &
                            timedelta_in_seconds(start_date, end_date))

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
        do i=1, size(in_fields)
            call coupler%get(in_fields(i), cur_date, err)
        enddo

        ! Do work, i.e. use the in_fields and populate the out_fields

        ! Send fields to ice
        do i=1, size(out_fields)
            call coupler%put(out_fields(i), cur_date, err)
        enddo

        cur_date = cur_date + timedelta(seconds=dt)
        if (cur_date == end_date) then
            exit
        endif
        call assert(cur_date < end_date, 'ICE: current date after end date')
        call assert(cur_date >= start_date, 'ICE: current date before start date')
    enddo

    ! Write out restart.
    call restart%init('o2i.nc')
    ! FIXME: dodgy hack, we need to change the out_field names to have
    ! '_i' prefix. This exists in ACCESS-OM2 and will go away once we do
    ! ocean and ice restarts properly.
    do i=1, size(out_fields)
        out_fields(i)%name = trim(out_fields(i)%name)//'_i'
    enddo
    call restart%write(cur_date, out_fields)

    call accessom2%deinit(cur_date)
    call coupler%deinit(cur_date)

end program ocean
