
program ocean

    use coupler_mod, only : coupler_type => coupler
    use error_handler, only : assert
    use field_mod, only : field_type => field
    use restart_mod, only : restart_type => restart
    use accessom2_mod, only : accessom2_type => accessom2
    use mod_oasis, only : OASIS_IN, OASIS_OUT
    use ocean_version_mod, only : OCEAN_STUB_COMMIT_HASH

    implicit none

    integer, parameter :: MAX_FIELDS = 20, MAX_FIELD_NAME_LEN = 128, &
                          MAX_FILE_NAME_LEN = 1024

    type(coupler_type) :: coupler
    type(restart_type) :: restart
    type(accessom2_type) :: accessom2

    ! Namelist parameters
    integer :: dt, i, idx, tmp_unit, err
    integer, dimension(2) :: resolution
    type(field_type), dimension(:), allocatable :: in_fields, out_fields
    character(len=MAX_FIELD_NAME_LEN), dimension(MAX_FIELDS) :: &
        from_ice_field_names = '', to_ice_field_names = ''
    integer :: num_from_ice_fields, num_to_ice_fields
    integer :: cur_runtime_in_seconds
    logical :: file_exists
    character(len=MAX_FILE_NAME_LEN) :: accessom2_config_dir

    namelist /ocean_nml/ dt, resolution, accessom2_config_dir, &
                         from_ice_field_names, to_ice_field_names
    accessom2_config_dir = './'

    print *, OCEAN_STUB_COMMIT_HASH

    ! Read namelist which model resolution and names and
    ! direction of coupling fields.
    inquire(file='ocean.nml', exist=file_exists)
    call assert(file_exists, 'Input ocean.nml does not exist.')
    open(newunit=tmp_unit, file='ocean.nml')
    read(tmp_unit, nml=ocean_nml)
    close(tmp_unit)

    ! Initialise time manager
    call accessom2%init('mom5xx', config_dir=accessom2_config_dir)
    call accessom2%print_version_info()

    call coupler%init_begin('mom5xx',  accessom2%logger, config_dir=accessom2_config_dir)
    ! Synchronise accessom2 'state' (i.e. configuration) between all models.
    call accessom2%sync_config(coupler)

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
    call coupler%init_end(accessom2%get_total_runtime_in_seconds(), &
                          accessom2%get_coupling_field_timesteps())

    cur_runtime_in_seconds = accessom2%get_cur_runtime_in_seconds()
    do
        ! Get fields from ice
        do i=1, size(in_fields)
            call coupler%get(in_fields(i), cur_runtime_in_seconds, err)
        enddo

        ! Do work, i.e. use the in_fields and populate the out_fields

        call accessom2%progress_date(dt)
        if (accessom2%run_finished()) then
            exit
        endif
        cur_runtime_in_seconds = accessom2%get_cur_runtime_in_seconds()

        ! Send fields to ice
        do i=1, size(out_fields)
            out_fields(i)%data_array(:, :) = 0.0
            call coupler%put(out_fields(i), cur_runtime_in_seconds, err)
        enddo
    enddo

    ! Write out restart.
    call restart%init('o2i.nc')
    ! FIXME: dodgy hack, we need to change the out_field names to have
    ! '_i' prefix. This exists in ACCESS-OM2 and will go away once we do
    ! ocean and ice restarts properly.
    !do i=1, size(out_fields)
    !    idx = index(out_fields(i)%name, '_oi')
    !    call assert(idx /= 0, 'Did not find expected substring _oi')
    !    out_fields(i)%name = out_fields(i)%name(1:idx-1)//'_i'
    !enddo

    ! Write out restart.
    call restart%write(accessom2%get_cur_exp_date(), out_fields)
    call coupler%deinit()
    call accessom2%deinit(finalize=.true.)

end program ocean
