
program forcing_test

    use, intrinsic :: iso_c_binding, only: c_null_char
    use datetime_module, only : datetime, c_strptime, tm2date, tm_struct
    use forcing_config_mod, only : forcing_config_type => forcing_config

    implicit none

    integer, parameter :: MAX_FILE_NAME_LEN = 1024

    type(forcing_config_type) :: forcing_config
    character(len=MAX_FILE_NAME_LEN) :: forcing_config_file
    integer :: nfields, dt, fp, rc, i
    type(datetime) :: start_date, forcing_date, experiment_date
    type(tm_struct) :: ctime

    forcing_config_file = 'forcing.json'

    rc = c_strptime("1900-06-06%00:00:00"//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    start_date = tm2date(ctime)
    forcing_date = start_date
    experiment_date = start_date

    call forcing_config%init(forcing_config_file)
    call forcing_config%parse(nfields)

    ! Read in base forcing, apply pertubations and write out
    do i=1, nfields
        call forcing_config%forcing_fields(i)%init(start_date, dt)
        call forcing_config%forcing_fields(i)%update(forcing_date, experiment_date)
    enddo

    ! Write data out, this is expected to have the perturbed value
    ! FIXME: need to write out to a netCDF file
    open(newunit=fp, file='test.dat', form='unformatted')
    write(fp) forcing_config%forcing_fields(1)%data_array
    close(fp)

    call exit(0)

end program forcing_test
