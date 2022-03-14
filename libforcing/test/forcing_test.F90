
program forcing_test

    use, intrinsic :: iso_c_binding, only: c_null_char
    use netcdf
    use logger_mod, only : logger_type => logger, LOG_DEBUG
    use datetime_module, only : datetime, c_strptime, tm2date, tm_struct
    use forcing_config_mod, only : forcing_config_type => forcing_config
    use forcing_field_mod, only : forcing_field_type => forcing_field

    implicit none

    integer, parameter :: MAX_FILE_NAME_LEN = 1024
    integer, parameter :: NDIMS = 2

    type(forcing_config_type) :: forcing_config
    type(forcing_field_type), dimension(:), allocatable :: forcing_fields
    character(len=MAX_FILE_NAME_LEN) :: forcing_config_file
    character(len=19) :: datetime_str
    integer :: dt, fp, rc, i, length
    type(datetime) :: start_date, forcing_date, experiment_date
    type(tm_struct) :: ctime

    integer :: nx, ny
    integer :: ncid, varid, dimids(NDIMS)
    integer :: x_dimid, y_dimid
    integer :: num_atm_to_ice_fields, num_land_fields
    character(len=9) :: calendar
    type(logger_type) :: logger

    call logger%init('test', loglevel='DEBUG')

    forcing_config_file = 'forcing.json'

    call get_command_argument(1, value=datetime_str, length=length, status=rc)
    if (rc /= 0 .or. length /= 19) then
        print*, 'Provide a date in format 1900-06-06T00:00:00 as an argument'
        stop 1
    endif

    rc = c_strptime(datetime_str//c_null_char, &
                    "%Y-%m-%dT%H:%M:%S"//c_null_char, ctime)
    start_date = tm2date(ctime)
    forcing_date = start_date
    experiment_date = start_date

    call forcing_config%init(forcing_config_file, logger, num_atm_to_ice_fields)
    allocate(forcing_fields(num_atm_to_ice_fields))
    call forcing_config%parse(forcing_fields, start_date, &
                              num_land_fields, dt, calendar)

    ! Read in base forcing, apply pertubations and write out
    do i=1, num_atm_to_ice_fields
        call forcing_fields(i)%update(forcing_date, experiment_date)
    enddo

    ! Write out results to NetCDF
    ny = size(forcing_fields(1)%data_array, 1)
    nx = size(forcing_fields(1)%data_array, 2)

    call check(nf90_create("test_output.nc", NF90_CLOBBER, ncid))
    call check(nf90_def_dim(ncid, "x", nx, x_dimid))
    call check(nf90_def_dim(ncid, "y", ny, y_dimid))

    dimids =  (/ y_dimid, x_dimid /)

    call check(nf90_def_var(ncid, forcing_fields(1)%names(1), &
               NF90_REAL, dimids, varid))

    call check(nf90_enddef(ncid))
    call check(nf90_put_var(ncid, varid, forcing_fields(1)%data_array))
    call check(nf90_close(ncid))

contains
    subroutine check(status)
        integer, intent ( in) :: status

        if(status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped"
        end if
    end subroutine check
end program forcing_test
