
program basic_test

    use forcing_config_mod, only : forcing_config_type => forcing_config

    implicit none

    integer, parameter :: MAX_FILE_NAME_LEN = 1024

    type(forcing_config_type) :: forcing_config
    character(len=MAX_FILE_NAME_LEN) :: forcing_config_file
    integer :: nfields

    forcing_config_file = 'forcing.json'

    call forcing_config%init(forcing_config_file, nfields)
    call forcing_config%parse()

    ! Read in base forcing, apply pertubations and write out

    exit 0

end program basic_test
