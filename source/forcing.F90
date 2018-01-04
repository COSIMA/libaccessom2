module forcing

use error_handler , only : assert
use json_module
use datetime_module, only: datetime
use field, only: init, update

implicit none

private
public init, get_atm_forcing

type forcing_type
    private
    type(datetime) :: start_date
    integer :: period
    class(field_type), dimension(:), allocatable :: fields
contains
    private
    procedure, public :: init => forcing_init
    procedure, public :: update => forcing_update
    procedure, public :: num_fields
end type forcing_type

contains

!> Parse forcing file into a dictionary.
subroutine forcing_init(this, config, start_date, period)

    class(forcing_type), intent(inout) :: this
    character(len=*), intent(in) :: config
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: period

    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: root, inputs, fp
    integer :: i

    this%start_date = start_date
    this%period = period
    this%index_guess = 1

    call json%initialize()
    call json%load_file(filename=config)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        call assert(.false., 'forcing_init() failed')
    endif

    call core%initialize()
    call json%get(root)
    call core%get_child(root, "inputs", inputs)

    n_inputs = core%count(inputs)
    allocate(fields(n_inputs))

    do i, n_inputs
        call core%get_child(inputs, i, fp, found)
        call assert(found, "Input not found in forcing config.")

        call json%get(fp, "filename", filename, found)
        call assert(found, "Entry 'filename' not found in forcing config.")

        call json%get(fp, "fieldname", fieldname, found)
        call assert(found, "Entry 'fieldname' not found in forcing config.")

        call json%get(fp, "cname", cname, found)
        call assert(found, "Entry 'cname' not found in forcing config.")

        ! Get the shape of forcing fields
        fname  = filename_for_year(filename, start_date.getYear())
        call ncheck(nf90_open(trim(fname), NF90_NOWRITE, ncid), &
                    'Opening '//trim(fname))
        call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
                    'Inquire: '//trim(fieldname))

        call get_var_dims(ncid, varid, unused, nx, ny, unused)
        call ncheck(nf90_close(ncid), 'Closing '//trim(fname))

        ! Initialise a new field object.
        fields(i)%init(cname, filename, fieldname, nx, ny)
    enddo

    call core%destroy()
    call json%destroy()

end subroutine forcing_init

end module forcing
