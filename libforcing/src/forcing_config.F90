module forcing_config_mod

use error_handler, only : assert
use json_module
use json_kinds
use datetime_module, only : datetime
use util_mod, only : get_var_dims, replace_text
use forcing_field_mod, only : forcing_field

use forcing_pertubation_mod, only : FORCING_PERTUBATION_TYPE_SCALING, &
                              FORCING_PERTUBATION_TYPE_OFFSET, &
                              FORCING_PERTUBATION_DIMENSION_SPATIAL, &
                              FORCING_PERTUBATION_DIMENSION_TEMPORAL, &
                              FORCING_PERTUBATION_DIMENSION_SPATIOTEMPORAL, &
                              FORCING_PERTUBATION_DIMENSION_CONSTANT, &
                              FORCING_PERTUBATION_CALENDAR_EXPERIMENT, &
                              FORCING_PERTUBATION_CALENDAR_FORCING

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none
private

type, public :: forcing_config
    type(datetime) :: start_date
    type(json_file) :: json
    type(json_core) :: core
    integer :: num_land_fields
    type(forcing_field), dimension(:), allocatable :: forcing_fields
contains
    procedure, pass(self), public :: init => forcing_config_init
    procedure, pass(self), public :: deinit => forcing_config_deinit
    procedure, pass(self), public :: parse => forcing_config_parse
    procedure, pass(self), private :: parse_field => forcing_config_parse_field
endtype forcing_config

contains

!> Open forcing file and find fields
subroutine forcing_config_init(self, config)

    class(forcing_config), intent(inout) :: self
    character(len=*), intent(in) :: config

    call self%json%initialize()
    call self%json%load_file(filename=trim(config))
    if (self%json%failed()) then
        call self%json%print_error_message(stderr)
        call assert(.false., 'forcing_init() failed')
    endif

    call self%core%initialize()
    self%num_land_fields = 0

endsubroutine forcing_config_init


!> Parse forcing file into a dictionary.
subroutine forcing_config_parse(self, num_inputs)
    class(forcing_config), intent(inout) :: self
    integer, intent(out) :: num_inputs

    type(json_value), pointer :: field_jv_ptr
    integer :: i
    logical :: found
    type(json_value), pointer :: root, inputs

    call self%json%get(root)
    call self%core%get_child(root, "inputs", inputs, found)
    call assert(found, "No inputs found in forcing config.")

    num_inputs = self%core%count(inputs)
    allocate(self%forcing_fields(num_inputs))

    do i=1, num_inputs
        call self%core%get_child(inputs, i, field_jv_ptr, found)
        call assert(found, "No inputs found in forcing config.")
        call self%parse_field(field_jv_ptr, self%forcing_fields(i))
    enddo

endsubroutine forcing_config_parse


subroutine forcing_config_parse_field(self, field_jv_ptr, field_ptr)

    class(forcing_config), intent(inout) :: self
    type(json_value), pointer :: field_jv_ptr
    type(forcing_field) :: field_ptr

    character(kind=CK, len=:), allocatable :: cname, fieldname, domain_str
    character(kind=CK, len=:), allocatable :: filename, pertubation_filename
    character(kind=CK, len=:), allocatable :: pertubation_type
    character(kind=CK, len=:), allocatable :: dimension_type
    character(kind=CK, len=:), allocatable :: calendar
    integer :: pertubation_constant_value
    logical :: found, domain_found
    integer :: i, num_pertubations

    type(json_value), pointer :: pertubation_jv_ptr
    type(json_value), pointer :: pertubation_list

    call self%core%get(field_jv_ptr, "fieldname", fieldname, found)
    call assert(found, "Entry 'fieldname' not found in forcing config.")

    call self%core%get(field_jv_ptr, "filename", filename, found)
    call assert(found, "Entry 'filename' not found in forcing config.")

    call self%core%get(field_jv_ptr, "cname", cname, found)
    call assert(found, "Entry 'cname' not found in forcing config.")

    call self%core%get(field_jv_ptr, "domain", domain_str, domain_found)
    if (domain_found) then
        call assert(domain_str == "land" .or. domain_str == "atmosphere", &
                    "forcing_parse_field: invalid domain value.")
    else
        domain_str = "atmosphere"
    endif

    if (domain_str == "land") then
        self%num_land_fields = self%num_land_fields + 1
    endif

    call field_ptr%new(fieldname, filename, cname, domain_str)

    call self%core%get_child(field_jv_ptr, "pertubations", pertubation_list, found)
    if (.not. found) then
        return
    endif

    num_pertubations = self%core%count(pertubation_list)
    allocate(field_ptr%pertubations(num_pertubations))

    do i=1, num_pertubations
        call self%core%get_child(pertubation_list, i, pertubation_jv_ptr, found)
        call assert(found, "Expected to find pertubation entry.")

        field_ptr%pertubations(i)%name = fieldname

        call self%core%get(pertubation_jv_ptr, "type", pertubation_type, found)
        call assert(found, "No type in pertubation entry.")
        call assert(trim(pertubation_type) == 'scaling' .or. &
                    trim(pertubation_type) == 'offset', &
                    "forcing_parse_field: invalid pertubation type")
        if (trim(pertubation_type) == 'scaling') then
            field_ptr%pertubations(i)%pertubation_type = &
                FORCING_PERTUBATION_TYPE_SCALING
        else
            field_ptr%pertubations(i)%pertubation_type = &
                FORCING_PERTUBATION_TYPE_OFFSET
        endif

        call self%core%get(pertubation_jv_ptr, "dimension", dimension_type, found)
        call assert(found, "No dimension in pertubation entry.")
        call assert(trim(dimension_type) == 'spatial' .or. &
                    trim(dimension_type) == 'temporal' .or. &
                    trim(dimension_type) == 'spatiotemporal' .or. &
                    trim(dimension_type) == 'constant', &
                    "forcing_parse_field: invalid pertubation dimension type")
        if (trim(dimension_type) == 'spatial') then
            field_ptr%pertubations(i)%dimension_type = &
                FORCING_PERTUBATION_DIMENSION_SPATIAL
        elseif (trim(dimension_type) == 'temporal') then
            field_ptr%pertubations(i)%dimension_type = &
                FORCING_PERTUBATION_DIMENSION_TEMPORAL
        elseif (trim(dimension_type) == 'spatiotemparal') then
            field_ptr%pertubations(i)%dimension_type = &
                FORCING_PERTUBATION_DIMENSION_SPATIOTEMPORAL
        else
            field_ptr%pertubations(i)%dimension_type = &
                FORCING_PERTUBATION_DIMENSION_CONSTANT
        endif

        if (field_ptr%pertubations(i)%dimension_type == &
                FORCING_PERTUBATION_DIMENSION_CONSTANT) then
            call self%core%get(pertubation_jv_ptr, "value", &
                               pertubation_constant_value, found)
            call assert(found, "No value in pertubation entry.")
            field_ptr%pertubations(i)%constant_value = &
                pertubation_constant_value
        else
            call self%core%get(pertubation_jv_ptr, "value", &
                               pertubation_filename, found)
            call assert(found, "No value in pertubation entry.")
            field_ptr%pertubations(i)%filename_template = pertubation_filename
        endif

        call self%core%get(pertubation_jv_ptr, "calendar", calendar, found)
        if (.not. found) then
            call assert(field_ptr%pertubations(i)%dimension_type == &
                        FORCING_PERTUBATION_DIMENSION_CONSTANT, &
                        "forcing_parse_field: missing calendar type")
        else
            call assert(trim(pertubation_type) == 'forcing' .or. &
                        trim(pertubation_type) == 'experiment', &
                        "forcing_parse_field: invalid pertubation calendar type")
            if (trim(calendar) == 'forcing') then
                field_ptr%pertubations(i)%calendar = &
                    FORCING_PERTUBATION_CALENDAR_FORCING
            else
                field_ptr%pertubations(i)%calendar = &
                    FORCING_PERTUBATION_CALENDAR_EXPERIMENT
            endif
        endif

        call field_ptr%pertubations(i)%init()
    enddo

end subroutine forcing_config_parse_field


subroutine forcing_config_deinit(self)
    class(forcing_config), intent(inout) :: self

    call self%core%destroy()
    call self%json%destroy()

end subroutine forcing_config_deinit

endmodule forcing_config_mod
