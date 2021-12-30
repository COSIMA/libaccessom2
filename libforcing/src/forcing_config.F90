module forcing_config_mod

use error_handler, only : assert
use json_module
use json_kinds
use datetime_module, only : datetime
use util_mod, only : get_var_dims, replace_text
use forcing_field_mod, only : forcing_field
use logger_mod, only : logger_type => logger, LOG_DEBUG

use forcing_perturbation_mod, only : FORCING_PERTURBATION_TYPE_SCALING, &
                              FORCING_PERTURBATION_TYPE_OFFSET, &
                              FORCING_PERTURBATION_TYPE_SEPARABLE, &
                              FORCING_PERTURBATION_DIMENSION_SPATIAL, &
                              FORCING_PERTURBATION_DIMENSION_TEMPORAL, &
                              FORCING_PERTURBATION_DIMENSION_SPATIOTEMPORAL, &
                              FORCING_PERTURBATION_DIMENSION_CONSTANT, &
                              FORCING_PERTURBATION_CALENDAR_EXPERIMENT, &
                              FORCING_PERTURBATION_CALENDAR_FORCING

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

implicit none
private

type, public :: forcing_config
    type(json_file) :: json
    type(json_core) :: core
    integer :: num_inputs
    integer :: min_dt
    character(len=9) :: calendar

    integer :: time_cache_size
    type(logger_type), pointer :: logger
contains
    procedure, pass(self), public :: init => forcing_config_init
    procedure, pass(self), public :: deinit => forcing_config_deinit
    procedure, pass(self), public :: parse => forcing_config_parse
    procedure, pass(self), private :: parse_input => forcing_config_parse_input
    procedure, pass(self), private :: parse_permutations => &
                                        forcing_config_parse_permutations
endtype forcing_config

contains

!> Open forcing file and find fields
subroutine forcing_config_init(self, config, loggerin, num_fields, &
                               time_cache_size)

    class(forcing_config), intent(inout) :: self
    character(len=*), intent(in) :: config
    type(logger_type), target, intent(in) :: loggerin
    integer, intent(out) :: num_fields
    integer, intent(in), optional :: time_cache_size

    type(json_value), pointer :: root, inputs
    logical :: found

    if (present(time_cache_size)) then
        self%time_cache_size = time_cache_size
    else
        self%time_cache_size = -1
    endif

    call self%json%initialize()
    call self%json%load_file(filename=trim(config))
    if (self%json%failed()) then
        call self%json%print_error_message(stderr)
        call assert(.false., 'forcing_init() failed')
    endif

    call self%core%initialize()

    call self%json%get(root)
    call self%core%get_child(root, "inputs", inputs, found)
    call assert(found, "No inputs found in forcing config.")

    self%num_inputs = self%core%count(inputs)
    num_fields = self%num_inputs
    self%logger => loggerin

endsubroutine forcing_config_init


!> Parse forcing file
subroutine forcing_config_parse(self, forcing_fields, start_date, &
                                num_land_fields, min_dt, calendar)
    class(forcing_config), intent(inout) :: self
    type(forcing_field), dimension(:), intent(inout) :: forcing_fields
    type(datetime), intent(in) :: start_date
    integer, intent(out) :: min_dt, num_land_fields
    character(len=9), intent(out) :: calendar

    type(json_value), pointer :: input_jv_ptr
    integer :: i, dt
    character(len=9) :: calendar_str
    character(kind=CK, len=:), allocatable :: product_name

    type(json_value), pointer :: root, inputs
    logical :: found, is_land_field

    call assert(size(forcing_fields) == self%num_inputs, &
                "Insufficient number of forcing_fields allocated.")

    call self%json%get(root)
    call self%core%get_child(root, "inputs", inputs, found)
    call assert(found, "No inputs found in forcing config.")

    call self%core%get(root, "forcing_product_name", product_name, found)
    call assert(found, "No forcing_product_name found in forcing config.")

    min_dt = HUGE(1)
    calendar = ''
    num_land_fields = 0
    do i=1, self%num_inputs
        call self%core%get_child(inputs, i, input_jv_ptr, found)
        call assert(found, "No inputs found in forcing config.")
        call self%parse_input(input_jv_ptr, forcing_fields(i), start_date, &
                              product_name, dt, calendar_str, is_land_field)
        if (dt < min_dt) then
            min_dt = dt
        endif
        if (calendar == '') then
            calendar = calendar_str
        else
            call assert(calendar == calendar_str, &
                        "Inconsistent calendar between forcing fields.")
        endif
        if (is_land_field) then
            num_land_fields = num_land_fields + 1
        endif
    enddo

endsubroutine forcing_config_parse

!> Parse a single forcing input. This corresponds to a single coupling field.
subroutine forcing_config_parse_input(self, input_jv_ptr, field_ptr, &
                                      start_date, product_name, dt, forcing_calendar, &
                                      is_land_field)

    class(forcing_config), intent(inout) :: self
    type(json_value), pointer :: input_jv_ptr
    type(forcing_field) :: field_ptr
    type(datetime), intent(in) :: start_date
    character(len=*), intent(in) :: product_name
    integer, intent(out) :: dt
    character(len=9), intent(out) :: forcing_calendar
    logical, intent(out) :: is_land_field

    character(kind=CK, len=:), allocatable :: cname, realm_str
    character(kind=CK, len=:), allocatable :: filename, fieldname
    character(len=256), dimension(:), allocatable :: fieldname_list, filename_list
    integer :: num_input_fields
    logical :: found
    integer :: i, j

    type(json_value), pointer :: input_field_jv_list, input_field_jv_ptr

    call self%core%get(input_jv_ptr, "coupling_field_name", cname, found)
    call assert(found, "Entry 'coupling_field_name' not found in forcing config.")

    call self%core%get(input_jv_ptr, "realm", realm_str, found)
    if (found) then
        call assert(realm_str == "land" .or. realm_str == "atmosphere", &
                    "forcing_parse_field: invalid domain value.")
    else
        realm_str = "atmosphere"
    endif

    is_land_field = .false.
    if (realm_str == "land") then
        is_land_field = .true.
    endif

    ! Each coupling field can have multiple input fields associated with it.
    ! The YATM code can combine these as it sees fit.
    call self%core%get_child(input_jv_ptr, "input_fields", &
                             input_field_jv_list, found)
    num_input_fields = self%core%count(input_field_jv_list)
    allocate(filename_list(num_input_fields), fieldname_list(num_input_fields))

    do i=1, num_input_fields
        call self%core%get_child(input_field_jv_list, i, &
                                 input_field_jv_ptr, found)

        call self%core%get(input_field_jv_ptr, "filename", filename, found)
        call assert(found, "Expected to find filename entry.")
        filename_list(i) = trim(filename)

        call self%core%get(input_field_jv_ptr, "fieldname", fieldname, found)
        call assert(found, "Expected to find fieldname entry.")
        fieldname_list(i) = trim(fieldname)

        ! Each input field can have a number of permutations.
        call self%parse_permutations(input_field_jv_ptr, field_ptr, fieldname)
    enddo

    call field_ptr%init(fieldname_list, filename_list, cname, realm_str, &
                        start_date, product_name, self%logger, &
                        self%time_cache_size, dt, forcing_calendar) 

end subroutine forcing_config_parse_input


subroutine forcing_config_parse_permutations(self, field_jv_ptr, field_ptr, fieldname)

    class(forcing_config), intent(inout) :: self
    type(json_value), pointer :: field_jv_ptr
    type(forcing_field) :: field_ptr
    character(len=*) :: fieldname

    integer :: perturbation_constant_value
    logical :: found
    integer :: num_perturbations, num_fields
    integer :: i, j

    type(json_value), pointer :: perturbation_jv_ptr
    type(json_value), pointer :: dimension_jv_ptr, value_jv_ptr
    type(json_value), pointer :: perturbation_list, dimension_list
    type(json_value), pointer :: value_list

    character(kind=CK, len=:), allocatable :: filename, perturbation_filename
    character(kind=CK, len=:), allocatable :: comment
    character(kind=CK, len=:), allocatable :: perturbation_type
    character(kind=CK, len=:), allocatable :: dimension_type
    character(kind=CK, len=:), allocatable :: perturbation_calendar

    call self%core%get_child(field_jv_ptr, "perturbations", perturbation_list, found)
    if (.not. found) then
        return
    endif

    num_perturbations = self%core%count(perturbation_list)
    allocate(field_ptr%perturbations(num_perturbations))
    allocate(field_ptr%separated_perturbations(num_perturbations))

    do i=1, num_perturbations
        call self%core%get_child(perturbation_list, i, &
                                 perturbation_jv_ptr, found)
        call assert(found, "Expected to find perturbation entry.")

        field_ptr%perturbations(i)%name = fieldname
        field_ptr%perturbations(i)%valid = .true.
        ! These are only valid for the separable permutation type
        field_ptr%separated_perturbations(i)%valid = .false.

        call self%core%get(perturbation_jv_ptr, "type", perturbation_type, found)
        call assert(found, "No type in perturbation entry.")
        call assert(trim(perturbation_type) == 'scaling' .or. &
                    trim(perturbation_type) == 'offset' .or. &
                    trim(perturbation_type) == 'separable', &
                    "forcing_parse_field: invalid perturbation type")
        if (trim(perturbation_type) == 'scaling') then
            field_ptr%perturbations(i)%perturbation_type = &
                FORCING_PERTURBATION_TYPE_SCALING
        elseif (trim(perturbation_type) == 'offset') then
            field_ptr%perturbations(i)%perturbation_type = &
                FORCING_PERTURBATION_TYPE_OFFSET
        else
            field_ptr%perturbations(i)%perturbation_type = &
                FORCING_PERTURBATION_TYPE_SEPARABLE

            field_ptr%separated_perturbations(i)%perturbation_type = &
                FORCING_PERTURBATION_TYPE_SEPARABLE
            field_ptr%separated_perturbations(i)%name = fieldname
            field_ptr%separated_perturbations(i)%valid = .true.
        endif

        if (field_ptr%perturbations(i)%perturbation_type == &
                FORCING_PERTURBATION_TYPE_SEPARABLE) then
            call self%core%get_child(perturbation_jv_ptr, "dimension", &
                                     dimension_list, found)
            call assert(self%core%count(dimension_list) == 2, &
                   'forcing_parse_field: invalid no of serparable dimensions')
            do j=1, 2
                call self%core%get_child(dimension_list, j, dimension_jv_ptr, found)
                call assert(found, "Missing dimension list in perturbation entry.")
                call self%core%serialize(dimension_jv_ptr, dimension_type)
                dimension_type = replace_text(dimension_type, '"', '')
                dimension_type = replace_text(dimension_type, NEW_LINE('A'), '')
                if (j == 1) then
                    call assert(trim(dimension_type) == 'temporal', &
                               'forcing_parse_field: invalid separable dimension')
                    field_ptr%perturbations(i)%dimension_type = &
                        FORCING_PERTURBATION_DIMENSION_TEMPORAL
                else
                    call assert(trim(dimension_type) == 'spatial', &
                                'forcing_parse_field: invalid separable dimension')
                    field_ptr%separated_perturbations(i)%dimension_type = &
                        FORCING_PERTURBATION_DIMENSION_SPATIAL
                endif
            enddo
        else
            call self%core%get(perturbation_jv_ptr, "dimension", &
                                dimension_type, found)
            call assert(found, "No dimension in perturbation entry.")
            call assert(trim(dimension_type) == 'spatial' .or. &
                        trim(dimension_type) == 'temporal' .or. &
                        trim(dimension_type) == 'spatiotemporal' .or. &
                        trim(dimension_type) == 'constant', &
                        "forcing_parse_field: invalid perturbation dimension type")
            if (trim(dimension_type) == 'spatial') then
                field_ptr%perturbations(i)%dimension_type = &
                    FORCING_PERTURBATION_DIMENSION_SPATIAL
            elseif (trim(dimension_type) == 'temporal') then
                field_ptr%perturbations(i)%dimension_type = &
                    FORCING_PERTURBATION_DIMENSION_TEMPORAL
            elseif (trim(dimension_type) == 'spatiotemporal') then
                field_ptr%perturbations(i)%dimension_type = &
                    FORCING_PERTURBATION_DIMENSION_SPATIOTEMPORAL
            else
                call assert(trim(dimension_type) == 'constant', &
                            "forcing_parse_field: bad dimension type")
                field_ptr%perturbations(i)%dimension_type = &
                    FORCING_PERTURBATION_DIMENSION_CONSTANT
            endif
        endif

        if (field_ptr%perturbations(i)%perturbation_type == &
                FORCING_PERTURBATION_TYPE_SEPARABLE) then

            call self%core%get_child(perturbation_jv_ptr, "value", value_list, found)
            call assert(self%core%count(value_list) == 2, &
                        "forcing_parse_field: invalid number of dimensions &
                            for perturbation type serperable")
            do j=1, 2
                call self%core%get_child(value_list, j, value_jv_ptr, found)
                call assert(found, "Missing value list in perturbation entry.")
                call self%core%serialize(value_jv_ptr, perturbation_filename)
                perturbation_filename = replace_text(perturbation_filename, &
                                                     '"', '')
                perturbation_filename = replace_text(perturbation_filename, &
                                                NEW_LINE('A'), '')
                if (j == 1) then
                    field_ptr%perturbations(i)%filename_template =  &
                        trim(perturbation_filename)
                else
                    field_ptr%separated_perturbations(i)%filename_template =  &
                        trim(perturbation_filename)
                endif
            enddo
        else
            if (field_ptr%perturbations(i)%dimension_type == &
                    FORCING_PERTURBATION_DIMENSION_CONSTANT) then
                call self%core%get(perturbation_jv_ptr, "value", &
                                   perturbation_constant_value, found)
                call assert(found, "No value in perturbation entry.")
                field_ptr%perturbations(i)%constant_value = &
                    perturbation_constant_value
            else
                call self%core%get(perturbation_jv_ptr, "value", &
                                   perturbation_filename, found)
                call assert(found, "No filename in perturbation entry.")
                field_ptr%perturbations(i)%filename_template = perturbation_filename
            endif
        endif

        num_fields = self%core%count(perturbation_jv_ptr)
        call self%core%get(perturbation_jv_ptr, "calendar", &
                           perturbation_calendar, found)
        ! Calendar is optional for dimension 'constant'
        if (.not. found) then
            call assert(field_ptr%perturbations(i)%dimension_type == &
                        FORCING_PERTURBATION_DIMENSION_CONSTANT, &
                        "forcing_parse_field: missing calendar type")
            call assert(num_fields == 4, 'forcing_parse_field: wrong number of fields'// &
                        ' in perturbation definition, should be 4.')
        else
            call assert(trim(perturbation_calendar) == 'forcing' .or. &
                        trim(perturbation_calendar) == 'experiment', &
                        "forcing_parse_field: invalid perturbation calendar type")
            call assert(num_fields == 5, 'forcing_parse_field: wrong number of fields'// &
                        ' in perturbation definition, should be 5.')
            if (trim(perturbation_calendar) == 'forcing') then
                field_ptr%perturbations(i)%calendar = &
                    FORCING_PERTURBATION_CALENDAR_FORCING
            else
                field_ptr%perturbations(i)%calendar = &
                    FORCING_PERTURBATION_CALENDAR_EXPERIMENT
            endif
        endif

        call self%core%get(perturbation_jv_ptr, "comment", &
                           comment, found)
        call assert(found, 'forcing_parse_field: perturbation missing "comment" field')

        call field_ptr%perturbations(i)%init()
        if (field_ptr%separated_perturbations(i)%valid) then
            call field_ptr%separated_perturbations(i)%init()
        endif
    enddo

end subroutine forcing_config_parse_permutations


subroutine forcing_config_deinit(self)
    class(forcing_config), intent(inout) :: self

    call self%core%destroy()
    call self%json%destroy()

end subroutine forcing_config_deinit

endmodule forcing_config_mod
