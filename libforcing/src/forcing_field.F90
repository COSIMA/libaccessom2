module forcing_field_mod

use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG
use datetime_module, only : datetime
use forcing_perturbation_mod, only : forcing_perturbation_type => forcing_perturbation
use forcing_perturbation_mod, only : FORCING_PERTURBATION_TYPE_SCALING, &
                                     FORCING_PERTURBATION_TYPE_OFFSET, &
                                     FORCING_PERTURBATION_TYPE_SEPARABLE
use ncvar_mod, only : ncvar_type => ncvar
use util_mod, only : filename_for_year

implicit none
private

! Forcing fields can have a domain
integer, parameter, public :: FORCING_FIELD_DOMAIN_NONE = 0
integer, parameter, public :: FORCING_FIELD_DOMAIN_ATMOSPHERE = 10
integer, parameter, public :: FORCING_FIELD_DOMAIN_LAND = 20

type, public :: forcing_field
    character(len=64), dimension(:), allocatable :: names
    character(len=64) :: coupling_name
    character(len=1024), dimension(:), allocatable :: filename_templates
    integer :: domain

    integer :: dt
    character(len=9) :: calendar
    character(len=64) :: product_name

    type(ncvar_type), dimension(:), allocatable :: ncvars
    real, dimension(:, :), allocatable :: data_array
    type(forcing_perturbation_type), dimension(:), allocatable :: perturbations
    type(forcing_perturbation_type), dimension(:), allocatable :: &
            separated_perturbations

    type(logger_type), pointer :: logger
contains
    procedure, pass(self), public :: init => forcing_field_init
    procedure, pass(self), public :: update => forcing_field_update
    procedure, pass(self), private :: calculate => forcing_field_calculate
    procedure, pass(self), private :: apply_perturbations => &
                forcing_field_apply_perturbations
    procedure, pass(self), public :: get_shape
endtype forcing_field

contains

subroutine forcing_field_init(self, name_list, filename_template_list, cname, domain, &
                              start_date, product_name, loggerin, dt, calendar)
    class(forcing_field), intent(inout) :: self
    character(len=*), dimension(:), intent(in) :: name_list
    character(len=*), dimension(:), intent(in) :: filename_template_list
    character(len=*), intent(in) :: cname
    character(len=*), intent(in) :: domain
    type(datetime), intent(in) :: start_date
    character(len=*), intent(in) :: product_name
    type(logger_type), target, intent(in) :: loggerin
    integer, intent(out) :: dt
    character(len=9), intent(out) :: calendar

    character(len=1024) :: filename
    integer :: num_file_inputs, i

    num_file_inputs = size(name_list)

    allocate(self%names(num_file_inputs))
    allocate(self%filename_templates(num_file_inputs))
    allocate(self%ncvars(num_file_inputs))

    self%names(:) = name_list(:)
    self%filename_templates(:) = filename_template_list(:)
    self%coupling_name = cname
    if (domain == 'atmosphere') then
        self%domain = FORCING_FIELD_DOMAIN_ATMOSPHERE
    else
        call assert(trim(domain) == 'land', &
                    "Invalid domain value.")
        self%domain = FORCING_FIELD_DOMAIN_LAND
    endif

    self%product_name = trim(product_name)
    self%logger => loggerin

    do i=1, num_file_inputs
        filename = filename_for_year(self%filename_templates(i), start_date%getYear())
        call self%ncvars(i)%init(self%names(i), filename)
    enddo

    ! Check that the metadata is the same on all input filenames
    if (num_file_inputs > 1) then
        do i=2,num_file_inputs
            call assert(self%ncvars(i)%dt == self%ncvars(1)%dt, &
             'File metadata (dt) for multi-field forcing does not match.')
            call assert(self%ncvars(i)%calendar == self%ncvars(1)%calendar, &
             'File metadata (calendar) for multi-field forcing does not match.')
            call assert(self%ncvars(i)%nx == self%ncvars(1)%nx, &
             'File metadata (nx) for multi-field forcing does not match.')
            call assert(self%ncvars(i)%ny == self%ncvars(1)%ny, &
             'File metadata (ny) for multi-field forcing does not match.')
        enddo
    endif

    allocate(self%data_array(self%ncvars(1)%nx, self%ncvars(1)%ny))
    self%data_array(:, :) = HUGE(1.0)

    self%dt = self%ncvars(1)%dt
    self%calendar = self%ncvars(1)%calendar
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

    type(logger_type), pointer :: logger
contains
    procedure, pass(self), public :: init => forcing_config_init
    procedure, pass(self), public :: deinit => forcing_config_deinit
    procedure, pass(self), public :: parse => forcing_config_parse
    procedure, pass(self), private :: parse_field => forcing_config_parse_field
endtype forcing_config

contains

!> Open forcing file and find fields
subroutine forcing_config_init(self, config, loggerin, num_fields)

    class(forcing_config), intent(inout) :: self
    character(len=*), intent(in) :: config
    type(logger_type), target, intent(in) :: loggerin
    integer, intent(out) :: num_fields

    type(json_value), pointer :: root, inputs
    logical :: found

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


!> Parse forcing file into a dictionary.
subroutine forcing_config_parse(self, fields, start_date, &
                                num_land_fields, min_dt, calendar)
    class(forcing_config), intent(inout) :: self
    type(forcing_field), dimension(:), intent(inout) :: fields
    type(datetime), intent(in) :: start_date
    integer, intent(out) :: min_dt, num_land_fields
    character(len=9), intent(out) :: calendar

    type(json_value), pointer :: input_jv_ptr
    integer :: i, dt
    character(len=9) :: calendar_str
    character(kind=CK, len=:), allocatable :: product_name

    type(json_value), pointer :: root, inputs
    logical :: found, is_land_field

    call assert(size(fields) == self%num_inputs, &
                "Insufficient number of fields allocated.")

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
        call self%parse_input(input_jv_ptr, fields(i), start_date, &
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

    character(kind=CK, len=:), allocatable :: cname, fieldname, domain_str
    character(kind=CK, len=:), allocatable :: filename, perturbation_filename
    character(kind=CK, len=:), allocatable :: comment
    character(kind=CK, len=:), allocatable :: perturbation_type
    character(kind=CK, len=:), allocatable :: dimension_type
    character(kind=CK, len=:), allocatable :: perturbation_calendar

    character(len=256), dimension(:), allocatable :: fieldname_list, filename_list

    integer :: num_fieldnames, num_filenames
    integer :: perturbation_constant_value
    logical :: found, domain_found
    integer :: num_perturbations, num_fields
    integer :: i, j

    type(json_value), pointer :: input_field_jv_list,
    type(json_value), pointer :: fieldname_jv_list, filename_jv_list
    type(json_value), pointer :: fieldname_jv_ptr, filename_jv_ptr
    type(json_value), pointer :: perturbation_jv_ptr
    type(json_value), pointer :: dimension_jv_ptr, value_jv_ptr
    type(json_value), pointer :: perturbation_list, dimension_list
    type(json_value), pointer :: value_list

    call self%core%get(input_jv_ptr, "coupling_field_name", cname, found)
    call assert(found, "Entry 'coupling_field_name' not found in forcing config.")

    call self%core%get(input_jv_ptr, "realm", realm_str, realm_found)
    if (realm_found) then
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
    call self%core%get_child(input_jv_ptr, "input_fields", input_field_jv_list, found)
    num_input_fields = self%core%count(input_field_jv_list)
    do i=1, num_input_fields

        call self%core%get_child(inputs, i, input_jv_ptr, found)

        call self%core%get(input_jv_list, i, &
                          fieldname_jv_ptr, found)
        call assert(found, "Expected to find fieldname entry.")
        call self%core%get(fieldname_jv_ptr, value=fieldname)
        fieldname_list(i) = trim(fieldname)

        call self%core%get_child(filename_jv_list, i, &
                                 filename_jv_ptr, found)
        call assert(found, "Expected to find filename entry.")
        call self%core%get(filename_jv_ptr, value=filename)
        filename_list(i) = trim(filename)

        call self%core%get_child(fieldname_jv_list, i, &
                                 fieldname_jv_ptr, found)
        call assert(found, "Expected to find fieldname entry.")
        call self%core%get(fieldname_jv_ptr, value=fieldname)
        fieldname_list(i) = trim(fieldname)

        call self%core%get_child(filename_jv_list, i, &
                                 filename_jv_ptr, found)
        call assert(found, "Expected to find filename entry.")
        call self%core%get(filename_jv_ptr, value=filename)
        filename_list(i) = trim(filename)

    enddo

    call field_ptr%init(fieldname_list, filename_list, cname, realm_str, start_date, &
                        product_name, self%logger, dt, forcing_calendar)



end subroutine forcing_config_parse_field


subroutine forcing_config_parse_permutations(self)


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


end subroutine forcing_config_parse_permutations(self)


subroutine forcing_config_deinit(self)
    class(forcing_config), intent(inout) :: self

    call self%core%destroy()
    call self%json%destroy()

end subroutine forcing_config_deinit

endmodule forcing_config_mod
    dt = self%dt
    calendar = self%calendar

endsubroutine forcing_field_init


subroutine forcing_field_update(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    character(len=1024) :: filename
    character(len=10) :: int_str
    integer :: indx, test_indx
    integer :: num_file_inputs, i

    num_file_inputs = size(self%ncvars)

    do i=1, num_file_inputs
        filename = filename_for_year(self%filename_templates(i), forcing_date%getYear())
        call assert(trim(filename) /= '', "File not found: "//filename)
        if (trim(filename) /= trim(self%ncvars(i)%filename)) then
            call self%ncvars(i)%refresh(filename)
        endif
    enddo

    indx = self%ncvars(1)%get_index_for_datetime(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvars(1)%get_index_for_datetime(forcing_date, .true.)
        call self%logger%write(LOG_DEBUG, &
                 'forcing_field_update: long forcing index search')
    endif
    call assert(indx /= -1, &
                "No forcing date "//forcing_date%isoformat()//" in "// &
                trim(filename))
    call self%logger%write(LOG_DEBUG, '{ "forcing_field_update-file" : "'// &
                                      trim(filename)//'" }')
    write(int_str, '(I10)') indx
    call self%logger%write(LOG_DEBUG, '{ "forcing_field_update-index" : '// &
                                       trim(int_str)//' }')

    ! If there are other ncvars then we need to check that they have
    ! give us the same index. This should be fast in the no-error case.
    if (num_file_inputs > 1) then
        do i=2,num_file_inputs
            test_indx = self%ncvars(i)%get_index_for_datetime(forcing_date, &
                                                             guess=indx)
            call assert(test_indx == indx, &
                    'Time indices on multi-file forcing do not match.')
        enddo
    endif

    call self%calculate(indx, self%data_array)
    call self%apply_perturbations(forcing_date, experiment_date)

end subroutine forcing_field_update


subroutine forcing_field_calculate(self, file_index, result_array)

    class(forcing_field), intent(inout) :: self
    integer, intent(in) :: file_index
    real, dimension(:, :), intent(inout) :: result_array

    if (trim(self%product_name) == 'ERA5') then
        if (trim(self%coupling_name) == 'rain_ai') then
            ! Rain is calculated as total precipitation - snow
            ! FIXME: do calculation with field name checks
            call self%ncvars(1)%read_data(file_index, result_array)
        elseif (trim(self%coupling_name) == 'qair_ai') then
            ! Humidity is calculated using surface temperature and dew point
            ! FIXME: do calculation with field name checks
            call self%ncvars(1)%read_data(file_index, result_array)
        else
            call self%ncvars(1)%read_data(file_index, result_array)
        endif
    elseif (trim(self%product_name) == 'JRA55-do') then
        call self%ncvars(1)%read_data(file_index, result_array)
    else
        call assert(.false., &
            'Unsupported forcing_product_name. Valid names are: "ERA5", "JRA55-do"')
    endif

endsubroutine forcing_field_calculate


! Iterate through perturbations and apply to base field in self%data
subroutine forcing_field_apply_perturbations(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    integer :: i
    character(len=10) :: int_str
    real, dimension(:, :), allocatable :: pertub_array, tmp, another_tmp
    integer :: num_scaling_perturbations, num_offset_perturbations
    integer :: num_separable_perturbations
    logical :: found

    if (size(self%perturbations) == 0) then
        return
    endif

    allocate(pertub_array(self%ncvars(1)%nx, self%ncvars(1)%ny))
    allocate(tmp(self%ncvars(1)%nx, self%ncvars(1)%ny))
    allocate(another_tmp(self%ncvars(1)%nx, self%ncvars(1)%ny))
    pertub_array(:, :) = 1.0

    ! First iterate over all of the scaling fields
    found = .false.
    num_scaling_perturbations = 0
    do i=1, size(self%perturbations)
        if (self%perturbations(i)%perturbation_type == &
            FORCING_PERTURBATION_TYPE_SCALING) then
            call self%perturbations(i)%load(forcing_date, experiment_date, tmp, &
                                            found)
            if (found) then
                pertub_array = pertub_array * tmp
                num_scaling_perturbations = num_scaling_perturbations + 1
            endif
        endif
    enddo

    ! Scale data
    if (found) then
        self%data_array(:, :) = self%data_array(:, :) * pertub_array(:, :)
    endif

    found = .false.
    num_offset_perturbations = 0
    pertub_array(:, :) = 0.0
    ! Iterate over offset fields
    do i=1, size(self%perturbations)
        if (self%perturbations(i)%perturbation_type == &
            FORCING_PERTURBATION_TYPE_OFFSET) then
            call self%perturbations(i)%load(forcing_date, experiment_date, tmp, &
                                            found)
            if (found) then
                pertub_array = pertub_array + tmp
                num_offset_perturbations = num_offset_perturbations + 1
            endif
        endif
    enddo

    ! Offset data
    if (found) then
        self%data_array(:, :) = self%data_array(:, :) + pertub_array(:, :)
    endif

    ! Do separable perturbations
    found = .false.
    num_separable_perturbations = 0
    pertub_array(:, :) = 0.0
    ! Iterate over offset fields
    do i=1, size(self%perturbations)
        if (self%perturbations(i)%perturbation_type == &
            FORCING_PERTURBATION_TYPE_SEPARABLE) then
            call self%perturbations(i)%load(forcing_date, experiment_date, tmp, &
                                            found)
            if (found) then
                call assert(self%separated_perturbations(i)%valid, &
                       'forcing_field_apply_perturbations: invalid perturbation')
                call self%separated_perturbations(i)%load(forcing_date, &
                                                experiment_date, &
                                                another_tmp, found)
                call assert(found, &
                        'forcing_field_apply_perturbations: '// &
                          'seprable permutation not found')
                pertub_array = pertub_array + (tmp(:, :)*another_tmp(:, :))
                num_separable_perturbations = num_separable_perturbations + 1
            endif
        endif
    enddo

    ! Separable data
    if (found) then
        self%data_array(:, :) = self%data_array(:, :) + pertub_array(:, :)
    endif

    if (num_offset_perturbations > 0 .or. &
            num_scaling_perturbations > 0) then
        write(int_str, '(I10)') num_scaling_perturbations
        call self%logger%write(LOG_DEBUG, &
               '{ "forcing_field_apply_perturbations-scaling_count" : "'// &
                           trim(int_str)//'" }')
        write(int_str, '(I10)') num_offset_perturbations
        call self%logger%write(LOG_DEBUG, &
               '{ "forcing_field_apply_perturbations-offset_count" : "'// &
                           trim(int_str)//'" }')
        write(int_str, '(I10)') num_separable_perturbations
        call self%logger%write(LOG_DEBUG, &
               '{ "forcing_field_apply_perturbations-separable_count" : "'// &
                           trim(int_str)//'" }')
        call self%logger%write(LOG_DEBUG, &
               '{ "forcing_field_apply_perturbations-forcing_date" : "'// &
                           trim(forcing_date%isoformat())//'" }')
        call self%logger%write(LOG_DEBUG, &
               '{ "forcing_field_apply_perturbations-experiment_date" : "'// &
                           trim(experiment_date%isoformat())//'" }')
    endif


endsubroutine forcing_field_apply_perturbations

function get_shape(self)
    class(forcing_field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule forcing_field_mod
