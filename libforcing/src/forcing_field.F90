module forcing_field_mod

use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG
use datetime_module, only : datetime
use forcing_perturbation_mod, only : forcing_perturbation_type => forcing_perturbation
use forcing_perturbation_mod, only : FORCING_PERTURBATION_TYPE_SCALING, &
                                     FORCING_PERTURBATION_TYPE_OFFSET, &
                                     FORCING_PERTURBATION_TYPE_SEPARABLE
use ncvar_mod, only : ncvar_type => ncvar
use util_mod, only : filename_for_date

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
        filename = filename_for_date(self%filename_templates(i), &
                                          start_date)
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
        filename = filename_for_date(self%filename_templates(i), &
                                     forcing_date)
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
