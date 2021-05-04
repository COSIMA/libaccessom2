module forcing_field_mod

use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG
use datetime_module, only : datetime
use forcing_perturbation_mod, only : forcing_perturbation_type => forcing_perturbation
use forcing_perturbation_mod, only : FORCING_PERTURBATION_TYPE_SCALING, &
                                     FORCING_PERTURBATION_TYPE_OFFSET
use ncvar_mod, only : ncvar_type => ncvar
use util_mod, only : filename_for_year

implicit none
private

! Forcing fields can have a domain
integer, parameter, public :: FORCING_FIELD_DOMAIN_NONE = 0
integer, parameter, public :: FORCING_FIELD_DOMAIN_ATMOSPHERE = 10
integer, parameter, public :: FORCING_FIELD_DOMAIN_LAND = 20

type, public :: forcing_field
    character(len=64) :: name
    character(len=64) :: coupling_name
    character(len=1024) :: filename_template
    integer :: domain

    integer :: dt
    character(len=9) :: calendar

    type(ncvar_type) :: ncvar
    real, dimension(:, :), allocatable :: data_array
    type(forcing_perturbation_type), dimension(:), allocatable :: perturbations

    type(logger_type), pointer :: logger
contains
    procedure, pass(self), public :: init => forcing_field_init
    procedure, pass(self), public :: update => forcing_field_update
    procedure, pass(self), private :: apply_perturbations => &
                forcing_field_apply_perturbations
    procedure, pass(self), public :: get_shape
endtype forcing_field

contains

subroutine forcing_field_init(self, name, filename_template, cname, domain, &
                              start_date, loggerin, dt, calendar)
    class(forcing_field), intent(inout) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: cname
    character(len=*), intent(in) :: filename_template
    character(len=*), intent(in) :: domain
    type(datetime), intent(in) :: start_date
    type(logger_type), target, intent(in) :: loggerin
    integer, intent(out) :: dt
    character(len=9), intent(out) :: calendar

    character(len=1024) :: filename

    self%name = name
    self%coupling_name = cname
    self%filename_template = filename_template
    if (domain == 'atmosphere') then
        self%domain = FORCING_FIELD_DOMAIN_ATMOSPHERE
    else
        call assert(trim(domain) == 'land', &
                    "Invalid domain value.")
        self%domain = FORCING_FIELD_DOMAIN_LAND
    endif

    self%logger => loggerin

    filename = filename_for_year(self%filename_template, start_date%getYear())
    call self%ncvar%init(self%name, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt
    dt = self%dt
    self%calendar = self%ncvar%calendar
    calendar = self%calendar

endsubroutine forcing_field_init

subroutine forcing_field_update(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    character(len=1024) :: filename
    character(len=10) :: int_str
    integer :: indx

    filename = filename_for_year(self%filename_template, forcing_date%getYear())
    call assert(trim(filename) /= '', "File not found: "//filename)
    if (trim(filename) /= trim(self%ncvar%filename)) then
        call self%ncvar%refresh(filename)
    endif

    indx = self%ncvar%get_index_for_datetime(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvar%get_index_for_datetime(forcing_date, .true.)
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

    call self%ncvar%read_data(indx, self%data_array)

    call self%apply_perturbations(forcing_date, experiment_date)

end subroutine forcing_field_update


! Iterate through perturbations and apply to base field in self%data
subroutine forcing_field_apply_perturbations(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    integer :: i
    character(len=10) :: int_str
    real, dimension(:, :), allocatable :: pertub_array, tmp

    write(int_str, '(I10)') size(self%perturbations)
    call self%logger%write(LOG_DEBUG, '{ "forcing_field_apply_perturbations-count" : "'// &
                                      trim(int_str)//'" }')

    if (size(self%perturbations) == 0) then
        return
    endif

    allocate(pertub_array(self%ncvar%nx, self%ncvar%ny))
    allocate(tmp(self%ncvar%nx, self%ncvar%ny))
    pertub_array(:, :) = 1.0

    ! First iterate over all of the scaling fields
    do i=1, size(self%perturbations)
        if (self%perturbations(i)%perturbation_type == &
            FORCING_PERTURBATION_TYPE_SCALING) then
            call self%perturbations(i)%load(forcing_date, experiment_date, tmp)
            pertub_array = pertub_array * tmp
        endif
    enddo

    ! Scale data
    self%data_array(:, :) = self%data_array(:, :) * pertub_array(:, :)

     pertub_array(:, :) = 0.0
    ! Iterate over offset fields
    do i=1, size(self%perturbations)
        if (self%perturbations(i)%perturbation_type == &
            FORCING_PERTURBATION_TYPE_OFFSET) then
            call self%perturbations(i)%load(forcing_date, experiment_date, tmp)
            pertub_array = pertub_array + tmp
        endif
    enddo

    ! Offset data
    self%data_array(:, :) = self%data_array(:, :) + pertub_array(:, :)

endsubroutine forcing_field_apply_perturbations

function get_shape(self)
    class(forcing_field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule forcing_field_mod
