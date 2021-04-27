module forcing_field_mod

use error_handler, only : assert
use datetime_module, only : datetime
use forcing_pertubation_mod, only : forcing_pertubation_type => forcing_pertubation
use forcing_pertubation_mod, only : FORCING_PERTUBATION_TYPE_SCALING, &
                                    FORCING_PERTUBATION_TYPE_OFFSET
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
    character(len=64) :: cname
    character(len=1024) :: filename_template
    integer :: domain
    type(datetime) :: timestamp

    integer :: dt
    type(ncvar_type) :: ncvar
    real, dimension(:, :), allocatable :: data_array
    type(forcing_pertubation_type), dimension(:), allocatable :: pertubations

contains
    procedure, pass(self), public :: new => forcing_field_new
    procedure, pass(self), public :: init => forcing_field_init
    procedure, pass(self), public :: update => forcing_field_update
    procedure, pass(self), private :: apply_pertubations => &
                forcing_field_apply_pertubations
endtype forcing_field

contains

subroutine forcing_field_new(self, name, filename, cname, domain)
    class(forcing_field), intent(inout) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: cname
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: domain

    self%name = name
    self%cname = cname
    self%filename_template = filename
    if (domain == 'atmosphere') then
        self%domain = FORCING_FIELD_DOMAIN_ATMOSPHERE
    else
        call assert(trim(domain) == 'land', &
                    "Invalid domain value.")
        self%domain = FORCING_FIELD_DOMAIN_LAND
    endif

endsubroutine forcing_field_new


subroutine forcing_field_init(self, start_date, dt)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: start_date
    integer, intent(out) :: dt

    character(len=1024) :: filename

    filename = filename_for_year(self%filename_template, start_date%getYear())

    call self%ncvar%init(self%name, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt
    dt = self%dt

endsubroutine forcing_field_init


subroutine forcing_field_update(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    character(len=1024) :: filename
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
    endif
    call assert(indx /= -1, &
                "No forcing date "//forcing_date%isoformat()//" in "// &
                trim(filename))

    call self%ncvar%read_data(indx, self%data_array)
    self%timestamp = forcing_date

    call self%apply_pertubations(forcing_date, experiment_date)

end subroutine forcing_field_update


! Iterate through pertubations and apply to base field in self%data
subroutine forcing_field_apply_pertubations(self, forcing_date, experiment_date)
    class(forcing_field), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date

    integer :: i
    logical :: do_scaling
    real, dimension(:, :), allocatable :: pertub_array, tmp

    if (size(self%pertubations) == 0) then
        return
    endif

    allocate(pertub_array(self%ncvar%nx, self%ncvar%ny))
    allocate(tmp(self%ncvar%nx, self%ncvar%ny))
    pertub_array(:, :) = 1.0

    ! First iterate over all of the scaling fields
    do_scaling = .false.
    do i=1, size(self%pertubations)
        if (self%pertubations(i)%pertubation_type == &
            FORCING_PERTUBATION_TYPE_SCALING) then
            call self%pertubations(i)%load(forcing_date, experiment_date, tmp)
            pertub_array = pertub_array * tmp
            do_scaling = .true.
        endif
    enddo

    if (.not. do_scaling) then
        pertub_array(:, :) = 0.0
    endif
    ! Iterate over offset fields
    do i=1, size(self%pertubations)
        if (self%pertubations(i)%pertubation_type == &
            FORCING_PERTUBATION_TYPE_OFFSET) then
            call self%pertubations(i)%load(forcing_date, experiment_date, tmp)
            pertub_array = pertub_array + tmp
        endif
    enddo


endsubroutine forcing_field_apply_pertubations

endmodule forcing_field_mod
