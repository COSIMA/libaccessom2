module forcing_field_mod

use error_handler, only : assert
use datetime_module, only : datetime
use forcing_pertubation_mod, only : forcing_pertubation_type => forcing_pertubation
use ncvar_mod, only : ncvar_type => ncvar

implicit none
private

! Forcing fields can have a domain
integer, parameter, public :: FORCING_FIELD_DOMAIN_NONE = 0
integer, parameter, public :: FORCING_FIELD_DOMAIN_ATMOSPHERE = 10
integer, parameter, public :: FORCING_FIELD_DOMAIN_LAND = 20

type, public :: forcing_field
    character(len=64) :: name
    character(len=64) :: cname
    character(len=1024) :: filename
    integer :: domain
    type(datetime) :: timestamp

    integer :: dt
    type(ncvar_type) :: ncvar
    real, dimension(:, :), allocatable :: data_array
    type(forcing_pertubation_type), dimension(:), allocatable :: pertubations

contains
    procedure, pass(self), public :: init => forcing_field_init
    procedure, pass(self), public :: update => forcing_field_update
    procedure, pass(self), private :: apply_pertubations => &
                forcing_field_apply_pertubations
endtype forcing_field

contains

subroutine forcing_field_init(self, name, filename, cname, domain)
    class(forcing_field), intent(inout) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: cname
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: domain

    self%name = name
    self%cname = cname
    self%filename = filename
    if (domain == 'atmosphere') then 
        self%domain = FORCING_FIELD_DOMAIN_ATMOSPHERE
    else
        call assert(trim(domain) == 'land', &
                    "Invalid domain value.")
        self%domain = FORCING_FIELD_DOMAIN_LAND
    endif

    call self%ncvar%init(name, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt

endsubroutine forcing_field_init


subroutine forcing_field_update(self, filename, forcing_date)
    class(forcing_field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    integer :: indx

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

end subroutine forcing_field_update

! Iterate throught pertubations and apply to base field
subroutine forcing_field_apply_pertubations(self)
    class(forcing_field), intent(inout) :: self

endsubroutine forcing_field_apply_pertubations

endmodule forcing_field_mod
