module forcing_field_mod

use error_handler, only : assert

implicit none
private

integer, parameter, public :: FORCING_PERTUBATION_TYPE_SCALING = 0
integer, parameter, public :: FORCING_PERTUBATION_TYPE_OFFSET = 10

integer, parameter, public :: FORCING_PERTUBATION_DIMENSION_SPATIAL = 0
integer, parameter, public :: FORCING_PERTUBATION_DIMENSION_TEMPORAL = 10
integer, parameter, public :: FORCING_PERTUBATION_DIMENSION_SPATIOTEMPORAL = 20
integer, parameter, public :: FORCING_PERTUBATION_DIMENSION_CONSTANT = 30

integer, parameter, public :: FORCING_PERTUBATION_CALENDAR_EXPERIMENT = 0
integer, parameter, public :: FORCING_PERTUBATION_CALENDAR_FORCING = 10

type, public :: forcing_pertubation
    integer :: pertubation_type ! Can be 'scaling' or 'offset'
    integer :: dimension_type   ! Can be 'spatial', 'temporal',
                                ! 'spatiotemporal' or 'constant'
    integer :: calendar         ! Can be 'experiment' or 'forcing'
    character(len=1024) :: filename
    integer :: constant_value
endtype forcing_pertubation


! Forcing fields can have a domain
integer, parameter, public :: FORCING_FIELD_DOMAIN_NONE = 0
integer, parameter, public :: FORCING_FIELD_DOMAIN_ATMOSPHERE = 10
integer, parameter, public :: FORCING_FIELD_DOMAIN_LAND = 20

type, public :: forcing_field
    character(len=64) :: name
    character(len=64) :: cname
    character(len=1024) :: filename
    integer :: domain
    type(forcing_pertubation), dimension(:), allocatable :: pertubations
contains
    procedure, pass(self), public :: init => forcing_field_init
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

endsubroutine forcing_field_init

endmodule forcing_field_mod
