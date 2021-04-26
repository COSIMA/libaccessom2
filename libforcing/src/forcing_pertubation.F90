module forcing_pertubation_mod

use ncvar_mod, only : ncvar_type => ncvar

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
    type(ncvar_type) :: ncvar
    real, dimension(:, :), allocatable :: data_array
endtype forcing_pertubation


endmodule forcing_pertubation_mod
