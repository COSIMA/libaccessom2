module forcing_pertubation_mod

use error_handler, only : assert
use ncvar_mod, only : ncvar_type => ncvar
use datetime_module, only : datetime
use util_mod, only : filename_for_year

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
    character(len=64) :: name
    character(len=1024) :: filename_template
    integer :: constant_value
    type(ncvar_type) :: ncvar
    logical :: initialised
contains
    procedure, pass(self), public :: init => forcing_pertubation_init
    procedure, pass(self), public :: load => forcing_pertubation_load
endtype forcing_pertubation

contains

subroutine forcing_pertubation_init(self)
    class(forcing_pertubation), intent(inout) :: self

    self%initialised = .false.

end subroutine forcing_pertubation_init


subroutine forcing_pertubation_load(self, forcing_date, experiment_date, &
                                    data_array)

    class(forcing_pertubation), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date
    real, dimension(:, :), intent(inout) :: data_array

    type(datetime) :: date
    integer :: constant_value, indx
    character(len=1024) :: filename

    if (self%dimension_type == FORCING_PERTUBATION_DIMENSION_CONSTANT) then
        data_array(:, :) = self%constant_value
        return
    endif

    if (self%calendar == FORCING_PERTUBATION_CALENDAR_EXPERIMENT) then
        date = experiment_date
    else
        date = forcing_date
    endif

    if (.not. self%initialised) then
        filename = filename_for_year(self%filename_template, date%getYear())

        if (self%dimension_type == FORCING_PERTUBATION_DIMENSION_SPATIAL) then
            call self%ncvar%init(self%name, filename, &
                                 expect_spatial_only=.true.)
        elseif (self%dimension_type == &
                FORCING_PERTUBATION_DIMENSION_TEMPORAL) then
            call self%ncvar%init(self%name, filename, &
                                 expect_temporal_only=.true.)
        else
            call self%ncvar%init(self%name, filename)
        endif
        self%initialised = .true.
    endif

    if (self%dimension_type == FORCING_PERTUBATION_DIMENSION_SPATIAL) then
        call self%ncvar%read_data(-1, data_array)
        return
    endif

    filename = filename_for_year(self%filename_template, date%getYear())
    call assert(trim(filename) /= '', "File not found: "//filename)
    if (trim(filename) /= trim(self%ncvar%filename)) then
        call self%ncvar%refresh(filename)
    endif

    indx = self%ncvar%get_index_for_datetime(date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvar%get_index_for_datetime(date, .true.)
    endif
    call assert(indx /= -1, &
                "No pertubation date "//date%isoformat()//" in "// &
                trim(filename))

    call assert((self%dimension_type == &
                 FORCING_PERTUBATION_DIMENSION_TEMPORAL) .or. &
                (self%dimension_type == &
                FORCING_PERTUBATION_DIMENSION_SPATIOTEMPORAL), &
                'Unexpected pertubation type')
    call self%ncvar%read_data(indx, data_array)

end subroutine forcing_pertubation_load


endmodule forcing_pertubation_mod
