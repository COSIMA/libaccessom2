module forcing_perturbation_mod

use error_handler, only : assert
use ncvar_mod, only : ncvar_type => ncvar
use datetime_module, only : datetime
use util_mod, only : filename_for_year

implicit none
private

integer, parameter, public :: FORCING_PERTURBATION_TYPE_SCALING = 1
integer, parameter, public :: FORCING_PERTURBATION_TYPE_OFFSET = 10
integer, parameter, public :: FORCING_PERTURBATION_TYPE_SEPARABLE = 20

integer, parameter, public :: FORCING_PERTURBATION_DIMENSION_SPATIAL = 30
integer, parameter, public :: FORCING_PERTURBATION_DIMENSION_TEMPORAL = 40
integer, parameter, public :: FORCING_PERTURBATION_DIMENSION_SPATIOTEMPORAL = 50
integer, parameter, public :: FORCING_PERTURBATION_DIMENSION_CONSTANT = 60

integer, parameter, public :: FORCING_PERTURBATION_CALENDAR_EXPERIMENT = 70
integer, parameter, public :: FORCING_PERTURBATION_CALENDAR_FORCING = 80

type, public :: forcing_perturbation
    integer :: perturbation_type ! Can be 'scaling', 'offset' or 'seperable'
    integer :: dimension_type   ! Can be 'spatial', 'temporal',
                                ! 'spatiotemporal' or 'constant'
    integer :: calendar         ! Can be 'experiment' or 'forcing'
    integer :: serperable_id
    character(len=64) :: name
    character(len=1024) :: filename_template
    integer :: constant_value
    type(ncvar_type) :: ncvar
    logical :: initialised
    logical :: valid
contains
    procedure, pass(self), public :: init => forcing_perturbation_init
    procedure, pass(self), public :: load => forcing_perturbation_load
endtype forcing_perturbation

contains

subroutine forcing_perturbation_init(self)
    class(forcing_perturbation), intent(inout) :: self

    self%initialised = .false.

    ! FIXME: would be good to check that spatial dimensions of perturbation
    ! and forcing files match.

end subroutine forcing_perturbation_init


subroutine forcing_perturbation_load(self, forcing_date, experiment_date, &
                                    data_array, found)

    class(forcing_perturbation), intent(inout) :: self
    type(datetime), intent(in) :: forcing_date, experiment_date
    real, dimension(:, :), intent(inout) :: data_array
    logical, intent(out) :: found

    type(datetime) :: date
    integer :: constant_value, indx
    character(len=1024) :: filename

    found = .true.

    if (self%dimension_type == FORCING_PERTURBATION_DIMENSION_CONSTANT) then
        data_array(:, :) = self%constant_value
        return
    endif

    if (self%calendar == FORCING_PERTURBATION_CALENDAR_EXPERIMENT) then
        date = experiment_date
    else
        date = forcing_date
    endif

    if (.not. self%initialised) then
        filename = filename_for_year(self%filename_template, date%getYear())

        if (self%dimension_type == FORCING_PERTURBATION_DIMENSION_SPATIAL) then
            call self%ncvar%init(self%name, filename, &
                                 expect_spatial_only=.true.)
        elseif (self%dimension_type == &
                FORCING_PERTURBATION_DIMENSION_TEMPORAL) then
            call self%ncvar%init(self%name, filename, &
                                 expect_temporal_only=.true.)
        else
            call self%ncvar%init(self%name, filename)
        endif
        self%initialised = .true.
    endif

    if (self%dimension_type == FORCING_PERTURBATION_DIMENSION_SPATIAL) then
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
    if (indx == -1) then
        found = .false.
        return
    endif

    call assert((self%dimension_type == &
                 FORCING_PERTURBATION_DIMENSION_TEMPORAL) .or. &
                (self%dimension_type == &
                FORCING_PERTURBATION_DIMENSION_SPATIOTEMPORAL), &
                'Unexpected perturbation type')
    call self%ncvar%read_data(indx, data_array)

end subroutine forcing_perturbation_load


endmodule forcing_perturbation_mod
