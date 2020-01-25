module field_mod

use netcdf, only : nf90_max_name
use ncvar_mod, only : ncvar_type => ncvar
use datetime_module, only : datetime
use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG

implicit none
private

! Fields can be either atmospheric or ocean. YATM will treat the differently,
! see atm.F90 for details.
integer, parameter, public :: FIELD_DOMAIN_NONE = 0
integer, parameter, public :: FIELD_DOMAIN_ATMOSPHERE = 10
integer, parameter, public :: FIELD_DOMAIN_LAND = 20

type, public :: field
    character(len=64) :: name
    integer :: domain
    character(len=1024) :: filename_template
    character(len=1024) :: scaling_filename
    type(datetime) :: timestamp
    integer :: oasis_varid
    integer :: oasis_partid

    integer :: dt
    character(len=9) :: calendar
    type(ncvar_type) :: ncvar, scaling_ncvar
    real, dimension(:, :), allocatable :: data_array
    real, dimension(:, :), allocatable :: scaling_data_array

    type(logger_type) :: logger
contains
    procedure, pass(self), public :: init => field_init
    procedure, pass(self), public :: update_data => field_update_data
    procedure, pass(self), public :: get_shape
endtype field

contains

subroutine field_init(self, name, ncname, filename_template, &
                      filename, domain, logger, scaling_filename)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: name, ncname
    character(len=*), intent(in) :: filename_template, filename
    character(len=*), intent(in) :: domain
    type(logger_type), intent(in) :: logger
    character(len=*), intent(in), optional :: scaling_filename

    self%name = name
    self%filename_template = filename_template
    self%timestamp = datetime(HUGE(1))
    self%logger = logger

    call self%ncvar%init(ncname, filename)
    allocate(self%data_array(self%ncvar%nx, self%ncvar%ny))
    self%data_array(:, :) = HUGE(1.0)
    self%dt = self%ncvar%dt
    self%calendar = self%ncvar%calendar

    if (present(scaling_filename)) then
        self%scaling_filename = scaling_filename
        call self%scaling_ncvar%init(ncname, scaling_filename)
        allocate(self%scaling_data_array(self%ncvar%nx, self%ncvar%ny))
        self%scaling_data_array(:, :) = HUGE(1.0)
    endif

    ! Set the field domain
    self%domain = FIELD_DOMAIN_NONE
    if (domain == 'atmosphere') then
        self%domain = FIELD_DOMAIN_ATMOSPHERE
    elseif (domain == 'land') then
        self%domain = FIELD_DOMAIN_LAND
    endif
    call assert(self%domain /= FIELD_DOMAIN_NONE, &
                'field_init: invalid field domain')

end subroutine

subroutine field_update_data(self, filename, forcing_date)
    class(field), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(datetime), intent(in) :: forcing_date

    integer :: indx
    character(len=10) :: int_str

    if (trim(filename) /= trim(self%ncvar%filename)) then
        call self%ncvar%refresh(filename)
    endif

    indx = self%ncvar%get_index_for_datetime(forcing_date)
    if (indx == -1) then
        ! Search from the beginning before failing
        indx = self%ncvar%get_index_for_datetime(forcing_date, .true.)
        call self%logger%write(LOG_DEBUG, &
                               'field_update_data: long forcing index search')
    endif
    call assert(indx /= -1, &
                "No forcing date "//forcing_date%isoformat()//" in "// &
                trim(filename))

    call self%logger%write(LOG_DEBUG, '{ "field_update_data-file" : "'// &
                                      trim(filename)//'" }')
    write(int_str, '(I10)') indx
    call self%logger%write(LOG_DEBUG, '{ "field_update_data-index" : '// &
                                       trim(int_str)//' }')
    call self%ncvar%read_data(indx, self%data_array)
    self%timestamp = forcing_date

    ! Read the scaling data for this date and apply it (if there is any)
    if (allocated(self%scaling_data_array)) then
        indx = self%scaling_ncvar%get_index_for_datetime(forcing_date, .true.)
        if (indx /= -1) then
            call self%scaling_ncvar%read_data(indx, self%scaling_data_array)
            call self%logger%write(LOG_DEBUG, &
                                  '{ "field_update_data-scaling_file" : "'// &
                                   trim(self%scaling_filename)//'" }')
            call self%logger%write(LOG_DEBUG, &
                                  '{ "field_update_data-scaling_date" : "'// &
                                   forcing_date%isoformat()//'" }')
            self%data_array(:, :) = self%data_array(:, :) * &
                                        self%scaling_data_array(:, :)
        endif
    endif

end subroutine field_update_data

function get_shape(self)
    class(field), intent(in) :: self
    integer, dimension(2) :: get_shape

    get_shape = shape(self%data_array)
endfunction

endmodule field_mod
