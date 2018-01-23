module params_mod

use datetime

implicit none

private
public params

type params
    private
    type(datetime) :: start_date, end_date
    integer :: forcing_period_years
    logical :: debug_output
    character(len=256) :: runoff_remap_weights_file
contains
    procedure, pass(self), public :: init => params_init
endtype params

namelist /atm_nml/ start_date, end_date forcing_period_years, &
                   debug_output, runoff_remap_weights

contains

subroutine params_init(self)

    class(params), intent(inout) :: self

    character(len=19) :: start_date, end_date
    character(len=256) :: runoff_remap_weights_file
    integer :: forcing_period_years
    logical :: debug_output

    start_date = '1900-01-01 00:00:00'
    end_date = '1901-01-01 00:00:00'
    forcing_period_years = 1
    debug_output = .false.
    runoff_remap_weights_file = 'rmp_jrar_to_cict_CONSERV.nc'

    ! Rean input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, nml=atm_nml)
    close(unit=99)

    self%start_date = strptime(start_date, '%Y-%m-%d %H:%M:%S')
    self%end_date = strptime(end_date, '%Y-%m-%d %H:%M:%S')
    self%forcing_period_years = forcing_period_years
    self%debug_output = debug_output
    self%runoff_remap_weights_file = trim(runoff_remap_weights_file)

endsubroutine params_init

endmodule params_mod
