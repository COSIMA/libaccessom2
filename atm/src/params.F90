module params_mod

implicit none

private

type, public :: params
    character(len=8) :: log_level
    character(len=1024) :: runoff_remap_weights_file
    character(len=1024) :: forcing_file
contains
    procedure, pass(self), public :: init => params_init
endtype params

character(len=1024) :: runoff_remap_weights_file, forcing_file
character(len=8) :: log_level

namelist /atm_nml/ log_level, runoff_remap_weights_file

contains

subroutine params_init(self)

    class(params), intent(inout) :: self

    log_level = 'ERROR'
    runoff_remap_weights_file = 'rmp_jra55_runoff_cice_conserve.nc'
    forcing_file = 'forcing.json'

    ! Read input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, nml=atm_nml)
    close(unit=99)

    self%log_level = log_level
    self%runoff_remap_weights_file = runoff_remap_weights_file
    self%forcing_file = forcing_file

endsubroutine params_init

endmodule params_mod
