module params_mod

implicit none

private

type, public :: params
    logical :: verbose
    integer :: dt
    character(len=256) :: runoff_remap_weights_file
contains
    procedure, pass(self), public :: init => params_init
endtype params

character(len=256) :: runoff_remap_weights_file
integer :: dt
logical :: verbose

namelist /atm_nml/ verbose, runoff_remap_weights_file, dt

contains

subroutine params_init(self)

    class(params), intent(inout) :: self

    verbose = .false.
    runoff_remap_weights_file = 'rmp_jra55_runoff_cice_conserve.nc'
    dt = 3600

    ! Read input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, nml=atm_nml)
    close(unit=99)

    self%verbose = verbose
    self%runoff_remap_weights_file = trim(runoff_remap_weights_file)
    ! FIXME: this should go away. dt will depend on the forcing frequency.
    ! Keeping it for the time being for compatibility with ACCESS-OM2.
    self%dt = dt

endsubroutine params_init

endmodule params_mod
