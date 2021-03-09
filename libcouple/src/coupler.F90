module coupler_mod

use mpi
use mod_oasis, only : oasis_init_comp, oasis_def_var, &
                      oasis_def_partition, oasis_enddef, OASIS_OK, OASIS_REAL, &
                      OASIS_RECVD, OASIS_SENT, OASIS_SENTOUT, OASIS_TOREST, &
                      OASIS_TORESTOUT, OASIS_IN, OASIS_OUT, &
                      oasis_put, oasis_get, oasis_terminate, oasis_get_localcomm
use datetime_module, only : datetime
use error_handler, only : assert
use logger_mod, only : logger_type => logger, LOG_DEBUG
use field_mod, only : field_type => field

use mod_oasis_data, only : mpi_root_global, prism_nmodels, prism_modnam

implicit none
private
public coupler

type coupler
    private

    integer :: comp_id  ! Component ID
    integer :: size     ! Total number of processes

    integer, public :: localcomm
    integer, public :: my_local_pe, my_global_pe
    integer, public :: atm_root, ice_root, ocean_root

    character(len=6) :: model_name

    type(logger_type), pointer :: logger

contains
    private
    procedure, pass(self), public :: init_begin => coupler_init_begin
    procedure, pass(self), public :: init_end => coupler_init_end
    procedure, pass(self), public :: deinit => coupler_deinit
    procedure, pass(self), public :: init_field => coupler_init_field
    procedure, pass(self), public :: put => coupler_put
    procedure, pass(self), public :: get => coupler_get
    procedure, pass(self) :: write_checksum
endtype coupler

contains

subroutine coupler_init_begin(self, model_name, comm_world, loggerin, config_dir)
    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: model_name
    integer, intent(in) :: comm_world
    type(logger_type), optional, target, intent(in) :: loggerin
    character(len=*), optional, intent(in) :: config_dir

    character(len=*), parameter :: coupler_nml_fname = 'accessom2.nml'

    integer :: tmp_unit, n, err
    logical :: initialized

    ! Validate model names
    call assert(model_name == 'matmxx' .or. model_name == 'cicexx' &
                .or. model_name == 'mom5xx' .or. model_name == 'monito', &
                'Bad model name')
    self%model_name = model_name

    call MPI_Initialized(initialized, err)
    if (.not. initialized) then
        call MPI_Init(err)
    endif

    if (present(loggerin)) then
        self%logger => loggerin
    else
        self%logger => null()
    endif

    if (present(config_dir)) then
        call oasis_init_comp(self%comp_id, model_name, err, &
                            config_dir=config_dir, commworld=comm_world)
    else
        call oasis_init_comp(self%comp_id, model_name, err, &
                             commworld=comm_world)
    endif
    call assert(err == OASIS_OK, 'oasis_init_comp')

    call oasis_get_localcomm(self%localcomm, err)
    call assert(err == OASIS_OK, 'oasis_get_localcomm')
    call MPI_Comm_rank(self%localcomm, self%my_local_pe, err)
    call MPI_Comm_rank(comm_world, self%my_global_pe, err)

    ! Get root PE for each model.
    do n = 1, prism_nmodels
        select case (trim(prism_modnam(n)))
            case ('matmxx')
                self%atm_root = mpi_root_global(n)
            case ('cicexx')
                self%ice_root = mpi_root_global(n)
            case ('mom5xx')
                self%ocean_root = mpi_root_global(n)
        end select
    end do

endsubroutine coupler_init_begin

subroutine coupler_init_field(self, field, direction)
    class(coupler), intent(inout) :: self
    class(field_type), intent(inout) :: field
    integer, intent(in) :: direction

    integer, dimension(2) :: var_nodims
    integer, dimension(4) :: var_shape
    integer, dimension(3) :: part_def
    integer :: err, varid, partid, nx, ny
    integer, dimension(2) :: dims

    call assert(direction == OASIS_OUT .or. direction == OASIS_IN, &
                'Bad coupling field direction')

    var_nodims(1) = 2 ! rank of coupling field
    var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    ! Iterate over fields and define oasis partitions and variables.
    ! Apple partition: this only supports monoprocess
    part_def(1) = 1
    part_def(2) = 0
    part_def(3) = product(field%get_shape())
    call oasis_def_partition(partid, part_def, err)

    dims(:) = field%get_shape()
    var_shape(1) = 1       ! min index for the coupling field local dim
    var_shape(2) = dims(1) ! max index for the coupling field local dim
    var_shape(3) = 1
    var_shape(4) = dims(2)
    call oasis_def_var(varid, field%name, partid, &
                       var_nodims, direction, var_shape, &
                       OASIS_REAL, err)
    call assert(err == OASIS_OK, "oasis_def_var() failed")

    field%oasis_varid = varid
    field%oasis_partid = partid

endsubroutine coupler_init_field

subroutine coupler_init_end(self, total_runtime_in_seconds, &
                            coupling_field_timesteps)
    class(coupler), intent(in) :: self
    integer, intent(in) :: total_runtime_in_seconds
    integer, dimension(:), intent(in) :: coupling_field_timesteps

    integer :: err

    call oasis_enddef(err, runtime=total_runtime_in_seconds, &
                      coupling_field_timesteps=coupling_field_timesteps)
endsubroutine coupler_init_end

subroutine coupler_put(self, field, timestamp, err)

    class(coupler), intent(inout) :: self
    class(field_type), intent(inout) :: field
    integer, intent(in) :: timestamp
    integer, intent(out) :: err

    character(len=10) :: timestamp_str

    call oasis_put(field%oasis_varid, timestamp, field%data_array, err)
    ! Only output field checksum if it is actually sent.
    if (err == OASIS_SENT .or. err == OASIS_SENTOUT .or. err == OASIS_TOREST &
           .or. err == OASIS_TORESTOUT) then
        write(timestamp_str, '(I10.10)') timestamp
        call self%write_checksum(trim(self%model_name)//'-'//trim(field%name)//'-'//trim(timestamp_str), &
                            field%data_array)
    endif

endsubroutine coupler_put

subroutine coupler_get(self, field, timestamp, err)

    class(coupler), intent(inout) :: self
    class(field_type), intent(inout) :: field
    integer, intent(in) :: timestamp
    integer, intent(out) :: err

    character(len=10) :: timestamp_str

    call oasis_get(field%oasis_varid, timestamp, field%data_array, err)
    write(timestamp_str, '(I10.10)') timestamp
    call self%write_checksum(trim(self%model_name)//'-'//trim(field%name)//'-'//trim(timestamp_str), &
                             field%data_array)

endsubroutine coupler_get

subroutine coupler_deinit(self)
    class(coupler), intent(in) :: self

    integer :: err

    call oasis_terminate(err)
    call assert(err == OASIS_OK, 'oasis_terminate')

endsubroutine coupler_deinit

subroutine write_checksum(self, name, array)
    class(coupler), intent(inout) :: self
    character(len=*), intent(in) :: name
    real, dimension(:,:), intent(in) :: array

    real :: checksum
    character(len=17) :: checksum_str

    ! FIXME: come up with a better way to do checksums
    if (associated(self%logger)) then
        write(checksum_str, '(E17.10E3)') sum(array)
        call self%logger%write(LOG_DEBUG, '{ "checksum-'//trim(name)//'": '//checksum_str//' }')
    endif

endsubroutine write_checksum

endmodule coupler_mod
