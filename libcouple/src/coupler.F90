module coupler_mod

use mpi
use mod_oasis, only : oasis_init_comp, oasis_def_var, oasis_get_intercomm, &
                      oasis_def_partition, oasis_enddef, OASIS_OK, OASIS_REAL, &
                      OASIS_RECVD, OASIS_SENT, OASIS_TOREST, &
                      OASIS_IN, OASIS_OUT, oasis_put, oasis_get, oasis_terminate
use datetime_module, only : datetime, timedelta
use error_handler, only : assert
use util_mod, only : runtime_in_seconds
use field_mod, only : field_type => field

use, intrinsic :: iso_fortran_env, only : stdout=>output_unit

implicit none
private
public coupler

type coupler
    private

    integer :: comp_id  ! Component ID
    integer :: size     ! Total number of processes

    ! Intercommunicators
    integer :: ice_intercomm, ocean_intercomm
    integer :: localcomm
    integer :: my_local_pe

    character(len=6) :: model_name

    type(datetime) :: start_date

contains
    private
    procedure, pass(self), public :: init_begin => coupler_init_begin
    procedure, pass(self), public :: init_end => coupler_init_end
    procedure, pass(self), public :: deinit => coupler_deinit
    procedure, pass(self), public :: init_field => coupler_init_field
    procedure, pass(self), public :: put => coupler_put
    procedure, pass(self), public :: get => coupler_get
    procedure, pass(self), public :: sync => coupler_sync
    procedure, pass(self), public :: get_peer_intercomm
endtype coupler

contains

subroutine coupler_init_begin(self, model_name, start_date)

    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: model_name
    type(datetime), intent(in) :: start_date

    integer :: err

    call assert(model_name == 'matmxx' .or. model_name == 'cicexx' &
                .or. model_name == 'oceanx', 'Bad model name')
    self%model_name = model_name

    call MPI_Init(err)
    call oasis_init_comp(self%comp_id, model_name, err)
    call assert(err == OASIS_OK, 'oasis_init_comp')

    call oasis_get_localcomm(self%local_comm, err)
    call assert(err == OASIS_OK, 'oasis_get_localcomm')
    call MPI_Comm_rank(self%local_comm, self%my_local_pe, err)

    ! Get an intercommunicator with the peer.
    if (model_name == 'matmxx') then
        call oasis_get_intercomm(self%ice_intercomm, 'cicexx', err)
        call oasis_get_intercomm(self%ocean_intercomm, 'cicexx', err)
    elseif (model_name == 'cicexx') then
        call oasis_get_intercomm(self%ice_intercomm, 'matmxx', err)
    elseif (model_name == 'oceanx') then
        call oasis_get_intercomm(self%ocean_intercomm, 'matmxx', err)
    endif

    self%start_date = start_date

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

subroutine coupler_init_end(self)
    class(coupler), intent(in) :: self

    integer :: err

    call oasis_enddef(err)
endsubroutine coupler_init_end

subroutine coupler_put(self, field, date, debug)

    class(coupler), intent(in) :: self
    class(field_type), intent(inout) :: field
    type(datetime), intent(in) :: date
    logical, optional, intent(in) :: debug

    integer :: err, runtime

    if (present(debug)) then
        if (debug) then
            write(stdout, *) 'chksum '//trim(field%name)//':', sum(field%data_array)
        endif
    endif

    runtime = runtime_in_seconds(self%start_date, date)

    call oasis_put(field%oasis_varid, runtime, field%data_array, err)
    call assert(err == OASIS_OK .or. err == OASIS_SENT .or. err == OASIS_TOREST, 'oasis_put')

endsubroutine coupler_put

subroutine coupler_get(self, field, date, debug)

    class(coupler), intent(in) :: self
    class(field_type), intent(inout) :: field
    type(datetime), intent(in) :: date
    logical, optional, intent(in) :: debug

    integer :: err, runtime

    runtime = runtime_in_seconds(self%start_date, date)

    call oasis_get(field%oasis_varid, runtime, field%data_array, err)
    call assert(err == OASIS_OK .or. err == OASIS_RECVD, 'oasis_get')

    if (present(debug)) then
        if (debug) then
            write(stdout, *) 'chksum '//trim(field%name)//':', sum(field%data_array)
        endif
    endif

endsubroutine coupler_get

subroutine coupler_sync(self, model_name)

    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: model_name

    integer :: stat(MPI_STATUS_SIZE)
    integer, dimension(1) :: buf
    integer :: err, tag, request

    if (model_name == 'matmxx') then
        tag = MPI_ANY_TAG
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm, stat, err)
    elseif (model_name == 'cicexx')
        tag = 0
        call MPI_isend(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm, request, err)
    endif

endsubroutine coupler_sync

subroutine coupler_deinit(self)

    class(coupler), intent(in) :: self

    integer :: err

    call oasis_terminate(err)
    call assert(err == OASIS_OK, 'oasis_terminate')

    call MPI_Finalize(err)

endsubroutine coupler_deinit

pure elemental integer function get_peer_intercomm(self)
    class(coupler), intent(in) :: self
    get_peer_intercomm = self%peer_intercomm
endfunction

endmodule coupler_mod
