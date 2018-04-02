module coupler_mod

use mpi
use mod_oasis, only : oasis_init_comp, oasis_def_var, oasis_get_intercomm, &
                      oasis_def_partition, oasis_enddef, OASIS_OK, OASIS_REAL, &
                      OASIS_RECVD, OASIS_SENT, OASIS_TOREST, &
                      OASIS_IN, OASIS_OUT, oasis_put, oasis_get, oasis_terminate, &
                      oasis_get_localcomm
use datetime_module, only : datetime, date2num
use error_handler, only : assert
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
    integer :: monitor_intercomm, atm_intercomm, ice_intercomm, ocean_intercomm
    integer :: localcomm
    integer :: my_local_pe

    character(len=6) :: model_name


contains
    private
    procedure, pass(self), public :: init_begin => coupler_init_begin
    procedure, pass(self), public :: init_end => coupler_init_end
    procedure, pass(self), public :: deinit => coupler_deinit
    procedure, pass(self), public :: init_field => coupler_init_field
    procedure, pass(self), public :: put => coupler_put
    procedure, pass(self), public :: get => coupler_get
    procedure, pass(self), public :: atm_ice_sync => coupler_atm_ice_sync
    procedure, pass(self), public :: get_peer_intercomm
endtype coupler

contains

subroutine coupler_init_begin(self, model_name, &
                              total_runtime_in_seconds)
    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: model_name
    integer, intent(in) :: total_runtime_in_seconds

    integer :: err

    call assert(model_name == 'matmxx' .or. model_name == 'cicexx' &
                .or. model_name == 'oceanx' .or. model_name == 'monito', &
                'Bad model name')
    self%model_name = model_name

    call MPI_Init(err)
    call oasis_init_comp(self%comp_id, model_name, err, &
                         total_runtime_in_seconds)
    call assert(err == OASIS_OK, 'oasis_init_comp')

    call oasis_get_localcomm(self%localcomm, err)
    call assert(err == OASIS_OK, 'oasis_get_localcomm')
    call MPI_Comm_rank(self%localcomm, self%my_local_pe, err)

    ! Get an intercommunicator with the peer.
    if (model_name == 'matmxx') then
        call oasis_get_intercomm(self%ice_intercomm, 'cicexx', err)
        call oasis_get_intercomm(self%ocean_intercomm, 'oceanx', err)
    elseif (model_name == 'cicexx') then
        call oasis_get_intercomm(self%atm_intercomm, 'matmxx', err)
    elseif (model_name == 'oceanx') then
        call oasis_get_intercomm(self%atm_intercomm, 'matmxx', err)
    endif

endsubroutine coupler_init_begin

function get_peer_intercomm(self)
    class(coupler), intent(inout) :: self

    integer :: get_peer_intercomm

    call assert(self%model_name == 'matmxx' .or. self%model_name == 'cicexx', &
                'No peer intercomm for '//self%model_name)

    if (self%model_name == 'matmxx') then
        get_peer_intercomm = self%ice_intercomm
    elseif(self%model_name == 'cicexx') then
        get_peer_intercomm = self%atm_intercomm
    endif

endfunction get_peer_intercomm

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

subroutine coupler_put(self, field, timestamp, err)

    class(coupler), intent(in) :: self
    class(field_type), intent(inout) :: field
    integer, intent(in) :: timestamp
    integer, intent(out) :: err

#if defined(DEBUG)
    write(stdout, *) 'chksum '//trim(self%model_name)//' '//trim(field%name)//': ', sum(field%data_array)
#endif

    call oasis_put(field%oasis_varid, timestamp, field%data_array, err)

endsubroutine coupler_put

subroutine coupler_get(self, field, timestamp, err)

    class(coupler), intent(in) :: self
    class(field_type), intent(inout) :: field
    integer, intent(in) :: timestamp
    integer, intent(out) :: err

    call oasis_get(field%oasis_varid, timestamp, field%data_array, err)

#if defined(DEBUG)
    write(stdout, *) 'chksum '//trim(self%model_name)//' '//trim(field%name)//': ', sum(field%data_array)
#endif

endsubroutine coupler_get

subroutine coupler_atm_ice_sync(self)

    class(coupler), intent(inout) :: self

    integer :: stat(MPI_STATUS_SIZE)
    integer, dimension(1) :: buf
    integer :: err, tag, request

    if (self%model_name == 'matmxx') then
        tag = MPI_ANY_TAG
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm, stat, err)
    elseif (self%model_name == 'cicexx') then
        tag = 0
        call MPI_isend(buf, 1, MPI_INTEGER, 0, tag, self%atm_intercomm, request, err)
    endif

endsubroutine coupler_atm_ice_sync

subroutine coupler_deinit(self, cur_date)
    class(coupler), intent(in) :: self
    type(datetime), intent(in) :: cur_date

    integer :: stat(MPI_STATUS_SIZE)
    integer, dimension(1) :: buf
    integer :: err, tag, request
    integer :: checksum

    checksum = date2num(cur_date)

    ! Check that cur_date is the same between all models.
    if (self%model_name == 'matmxx') then
        tag = MPI_ANY_TAG
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm, stat, err)
        call assert(buf(1) == checksum, 'Models are out of sync.')
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ocean_intercomm, stat, err)
        call assert(buf(1) == checksum, 'Models are out of sync.')
    elseif (self%model_name == 'cicexx') then
        tag = 0
        buf(1) = checksum
        call MPI_isend(buf, 1, MPI_INTEGER, 0, tag, self%atm_intercomm, request, err)
    elseif (self%model_name == 'oceanx') then
        tag = 0
        buf(1) = checksum
        call MPI_isend(buf, 1, MPI_INTEGER, 0, tag, self%atm_intercomm, request, err)
    endif

    call oasis_terminate(err)
    call assert(err == OASIS_OK, 'oasis_terminate')
    call MPI_Finalize(err)

endsubroutine coupler_deinit

endmodule coupler_mod
