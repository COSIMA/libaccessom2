module coupler_mod

use mpi
use mod_oasis, only : oasis_init_comp, oasis_def_var, oasis_get_localcomm, &
                      oasis_def_partition, oasis_enddef, OASIS_OK, OASIS_REAL, &
                      OASIS_IN, OASIS_OUT, oasis_put, oasis_get, oasis_terminate
use datetime_module, only : datetime, timedelta
use error_handler, only : assert

use, intrinsic :: iso_fortran_env, only : stdout=>output_unit

implicit none
private
public coupler

type field
    private
    integer :: varid
    integer :: partid
    character(len=64) :: name
    integer :: nx, ny
endtype field

type coupler
    private

    integer :: comp_id  ! Component ID
    integer :: size     ! Total number of processes

    ! Ice intercommunicator and peer task
    integer :: ice_intercomm
    integer :: ice_task

    character(len=6) :: model_name

    type(datetime) :: start_date

    type(field), dimension(:), allocatable :: fields
    ! Index of next field index to write to
    integer :: next

contains
    private
    procedure, pass(self), public :: init_begin => coupler_init_begin
    procedure, pass(self), public :: init_end => coupler_init_end
    procedure, pass(self), public :: deinit => coupler_deinit
    procedure, pass(self), public :: add_field => coupler_add_field
    procedure, pass(self), public :: put => coupler_put
    procedure, pass(self), public :: get => coupler_get
    procedure, pass(self), public :: sync => coupler_sync
    procedure, pass(self), public :: get_ice_intercomm
endtype coupler

contains

subroutine coupler_init_begin(self, model_name, start_date, num_fields)

    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: model_name
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: num_fields

    integer :: err

    call assert(model_name == 'matmxx' .or. model_name == 'cicexx', &
                'Bad model name')
    self%model_name == model_name

    call MPI_INIT(err)
    call oasis_init_comp(self%comp_id, model_name, err)
    call assert(err == OASIS_OK, 'oasis_init_comp')

    ! Get an intercommunicator with the ice.
    if (model_name == 'atmxx') then
        call oasis_get_intercomm(self%ice_intercomm, 'cicexx', err)
        call MPI_Comm_Rank(self%ice_intercomm, self%ice_task, err)
    endif

    self%start_date = start_date
    allocate(self%fields(num_fields))
    self%next = 1

endsubroutine coupler_init_begin

subroutine coupler_add_field(self, field_name, dims, direction)
    class(coupler), intent(inout) :: self
    character(len=64), intent(in) :: field_name
    integer, dimension(2), intent(in) :: dims
    integer, intent(in) :: direction

    integer, dimension(2) :: var_nodims
    integer, dimension(4) :: var_shape
    integer, dimension(3) :: part_def
    integer :: err, varid, partid

    call assert(direction == OASIS_OUT .or. direction == OASIS_IN, &
                'Bad coupling field direction')

    var_nodims(1) = 2 ! rank of coupling field
    var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    ! Iterate over fields and define oasis partitions and variables.
    ! This only supports a monoprocess atm/ice_stub
    part_def( clim_strategy ) = clim_serial
    part_def( clim_offset   ) = 0
    part_def( clim_length   ) = dims(1)*dims(2)
    call oasis_def_partition(partid, part_def, err)

    var_shape(1) = 1       ! min index for the coupling field local dim
    var_shape(2) = dims(1) ! max index for the coupling field local dim
    var_shape(3) = 1
    var_shape(4) = dims(2)
    call oasis_def_var(varid, trim(field_name), partid, &
                       var_nodims, direction, var_shape, &
                       OASIS_REAL, err)
    call assert(err == OASIS_OK, "oasis_def_var() failed")

    call assert(self%next <= size(self%fields), 'coupler_add_field: bad next index')
    self%fields(self%next)%varid = varid
    self%fields(self%next)%partid = partid
    self%fields(self%next)%name = trim(field_name)
    self%fields(self%next)%nx = dims(1)
    self%fields(self%next)%ny = dims(2)
    self%next = self%next + 1

endsubroutine coupler_add_field

subroutine coupler_init_end(self)
    class(coupler), intent(in) :: self

    integer :: err

    call prism_enddef_proto(err)
endsubroutine coupler_init_end

subroutine coupler_put(self, name, data, date, debug)

    class(coupler), intent(in) :: self
    character(len=*), intent(in) :: name
    real, dimension(:,:), intent(in) :: data
    type(datetime), intent(in) :: date
    logical, optional, intent(in) :: debug

    integer :: err, i, varid
    type(timedelta) :: td

    if (present(debug)) then
        if (debug) then
            write(stdout, *) 'chksum '//trim(name)//':', sum(data)
        endif
    endif

    ! Find the varid for this name
    varid = -1
    do i=1, size(self%fields)
        if (trim(name) == self%fields(i)%name) then
            varid = self%fields(i)%varid
        endif
    enddo
    call assert(varid /= -1, 'coupler_put: field not found')

    ! Convert date to number of seconds since start
    td = date - self%start_date

    call oasis_put(varid, td%getSeconds(), data, err)
    call assert(err == OASIS_OK, 'oasis_put')

endsubroutine coupler_put

subroutine coupler_get(self, name, data, date, debug)

    class(coupler), intent(in) :: self
    character(len=*), intent(in) :: name
    real, dimension(:,:), intent(inout) :: data
    type(datetime), intent(in) :: date
    logical, optional, intent(in) :: debug

    integer :: err, i, varid
    type(timedelta) :: td

    if (present(debug)) then
        if (debug) then
            write(stdout, *) 'chksum '//trim(name)//':', sum(data)
        endif
    endif

    ! Find the varid for this name
    varid = -1
    do i=1, size(self%fields)
        if (trim(name) == self%fields(i)%name) then
            varid = self%fields(i)%varid
        endif
    enddo
    call assert(varid /= -1, 'coupler_put: field not found')

    ! Convert date to number of seconds since start
    td = date - self%start_date

    call oasis_get(varid, td%getSeconds(), data, err)
    call assert(err == OASIS_OK, 'oasis_get')

endsubroutine coupler_get

subroutine coupler_sync(self, model_name)

    class(coupler), intent(inout) :: self

    integer :: stat(MPI_STATUS_SIZE)
    integer, dimension(1) :: buf
    integer :: err, tag

    tag = MPI_ANY_TAG
    call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm,  stat, err)

endsubroutine coupler_sync

subroutine coupler_deinit(self)

    class(coupler), intent(in) :: self

    integer :: err

    call oasis_terminate(err)
    call assert(err == OASIS_OK, 'oasis_terminate')

    call MPI_Finalize(err)

endsubroutine coupler_deinit

pure elemental integer function get_ice_intercomm(self)
    class(coupler), intent(in) :: self
    get_ice_intercomm = self%ice_intercomm
endfunction

endmodule coupler_mod
