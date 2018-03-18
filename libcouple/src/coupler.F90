module coupler_mod

use mpi
use mod_prism
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
    procedure, pass(self), public :: sync => coupler_sync
    procedure, pass(self), public :: get_ice_intercomm
endtype coupler

contains

subroutine coupler_init_begin(self, name, start_date, num_fields)

    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: name
    type(datetime), intent(in) :: start_date
    integer, intent(in) :: num_fields

    integer :: err

    call MPI_INIT(err)
    call prism_init_comp_proto(self%comp_id, trim(name), err)
    call assert(err == PRISM_Ok, 'prism_init_comm_proto')

    ! Get an intercommunicator with the ice.
    call prism_get_intercomm(self%ice_intercomm, 'cicexx', err)
    call MPI_Comm_Rank(self%ice_intercomm, self%ice_task, err)

    self%start_date = start_date
    allocate(self%fields(num_fields))
    self%next = 1

endsubroutine coupler_init_begin

subroutine coupler_add_field(self, field_name, dims)
    class(coupler), intent(inout) :: self
    character(len=64), intent(in) :: field_name
    integer, dimension(2), intent(in) :: dims

    integer, dimension(2) :: var_nodims
    integer, dimension(4) :: var_shape
    integer, dimension(3) :: part_def
    integer :: err, varid, partid

    var_nodims(1) = 2 ! rank of coupling field
    var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    ! Iterate over fields and define oasis partitions and variables.
    part_def( clim_strategy ) = clim_serial
    part_def( clim_offset   ) = 0
    part_def( clim_length   ) = dims(1)*dims(2)
    call prism_def_partition_proto(partid, part_def, err)

    var_shape(1) = 1       ! min index for the coupling field local dim
    var_shape(2) = dims(1) ! max index for the coupling field local dim
    var_shape(3) = 1
    var_shape(4) = dims(2)
    call prism_def_var_proto(varid, trim(field_name), partid, &
                             var_nodims, PRISM_Out, var_shape, &
                             PRISM_Real, err)

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

    call prism_put_proto(varid, td%getSeconds(), data, err)
    call assert(err == PRISM_Ok, 'prism_put_proto')

endsubroutine coupler_put

subroutine coupler_sync(self)

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

    call prism_terminate_proto(err)
    call assert(err == PRISM_Ok, 'prism_terminate_proto')

    call MPI_Finalize(err)

endsubroutine coupler_deinit

pure elemental integer function get_ice_intercomm(self)
    class(coupler), intent(in) :: self
    get_ice_intercomm = self%ice_intercomm
endfunction

endmodule coupler_mod
