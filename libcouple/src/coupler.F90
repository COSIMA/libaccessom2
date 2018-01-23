module coupler_mod

use mpi
use mod_prism
use field

use, intrinsic :: iso_fortran_env, only : stdout=>output_unit

implicit none
private
public coupler

type coupler
    private

    integer :: comp_id  ! Component ID
    integer :: size     ! Total number of processes

    ! Ice intercommunicator and peer task
    integer :: ice_intercomm
    integer :: ice_task

    type(datetime) :: start_date

contains
    private
    procedure, pass(self), public :: init => coupler_init
    procedure, pass(self), public :: put => coupler_put
    procedure, pass(self), public :: get_ice_intercomm
    procedure, pass(self) :: oasis_init
endtype coupler

contains

subroutine coupler_init(self, name, start_date, fields)

    class(coupler), intent(inout) :: self
    character(len=6), intent(in) :: name
    type(datetime), intent(in) :: start_date
    type(field), dimension(:), intent(in) :: fields

    integer, dimension(2) :: fieldshape
    integer, dimension(2) :: var_nodims
    integer, dimension(4) :: var_shape
    integer, dimension(3) :: part_def

    integer :: nx, ny, varid, partid
    integer :: ierror

    self%start_date = start_date

    call oasis_init()

    var_nodims(1) = 2 ! rank of coupling field
    var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    ! Iterate over fields and define oasis partitions and variables.
    do i=1, size(fields)
        nx = size(fields(i), 1)
        ny = size(fields(i), 2)

        part_def( clim_strategy ) = clim_serial
        part_def( clim_offset   ) = 0
        part_def( clim_length   ) = nx*ny
        call prism_def_partition_proto(partid, part_def, ierror)

        var_shape(1) = 1  ! min index for the coupling field local dim
        var_shape(2) = nx ! max index for the coupling field local dim
        var_shape(3) = 1
        var_shape(4) = ny
        call prism_def_var_proto(var_id, trim(fields(i)%name), partid, &
                                 var_nodims, PRISM_Out, var_shape, &
                                 PRISM_Real, ierror)

        field(i)%set_oasis_id(var_id)
    end do

    ! PSMILe end of declaration phase
    call prism_enddef_proto(ierror)

endsubroutine coupler_init

subroutine oasis_init(self, comp_name)

    class(coupler), intent(in) :: self
    character(len=6), intent(in) :: comp_name

    integer :: err

    call MPI_INIT(err)
    call prism_init_comp_proto(self%comp_id, 'matmxx', err)
    call assert(err == PRISM_Ok, 'prism_init_comm_proto')

    call prism_get_localcomm_proto(local_comm, err)
    call assert(err == PRISM_Ok, 'prism_get_localcomm_proto')

    ! Get an intercommunicator with the ice.
    call prism_get_intercomm(self%ice_intercomm, 'cicexx', err)
    call MPI_Comm_Rank(self%ice_intercomm, self%ice_task, err)

endsubroutine oasis_init

subroutine coupler_put(self, name, data, oasis_varid, date, debug)

    class(coupler_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real, dimension(:,:), intent(in) :: data
    integer, intent(in) :: oasis_varid
    type(datetime), intent(in) :: date
    logical, optional, intent(in) :: debug

    integer :: err
    type(timedelta) :: td

    if (present(debug)) the
        if (debug) then
            write(output_unit, *), 'chksum '//trim(name)//':', sum(data)
        endif
    endif

    ! Convert date to number of seconds since start
    td = date - self%start_date

    call prism_put_proto(oasis_varid, td%getSeconds(), data, err)
    call assert(err == PRISM_Ok, 'prism_put_proto')

endsubroutine coupler_put

subroutine coupler_sync(self)

    class(coupler_type), intent(inout) :: self

    integer :: stat(MPI_STATUS_SIZE)
    integer, dimension(1) :: buf
    integer :: err, tag

    tag = MPI_ANY_TAG
    call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, self%ice_intercomm,  stat, err)

endsubroutine coupler_sync

subroutine coupler_deinit(self)

    class(coupler_type), intent(in) :: self

    integer :: err

    call prism_terminate_proto(err)
    call assert(err == PRISM_Ok, 'prism_terminate_proto')

    call MPI_Finalize(err)

endsubroutine coupler_deinit

function integer get_ice_intercomm(self)
    class(coupler_type), intent(inout) :: self
    get_ice_intercomm = self%ice_intercomm
endfunction

endmodule coupler_mod
