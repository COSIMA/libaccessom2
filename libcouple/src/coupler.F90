module coupler_mod

use mpi
use mod_prism

implicit none
private
public coupler_type

type coupler_type
    private

    integer :: comp_id  ! Component ID
    integer :: size     ! Total number of processes

    ! Ice intercommunicator and peer task
    integer :: ice_intercomm
    integer :: ice_task

contains
    private
    type(datetime) :: start_date
    procedure, public :: init => coupler_init
end type coupler_type

contains

subroutine coupler_init(this, name, start_date, fields)

    class(coupler_type), intent(inout) :: this
    character(len=6), intent(in) :: name
    type(datetime), intent(in) :: start_date
    type(field_type), dimension(:), intent(in) :: fields

    integer, dimension(2) :: fieldshape
    integer, dimension(2) :: var_nodims
    integer, dimension(4) :: var_shape
    integer, dimension(3) :: part_def

    integer :: nx, ny, varid, partid
    integer :: ierror

    this%start_date = start_date

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

end subroutine coupler_init

subroutine oasis_init(this, comp_name)

    character(len=6), intent(in) :: comp_name
    class(coupler_type), intent(inout) :: this

    integer :: ierror

    call MPI_INIT(ierror)
    call prism_init_comp_proto(this%comp_id, 'matmxx', ierror)
    call assert(ierrror == PRISM_Ok, 'prism_init_comm_proto')

    call prism_get_localcomm_proto(local_comm, ierror)
    call assert(ierrror == PRISM_Ok, 'prism_get_localcomm_proto')

    ! Get an intercommunicator with the ice.
    call prism_get_intercomm(this%ice_intercom, 'cicexx', ierror)
    call MPI_Comm_Rank(this%ice_intercom, this%ice_task, ierror)

end subroutine oasis_init

subroutine coupler_put(this, field, date)

    class(coupler_type), intent(inout) :: this
    type(field_type), intent(in) :: field
    type(datetime), intent(in) :: date
    integer :: ierror

    ! Convert date to number of seconds since start

    call prism_put_proto(field%oasis_id, istep1, field%array(:,:), ierror)

end subroutine coupler_send

subroutine coupler_sync(this)

    class(coupler_type), intent(inout) :: this

    integer :: ierror

    if (my_commice_task == 0) then
        tag = MPI_ANY_TAG
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
    endif

end subroutine coupler_sync

subroutine coupler_deinit(this)

    class(coupler_type), intent(inout) :: this

    integer :: ierror

    call prism_terminate_proto(ierror)
    call assert(ierror == PRISM_Ok, 'prism_terminate_proto')

    call MPI_Finalize(ierror)

end subroutine coupler_deinit

end module coupler_mod
