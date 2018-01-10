module coupler_mod

use mpi
use mod_prism
use dictionary

implicit none
private
public coupler_type

type coupler_type
    private
contains
    private
    procedure, public :: init => coupler_init
end type coupler_type

integer :: ierror
integer :: il_comp_id     ! Component ID
integer :: il_commlocal   ! Component internal communicator
integer :: il_nbtotproc   ! Total number of processes

integer, dimension(2) :: var_nodims
integer, dimension(4) :: var_shape

! Partition definition
integer, dimension(3) :: part_def
integer, dimension(:), allocatable :: partid
integer, dimension(:), allocatable :: varid

type(dict) :: varid_dict

integer :: il_commice, my_commice_task

integer :: nx_global_ice, ny_global_ice
real, dimension(:, :), allocatable :: ice_lats, ice_lons
real, dimension(:, :), allocatable :: ice_mask
real, dimension(:, :), allocatable :: runof_save
real, dimension(:, :), allocatable :: remapped_runoff

contains

subroutine coupler_init(this, fields)

    class(coupler_type), intent(inout) :: this
    type(field_type), dimension(:), intent(in) :: fields

    type(dict) :: tmp
    integer, dimension(2) :: fieldshape
    integer :: nx, ny

    call init_oasis()
    call init_remap_runoff()

    var_nodims(1) = 2 ! rank of coupling field
    var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    allocate(part_id(len(field_dict)))
    allocate(var_id(len(field_dict)))

    ! Iterate over field_dict keys to define oasis partitions and variables.
    tmp = .first. field_dict
    i = 1
    do while(.not. .empty. tmp)
        call assign(fieldshape, .val. tmp)

        nx = fieldshape(1)
        ny = fieldshape(2)

        part_def( clim_strategy ) = clim_serial
        part_def( clim_offset   ) = 0
        part_def( clim_length   ) = nx*ny
        call prism_def_partition_proto(part_id(i), part_def, ierror)

        var_shape(1) = 1  ! min index for the coupling field local dim
        var_shape(2) = nx ! max index for the coupling field local dim
        var_shape(3) = 1
        var_shape(4) = ny
        call prism_def_var_proto(var_id(i), .key. tmp, part_id(i), &
            var_nodims, PRISM_Out, var_shape, PRISM_Real, ierror)
        i = i + 1

        ! Create dictionary of variable ids. 
        varid_dict = varid_dict // (.key. tmp .kv. var_id(i))

        tmp = .next. tmp
    enddo

    ! PSMILe end of declaration phase 
    call prism_enddef_proto(ierror)

end subroutine init_coupler

subroutine init_oasis

    call MPI_INIT(ierror)
    call prism_init_comp_proto (il_comp_id, cp_modnam, ierror)

    if (ierror /= PRISM_Ok) then 
        call prism_abort_proto(il_comp_id, 'matm prism_init','STOP 1')
    endif

    call prism_get_localcomm_proto(il_commlocal, ierror)
    if (ierror /= PRISM_Ok) then
        call prism_abort_proto(il_comp_id, 'matm prism_init','STOP 3')
    endif

    ! Get an intercommunicator with the ice.
    call prism_get_intercomm(il_commice, 'cicexx', ierror)
    call MPI_Comm_Rank(il_commice, my_commice_task, ierror)

    call MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
    call MPI_Comm_Rank(il_commlocal, my_task, ierror)

end subroutine init_oasis

subroutine coupler_put(field, date)

  integer(kind=int_kind), intent(in) :: istep1
  integer(kind=int_kind) :: i, jf, tag
  integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)
  integer(kind=int_kind) :: buf(1)

  if (debug_output) then
    print*, '[atm chksum] filename:', sum(work)
  endif

  call prism_put_proto(il_var_id_out(1), istep1, swfld, ierror)

end subroutine coupler_send

subroutine coupler_sync

   if (my_commice_task == 0) then
        tag = MPI_ANY_TAG
        call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
    endif

end subroutine coupler_sync

subroutine coupler_deinit()

    deallocate(part_id)

    deallocate(remapped_runoff)
    deallocate(ice_lats)
    deallocate(ice_lons)
    deallocate(ice_mask)

    call prism_terminate_proto(ierror)
    if (ierror /= PRISM_Ok) then
        write (il_out,*) 'An error occured in prism_terminate = ', ierror
    endif

    call MPI_Finalize(ierror)

end subroutine coupler_deinit

! Save the atmosphere <-> coupling fields. This will then be used as a restart
! for the ice model.
subroutine coupler_write_restart(fname)

    implicit none

    character(len=*), intent(in) :: fname

    integer(kind=int_kind) :: ncid
    integer(kind=int_kind) :: jf, ll, ilout

    call create_ncfile(fname, ncid, nx_global, ny_global, ilout=il_out)

    call write_nc2D_notime(ncid, 'vwnd_ai', uwnd, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'uwnd_ai', vwnd, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'qair_ai', qair, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'tair_ai', tair, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'runof_ai', runof, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'press_ai', press, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'rain_ai', rain, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'snow_ai', snow, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'lwfld_ai', lwfld, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'swfld_ai', swfld, nx_global, ny_global)

    call ncheck(nf_close(ncid))

end subroutine write_restart

end module coupler_mod
