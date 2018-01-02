module cpl_interfaces

use mpi
use mod_prism
use dictionary

use remap_runoff_mod, only : remap_runoff_class, remap_runoff_do
use remap_runoff_mod, only : remap_runoff_new, remap_runoff_del

implicit none

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

type(remap_runoff_class) :: remap_runoff

contains

subroutine init_coupler(field_dict)

    type(dict), intent(in) :: field_dict

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

subroutine init_remap_runoff

    integer :: tag
    integer(kind=int_kind), dimension(2) :: buf_int
    real(kind=dbl_kind), dimension(:), allocatable :: buf_real
    integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)

    ! Receive dimensions of the ice grid that we're coupled to.

    tag = MPI_ANY_TAG
    call MPI_recv(buf_int, 2, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
    nx_global_ice = buf_int(1)
    ny_global_ice = buf_int(2)

    allocate(ice_lats(nx_global_ice, ny_global_ice))
    allocate(ice_lons(nx_global_ice, ny_global_ice))
    allocate(ice_mask(nx_global_ice, ny_global_ice))
    allocate(buf_real(nx_global_ice*ny_global_ice))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_lats(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_lons(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_mask(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))
    deallocate(buf_real)

    ! Initialise module level variables with details about the ice grid.
    call recv_grid_from_ice()
    if (trim(dataset) == 'jra55') then
      call remap_runoff_new(remap_runoff, 'rmp_jrar_to_cict_CONSERV.nc', &
                            ice_lats, ice_lons, ice_mask, &
                            num_runoff_caps, runoff_caps, &
                            runoff_caps_is, runoff_caps_ie, &
                            runoff_caps_js, runoff_caps_je)
    else
      call remap_runoff_new(remap_runoff, 'rmp_corr_to_cict_CONSERV.nc', &
                            ice_lats, ice_lons, ice_mask, &
                            num_runoff_caps, runoff_caps, &
                            runoff_caps_is, runoff_caps_ie, &
                            runoff_caps_js, runoff_caps_je)
    endif

end subroutine init_remap_runoff


subroutine coupler_send(fieldname, timestamp)

  integer(kind=int_kind), intent(in) :: istep1
  integer(kind=int_kind) :: i, jf, tag
  integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)
  integer(kind=int_kind) :: buf(1)

  if (debug_output) then
    print*, '[atm chksum] filename:', sum(work)
  endif

  ! Check whether runoff has changed before remapping.
  if (.not. all(runof(:, :) == runof_save(:, :))) then
    call remap_runoff_do(remap_runoff, runof, remapped_runoff, ice_mask)
    runof_save(:, :) = runof(:, :)
  endif

  call prism_put_proto(il_var_id_out(1), istep1, swfld, ierror)

end subroutine coupler_send

subroutine coupler_sync

       if (my_commice_task == 0) then
            tag = MPI_ANY_TAG
            call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
        endif


end subroutine coupler_sync

subroutine deinit_coupler()

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

end subroutine coupler_termination

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

end module cpl_interfaces
