module cpl_interfaces

use mpi
use mod_prism

use atm_kinds
use atm_domain
use cpl_parameters
use cpl_arrays
use cpl_forcing_handler

use remap_runoff_mod, only : remap_runoff_class, remap_runoff_do
use remap_runoff_mod, only : remap_runoff_new, remap_runoff_del

implicit none

logical :: mpiflag

integer(kind=int_kind)  :: ierror, ibou
character(len=12) :: chatmout
character(len=6) :: chout 
integer(kind=int_kind) :: il_comp_id     ! Component ID
integer(kind=int_kind) :: il_commlocal   ! Component internal communicator
integer(kind=int_kind) :: il_nbtotproc   ! Total number of processes
integer(kind=int_kind) :: il_nbcplproc   ! Number of processes involved in the coupling
integer(kind=int_kind) :: il_part_id, il_part_runoff_id  ! Local partition ID
integer(kind=int_kind) :: il_length      ! Size of partial field for each process
!integer(kind=int_kind), dimension(2) :: il_var_nodims, il_var_shape ! see below
integer(kind=int_kind), dimension(2) :: il_var_nodims
integer(kind=int_kind), dimension(4) :: il_var_shape
  
integer(kind=int_kind), dimension(3) :: il_paral, il_paral_runoff ! Definition of monoprocess partition
  
integer(kind=int_kind) :: il_commice, my_commice_task   

integer(kind=int_kind) :: il_flag        ! Flag for grid writing
integer(kind=int_kind) :: il_status, il_fileid, il_varid 
integer(kind=int_kind), dimension(2) :: icnt   !,ist ==> what is it?
real(kind=dbl_kind),dimension(nx_global,ny_global) :: dla_lon, dla_lat, dla_srf
integer(kind=int_kind),dimension(nx_global,ny_global) :: ila_msk 
real(kind=dbl_kind),dimension(nx_global,ny_global,4) :: dla_lonb, dla_latb

integer(kind=int_kind) :: nx_global_ice, ny_global_ice
real(kind=dbl_kind), dimension(:, :), allocatable :: ice_lats, ice_lons
real(kind=dbl_kind), dimension(:, :), allocatable :: ice_mask

type(remap_runoff_class) :: remap_runoff

contains

  !======================================================================
  subroutine prism_init
  !-------------------
  ! Initialize PSMILe.
  !-------------------

  
  ! Initialise MPI
  mpiflag = .FALSE.
  call MPI_Initialized (mpiflag, ierror)
!  print *,'MATM: (prism_init) BF MPI_INIT, mpiflag = ',mpiflag
  
  call MPI_INIT(ierror)
  
  call MPI_Initialized (mpiflag, ierror)
!  print *,'MATM: (prism_init) AF MPI_INIT, mpiflag = ',mpiflag
  
!  print *
!  print *, 'MATM: (prism_init) calling prism_init_comp_proto...'
  
  call prism_init_comp_proto (il_comp_id, cp_modnam, ierror)
  
  if (ierror /= PRISM_Ok) then 
      call prism_abort_proto(il_comp_id, 'matm prism_init','STOP 1')
  else
!      print *, 'MATM: (prism_init) called prism_init_comp_proto !'
  endif
  
  if (ierror /= PRISM_Ok) then
!      print *, 'MATM: (prism_init) Error in MPI_Buffer_Attach.'
      call prism_abort_proto(il_comp_id, 'matm prism_init','STOP 2')
  else
!      print *, 'MATM: (prism_init) MPI_Buffer_Attach ok!'
  endif
  
  !  PSMILe attribution of local communicator.
  !
  !   Either MPI_COMM_WORLD if MPI2 is used,
  !   or a local communicator created by Oasis if MPI1 is used.
  
  call prism_get_localcomm_proto(il_commlocal, ierror)
  
  if (ierror /= PRISM_Ok) then
!      print *, 'MATM: Error in prism_get_localcomm_proto'
      call prism_abort_proto(il_comp_id, 'matm prism_init','STOP 3')
  else
      print *, 'MOCN: _get_localcomm_ OK! il_commlocal= ',il_commlocal
  endif

  ! Get an intercommunicator with the ice.
  call prism_get_intercomm(il_commice, 'cicexx', ierror)
  call MPI_Comm_Rank(il_commice, my_commice_task, ierror)

  end subroutine prism_init

  !=======================================================================
  subroutine init_cpl(nx_global_runoff, ny_global_runoff)
  !----------------------------------------------
  ! To initialize/setup the coupling environment
  !----------------------------------------------
  integer(kind=int_kind), intent(in) :: nx_global_runoff, ny_global_runoff

  integer(kind=int_kind) :: jf

  ! Inquire if atm is parallel or not and open the process log file
  
  call MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  call MPI_Comm_Rank(il_commlocal, my_task, ierror)
  
  il_out = 85 + my_task
  write(chout,'(I6.6)')il_out
  chatmout='atmout'//trim(chout)
  
  open(il_out,file=chatmout,form='formatted')
  write(il_out,*) 'Number of processes:', il_nbtotproc
  write(il_out,*) 'Local process number:', my_task
  write(il_out,*) 'Local communicator is : ',il_commlocal

  ! Initialise module level variables with details about the ice grid.
  call recv_grid_from_ice()
  call remap_runoff_new(remap_runoff, 'rmp_jrar_to_cict_CONSERV.nc', &
                        ice_lats, ice_lons, ice_mask)

  ! Compare the total number of processes and the number of processes
  ! involved in the coupling.
  !
  !  3 cases are illustrated here:
  !  . A monoprocess atm which process is involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel atm with only the master process involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel atm with all processes involved in the coupling
  !   (ll_comparal = .TRUE.);  put il_nbcplproc = 3 here after.
  !   Here, we hard code these numbers but later specific PSMILe routines
  !   will be developed to access this information directly in the SCC file.
  !   The case in which n processes are involved in the coupling among
  !   a total number m of processes should work but has not been tested.
  
  il_nbcplproc = 1              !B: better be input from a control file!
  
  if (il_nbcplproc == il_nbtotproc .and. il_nbtotproc /= 1) then
      ll_comparal = .true.
  else
      ll_comparal = .false.
  endif
  write(il_out,*)'ll_comparal = ',ll_comparal
  ! 
  if (my_task == 0 .OR. ll_comparal) then
    ! 
    ! The following steps need to be done:
    ! -> by the process if atm is monoprocess;
    ! -> only by the master process, if atm is parallel and only
    !    master process is involved in the coupling;
    ! -> by all processes, if atm is parallel and all processes
    ! are involved in the coupling.
    !
    ! 5- Define parallel partitions
    !    (prism_def_partition_proto is called in decomp_def)
    !    and allocate coupling fields accordingly
    !
    !!!!call decomp_def (il_part_id, il_length, ncells, &
    !!!!   my_task, il_nbcplproc, ll_comparal, il_out)

    !-----Define monoprocess partition (the whole field):
    il_paral ( clim_strategy ) = clim_serial
    il_paral ( clim_offset   ) = 0
    il_paral ( clim_length   ) = nx_global*ny_global

    call prism_def_partition_proto (il_part_id, il_paral, ierror)

    il_paral_runoff( clim_strategy ) = clim_serial
    il_paral_runoff( clim_offset   ) = 0
    il_paral_runoff( clim_length   ) = nx_global_ice*ny_global_ice
    call prism_def_partition_proto (il_part_runoff_id, il_paral_runoff, ierror)
 
    allocate(isst(nx_global,ny_global))   ; isst(:,:) = 0
!!!    allocate(albvdr(nx_global,ny_global)) ; albvdr(:,:) = 0
!!!    allocate(albidr(nx_global,ny_global)) ; albidr(:,:) = 0
!!!    allocate(albvdf(nx_global,ny_global)) ; albvdf(:,:) = 0
!!!    allocate(albidf(nx_global,ny_global)) ; albidf(:,:) = 0
    
    allocate(tair(nx_global,ny_global))  ; tair(:,:) = 0
    allocate(qair(nx_global,ny_global))  ; qair(:,:) = 0
    allocate(swfld(nx_global,ny_global)) ; swfld(:,:) = 0
    allocate(lwfld(nx_global,ny_global)) ; lwfld(:,:) = 0
    allocate(uwnd(nx_global,ny_global))  ; uwnd(:,:) = 0
    allocate(vwnd(nx_global,ny_global))  ; vwnd(:,:) = 0
    allocate(rain(nx_global,ny_global))  ; rain(:,:) = 0
    allocate(snow(nx_global,ny_global))  ; snow(:,:) = 0
    allocate(press(nx_global,ny_global)) ; press(:,:) = 0
    allocate(runof(nx_global_runoff,ny_global_runoff)) ; runof(:,:) = 0
    
    allocate(vwork(nx_global,ny_global)) ; vwork(:,:) = 0
    
    ! PSMILe coupling fields declaration
    
    
    ! Define name (as in namcouple) and declare each field sent by atm
    ! (NOTE: names of variables on atmos grid-nt62, NOT names on ocn grid-cice!)
    
    !! atm ==> ice/ocn
    cl_writ( 1) = 'swfld_ai'
    cl_writ( 2) = 'lwfld_ai'
    cl_writ( 3) = 'rain_ai'
    cl_writ( 4) = 'snow_ai'
    cl_writ( 5) = 'press_ai'
    cl_writ( 6) = 'runof_ai'
    cl_writ( 7) = 'tair_ai'
    cl_writ( 8) = 'qair_ai'
    cl_writ( 9) = 'uwnd_ai'
    cl_writ(10) = 'vwnd_ai'

    il_var_nodims(1) = 2 ! rank of coupling field
    il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)

    do jf=1, jpfldout
      if (cl_writ(jf) == 'runof_ai') then
        il_var_shape(1)= 1     ! min index for the coupling field local dim
        il_var_shape(2)= nx_global_ice ! max index for the coupling field local dim
        il_var_shape(3)= 1
        il_var_shape(4)= ny_global_ice
        call prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_runoff_id, &
         il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)

      else
        il_var_shape(1)= 1     ! min index for the coupling field local dim
        il_var_shape(2)= nx_global ! max index for the coupling field local dim
        il_var_shape(3)= 1
        il_var_shape(4)= ny_global
        call prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
         il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)

      endif
    enddo

    ! PSMILe end of declaration phase 
    call prism_enddef_proto (ierror)

  endif


  end subroutine init_cpl

subroutine recv_grid_from_ice()

  integer :: tag
  integer(kind=int_kind), dimension(2) :: buf_int
  real(kind=dbl_kind), dimension(:), allocatable :: buf_real
  integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)

  ! Receive dimensions of the ice grid that we're coupled to.
  if (my_task == 0) then

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
  endif

end subroutine recv_grid_from_ice

  !=======================================================================
  subroutine from_cpl(istep1)
  !--------------------------------------------------------------
  ! To pass the coupling fields using the MPI1/2 message passing interface:
  !   
  ! PSMILe prism_get_proto upon call from time loop.
  ! istep1: Model time in seconds since start of model run.
  !--------------------------------------------------------------
    
  integer(kind=int_kind), intent(in) :: istep1
  integer(kind=int_kind) :: jf
    
  if (my_task == 0 .OR. ll_comparal) then
    
   write(il_out,*) '(from_cpl) receiving coupling fields at stime= ',istep1
       
   do jf = 1, jpfldin

     !jf-th field in
     write(il_out,*)
     write(il_out,*) '*** receiving coupling field No. : ', jf, cl_read(jf)
     !call flush(il_out)
 
     call prism_get_proto (il_var_id_in(jf), istep1, vwork, ierror)
    
     if ( ierror /= PRISM_Ok .and. ierror < PRISM_Recvd) then
       write(il_out,*) 'Err in _get_ sst at time with error: ', istep1, ierror
       call prism_abort_proto(il_comp_id, 'matm from_cpl','stop 1')
     else 
       write(il_out,*)
       write(il_out,*)'(from_cpl) rcvd at time with err: ',cl_read(1),istep1,ierror
     endif

     if (jf == 1) then
        isst   = vwork
     endif
!!!     if (jf == 2) albvdr = vwork
!!!     if (jf == 3) albidr = vwork
!!!     if (jf == 4) albvdf = vwork
!!!     if (jf == 5) albidf = vwork
   enddo
   write(il_out,*)
   write(il_out,*)'(from_cpl) called all _get_ for imported fields! '

  endif   !if (my_task == 0 .OR. ll_comparal)

  if (chk_i2a_fields .and. mod(istep1, chk_fields_period) == 0) then
    call check_i2a_fields(istep1)
  endif

  end subroutine from_cpl

subroutine into_cpl(istep1)
  !--------------------------------------------------------------
  ! To pass the coupling fields from the atm model to the coupler
  ! using the MPI1/2 message passing interface.
  !--------------------------------------------------------------

  integer(kind=int_kind), intent(in) :: istep1
  integer(kind=int_kind) :: i, jf, tag
  integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)
  integer(kind=int_kind) :: buf(1)
  real(kind=dbl_kind), dimension(:, :), allocatable :: remapped_runoff

  allocate(remapped_runoff(nx_global_ice, ny_global_ice))

  if (.true.) then
    call write_boundary_chksums(istep1)
  endif

  call prism_put_proto(il_var_id_out(1), istep1, swfld, ierror)
  call prism_put_proto(il_var_id_out(2), istep1, lwfld, ierror)
  call prism_put_proto(il_var_id_out(3), istep1, rain, ierror)
  call prism_put_proto(il_var_id_out(4), istep1, snow, ierror)
  call prism_put_proto(il_var_id_out(5), istep1, press, ierror)

  if (maxval(runof) < 0.0) then
    call prism_abort_proto(il_comp_id, 'matm into_cpl', 'negative runoff')
  endif
  call remap_runoff_do(remap_runoff, runof, remapped_runoff, ice_mask)
  call prism_put_proto(il_var_id_out(6), istep1, remapped_runoff, ierror)

  call prism_put_proto(il_var_id_out(7), istep1, tair, ierror)
  call prism_put_proto(il_var_id_out(8), istep1, qair, ierror)
  call prism_put_proto(il_var_id_out(9), istep1, uwnd, ierror)
  call prism_put_proto(il_var_id_out(10), istep1, vwnd, ierror)

  if (chk_a2i_fields .and. mod(istep1, chk_fields_period) == 0) then
    call check_a2i_fields(istep1)
  endif

  ! Block until we receive from ice. This prevents the atm from sending
  ! continuously.
  if (my_commice_task == 0) then
    tag = MPI_ANY_TAG
    call MPI_recv(buf, 1, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
  endif

  ! This is pointless since we're required to be on only one PE.
  call MPI_Barrier(il_commlocal, ierror)

  deallocate(remapped_runoff)

end subroutine into_cpl

subroutine write_boundary_chksums(timestamp)

    integer, intent(in) :: timestamp

    print*, '[atm chksum] time:', timestamp
    print*, '[atm chksum] swfld:', sum(swfld)
    print*, '[atm chksum] lwfld:', sum(lwfld)
    print*, '[atm chksum] rain:', sum(rain)
    print*, '[atm chksum] snow:', sum(snow)
    print*, '[atm chksum] press:', sum(press)
    print*, '[atm chksum] runof:', sum(runof)
    print*, '[atm chksum] tair:', sum(tair)
    print*, '[atm chksum] qair:', sum(qair)
    print*, '[atm chksum] uwnd:', sum(uwnd)
    print*, '[atm chksum] vwnd:', sum(vwnd)

end subroutine

subroutine coupler_termination()

    implicit none

    ! Write out restart file.
    call save_a2i_fields('INPUT/a2i.nc')

    deallocate(isst)
    deallocate(tair) 
    deallocate(qair) 
    deallocate(swfld)
    deallocate(lwfld)
    deallocate(uwnd) 
    deallocate(vwnd) 
    deallocate(rain) 
    deallocate(press) 
    deallocate(runof)
    deallocate(snow)
    deallocate(vwork)

    call prism_terminate_proto(ierror)
    if (ierror /= PRISM_Ok) then
        write (il_out,*) 'An error occured in prism_terminate = ', ierror
    endif

    print *
    print *, '********** End of MATM **********'
    print *

    close(il_out)
    call MPI_Finalize (ierror)

end subroutine coupler_termination

subroutine regrid_runoff

end subroutine regrid_runoff

end module cpl_interfaces
