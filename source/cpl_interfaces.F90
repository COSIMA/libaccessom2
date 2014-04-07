module cpl_interfaces
!
!============================================================================
! coupling interface between this dummy (data) atmo model and the oasis3_25
! coupler (via MPI2) using the PRISM System Model Interface (PSMILe).
!
!============================================================================

#ifdef OASIS3_MCT
  use mod_prism
#else
  use mod_prism_proto
  use mod_prism_def_partition_proto
  use mod_prism_put_proto
  use mod_prism_get_proto
  !use mod_prism_grids_writing
#endif

use atm_kinds
use atm_domain
use cpl_parameters
use cpl_arrays
use cpl_forcing_handler

implicit none

include 'mpif.h'
!...not needed if 'use mod_prism_grids_writing'

logical :: mpiflag

integer(kind=int_kind)  :: ierror, ibou
character(len=8) :: chatmout
character(len=2) :: chout 
integer(kind=int_kind) :: il_comp_id     ! Component ID
integer(kind=int_kind) :: il_commlocal   ! Component internal communicator
integer(kind=int_kind) :: il_nbtotproc   ! Total number of processes
integer(kind=int_kind) :: il_nbcplproc   ! Number of processes involved in the coupling
integer(kind=int_kind) :: il_part_id     ! Local partition ID
integer(kind=int_kind) :: il_length      ! Size of partial field for each process
!integer(kind=int_kind), dimension(2) :: il_var_nodims, il_var_shape ! see below
integer(kind=int_kind), dimension(2) :: il_var_nodims
integer(kind=int_kind), dimension(4) :: il_var_shape
  
integer(kind=int_kind), dimension(3) :: il_paral ! Definition of monoprocess partition
  
integer(kind=int_kind) :: il_flag        ! Flag for grid writing
integer(kind=int_kind) :: il_status, il_fileid, il_varid 
integer(kind=int_kind), dimension(2) :: icnt   !,ist ==> what is it?
real(kind=dbl_kind),dimension(nx_global,ny_global) :: dla_lon, dla_lat, dla_srf
integer(kind=int_kind),dimension(nx_global,ny_global) :: ila_msk 
real(kind=dbl_kind),dimension(nx_global,ny_global,4) :: dla_lonb, dla_latb
 
integer(kind=int_kind) :: io_size, ii, il_bufsize, il_real, il_bufsizebyt
integer(kind=int_kind) :: integer_byte_size, integer_io_size 
real(kind=dbl_kind), dimension(nx_global,ny_global)  :: rla_array
real(kind=dbl_kind), dimension(:), allocatable :: rla_bufsend

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
  
  !B: the following part may not be really needed(?)
  ! 
  ! Let's suppose the model attaches to a MPI buffer for its own use
  ! 
  ! ! Sophisticated way to determine buffer size needed (without "kind")
  ! ! Here one message containing rla_array

  integer_byte_size = BIT_SIZE(ii)/8
  inquire (iolength=io_size) ii
  integer_io_size = io_size
  inquire (iolength=io_size) rla_array(1,1)
  il_real = io_size/integer_io_size*integer_byte_size
  il_bufsize = ncells + MPI_BSEND_OVERHEAD/il_real + 1
  allocate (rla_bufsend(il_bufsize), stat = ierror)
  il_bufsizebyt = il_bufsize * il_real
  call MPI_Buffer_Attach(rla_bufsend, il_bufsizebyt, ierror)

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

  end subroutine prism_init

  !=======================================================================
  subroutine init_cpl
  !----------------------------------------------
  ! To initialize/setup the coupling environment
  !----------------------------------------------

  integer(kind=int_kind) :: jf

  ! Inquire if atm is parallel or not and open the process log file
  
  call MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  call MPI_Comm_Rank(il_commlocal, my_task, ierror)
  
  il_out = 85 + my_task
  write(chout,'(I2)')il_out
  chatmout='atmout'//chout
  
  open(il_out,file=chatmout,form='formatted')
  write(il_out,*) 'Number of processes:', il_nbtotproc
  write(il_out,*) 'Local process number:', my_task
  write(il_out,*) 'Local communicator is : ',il_commlocal
  
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
    il_paral ( clim_length   ) = ncells
    write(il_out,*)'(init_cpl) ncells = ',ncells
  
    call prism_def_partition_proto (il_part_id, il_paral, ierror)
  
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
    allocate(runof(nx_global,ny_global)) ; runof(:,:) = 0
    
    allocate(vwork(nx_global,ny_global)) ; vwork(:,:) = 0
    
    ! PSMILe coupling fields declaration
    
    il_var_nodims(1) = 2 ! rank of coupling field
    il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)
    il_var_shape(1)= 1     ! min index for the coupling field local dim
    il_var_shape(2)= nx_global ! max index for the coupling field local dim
    il_var_shape(3)= 1
    il_var_shape(4)= ny_global
    
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

    do jf=1, jpfldout
      call prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
         il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)
    enddo
    
    ! Define name (as in namcouple) and declare each field received by atm
    
    cl_read(1)='isst_a'
!!!    cl_read(2)='alvdr_a'
!!!    cl_read(3)='alidr_a'
!!!    cl_read(4)='alvdf_a'
!!!    cl_read(5)='alidf_a'
    !
    do jf=1, jpfldin
      call prism_def_var_proto (il_var_id_in(jf), cl_read(jf), il_part_id, &
         il_var_nodims, PRISM_In, il_var_shape, PRISM_Real, ierror)
    enddo
    
    ! PSMILe end of declaration phase 
    
    call prism_enddef_proto (ierror)
    
  endif
    
  end subroutine init_cpl
    
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

  !=======================================================================
  subroutine into_cpl(istep1)
  !--------------------------------------------------------------
  ! To pass the coupling fields from the atm model to the coupler
  ! using the MPI1/2 message passing interface.
  !--------------------------------------------------------------

  integer(kind=int_kind), intent(in) :: istep1
  integer(kind=int_kind) :: i, jf

  if (my_task == 0 .or. ll_comparal) then

   write(il_out,*) '(into_cpl) sending coupling fields at stime= ',istep1

   do jf = 1, jpfldout

     write(il_out,*)
     write(il_out,*) '*** sending coupling field No. ', jf, cl_writ(jf)
     !call flush(il_out)

        select case (jf)
            case (1)
                vwork = swfld
            case (2)
                vwork = lwfld
            case (3)
                vwork = rain
            case (4)
                vwork = snow
            case (5)
                vwork = press
            case (6)
                vwork = runof
            case (7)
                vwork = tair
            case (8)
                vwork = qair
            case (9)
                vwork = uwnd
            case (10)
                vwork = vwnd
            case default
                call prism_abort_proto(il_comp_id, 'matm into_cpl','bad case')
            end select

     call prism_put_proto(il_var_id_out(jf), istep1, vwork, ierror)

     if ( ierror /= PRISM_Ok .and. ierror < PRISM_Sent) then
       write(il_out,*)
       write(il_out,*) '(into_cpl) Err in _put_ ', cl_writ(jf), istep1, ierror
       call prism_abort_proto(il_comp_id, 'matm into_cpl','STOP 1')
     else 
       write(il_out,*)
       write(il_out,*)'(into_cpl) sent: ', cl_writ(jf), istep1, ierror
     endif

   enddo     !jf = 1, jpfldout
  
   write(il_out,*)
   write(il_out,*) '*** sent all fields at r_time = ',istep1

  endif    !if(my_task .eq. 0 .or. ll_comparal)

  if (chk_a2i_fields .and. mod(istep1, chk_fields_period) == 0) then
    call check_a2i_fields(istep1)
  endif

  return
  end subroutine into_cpl
  

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

    ! Detach from MPI buffer, FIXME this may not be needed. 
    call MPI_Buffer_Detach(rla_bufsend, il_bufsize, ierror)
    deallocate (rla_bufsend)

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

!=======================================================================
end module cpl_interfaces


