program atm

    !
    !============================================================================
    !* This data model currently supports reading in 3 atmospheric forcing data *
    !* sets, namely, NCEP-r2 6 hourly, ERA40 6 hourly, and CORE 6 hourly except *
    !* for daily radiation fluxes and monthly precipitation. These 3 datasets   * 
    !* are all of netcdf format but with different layouts and thus need be read*
    !* and handle in different ways, as done in module read_forcing_mod......   *
    !
    !* It runs with mom4 and cice in the AusCOM coupled model under the OASIS3  *
    !* prism 2-5 framework (oasis325). Attention must be paid to the time flow  *
    !* and control in the context of matching that within cice and/or mom4.     *
    !									    !	
    !* Taking the era40 data as an example for the 6 hourly forcing reading:    *
    !									    !	
    !* At the very beginning of a run, cice receives from oasis the very first  !
    !* package of forcing data (read from the pre-processed coupling restart    !
    !* file a2i.nc), which is of time 00:00, the FIRST record of the era40 data,!
    !* which will be used for the first A<==>I coupling interval (00:00--06:00) !
    !* in cice. For the second coupling interval (06:00--12:00), new forcing    !
    !* data rcvd from oasis (MUST BE of time 6:00, the SECOND record) will be   !
    !* used ...... and so on.  						    !
    !									    !	
    !* Therefore, in matm, the first cpl-interval should read in the SECOND     *
    !* record (time 6:00) from era40 because it will be sent to cice/mom4 for   *
    !* their use in the second cpl-interval (06:00-12:00)! 		            *
    !									    *
    !* This is why, in the code below, we set 				    *
    !									    !
    !* nt_read = itap_sec/dt_cpl + 2 					    *
    !									    !
    !* Note there itap_sec/dt_cpl = 0 for the first coupling interval!	    *
    !                                                                           !
    ! Modified by Fabio Dias (21/02/2016) to accept JRA-55 forcing.             !
    !============================================================================

    use atm_kinds
    use atm_domain
    use atm_forcing, only : init_atm_forcing, get_atm_forcing
    use atm_params, only : init_atm_params, start_date
    use cpl_arrays
    use cpl_interfaces   
    use atm_calendar
    use cpl_netcdf_setup, only : get_field_dims

    implicit none

    integer :: jf, icpl, itap, itap_sec, icpl_sec, rtimestamp, stimestamp

    integer :: num_cpl              ! = runtime/dt_cpl !
    integer :: num_cpl_in_year      ! = number of coupling steps in one year
    integer :: npas                 ! = dt_cpl/dt_atm !

    character(len=80), dimension(:), allocatable :: cfile
    character(len=8),  dimension(:), allocatable :: cfield
    integer :: nt_read, nfields, nrec 
    integer :: nt_read12, nt_read56  !for core data special handling
    integer :: iday, imonth, iyear
    integer :: i, unused

    integer :: nx_global_runoff, ny_global_runoff


    real :: dt_accum                    !sec. for the time accumulated data

    real, dimension(:, :), allocatable :: work

    call init_atm_params()
    call init_atm_forcing("atm_forcing.json", start_date)

    call get_atm_forcing_info('runof', nx_global_runoff, ny_global_runoff)
    ! We need the runoff shape to coupler initialisation.
    call init_coupler(nx_global_runoff, ny_global_runoff)
    
! --- *** BE CAREFUL WITH the timestamp for coupling operation: *** ---
!       (here rtimestamp for receiving and stimestamp for sending)

  do icpl = 1, num_cpl

    icpl_sec = dt_cpl * (icpl - 1)        !runtime for this run segment! 
    rtimestamp = icpl_sec                 !recv timestamp

    !=== matm internal time loop ===!

    do itap = 1, npas

      istep = istep + 1      ! update time step counters
      istep1 = istep1 + 1
      time = time + dt_atm   ! determine the time and date
      call calendar(time-truntime0) 

      iday = mod(idate, 100)
      imonth = mod( (idate - iday)/100, 100)
      iyear = idate / 10000

      stimestamp = icpl_sec + (itap-1) * dt_atm   
      ! runtime for this run, used for sending timestamp following this loop!

      itap_sec = stimestamp + yruntime0
      ! total run time for current model year

      write(il_out,*) '(main) icpl, itap, stimestamp, yruntime0, itap_sec: ', &
                              icpl, itap, stimestamp, yruntime0, itap_sec 

      !--- here we update the atm fields/variables -------------
      if ( mod(itap_sec, dt_cpl) == 0 ) then
        ! read once every 6 hours <=> dt_cpl=21600, ie, at the beginning of each
        ! coupling interval! 

        ! Note the 'position' of the record in the yearly data (yruntime0 counted in)!
        nt_read = itap_sec/dt_cpl + 2

        write(il_out,*)
        write(il_out,*) 'idate, iday, imonth, iyear: ', idate, iday, imonth, iyear
        write(il_out,*)
        write(il_out,*) '(main) reading atm data at runtime = ',itap_sec
        write(il_out,*) '      (for 6-hourly dataset) timelevel = ',nt_read

        !-------------------------------------------------!
        ! be careful to make the read-in fields match the !
        ! coupling fields defined in the coupling module. ! 
        !-------------------------------------------------!

        if (trim(dataset) == 'core' .or. trim(dataset) == 'core2') then

          nt_read12 = daycal365(imonth) + iday   ! radiation daily data
          nt_read56 = imonth                     ! precipitation monthly data

          write(il_out,*) '      (for monthly dataset) timelevel = ',nt_read56
          write(il_out,*) '      (for daily   dataset) timelevel = ',nt_read12

          do jf = 1, nfields

          ! need read in the First record from next year dataset which will not be
          ! used for the current year but saved as the 'initial' a2i data for oasis
          ! to read in at the beginning of next year
          !------------------------------------------------------------------------!
          if (imonth == 12) then
            if (mod(icpl, num_cpl_in_year) == 0) then           !the last cpl interval
              ! Comment out below to re-use the same file year after year.    
              !if ( (jf-1)*(jf-2)*(jf-5)*(jf-6) /= 0 ) then
              !  call nextyear_forcing(cfile(jf))  
              !endif
              nt_read = 1
            endif
          endif

          if ( (jf-1)*(jf-2) == 0 ) then
            nrec = nt_read12
          else if ( (jf-5)*(jf-6) == 0 ) then
            nrec = nt_read56
          else
            nrec = nt_read
          endif

          write(il_out,*) '(main) reading core data no: ', jf, ' ', trim(cfield(jf))  
          write(il_out,*) '       recond no: ', nrec

          call read_core(vwork, nx_global,ny_global, nrec, trim(cfield(jf)), cfile(jf))

          if (jf == 1) swfld = vwork
          if (jf == 2) lwfld = vwork
          if (jf == 3) uwnd  = vwork
          if (jf == 4) vwnd  = vwork
          if (jf == 5) rain  = vwork
          if (jf == 6) snow  = vwork
          if (jf == 7) press = vwork
          if (jf == 8) runof = vwork
          if (jf == 9) tair  = vwork
          if (jf ==10) qair  = vwork

          enddo

        else if (trim(dataset) == 'jra55') then

          do jf = 1, nfields

          ! need read in the First record from next year dataset
          !-------------------------------------------------------!
          if (imonth == 12) then
            if (mod(icpl, num_cpl_in_year) == 0) then           !the last cpl interval
               if ( runtype == 'IA' ) then
                  call nextyear_forcing(cfile(jf))
               end if
              nt_read = 1
            endif
          endif

          if ( jf /= 8 ) then
            ! Take the modulo in case we are doing a multi-year run.
            nrec = mod(nt_read, num_cpl_in_year)
          else
            nrec = mod(((nt_read - 1)/8) + 1, 365)
          endif

          ! The mod() above may set nrec = 0, minimum is 1.
          nrec = max(nrec, 1)

          write(il_out,*) '(main) reading forcing data no: ', jf, ' ',trim(cfield(jf))
          write(il_out,*) '       record no: ', nrec

          if ( jf==8 ) then
            call read_core(runof, nx_global_runoff , ny_global_runoff, nrec, trim(cfield(jf)),cfile(jf))
          else
            call read_core(vwork, nx_global,ny_global, nrec, trim(cfield(jf)),cfile(jf))
            if (jf == 1) swfld = vwork
            if (jf == 2) lwfld = vwork
            if (jf == 3) uwnd  = vwork
            if (jf == 4) vwnd  = vwork
            if (jf == 5) rain  = vwork
            if (jf == 6) snow  = vwork
            if (jf == 7) press = vwork
            if (jf == 9) tair  = vwork
            if (jf ==10) qair  = vwork
          endif

          enddo
        endif   !if (trim(dataset) == 'core')
      endif  !if (mod(itap_sec, dt_cpl) == 0) 

    enddo      !itap = 1, npas
    write(il_out,*)
    write(il_out,*) '(main) calling into_cpl at icpl, stime= ',icpl, stimestamp

    call into_cpl(stimestamp) 	! stimestamp updated in the itap loop above.

    write(il_out,*)
    write(il_out,*) '(main) called into_cpl at icpl, stime= ',icpl,stimestamp

  enddo        !icpl = 1, num_cpl

  call coupler_termination

  deallocate(cfile)
  deallocate(cfield) 

end program atm
