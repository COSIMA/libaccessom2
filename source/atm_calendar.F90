! Adopted from ice_calendar.F90
!=======================================================================
!BOP
!
! !MODULE: atm_calendar - calendar routines for managing time
!
!-------------------------------------------------------------------------------
!
      module atm_calendar
!
! !USES:
!
      use atm_kinds
      use cpl_parameters, only: truntime0, dt_atm, caltype, days_per_year
      use cpl_parameters, only: iniday, inimon, iniyear, inidate 
                                ! date info of the beginning of this run !
      use cpl_parameters, only: il_out
      use cpl_parameters, only: init_date ! date when this exp starts

      implicit none
      save

      integer (kind=int_kind) :: &
!         days_per_year        , & ! number of days in one year
         daymo(12)            , & ! number of days in each month
         daycal(13)               ! day number at end of month

      ! 360-day year data
      integer (kind=int_kind) :: &
         daymo360(12)         , & ! number of days in each month
         daycal360(13)            ! day number at end of month
      data daymo360 /   30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30/
      data daycal360/ 0,30, 60, 90,120,150,180,210,240,270,300,330,360/

      ! 365-day year data
      integer (kind=int_kind) :: &
         daymo365(12)         , & ! number of days in each month
         daycal365(13)            ! day number at end of month
      data daymo365 /   31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data daycal365/ 0,31, 59, 90,120,151,181,212,243,273,304,334,365/

      ! 366-day year data
      integer (kind=int_kind) :: &
         daymo366(12)         , & ! number of days in each month
         daycal366(13)            ! day number at end of month
      data daymo366 /   31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data daycal366/ 0,31, 60, 91,121,152,182,213,244,274,305,335,366/

      integer (kind=int_kind) :: mytest

      integer (kind=int_kind) :: &
         istep    , & ! local step counter for time loop
         istep0   , & ! counter, number of steps taken in previous run
         istep1   , & ! counter, number of steps at current timestep
         mday     , & ! day of the month
         hour     , & ! hour of the year
         month    , & ! month number, 1 to 12
         monthp   , & ! last month
         year_init, & ! initial year
         nyr      , & ! year number
         idate    , & ! date (yyyymmdd)
         idate0   , & ! initial date (yyyymmdd)
         sec      , & ! elapsed seconds into date
         npt          ! total number of time steps (dt)

      real (kind=dbl_kind) :: &
         time           , & ! total elapsed time (s)
         time_forc      , & ! time of last forcing update (s)
         yday           , & ! day of the year
         tday           , & ! absolute day number
         dayyr              ! number of days per year

      logical (kind=log_kind) :: &
         new_year       , & ! new year = .true.
         new_month      , & ! new month = .true.
         new_day        , & ! new day = .true.
         new_hour           ! new hour = .true.

      integer (kind=int_kind) :: &
         c0 = 0, &
         c1 = 1, &
         secday = 86400

!=======================================================================

      contains

!=======================================================================
!BOP
!
! !IROUTINE: init_calendar - initialize calendar variables
!
! !INTERFACE:
!
      subroutine init_calendar
!
! !DESCRIPTION:
!
! Initialize calendar variables
!
! !REVISION HISTORY:
!
! authors: Elizabeth C. Hunke, LANL
!          Tony Craig, NCAR
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!
      integer (kind=int_kind) :: mytest = 88

      integer (kind=int_kind) :: &
         k                          , &

      istep = 0         ! local timestep number
      time=istep0*dt_atm    ! s
      yday=c0           ! absolute day number
      mday=0            ! day of the month
      month=0           ! month
      nyr=0             ! year
      idate=00000101    ! date
      sec=0             ! seconds into date
      istep1 = istep0   ! number of steps at current timestep
                        ! real (dumped) or imagined (use to set calendar)

      dayyr = real(days_per_year, kind=dbl_kind)

!----------------------------
      time = truntime0
!----------------------------

      if (days_per_year.eq.360) then
        daymo  = daymo360
        daycal = daycal360
      elseif (days_per_year.eq.365) then
        daymo  = daymo365
        daycal = daycal365
      elseif (days_per_year.eq.366) then
        daymo  = daymo366
        daycal = daycal366
      else
	 stop 'MATM: year must have 360, 365 or 366 days'
      endif

      year_init = init_date/10000

      ! determine initial date (assumes namelist year_init, istep0 unchanged)     
      sec = mod(int(time),secday)            ! elapsed seconds into date at
                                        ! end of dt
      tday = (time-sec)/secday + c1     ! absolute day number
      yday = mod(tday-c1,days_year(year_init)) + c1    ! day of the year

      do k = 1, 12
        if (yday > real(daycal(k),kind=dbl_kind)) month = k
      enddo
      mday = int(yday) - daycal(month)  ! day of the month
      nyr = int((tday-c1)/days_year(year_init)) + 1    ! year number
      idate0 = (nyr+year_init-1)*10000 + month*100 + mday ! date (yyyymmdd) 

      write(il_out,*) '(init_calendar) istep0, dt, time, sec = ', istep0, dt_atm, time, sec
      write(il_out,*) '(init_calendar) tday, yday, mday, nyr = ', tday, yday, mday, nyr
      write(il_out,*) '(init_calendar) idate0 = ', idate0


      write(il_out,*)'(init_calendar) truntime0, time, sec = ', truntime0, time, sec
      write(il_out,*)'(init_calendar) tday, yday, mday, month, nyr = ',tday, yday, mday, month, nyr
      write(il_out,*)'(init_calendar) idate0 = ', idate0


      !??? the above is not working as expected ???
      idate0 = init_date  
      time = time + dt_atm
 
      write(il_out,*) '(init_calendar) idate0 (-corrected-) = ',idate0
      print *, 'MATM (init_calendar) idate0 = ', idate0 

      end subroutine init_calendar

!=======================================================================
!BOP
!
! !IROUTINE: calendar - computes date at the end of the time step
!
! !INTERFACE:
!
      subroutine calendar(ttime)
!
! !DESCRIPTION:
!
! Determine the date at the end of the time step
!
! !REVISION HISTORY:
!
! authors: Elizabeth C. Hunke, LANL
!          Tony Craig, NCAR
!
! !USES:
!???      use ice_fileunits
!???      use ice_communicate, only: my_task, master_task
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         ttime                          ! time variable
!
!EOP
!
      integer (kind=int_kind) :: &
         k                          , &
         nyrp,mdayp,hourp           , & ! previous year, day, hour
         elapsed_days               , & ! since beginning this run
         elapsed_months             , & ! since beginning this run
         elapsed_hours                  ! since beginning this run

      integer (kind=int_kind) :: &
         newh, newd, newm, newy         !date by the end of this step         

      nyrp=nyr
      monthp=month
      mdayp=mday
      hourp=hour
      new_year=.false.
      new_month=.false.
      new_day=.false.
      new_hour=.false.

      write(il_out,*) '(calendar) ttime = ', ttime

      sec = mod(int(ttime),secday)           ! elapsed seconds into date at
                                        ! end of dt
      call get_idate(ttime, newh, newd, newm, newy)
      !
      !note ttime is seconds accumulated from the beginning of this run only.
      !the following stuff is required here or there in other routines ... 
      !
      yday = (ttime-sec)/secday + c1    ! day of the year
      hour = newh
      mday = newd
      month = newm
      nyr = newy - year_init + 1
      !
      elapsed_months = (nyr - 1)*12 + month - 1
      tday = (ttime+truntime0 - mod(ttime+truntime0,real(secday)))/secday + c1
      elapsed_days = int(yday) - 1
      elapsed_hours = int(ttime/3600)

      idate = (nyr+year_init-1)*10000 + month*100 + mday ! date (yyyymmdd) 

      write(il_out,*) '(calendar) truntime0 = ', truntime0
      write(il_out,*) '(calendar) nyr, year_init, month, mday = ', nyr, year_init, month, mday
      write(il_out,*) '(calendar)  idate = ', idate

      if (nyr   /= nyrp)   new_year = .true.
      if (month /= monthp) new_month = .true.
      if (mday  /= mdayp)  new_day = .true.
      if (hour  /= hourp)  new_hour = .true.

      if ( new_day) then 
        write(*,*) ' '
        write(*,'(a12,i10,4x,a6,i10,4x,a4,i10)') &
             'MATM istep1:', istep1, 'idate:', idate, 'sec:', sec
      endif

      end subroutine calendar

!=======================================================================
subroutine get_idate(ttime, khfin, kdfin, kmfin, kyfin)

implicit none

real (kind=dbl_kind), intent(in) :: ttime
integer, intent(out) :: khfin, kdfin, kmfin, kyfin 

integer :: klmo(12)	!length of the months
integer :: inc_day	!increment of days since the beginning of this run
integer :: jm, jd

logical :: lleap

inc_day = int ((ttime + 0.5)/86400. )
khfin = (ttime - inc_day*86400)/3600

IF (caltype .eq. 0 .or. caltype .eq. 1) THEN

  !
  ! 1. Length of the months
  !
  DO jm = 1, 12
    klmo(jm) = 31
    if ( (jm-4)*(jm-6)*(jm-9)*(jm-11) == 0) klmo(jm) = 30
    IF (jm .eq. 2) THEN
      !
      !* Leap years
      !
      lleap = .FALSE.
      IF (caltype .eq. 1) THEN
        IF (MOD(iniyear,  4) .eq. 0) lleap = .TRUE.
        IF (MOD(iniyear,100) .eq. 0) lleap = .FALSE.
        IF (MOD(iniyear,400) .eq. 0) lleap = .TRUE.
      ENDIF
      klmo(jm) = 28 
      if (lleap) klmo(jm) = 29
    ENDIF
  ENDDO  !jm=1,12
     
  kdfin = iniday
  kmfin = inimon
  kyfin = iniyear

  !
  ! 2. Loop on the days
  !  

  DO 210 jd = 1, inc_day
    kdfin = kdfin + 1
    IF (kdfin .le. klmo(kmfin)) GOTO 210
    kdfin = 1
    kmfin = kmfin + 1
    IF (kmfin .le. 12) GOTO 210
    kmfin = 1
    kyfin = kyfin + 1
    !
    !* Leap years
    !
    lleap = .FALSE.
    IF (caltype .eq. 1) THEN
      IF (MOD(kyfin,  4) .eq. 0) lleap = .TRUE.
      IF (MOD(kyfin,100) .eq. 0) lleap = .FALSE.
      IF (MOD(kyfin,400) .eq. 0) lleap = .TRUE.
    ENDIF
    klmo(2) = 28
    if (lleap) klmo(2) = 29
210 CONTINUE

ELSE            !for years with constant length of months

  !
  ! 1. Calculate month lengths for current year
  !
  DO jm = 1, 12
    klmo(jm) = caltype
  ENDDO
  kdfin = iniday
  kmfin = inimon
  kyfin = iniyear

  !
  ! 2. Loop on the days
  !

  DO 410 jd = 1, inc_day
    kdfin = kdfin + 1
    IF (kdfin .le. klmo(kmfin)) GOTO 410
    kdfin = 1
    kmfin = kmfin + 1
    IF (kmfin .le. 12) GOTO 410
    kmfin = 1
    kyfin = kyfin + 1
410 CONTINUE

ENDIF

end subroutine get_idate

!=======================================================================
function days_year(year)

!use cpl_parameters, only : caltype

implicit none

integer, intent(in) :: year
real (kind=dbl_kind) :: days_year
logical :: lleap

IF (caltype .eq. 0 .or. caltype .eq. 1) THEN
  lleap = .FALSE.
  days_year = 365.
  IF (caltype .eq. 1) THEN
    IF (MOD(year,  4) .eq. 0) lleap = .TRUE.
    IF (MOD(year,100) .eq. 0) lleap = .FALSE.
    IF (MOD(year,400) .eq. 0) lleap = .TRUE.
  ENDIF
  if (lleap) days_year = 366.
ELSE
  days_year = dayyr
ENDIF

return
end function days_year

!=======================================================================
end module atm_calendar

