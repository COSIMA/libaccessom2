module cpl_parameters
!
!============================================================================

use atm_kinds

implicit none

integer(kind=int_kind), parameter :: jpfldout = 10  ! Number of fields sent
!!!integer(kind=int_kind), parameter :: jpfldin  = 5   ! Number of fields rcvd
integer(kind=int_kind), parameter :: jpfldin  = 0

character(len=8), dimension(jpfldout) :: cl_writ ! Symb names fields sent
character(len=8), dimension(jpfldin)  :: cl_read ! Symb names fields rcvd

integer(kind=int_kind), dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
integer(kind=int_kind), dimension(jpfldin)  :: il_var_id_in  ! ID for fields rcvd

character(len=6), parameter :: cp_modnam='matmxx' ! Component model name

integer(kind=int_kind) :: il_out
integer(kind=int_kind) :: my_task = 0 
logical :: ll_comparal = .false. ! Logical true if component is parallel 
                             ! and if all process communicates with Oasis.

integer(kind=int_kind) :: iniday, inimon, iniyear  !calculated from inidate
real(kind=dbl_kind) :: truntime0 = 0.
                             !truntime0 can be too large as int to read in correctly
integer(kind=int_kind) :: &
   dt_atm = 3600,         &
   yruntime0 = 0,         &
   runtime = 86400,       &
   dt_cpl = 21600,        &
   init_date = 00010101,  &  ! initial date of this EXP !  
   inidate = 01010101,    &  ! initial date of this RUN segment !
   days_per_year = 365
   !yruntime0 is the 'accumulated' time in seconds, at the beginning of a run segment,
   !   for a certain year in the experiment.  it's calculated in main program.
   !runtime the time length for this run segment, and 
   !truntime0 is the total runtime of this experiemnt

integer(kind=int_kind) :: caltype = 0
   !0: 365days/year; 1: Gregorian (365/366 days per year); or n: n days/month

character(len=10) :: dataset = 'core'	! currently 'ncep2', 'era40', 'core', 'core2' 
character(len=2)  :: runtype = 'IA'  	! 'IA' for inter-annual, 'NY' for normal year
logical :: chk_a2i_fields = .false.
logical :: chk_i2a_fields = .false.
logical :: debug_output = .false.
! How often to dump the coupling fields if any of the chk_*_fields options are .true.
! The unit of time is seconds. By default fields are dumped every timestep.
integer(kind=int_kind) :: chk_fields_period = 1

real(kind=dbl_kind) :: global_runoff_cap = 0.03  ! kg/m^2/s
! conservatively spread runoff exceeding global_runoff_cap; set to global_runoff_cap=0.0 to have no global limit to runoff
integer, parameter :: max_caps = 4 ! maximum number of runoff cap regions in addition to global (increase if want more)
integer :: num_runoff_caps = 0 ! number of runoff cap regions actually used, in addition to global; anything more than max_caps is ignored
real(kind=dbl_kind),dimension(max_caps) :: runoff_caps = (/ 0.0, 0.0, 0.0, 0.0 /) ! kg/m^2/s  runoff cap applied in each region (0.0 = no cap)
integer,dimension(max_caps) :: runoff_caps_is = (/ 0, 0, 0, 0 /) ! starting i index for each runoff region
integer,dimension(max_caps) :: runoff_caps_ie = (/ 0, 0, 0, 0 /) ! ending i index for each runoff region
integer,dimension(max_caps) :: runoff_caps_js = (/ 0, 0, 0, 0 /) ! starting j index for each runoff region
integer,dimension(max_caps) :: runoff_caps_je = (/ 0, 0, 0, 0 /) ! ending j index for each runoff region

namelist/coupling/ &
   init_date, &
   inidate,   &
   runtime,   &
   dt_cpl,    &
   dt_atm,    &
   truntime0, &
   dataset,   &
   runtype,   &
   caltype,   &
   days_per_year, &
   chk_a2i_fields, &   
   chk_i2a_fields, &
   chk_fields_period, &
   global_runoff_cap, &
   num_runoff_caps, &
   runoff_caps, &
   runoff_caps_is, &
   runoff_caps_ie, &
   runoff_caps_js, &
   runoff_caps_je, &
   debug_output

!====================================================================================
end module cpl_parameters

