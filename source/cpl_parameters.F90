module atm_params

implicit none

integer, parameter :: jpfldout = 10  ! Number of fields sent
integer, parameter :: jpfldin  = 0

character(len=8), dimension(jpfldout) :: cl_writ ! Symb names fields sent
character(len=8), dimension(jpfldin)  :: cl_read ! Symb names fields rcvd

integer, dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
integer, dimension(jpfldin)  :: il_var_id_in  ! ID for fields rcvd

! Component model name
character(len=6), parameter :: cp_modnam='matmxx' 

integer(kind=int_kind) :: dt = 3600

character(len=10) :: calendar = 'NOLEAP'

logical :: chk_a2i_fields = .false.
logical :: chk_i2a_fields = .false.
logical :: debug_output = .false.

! How often to dump the coupling fields if any of the chk_*_fields options are .true.
! The unit of time is seconds. By default fields are dumped every timestep.
integer(kind=int_kind) :: chk_fields_period = 1

! Conservatively redistribute runoff exceeding runoff_cap in specified regions.
! Regions specify grid points that will be checked for whether they exceed the cap;
! excess runoff from those grid points may be redistributed outside the specified region.
integer, parameter :: max_caps = 4 ! maximum number of runoff cap regions (increase if want more; also make the default arrays below match)
integer :: num_runoff_caps = 1 ! number of runoff cap regions to actually use
real(kind=dbl_kind), dimension(max_caps) :: runoff_caps = (/ 0.03, 0.0, 0.0, 0.0 /) ! kg/m^2/s  runoff cap applied in each region (0.0 = no cap)
! runoff cap is applied to all points between or including these index limits
integer, dimension(max_caps) :: runoff_caps_is = (/ 0, 0, 0, 0 /) ! starting i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_ie = (/ 1000000, -1, -1, -1 /) ! ending i index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_js = (/ 0, 0, 0, 0 /) ! starting j index for each runoff region (count from 1)
integer, dimension(max_caps) :: runoff_caps_je = (/ 1000000, -1, -1, -1 /) ! ending j index for each runoff region (count from 1)

namelist/datm_nml/ &
   start_date, &
   runtime,   &
   dataset,   &
   calendar,   &
   dt,         &
   restart_dir, &
   debug_output, &
   chk_a2i_fields, &   
   chk_i2a_fields

namelist/runoff_nml/ &
    remap_weights, & 
    num_runoff_caps, &
    runoff_caps, &
    runoff_caps_is, &
    runoff_caps_ie, &
    runoff_caps_js, &
    runoff_caps_je

contains

subroutine init_atm_params()

    ! Rean input namelist
    open(unit=99, file="input_atm.nml", form="formatted", status="old")
    read(99, datm_nml)
    close(unit=99)
    num_runoff_caps = max(0, min(num_runoff_caps, max_caps))

end subroutine init_atm_params

end module atm_params

