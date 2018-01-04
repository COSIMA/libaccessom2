module params_mod

implicit none

! Component model name
character(len=6), parameter :: cp_modnam='matmxx' 
integer(kind=int_kind) :: dt = 3600
character(len=10) :: calendar = 'NOLEAP'
logical :: debug_output = .false.

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
   debug_output

namelist/runoff_nml/ &
    remap_weights, & 
    num_runoff_caps, &
    runoff_caps, &
    runoff_caps_is, &
    runoff_caps_ie, &
    runoff_caps_js, &
    runoff_caps_je

contains

subroutine params_init()

    ! Rean input namelist
    open(unit=99, file="input_atm.nml", form="formatted", status="old")
    read(99, datm_nml)
    close(unit=99)
    num_runoff_caps = max(0, min(num_runoff_caps, max_caps))

end subroutine params_init

end module params_mod

