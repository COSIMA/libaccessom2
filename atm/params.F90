module params_mod

implicit none

! Component model name
character(len=6), parameter :: cp_modnam='matmxx' 
integer(kind=int_kind) :: dt = 3600
character(len=10) :: calendar = 'NOLEAP'
logical :: debug_output = .false.

namelist/datm_nml/ &
   start_date, &
   runtime,   &
   dataset,   &
   calendar,   &
   dt,         &
   restart_dir, &
   debug_output

contains

subroutine params_init()

    ! Rean input namelist
    open(unit=99, file="input_atm.nml", form="formatted", status="old")
    read(99, datm_nml)
    close(unit=99)

end subroutine params_init

end module params_mod

