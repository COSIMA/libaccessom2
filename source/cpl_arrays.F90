module cpl_arrays
!
!============================================================================
! It's designed to include the following 'possible' coupling fields at the
! air-ice surface:
!
! A> atm==>ice
! (1) shortwave radiation down (J/m^2)        swfld    
! (2) longwave radiation down (J/m^2)         lwfld
! (3) rainall rate  (kg/m^2/s) 		      rain 
! (4) snowfall rate (kg/m^2/s)                snow
! (5) pressure            (Pa)                press
! (6) runof               (kg/m^2/s)          runof 
! (7) 2m air temperature  (K)                 tair 
! (8) 2m air specific humidity (kg/kg)        qair 
! (9) 10m 'zonal' wind speed (m/s)            uwnd
! (10)10m 'meridional' wind speed (m/s)       vwnd
!
! B> ice==>atm
! (1) sea/ice/snow surface temperature (C)    sst   
!!!! (2) albedo: visible, direct (fraction)      albvdr
!!!! (3) albedo: near IR, direct (fraction)      albidr
!!!! (4) albedo: visible, diffuse (fraction)     albvdf
!!!! (5) albedo: near IR, diffuse (fraction)     albidf
!
!!!! 10 out,  5 in => thus we set jpfldout=10, jpfldin=5 (in cpl_parameters)
!
!
! Note we take a coupling approach similar to that in GFDL FMS for MOM4:
!      all atmospheric forcing to the ocean will be through the sea ice,
!      namely, all the atmospheric fluxes required by the ocean will be 
!      calculated in a boundary layer set in the ice model, therefore there
!      is no 'direct interaction' (thus data passing) between atmosphere
!      and ocean. this also means that atmosphere will receive the joint
!      underlying boundary forcing from the ocean and sea ice. 
!============================================================================

  use atm_kinds

  implicit none

  ! Fields sent
  real(kind=dbl_kind), dimension(:,:), allocatable :: &
      swfld, lwfld, uwnd, vwnd, rain, snow, press, runof, tair, qair 
      !----------------------------------------------------------------
      ! be CAREFUL with file 'core_4_matm.table' etc: the above 10 
      ! fields/files must be put in the correct order as shown here!
      !----------------------------------------------------------------
  
  ! Fields received
  real(kind=dbl_kind),dimension(:,:),allocatable :: &
!!!!      isst, albvdr, albidr, albvdf, albidf
     isst
  
  real(kind=dbl_kind), dimension(:,:), allocatable :: vwork  !tmp array
  
!============================================================================
end module cpl_arrays

