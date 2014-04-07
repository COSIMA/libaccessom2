module atm_read
!
!=============================================================================

use atm_kinds

include 'netcdf.inc'

public read_core, read_era40, read_ncep2, read_hadgem3

contains

  !===========================================================================
  subroutine ncheck(status) 

  !use netcdf   ! NetCDF fortran 90 interface !
  implicit none

  integer(kind=int_kind), intent(in) :: status

  !subroutine to handle NetCDF errors

  if (status /= nf_noerr) then
    write(*,'(/a)')   'error - from NetCDF library'
    write(*,'(a/)')   trim(nf_strerror(status))
    stop
  end if
  end subroutine ncheck

  !===========================================================================
  subroutine dewpt2sh(nx, ny, dewp, temp, pres, sh) 
  !used for ERA40 data dewpoint to specific humudity conversion

  implicit none

  integer(kind=int_kind), intent(in) :: nx, ny
  real(kind=dbl_kind), dimension(nx,ny), intent(in) :: &
     dewp, & ! (2m) dewpoint temperature in K
     temp, & ! (2m) air temperature in K
     pres    ! (2m or simply surface) pressure in hPa
  real(kind=dbl_kind), dimension(nx,ny), intent(out) :: &
     sh      ! (2m) specific humidity (kg/kg)

  real(kind=real_kind), dimension(nx,ny) :: vapp  !vapour pressure
  real(kind=real_kind), dimension(nx,ny) :: mixr  !mixing ratio
  real(kind=real_kind), dimension(nx,ny) :: work  !working array    

  work = 7.5 * (dewp - 273.16)/(dewp - 35.86) 
  vapp = 6.11 * 10.0**(work)                       !hPa
  mixr = 0.622*vapp/(pres - vapp)                  !kg/kg
  sh   = mixr/(1.0 + mixr)
 
  end subroutine dewpt2sh

  !===========================================================================
  subroutine read_core(dataout,nx,ny,nstep,varname,filename)

  implicit none

  !arguments:
  integer(kind=int_kind), intent(in) :: nx,ny,nstep
  !real, dimension(nx,ny) :: data
  real(kind=real_kind), dimension(nx,ny) :: data

  integer(kind=int_kind) :: i
  !
  real, dimension(nx,ny), intent(out) :: dataout
  !
  character(len=*) :: filename,varname

  !local variables
  integer(kind=int_kind) :: dimensions, ncid, varid, iz, status
  integer(kind=int_kind), dimension(:), allocatable :: count, start
  real(kind=real_kind) :: add_offset, scale_factor
  real(kind=real_kind) :: missing_value, FillValue
  real(kind=real_kind), dimension(2) :: actual_range
  integer(kind=int_kind) :: ierr, atype, alen
  
  scale_factor = 1.;      add_offset = 0.
  missing_value = 999999.; FillValue = 999999.
  actual_range = (/-1.e33, 1.e33/)

  !print *
  !print *, 'MATM: (read_core) reading data: ',varname
  !print *, 'MATM: (read_core)    from file: ',filename

  ! Open file for read access
  call ncheck( nf_open(filename, nf_nowrite, ncid) )

  ! Get variable ID
  call ncheck( nf_inq_varid(ncid, varname, varid) )

  ! Get number of dimensions, and allocate count and start accordingly
  call ncheck( nf_inq_varndims(ncid, varid, dimensions) )

  allocate (count(dimensions), start(dimensions))

  ! Get data ---------
  ! we select a specfic time-point of data to read !
  if (dimensions == 3) then
    start = (/ 1, 1, nstep /)
    count = (/ nx, ny, 1 /)
  else
    start = (/ 1, 1, 1, nstep /)
    count = (/ nx, ny, 1, 1 /)
  end if
  call ncheck( nf_get_vara_real(ncid, varid, start, count, data) )
  ! Get attribues
  status = nf_inq_att(ncid, varid, "add_offset", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "add_offset", add_offset) )
!    print *, 'MATM: (read_core) read in add_offset = ',add_offset
  else
!   print *, 'MATM: Attribute "add_offset" unavailable in the dataset!'
  endif
  status = nf_inq_att(ncid, varid, "scale_factor", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "scale_factor", scale_factor) )
!    print *, 'MATM: (read_core) read in scale_factor = ',scale_factor
  else
!    print *, 'MATM: Attribute "scale_factor" unavailable in the dataset!'
  endif
  !
  ! check missing_value and _FillValue 'cos some of the core
  !   fields (e.g., runoff and ice data) may need be 'masked'!
  status = nf_inq_att(ncid, varid, "missing_value", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "missing_value", missing_value) )
    !status =  nf_get_att_int2(ncid, varid, "missing_value", missing_value)
  else
!    print *, 'MATM: Attribute "missing_value" unavailable in the dataset!'
  endif
!  print *, 'MATM: (read_core) missing_value = ', missing_value
  !
  status = nf_inq_att(ncid, varid, "_FillValue", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "_FillValue", FillValue) )
    !status =  nf_get_att_int2(ncid, varid, "_FillValue", FillValue)
  else 
!    print *, 'MATM: Attribute "_FillValue" unavailable in the dataset!'
  endif 
!  print *
!  print *, 'MATM: (read_core) _FillValue = ', FillValue
 
  ! Close file
  status = nf_close(ncid)
  if (status /= nf_noerr) then
    print *, 'read_core: Error closing netCDF file ', filename
    stop
  endif

  !where (data /= missing_value .and. data /= FillValue)
    dataout = data  
  !elsewhere
  !  dataout = 0.0
  !endwhere

  !print *, 'MATM (read_core): data(i=1:5,j=10): ', (data(i,10),i=1,5)
  return
  !======================
  end subroutine read_core

  !===========================================================================
  subroutine read_era40(dataout,nx,ny,nstep,varname,filename)

  implicit none

  !arguments:
  integer(kind=int_kind), intent(in) :: nx,ny,nstep
  !real, dimension(nx,ny) :: data
  real(kind=real_kind), dimension(nx,ny) :: data

  integer(kind=int_kind) :: i, j
  !
  real(kind=dbl_kind), dimension(nx,ny), intent(out) :: dataout
  !
  character(len=*) :: filename,varname

  !local variables
  integer(kind=int_kind) :: dimensions, ncid, varid, status
  integer(kind=int_kind), dimension(:), allocatable :: count, start
  real(kind=real_kind) :: add_offset, scale_factor
  real(kind=real_kind) :: missing_value, FillValue
  real(kind=real_kind), dimension(2) :: actual_range
  integer(kind=int_kind) :: atype, alen
  
  scale_factor = 1.;      add_offset = 0.
  missing_value = 999999.; FillValue = 999999.
  actual_range = (/-1.e33, 1.e33/)

  print *
  print *, 'MATM: (read_era40) reading data: ',varname
  print *, 'MATM: (read_era40)    from file: ',filename

  ! Open file for read access
  call ncheck( nf_open(filename, nf_nowrite, ncid) )

  ! Get variable ID
  call ncheck( nf_inq_varid(ncid, varname, varid) )

  ! Get number of dimensions, and allocate count and start accordingly
  call ncheck( nf_inq_varndims(ncid, varid, dimensions) )

  allocate (count(dimensions), start(dimensions))

  ! Get data ---------
  ! we select a specfic time-point of data to read !
  if (dimensions == 3) then
    start = (/ 1, 1, nstep /)
    count = (/ nx, ny, 1 /)
  else
    start = (/ 1, 1, 1, nstep /)
    count = (/ nx, ny, 1, 1 /)
  end if
  call ncheck( nf_get_vara_real(ncid, varid, start, count, data) )
  ! Get attribues
  status = nf_inq_att(ncid, varid, "add_offset", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "add_offset", add_offset) )
!    print *, 'MATM: (read_era40) read in add_offset = ',add_offset
  else
!   print *, 'MATM: Attribute "add_offset" unavailable in the dataset!'
  endif
  status = nf_inq_att(ncid, varid, "scale_factor", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "scale_factor", scale_factor) )
!    print *, 'MATM: (read_era40) read in scale_factor = ',scale_factor
  else
!    print *, 'MATM: Attribute "scale_factor" unavailable in the dataset!'
  endif
  !
  ! check missing_value and _FillValue 'cos some of the era40
  !   fields (e.g., runoff and ice data) may need be 'masked'!
  status = nf_inq_att(ncid, varid, "missing_value", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "missing_value", missing_value) )
    !status =  nf_get_att_int2(ncid, varid, "missing_value", missing_value)
  else
!    print *, 'MATM: Attribute "missing_value" unavailable in the dataset!'
  endif
!  print *, 'MATM: (read_era40) missing_value = ', missing_value
  !
  status = nf_inq_att(ncid, varid, "_FillValue", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "_FillValue", FillValue) )
    !status =  nf_get_att_int2(ncid, varid, "_FillValue", FillValue)
  else 
!    print *, 'MATM: Attribute "_FillValue" unavailable in the dataset!'
  endif 
!  print *
!  print *, 'MATM: (read_era40) _FillValue = ', FillValue
 
  ! Close file
  status = nf_close(ncid)
  if (status /= nf_noerr) then
    print *, 'read_era40: Error closing netCDF file ', filename
    stop
  endif

  !---------------------------------------------------------------------
  !flip over the era40 data (Mark Collier's T62 gaussian version) from 
  !north-south to south-north as required by the coupler 
  !---------------------------------------------------------------------
  do j = 1, ny
    !where (data(:,ny-j+1) /= missing_value .and. data(:,ny-j+1) /= FillValue)
    !  dataout(:,j) = scale_factor * real(data(:,ny-j+1)) + add_offset
    !elsewhere
    !  dataout(:,j) = 0.
    !endwhere
    dataout(:,j) = data(:,ny-j+1)
  enddo
  !---------------------------------------------------------------------
  !print *, 'MATM (read_era40): data(i=1:5,j=10): ', (data(i,10),i=1,5)
  return
  !========================
  end subroutine read_era40

  !===========================================================================
  subroutine read_ncep2(dataout,nx,ny,nstep,varname,filename)

  implicit none

  !arguments:
  integer(kind=int_kind), intent(in) :: nx,ny,nstep
  !real, dimension(nx,ny), intent(out) :: data
  integer(kind=2), dimension(nx,ny) :: data
  integer(kind=int_kind) :: j
  !
  real(kind=dbl_kind), dimension(nx,ny), intent(out) :: dataout
  !
  character(len=*) :: filename,varname

  !local variables
  integer(kind=int_kind) :: dimensions, ncid, varid, iz, status
  integer(kind=int_kind), dimension(:), allocatable :: count, start
  real(kind=4) :: add_offset, scale_factor
  !missing_value, FillValue may be crucial for some fields such as runoff 
  !which needs be masked with invalid values.............................!  
  integer(kind=int_kind) :: missing_value, FillValue
  real(kind=4), dimension(2) :: actual_range
  integer(kind=int_kind) :: atype, alen
  
  scale_factor = 1.;      add_offset = 0.
  missing_value = 999999; FillValue = 999999
  actual_range = (/-1.e33, 1.e33/)

  print *
  print *, 'MATM: (read_ncep) reading data: ',varname
  print *, 'MATM: (read_ncep)    from file: ',filename

  ! Open file for read access
  call ncheck( nf_open(filename, nf_nowrite, ncid) )

  ! Get variable ID
  call ncheck( nf_inq_varid(ncid, varname, varid) )

  ! Get number of dimensions, and allocate count and start accordingly
  call ncheck( nf_inq_varndims(ncid, varid, dimensions) )

  allocate (count(dimensions), start(dimensions))

  ! Get data ---------
  ! we select a specfic time-point of data to read !
  if (dimensions == 3) then
    start = (/ 1, 1, nstep /)
    count = (/ nx, ny, 1 /)
  else
    start = (/ 1, 1, 1, nstep /)
    count = (/ nx, ny, 1, 1 /)
  end if
  call ncheck( nf_get_vara_int2(ncid, varid, start, count, data) )

  ! Get attribues

  status = nf_inq_att(ncid, varid, "add_offset", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "add_offset", add_offset) )
!    print *, 'MATM: (read_core) read in "add_offset" = ',add_offset
  else
!    print *, 'MATM: Attribute "add_offset" unavailable in the dataset!'
  endif
  status = nf_inq_att(ncid, varid, "scale_factor", atype, alen)
  if (status == 0) then
    call ncheck( nf_get_att_real(ncid, varid, "scale_factor", scale_factor) )
!    print *, 'MATM: (read_ncep) read in "scale_factor" = ',scale_factor
  else
!    print *, 'MATM: Attribute "scale_factor" unavailable in the dataset!'
  endif
  !
  !B: IMPORTANT--check missing_value and _FillValue 'cos some of the ncep2
  !   fields (e.g., runoff and ice data) need be 'masked'!
  status = nf_inq_att(ncid, varid, "missing_value", atype, alen)
  if (status == 0) then
    !call ncheck( nf_get_att_int2(ncid, varid, "missing_value", missing_value) )
    !20100406 
     call ncheck( nf_get_att_real(ncid, varid, "missing_value", missing_value) )
!    print *, 'MATM: (read_ncep) read in "missing_value" = ', missing_value 
  else
!    print *, 'MATM: Attribute "missing_value" unavailable in the dataset!'
  endif
!  print *, 'MATM: (read_ncep) "missing_value" = ', missing_value
  !
  status = nf_inq_att(ncid, varid, "_FillValue", atype, alen)
  if (status == 0) then
    !call ncheck( nf_get_att_int2(ncid, varid, "_FillValue", FillValue) )
    !20100406
    call ncheck( nf_get_att_real(ncid, varid, "_FillValue", FillValue) )
!    print *, 'MATM: (read_ncep) read in "_FillValue" = ', FillValue 
  else 
!    print *, 'MATM: Attribute "_FillValue" unavailable in the dataset!'
  endif 
 
  ! Close file
  status = nf_close(ncid)
  if (status /= nf_noerr) then
    print *, 'read_ncep: Error closing netCDF file ', filename
    stop
  endif

  !---------------------------------------------------------------------
  !flip over the ncep2 data (from north-south to south-north as required 
  !by the coupler (see comment on the coupling restart files) 05/03/07 
  !---------------------------------------------------------------------
  do j = 1, ny
    where (data(:,ny-j+1) /= missing_value .and. data(:,ny-j+1) /= FillValue)
      dataout(:,j) = scale_factor * real(data(:,ny-j+1)) + add_offset 
    elsewhere
      dataout(:,j) = 0.
    endwhere
  enddo
  !---------------------------------------------------------------------
  return
  !========================
  end subroutine read_ncep2

!========================================================================
end module atm_read

