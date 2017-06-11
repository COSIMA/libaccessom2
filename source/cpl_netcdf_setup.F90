module cpl_netcdf_setup

use atm_kinds
use atm_calendar, only: idate, sec
use cpl_parameters, only: my_task

use netcdf

implicit none

integer(kind=int_kind) :: pLonDimId, pLatDimId, timeDimId, pDepDimId

private

public :: ncheck, create_ncfile, write_nc_1Dtime, write_nc2D, write_nc2D_notime
public :: get_field_dims

contains

subroutine ncheck(status, error_str)

    implicit none

    integer(kind=int_kind), intent(in) :: status
    character(len=*), intent(in), optional :: error_str

    if (status /= nf90_noerr) then
      write(*,'(/a)')   'MATM: error - from NetCDF library'
    if (present(error_str)) then
        write(*,'(a)')   error_str
      endif
      write(*,'(a/)')   trim(nf90_strerror(status))
      stop
    end if

end subroutine ncheck

subroutine create_ncfile(ncfile, ncid, ii, jj, kk, ll, ilout)
    ! Create 2, 3,or 4D ncfile, depending on optional args (kk,ll)

    implicit none

    integer(kind=int_kind), intent(in) :: ii,jj     	!x, y dimension size
    integer(kind=int_kind), optional :: kk, ll !z, t dimension size
    integer(kind=int_kind), optional :: ilout  !format io file id
    character(len=*), intent(in) :: ncfile  
    integer(kind=int_kind), intent(out) :: ncid

    if (present(ilout)) then 
      write(ilout,*) 'creating a new netcdf file: ',ncfile
      write(ilout,*) '    with nx, ny = ', ii, jj
    endif

    !create a new NetCDF and define the grid:
    call ncheck(nf90_create(trim(ncfile),nf90_write,ncid), "Creating: "//trim(ncfile))

    !define the dimensions
    if (present(ll)) then
        call ncheck(nf90_def_dim(ncid,"time", nf90_unlimited,  timeDimId), 'Defining time in '//trim(ncfile)//' in create_ncfile()')
    endif
    if (present(kk)) then
        call ncheck(nf90_def_dim(ncid,"nz", kk,  pDepDimId), 'Defining nz in '//trim(ncfile)//' in create_ncfile()')
    endif
    call ncheck(nf90_def_dim(ncid, "ny", jj,  pLatDimId), 'Defining ny in '//trim(ncfile)//' in create_ncfile()')
    call ncheck(nf90_def_dim(ncid, "nx", ii,  pLonDimId), 'Defining nx in '//trim(ncfile)//' in create_ncfile()')

    call ncheck(nf90_enddef(ncid))

    return
end subroutine create_ncfile

subroutine write_nc_1Dtime(vin, nt, vname, ncid)

    implicit none

    integer(kind=int_kind), intent(in) :: ncid,nt
    integer(kind=int_kind) :: varid, ncstatus
    integer(kind=int_kind), dimension(1:6) :: adate
    real, intent(in) :: vin 	
    ! NOTE here real is default real*8 (which is actually the same as dbl_kind!)
    ! somehow the netcdf lib used here takes 'real' as real*4. therefore we need: 
    real*4 :: vtmp
    character(len=*), intent(in) :: vname
    character*80 ctimeatt

    vtmp = real(vin)
    ncstatus=nf90_inq_varid(ncid,vname,varid)

    if (ncstatus/=nf90_noerr) then
      adate(1) = idate/10000
      adate(2) = (idate - (idate/10000)*10000)/100
      adate(3) = idate - (idate/100)*100
      adate(4:6) = 0  !OK for 'whole-day' runs
      call ncheck(nf90_redef(ncid))
      call ncheck(nf90_def_var(ncid, trim(vname), nf90_real, (/ timeDimId /), varid), &
                  'Defining: '//trim(vname)//' in write_nc_1Dtime()')
      write(ctimeatt, &
          '("seconds since ",I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') adate(:)
      call ncheck(nf90_put_att(ncid, varid, "units", trim(ctimeatt)), &
                  'Adding attribute units to '//trim(vname)//' in write_nc_1Dtime()')
      call ncheck(nf90_enddef(ncid))
    end if

    !write values into the 1D array
    call ncheck(nf90_put_var(ncid, varid, vtmp, start=(/ nt /)), 'Writing values to '//trim(vname)//' in write_nc_1Dtime()')

    return
end subroutine write_nc_1Dtime

subroutine write_nc2D(ncid, vname, vin, prcn, nx, ny, istep, ilout)
    !
    !to output a 2D array into a 3D field (with time dimension) 
    !with either single or double precisioin depending on argumnet 'prcn'!
    !

    implicit none

    integer(kind=int_kind), intent(in) :: ncid
    integer(kind=int_kind), intent(in) :: prcn	!precision choice (1/2: signle/double)
    character(len=*), intent(in) :: vname
    integer(kind=int_kind), intent(in) :: nx, ny
    integer(kind=int_kind), intent(in) :: istep	!position in the time dim (No of record) 
    integer(kind=int_kind), optional :: ilout
    real(kind=dbl_kind), dimension(nx,ny), intent(in) :: vin

    integer(kind=int_kind) :: varid, ncstatus 
    real*4, dimension(nx,ny) :: vtmp   !single precision

    if (present(ilout)) write(ilout,*) 'write_nc2D: handling var *** ',vname, ' rec: ', istep

    ncstatus=nf90_inq_varid(ncid,vname,varid)
    if (ncstatus/=nf90_noerr) then
      call ncheck(nf90_redef(ncid))
      if (prcn == 1) then
        call ncheck(nf90_def_var(ncid,trim(vname), nf90_real, &
                (/pLonDimId, pLatDimId, timeDimId/),varid), 'Defining real '//trim(vname)//' in write_nc2D')
      else
        call ncheck(nf90_def_var(ncid,trim(vname), nf90_double, &
                (/pLonDimId, pLatDimId, timeDimId/),varid), 'Defining double '//trim(vname)//' in write_nc2D')
      endif
      call ncheck(nf90_enddef(ncid))
      if (present(ilout)) write(ilout,*) 'write_nc2D: defined new var ***', vname 
    else
      if (present(ilout)) write(ilout,*) 'write_nc2D: found   old var ***', vname
    end if

    select case(prcn)
      case (1)
        vtmp = real(vin) !dbl precision to single precision
        call ncheck(nf90_put_var(ncid, varid, vtmp, start=(/1,1,istep/), count=(/nx,ny,1/)), &
                    'Writing real values to'//trim(vname)//' in write_nc2D')


      case default    !case (2)
        call ncheck(nf90_put_var(ncid,varid,vin, start=(/1,1,istep/),count=(/nx,ny,1/)), &
                    'Writing double values to'//trim(vname)//' in write_nc2D')
    end select

    return
end subroutine write_nc2D

! Write out a netcdf variable with no time dimentsion. FIXME: rename this
! subroutine and the one above.
subroutine write_nc2D_notime(ncid, var_name, var_data, nx, ny)

    implicit none

    integer(kind=int_kind), intent(in) :: ncid
    character(len=*), intent(in) :: var_name
    integer(kind=int_kind), intent(in) :: nx, ny
    real(kind=dbl_kind), dimension(nx,ny), intent(in) :: var_data

    integer(kind=int_kind) :: varid, ncstatus 

    ncstatus = nf90_inq_varid(ncid, var_name, varid)
    if (ncstatus /= nf90_noerr) then
        call ncheck(nf90_redef(ncid))
        call ncheck(nf90_def_var(ncid, trim(var_name), nf90_double, (/ pLonDimId, pLatDimId /), varid))
        call ncheck(nf90_enddef(ncid))
    endif

    call ncheck(nf90_put_var(ncid, varid, var_data, start=(/1,1/), count=(/nx,ny/)))

end subroutine write_nc2D_notime

! Return the spatial and time dimensions of a field.
subroutine get_field_dims(nx, ny, time, filename, varname)
  integer, intent(out) :: nx, ny, time
  character(len=*), intent(in) :: filename, varname

  integer :: ncid, varid
  integer, dimension(:), allocatable :: dimids
  integer :: ndims, i, len
  character(len=nf90_max_name) :: dimname

  call ncheck(nf90_open(filename, NF90_NOWRITE, ncid))
  call ncheck(nf90_inq_varid(ncid, varname, varid))

  ! Get dimensions used by this var.
  call ncheck(nf90_inquire_variable(ncid, varid, ndims=ndims))

  allocate(dimids(ndims))
  call ncheck(nf90_inquire_variable(ncid, varid, dimids=dimids))

  ! Only support dimension names: time, latitude, longitude for now.
  nx = 0
  ny = 0
  time = 0
  do i = 1,ndims
    call ncheck(nf90_inquire_dimension(ncid, dimids(i), name=dimname, len=len))
    if (trim(dimname) == 'time') then
      time = len
    elseif (trim(dimname) == 'latitude') then
      ny = len
    elseif (trim(dimname) == 'longitude') then
      nx = len
    else
      stop 'get_field_dim_lens: Unsupported dimension name'
    endif
  enddo

  deallocate(dimids)

  if (nx == 0 .or. ny == 0 .or. time == 0) then
    stop "get_field_dim_lens: couldn't get all dimensions"
  endif

end subroutine get_field_dims

end module cpl_netcdf_setup
