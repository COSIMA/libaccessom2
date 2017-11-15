module atm_read

use atm_kinds
use cpl_netcdf_setup , only: ncheck
use netcdf

implicit none

private

public read_core

contains

subroutine read_core(dataout,nx,ny,nstep,varname,filename)

  real, dimension(nx,ny), intent(out) :: dataout
  integer(kind=int_kind), intent(in) :: nx,ny,nstep
  character(len=*) :: filename,varname
  integer(kind=int_kind) :: i

  !local variables
  integer(kind=int_kind) :: ndims, ncid, varid, status
  integer(kind=int_kind), dimension(:), allocatable :: count, start

  ! Open file for read access
  call ncheck(nf90_open(trim(filename), NF90_NOWRITE, ncid), &
              'Opening '//trim(filename))

  ! Get variable ID
  call ncheck(nf90_inq_varid(ncid, trim(varname), varid), &
              'Inquire: '//trim(varname))

  ! Get number of dimensions, and allocate count and start accordingly
  call ncheck(nf90_inquire_variable(ncid, varid, ndims=ndims), &
              'Inquire dims: '//trim(varname))

  allocate (count(ndims), start(ndims))

  ! Get data, we select a specfic time-point of data to read
  if (ndims == 3) then
    start = (/ 1, 1, nstep /)
    count = (/ nx, ny, 1 /)
  else
    start = (/ 1, 1, 1, nstep /)
    count = (/ nx, ny, 1, 1 /)
  end if
  call ncheck(nf90_get_var(ncid, varid, dataout, start=start, count=count), &
              'Get var '//trim(varname))
  deallocate(count, start)

  ! Close file
  call ncheck(nf90_close(ncid), & 'Closing '//trim(filename))

end subroutine read_core

end module atm_read
