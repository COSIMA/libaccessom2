module cpl_forcing_handler

use atm_domain
use cpl_parameters
use cpl_netcdf_setup
use cpl_arrays

implicit none

contains

!===========================================================================
subroutine check_i2a_fields(nstep)

implicit none

integer(kind=int_kind), intent(in) :: nstep
integer(kind=int_kind) :: ilout, ll
integer(kind=int_kind), save :: ncid,currstep
data currstep/0/

currstep=currstep+1

if (currstep == 1) then
  call create_ncfile('fields_i2a_in_atm.nc',ncid,nx_global,ny_global,ll=1,ilout=il_out)
endif

write(il_out,*) 'opening file fields_i2a_in_atm.nc at nstep = ', nstep
call ncheck( nf_open('fields_i2a_in_atm.nc',nf_write,ncid) )
call write_nc_1Dtime(real(nstep),currstep,'time',ncid)

call gather_global(vwork, isst)
call write_nc2D(ncid, 'isst_a', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)

call ncheck(nf_close(ncid))

return
end subroutine check_i2a_fields

!===========================================================================
subroutine check_a2i_fields(nstep)

implicit none

integer(kind=int_kind), intent(in) :: nstep
integer(kind=int_kind) :: ncid,currstep,ll,ilout
data currstep/0/
save currstep

currstep=currstep+1

if (currstep == 1) then
  call create_ncfile('fields_a2i_in_atm.nc',ncid,nx_global,ny_global,ll=1,ilout=il_out)
endif

write(il_out,*) 'opening file fields_a2i_in_atm.nc at nstep = ', nstep
call ncheck( nf_open('fields_a2i_in_atm.nc',nf_write,ncid) )
call write_nc_1Dtime(real(nstep),currstep,'time',ncid)

call gather_global(vwork, tair)
call write_nc2D(ncid, 'tair', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, swfld)
call write_nc2D(ncid, 'swfld', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, lwfld)
call write_nc2D(ncid, 'lwfld', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, uwnd)
call write_nc2D(ncid, 'uwnd', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, vwnd)
call write_nc2D(ncid, 'vwnd', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, qair)
call write_nc2D(ncid, 'qair', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, rain)
call write_nc2D(ncid, 'rain', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, snow)
call write_nc2D(ncid, 'snow', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, runof)
call write_nc2D(ncid, 'runof', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)
call gather_global(vwork, press)
call write_nc2D(ncid, 'press', vwork, 1, nx_global,ny_global,currstep,ilout=il_out)

call ncheck(nf_close(ncid))

return
end subroutine check_a2i_fields

! Save the atmosphere <-> coupling fields. This will then be used as a restart
! for the ice model. FIXME: merge with above.
subroutine save_a2i_fields(fname)

    implicit none

    character(len=*), intent(in) :: fname

    integer(kind=int_kind) :: ncid
    integer(kind=int_kind) :: jf, ll, ilout

    call create_ncfile(fname, ncid, nx_global, ny_global, ilout=il_out)

    call write_nc2D_notime(ncid, 'vwnd_ai', uwnd, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'uwnd_ai', vwnd, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'qair_ai', qair, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'tair_ai', tair, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'runof_ai', runof, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'press_ai', press, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'rain_ai', rain, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'snow_ai', snow, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'lwfld_ai', lwfld, nx_global, ny_global)
    call write_nc2D_notime(ncid, 'swfld_ai', swfld, nx_global, ny_global)

    call ncheck(nf_close(ncid))

end subroutine save_a2i_fields

!===========================================================================
subroutine gather_global(vout, vin)
!'empty', MPI-free "gathering" routine!

implicit none

real (kind=dbl_kind), dimension(nx_global,ny_global), intent(inout) :: vout, vin

vout = vin

return
end subroutine gather_global

!============================================================================
function file_exist (file_name)
!
character(len=*), intent(in) :: file_name
logical  file_exist

file_exist = .false.
if (len_trim(file_name) == 0) return
if (file_name(1:1) == ' ')    return

inquire (file=trim(file_name), exist=file_exist)

end function file_exist

!===========================================================================
end module cpl_forcing_handler
