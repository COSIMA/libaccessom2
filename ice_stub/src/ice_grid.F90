module ice_grid_mod

use mpi
use netcdf
use util_mod, only : ncheck

implicit none
private
public ice_grid

type ice_grid
    real, dimension(:, :), allocatable :: lats, lons, mask
    integer :: nx
    integer :: ny
contains
    private
    procedure, public :: init => ice_grid_init
    procedure, public :: send => ice_grid_send
endtype ice_grid

contains

subroutine ice_read_global_nc(ncid, fieldname, dataout)

    integer, intent(in) :: ncid
    character(len=*), intent(in) :: fieldname
    real, dimension(:,:), intent(inout) ::  dataout

    integer :: varid

    dataout(:,:) = 0.0
    call ncheck(nf90_inq_varid(ncid, trim(fieldname), varid), &
               'Inquire: '//trim(fieldname))
    call ncheck(nf90_get_var(ncid, varid, dataout, start=(/1,1/), & 
                count=(/size(dataout, 1), size(dataout, 2)/)), &
                'Read: '//trim(fieldname))

endsubroutine ice_read_global_nc

subroutine ice_grid_init(self, grid_filename, kmt_filename, resolution)

    class(ice_grid), intent(inout) :: self
    character(len=*), intent(in) :: grid_filename, kmt_filename
    integer, dimension(2), intent(in) :: resolution
    integer :: ncid

    self%nx = resolution(1)
    self%ny = resolution(2)

    call ncheck(nf90_open(trim(grid_filename), NF90_NOWRITE, ncid), &
                'Opening '//trim(grid_filename))

    allocate(self%lats(self%nx, self%ny))
    allocate(self%lons(self%nx, self%ny))
    call ice_read_global_nc(ncid , 'tlat' , self%lats)
    call ice_read_global_nc(ncid , 'tlon' , self%lons)
    call ncheck(nf90_close(ncid), 'Closing '//trim(grid_filename))

    allocate(self%mask(self%nx, self%ny))
    call ncheck(nf90_open(trim(kmt_filename), NF90_NOWRITE, ncid), &
                'Opening '//trim(kmt_filename))
    call ice_read_global_nc(ncid , 'kmt' , self%mask)
    call ncheck(nf90_close(ncid), 'Closing '//trim(kmt_filename))

endsubroutine ice_grid_init

subroutine ice_grid_send(self)

    class(ice_grid), intent(inout) :: self

    integer :: tag, err
    integer, dimension(2) :: buf_int
    real, dimension(:), allocatable :: buf_real

    ! Send my details to the atm.
    tag = 0
    buf_int(1) = self%nx
    buf_int(2) = self%ny
    call MPI_send(buf_int, 2, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, err)

    allocate(buf_real(product(buf_int)))
    buf_real(:) = reshape(self%lats(:, :), (/ size(self%lats) /))
    call MPI_send(buf_real, size(buf_real), MPI_DOUBLE, 0, tag, &
                  MPI_COMM_WORLD, err)

    buf_real(:) = reshape(self%lons(:, :), (/ size(self%lons) /))
    call MPI_send(buf_real, size(buf_real), MPI_DOUBLE, 0, tag, &
                  MPI_COMM_WORLD, err)

    buf_real(:) = reshape(self%mask(:, :), (/ size(self%mask) /))
    call MPI_send(buf_real, size(buf_real), MPI_DOUBLE, 0, tag, &
                  MPI_COMM_WORLD, err)

endsubroutine ice_grid_send

endmodule ice_grid_mod
