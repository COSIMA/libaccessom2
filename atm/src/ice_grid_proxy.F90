module ice_grid_proxy_mod

use mpi

implicit none
private
public ice_grid_proxy

type ice_grid_proxy
    integer :: peer_intercomm
    real, dimension(:, :), allocatable :: lats, lons, mask
    integer :: nx
    integer :: ny
contains
    private
    procedure, public :: init => ice_grid_proxy_init
    procedure, public :: recv => ice_grid_proxy_recv
    procedure, public :: get_shape => ice_grid_proxy_get_shape
endtype ice_grid_proxy

contains

subroutine ice_grid_proxy_init(self, peer_intercomm)

    class(ice_grid_proxy), intent(inout) :: self
    integer, intent(in) :: peer_intercomm

    self%peer_intercomm = peer_intercomm

endsubroutine ice_grid_proxy_init

subroutine ice_grid_proxy_recv(self)

    class(ice_grid_proxy), intent(inout) :: self

    integer :: tag, err
    integer, dimension(2) :: buf_int
    real, dimension(:), allocatable :: buf_real
    integer :: stat(MPI_STATUS_SIZE)

    ! Receive dimensions of the ice grid that we're coupled to.
    tag = MPI_ANY_TAG
    call MPI_recv(buf_int, 2, MPI_INTEGER, 0, tag, self%peer_intercomm,  stat, err)
    self%nx = buf_int(1)
    self%ny = buf_int(2)

    allocate(self%lats(self%nx, self%ny))
    allocate(self%lons(self%nx, self%ny))
    allocate(self%mask(self%nx, self%ny))
    allocate(buf_real(self%nx*self%ny))

    call MPI_recv(buf_real, size(buf_real), &
                  MPI_DOUBLE, 0, tag, self%peer_intercomm, stat, err)
    self%lats(:, :) = reshape(buf_real, (/ self%nx, self%ny /))

    call MPI_recv(buf_real, size(buf_real), &
                  MPI_DOUBLE, 0, tag, self%peer_intercomm, stat, err)
    self%lons(:, :) = reshape(buf_real, (/ self%nx, self%ny /))

    call MPI_recv(buf_real, size(buf_real), &
                  MPI_DOUBLE, 0, tag, self%peer_intercomm, stat, err)
    self%mask(:, :) = reshape(buf_real, (/ self%nx, self%ny /))
    deallocate(buf_real)

endsubroutine ice_grid_proxy_recv

function ice_grid_proxy_get_shape(self)

    class(ice_grid_proxy), intent(in) :: self
    integer, dimension(2) :: ice_grid_proxy_get_shape

    ice_grid_proxy_get_shape(1) = self%nx
    ice_grid_proxy_get_shape(2) = self%ny
endfunction ice_grid_proxy_get_shape

endmodule ice_grid_proxy_mod
