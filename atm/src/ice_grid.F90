module ice_grid_mod

use mpi

implicit none
private
public ice_grid

type ice_grid
    private
    real, dimension(:, :), allocatable :: lats, lons, mask
    integer :: nx
    integer :: ny
contains
    private
    procedure, pass(self), public :: init => ice_grid_init
    procedure, pass(self), public :: get_shape => ice_grid_get_shape
endtype ice_grid

contains

subroutine ice_grid_init(self, ice_intercomm)

    type(ice_grid_type), intent(inout) :: self
    integer, intent(in) :: ice_intercomm

    integer :: tag, err
    integer, dimension(2) :: buf_int
    real, dimension(:), allocatable :: buf_real
    integer :: stat(MPI_STATUS_SIZE)

    ! Receive dimensions of the ice grid that we're coupled to.
    tag = MPI_ANY_TAG
    call MPI_recv(buf_int, 2, MPI_INTEGER, 0, tag, ice_intercomm,  stat, err)
    self%nx = buf_int(1)
    self%ny = buf_int(2)

    allocate(self%lats(self%nx, self%ny))
    allocate(self%lons(self%nx, self%ny))
    allocate(self%mask(self%nx, self%ny))
    allocate(buf_real(self%nx*self%ny))

    call MPI_recv(buf_real, self%nx*self%ny, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, err)
    self%lats(:, :) = reshape(buf_real, (/ self%nx, self%ny /))

    call MPI_recv(buf_real, self%nx*self%ny, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, err)
    self%lons(:, :) = reshape(buf_real, (/ self%nx, self%ny /))

    call MPI_recv(buf_real, self%nx*self%ny, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, err)
    self%mask(:, :) = reshape(buf_real, (/ self%nx, self%ny /))
    deallocate(buf_real)

endsubroutine ice_grid_init

pure function get_shape(self)

    type(ice_grid_type), intent(in) :: self
    real, dimension(2), intent(out) :: get_shape

    get_shape(1) = nx
    get_shape(2) = ny
endfunction

endmodule ice_grid_mod
