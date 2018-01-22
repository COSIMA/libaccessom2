module ice_grid_mod

implicit none
private
public ice_grid_type

type ice_grid_type
    private
    real, dimension(:, :), allocatable :: lats, lons, mask
contains
    private
    procedure, public :: init
end type ice_grid_type

contains

subroutine init(this, ice_intercomm)

    type(ice_grid_type), intent(inout) :: this
    integer, intent(in) :: ice_intercomm

    integer :: tag
    integer, dimension(2) :: buf_int
    real, dimension(:), allocatable :: buf_real
    integer :: stat(MPI_STATUS_SIZE)

    ! Receive dimensions of the ice grid that we're coupled to.
    tag = MPI_ANY_TAG
    call MPI_recv(buf_int, 2, MPI_INTEGER, 0, tag, ice_intercomm,  stat, ierror)
    nx_global_ice = buf_int(1)
    ny_global_ice = buf_int(2)

    allocate(this%lats(nx_global_ice, ny_global_ice))
    allocate(this%lons(nx_global_ice, ny_global_ice))
    allocate(this%mask(nx_global_ice, ny_global_ice))
    allocate(buf_real(nx_global_ice*ny_global_ice))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, ierror)
    lats(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, ierror)
    lons(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, ice_intercomm,  stat, ierror)
    mask(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))
    deallocate(buf_real)

end subroutine ice_grid_init

end module ice_grid_mod
