module ice_grid_mod

implicit none
private
public ice_grid_type

type ice_grid_type
    private
contains
    private
    procedure, public :: init => ice_grid_init
end type ice_grid_type

contains

subroutine ice_grid_init(ice_intercomm)

    integer :: tag
    integer(kind=int_kind), dimension(2) :: buf_int
    real(kind=dbl_kind), dimension(:), allocatable :: buf_real
    integer(kind=int_kind) :: stat(MPI_STATUS_SIZE)

    ! Receive dimensions of the ice grid that we're coupled to.

    tag = MPI_ANY_TAG
    call MPI_recv(buf_int, 2, MPI_INTEGER, 0, tag, il_commice,  stat, ierror)
    nx_global_ice = buf_int(1)
    ny_global_ice = buf_int(2)

    allocate(ice_lats(nx_global_ice, ny_global_ice))
    allocate(ice_lons(nx_global_ice, ny_global_ice))
    allocate(ice_mask(nx_global_ice, ny_global_ice))
    allocate(buf_real(nx_global_ice*ny_global_ice))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_lats(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_lons(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))

    call MPI_recv(buf_real, nx_global_ice*ny_global_ice, &
                  MPI_DOUBLE, 0, tag, il_commice,  stat, ierror)
    ice_mask(:, :) = reshape(buf_real, (/ nx_global_ice, ny_global_ice /))
    deallocate(buf_real)

end subroutine ice_grid_init

end module ice_grid_mod
