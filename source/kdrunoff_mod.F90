
! This module uses KDTREE2 from https://github.com/jmhodges/kdtree2.git (see
! file kdtree2_module.F90) to move runoff from land points to the nearest ocean
! point. See file remap_runoff_mod.F90 for example usage.

module kdrunoff_mod

  use kdtree2_module

  implicit none
  private

  type :: kdrunoff_class
    real(kdkind), dimension(:,:), allocatable :: ocean_points
    real(kdkind), dimension(:,:), allocatable :: ocean_indices
    real(kdkind), dimension(:,:), allocatable :: land_points
    real(kdkind), dimension(:,:), allocatable :: land_indices
    type(kdtree2), pointer :: tree
    integer :: num_ocean_points
  end type kdrunoff_class

  integer, parameter :: D = 3

  public kdrunoff_class, kdrunoff_new, kdrunoff_del
  public kdrunoff_remap

contains

  subroutine kdrunoff_new(this, land_sea_mask, x_t, y_t, &
                           num_land_pts, num_ocean_pts)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(in) :: land_sea_mask
    real, dimension(:, :), intent(in) :: x_t, y_t
    integer, intent(out) :: num_land_pts, num_ocean_pts

    integer :: nx, ny, i, j, n_ocn, n_land

    ! Total number of ocean points.
    this%num_ocean_points = sum(land_sea_mask)
    nx = size(land_sea_mask, 1)
    ny = size(land_sea_mask, 2)

    num_ocean_pts = this%num_ocean_points
    num_land_pts = nx*ny - num_ocean_pts

    if (num_ocean_pts == 0) then
      return
    endif

    allocate(this%ocean_points(D, num_ocean_pts))
    allocate(this%ocean_indices(2, num_ocean_pts))
    allocate(this%land_points(D, num_land_pts))
    allocate(this%land_indices(2, num_land_pts))

    ! Make lists of ocean and land points, also indices to those points.
    n_ocn = 1
    n_land = 1
    do j=1,ny
      do i=1,nx
        if (land_sea_mask(i, j) > 0.5) then
          this%ocean_points(1, n_ocn) = cos(y_t(i, j))*cos(x_t(i, j))
          this%ocean_points(2, n_ocn) = cos(y_t(i, j))*sin(x_t(i, j))
          this%ocean_points(3, n_ocn) = sin(y_t(i, j))
          this%ocean_indices(1, n_ocn) = i
          this%ocean_indices(2, n_ocn) = j
          n_ocn = n_ocn + 1
        else
          this%land_points(1, n_land) = cos(y_t(i, j))*cos(x_t(i, j))
          this%land_points(2, n_land) = cos(y_t(i, j))*sin(x_t(i, j))
          this%land_points(3, n_land) = sin(y_t(i, j))
          this%land_indices(1, n_land) = i
          this%land_indices(2, n_land) = j
          n_land = n_land + 1
        endif
      enddo
    enddo

    ! Create kdtree data structure
    this%tree => kdtree2_create(this%ocean_points)

  end subroutine kdrunoff_new

  subroutine kdrunoff_remap(this, runoff, areas, rspread)
    type(kdrunoff_class), intent(inout) :: this

    real, dimension(:, :), intent(inout) :: runoff
    real, dimension(:, :), intent(in) :: areas
    integer, intent(in) :: rspread

    integer :: n, i, j, nni, nnj
    real :: val, area_ratio
    type(kdtree2_result), allocatable :: results(:)

    if (this%num_ocean_points == 0) then
      return
    endif

    allocate(results(rspread))

    do n=1, size(this%land_points, 2)
      i = this%land_indices(1, n)
      j = this%land_indices(2, n)
      val = runoff(i, j)
      if (val > 0.0) then
        call kdtree2_n_nearest(tp=this%tree, qv=this%land_points(:, n), &
                               nn=rspread, results=results)
        runoff(i, j) = runoff(i, j) - val
        nni = this%ocean_indices(1, results(1)%idx)
        nnj = this%ocean_indices(2, results(1)%idx)
        area_ratio = areas(i, j) / areas(nni, nnj)
        runoff(nni, nnj) = runoff(nni, nnj) + val*area_ratio
      endif
    enddo

    deallocate(results)
  end subroutine kdrunoff_remap

  subroutine kdrunoff_del(this)
    type(kdrunoff_class), intent(inout) :: this

    call kdtree2_destroy(this%tree)

    deallocate(this%ocean_points)
    deallocate(this%ocean_indices)
    deallocate(this%land_points)
    deallocate(this%land_indices)

  end subroutine kdrunoff_del

end module kdrunoff_mod

