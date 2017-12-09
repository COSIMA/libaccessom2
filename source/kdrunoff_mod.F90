
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
    ! Maximum runoff (kg/m^2/s) desired. Beyond this and
    ! runoff is redistributed.
    integer :: num_runoff_caps
    real, dimension(:), allocatable :: runoff_caps
    integer, dimension(:), allocatable :: runoff_caps_is
    integer, dimension(:), allocatable :: runoff_caps_ie
    integer, dimension(:), allocatable :: runoff_caps_js
    integer, dimension(:), allocatable :: runoff_caps_je
    integer :: num_ocean_points
  end type kdrunoff_class

  integer, parameter :: D = 3

  public kdrunoff_class, kdrunoff_new, kdrunoff_del
  public kdrunoff_remap

contains

  subroutine kdrunoff_new(this, land_sea_mask, x_t, y_t, &
                          num_land_pts, num_ocean_pts, &
                          num_runoff_caps, runoff_caps, &
                          runoff_caps_is, runoff_caps_ie, &
                          runoff_caps_js, runoff_caps_je)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(in) :: land_sea_mask
    real, dimension(:, :), intent(in) :: x_t, y_t
    integer, intent(out) :: num_land_pts, num_ocean_pts
    integer, intent(in) :: num_runoff_caps
    real, dimension(:), intent(in) :: runoff_caps
    integer, dimension(:), intent(in) :: runoff_caps_is
    integer, dimension(:), intent(in) :: runoff_caps_ie
    integer, dimension(:), intent(in) :: runoff_caps_js
    integer, dimension(:), intent(in) :: runoff_caps_je

    integer :: nx, ny, i, j, n_ocn, n_land

    allocate(this%runoff_caps(num_runoff_caps))
    allocate(this%runoff_caps_is(num_runoff_caps))
    allocate(this%runoff_caps_ie(num_runoff_caps))
    allocate(this%runoff_caps_js(num_runoff_caps))
    allocate(this%runoff_caps_je(num_runoff_caps))
    this%num_runoff_caps = num_runoff_caps
    this%runoff_caps = runoff_caps
    this%runoff_caps_is = runoff_caps_is
    this%runoff_caps_ie = runoff_caps_ie
    this%runoff_caps_js = runoff_caps_js
    this%runoff_caps_je = runoff_caps_je

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
          ! Reduce the number of ocean points by excluding those that cannot be
          ! coastal, i.e. those are in the middle of a tile which is all ocean.
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
    this%tree => kdtree2_create(this%ocean_points, sort=.true.)

  end subroutine kdrunoff_new

  subroutine kdrunoff_remap(this, runoff, areas)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(inout) :: runoff
    real, dimension(:, :), intent(in) :: areas

    integer :: n, i, j, nni, nnj, nn
    real :: val
    real :: redist, redist_mass
    type(kdtree2_result), allocatable :: results(:)

    if (this%num_ocean_points == 0) then
      return
    endif

    allocate(results(1))

    ! Move any runoff that occurs on land nearest ocean point
    do n=1, size(this%land_points, 2)
      i = this%land_indices(1, n)
      j = this%land_indices(2, n)
      val = runoff(i, j)
      if (val > 0.0) then
        call kdtree2_n_nearest(tp=this%tree, qv=this%land_points(:, n), &
                               nn=1, results=results)
        ! Remove runoff from land
        runoff(i, j) = runoff(i, j) - val
        nni = this%ocean_indices(1, results(1)%idx)
        nnj = this%ocean_indices(2, results(1)%idx)
        ! Add runoff to ocean point
        runoff(nni, nnj) = runoff(nni, nnj) + &
                           ((val * areas(i, j)) / areas(nni, nnj))
      endif
    enddo

    deallocate(results)
    
    do n=1,this%num_runoff_caps
      call kdrunoff_cap(this, runoff, areas, this%runoff_caps(n), &
                  this%runoff_caps_is(n), this%runoff_caps_ie(n), &
                  this%runoff_caps_js(n), this%runoff_caps_je(n))
    enddo

  end subroutine kdrunoff_remap
  
  subroutine kdrunoff_cap(this, runoff, areas, cap, is, ie, js, je)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(inout) :: runoff
    real, dimension(:, :), intent(in) :: areas
    real, intent(in) :: cap
    integer, intent(in) :: is
    integer, intent(in) :: ie
    integer, intent(in) :: js
    integer, intent(in) :: je

    integer :: n, i, j, nni, nnj, nn, nn_accum
    real :: val
    real :: redist, redist_mass, avail_mass
    type(kdtree2_result), allocatable :: results(:)

    if (this%num_ocean_points == 0) then
      return
    endif

    ! Now apply a limit to runoff. Runoff is evenly distributed
    ! to a number of neighbours.
    if (cap > 0.0) then
      do n=1, size(this%ocean_points, 2)
        i = this%ocean_indices(1, n)
        j = this%ocean_indices(2, n)
        if (i >= is .and. i <= ie .and. j >=js .and. j <= je) then
            redist = runoff(i, j) - cap

            if (redist > 0.0) then
              ! Remove 'redist' to bring down to cap
              runoff(i, j) = runoff(i, j) - redist
              redist_mass = redist * areas(i, j)

              ! Try to redistribute all in several passes because we don't know how
              ! many nearest neighbours are going to be needed.
              nn_accum = 1
              do while (redist_mass > 0.0)
                ! Guess how many nearest neighbours are needed
                ! to redistribute 'redist'. This is big to begin with and grows
                ! linearly.
                nn = (ceiling(redist / cap) * 100) + nn_accum
                if (nn > size(this%ocean_points) / 10.0) then
                    stop 'Error in kdrunoff_remap: runoff too large to redistribute.'
                endif
                allocate(results(nn))
                call kdtree2_n_nearest(tp=this%tree, qv=this%ocean_points(:, n), &
                                       nn=nn, results=results)
                do nn=nn_accum, size(results)
                  nni = this%ocean_indices(1, results(nn)%idx)
                  nnj = this%ocean_indices(2, results(nn)%idx)
                  ! How much of redist can this neighbour accommodate?
                  avail_mass = (cap - runoff(nni, nnj)) * areas(nni, nnj)
                  if (avail_mass > 0.0) then
                    avail_mass = min(avail_mass, redist_mass)
                    runoff(nni, nnj) = runoff(nni, nnj) + &
                                      (avail_mass / areas(nni, nnj))
                    redist_mass = redist_mass - avail_mass
                  endif
                enddo
                nn_accum = size(results)
                deallocate(results)
              enddo
            endif
        endif
      enddo
    endif

  end subroutine kdrunoff_cap

  subroutine kdrunoff_del(this)
    type(kdrunoff_class), intent(inout) :: this

    call kdtree2_destroy(this%tree)

    deallocate(this%ocean_points)
    deallocate(this%ocean_indices)
    deallocate(this%land_points)
    deallocate(this%land_indices)

  end subroutine kdrunoff_del

end module kdrunoff_mod

