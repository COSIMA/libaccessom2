
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
    real :: max_runoff
    integer :: num_ocean_points
  end type kdrunoff_class

  integer, parameter :: D = 3

  public kdrunoff_class, kdrunoff_new, kdrunoff_del
  public kdrunoff_remap

contains

  subroutine kdrunoff_new(this, land_sea_mask, x_t, y_t, &
                          num_land_pts, num_ocean_pts, &
                          max_runoff)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(in) :: land_sea_mask
    real, dimension(:, :), intent(in) :: x_t, y_t
    integer, intent(out) :: num_land_pts, num_ocean_pts
    real, optional, intent(in) :: max_runoff

    integer :: nx, ny, i, j, n_ocn, n_land

    if (present(max_runoff)) then
      this%max_runoff = max_runoff
    else
      ! No limit to runoff.
      this%max_runoff = 0.0
    endif

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

    integer :: n, i, j, nni, nnj, nn, nn_accum
    real :: val, val_per_nbr, area_ratio
    real :: redist, redist_mass, avail_mass
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
    
    call kdrunoff_cap(this, runoff, areas)

    ! ! Now apply a limit to runoff. Runoff is evenly distributed
    ! ! to a number of neighbours.
    ! if (this%max_runoff > 0.0) then
    !   do n=1, size(this%ocean_points, 2)
    !     i = this%ocean_indices(1, n)
    !     j = this%ocean_indices(2, n)
    !     redist = runoff(i, j) - this%max_runoff
    ! 
    !     if (redist > 0.0) then
    !       ! Remove 'redist' to bring down to max_runoff
    !       runoff(i, j) = runoff(i, j) - redist
    !       redist_mass = redist * areas(i, j)
    ! 
    !       ! Try to redistribute all in several passes because we don't know how
    !       ! many nearest neighbours are going to be needed.
    !       nn_accum = 1
    !       do while (redist_mass > 0.0)
    !         ! Guess how many nearest neighbours are needed
    !         ! to redistribute 'redist'. This is big to begin with and grows
    !         ! linearly.
    !         nn = (ceiling(redist / this%max_runoff) * 100) + nn_accum
    !         if (nn > size(this%ocean_points) / 10.0) then
    !             stop 'Error in kdrunoff_remap: runoff too large to redistribute.'
    !         endif
    !         allocate(results(nn))
    !         call kdtree2_n_nearest(tp=this%tree, qv=this%ocean_points(:, n), &
    !                                nn=nn, results=results)
    !         do nn=nn_accum, size(results)
    !           nni = this%ocean_indices(1, results(nn)%idx)
    !           nnj = this%ocean_indices(2, results(nn)%idx)
    !           ! How much of redist can this neighbour accommodate?
    !           avail_mass = (this%max_runoff - runoff(nni, nnj)) * areas(nni, nnj)
    !           if (avail_mass > 0.0) then
    !             avail_mass = min(avail_mass, redist_mass)
    !             runoff(nni, nnj) = runoff(nni, nnj) + &
    !                               (avail_mass / areas(nni, nnj))
    !             redist_mass = redist_mass - avail_mass
    !           endif
    !         enddo
    !         nn_accum = size(results)
    !         deallocate(results)
    !       enddo
    !     endif
    !   enddo
    ! endif

  end subroutine kdrunoff_remap
  
  subroutine kdrunoff_cap(this, runoff, areas)
    type(kdrunoff_class), intent(inout) :: this
    real, dimension(:, :), intent(inout) :: runoff
    real, dimension(:, :), intent(in) :: areas

    integer :: n, i, j, nni, nnj, nn, nn_accum
    real :: val, val_per_nbr, area_ratio
    real :: redist, redist_mass, avail_mass
    type(kdtree2_result), allocatable :: results(:)

    if (this%num_ocean_points == 0) then
      return
    endif

    ! allocate(results(1))
    ! 
    ! ! Move any runoff that occurs on land nearest ocean point
    ! do n=1, size(this%land_points, 2)
    !   i = this%land_indices(1, n)
    !   j = this%land_indices(2, n)
    !   val = runoff(i, j)
    !   if (val > 0.0) then
    !     call kdtree2_n_nearest(tp=this%tree, qv=this%land_points(:, n), &
    !                            nn=1, results=results)
    !     ! Remove runoff from land
    !     runoff(i, j) = runoff(i, j) - val
    !     nni = this%ocean_indices(1, results(1)%idx)
    !     nnj = this%ocean_indices(2, results(1)%idx)
    !     ! Add runoff to ocean point
    !     runoff(nni, nnj) = runoff(nni, nnj) + &
    !                        ((val * areas(i, j)) / areas(nni, nnj))
    !   endif
    ! enddo
    ! 
    ! deallocate(results)

    ! Now apply a limit to runoff. Runoff is evenly distributed
    ! to a number of neighbours.
    if (this%max_runoff > 0.0) then
      do n=1, size(this%ocean_points, 2)
        i = this%ocean_indices(1, n)
        j = this%ocean_indices(2, n)
        redist = runoff(i, j) - this%max_runoff

        if (redist > 0.0) then
          ! Remove 'redist' to bring down to max_runoff
          runoff(i, j) = runoff(i, j) - redist
          redist_mass = redist * areas(i, j)

          ! Try to redistribute all in several passes because we don't know how
          ! many nearest neighbours are going to be needed.
          nn_accum = 1
          do while (redist_mass > 0.0)
            ! Guess how many nearest neighbours are needed
            ! to redistribute 'redist'. This is big to begin with and grows
            ! linearly.
            nn = (ceiling(redist / this%max_runoff) * 100) + nn_accum
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
              avail_mass = (this%max_runoff - runoff(nni, nnj)) * areas(nni, nnj)
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

