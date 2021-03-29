module pio_wrapper_mod

use mpi
use error_handler, only : assert
use pio, only: pio_init
use pio, only: iosystem_desc_t
use pio, only: pio_set_log_level
use pio, only: pio_clobber, pio_noclobber, pio_write, pio_nowrite
use pio, only: pio_iotype_netcdf4p, PIO_REARR_BOX

implicit none
private
public pio_wrapper

type pio_wrapper
    private

    logical :: pio_initialized
    type(iosystem_desc_t), public :: pio_subsystem

contains
    private
    procedure, pass(self), public :: init => pio_wrapper_init
    procedure, pass(self), public :: deinit => pio_wrapper_deinit
endtype pio_wrapper

contains

subroutine pio_wrapper_init(self, model_name, num_io_procs, new_comm)
    class(pio_wrapper), intent(inout) :: self

    character(len=*), intent(in) :: model_name
    integer, intent(in) :: num_io_procs
    integer, intent(out) :: new_comm

    integer :: pio_log_level
    integer :: num_total_procs
    integer :: ierr, i, num_comp_procs, my_pe, proc_counter
    integer :: io_comm
    integer, dimension(3) :: procs_per_component, comp_comm
    integer, dimension(num_io_procs) :: io_proc_list
    integer, allocatable, dimension(:, :) :: comp_proc_list
    type(iosystem_desc_t), allocatable, dimension(:) :: tmp_pio_subsystem

    if (self%pio_initialized) then
        return
    endif

    call MPI_Comm_size(MPI_COMM_WORLD, num_total_procs, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_pe, ierr)

    num_comp_procs = num_total_procs - num_io_procs
    procs_per_component(1) = 1
    procs_per_component(2) = 216
    procs_per_component(3) = 24

    allocate(comp_proc_list(216, 3))
    comp_proc_list(:, :) = 0
    proc_counter = 0
    comp_proc_list(1, 1) = proc_counter

    do i=1, 216
        proc_counter = proc_counter + 1
        comp_proc_list(i , 2) = proc_counter
    enddo

    do i=1, 24
        proc_counter = proc_counter + 1
        comp_proc_list(i , 3) = proc_counter
    enddo

    ! Put the IO proc right at the end
    io_proc_list(1) = proc_counter + 1

    print*, 'my_pe: ', my_pe
    print*, 'num_total_procs: ', num_total_procs
    print*, 'num_io_procs: ', num_io_procs
    print*, 'num_comp_procs: ', num_comp_procs
    !print*, 'comp_proc_list(:, 3): ', comp_proc_list(:, 3)
    print*, 'io_proc_list: ', io_proc_list

    ! Create new world communicator that contains only the comp_procs,
    ! this will become the new COMM_WORLD
    if (any(io_proc_list == my_pe)) then
        call MPI_Comm_split(MPI_COMM_WORLD, 0, my_pe, new_comm, ierr)
    else
        call MPI_Comm_split(MPI_COMM_WORLD, 1, my_pe, new_comm, ierr)
    endif
    call assert(ierr == MPI_SUCCESS, &
                'pio_wrapper_init: could not make new communicator')

    pio_log_level = 10
    ierr = pio_set_log_level(pio_log_level)
    print*, 'log level set to: ', pio_log_level

    allocate(tmp_pio_subsystem(3))

    ! FIXME: we could also try to use PIOc_init_async_from_comms()
    call pio_init(tmp_pio_subsystem,          & ! iosystem
                  MPI_COMM_WORLD,             & ! MPI communicator
                  procs_per_component,        & ! number of tasks per component model
                  comp_proc_list,             & ! list of comp procs per component
                  io_proc_list,               & ! list of io procs
                  PIO_REARR_BOX,              & ! rearranger to use (currently only BOX is supported)
                  comp_comm,                  & ! comp_comm to be returned
                  io_comm)                      ! io_comm to be returned

    ! FIXME: Correct way to do this.
    if (io_comm == MPI_COMM_NULL) then
        if (trim(model_name) == 'matmxx') then
            self%pio_subsystem = tmp_pio_subsystem(1)
        elseif (trim(model_name) == 'cicexx') then
            self%pio_subsystem = tmp_pio_subsystem(3)
        else
            self%pio_subsystem = tmp_pio_subsystem(2)
        endif
    endif

    print*, 'my_pe done pio_init:', my_pe

    self%pio_initialized = .true.

endsubroutine pio_wrapper_init

subroutine pio_wrapper_deinit(self)
    class(pio_wrapper), intent(inout) :: self

endsubroutine pio_wrapper_deinit

endmodule pio_wrapper_mod
