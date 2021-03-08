module pio_wrapper_mod

use mpi
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

subroutine pio_wrapper_init(self, num_io_procs, new_comp_comm, io_comm)
    class(pio_wrapper), intent(inout) :: self

    integer, intent(in) :: num_io_procs
    integer, intent(out) :: new_comp_comm, io_comm

    integer :: num_total_procs
    integer :: ierr, i, num_comp_procs, my_pe
    integer, dimension(1) :: procs_per_component, comp_comm
    integer, dimension(num_io_procs) :: io_proc_list
    integer, allocatable, dimension(:, :) :: comp_proc_list
    type(iosystem_desc_t), dimension(1) :: tmp_pio_subsystem

    if (self%pio_initialized) then
        return
    endif

    call MPI_Comm_size(MPI_COMM_WORLD, num_total_procs, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_pe, ierr)

    num_comp_procs = num_total_procs - num_io_procs
    procs_per_component(1) = num_comp_procs

    allocate(comp_proc_list(num_comp_procs, 1))
    do i=1, num_total_procs
        if (i <= num_comp_procs) then
            comp_proc_list(i,1) = i - 1
        else
            io_proc_list(i-num_comp_procs) = i - 1
        endif
    enddo

    print*, 'my_pe: ', my_pe
    print*, 'num_total_procs: ', num_total_procs
    print*, 'num_io_procs: ', num_io_procs
    print*, 'num_comp_procs: ', num_io_procs
    print*, 'io_proc_list: ', io_proc_list

    ierr = pio_set_log_level(10)
    print*, 'log level set to 10, ierr: ', ierr

    call pio_init(tmp_pio_subsystem,          & ! iosystem
                  MPI_COMM_WORLD,             & ! MPI communicator
                  procs_per_component,        & ! number of tasks per component model
                  comp_proc_list,             & ! list of comp procs per component
                  io_proc_list,               & ! list of io procs
                  PIO_REARR_BOX,              & ! rearranger to use (currently only BOX is supported)
                  comp_comm,                  & ! comp_comm to be returned
                  io_comm)                      ! io_comm to be returned

    print*, 'my_pe done pio_init:', my_pe

    self%pio_subsystem = tmp_pio_subsystem(1)
    new_comp_comm = comp_comm(1)

    self%pio_initialized = .true.

endsubroutine pio_wrapper_init

subroutine pio_wrapper_deinit(self)
    class(pio_wrapper), intent(inout) :: self

endsubroutine pio_wrapper_deinit

endmodule pio_wrapper_mod
