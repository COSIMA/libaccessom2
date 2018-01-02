program atm

    use atm_forcing, only : init_atm_forcing, get_atm_forcing, field_dict
    use atm_params, only : init_atm_params, start_date
    use dictionary
    use cpl_arrays
    use cpl_interfaces

    implicit none

    integer :: timestamp, i, unused
    real, dimension(:, :), allocatable :: work
    real, dimension(2) :: fieldshape
    type(dict) :: tmp

    call init_params()
    call init_atm_forcing("atm_forcing.json", start_date)

    call init_coupler(field_dict)

    timestamp = 0

    do
        ! Iterate over field_dict keys
        tmp = .first. field_dict
        do while(.not. .empty. tmp)
            call assign(fieldshape, .val. tmp)

            ! Read forcing
            allocate(work(fieldshape(1), fieldshape(2)))
            call get_atm_forcing(.key. tmp, cur_date, work)

            ! Send forcing data
            call coupler_send(work, timestamp, work)
            deallocate(work)


            tmp = .next. tmp
        enddo

        timestamp = timestamp + dt
        cur_date = cur_date + timedelta(seconds=dt)

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler_sync()

         if (); exit
    enddo

    call coupler_write_restart()
    call deinit_coupler()

end program atm
