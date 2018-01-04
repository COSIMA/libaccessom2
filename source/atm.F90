program atm

    use forcing_mod, only : forcing_type
    use params_mod, only : params_type
    use coupler_mod, only : coupler_type

    implicit none

    type(params_type) :: params  
    type(forcing_type) :: forcing  

    call params%init()
    call forcing%init("atm_forcing.json", params%start_date, params%period)

    call coupler%init(forcing%get_fields())
    do i=1, forcing%num_fields()
        coupler%add_field(forcing%fields(i))
    enddo
    call coupler%enddef()

    do
        do i=1, forcing%num_fields()
            forcing%fields(i)%update(cur_date)
            coupler%put(forcing%fields(i))
        enddo

        cur_date = cur_date + timedelta(seconds=dt)

        ! Block until we receive from ice. This prevents the atm from sending
        ! continuously.
        call coupler%sync()

        if (cur_date > end_date); exit
    enddo

    call coupler%write_restart()
    call coupler%deinit()

end program atm
