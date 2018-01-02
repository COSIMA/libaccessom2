program test_forcing
  
    use atm_forcing, only : parse_forcing, get_forcing

    implicit none

    real, dimension(10, 10) :: field

    call parse_forcing('atm_forcing.json')
    call get_forcing('tair', field)

end program test_forcing
