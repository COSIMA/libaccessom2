module params_mod

implicit none

private
public params_type

type params_type
    type(datetime) :: start_date, end_date
    integer :: forcing_period_years
    logical :: debug_output
end type params_type

namelist /atm_nml/ start_date, end_date forcing_period_years, &
                   debug_output

contains

subroutine params_init(this)

    class(params_type), intent(inout) :: this

    character(len=19) :: start_date, end_date
    integer :: forcing_period_years
    logical :: debug_output

    start_date = '1900-01-01 00:00:00'
    end_date = '1901-01-01 00:00:00'
    forcing_period_years = 1
    debug_output = .false.

    ! Rean input namelist
    open(unit=99, file="atm.nml", form="formatted", status="old")
    read(99, nml=atm_nml)
    close(unit=99)

    this%start_date = strptime(start_date, '%Y-%m-%d %H:%M:%S')
    this%end_date = strptime(end_date, '%Y-%m-%d %H:%M:%S')
    this%forcing_period_years = forcing_period_years
    this%debug_output = debug_output

end subroutine params_init

end module params_mod
