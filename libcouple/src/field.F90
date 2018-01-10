
module field

use error_handler , only : assert
use datetime_module, only: datetime

implicit none

private
public init, update, set_id

type field_type
    private
    integer :: oasis_id
    character(len=64) :: name
    type(datetime) :: timestamp
    real, dimension(:,:), allocatable :: array
contains
    public     
    procedure :: init => field_init
    procedure :: update => field_update
    procedure :: set_id => field_set_id
    procedure :: get_filename => field_get_filename
end type field_type

contains

subroutine field_init(this, name, nx, ny)

    class(field_type), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: nx, ny

    this%name = trim(name)

    call assert(.not. allocated(this%array), 'Field data already allocated')
    allocate(this%array(nx, ny))

end subroutine field_init

subroutine field_update(this, date) 

    class(field_type), intent(inout) :: this
    type(datetime), intent(in) :: date

    ! Update the data. 
    this%timestamp = date

end subroutine field_update

subroutine field_set_id(this, id)

    class(field_type), intent(inout) :: this
    integer, intent(in) :: id

    this%id = id
 
end subroutine field_set_id

end module field
