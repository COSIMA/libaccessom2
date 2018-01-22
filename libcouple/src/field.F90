
module field

use error_handler , only : assert
use datetime_module, only: datetime

implicit none

private
public field_type

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
    procedure :: set_oasis_id => field_set_id
    procedure :: get_filename => field_get_filename
end type field_type

contains

subroutine field_init(this, name, dims)

    class(field_type), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, dimension(2), intent(in) :: dims

    this%name = trim(name)

    call assert(.not. allocated(this%array), 'Field data already allocated')
    allocate(this%array(nx(1), ny(2)))

end subroutine field_init

subroutine field_update(this, date, data) 

    class(field_type), intent(inout) :: this
    type(datetime), intent(in) :: date
    real, dimension(:, :), intent(in) :: data

    ! Update the data. 
    this%timestamp = date
    this%array(:, :) = data(:, :)

end subroutine field_update

subroutine field_set_oasis_id(this, id)

    class(field_type), intent(inout) :: this
    integer, intent(in) :: id

    this%oasis_id = id
 
end subroutine field_set_id

end module field
