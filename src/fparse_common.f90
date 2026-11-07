!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Common kinds, string, and error types used throughout the fparse library.

module fparse_common

use, intrinsic :: iso_fortran_env, only: real64

implicit none (type, external)

private

integer, parameter, public :: wp = real64 !! real kind used by the library

type, public :: string_t
    !! A variable-length string (used to build arrays of strings
    !! with different lengths).
    character(len=:), allocatable :: str
end type string_t

type, public :: val_t
    !! A value (and optionally its derivative). Scalars are arrays of size 1.
    real(wp), dimension(:), allocatable :: v !! the value
    real(wp), dimension(:,:), allocatable :: d !! the derivative `[size(v), nd]`
                                                !! (only used if derivatives are being computed)
end type val_t

type, public :: error_t
    !! Error status information.
    logical :: failed = .false. !! true if an error has occurred
    character(len=:), allocatable :: message !! the error message
    integer :: pos = 0 !! character position in the expression where
                       !! the error occurred (0 if not applicable)
contains
    procedure :: set => error_set
    procedure :: clear => error_clear
end type error_t

public :: to_lower
public :: int_to_str
public :: is_valid_name

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set an error.

subroutine error_set(me, message, pos)

    class(error_t), intent(inout) :: me !! the error object
    character(len=*), intent(in) :: message !! the error message
    integer, intent(in), optional :: pos !! position in the expression string

    me%failed = .true.
    me%message = message
    if (present(pos)) then
        me%pos = pos
    else
        me%pos = 0
    end if

end subroutine error_set
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear an error.

subroutine error_clear(me)

    class(error_t), intent(inout) :: me !! the error object

    me%failed = .false.
    me%pos = 0
    if (allocated(me%message)) deallocate(me%message)

end subroutine error_clear
!*****************************************************************************************

!*****************************************************************************************
!>
!  Return a lowercase copy of a string.

pure function to_lower(str) result(lower)

    character(len=*), intent(in) :: str !! the string
    character(len=len(str)) :: lower !! the string in lowercase

    integer :: i, ic

    lower = str
    do i = 1, len(str)
        ic = iachar(str(i:i))
        if (ic >= iachar('A') .and. ic <= iachar('Z')) then
            lower(i:i) = achar(ic + (iachar('a') - iachar('A')))
        end if
    end do

end function to_lower
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert an integer to a string.

pure function int_to_str(i) result(str)

    integer, intent(in) :: i !! the integer
    character(len=:), allocatable :: str !! the integer as a string

    character(len=32) :: buffer

    write(buffer, '(I0)') i
    str = trim(buffer)

end function int_to_str
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the string is a valid name for a variable or function:
!  a letter followed by letters, digits, or underscores.

pure logical function is_valid_name(str)

    character(len=*), intent(in) :: str !! the name to check

    integer :: i
    character(len=1) :: c

    is_valid_name = .false.
    if (len(str) == 0) return
    c = to_lower(str(1:1))
    if (c < 'a' .or. c > 'z') return
    do i = 2, len(str)
        c = to_lower(str(i:i))
        if (.not. ((c >= 'a' .and. c <= 'z') .or. (c >= '0' .and. c <= '9') .or. c == '_')) return
    end do
    is_valid_name = .true.

end function is_valid_name
!*****************************************************************************************

!*****************************************************************************************
end module fparse_common
!*****************************************************************************************
