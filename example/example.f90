!*****************************************************************************************
!>
!  Example usage of the fparse library.

    program example

    use fparse

    implicit none

    type(fparser) :: p
    real(wp) :: w, dwdx
    real(wp), dimension(:), allocatable :: c
    real(wp), dimension(:,:), allocatable :: jac
    integer :: status
    character(len=:), allocatable :: errmsg

    ! scalar expressions that depend on each other (the order is determined automatically)
    call p%parse(['x', 'y'], [character(len=40) :: &
                 'w = z + x', &
                 'z = sin(x) + 2.0*y', &
                 'q = if(x**2, -x, x > 0)'], status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg

    call p%set_variable('x', 1.0_wp)
    call p%set_variable('y', 2.0_wp)
    call p%evaluate(wrt=['x'], status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg

    call p%get('w', w)
    call p%get_derivative('w', 'x', dwdx)
    write(*, '(A,F12.6)') 'w      = ', w
    write(*, '(A,F12.6)') 'dw/dx  = ', dwdx

    ! vector expressions
    call p%parse([character(len=1) :: 'a', 'b'], ['c = cross(a, b) * norm2(a)'], &
                 status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    call p%set_variable('a', [1.0_wp, 2.0_wp, 3.0_wp])
    call p%set_variable('b', [4.0_wp, 5.0_wp, 6.0_wp])
    call p%evaluate(wrt=['a'], status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    call p%get('c', c)
    call p%get_derivative('c', 'a', jac)
    write(*, '(A,3F12.6)') 'c      = ', c
    write(*, '(A,3F12.6)') 'dc/da  = ', jac(1,:)
    write(*, '(A,3F12.6)') '         ', jac(2,:)
    write(*, '(A,3F12.6)') '         ', jac(3,:)

    ! error example
    call p%parse(['x'], ['sin(x'], status=status, errmsg=errmsg)
    write(*, '(A)') errmsg

    end program example
!*****************************************************************************************
