!*****************************************************************************************
!>
!  Tests for user-defined functions.

    module test_user_functions

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_user_functions

    contains

    subroutine collect_user_functions(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("basic", test_basic_user), &
        new_unittest("vector", test_vector_user), &
        new_unittest("variadic", test_variadic), &
        new_unittest("override_builtin", test_override), &
        new_unittest("analytic_derivative", test_analytic_derivative), &
        new_unittest("fd_derivative", test_fd_derivative), &
        new_unittest("errors", test_user_errors) ]
    end subroutine collect_user_functions

    ! --- user functions used in the tests ---

    subroutine sq(args, res, stat)
    !! `sq(x) = x^2` (elementwise)
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    res = args(1)%v**2
    end subroutine sq

    subroutine dsq(args, iarg, jac, stat)
    type(fparse_value), dimension(:), intent(in) :: args
    integer, intent(in) :: iarg
    real(wp), dimension(:,:), allocatable, intent(out) :: jac
    integer, intent(inout) :: stat
    integer :: i, n
    n = size(args(1)%v)
    allocate(jac(n, n), source=0.0_wp)
    do i = 1, n
        jac(i, i) = 2.0_wp * args(1)%v(i)
    end do
    end subroutine dsq

    subroutine wsum(args, res, stat)
    !! `wsum(a, b) = a(1)*b + a(2)` : a is a 2-vector
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    if (size(args(1)%v) /= 2) then
        stat = 1
        return
    end if
    res = args(1)%v(1) * args(2)%v + args(1)%v(2)
    end subroutine wsum

    subroutine count_args(args, res, stat)
    !! returns `[nargs, sum of all elements of all arguments]`
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    integer :: i
    res = [real(size(args), wp), 0.0_wp]
    do i = 1, size(args)
        res(2) = res(2) + sum(args(i)%v)
    end do
    end subroutine count_args

    subroutine my_sin(args, res, stat)
    !! replaces the built-in sin
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    res = 2.0_wp * args(1)%v
    end subroutine my_sin

    subroutine fails(args, res, stat)
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    stat = 42
    end subroutine fails

    subroutine bad_dsq(args, iarg, jac, stat)
    !! returns the wrong shape
    type(fparse_value), dimension(:), intent(in) :: args
    integer, intent(in) :: iarg
    real(wp), dimension(:,:), allocatable, intent(out) :: jac
    integer, intent(inout) :: stat
    allocate(jac(5, 5), source=0.0_wp)
    end subroutine bad_dsq

    ! --- tests ---

    subroutine test_basic_user(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: r
    call p%add_function('Sq', sq, is_pure=.true., nargs=1)
    call p%parse(['x'], ['sq(x) + SQ(2)'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable('x', 3.0_wp)
    call p%evaluate()
    call p%get(1, r)
    call check(error, r, 13.0_wp)
    end subroutine test_basic_user

    subroutine test_vector_user(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:), allocatable :: v
    call p%add_function('wsum', wsum, is_pure=.true., nargs=2)
    call p%add_function('sq', sq, is_pure=.true., nargs=1)
    call p%parse(['a','b'], ['sq(wsum(a, b))'])
    call p%set_variable('a', [2.0_wp, 1.0_wp])
    call p%set_variable('b', [1.0_wp, 2.0_wp, 3.0_wp])
    call p%evaluate()
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get(1, v)
    call check(error, all(v == [9.0_wp, 25.0_wp, 49.0_wp]), 'value'); if (allocated(error)) return
    end subroutine test_vector_user

    subroutine test_variadic(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:), allocatable :: v
    call p%add_function('count', count_args, is_pure=.true.)   ! any number of arguments
    call p%parse(['x'], str_list('count()', 'count(x)', 'count(1, [x, 2], 3)'))
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable('x', 10.0_wp)
    call p%evaluate()
    call p%get(1, v); call check(error, all(v == [0.0_wp, 0.0_wp]), 'count()'); if (allocated(error)) return
    call p%get(2, v); call check(error, all(v == [1.0_wp, 10.0_wp]), 'count(x)'); if (allocated(error)) return
    call p%get(3, v); call check(error, all(v == [3.0_wp, 16.0_wp]), 'count(...)'); if (allocated(error)) return
    end subroutine test_variadic

    subroutine test_override(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: r
    call p%add_function('sin', my_sin, is_pure=.true., nargs=1)
    call p%parse(['x'], ['sin(x)'])
    call p%set_variable('x', 3.0_wp)
    call p%evaluate()
    call p%get(1, r)
    call check(error, r, 6.0_wp)
    end subroutine test_override

    subroutine test_analytic_derivative(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: jac
    call p%add_function('sq', sq, is_pure=.true., nargs=1, df=dsq)
    call p%parse(['x','y'], ['f = sq([x, x*y]) * y'])
    call p%set_variables([2.0_wp, 3.0_wp])
    call p%evaluate(wrt=['x','y'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    ! f = [x^2*y, x^2*y^3]
    call p%get_derivative('f', 'x', jac)
    call check(error, all(abs(jac(:,1) - [2*2.0_wp*3.0_wp, 2*2.0_wp*27.0_wp]) < 1.0e-13_wp), 'df/dx')
    if (allocated(error)) return
    call p%get_derivative('f', 'y', jac)
    call check(error, all(abs(jac(:,1) - [4.0_wp, 3*4.0_wp*9.0_wp]) < 1.0e-13_wp), 'df/dy')
    if (allocated(error)) return
    end subroutine test_analytic_derivative

    subroutine test_fd_derivative(error)
    !! derivative computed using finite differences
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: jac
    call p%add_function('wsum', wsum, is_pure=.true., nargs=2)
    call p%parse(['a','b'], ['f = wsum(a, b)'])
    call p%set_variable('a', [2.0_wp, 1.0_wp])
    call p%set_variable('b', [1.0_wp, 2.0_wp, 3.0_wp])
    call p%evaluate(wrt=['a','b'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get_derivative('f', 'a', jac)
    call check(error, all(shape(jac) == [3, 2]), 'shape'); if (allocated(error)) return
    call check(error, all(abs(jac(:,1) - [1.0_wp, 2.0_wp, 3.0_wp]) < 1.0e-8_wp), 'df/da1'); if (allocated(error)) return
    call check(error, all(abs(jac(:,2) - 1.0_wp) < 1.0e-8_wp), 'df/da2'); if (allocated(error)) return
    call p%get_derivative('f', 'b', jac)
    call check(error, all(shape(jac) == [3, 3]), 'shape'); if (allocated(error)) return
    call check(error, all(abs(jac - reshape([2.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 2.0_wp, 0.0_wp, &
                                             0.0_wp, 0.0_wp, 2.0_wp], [3, 3])) < 1.0e-8_wp), 'df/db')
    end subroutine test_fd_derivative

    subroutine test_user_errors(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg

    ! invalid name
    call p%add_function('1bad', sq, is_pure=.true., status=status, errmsg=errmsg)
    call check(error, status /= 0, 'invalid name should fail'); if (allocated(error)) return

    ! wrong number of arguments
    call p%add_function('sq', sq, is_pure=.true., nargs=1)
    call p%parse(['x'], ['sq(x, x)'], status=status, errmsg=errmsg)
    call check(error, status /= 0, 'wrong number of args should fail'); if (allocated(error)) return
    call check(error, index(errmsg, 'requires 1 argument') > 0, errmsg); if (allocated(error)) return

    ! function returns an error code
    call p%add_function('fails', fails, is_pure=.true.)
    call p%parse(['x'], ['1 + fails(x)'])
    call p%set_variable('x', 1.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status /= 0, 'user function error should propagate'); if (allocated(error)) return
    call check(error, index(errmsg, 'error code 42') > 0, errmsg); if (allocated(error)) return

    ! function error at runtime due to bad input size
    call p%add_function('wsum', wsum, is_pure=.true., nargs=2)
    call p%parse(['x'], ['wsum(x, x)'])
    call p%set_variable('x', 1.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status /= 0, 'user function error should propagate'); if (allocated(error)) return

    ! derivative with the wrong shape
    call p%add_function('sq', sq, is_pure=.true., nargs=1, df=bad_dsq)
    call p%parse(['x'], ['sq(x)'])
    call p%set_variable('x', 1.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status == 0, errmsg); if (allocated(error)) return  ! no derivatives: ok
    call p%evaluate(wrt=['x'], status=status, errmsg=errmsg)
    call check(error, status /= 0, 'wrong jacobian shape should fail'); if (allocated(error)) return
    call check(error, index(errmsg, 'wrong shape') > 0, errmsg); if (allocated(error)) return
    end subroutine test_user_errors

    end module test_user_functions
!*****************************************************************************************
