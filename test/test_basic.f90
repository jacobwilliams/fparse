!*****************************************************************************************
!>
!  Tests for basic arithmetic, operator precedence, numbers, logic and case insensitivity.

    module test_basic

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_basic

    contains

    subroutine collect_basic(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("numbers", test_numbers), &
        new_unittest("arithmetic", test_arithmetic), &
        new_unittest("precedence", test_precedence), &
        new_unittest("relational", test_relational), &
        new_unittest("logical", test_logical), &
        new_unittest("case_insensitive", test_case), &
        new_unittest("whitespace", test_whitespace), &
        new_unittest("constants", test_constants), &
        new_unittest("reuse_parser", test_reuse) ]
    end subroutine collect_basic

    subroutine test_numbers(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '42', 42.0_wp); if (allocated(error)) return
    call check_expr(error, '3.25', 3.25_wp); if (allocated(error)) return
    call check_expr(error, '.5', 0.5_wp); if (allocated(error)) return
    call check_expr(error, '5.', 5.0_wp); if (allocated(error)) return
    call check_expr(error, '1e3', 1000.0_wp); if (allocated(error)) return
    call check_expr(error, '1.5E-2', 0.015_wp); if (allocated(error)) return
    call check_expr(error, '2.5d0', 2.5_wp); if (allocated(error)) return
    call check_expr(error, '2.5D+1', 25.0_wp); if (allocated(error)) return
    call check_expr(error, '1.e2', 100.0_wp); if (allocated(error)) return
    call check_expr(error, '-7', -7.0_wp); if (allocated(error)) return
    call check_expr(error, '1.0.and.0', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '1.eq.1', 1.0_wp); if (allocated(error)) return
    end subroutine test_numbers

    subroutine test_arithmetic(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '1+2', 3.0_wp); if (allocated(error)) return
    call check_expr(error, '5-8', -3.0_wp); if (allocated(error)) return
    call check_expr(error, '3*4', 12.0_wp); if (allocated(error)) return
    call check_expr(error, '7/2', 3.5_wp); if (allocated(error)) return
    call check_expr(error, '2^10', 1024.0_wp); if (allocated(error)) return
    call check_expr(error, '2**10', 1024.0_wp); if (allocated(error)) return
    call check_expr(error, '(-2)^3', -8.0_wp); if (allocated(error)) return
    call check_expr(error, '4^0.5', 2.0_wp); if (allocated(error)) return
    call check_expr(error, '2^-1', 0.5_wp); if (allocated(error)) return
    call check_expr(error, 'x*y - x/y', 2.0_wp*4.0_wp - 0.5_wp, ['x','y'], [2.0_wp, 4.0_wp]); if (allocated(error)) return
    call check_expr(error, '--x', 3.0_wp, ['x'], [3.0_wp]); if (allocated(error)) return
    call check_expr(error, '+x', 3.0_wp, ['x'], [3.0_wp]); if (allocated(error)) return
    call check_expr(error, '-x', -3.0_wp, ['x'], [3.0_wp]); if (allocated(error)) return
    call check_expr(error, '0^0', 1.0_wp); if (allocated(error)) return
    end subroutine test_arithmetic

    subroutine test_precedence(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '1+2*3', 7.0_wp); if (allocated(error)) return
    call check_expr(error, '(1+2)*3', 9.0_wp); if (allocated(error)) return
    call check_expr(error, '10-4-3', 3.0_wp); if (allocated(error)) return
    call check_expr(error, '24/4/2', 3.0_wp); if (allocated(error)) return
    call check_expr(error, '2^3^2', 512.0_wp); if (allocated(error)) return  ! right associative
    call check_expr(error, '-2^2', -4.0_wp); if (allocated(error)) return   ! -(2^2)
    call check_expr(error, '-x^2', -9.0_wp, ['x'], [3.0_wp]); if (allocated(error)) return
    call check_expr(error, '2*3^2', 18.0_wp); if (allocated(error)) return
    call check_expr(error, '1 + 2 < 4', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 < 2 && 3 < 2 || 1', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 || 0 && 0', 1.0_wp); if (allocated(error)) return  ! && binds tighter
    call check_expr(error, '.not. 1 > 2', 1.0_wp); if (allocated(error)) return  ! not(1>2)
    call check_expr(error, '((((1))))', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '2*-3', -6.0_wp); if (allocated(error)) return
    end subroutine test_precedence

    subroutine test_relational(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '1 == 1', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 == 2', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '1 /= 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 != 1', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '1 < 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '2 <= 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 > 2', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '2 >= 3', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '1 .lt. 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '2 .LE. 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '3 .gt. 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '2 .ge. 3', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '2 .eq. 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '2 .ne. 2', 0.0_wp); if (allocated(error)) return
    end subroutine test_relational

    subroutine test_logical(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '1 && 1', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 && 0', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '0 || 0', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '0 || 5', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '!0', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '!3', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '.not. 0', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 .and. 2', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '0 .or. 0', 0.0_wp); if (allocated(error)) return
    call check_expr(error, '-2 && 0.1', 1.0_wp); if (allocated(error)) return
    call check_expr(error, '1 + !0', 2.0_wp); if (allocated(error)) return
    end subroutine test_logical

    subroutine test_case(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'X + x', 4.0_wp, ['x'], [2.0_wp]); if (allocated(error)) return
    call check_expr(error, 'SIN(x) + Cos(X)', sin(2.0_wp) + cos(2.0_wp), ['X'], [2.0_wp]); if (allocated(error)) return
    call check_expr(error, 'Alpha_1 * 2', 6.0_wp, ['ALPHA_1'], [3.0_wp]); if (allocated(error)) return
    call check_expr(error, 'PI', acos(-1.0_wp)); if (allocated(error)) return
    end subroutine test_case

    subroutine test_whitespace(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, '   1   +   2   ', 3.0_wp); if (allocated(error)) return
    call check_expr(error, 'sin ( x )', sin(1.0_wp), ['x'], [1.0_wp]); if (allocated(error)) return
    call check_expr(error, achar(9)//'2'//achar(9)//'*3', 6.0_wp); if (allocated(error)) return
    end subroutine test_whitespace

    subroutine test_constants(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'pi', acos(-1.0_wp)); if (allocated(error)) return
    call check_expr(error, 'cos(pi)', -1.0_wp); if (allocated(error)) return
    ! a variable named pi takes precedence
    call check_expr(error, 'pi', 3.0_wp, ['pi'], [3.0_wp]); if (allocated(error)) return
    end subroutine test_constants

    subroutine test_reuse(error)
    !! the same parser can be evaluated repeatedly, and re-parsed
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    integer :: i
    call p%parse(['x'], ['x^2'])
    do i = 1, 5
        call p%set_variable('x', real(i, wp))
        call p%evaluate()
        call p%get(1, r)
        call check(error, r, real(i*i, wp)); if (allocated(error)) return
    end do
    call p%parse(['a', 'b'], ['a-b'])
    call p%set_variables([5.0_wp, 3.0_wp])
    call p%evaluate()
    call p%get(1, r)
    call check(error, r, 2.0_wp); if (allocated(error)) return
    call check(error, p%num_inputs(), 2); if (allocated(error)) return
    call check(error, p%num_outputs(), 1); if (allocated(error)) return
    call check(error, p%input_name(2), 'b'); if (allocated(error)) return
    call p%destroy()
    call check(error, p%num_inputs(), 0); if (allocated(error)) return
    end subroutine test_reuse

    end module test_basic
!*****************************************************************************************
