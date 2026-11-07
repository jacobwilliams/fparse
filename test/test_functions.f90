!*****************************************************************************************
!>
!  Tests for the built-in functions (scalar arguments).

    module test_functions

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type
    use test_utils

    implicit none

    private

    public :: collect_functions

    contains

    subroutine collect_functions(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("elementary", test_elementary), &
        new_unittest("trig", test_trig), &
        new_unittest("hyperbolic", test_hyperbolic), &
        new_unittest("rounding", test_rounding), &
        new_unittest("two_arg", test_two_arg), &
        new_unittest("max_min", test_max_min), &
        new_unittest("aliases", test_aliases), &
        new_unittest("nested", test_nested) ]
    end subroutine collect_functions

    subroutine test_elementary(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp), parameter :: x = 0.7_wp
    call check_expr(error, 'abs(-x)', x, ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'sqrt(x)', sqrt(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'exp(x)', exp(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'log(x)', log(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'log10(x)', log10(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'erf(x)', erf(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'erfc(x)', erfc(x), ['x'], [x]); if (allocated(error)) return
    end subroutine test_elementary

    subroutine test_trig(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp), parameter :: x = 0.3_wp
    call check_expr(error, 'sin(x)', sin(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'cos(x)', cos(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'tan(x)', tan(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'asin(x)', asin(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'acos(x)', acos(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'atan(x)', atan(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'atan2(x, -1)', atan2(x, -1.0_wp), ['x'], [x]); if (allocated(error)) return
    end subroutine test_trig

    subroutine test_hyperbolic(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp), parameter :: x = 0.4_wp
    call check_expr(error, 'sinh(x)', sinh(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'cosh(x)', cosh(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'tanh(x)', tanh(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'asinh(x)', asinh(x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'acosh(1+x)', acosh(1.0_wp+x), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'atanh(x)', atanh(x), ['x'], [x]); if (allocated(error)) return
    end subroutine test_hyperbolic

    subroutine test_rounding(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'floor(2.7)', 2.0_wp); if (allocated(error)) return
    call check_expr(error, 'floor(-2.2)', -3.0_wp); if (allocated(error)) return
    call check_expr(error, 'ceiling(2.2)', 3.0_wp); if (allocated(error)) return
    call check_expr(error, 'ceiling(-2.7)', -2.0_wp); if (allocated(error)) return
    call check_expr(error, 'ceiling(3)', 3.0_wp); if (allocated(error)) return
    call check_expr(error, 'nint(2.5)', 3.0_wp); if (allocated(error)) return
    call check_expr(error, 'nint(-2.4)', -2.0_wp); if (allocated(error)) return
    call check_expr(error, 'aint(-2.7)', -2.0_wp); if (allocated(error)) return
    call check_expr(error, 'floor(1e20)', 1.0e20_wp); if (allocated(error)) return
    end subroutine test_rounding

    subroutine test_two_arg(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'mod(7, 3)', 1.0_wp); if (allocated(error)) return
    call check_expr(error, 'mod(-7, 3)', -1.0_wp); if (allocated(error)) return
    call check_expr(error, 'modulo(-7, 3)', 2.0_wp); if (allocated(error)) return
    call check_expr(error, 'mod(5.5, 2)', 1.5_wp); if (allocated(error)) return
    call check_expr(error, 'sign(3, -1)', -3.0_wp); if (allocated(error)) return
    call check_expr(error, 'sign(-3, 2)', 3.0_wp); if (allocated(error)) return
    call check_expr(error, 'hypot(3, 4)', 5.0_wp); if (allocated(error)) return
    call check_expr(error, 'atan2(1, 1)', atan(1.0_wp)); if (allocated(error)) return
    end subroutine test_two_arg

    subroutine test_max_min(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'max(x, y)', 3.0_wp, ['x','y'], [2.0_wp, 3.0_wp]); if (allocated(error)) return
    call check_expr(error, 'min(x, y)', 2.0_wp, ['x','y'], [2.0_wp, 3.0_wp]); if (allocated(error)) return
    call check_expr(error, 'max(1, 5, 3, -2)', 5.0_wp); if (allocated(error)) return
    call check_expr(error, 'min(1, 5, 3, -2)', -2.0_wp); if (allocated(error)) return
    end subroutine test_max_min

    subroutine test_aliases(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_expr(error, 'ln(2)', log(2.0_wp)); if (allocated(error)) return
    call check_expr(error, 'ceil(1.1)', 2.0_wp); if (allocated(error)) return
    call check_expr(error, 'round(1.6)', 2.0_wp); if (allocated(error)) return
    call check_expr(error, 'trunc(-1.6)', -1.0_wp); if (allocated(error)) return
    call check_expr(error, 'merge(1, 2, 0)', 2.0_wp); if (allocated(error)) return
    end subroutine test_aliases

    subroutine test_nested(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp), parameter :: x = 0.5_wp, y = 1.5_wp
    call check_expr(error, 'sin(cos(exp(x)))', sin(cos(exp(x))), ['x'], [x]); if (allocated(error)) return
    call check_expr(error, 'sqrt(x^2 + y^2) * atan2(y, x) - log(1 + abs(sin(x*y)))', &
                    sqrt(x**2 + y**2) * atan2(y, x) - log(1.0_wp + abs(sin(x*y))), &
                    ['x','y'], [x, y]); if (allocated(error)) return
    call check_expr(error, 'max(sin(x), cos(y), x*y/3)', max(sin(x), cos(y), x*y/3.0_wp), &
                    ['x','y'], [x, y]); if (allocated(error)) return
    end subroutine test_nested

    end module test_functions
!*****************************************************************************************
