!*****************************************************************************************
!>
!  Tests for `if` expressions.

    module test_conditionals

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type
    use test_utils

    implicit none

    private

    public :: collect_conditionals

    contains

    subroutine collect_conditionals(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("scalar_if", test_scalar_if), &
        new_unittest("nested_if", test_nested_if), &
        new_unittest("lazy_if", test_lazy_if), &
        new_unittest("vector_if", test_vector_if) ]
    end subroutine collect_conditionals

    subroutine test_scalar_if(error)
    type(error_type), allocatable, intent(out) :: error
    call check_expr(error, 'if(10, 20, 1)', 10.0_wp); if (allocated(error)) return
    call check_expr(error, 'if(10, 20, 0)', 20.0_wp); if (allocated(error)) return
    call check_expr(error, 'if(10, 20, -0.5)', 10.0_wp); if (allocated(error)) return   ! nonzero is true
    call check_expr(error, 'if(x, -x, x > 0)', 2.0_wp, ['x'], [2.0_wp]); if (allocated(error)) return
    call check_expr(error, 'if(x, -x, x > 0)', 2.0_wp, ['x'], [-2.0_wp]); if (allocated(error)) return
    call check_expr(error, 'if(sin(x), cos(x), x >= 1 && x < 2)', sin(1.5_wp), ['x'], [1.5_wp]); if (allocated(error)) return
    call check_expr(error, '1 + if(1, 2, x) * 3', 7.0_wp, ['x'], [0.0_wp]); if (allocated(error)) return
    end subroutine test_scalar_if

    subroutine test_nested_if(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), parameter :: sgn = 'if(1, if(-1, 0, x < 0), x > 0)'
    character(len=*), parameter :: grade = 'if(4, if(3, if(2, 1, x >= 60), x >= 70), x >= 80)'
    call check_expr(error, sgn, 1.0_wp, ['x'], [5.0_wp]); if (allocated(error)) return
    call check_expr(error, sgn, -1.0_wp, ['x'], [-5.0_wp]); if (allocated(error)) return
    call check_expr(error, sgn, 0.0_wp, ['x'], [0.0_wp]); if (allocated(error)) return
    call check_expr(error, grade, 4.0_wp, ['x'], [95.0_wp]); if (allocated(error)) return
    call check_expr(error, grade, 3.0_wp, ['x'], [75.0_wp]); if (allocated(error)) return
    call check_expr(error, grade, 2.0_wp, ['x'], [65.0_wp]); if (allocated(error)) return
    call check_expr(error, grade, 1.0_wp, ['x'], [5.0_wp]); if (allocated(error)) return
    ! nested in the condition:
    call check_expr(error, 'if(1, 2, if(x, y, x > y) > 2)', 1.0_wp, ['x','y'], [3.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_expr(error, 'if(1, 2, if(x, y, x > y) > 2)', 2.0_wp, ['x','y'], [1.0_wp, 2.0_wp]); if (allocated(error)) return
    end subroutine test_nested_if

    subroutine test_lazy_if(error)
    !! with a scalar condition, only the selected branch is evaluated
    type(error_type), allocatable, intent(out) :: error
    call check_expr(error, 'if(log(x), 0, x > 0)', 0.0_wp, ['x'], [-1.0_wp]); if (allocated(error)) return
    call check_expr(error, 'if(1/x, 0, x /= 0)', 0.0_wp, ['x'], [0.0_wp]); if (allocated(error)) return
    call check_expr(error, 'if(sqrt(x), sqrt(-x), x >= 0)', 2.0_wp, ['x'], [-4.0_wp]); if (allocated(error)) return
    end subroutine test_lazy_if

    subroutine test_vector_if(error)
    !! with a vector condition, the selection is element-wise
    type(error_type), allocatable, intent(out) :: error
    real(wp), dimension(4), parameter :: x = [-2.0_wp, -1.0_wp, 1.0_wp, 2.0_wp]
    call check_vexpr(error, 'if(x, -x, x > 0)', abs(x), ['x'], [fparse_value(x)]); if (allocated(error)) return
    call check_vexpr(error, 'if(x, 0, x > 0)', max(x, 0.0_wp), ['x'], [fparse_value(x)]); if (allocated(error)) return
    call check_vexpr(error, 'if(1, 0, x)', [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp], ['x'], [fparse_value(x)])
    if (allocated(error)) return
    call check_vexpr(error, 'if(x, 2*x, 1)', x, ['x'], [fparse_value(x)]); if (allocated(error)) return
    call check_vexpr(error, 'if(x^2, if(x, 0, x<-1.5), x > 0)', [-2.0_wp, 0.0_wp, 1.0_wp, 4.0_wp], &
                     ['x'], [fparse_value(x)]); if (allocated(error)) return
    end subroutine test_vector_if

    end module test_conditionals
!*****************************************************************************************
