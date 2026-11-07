!*****************************************************************************************
!>
!  Tests for caching: [[fparser:evaluate]] reuses the previous results
!  if the inputs haven't changed.
!
!  A user function that counts its calls is used to detect if the
!  expressions were actually evaluated.

    module test_cache

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_cache

    integer :: ncalls = 0 !! number of calls to `counted`

    contains

    subroutine collect_cache(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("values", test_values_cached), &
        new_unittest("derivatives", test_derivatives_cached), &
        new_unittest("impure_functions", test_impure), &
        new_unittest("disabled", test_disabled), &
        new_unittest("exact_comparison", test_exact), &
        new_unittest("invalidation", test_invalidation), &
        new_unittest("errors_not_cached", test_errors_not_cached) ]
    end subroutine collect_cache

    subroutine counted(args, res, stat)
    !! `counted(x) = 2*x`, counting the calls
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    ncalls = ncalls + 1
    res = 2.0_wp * args(1)%v
    end subroutine counted

    subroutine counted3(args, res, stat)
    !! `counted3(x) = 3*x`, counting the calls
    type(fparse_value), dimension(:), intent(in) :: args
    real(wp), dimension(:), allocatable, intent(out) :: res
    integer, intent(inout) :: stat
    ncalls = ncalls + 1
    res = 3.0_wp * args(1)%v
    end subroutine counted3

    subroutine setup(p, is_pure)
    !! `f = counted(x) + y`, `g = f * y`, with x = 1, y = 2
    type(fparser), intent(out) :: p
    logical, intent(in) :: is_pure
    call p%add_function('counted', counted, is_pure=is_pure, nargs=1)
    call p%parse(['x', 'y'], str_list('f = counted(x) + y', 'g = f * y'))
    call p%set_variable('x', 1.0_wp)
    call p%set_variable('y', 2.0_wp)
    ncalls = 0
    end subroutine setup

    subroutine test_values_cached(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: r
    call setup(p, .true.)
    call p%evaluate()
    call check(error, ncalls, 1); if (allocated(error)) return
    call p%evaluate() ! nothing changed
    call check(error, ncalls, 1); if (allocated(error)) return
    call p%get('g', r)
    call check(error, r, 8.0_wp); if (allocated(error)) return
    call p%set_variable('x', 1.0_wp) ! same value: not a change
    call p%set_variables([1.0_wp, 2.0_wp])
    call p%evaluate()
    call check(error, ncalls, 1); if (allocated(error)) return
    call p%set_variable('x', 2.0_wp) ! changed
    call p%evaluate()
    call check(error, ncalls, 2); if (allocated(error)) return
    call p%get('g', r)
    call check(error, r, 12.0_wp); if (allocated(error)) return
    call p%set_variables([2.0_wp, 3.0_wp]) ! changed
    call p%evaluate()
    call check(error, ncalls, 3); if (allocated(error)) return
    call p%get('g', r)
    call check(error, r, 21.0_wp); if (allocated(error)) return
    end subroutine test_values_cached

    subroutine test_derivatives_cached(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: d, r
    integer :: n, status
    call setup(p, .true.)
    call p%evaluate(wrt=['x', 'y'])
    n = ncalls ! includes the finite-difference calls
    call p%get_derivative('g', 'x', d)
    call check(error, d, 4.0_wp, thr=1.0e-8_wp); if (allocated(error)) return
    call p%evaluate(wrt=['x', 'y']) ! same request, nothing changed
    call check(error, ncalls, n); if (allocated(error)) return
    call p%get_derivative('g', 'x', d)
    call check(error, d, 4.0_wp, thr=1.0e-8_wp); if (allocated(error)) return
    call p%get_derivative('g', 'y', d)
    call check(error, d, 6.0_wp, thr=1.0e-8_wp); if (allocated(error)) return
    call p%evaluate(wrt=['y']) ! different request: recomputed
    call check(error, ncalls > n, 'different wrt should be recomputed'); if (allocated(error)) return
    n = ncalls
    call p%evaluate() ! values only: reused, and the derivatives are discarded
    call check(error, ncalls, n); if (allocated(error)) return
    call p%get('g', r)
    call check(error, r, 8.0_wp); if (allocated(error)) return
    call p%get_derivative('g', 'y', d, status=status)
    call check(error, status /= 0, 'derivatives should have been discarded'); if (allocated(error)) return
    call p%evaluate(wrt=['y']) ! the derivatives must be recomputed
    call check(error, ncalls > n, 'derivatives should be recomputed'); if (allocated(error)) return
    call p%get_derivative('g', 'y', d)
    call check(error, d, 6.0_wp, thr=1.0e-8_wp); if (allocated(error)) return
    end subroutine test_derivatives_cached

    subroutine test_impure(error)
    !! with an impure function, the results are never cached
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    integer :: n
    call setup(p, .false.)
    call p%evaluate()
    call p%evaluate()
    call check(error, ncalls, 2); if (allocated(error)) return
    call p%evaluate(wrt=['x'])
    n = ncalls
    call p%evaluate(wrt=['x'])
    call check(error, ncalls > n, 'impure: derivatives should be recomputed'); if (allocated(error)) return
    ! an impure function that is not used doesn't prevent caching:
    call p%add_function('other', counted, is_pure=.false.)
    call p%add_function('counted', counted, is_pure=.true.)
    call p%evaluate()
    n = ncalls
    call p%evaluate()
    call check(error, ncalls, n); if (allocated(error)) return
    end subroutine test_impure

    subroutine test_disabled(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    call setup(p, .true.)
    call p%set_caching(.false.)
    call p%evaluate()
    call p%evaluate()
    call check(error, ncalls, 2); if (allocated(error)) return
    call p%set_caching(.true.)
    call p%evaluate()
    call p%evaluate()
    call check(error, ncalls, 3); if (allocated(error)) return
    end subroutine test_disabled

    subroutine test_exact(error)
    !! the inputs must be exactly the same
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: nan
    call setup(p, .true.)
    call p%set_variable('x', 0.0_wp)
    call p%evaluate()
    call check(error, ncalls, 1); if (allocated(error)) return
    call p%set_variable('x', -0.0_wp) ! a different zero
    call p%evaluate()
    call check(error, ncalls, 2); if (allocated(error)) return
    call p%set_variable('x', nextafter_up(-0.0_wp)) ! tiny change
    call p%evaluate()
    call check(error, ncalls, 3); if (allocated(error)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    call p%set_domain_checking(.false.)
    call p%set_variable('x', nan) ! NaNs are never the same
    call p%evaluate()
    call p%set_variable('x', nan)
    call p%evaluate()
    call check(error, ncalls, 5); if (allocated(error)) return
    ! vectors:
    call p%set_variable('x', [1.0_wp, 2.0_wp])
    call p%evaluate()
    call p%set_variable('x', [1.0_wp, 2.0_wp])
    call p%evaluate()
    call check(error, ncalls, 6); if (allocated(error)) return
    call p%set_variable('x', [1.0_wp, 2.0_wp, 3.0_wp]) ! different size
    call p%evaluate()
    call check(error, ncalls, 7); if (allocated(error)) return
    call p%set_variable('x', 1.0_wp) ! back to a scalar
    call p%evaluate()
    call check(error, ncalls, 8); if (allocated(error)) return
    end subroutine test_exact

    pure real(wp) function nextafter_up(x)
    real(wp), intent(in) :: x
    nextafter_up = nearest(x, 1.0_wp)
    end function nextafter_up

    subroutine test_invalidation(error)
    !! changing options or functions invalidates the cache
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: r
    call setup(p, .true.)
    call p%evaluate()
    call p%set_domain_checking(.false.)
    call p%evaluate()
    call check(error, ncalls, 2); if (allocated(error)) return
    call p%set_scalar_optimization(.false.)
    call p%evaluate()
    call check(error, ncalls, 3); if (allocated(error)) return
    call p%set_sparsity(.false.)
    call p%evaluate()
    call check(error, ncalls, 4); if (allocated(error)) return
    call p%evaluate()
    call check(error, ncalls, 4); if (allocated(error)) return
    ! replacing the function changes the result:
    call p%add_function('counted', counted3, is_pure=.true., nargs=1)
    call p%evaluate()
    call check(error, ncalls, 5); if (allocated(error)) return
    call p%get('f', r)
    call check(error, r, 5.0_wp); if (allocated(error)) return
    ! parsing again:
    call p%parse(['x', 'y'], str_list('f = counted(x) + y'))
    call p%set_variables([1.0_wp, 2.0_wp])
    call p%evaluate()
    call check(error, ncalls, 6); if (allocated(error)) return
    end subroutine test_invalidation

    subroutine test_errors_not_cached(error)
    !! a failed evaluation is not cached
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    integer :: status
    call p%parse(['x'], ['log(x)'])
    call p%set_variable('x', -1.0_wp)
    call p%evaluate(status=status)
    call check(error, status /= 0, 'should fail'); if (allocated(error)) return
    call p%evaluate(status=status)
    call check(error, status /= 0, 'should fail again'); if (allocated(error)) return
    call check(error, index(p%error_message(), 'log') > 0, p%error_message()); if (allocated(error)) return
    end subroutine test_errors_not_cached

    end module test_cache
!*****************************************************************************************
