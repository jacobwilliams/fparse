!*****************************************************************************************
!>
!  Tests that the fast scalar evaluator and the general tree evaluator
!  give the same results, errors, and derivatives.

    module test_consistency

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, test_failed
    use test_utils

    implicit none

    private

    public :: collect_consistency

    character(len=*), dimension(*), parameter :: exprs = [character(len=80) :: &
        'x + y*z - x/y', 'x^2 + y^3 - z^-1', '(-x)^3', 'x^y', 'x^0.5 + 2^x', '-x^2', &
        'abs(x - y) + sqrt(y) + exp(z) + log(y) + log10(z)', &
        'sin(x) + cos(y) + tan(z) + asin(x) + acos(x) + atan(y)', &
        'sinh(x) + cosh(y) + tanh(z) + asinh(y) + acosh(y + 1) + atanh(x)', &
        'erf(x) + erfc(y) + floor(y) + ceiling(z) + nint(y) + aint(z)', &
        'atan2(x, y) + mod(y, x) + modulo(-y, x) + sign(x, -z) + hypot(x, y)', &
        'max(x, y) + min(x, y, z) + max(x, y, z, 1.5)', &
        'sum(x) + product(y) + norm2(-z) + maxval(x) + minval(y) + size(z)', &
        'dot_product(x, y) + [x][1]*0 + [y]*2', &
        'x < y && y <= z || x == y', '!(x > y) + (x /= y) + (x >= z)', &
        'if(x, y, x > y)', 'if(if(1, 2, x < y), if(3, 4, z), y > z) + x', &
        'if(log(x - 10), sqrt(y), x > 10)', &
        'pi * x' ]

    contains

    subroutine collect_consistency(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("values", test_values), &
        new_unittest("errors", test_errors_both), &
        new_unittest("derivatives", test_derivs_both), &
        new_unittest("mixed_sizes", test_mixed_sizes) ]
    end subroutine collect_consistency

    subroutine eval_both(error, expr, vals, wrt, r1, r2, msg1, msg2, d1, d2)
    !! evaluate an expression with both evaluators
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: expr
    real(wp), dimension(3), intent(in) :: vals
    logical, intent(in) :: wrt
    real(wp), intent(out) :: r1, r2
    character(len=:), allocatable, intent(out) :: msg1, msg2
    real(wp), dimension(3), intent(out) :: d1, d2
    type(fparser) :: p
    integer :: i, status
    character(len=1), dimension(3), parameter :: vars = ['x', 'y', 'z']
    r1 = 0; r2 = 0; d1 = 0; d2 = 0
    call p%parse(vars, [expr], status=status, errmsg=msg1)
    if (status /= 0) then
        call test_failed(error, 'parse failed: '//msg1)
        return
    end if
    call p%set_variables(vals)
    do i = 1, 2
        call p%set_scalar_optimization(i == 1)
        if (wrt) then
            call p%evaluate(wrt=vars, status=status)
        else
            call p%evaluate(status=status)
        end if
        if (i == 1) then
            msg1 = p%error_message()
            if (status == 0) call p%get(1, r1)
            if (status == 0 .and. wrt) call get_grad(d1)
        else
            msg2 = p%error_message()
            if (status == 0) call p%get(1, r2)
            if (status == 0 .and. wrt) call get_grad(d2)
        end if
    end do
    contains
        subroutine get_grad(d)
        real(wp), dimension(3), intent(out) :: d
        integer :: k
        do k = 1, 3
            call p%get_derivative(1, vars(k), d(k))
        end do
        end subroutine get_grad
    end subroutine eval_both

    subroutine test_values(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: i
    real(wp) :: r1, r2
    real(wp), dimension(3) :: d1, d2
    character(len=:), allocatable :: m1, m2
    do i = 1, size(exprs)
        call eval_both(error, trim(exprs(i)), [0.3_wp, 1.7_wp, 2.2_wp], .false., r1, r2, m1, m2, d1, d2)
        if (allocated(error)) return
        if (m1 /= '' .or. m2 /= '') then
            call test_failed(error, 'evaluation failed for "'//trim(exprs(i))//'"', m1//' / '//m2)
            return
        end if
        if (.not. close_to(r1, r2, 1.0e-15_wp)) then
            call test_failed(error, 'different values for "'//trim(exprs(i))//'"')
            return
        end if
    end do
    end subroutine test_values

    subroutine test_errors_both(error)
    !! domain errors give the same message with both evaluators
    type(error_type), allocatable, intent(out) :: error
    character(len=*), dimension(*), parameter :: bad = [character(len=40) :: &
        '1/(x-x)', 'sqrt(-x)', 'log(x-x)', 'log10(-y)', 'asin(y)', 'acos(-y)', 'acosh(x)', &
        'atanh(y)', '(-x)^0.5', '(x-x)^-1', 'mod(y, x-x)', 'modulo(y, x-x)', 'atan2(x-x, x-x)', &
        'if(1/(x-x), 0, y > x)']
    integer :: i
    real(wp) :: r1, r2
    real(wp), dimension(3) :: d1, d2
    character(len=:), allocatable :: m1, m2
    do i = 1, size(bad)
        call eval_both(error, trim(bad(i)), [0.3_wp, 1.7_wp, 2.2_wp], .false., r1, r2, m1, m2, d1, d2)
        if (allocated(error)) return
        if (m1 == '' .or. m1 /= m2) then
            call test_failed(error, 'error messages differ for "'//trim(bad(i))//'"', '"'//m1//'" / "'//m2//'"')
            return
        end if
    end do
    end subroutine test_errors_both

    subroutine test_derivs_both(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: i
    real(wp) :: r1, r2
    real(wp), dimension(3) :: d1, d2
    character(len=:), allocatable :: m1, m2
    do i = 1, size(exprs)
        call eval_both(error, trim(exprs(i)), [0.3_wp, 1.7_wp, 2.2_wp], .true., r1, r2, m1, m2, d1, d2)
        if (allocated(error)) return
        if (m1 /= '' .or. m2 /= '') then
            call test_failed(error, 'evaluation failed for "'//trim(exprs(i))//'"', m1//' / '//m2)
            return
        end if
        if (.not. close_to(r1, r2, 1.0e-15_wp) .or. .not. all(abs(d1 - d2) <= 1.0e-14_wp * max(1.0_wp, abs(d1)))) then
            call test_failed(error, 'different values or derivatives for "'//trim(exprs(i))//'"')
            return
        end if
    end do
    end subroutine test_derivs_both

    subroutine test_mixed_sizes(error)
    !! a scalar expression depending on a vector output uses the general evaluator,
    !! and switching variables between scalar and vector works
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp) :: r
    real(wp), dimension(:), allocatable :: v
    call p%parse(['a', 'b'], str_list('c = a * 2', 'd = sum(c) + b'))
    call p%set_variable('a', [1.0_wp, 2.0_wp, 3.0_wp])
    call p%set_variable('b', 1.0_wp)
    call p%evaluate()
    call p%get('d', r)
    if (.not. close_to(r, 13.0_wp)) then
        call test_failed(error, 'wrong value (vector)')
        return
    end if
    call p%set_variable('a', 5.0_wp) ! now a scalar
    call p%evaluate()
    call p%get('d', r)
    if (.not. close_to(r, 11.0_wp)) then
        call test_failed(error, 'wrong value (scalar)')
        return
    end if
    call p%set_variable('a', [1.0_wp, 1.0_wp]) ! back to a vector
    call p%evaluate(wrt=['a'])
    call p%get('c', v)
    if (size(v) /= 2) then
        call test_failed(error, 'wrong size')
        return
    end if
    call p%get('d', r)
    if (.not. close_to(r, 5.0_wp) .or. p%failed()) call test_failed(error, 'wrong value (vector 2)')
    end subroutine test_mixed_sizes

    end module test_consistency
!*****************************************************************************************
