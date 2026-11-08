!*****************************************************************************************
!>
!  A large set of expression tests: odd syntax, precedence corner cases, numeric
!  edge cases, complicated expressions, long and deeply nested expressions, large
!  systems, and error cases. The results are compared with native Fortran, and
!  every scalar expression is evaluated with both evaluators (the scalar program
!  and the tree evaluator).

    module test_expressions

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    use test_utils
    use, intrinsic :: iso_fortran_env, only: int64

    implicit none

    private

    public :: collect_expressions

    ! the values of the variables `x`, `y`, `z` and `w`:
    real(wp), parameter :: xv = 0.7_wp
    real(wp), parameter :: yv = -1.3_wp
    real(wp), parameter :: zv = 2.5_wp
    real(wp), parameter :: wv = 4.0_wp
    real(wp), parameter :: pi = acos(-1.0_wp)

    type :: case_t
        !! an expression and its expected value
        character(len=12000) :: expr = '' !! the expression (of `x`, `y`, `z` and `w`)
        real(wp) :: expected = 0.0_wp !! the expected value
    end type case_t

    type :: err_case_t
        !! an expression that fails, and the expected error message
        character(len=12000) :: expr = '' !! the expression (of `x`, `y`, `z` and `w`)
        character(len=100) :: msg = '' !! text that the error message must contain
    end type err_case_t

    contains

    subroutine collect_expressions(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("precedence", test_precedence), &
        new_unittest("logical_relational", test_logical_relational), &
        new_unittest("number_formats", test_number_formats), &
        new_unittest("number_conversion", test_number_conversion), &
        new_unittest("formatting", test_formatting), &
        new_unittest("numeric_edge_cases", test_numeric_edge_cases), &
        new_unittest("identities", test_identities), &
        new_unittest("complicated", test_complicated), &
        new_unittest("conditionals", test_conditionals), &
        new_unittest("vector_to_scalar", test_vector_to_scalar), &
        new_unittest("vector_results", test_vector_results), &
        new_unittest("deep_nesting", test_deep_nesting), &
        new_unittest("long_expressions", test_long_expressions), &
        new_unittest("very_deep_trees", test_very_deep_trees), &
        new_unittest("big_vectors", test_big_vectors), &
        new_unittest("many_variables", test_many_variables), &
        new_unittest("large_systems", test_large_systems), &
        new_unittest("random_systems", test_random_systems), &
        new_unittest("names", test_names), &
        new_unittest("exact_derivatives", test_exact_derivatives), &
        new_unittest("fd_derivatives", test_fd_derivatives), &
        new_unittest("parse_errors", test_parse_errors), &
        new_unittest("eval_errors", test_eval_errors) ]
    end subroutine collect_expressions

    ! --- helpers ---

    subroutine check_cases(error, cases, tol)
    !! check a list of scalar expressions of `x`, `y`, `z` and `w`,
    !! with both evaluators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(case_t), dimension(:), intent(in) :: cases !! the expressions and their expected values
    real(wp), intent(in), optional :: tol !! the tolerance
    integer :: i
    do i = 1, size(cases)
        call check_one(error, trim(cases(i)%expr), cases(i)%expected, tol)
        if (allocated(error)) return
    end do
    end subroutine check_cases

    subroutine check_one(error, expr, expected, tol)
    !! check a scalar expression of `x`, `y`, `z` and `w`, with both evaluators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=*), intent(in) :: expr !! the expression
    real(wp), intent(in) :: expected !! the expected value
    real(wp), intent(in), optional :: tol !! the tolerance
    type(fparser) :: p
    integer :: imode, status
    character(len=:), allocatable :: errmsg
    real(wp) :: r
    character(len=30) :: buf1, buf2
    character(len=*), dimension(2), parameter :: mode = ['scalar program', 'tree evaluator']
    call p%parse(['x', 'y', 'z', 'w'], [expr], status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//short(expr)//'"', short(errmsg))
        return
    end if
    call p%set_variables([xv, yv, zv, wv])
    do imode = 1, 2
        call p%set_scalar_optimization(imode == 1)
        call p%evaluate(status=status, errmsg=errmsg)
        if (status /= 0) then
            call test_failed(error, 'evaluate failed for "'//short(expr)//'" ('//trim(mode(imode))//')', short(errmsg))
            return
        end if
        call p%get(1, r)
        if (.not. close_to(r, expected, tol)) then
            write(buf1, '(ES25.17)') r
            write(buf2, '(ES25.17)') expected
            call test_failed(error, 'wrong value for "'//short(expr)//'" ('//trim(mode(imode))//')', &
                             'got '//trim(adjustl(buf1))//', expected '//trim(adjustl(buf2)))
            return
        end if
    end do
    end subroutine check_one

    pure function short(str) result(s)
    !! a string, shortened for an error message
    character(len=*), intent(in) :: str !! the string
    character(len=:), allocatable :: s !! the first 100 characters of the string
    if (len(str) > 100) then
        s = str(1:100)//'...'
    else
        s = str
    end if
    end function short

    subroutine check_error_cases(error, cases)
    !! check that a list of expressions of `x`, `y`, `z` and `w` fail to evaluate,
    !! with both evaluators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(err_case_t), dimension(:), intent(in) :: cases !! the expressions and the expected messages
    type(fparser) :: p
    integer :: i, imode, status
    character(len=:), allocatable :: errmsg
    do i = 1, size(cases)
        call p%parse(['x', 'y', 'z', 'w'], [trim(cases(i)%expr)], status=status, errmsg=errmsg)
        if (status /= 0) then
            call test_failed(error, 'parse failed for "'//trim(cases(i)%expr)//'"', errmsg)
            return
        end if
        call p%set_variables([xv, yv, zv, wv])
        do imode = 1, 2
            call p%set_scalar_optimization(imode == 1)
            call p%evaluate(status=status, errmsg=errmsg)
            if (status == 0) then
                call test_failed(error, 'evaluate should have failed for "'//trim(cases(i)%expr)//'"')
                return
            end if
            if (index(errmsg, trim(cases(i)%msg)) == 0) then
                call test_failed(error, 'unexpected error message for "'//trim(cases(i)%expr)//'"', &
                                 'got "'//errmsg//'", expected it to contain "'//trim(cases(i)%msg)//'"')
                return
            end if
        end do
    end do
    end subroutine check_error_cases

    subroutine check_vector(error, expr, expected)
    !! check a vector expression of `x`, `y`, `z` and `w`
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=*), intent(in) :: expr !! the expression
    real(wp), dimension(:), intent(in) :: expected !! the expected value
    call check_vexpr(error, expr, expected, ['x', 'y', 'z', 'w'], &
                     [fparse_value([xv]), fparse_value([yv]), fparse_value([zv]), fparse_value([wv])])
    end subroutine check_vector

    pure function itoa(i) result(s)
    !! integer to string
    integer, intent(in) :: i !! the integer
    character(len=:), allocatable :: s !! the integer as a string
    character(len=20) :: buf
    write(buf, '(I0)') i
    s = trim(buf)
    end function itoa

    ! --- tests ---

    subroutine test_precedence(error)
    !! precedence and associativity corner cases
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_cases(error, [ &
        case_t('2^3^2', 512.0_wp), &                ! right associative
        case_t('(2^3)^2', 64.0_wp), &
        case_t('2**3**2', 512.0_wp), &
        case_t('-2^2', -4.0_wp), &                  ! as in Fortran: -(2^2)
        case_t('(-2)^2', 4.0_wp), &
        case_t('-x**2', -xv**2), &
        case_t('2^-1', 0.5_wp), &                   ! unary minus in an exponent
        case_t('2^-2^2', 2.0_wp**(-4)), &           ! 2^(-(2^2))
        case_t('2^+3', 8.0_wp), &
        case_t('x**-y', xv**(-yv)), &
        case_t('--x', xv), &
        case_t('+-+x', -xv), &
        case_t('- - - x', -xv), &
        case_t('2*-x', -2.0_wp*xv), &
        case_t('x/-y/-z', xv/(-yv)/(-zv)), &
        case_t('x-y-z', xv-yv-zv), &                ! left associative
        case_t('x/y/z', xv/yv/zv), &
        case_t('x/y*z', xv/yv*zv), &
        case_t('x-y+z-w', xv-yv+zv-wv), &
        case_t('2+3*4^2/8-1', 7.0_wp), &
        case_t('2*3^2', 18.0_wp), &
        case_t('-3^2*2', -18.0_wp), &
        case_t('w^z^x', wv**(zv**xv)), &
        case_t('-w^-z^-x', -(wv**(-(zv**(-xv))))), &
        case_t('(((x+y)*(z-w))/((x-y)*(z+w)))', ((xv+yv)*(zv-wv))/((xv-yv)*(zv+wv))) ])
    end subroutine test_precedence

    subroutine test_logical_relational(error)
    !! logical and relational operators, and their precedence
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_cases(error, [ &
        case_t('!0', 1.0_wp), &
        case_t('!!5', 1.0_wp), &
        case_t('!x > 0', 0.0_wp), &                 ! (!x) > 0
        case_t('!(x > 0)', 0.0_wp), &
        case_t('1 < 2 < 3', 1.0_wp), &              ! (1 < 2) < 3
        case_t('3 > 2 > 1', 0.0_wp), &              ! (3 > 2) > 1
        case_t('0.1 + 0.2 == 0.3', 0.0_wp), &       ! floating point
        case_t('0.5 + 0.25 == 0.75', 1.0_wp), &
        case_t('1 != 2', 1.0_wp), &
        case_t('1 /= 1', 0.0_wp), &
        case_t('0 || 0 && 1', 0.0_wp), &            ! && before ||
        case_t('1 || 0 && 0', 1.0_wp), &
        case_t('.not. 0 .and. 1', 1.0_wp), &
        case_t('.NOT.x.AND.y', 0.0_wp), &
        case_t('1.and.0', 0.0_wp), &
        case_t('1.or.0', 1.0_wp), &
        case_t('x .gt. y .and. z .ge. 2.5', 1.0_wp), &
        case_t('x .lt. y .or. z .le. 2.5', 1.0_wp), &
        case_t('x .eq. 0.7 .and. y .ne. 0', 1.0_wp), &
        case_t('x > y == 1', 1.0_wp), &             ! (x > y) == 1
        case_t('-x < -y', 1.0_wp), &
        case_t('!(x < y) && (z >= 2.5)', 1.0_wp), &
        case_t('(x > 0) + (y > 0) + (z > 0) + (w > 0)', 3.0_wp), &
        case_t('2 * (x > y) - 3 * (x < y)', 2.0_wp), &
        case_t('-1 && -2', 1.0_wp), &               ! any nonzero value is true
        case_t('0.0 || -0.0', 0.0_wp) ])
    end subroutine test_logical_relational

    subroutine test_number_formats(error)
    !! numeric literals
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    call check_cases(error, [ &
        case_t('1.e2', 100.0_wp), &
        case_t('1.5d2', 150.0_wp), &
        case_t('1D-2', 0.01_wp), &
        case_t('2.5E+1', 25.0_wp), &
        case_t('.5', 0.5_wp), &
        case_t('5.', 5.0_wp), &
        case_t('0001.5000', 1.5_wp), &
        case_t('1e0', 1.0_wp), &
        case_t('123456789012345678', 123456789012345678.0_wp), &
        case_t('0.000000000000000000001', 1.0e-21_wp), &
        case_t('1.7976931348623157e308', huge(1.0_wp)), &
        case_t('2.5e-1*4', 1.0_wp) ])
    if (allocated(error)) return
    ! subnormal (compared exactly, since check_cases uses an absolute tolerance near zero):
    call p%parse(['x'], ['1e-320'])
    call p%set_variable('x', 0.0_wp)
    call p%evaluate()
    call p%get(1, r)
    call check(error, r == 1.0e-320_wp, 'subnormal literal')
    end subroutine test_number_formats

    subroutine test_number_conversion(error)
    !! numeric literals of many forms must have exactly the same value as with `read`
    !! (the lexer has a fast path for numbers with few digits)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 3000
    type(fparser) :: p
    character(len=40), dimension(n) :: literals
    character(len=40) :: tmp
    real(wp) :: r, expected
    integer :: i, k, nint_digits, nfrac_digits, iexp, ios
    integer(int64) :: seed
    seed = 12345
    do i = 1, n
        ! integer digits, fraction digits, exponent (some with more than 15 digits):
        nint_digits = next(0, merge(20, 8, mod(i, 7) == 0))
        nfrac_digits = next(0, merge(20, 8, mod(i, 11) == 0))
        if (nint_digits + nfrac_digits == 0) nint_digits = 1
        literals(i) = ''
        do k = 1, nint_digits
            literals(i) = trim(literals(i))//achar(iachar('0') + next(0, 9))
        end do
        if (nfrac_digits > 0 .or. next(0, 3) == 0) literals(i) = trim(literals(i))//'.'
        do k = 1, nfrac_digits
            literals(i) = trim(literals(i))//achar(iachar('0') + next(0, 9))
        end do
        if (next(0, 2) == 0) then
            iexp = next(-30, 30)
            select case (next(0, 3))
            case (0); literals(i) = trim(literals(i))//'e'//itoa(iexp)
            case (1); literals(i) = trim(literals(i))//'D'//itoa(iexp)
            case (2); literals(i) = trim(literals(i))//'E+'//itoa(abs(iexp))
            case default; literals(i) = trim(literals(i))//'d-'//itoa(abs(iexp))
            end select
        end if
    end do
    call p%parse(['x'], literals)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable('x', 0.0_wp)
    call p%evaluate()
    do i = 1, n
        call p%get(i, r)
        tmp = literals(i)
        do k = 1, len_trim(tmp)
            if (tmp(k:k) == 'd' .or. tmp(k:k) == 'D') tmp(k:k) = 'e'
        end do
        read(tmp, *, iostat=ios) expected
        call check(error, ios == 0 .and. r == expected, 'wrong value for "'//trim(literals(i))//'"')
        if (allocated(error)) return
    end do
    contains
        integer function next(lo, hi)
        !! a pseudo-random integer in `[lo, hi]` (a linear congruential generator)
        integer, intent(in) :: lo !! the lower bound
        integer, intent(in) :: hi !! the upper bound
        seed = mod(1103515245_int64 * seed + 12345_int64, 2147483648_int64)
        next = lo + int(mod(seed / 65536_int64, int(hi - lo + 1, int64)))
        end function next
    end subroutine test_number_conversion

    subroutine test_formatting(error)
    !! whitespace and case
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=1), parameter :: tab = achar(9)
    call check_cases(error, [ &
        case_t('x'//tab//'+'//tab//'y', xv+yv), &
        case_t('   x   ', xv), &
        case_t('SIN( X )', sin(xv)), &
        case_t('sIn(x)', sin(xv)), &
        case_t('sin (x)', sin(xv)), &
        case_t('Max ( X , Y , Z )', zv), &
        case_t('PI + Pi + pI', 3*pi), &
        case_t('x**2', xv**2), &
        case_t('x ** 2', xv**2), &
        case_t('x*y', xv*yv), &
        case_t('[ x , y ] [ 2 ]', yv) ])
    end subroutine test_formatting

    subroutine test_numeric_edge_cases(error)
    !! special values of the functions and operators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    call check_cases(error, [ &
        case_t('0^0', 1.0_wp), &
        case_t('0^2', 0.0_wp), &
        case_t('(-2)^3', -8.0_wp), &                ! negative base, integer exponent
        case_t('(-2)^2.0', 4.0_wp), &
        case_t('(-2)^-2', 0.25_wp), &
        case_t('x^0.5', sqrt(xv)), &
        case_t('1^1e10', 1.0_wp), &
        case_t('atan2(0, -1)', pi), &
        case_t('atan2(-0.0, -1)', -pi), &
        case_t('hypot(3e200, 4e200)', 5.0e200_wp), &  ! no overflow
        case_t('hypot(3e-200, 4e-200)', 5.0e-200_wp), &
        case_t('mod(-7.5, 2)', -1.5_wp), &
        case_t('modulo(-7.5, 2)', 0.5_wp), &
        case_t('mod(7.5, -2)', 1.5_wp), &
        case_t('modulo(7.5, -2)', -0.5_wp), &
        case_t('sign(3, y)', -3.0_wp), &
        case_t('sign(-3, x)', 3.0_wp), &
        case_t('floor(-2.5) + ceil(-2.5) + nint(2.5) + aint(-2.7)', -4.0_wp), &
        case_t('round(-2.5) + trunc(2.7) + anint(-0.5)', -2.0_wp), &
        case_t('floor(-0.0) + ceiling(0.0)', 0.0_wp), &
        case_t('abs(-0.0)', 0.0_wp), &
        case_t('exp(-1000)', 0.0_wp), &
        case_t('tan(pi/4)', 1.0_wp), &
        case_t('max(1,2,3,4,5,6,7,8,9,0)', 9.0_wp), &
        case_t('min(1,2,3,4,5,6,7,8,9,0)', 0.0_wp), &
        case_t('max(-0.0, 0.0) + min(-1e-300, 1e-300)', -1.0e-300_wp) ], tol=1.0e-15_wp)
    if (allocated(error)) return
    ! signed zeros:
    call p%parse(['x'], str_list('-0.0', 'sqrt(-0.0)', 'sign(1, -0.0)', '0.0 * -1'))
    call p%set_variable('x', 0.0_wp)
    call p%evaluate()
    call p%get(1, r); call check(error, r == 0.0_wp .and. sign(1.0_wp, r) < 0, '-0.0'); if (allocated(error)) return
    call p%get(2, r); call check(error, r == 0.0_wp .and. sign(1.0_wp, r) < 0, 'sqrt(-0.0)'); if (allocated(error)) return
    call p%get(3, r); call check(error, r, -1.0_wp); if (allocated(error)) return
    call p%get(4, r); call check(error, r == 0.0_wp .and. sign(1.0_wp, r) < 0, '0.0 * -1'); if (allocated(error)) return
    end subroutine test_numeric_edge_cases

    subroutine test_identities(error)
    !! mathematical identities
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_cases(error, [ &
        case_t('sin(x)^2 + cos(x)^2', 1.0_wp), &
        case_t('cosh(y)^2 - sinh(y)^2', 1.0_wp), &
        case_t('tan(x) - sin(x)/cos(x)', 0.0_wp), &
        case_t('tanh(y) - sinh(y)/cosh(y)', 0.0_wp), &
        case_t('erf(x) + erfc(x)', 1.0_wp), &
        case_t('erf(-x) + erf(x)', 0.0_wp), &
        case_t('asin(sin(x)) + acos(cos(x)) + atan(tan(x))', 3*xv), &
        case_t('asinh(sinh(z)) + acosh(cosh(z)) + atanh(tanh(x))', 2*zv + xv), &
        case_t('exp(log(z))', zv), &
        case_t('ln(exp(y))', yv), &
        case_t('10^log10(w)', wv), &
        case_t('sqrt(z)^2', zv), &
        case_t('exp(x + y) - exp(x)*exp(y)', 0.0_wp), &
        case_t('log(z*w) - log(z) - log(w)', 0.0_wp), &
        case_t('sin(2*x) - 2*sin(x)*cos(x)', 0.0_wp), &
        case_t('atan2(y, x) - atan(y/x)', 0.0_wp), &  ! x > 0
        case_t('hypot(x, y) - sqrt(x^2 + y^2)', 0.0_wp), &
        case_t('mod(w*z, z)', 0.0_wp), &
        case_t('sin(pi/6) + cos(pi/3)', 1.0_wp), &
        case_t('cos(pi)', -1.0_wp) ], tol=1.0e-14_wp)
    end subroutine test_identities

    subroutine test_complicated(error)
    !! complicated expressions, compared with native Fortran
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp), parameter :: s = 0.3_wp
    call check_cases(error, [ &
        case_t('sqrt(x^2 + y^2 + z^2) * exp(-x*y) / (1 + abs(z))', &
               sqrt(xv**2 + yv**2 + zv**2) * exp(-xv*yv) / (1 + abs(zv))), &
        case_t('atan2(y, x) + hypot(x, y) - log10(z) * log(z)', &
               atan2(yv, xv) + hypot(xv, yv) - log10(zv) * log(zv)), &
        case_t('max(x, y, z, -1, 3) - min(x, y, z)', 3.0_wp - yv), &
        case_t('((((3*x - 2)*x + 1)*x - 5)*x + 7)', ((((3*xv - 2)*xv + 1)*xv - 5)*xv + 7)), &
        case_t('(x^3 - 2*x + 1)/(x^2 + y^2 + 1)', (xv**3 - 2*xv + 1)/(xv**2 + yv**2 + 1)), &
        case_t('exp(-((x - 0.5)^2 + (y + 1)^2) / (2*0.3^2)) / (2*pi*0.3^2)', &
               exp(-((xv - 0.5_wp)**2 + (yv + 1)**2) / (2*s**2)) / (2*pi*s**2)), &
        case_t('100*(y - x^2)^2 + (1 - x)^2', 100*(yv - xv**2)**2 + (1 - xv)**2), &
        case_t('z - x*sin(z) - w', zv - xv*sin(zv) - wv), &
        case_t('sqrt(abs(y)) * cos(w*pi/180) + sin(z)^3 / (1 + x^4)', &
               sqrt(abs(yv)) * cos(wv*pi/180) + sin(zv)**3 / (1 + xv**4)), &
        case_t('-20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20', &
               -20*exp(-0.2_wp*sqrt(0.5_wp*(xv**2 + yv**2))) - exp(0.5_wp*(cos(2*pi*xv) + cos(2*pi*yv))) &
               + exp(1.0_wp) + 20), &                     ! Ackley function
        case_t('(x + y)^2 * (z - w)^3 / ((x*y*z*w)^2 + 1)', (xv + yv)**2 * (zv - wv)**3 / ((xv*yv*zv*wv)**2 + 1)), &
        case_t('sinh(x)*cosh(y)*tanh(z) + asinh(w)*atan(x)*erf(y)', &
               sinh(xv)*cosh(yv)*tanh(zv) + asinh(wv)*atan(xv)*erf(yv)), &
        case_t('sqrt(sqrt(sqrt(sqrt(w))))', sqrt(sqrt(sqrt(sqrt(wv))))), &
        case_t('exp(sin(exp(cos(exp(x)))))', exp(sin(exp(cos(exp(xv)))))), &
        case_t('1/(1 + 1/(1 + 1/(1 + 1/(1 + 1/(1 + x)))))', 1/(1 + 1/(1 + 1/(1 + 1/(1 + 1/(1 + xv)))))), &
        case_t('mod(floor(w*z*10), 7) + modulo(-nint(z*w), 3) + sign(abs(y), -x)', &
               mod(floor(wv*zv*10), 7) + modulo(-nint(zv*wv), 3) + sign(abs(yv), -xv)), &
        case_t('max(min(x, y), min(z, w), -max(x, -y))', max(min(xv, yv), min(zv, wv), -max(xv, -yv))), &
        case_t('z^x^-w', zv**(xv**(-wv))), &
        case_t('w^(x*y) + z^(-x) + x^(z/w)', wv**(xv*yv) + zv**(-xv) + xv**(zv/wv)) ], tol=1.0e-14_wp)
    end subroutine test_complicated

    subroutine test_conditionals(error)
    !! conditionals: nesting, laziness, and with relational and logical operators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_cases(error, [ &
        case_t('if(sin(x), cos(x), x > y) * if(1, 2, 0)', 2*sin(xv)), &
        case_t('if(if(1, 2, x < y), if(3, 4, z > w), x > 0)', 2.0_wp), &
        case_t('if(1, if(2, if(3, 4, 0), 0), 0)', 4.0_wp), &
        case_t('if(x, y, z) + if(x, y, 0)', xv + yv), &
        case_t('if(1, 2, x > y && z < w || 0)', 1.0_wp), &
        case_t('if(-x, x, x < 0) + if(-y, y, y < 0)', xv - yv), &       ! abs
        case_t('if(1/0, 2, 0)', 2.0_wp), &                  ! only the selected branch is evaluated
        case_t('if(2, 1/0, 1)', 2.0_wp), &
        case_t('if(sqrt(y), log(-y), y > 0)', log(-yv)), &
        case_t('merge(x, y, z)', xv), &
        case_t('2*if(x, y, 1) + 3', 2*xv + 3), &
        case_t('if(x^2, y^2, x > y)^0.5', abs(xv)), &
        ! nested, with errors in the branches that are skipped:
        case_t('if(if(1/0, 2, 0), 3, if(5, 1/0, x > y))', 2.0_wp), &
        case_t('if(1/0, if(log(y), 2, 0), if(x, y, z) < 0)', 2.0_wp), &
        case_t('sum(if(if(1, 1/0, 1), [2, 3], [1, 0]))', 4.0_wp), &
        case_t('sum(if(if(z, 1/0, w > 0) * [1, 2], 0, [x, y] > 0))', zv), &
        case_t('if(if(x, y, 0), if(0, 1/0, 1), if(1, 0, 1))', yv) ], tol=1.0e-14_wp)
    end subroutine test_conditionals

    subroutine test_vector_to_scalar(error)
    !! vectors with scalar results
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_cases(error, [ &
        case_t('[1,2,3][2]', 2.0_wp), &
        case_t('[[1,2],[3]][3]', 3.0_wp), &               ! constructors are concatenated
        case_t('[1,2,3][3][1]', 3.0_wp), &
        case_t('[1][1][1]', 1.0_wp), &
        case_t('x[1]', xv), &                              ! a scalar is a vector of size 1
        case_t('x[1][1]', xv), &
        case_t('[10,20,30][1+1]', 20.0_wp), &
        case_t('[x,y,z][[3,1][2]]', xv), &
        case_t('[x,y,z,w][size([x,y,z,w])]', wv), &
        case_t('size([x,y,z,1])', 4.0_wp), &
        case_t('size(1)', 1.0_wp), &
        case_t('sum(5)', 5.0_wp), &
        case_t('norm2(-3)', 3.0_wp), &
        case_t('norm2([3,4])', 5.0_wp), &
        case_t('dot([1,2,3],[4,5,6])', 32.0_wp), &
        case_t('dot_product([1,2],[3,4])', 11.0_wp), &
        case_t('cross([1,0,0],[0,1,0])[3]', 1.0_wp), &
        case_t('product([1,2,3,4])', 24.0_wp), &
        case_t('maxval([x,y,z])', zv), &
        case_t('minval([x,y,z])', yv), &
        case_t('sum(sin([x,y,z]))', sin(xv) + sin(yv) + sin(zv)), &
        case_t('sum([1,2,3]*[4,5,6])', 32.0_wp), &
        case_t('sum(max(1,[2,0,3]))', 6.0_wp), &
        case_t('sum(if([1,2],[3,4],[1,0]))', 5.0_wp), &
        case_t('sum([x,y,z] > 0)', 2.0_wp), &
        case_t('dot([x,y,z], [x,y,z]) - norm2([x,y,z])^2', 0.0_wp), &
        case_t('norm2(cross([x,y,z],[w,1,2]))', &
               norm2([yv*2 - zv*1, zv*wv - xv*2, xv*1 - yv*wv])), &
        case_t('dot(cross([x,y,z],[w,1,2]), [x,y,z])', 0.0_wp), &   ! perpendicular
        case_t('sum([x,y]^2) + product([z,w] - 1)', xv**2 + yv**2 + (zv - 1)*(wv - 1)), &
        case_t('maxval(abs([x,y,z,w] - 2))', 3.3_wp) ], tol=1.0e-14_wp)
    end subroutine test_vector_to_scalar

    subroutine test_vector_results(error)
    !! vector results
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_vector(error, '[[1,2],[3]]', [1.0_wp, 2.0_wp, 3.0_wp]); if (allocated(error)) return
    call check_vector(error, '[x, [y, z], [[w]]]', [xv, yv, zv, wv]); if (allocated(error)) return
    call check_vector(error, 'max(1,[2,0,3])', [2.0_wp, 1.0_wp, 3.0_wp]); if (allocated(error)) return
    call check_vector(error, 'min([1,5],[3,2],4)', [1.0_wp, 2.0_wp]); if (allocated(error)) return
    call check_vector(error, 'if(1,2,[1,0,1])', [1.0_wp, 2.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_vector(error, 'if([1,2],[3,4],[1,0])', [1.0_wp, 4.0_wp]); if (allocated(error)) return
    call check_vector(error, '[x,y]*2 + 1', [2*xv + 1, 2*yv + 1]); if (allocated(error)) return
    call check_vector(error, 'cross([1,2,3],[4,5,6])', [-3.0_wp, 6.0_wp, -3.0_wp]); if (allocated(error)) return
    call check_vector(error, 'sin([0, pi/2])', [0.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_vector(error, '-[1,2]^2', [-1.0_wp, -4.0_wp]); if (allocated(error)) return
    call check_vector(error, '2^[1,2,3]', [2.0_wp, 4.0_wp, 8.0_wp]); if (allocated(error)) return
    call check_vector(error, '[1,2,3] > 2', [0.0_wp, 0.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_vector(error, '![1,0]', [0.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_vector(error, '[1,0] && [1,1] || [0,0]', [1.0_wp, 0.0_wp]); if (allocated(error)) return
    call check_vector(error, 'atan2([1,-1], [-1,-1])', [atan2(1.0_wp, -1.0_wp), atan2(-1.0_wp, -1.0_wp)])
    if (allocated(error)) return
    call check_vector(error, 'mod([7,-7], [3,3]) + modulo([7,-7], 3)', [2.0_wp, 1.0_wp]); if (allocated(error)) return
    call check_vector(error, 'cross([x,y,z], cross([x,y,z],[1,0,0]))', &
                      cross3([xv, yv, zv], cross3([xv, yv, zv], [1.0_wp, 0.0_wp, 0.0_wp])))
    if (allocated(error)) return
    call check_vector(error, '[x,y,z] / norm2([x,y,z])', [xv, yv, zv] / norm2([xv, yv, zv]))
    contains
        pure function cross3(a, b) result(c)
        !! cross product
        real(wp), dimension(3), intent(in) :: a !! the first vector
        real(wp), dimension(3), intent(in) :: b !! the second vector
        real(wp), dimension(3) :: c !! `a x b`
        c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]
        end function cross3
    end subroutine test_vector_results

    subroutine test_deep_nesting(error)
    !! deeply nested expressions (within the recursion limit of the default stack)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 500
    real(wp) :: expected
    integer :: i
    call check_cases(error, [case_t(repeat('(', n)//'x'//repeat(')', n), xv)])
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('-', n+1)//'x', -xv)])
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('!', n)//'x', 1.0_wp)])
    if (allocated(error)) return
    expected = xv
    do i = 1, 200
        expected = sin(expected)
    end do
    call check_cases(error, [case_t(repeat('sin(', 200)//'x'//repeat(')', 200), expected)], tol=1.0e-14_wp)
    if (allocated(error)) return
    expected = zv
    do i = 1, 100
        expected = 1.0_wp / (1.0_wp + expected)
    end do
    call check_cases(error, [case_t(repeat('1/(1 + ', 100)//'z'//repeat(')', 100), expected)], tol=1.0e-14_wp)
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('if(x, 0, ', 100)//'1'//repeat(')', 100), xv)])
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('[', 100)//'x'//repeat(']', 100)//repeat('[1]', 100), xv)])
    end subroutine test_deep_nesting

    subroutine test_long_expressions(error)
    !! expressions with many terms
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 5000
    character(len=:), allocatable :: e
    integer :: i
    real(wp) :: expected
    call check_cases(error, [case_t(repeat('x+', n-1)//'x', n*xv)], tol=1.0e-12_wp)
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('x-', n)//'x', xv - n*xv)], tol=1.0e-12_wp)
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('1.001*', 999)//'1.001', 1.001_wp**1000)], tol=1.0e-12_wp)
    if (allocated(error)) return
    call check_cases(error, [case_t(repeat('x/', 50)//'1', xv**(-48))], tol=1.0e-12_wp)  ! ((x/x)/x)/.../1 = x^(1-49)
    if (allocated(error)) return
    ! a polynomial with many terms: sum(i * x^i)
    e = '0'
    expected = 0.0_wp
    do i = 1, 200
        e = e//' + '//itoa(i)//'*x^'//itoa(i)
        expected = expected + i * xv**i
    end do
    call check_cases(error, [case_t(e, expected)], tol=1.0e-13_wp)
    if (allocated(error)) return
    ! many function calls:
    e = 'max(x'
    do i = 1, 300
        e = e//', sin('//itoa(i)//'*y)'
    end do
    e = e//')'
    expected = xv
    do i = 1, 300
        expected = max(expected, sin(i*yv))
    end do
    call check_cases(error, [case_t(e, expected)], tol=1.0e-14_wp)
    end subroutine test_long_expressions

    subroutine test_very_deep_trees(error)
    !! expressions with very deep trees (a long sum is a left-leaning tree with one level
    !! per term), which would overflow the stack if they were evaluated recursively
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 200000
    ! (the tolerance allows for the rounding errors of the long sums)
    call check_one(error, repeat('x-', n)//'x', xv - n*xv, tol=1.0e-10_wp)
    if (allocated(error)) return
    call check_exact_derivative(error, repeat('x-', n)//'x', 'x', 1.0_wp - n)
    if (allocated(error)) return
    call check_one(error, repeat('(z/z)*', n/2)//'z', zv, tol=1.0e-10_wp)
    if (allocated(error)) return
    call check_exact_derivative(error, repeat('(z/z)*', n/2)//'z', 'z', 1.0_wp)
    if (allocated(error)) return
    call check_one(error, repeat('if(x, 1/0, x > y) + ', n/4)//'0', (n/4)*xv, tol=1.0e-10_wp)
    if (allocated(error)) return
    call check_one(error, 'sum('//repeat('[x, y] + ', n/4)//'0)', (n/4)*(xv + yv), tol=1.0e-10_wp)
    end subroutine test_very_deep_trees

    subroutine test_big_vectors(error)
    !! large vector constructors and variables
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: e
    real(wp), dimension(:), allocatable :: v, r
    integer :: i
    integer, parameter :: n = 1000
    e = '[1'
    do i = 2, n
        e = e//','//itoa(i)
    end do
    e = e//']'
    call check_cases(error, [case_t('sum('//e//')', real(n*(n+1)/2, wp)), &
                             case_t(e//'['//itoa(n)//']', real(n, wp)), &
                             case_t('size('//e//')', real(n, wp))])
    if (allocated(error)) return
    v = [(real(i, wp) / n, i = 1, 100000)]
    call p%parse(['v'], str_list('s = sum(sin(v)^2 + cos(v)^2)', 'm = maxval(v*(1 - v))', 'u = v[size(v)]'))
    call p%set_variable('v', v)
    call p%evaluate()
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get('s', r); call check(error, abs(r(1) - size(v)) < 1.0e-8_wp, 's'); if (allocated(error)) return
    call p%get('m', r); call check(error, r(1), maxval(v*(1 - v))); if (allocated(error)) return
    call p%get('u', r); call check(error, r(1), v(size(v))); if (allocated(error)) return
    end subroutine test_big_vectors

    subroutine test_many_variables(error)
    !! an expression of many variables
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 300
    type(fparser) :: p
    character(len=10), dimension(n) :: vars
    character(len=:), allocatable :: e
    real(wp), dimension(n) :: vals
    real(wp) :: r
    integer :: i
    e = '0'
    do i = 1, n
        vars(i) = 'v'//itoa(i)
        vals(i) = real(i, wp)
        e = e//' + '//trim(vars(i))//'^2'
    end do
    call p%parse(vars, [e])
    call p%set_variables(vals)
    call p%evaluate()
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get(1, r)
    call check(error, r, sum(vals**2)); if (allocated(error)) return
    end subroutine test_many_variables

    subroutine test_large_systems(error)
    !! systems of many expressions
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 500
    type(fparser) :: p
    character(len=40), dimension(n) :: exprs
    character(len=:), allocatable :: e
    real(wp) :: r
    integer :: i
    integer, dimension(:), allocatable :: order

    ! a chain, given in reverse order: e1 = x, e(i) = e(i-1) + i
    do i = 1, n
        if (i == 1) then
            exprs(n) = 'e1 = x'
        else
            exprs(n-i+1) = 'e'//itoa(i)//' = e'//itoa(i-1)//' + '//itoa(i)
        end if
    end do
    call p%parse(['x'], exprs)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    order = p%evaluation_order()
    call check(error, all(order == [(n-i+1, i = 1, n)]), 'evaluation order'); if (allocated(error)) return
    call p%set_variable('x', xv)
    call p%evaluate()
    call p%get('e'//itoa(n), r)
    call check(error, r, xv + real(n*(n+1)/2 - 1, wp)); if (allocated(error)) return

    ! fan out and in: f(i) = sin(i*x), and s = sum of all of them
    e = 's = 0'
    do i = 1, n - 1
        exprs(i) = 'f'//itoa(i)//' = sin('//itoa(i)//'*x)'
        e = e//' + f'//itoa(i)
    end do
    call p%parse(['x'], [character(len=max(len(e), 40)) :: e, exprs(1:n-1)])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable('x', xv)
    call p%evaluate()
    call p%get('s', r)
    call check(error, abs(r - sum([(sin(i*xv), i = 1, n - 1)])) < 1.0e-12_wp, 'sum of the outputs'); if (allocated(error)) return
    end subroutine test_large_systems

    subroutine test_random_systems(error)
    !! the evaluation order of random systems of expressions (listed in a random
    !! order) must be the one given by repeatedly sweeping through the expressions
    !! in index order, adding each one whose dependencies have all been added
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    integer, parameter :: n = 60 !! number of expressions in each system
    type(fparser) :: p
    character(len=100), dimension(n) :: exprs
    integer, dimension(n) :: perm, expected
    integer, dimension(:), allocatable :: order, inputs, outputs
    logical, dimension(n) :: done
    integer :: isys, i, j, k, m, tmp, ne
    integer(int64) :: seed
    seed = 271828
    do isys = 1, 20
        ! a random permutation (the position of each expression in the list):
        perm = [(i, i = 1, n)]
        do i = n, 2, -1
            j = next(1, i)
            tmp = perm(i); perm(i) = perm(j); perm(j) = tmp
        end do
        ! e(k) depends on up to 3 random expressions e(j) with j < k (so there are no cycles):
        do k = 1, n
            exprs(perm(k)) = 'e'//itoa(k)//' = x'
            if (k > 1) then
                do m = 1, next(0, 3)
                    exprs(perm(k)) = trim(exprs(perm(k)))//' + e'//itoa(next(1, k-1))
                end do
            end if
        end do
        call p%parse(['x'], exprs)
        call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
        ! the expected order (sweeps):
        done = .false.
        ne = 0
        do while (ne < n)
            do i = 1, n
                if (done(i)) cycle
                call p%get_dependencies(i, inputs, outputs)
                if (all(done(outputs))) then
                    done(i) = .true.
                    ne = ne + 1
                    expected(ne) = i
                end if
            end do
        end do
        order = p%evaluation_order()
        call check(error, all(order == expected), 'evaluation order of system '//itoa(isys))
        if (allocated(error)) return
    end do
    contains
        integer function next(lo, hi)
        !! a pseudo-random integer in `[lo, hi]` (a linear congruential generator)
        integer, intent(in) :: lo !! the lower bound
        integer, intent(in) :: hi !! the upper bound
        seed = mod(1103515245_int64 * seed + 12345_int64, 2147483648_int64)
        next = lo + int(mod(seed / 65536_int64, int(hi - lo + 1, int64)))
        end function next
    end subroutine test_random_systems

    subroutine test_names(error)
    !! unusual variable names
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=100) :: long_name
    real(wp) :: r
    long_name = 'a'//repeat('b_1', 33)
    call p%parse([character(len=100) :: 'x_1', 'X2y', 'A_B_C', 'sin_', 'pi2', 'e', long_name, 'if_'], &
                 ['x_1 + x2y*a_b_c - SIN_ + pi2 + E + '//trim(long_name)//' + IF_ + sin(pi)'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variables([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp])
    call p%evaluate()
    call p%get(1, r)
    call check(error, abs(r - (1 + 2*3 - 4 + 5 + 6 + 7 + 8)) < 1.0e-14_wp, 'value'); if (allocated(error)) return
    ! a variable can have the name of a function (the function is still available):
    call p%parse(['sin', 'max'], ['sin(sin) + max(max, 1)'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variables([0.5_wp, -2.0_wp])
    call p%evaluate()
    call p%get(1, r)
    call check(error, r, sin(0.5_wp) + 1.0_wp)
    end subroutine test_names

    subroutine check_exact_derivative(error, expr, wrt, expected)
    !! check the derivative of a scalar expression of `x`, `y`, `z` and `w`
    !! with respect to one of them, with both evaluators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=*), intent(in) :: expr !! the expression
    character(len=*), intent(in) :: wrt !! the variable
    real(wp), intent(in) :: expected !! the expected derivative
    type(fparser) :: p
    integer :: imode, status
    character(len=:), allocatable :: errmsg
    real(wp) :: d
    character(len=30) :: buf1, buf2
    call p%parse(['x', 'y', 'z', 'w'], [expr], status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//short(expr)//'"', short(errmsg))
        return
    end if
    call p%set_variables([xv, yv, zv, wv])
    do imode = 1, 2
        call p%set_scalar_optimization(imode == 1)
        call p%evaluate(wrt=[wrt], status=status, errmsg=errmsg)
        if (status /= 0) then
            call test_failed(error, 'evaluate failed for "'//short(expr)//'"', short(errmsg))
            return
        end if
        call p%get_derivative(1, wrt, d)
        if (.not. close_to(d, expected, 1.0e-13_wp)) then
            write(buf1, '(ES25.17)') d
            write(buf2, '(ES25.17)') expected
            call test_failed(error, 'wrong d/d'//wrt//' of "'//short(expr)//'"', &
                             'got '//trim(adjustl(buf1))//', expected '//trim(adjustl(buf2)))
            return
        end if
    end do
    end subroutine check_exact_derivative

    subroutine test_exact_derivatives(error)
    !! derivatives of complicated expressions, compared with the analytic derivatives
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    real(wp) :: r, g
    ! Rosenbrock:
    call check_exact_derivative(error, '100*(y - x^2)^2 + (1 - x)^2', 'x', &
                                -400*xv*(yv - xv**2) - 2*(1 - xv)); if (allocated(error)) return
    call check_exact_derivative(error, '100*(y - x^2)^2 + (1 - x)^2', 'y', 200*(yv - xv**2)); if (allocated(error)) return
    ! chain rule through nested functions:
    call check_exact_derivative(error, 'exp(sin(x^2))', 'x', exp(sin(xv**2))*cos(xv**2)*2*xv); if (allocated(error)) return
    call check_exact_derivative(error, 'log(cosh(z*w))', 'w', tanh(zv*wv)*zv); if (allocated(error)) return
    call check_exact_derivative(error, 'atan2(y, x)', 'x', -yv/(xv**2 + yv**2)); if (allocated(error)) return
    call check_exact_derivative(error, 'atan2(y, x)', 'y', xv/(xv**2 + yv**2)); if (allocated(error)) return
    call check_exact_derivative(error, 'hypot(x, y)', 'y', yv/hypot(xv, yv)); if (allocated(error)) return
    ! variable exponent: d/dz (x^z) = x^z * log(x)
    call check_exact_derivative(error, 'x^z', 'z', xv**zv*log(xv)); if (allocated(error)) return
    call check_exact_derivative(error, 'z^z', 'z', zv**zv*(log(zv) + 1)); if (allocated(error)) return
    ! quotient:
    call check_exact_derivative(error, '(x^3 - 2*x + 1)/(x^2 + y^2 + 1)', 'x', &
        ((3*xv**2 - 2)*(xv**2 + yv**2 + 1) - (xv**3 - 2*xv + 1)*2*xv)/(xv**2 + yv**2 + 1)**2)
    if (allocated(error)) return
    ! inverse functions:
    call check_exact_derivative(error, 'asin(x) + acos(x/2) + atan(x*3)', 'x', &
        1/sqrt(1 - xv**2) - 0.5_wp/sqrt(1 - (xv/2)**2) + 3/(1 + 9*xv**2)); if (allocated(error)) return
    call check_exact_derivative(error, 'asinh(y) + acosh(z) + atanh(x)', 'z', 1/sqrt(zv**2 - 1)); if (allocated(error)) return
    call check_exact_derivative(error, 'erf(x) + erfc(y)', 'x', 2/sqrt(pi)*exp(-xv**2)); if (allocated(error)) return
    ! conditionals and max/min (away from the switching points):
    call check_exact_derivative(error, 'if(x^2, sin(x), x > y)', 'x', 2*xv); if (allocated(error)) return
    call check_exact_derivative(error, 'max(x*z, y, w)', 'z', 0.0_wp); if (allocated(error)) return
    call check_exact_derivative(error, 'max(x*w, y, z)', 'x', wv); if (allocated(error)) return
    call check_exact_derivative(error, 'abs(y)*x', 'y', -xv); if (allocated(error)) return
    ! vectors with scalar results:
    call check_exact_derivative(error, 'norm2([x, y, z])', 'y', yv/norm2([xv, yv, zv])); if (allocated(error)) return
    call check_exact_derivative(error, 'dot([x, y, z], [w, x, 1])', 'x', wv + yv); if (allocated(error)) return
    call check_exact_derivative(error, 'cross([x, y, z], [w, 1, 2])[3]', 'y', -wv); if (allocated(error)) return
    call check_exact_derivative(error, 'product([x, y, z])', 'z', xv*yv); if (allocated(error)) return
    ! a long chain: d/dx of sin(sin(...sin(x)))
    r = xv
    g = 1.0_wp
    block
        integer :: i
        do i = 1, 50
            g = g * cos(r)
            r = sin(r)
        end do
    end block
    call check_exact_derivative(error, repeat('sin(', 50)//'x'//repeat(')', 50), 'x', g)
    end subroutine test_exact_derivatives

    subroutine test_fd_derivatives(error)
    !! derivatives of complicated systems, compared with finite differences
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_derivs_fd(error, str_list( &
        'a = sqrt(x^2 + y^2 + z^2) * exp(-x*y) / (1 + abs(z))', &
        'b = a*sin(a) + atan2(y, x)', &
        'c = -20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20', &
        'd = if(b, c, a > 1) + hypot(a, b)^z', &
        'e = max(a, b, c) * min(x, y)', &
        'f = 1/(1 + 1/(1 + 1/(1 + x*y*z)))'), &
        ['x', 'y', 'z'], [fparse_value([xv]), fparse_value([yv]), fparse_value([zv])], tol=1.0e-6_wp)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list( &
        'n = norm2(r)', &
        'h = cross(r, v)', &
        'e = cross(v, h)/mu - r/n', &
        'k = [dot(e, e), sum(h^2), product(r)]', &
        'q = if(r*v, r/v, [1, 0, 1]) * n'), &
        ['r ', 'v ', 'mu'], [fparse_value([1.2_wp, -0.4_wp, 0.9_wp]), fparse_value([0.3_wp, 1.1_wp, -0.2_wp]), &
                            fparse_value([1.5_wp])], tol=1.0e-6_wp)
    end subroutine test_fd_derivatives

    subroutine test_parse_errors(error)
    !! invalid expressions
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(err_case_t), dimension(:), allocatable :: cases
    integer :: i
    cases = [ &
        err_case_t('', 'empty expression'), &
        err_case_t('   ', 'empty expression'), &
        err_case_t('()', 'unexpected ")"'), &
        err_case_t('(x', 'expected ")"'), &
        err_case_t('((x)', 'expected ")"'), &
        err_case_t('x)', 'unexpected ")"'), &
        err_case_t('x +', 'unexpected end of expression'), &
        err_case_t('* x', 'unexpected "*"'), &
        err_case_t('x y', 'unexpected "y"'), &
        err_case_t('2x', 'unexpected "x"'), &
        err_case_t('1 2', 'unexpected "2"'), &
        err_case_t('x,y', 'unexpected ","'), &
        err_case_t('sin', 'undefined variable "sin"'), &
        err_case_t('sin()', 'requires 1 argument'), &
        err_case_t('sin(x,)', 'unexpected ")"'), &
        err_case_t('sin(x)(1)', 'unexpected "("'), &
        err_case_t('x(1)', 'unknown function "x"'), &
        err_case_t('pi(1)', 'unknown function "pi"'), &
        err_case_t('foo(x)', 'unknown function "foo"'), &
        err_case_t('e', 'undefined variable "e"'), &
        err_case_t('max(1)', 'requires at least 2 arguments'), &
        err_case_t('if(1,2)', 'requires 3 argument'), &
        err_case_t('atan2(1,2,3)', 'requires 2 argument'), &
        err_case_t('[,]', 'unexpected ","'), &
        err_case_t('[]', 'empty vector constructor'), &
        err_case_t('[1,2', 'expected "]"'), &
        err_case_t('x[', 'unexpected end of expression'), &
        err_case_t('x[]', 'unexpected "]"'), &
        err_case_t('1..2', 'unexpected'), &
        err_case_t('1e', 'unexpected "e"'), &
        err_case_t('1.5e+', 'unexpected "e"'), &
        err_case_t('1e2e', 'unexpected "e"'), &
        err_case_t('x . gt. y', 'invalid character "."'), &  ! no spaces in dot-operators
        err_case_t('@', 'invalid character "@"'), &
        err_case_t('x %y', 'invalid character "%"'), &
        err_case_t('x\y', 'invalid character "\"'), &
        err_case_t('x $ y', 'invalid character "$"'), &
        err_case_t('==1', 'unexpected "=="'), &
        err_case_t('x==', 'unexpected end of expression'), &
        err_case_t('1 <> 2', 'unexpected ">"'), &
        err_case_t('&& 1', 'unexpected "&&"'), &
        err_case_t('1 &&', 'unexpected end of expression'), &
        err_case_t('!', 'unexpected end of expression'), &
        err_case_t('-', 'unexpected end of expression'), &
        err_case_t('x***2', 'unexpected "*"'), &
        err_case_t('x^^2', 'unexpected "^"'), &
        err_case_t('x // y', 'unexpected "/"'), &
        err_case_t('.and. x', 'unexpected'), &
        err_case_t('x .foo. y', 'invalid character "."') ]
    do i = 1, size(cases)
        call check_parse_error(error, [trim(cases(i)%expr)], ['x', 'y'], trim(cases(i)%msg))
        if (allocated(error)) return
    end do
    end subroutine test_parse_errors

    subroutine test_eval_errors(error)
    !! expressions that fail to evaluate
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call check_error_cases(error, [ &
        err_case_t('1/0', 'division by zero'), &
        err_case_t('0/0', 'division by zero'), &
        err_case_t('x/(y + 1.3)', 'division by zero'), &
        err_case_t('0^-1', 'division by zero'), &
        err_case_t('(-8)^(1/3)', 'negative number raised to a non-integer power'), &
        err_case_t('y^z', 'negative number raised to a non-integer power'), &
        err_case_t('log(0)', 'log of a non-positive number'), &
        err_case_t('log10(y)', 'log10 of a non-positive number'), &
        err_case_t('sqrt(y)', 'sqrt'), &
        err_case_t('mod(5, 0)', 'division by zero in mod/modulo'), &
        err_case_t('modulo(x, 0)', 'division by zero in mod/modulo'), &
        err_case_t('atan2(0, 0)', 'atan2 arguments are both zero'), &
        err_case_t('acos(1.0000000000000002)', 'acos argument outside [-1,1]'), &
        err_case_t('asin(z)', 'asin'), &
        err_case_t('atanh(1)', 'atanh argument outside (-1,1)'), &
        err_case_t('acosh(0.5)', 'acosh argument less than 1'), &
        err_case_t('[1,2,3][1.5]', 'index must be an integer'), &
        err_case_t('[1,2,3][0]', 'index 0 is out of bounds (size is 3)'), &
        err_case_t('[1,2,3][4]', 'index 4 is out of bounds (size is 3)'), &
        err_case_t('x[2]', 'index 2 is out of bounds (size is 1)'), &
        err_case_t('cross([1,2],[3,4])', 'arguments of cross must have size 3'), &
        err_case_t('dot([1,2],[1,2,3])', 'different sizes'), &
        err_case_t('[1,2]+[1,2,3]', 'incompatible sizes'), &
        err_case_t('max([1,2],[1,2,3])', 'incompatible sizes'), &
        err_case_t('if([1,1], 1/0, [1,1])', 'division by zero'), &  ! vector condition: both branches
        err_case_t('0 && 1/0', 'division by zero'), &              ! no short circuit
        err_case_t('1 + sin(log(x - 0.7))', 'log of a non-positive number') ])
    end subroutine test_eval_errors

    end module test_expressions
!*****************************************************************************************
