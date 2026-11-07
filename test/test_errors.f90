!*****************************************************************************************
!>
!  Tests for error checking at parse time and at runtime.

    module test_errors

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    use test_utils

    implicit none

    private

    public :: collect_errors

    contains

    subroutine collect_errors(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("syntax", test_syntax), &
        new_unittest("names", test_names), &
        new_unittest("functions", test_function_errors), &
        new_unittest("circular", test_circular), &
        new_unittest("domain", test_domain), &
        new_unittest("domain_checking_off", test_domain_off), &
        new_unittest("sizes", test_sizes), &
        new_unittest("api_misuse", test_api_misuse), &
        new_unittest("error_position", test_error_position) ]
    end subroutine collect_errors

    subroutine test_syntax(error)
    type(error_type), allocatable, intent(out) :: error
    call check_parse_error(error, ['sin(x'], ['x'], 'expected ")"'); if (allocated(error)) return
    call check_parse_error(error, ['(x+1'], ['x'], 'expected ")"'); if (allocated(error)) return
    call check_parse_error(error, ['x+1)'], ['x'], 'unexpected ")"'); if (allocated(error)) return
    call check_parse_error(error, ['x+'], ['x'], 'unexpected end'); if (allocated(error)) return
    call check_parse_error(error, ['*x'], ['x'], 'unexpected "*"'); if (allocated(error)) return
    call check_parse_error(error, ['x y'], ['x', 'y'], 'unexpected "y"'); if (allocated(error)) return
    call check_parse_error(error, ['2x'], ['x'], 'unexpected "x"'); if (allocated(error)) return
    call check_parse_error(error, ['x # 2'], ['x'], 'invalid character'); if (allocated(error)) return
    call check_parse_error(error, ['x & 2'], ['x'], '&&'); if (allocated(error)) return
    call check_parse_error(error, ['x | 2'], ['x'], '||'); if (allocated(error)) return
    call check_parse_error(error, ['x .foo. 2'], ['x'], 'invalid character'); if (allocated(error)) return
    call check_parse_error(error, ['   '], ['x'], 'empty expression'); if (allocated(error)) return
    call check_parse_error(error, ['[]'], ['x'], 'empty vector'); if (allocated(error)) return
    call check_parse_error(error, ['[1, 2'], ['x'], 'expected "]"'); if (allocated(error)) return
    call check_parse_error(error, ['x[1'], ['x'], 'expected "]"'); if (allocated(error)) return
    call check_parse_error(error, ['max(1,)'], ['x'], 'unexpected ")"'); if (allocated(error)) return
    call check_parse_error(error, ['= x'], ['x'], 'invalid output name'); if (allocated(error)) return
    call check_parse_error(error, ['x = = 1'], ['y'], 'unexpected "="'); if (allocated(error)) return
    end subroutine test_syntax

    subroutine test_names(error)
    type(error_type), allocatable, intent(out) :: error
    call check_parse_error(error, ['y + 1'], ['x'], 'undefined variable "y"'); if (allocated(error)) return
    call check_parse_error(error, ['x'], ['x', 'X'], 'duplicate variable'); if (allocated(error)) return
    call check_parse_error(error, ['x'], ['1x'], 'invalid variable name'); if (allocated(error)) return
    call check_parse_error(error, ['x'], ['a b'], 'invalid variable name'); if (allocated(error)) return
    call check_parse_error(error, ['x = 1'], ['x'], 'duplicate name "x"'); if (allocated(error)) return
    call check_parse_error(error, str_list('a = 1', 'A = 2'), ['x'], 'duplicate name "a"'); if (allocated(error)) return
    call check_parse_error(error, ['2a = 1'], ['x'], 'invalid output name'); if (allocated(error)) return
    call check_parse_error(error, ['a.b = 1'], ['x'], 'invalid output name'); if (allocated(error)) return
    ! output names given with the `names` argument:
    call check_names_error(error, ['x','y'], str_list('x + y', 'x - y'), ['a', 'A'], 'duplicate name "a"')
    if (allocated(error)) return
    call check_names_error(error, ['x','y'], str_list('x + y', 'x - y'), ['a', 'Y'], 'duplicate name "y"')
    if (allocated(error)) return
    call check_names_error(error, ['x','y'], str_list('x + y'), ['2a'], 'invalid output name')
    if (allocated(error)) return
    call check_names_error(error, ['x','y'], str_list('w = x + y'), ['w'], 'unexpected "="')
    if (allocated(error)) return
    ! anonymous expressions cannot be referenced
    call check_parse_error(error, str_list('x + 1', 'b = c'), ['x'], 'undefined variable "c"'); if (allocated(error)) return
    end subroutine test_names

    subroutine check_names_error(error, vars, exprs, names, contains)
    !! check that parsing with the `names` argument fails with the expected message
    type(error_type), allocatable, intent(out) :: error
    character(len=*), dimension(:), intent(in) :: vars, exprs, names
    character(len=*), intent(in) :: contains
    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg
    call p%parse(vars, exprs, names=names, status=status, errmsg=errmsg)
    if (status == 0) then
        call test_failed(error, 'parse should have failed for names: '//names(1))
    else if (index(errmsg, contains) == 0) then
        call test_failed(error, 'unexpected error message', 'got "'//errmsg//'", expected "'//contains//'"')
    end if
    end subroutine check_names_error

    subroutine test_function_errors(error)
    type(error_type), allocatable, intent(out) :: error
    call check_parse_error(error, ['foo(x)'], ['x'], 'unknown function "foo"'); if (allocated(error)) return
    call check_parse_error(error, ['sin(x, x)'], ['x'], 'requires 1 argument'); if (allocated(error)) return
    call check_parse_error(error, ['sin()'], ['x'], 'requires 1 argument'); if (allocated(error)) return
    call check_parse_error(error, ['atan2(x)'], ['x'], 'requires 2 argument'); if (allocated(error)) return
    call check_parse_error(error, ['max(x)'], ['x'], 'requires at least 2'); if (allocated(error)) return
    call check_parse_error(error, ['if(x, 1)'], ['x'], 'requires 3 argument'); if (allocated(error)) return
    call check_parse_error(error, ['x(1)'], ['x'], 'unknown function "x"'); if (allocated(error)) return
    end subroutine test_function_errors

    subroutine test_circular(error)
    type(error_type), allocatable, intent(out) :: error
    call check_parse_error(error, ['a = a + 1'], ['x'], 'circular dependency'); if (allocated(error)) return
    call check_parse_error(error, str_list('a = b', 'b = c', 'c = a + x', 'd = x'), ['x'], &
                           'circular dependency between expressions: a, b, c'); if (allocated(error)) return
    call check_parse_error(error, str_list('d = x', 'a = b', 'b = a'), ['x'], 'circular'); if (allocated(error)) return
    end subroutine test_circular

    subroutine test_domain(error)
    type(error_type), allocatable, intent(out) :: error
    call check_eval_error(error, '1/x', ['x'], [fparse_value([0.0_wp])], 'division by zero'); if (allocated(error)) return
    call check_eval_error(error, 'sqrt(x)', ['x'], [fparse_value([-1.0_wp])], 'sqrt'); if (allocated(error)) return
    call check_eval_error(error, 'log(x)', ['x'], [fparse_value([0.0_wp])], 'log'); if (allocated(error)) return
    call check_eval_error(error, 'log10(x)', ['x'], [fparse_value([-1.0_wp])], 'log10'); if (allocated(error)) return
    call check_eval_error(error, 'asin(x)', ['x'], [fparse_value([1.5_wp])], 'asin'); if (allocated(error)) return
    call check_eval_error(error, 'acos(x)', ['x'], [fparse_value([-1.5_wp])], 'acos'); if (allocated(error)) return
    call check_eval_error(error, 'acosh(x)', ['x'], [fparse_value([0.5_wp])], 'acosh'); if (allocated(error)) return
    call check_eval_error(error, 'atanh(x)', ['x'], [fparse_value([1.0_wp])], 'atanh'); if (allocated(error)) return
    call check_eval_error(error, 'x^0.5', ['x'], [fparse_value([-4.0_wp])], 'non-integer power'); if (allocated(error)) return
    call check_eval_error(error, 'x^-1', ['x'], [fparse_value([0.0_wp])], 'division by zero'); if (allocated(error)) return
    call check_eval_error(error, 'mod(1, x)', ['x'], [fparse_value([0.0_wp])], 'division by zero'); if (allocated(error)) return
    call check_eval_error(error, 'atan2(x, x)', ['x'], [fparse_value([0.0_wp])], 'atan2'); if (allocated(error)) return
    ! vector: one bad element
    call check_eval_error(error, 'sqrt(x)', ['x'], [fparse_value([1.0_wp, -1.0_wp])], 'sqrt'); if (allocated(error)) return
    ! with a vector condition, both branches are evaluated:
    call check_eval_error(error, 'if(log(x), 0, x > 0)', ['x'], [fparse_value([1.0_wp, -1.0_wp])], 'log')
    if (allocated(error)) return
    end subroutine test_domain

    subroutine test_domain_off(error)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:), allocatable :: v
    integer :: status
    character(len=:), allocatable :: errmsg
    call p%set_domain_checking(.false.)
    call p%parse(['x'], ['if(log(x), 0, x > 0)'])
    call p%set_variable('x', [1.0_wp, -1.0_wp, 2.0_wp])
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%get(1, v)
    call check(error, all(v == [0.0_wp, 0.0_wp, log(2.0_wp)]), 'masked log'); if (allocated(error)) return
    call p%parse(['x'], ['1/x'])
    call p%set_variable('x', 0.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%get(1, v)
    call check(error, .not. ieee_is_finite(v(1)), 'expected Inf'); if (allocated(error)) return
    end subroutine test_domain_off

    subroutine test_sizes(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparse_value), dimension(2) :: v
    v = [fparse_value([1.0_wp, 2.0_wp]), fparse_value([1.0_wp, 2.0_wp, 3.0_wp])]
    call check_eval_error(error, 'a + b', ['a','b'], v, 'incompatible sizes (2 and 3)'); if (allocated(error)) return
    call check_eval_error(error, 'max(a, b)', ['a','b'], v, 'incompatible sizes'); if (allocated(error)) return
    call check_eval_error(error, 'dot(a, b)', ['a','b'], v, 'different sizes'); if (allocated(error)) return
    call check_eval_error(error, 'cross(a, b)', ['a','b'], v, 'size 3'); if (allocated(error)) return
    call check_eval_error(error, 'if(a, b, a > 1)', ['a','b'], v, 'incompatible sizes in if'); if (allocated(error)) return
    call check_eval_error(error, 'a[3]', ['a','b'], v, 'out of bounds'); if (allocated(error)) return
    call check_eval_error(error, 'a[0]', ['a','b'], v, 'out of bounds'); if (allocated(error)) return
    call check_eval_error(error, 'a[1.5]', ['a','b'], v, 'must be an integer'); if (allocated(error)) return
    call check_eval_error(error, 'b[a]', ['a','b'], v, 'must be a scalar'); if (allocated(error)) return
    end subroutine test_sizes

    subroutine test_api_misuse(error)
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg
    real(wp) :: r

    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not been parsed') > 0, errmsg); if (allocated(error)) return

    call p%parse(['x','y'], ['z = x + y'])
    call p%set_variable('x', 1.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, '"y" has not been set') > 0, errmsg); if (allocated(error)) return

    call p%get('z', r, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not been evaluated') > 0, errmsg); if (allocated(error)) return

    call p%set_variable('q', 1.0_wp, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'unknown input variable') > 0, errmsg); if (allocated(error)) return

    call p%set_variable('z', 1.0_wp, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'is an output') > 0, errmsg); if (allocated(error)) return

    call p%set_variable(3, 1.0_wp, status=status, errmsg=errmsg)
    call check(error, status /= 0, 'invalid index'); if (allocated(error)) return

    call p%set_variables([1.0_wp], status=status, errmsg=errmsg)
    call check(error, status /= 0, 'wrong number of values'); if (allocated(error)) return

    call p%set_variable('y', 2.0_wp)
    call p%evaluate(wrt=['z'], status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not an input variable') > 0, errmsg); if (allocated(error)) return

    call p%evaluate(wrt=['x', 'x'], status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'duplicate') > 0, errmsg); if (allocated(error)) return

    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call check(error, .not. p%failed(), 'error should be cleared'); if (allocated(error)) return
    call check(error, p%error_message(), ''); if (allocated(error)) return

    call p%get('nope', r, status=status)
    call check(error, status /= 0, 'unknown variable'); if (allocated(error)) return
    call p%clear_error()
    call check(error, .not. p%failed(), 'clear_error'); if (allocated(error)) return

    call p%get(5, r, status=status)
    call check(error, status /= 0, 'invalid expression index'); if (allocated(error)) return

    call p%parse(['x'], ['x'], names=['a', 'b'], status=status)
    call check(error, status /= 0, 'size of names'); if (allocated(error)) return

    ! a failed parse leaves the parser unusable until the next successful parse
    call p%evaluate(status=status)
    call check(error, status /= 0, 'evaluate after failed parse'); if (allocated(error)) return
    end subroutine test_api_misuse

    subroutine test_error_position(error)
    !! the error message includes the expression and position
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg
    call p%parse(['x'], ['w = x + foo(1)'], status=status, errmsg=errmsg)
    call check(error, index(errmsg, 'position 9') > 0, errmsg); if (allocated(error)) return
    call check(error, index(errmsg, 'expression 1') > 0, errmsg); if (allocated(error)) return
    call p%parse(['x'], str_list('a = 1', 'b = log(x)'))
    call p%set_variable('x', -1.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, index(errmsg, 'expression 2 "b = log(x)"') > 0, errmsg); if (allocated(error)) return
    end subroutine test_error_position

    end module test_errors
!*****************************************************************************************
