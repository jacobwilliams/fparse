!*****************************************************************************************
!>
!  Tests for the derivative computations.

    module test_derivatives

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_derivatives

    contains

    subroutine collect_derivatives(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("analytic", test_analytic), &
        new_unittest("operators_fd", test_operators_fd), &
        new_unittest("functions_fd", test_functions_fd), &
        new_unittest("chained_expressions", test_chained), &
        new_unittest("vector_jacobians", test_vector_jacobians), &
        new_unittest("vector_functions_fd", test_vector_functions_fd), &
        new_unittest("conditionals", test_conditionals_deriv), &
        new_unittest("subset_wrt", test_subset_wrt), &
        new_unittest("no_derivatives", test_no_derivatives) ]
    end subroutine collect_derivatives

    subroutine test_analytic(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: d
    real(wp), parameter :: x = 0.8_wp, y = 1.3_wp
    call p%parse(['x','y'], str_list('f = x^2 * y + sin(x*y)', 'g = 3'))
    call p%set_variables([x, y])
    call p%evaluate(wrt=['x','y'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get_derivative('f', 'x', d)
    call check(error, d, 2*x*y + y*cos(x*y), thr=1.0e-14_wp); if (allocated(error)) return
    call p%get_derivative('f', 'Y', d)
    call check(error, d, x**2 + x*cos(x*y), thr=1.0e-14_wp); if (allocated(error)) return
    call p%get_derivative('g', 'x', d)
    call check(error, d, 0.0_wp); if (allocated(error)) return
    call p%get_derivative(1, 'x', d)
    call check(error, d, 2*x*y + y*cos(x*y), thr=1.0e-14_wp); if (allocated(error)) return
    call p%get_derivative('x', 'x', d) ! input with respect to itself
    call check(error, d, 1.0_wp); if (allocated(error)) return
    call p%get_derivative('y', 'x', d)
    call check(error, d, 0.0_wp); if (allocated(error)) return
    end subroutine test_analytic

    subroutine test_operators_fd(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparse_value), dimension(2) :: v
    v = [fparse_value([0.7_wp]), fparse_value([1.9_wp])]
    call check_derivs_fd(error, str_list('x + y', 'x - y', 'x * y', 'x / y', '-x*y', 'x^y'), ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('x^3', '3^x', 'y^-2', '(x+y)^2.5', '(x > y) + x', '!(x*y)'), ['x','y'], v)
    if (allocated(error)) return
    ! negative base, integer exponent:
    call check_derivs_fd(error, str_list('x^3', 'x^2', 'x^-1'), ['x'], [fparse_value([-1.7_wp])])
    end subroutine test_operators_fd

    subroutine test_functions_fd(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparse_value), dimension(2) :: v
    v = [fparse_value([0.37_wp]), fparse_value([-1.4_wp])]
    call check_derivs_fd(error, str_list('abs(x)', 'abs(y)', 'sqrt(x)', 'exp(x*y)', 'log(x)', 'log10(x)'), ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('sin(x*y)', 'cos(x+y)', 'tan(x)', 'asin(x)', 'acos(x)', 'atan(y)'), ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('sinh(y)', 'cosh(y)', 'tanh(x*y)', 'asinh(y)', 'acosh(2-y)', 'atanh(x)'), &
                         ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('erf(x)', 'erfc(y)', 'floor(y)*x', 'ceiling(y)', 'nint(y)', 'aint(y)'), ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('atan2(y, x)', 'mod(y, x)', 'modulo(y, x)', 'sign(x, y)', 'hypot(x, y)'), &
                         ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('max(x, y)', 'min(x, y)', 'max(x, y, x*y, 2*y)', 'min(x^2, y^2)'), ['x','y'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('sin(cos(exp(x*y)))', 'sqrt(x^2+y^2)*atan2(y,x)-log(1+abs(sin(x*y)))'), &
                         ['x','y'], v)
    end subroutine test_functions_fd

    subroutine test_chained(error)
    !! derivatives through dependent expressions
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: d
    real(wp), parameter :: x = 0.5_wp, y = 2.0_wp
    call check_derivs_fd(error, str_list('w = z + x', 'z = sin(x) + 2.0*y', 'q = w*z^2'), ['x','y'], &
                         [fparse_value([x]), fparse_value([y])])
    if (allocated(error)) return
    call p%parse(['x','y'], str_list('w = z + x', 'z = sin(x) + 2.0*y'))
    call p%set_variables([x, y])
    call p%evaluate(wrt=['x','y'])
    call p%get_derivative('w', 'x', d)
    call check(error, d, cos(x) + 1.0_wp, thr=1.0e-14_wp); if (allocated(error)) return
    call p%get_derivative('w', 'y', d)
    call check(error, d, 2.0_wp, thr=1.0e-14_wp); if (allocated(error)) return
    end subroutine test_chained

    subroutine test_vector_jacobians(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: jac
    real(wp), dimension(3), parameter :: a = [1.0_wp, 2.0_wp, 3.0_wp], b = [-1.0_wp, 0.5_wp, 2.0_wp]
    integer :: i

    call p%parse(['a','b','s'], str_list('c = cross(a, b)', 'd = dot(a, b)', 'e = s*a', 'f = s^2'))
    call p%set_variable('a', a)
    call p%set_variable('b', b)
    call p%set_variable('s', 3.0_wp)
    call p%evaluate(wrt=['a','s'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return

    ! vector wrt vector: matrix
    call p%get_derivative('c', 'a', jac)
    call check(error, all(shape(jac) == [3, 3]), 'shape of dc/da'); if (allocated(error)) return
    ! d(a x b)/da = -[b]x
    call check(error, all(abs(jac - reshape([0.0_wp, -b(3), b(2), b(3), 0.0_wp, -b(1), -b(2), b(1), 0.0_wp], &
                                            [3, 3])) < 1.0e-14_wp), 'dc/da'); if (allocated(error)) return
    ! scalar wrt vector: row
    call p%get_derivative('d', 'a', jac)
    call check(error, all(shape(jac) == [1, 3]), 'shape of dd/da'); if (allocated(error)) return
    call check(error, all(jac(1,:) == b), 'dd/da'); if (allocated(error)) return
    ! vector wrt scalar: column
    call p%get_derivative('e', 's', jac)
    call check(error, all(shape(jac) == [3, 1]), 'shape of de/ds'); if (allocated(error)) return
    call check(error, all(jac(:,1) == a), 'de/ds'); if (allocated(error)) return
    call p%get_derivative('e', 'a', jac)
    do i = 1, 3
        call check(error, jac(i,i), 3.0_wp); if (allocated(error)) return
    end do
    call check(error, sum(abs(jac)), 9.0_wp); if (allocated(error)) return
    ! scalar wrt scalar
    call p%get_derivative('f', 's', jac)
    call check(error, all(shape(jac) == [1, 1]), 'shape of df/ds'); if (allocated(error)) return
    call check(error, jac(1,1), 6.0_wp); if (allocated(error)) return
    ! derivative with respect to b was not requested:
    call p%get_derivative('c', 'b', jac)
    call check(error, p%failed(), 'derivative wrt b should not be available'); if (allocated(error)) return
    end subroutine test_vector_jacobians

    subroutine test_vector_functions_fd(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparse_value), dimension(3) :: v
    v = [fparse_value([0.3_wp, -1.2_wp, 2.0_wp]), fparse_value([1.5_wp, 0.4_wp, -0.7_wp]), fparse_value([0.9_wp])]
    call check_derivs_fd(error, str_list('cross(a, b)', 'dot(a, b)', 'norm2(a)', 'sum(a*b)', 'product(a)', 's*a/norm2(b)'), &
                         ['a','b','s'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('maxval(a)', 'minval(b*s)', 'sin(a)*exp(b)', '[a, s, b[2]]', 'a[3]*b[1]', &
                         'size(a)*s'), ['a','b','s'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('max(a, b)', 'atan2(a, b)', 'a^2 + s^a', 'if(a, b, a > b)', 'hypot(a, s)'), &
                         ['a','b','s'], v)
    if (allocated(error)) return
    call check_derivs_fd(error, str_list('h = cross(a, b)', 'u = h / norm2(h)', 'q = dot(u, a) * s'), ['a','b','s'], v)
    end subroutine test_vector_functions_fd

    subroutine test_conditionals_deriv(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: d
    call p%parse(['x'], str_list('if(x^2, -x, x > 0)'))
    call p%set_variable('x', 3.0_wp)
    call p%evaluate(wrt=['x'])
    call p%get_derivative(1, 'x', d)
    call check(error, d, 6.0_wp); if (allocated(error)) return
    call p%set_variable('x', -3.0_wp)
    call p%evaluate(wrt=['x'])
    call p%get_derivative(1, 'x', d)
    call check(error, d, -1.0_wp); if (allocated(error)) return
    call check_derivs_fd(error, str_list('if(1, if(sin(x), x^3, x < 1), x > 0)', 'if(x, y, x*y > 1)'), ['x','y'], &
                         [fparse_value([0.5_wp]), fparse_value([3.0_wp])])
    end subroutine test_conditionals_deriv

    subroutine test_subset_wrt(error)
    !! wrt order and subsets
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: jac
    call p%parse(['x','y','z'], str_list('f = [x*y, y*z, z]'))
    call p%set_variable('x', 2.0_wp)
    call p%set_variable('y', [3.0_wp])
    call p%set_variable('z', 5.0_wp)
    call p%evaluate(wrt=['z','x'])
    call p%get_derivative('f', 'z', jac)
    call check(error, all(jac(:,1) == [0.0_wp, 3.0_wp, 1.0_wp]), 'df/dz'); if (allocated(error)) return
    call p%get_derivative('f', 'x', jac)
    call check(error, all(jac(:,1) == [3.0_wp, 0.0_wp, 0.0_wp]), 'df/dx'); if (allocated(error)) return
    end subroutine test_subset_wrt

    subroutine test_no_derivatives(error)
    !! derivatives are not available if not requested
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: d
    integer :: status
    call p%parse(['x'], ['x^2'])
    call p%set_variable('x', 3.0_wp)
    call p%evaluate(wrt=['x'])
    call p%evaluate()  ! derivatives from the previous call are cleared
    call p%get_derivative(1, 'x', d, status=status)
    call check(error, status /= 0, 'derivative should not be available'); if (allocated(error)) return
    call check(error, index(p%error_message(), 'were not computed') > 0, p%error_message())
    end subroutine test_no_derivatives

    end module test_derivatives
!*****************************************************************************************
