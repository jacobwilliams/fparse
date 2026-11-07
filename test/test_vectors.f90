!*****************************************************************************************
!>
!  Tests for vector variables and vector-valued functions.

    module test_vectors

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_vectors

    real(wp), dimension(3), parameter :: a = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), dimension(3), parameter :: b = [4.0_wp, -5.0_wp, 6.0_wp]

    contains

    subroutine collect_vectors(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("elementwise", test_elementwise), &
        new_unittest("broadcast", test_broadcast), &
        new_unittest("reductions", test_reductions), &
        new_unittest("dot_cross", test_dot_cross), &
        new_unittest("constructor", test_constructor), &
        new_unittest("indexing", test_indexing), &
        new_unittest("vector_functions", test_vector_functions), &
        new_unittest("get_vector", test_get_vector) ]
    end subroutine collect_vectors

    subroutine test_elementwise(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, 'a + b', a + b, ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'a * b', a * b, ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'a / b', a / b, ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'a ^ 2', a ** 2, ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, '-a', -a, ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'sin(a)', sin(a), ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'exp(-a) * cos(b)', exp(-a)*cos(b), ['a','b'], [fparse_value(a), fparse_value(b)])
    if (allocated(error)) return
    call check_vexpr(error, 'a > 1.5', [0.0_wp, 1.0_wp, 1.0_wp], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'max(a, b)', max(a, b), ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'atan2(a, b)', atan2(a, b), ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    end subroutine test_elementwise

    subroutine test_broadcast(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, '2 * a', 2*a, ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'a + s', a + 10, ['a','s'], [fparse_value(a), fparse_value([10.0_wp])]); if (allocated(error)) return
    call check_vexpr(error, 's - a', 10 - a, ['a','s'], [fparse_value(a), fparse_value([10.0_wp])]); if (allocated(error)) return
    call check_vexpr(error, '2 ^ a', 2**a, ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'max(a, 2)', max(a, 2.0_wp), ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'min(2, a, 2.5)', min(a, 2.0_wp), ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'hypot(a, 1)', hypot(a, 1.0_wp), ['a'], [fparse_value(a)]); if (allocated(error)) return
    end subroutine test_broadcast

    subroutine test_reductions(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, 'sum(a)', [sum(a)], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'product(b)', [product(b)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'norm2(b)', [norm2(b)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'maxval(b)', [maxval(b)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'minval(b)', [minval(b)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'size(b)', [3.0_wp], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'size(1)', [1.0_wp], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'sum(a*b)/size(a)', [sum(a*b)/3], ['a','b'], [fparse_value(a), fparse_value(b)])
    end subroutine test_reductions

    subroutine test_dot_cross(error)
    type(error_type), allocatable, intent(out) :: error
    real(wp), dimension(3) :: c
    c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]
    call check_vexpr(error, 'dot_product(a, b)', [dot_product(a, b)], ['a','b'], [fparse_value(a), fparse_value(b)])
    if (allocated(error)) return
    call check_vexpr(error, 'dot(a, a)', [dot_product(a, a)], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'cross(a, b)', c, ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'dot(cross(a, b), a)', [0.0_wp], ['a','b'], [fparse_value(a), fparse_value(b)])
    if (allocated(error)) return
    call check_vexpr(error, 'cross([1,0,0], [0,1,0])', [0.0_wp, 0.0_wp, 1.0_wp], ['a'], [fparse_value(a)])
    end subroutine test_dot_cross

    subroutine test_constructor(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, '[1, 2, 3]', [1.0_wp, 2.0_wp, 3.0_wp], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, '[x, 2*x]', [2.0_wp, 4.0_wp], ['x'], [fparse_value([2.0_wp])]); if (allocated(error)) return
    call check_vexpr(error, '[a, 0]', [a, 0.0_wp], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, '[a, b]', [a, b], ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, '[[1], [2, [3]]]', [1.0_wp, 2.0_wp, 3.0_wp], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'sum([1,2,3] * a)', [sum([1,2,3] * a)], ['a'], [fparse_value(a)]); if (allocated(error)) return
    end subroutine test_constructor

    subroutine test_indexing(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, 'a[1]', [a(1)], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'b[3]', [b(3)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'b[1+1]', [b(2)], ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, '(a+b)[2]', [a(2)+b(2)], ['a','b'], [fparse_value(a), fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'cross(a, b)[3]', [a(1)*b(2) - a(2)*b(1)], ['a','b'], [fparse_value(a), fparse_value(b)])
    if (allocated(error)) return
    call check_vexpr(error, '[5,6,7][2]', [6.0_wp], ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'a[2]^2', [a(2)**2], ['a'], [fparse_value(a)]); if (allocated(error)) return
    end subroutine test_indexing

    subroutine test_vector_functions(error)
    type(error_type), allocatable, intent(out) :: error
    call check_vexpr(error, 'a / norm2(a)', a / norm2(a), ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'abs(b)', abs(b), ['b'], [fparse_value(b)]); if (allocated(error)) return
    call check_vexpr(error, 'sqrt(a)', sqrt(a), ['a'], [fparse_value(a)]); if (allocated(error)) return
    call check_vexpr(error, 'mod(b, 4)', mod(b, 4.0_wp), ['b'], [fparse_value(b)]); if (allocated(error)) return
    end subroutine test_vector_functions

    subroutine test_get_vector(error)
    !! retrieving vector values, and scalar retrieval errors
    type(error_type), allocatable, intent(out) :: error
    type(fparser) :: p
    real(wp), dimension(:), allocatable :: v
    real(wp) :: s
    integer :: status
    call p%parse(['a'], str_list('c = 2*a', 'd = sum(c)'))
    call p%set_variable('a', a)
    call p%evaluate()
    call p%get('c', v)
    call check(error, size(v), 3); if (allocated(error)) return
    call check(error, all(v == 2*a), 'c'); if (allocated(error)) return
    call p%get('d', s)
    call check(error, s, sum(2*a)); if (allocated(error)) return
    call p%get('a', v)  ! inputs can also be retrieved
    call check(error, all(v == a), 'a'); if (allocated(error)) return
    call p%get('c', s, status=status)
    call check(error, status /= 0, 'getting a vector as a scalar should fail'); if (allocated(error)) return
    call check(error, index(p%error_message(), 'not a scalar') > 0, p%error_message())
    end subroutine test_get_vector

    end module test_vectors
!*****************************************************************************************
