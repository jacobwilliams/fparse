!*****************************************************************************************
!>
!  Tests for lists of expressions with dependencies between them.

    module test_multi

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_multi

    contains

    subroutine collect_multi(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("readme_example", test_readme_example), &
        new_unittest("names_argument", test_names_argument), &
        new_unittest("ordering", test_ordering), &
        new_unittest("anonymous", test_anonymous), &
        new_unittest("no_inputs", test_no_inputs), &
        new_unittest("vector_chain", test_vector_chain), &
        new_unittest("assignment_with_comparison", test_assignment_cmp) ]
    end subroutine collect_multi

    subroutine test_readme_example(error)
    !! `[z: 'sin(x) + 2.0*y', w: 'z + x']`
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: z, w
    integer :: status
    character(len=:), allocatable :: errmsg
    call p%parse(['x','y'], str_list('z = sin(x) + 2.0*y', 'w = z + x'), status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%set_variable('x', 0.5_wp)
    call p%set_variable('y', 2.0_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%get('z', z)
    call p%get('w', w)
    call check(error, z, sin(0.5_wp) + 4.0_wp); if (allocated(error)) return
    call check(error, w, sin(0.5_wp) + 4.5_wp); if (allocated(error)) return
    call check(error, p%output_name(2), 'w'); if (allocated(error)) return
    end subroutine test_readme_example

    subroutine test_names_argument(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: w
    integer :: status
    character(len=:), allocatable :: errmsg
    call p%parse(['x','y'], str_list('Z + x', 'sin(x) + 2.0*y'), names=['W','z'], &
                 status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%set_variables([0.5_wp, 2.0_wp])
    call p%evaluate()
    call p%get('w', w)
    call check(error, w, sin(0.5_wp) + 4.5_wp); if (allocated(error)) return
    call check(error, all(p%evaluation_order() == [2, 1]), 'evaluation order'); if (allocated(error)) return
    end subroutine test_names_argument

    subroutine test_ordering(error)
    !! expressions given in reverse dependency order
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    integer :: status
    character(len=:), allocatable :: errmsg
    integer, dimension(:), allocatable :: order
    call p%parse(['x'], str_list('e = d + c', 'd = c * 2', 'c = b + a', 'b = a^2', 'a = x + 1', 'f = e + a'), &
                 status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%set_variable('x', 1.0_wp)   ! a=2, b=4, c=6, d=12, e=18, f=20
    call p%evaluate(status=status, errmsg=errmsg)
    call check(error, status, 0, errmsg); if (allocated(error)) return
    call p%get('e', r); call check(error, r, 18.0_wp); if (allocated(error)) return
    call p%get('f', r); call check(error, r, 20.0_wp); if (allocated(error)) return
    call p%get(4, r); call check(error, r, 4.0_wp); if (allocated(error)) return
    order = p%evaluation_order()
    call check(error, all(order == [5, 4, 3, 2, 1, 6]), 'evaluation order'); if (allocated(error)) return
    end subroutine test_ordering

    subroutine test_anonymous(error)
    !! expressions without names, retrieved by index
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    call p%parse(['x'], str_list('x + 1', 'y = 2*x', 'y + 1'))
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable(1, 3.0_wp)
    call p%evaluate()
    call p%get(1, r); call check(error, r, 4.0_wp); if (allocated(error)) return
    call p%get(2, r); call check(error, r, 6.0_wp); if (allocated(error)) return
    call p%get(3, r); call check(error, r, 7.0_wp); if (allocated(error)) return
    call check(error, p%output_name(1), ''); if (allocated(error)) return
    end subroutine test_anonymous

    subroutine test_no_inputs(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    call p%parse([character(len=1) ::], str_list('a = 2', 'b = a * pi'))
    call p%evaluate()
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get('b', r)
    call check(error, r, 2*acos(-1.0_wp)); if (allocated(error)) return
    end subroutine test_no_inputs

    subroutine test_vector_chain(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp), dimension(:), allocatable :: v
    real(wp) :: r
    call p%parse(['r','v'], str_list('h = cross(r, v)', 'hmag = norm2(h)', 'u = h / hmag'))
    call p%set_variable('r', [7000.0_wp, 0.0_wp, 0.0_wp])
    call p%set_variable('v', [0.0_wp, 7.5_wp, 1.0_wp])
    call p%evaluate()
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get('u', v)
    call check(error, size(v), 3); if (allocated(error)) return
    call check(error, norm2(v), 1.0_wp, thr=1.0e-14_wp); if (allocated(error)) return
    call p%get('hmag', r)
    call check(error, r, 7000.0_wp*sqrt(7.5_wp**2 + 1.0_wp), rel=.true., thr=1.0e-14_wp)
    end subroutine test_vector_chain

    subroutine test_assignment_cmp(error)
    !! an assignment whose right-hand side contains comparison operators
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: r
    call p%parse(['x'], str_list('a = x == 1', 'b = x <= 0', 'c= x>=1', 'd =x/=1', 'e = x != 1'))
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%set_variable('x', 1.0_wp)
    call p%evaluate()
    call p%get('a', r); call check(error, r, 1.0_wp); if (allocated(error)) return
    call p%get('b', r); call check(error, r, 0.0_wp); if (allocated(error)) return
    call p%get('c', r); call check(error, r, 1.0_wp); if (allocated(error)) return
    call p%get('d', r); call check(error, r, 0.0_wp); if (allocated(error)) return
    call p%get('e', r); call check(error, r, 0.0_wp); if (allocated(error)) return
    end subroutine test_assignment_cmp

    end module test_multi
!*****************************************************************************************
