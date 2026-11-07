!*****************************************************************************************
!>
!  Tests for the use of sparsity in the derivative computations:
!  the jacobians must be the same with sparsity enabled and disabled.

    module test_sparsity

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    use test_utils

    implicit none

    private

    public :: collect_sparsity

    contains

    subroutine collect_sparsity(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("scalar_system", test_scalar_system), &
        new_unittest("vector_system", test_vector_system), &
        new_unittest("independent_outputs", test_independent), &
        new_unittest("conditionals", test_conditionals_sparse), &
        new_unittest("user_functions", test_user_sparse), &
        new_unittest("wrt_subsets", test_wrt_subsets), &
        new_unittest("toggle", test_toggle) ]
    end subroutine collect_sparsity

    ! --- a user function: f(a, b) = a * sum(b) ---
    subroutine fsum(args, res, stat)
    type(fparse_value), dimension(:), intent(in) :: args !! the function arguments
    real(wp), dimension(:), allocatable, intent(out) :: res !! the function result
    integer, intent(inout) :: stat !! status code (nonzero for an error)
    res = args(1)%v * sum(args(2)%v)
    end subroutine fsum

    subroutine compare_jacobians(error, vars, vals, exprs, wrt, names)
    !! evaluate the derivatives with sparsity on and off (and with both evaluators),
    !! and check that all the jacobians (including those of the inputs) are the same.
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=*), dimension(:), intent(in) :: vars !! the input variables
    type(fparse_value), dimension(:), intent(in) :: vals !! the variable values
    character(len=*), dimension(:), intent(in) :: exprs !! the expressions
    character(len=*), dimension(:), intent(in) :: wrt !! the variables to differentiate with respect to
    character(len=*), dimension(:), intent(in), optional :: names !! the output names
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: j_dense, j_sparse
    integer :: i, k, status, iscalar
    character(len=:), allocatable :: errmsg
    character(len=:), allocatable :: label

    call p%add_function('fsum', fsum, is_pure=.true., nargs=2)
    if (present(names)) then
        call p%parse(vars, exprs, names=names, status=status, errmsg=errmsg)
    else
        call p%parse(vars, exprs, status=status, errmsg=errmsg)
    end if
    if (status /= 0) then
        call test_failed(error, 'parse failed', errmsg)
        return
    end if
    do i = 1, size(vals)
        call p%set_variable(i, vals(i)%v)
    end do

    do iscalar = 1, 2
        call p%set_scalar_optimization(iscalar == 1)
        do i = 1, size(exprs)
            do k = 1, size(wrt)
                label = 'expression '//trim(exprs(i))//' wrt '//trim(wrt(k))
                call p%set_sparsity(.false.)
                call p%evaluate(wrt=wrt, status=status, errmsg=errmsg)
                if (status /= 0) then
                    call test_failed(error, 'evaluate failed (dense)', errmsg)
                    return
                end if
                call p%get_derivative(i, wrt(k), j_dense)
                call p%set_sparsity(.true.)
                call p%evaluate(wrt=wrt, status=status, errmsg=errmsg)
                if (status /= 0) then
                    call test_failed(error, 'evaluate failed (sparse)', errmsg)
                    return
                end if
                call p%get_derivative(i, wrt(k), j_sparse, status=status, errmsg=errmsg)
                if (status /= 0) then
                    call test_failed(error, 'get_derivative failed (sparse)', errmsg)
                    return
                end if
                if (any(shape(j_dense) /= shape(j_sparse))) then
                    call test_failed(error, 'different jacobian shapes for '//label)
                    return
                end if
                if (any(abs(j_dense - j_sparse) > 1.0e-14_wp * max(1.0_wp, abs(j_dense)))) then
                    call test_failed(error, 'different jacobians for '//label)
                    return
                end if
            end do
        end do
        ! inputs with respect to each other:
        do i = 1, size(vars)
            do k = 1, size(wrt)
                call p%set_sparsity(.false.)
                call p%evaluate(wrt=wrt)
                call p%get_derivative(trim(vars(i)), wrt(k), j_dense)
                call p%set_sparsity(.true.)
                call p%evaluate(wrt=wrt)
                call p%get_derivative(trim(vars(i)), wrt(k), j_sparse)
                if (any(shape(j_dense) /= shape(j_sparse))) then
                    call test_failed(error, 'different input jacobian shapes')
                    return
                end if
                if (any(j_dense /= j_sparse)) then
                    call test_failed(error, 'different input jacobians')
                    return
                end if
            end do
        end do
    end do

    end subroutine compare_jacobians

    subroutine test_scalar_system(error)
    !! a chain of scalar equations, each depending on a few variables
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call compare_jacobians(error, ['a','b','c','d','e'], &
        [fparse_value([0.3_wp]), fparse_value([1.1_wp]), fparse_value([-0.7_wp]), &
         fparse_value([2.0_wp]), fparse_value([0.5_wp])], &
        str_list('f = sin(a)*b', 'g = exp(c) + d^2', 'h = f*g', 'q = h + e', 'r = c*d', 's = 3'), &
        ['a','b','c','d','e'])
    end subroutine test_scalar_system

    subroutine test_vector_system(error)
    !! vector variables and vector outputs (the tree evaluator)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call compare_jacobians(error, ['u','v','w','s'], &
        [fparse_value([0.3_wp, 1.0_wp, -0.5_wp]), fparse_value([1.5_wp, 0.4_wp, -0.7_wp]), &
         fparse_value([2.0_wp, 0.1_wp]), fparse_value([0.9_wp])], &
        str_list('c = cross(u, v)', 'n = norm2(c) * s', 'm = w * s + sum(u)', 'k = [n, m, c[2]]', &
                 'z = dot(w, w)', 'y = product(u) + maxval(v)'), &
        ['u','v','w','s'])
    end subroutine test_vector_system

    subroutine test_independent(error)
    !! outputs that don't depend on some (or any) of the derivative variables
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp), dimension(:,:), allocatable :: jac
    real(wp) :: d
    call p%parse(['x','y','v'], str_list('a = 2*x', 'b = y^2', 'c = 42', 'e = v*a'))
    call p%set_variable('x', 1.0_wp)
    call p%set_variable('y', 3.0_wp)
    call p%set_variable('v', [1.0_wp, 2.0_wp])
    call p%evaluate(wrt=['x', 'y', 'v'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get_derivative('a', 'y', d)
    call check(error, d, 0.0_wp); if (allocated(error)) return
    call p%get_derivative('b', 'y', d)
    call check(error, d, 6.0_wp); if (allocated(error)) return
    call p%get_derivative('c', 'x', d)
    call check(error, d, 0.0_wp); if (allocated(error)) return
    call p%get_derivative('a', 'v', jac) ! scalar wrt a vector it doesn't depend on: zero row
    call check(error, all(shape(jac) == [1, 2]) .and. all(jac == 0.0_wp), 'da/dv'); if (allocated(error)) return
    call p%get_derivative('e', 'y', jac) ! vector wrt a scalar it doesn't depend on: zero column
    call check(error, all(shape(jac) == [2, 1]) .and. all(jac == 0.0_wp), 'de/dy'); if (allocated(error)) return
    call p%get_derivative('e', 'x', jac)
    call check(error, all(jac(:,1) == [2.0_wp, 4.0_wp]), 'de/dx'); if (allocated(error)) return
    call p%get_derivative('x', 'y', d) ! inputs
    call check(error, d, 0.0_wp); if (allocated(error)) return
    call p%get_derivative('x', 'x', d)
    call check(error, d, 1.0_wp); if (allocated(error)) return
    ! the same with sparsity disabled:
    call compare_jacobians(error, ['x','y','v'], &
        [fparse_value([1.0_wp]), fparse_value([3.0_wp]), fparse_value([1.0_wp, 2.0_wp])], &
        str_list('a = 2*x', 'b = y^2', 'c = 42', 'e = v*a'), ['x', 'y', 'v'])
    end subroutine test_independent

    subroutine test_conditionals_sparse(error)
    !! the dependency analysis is structural: both branches of an if are included
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call compare_jacobians(error, ['x','y','z'], &
        [fparse_value([0.5_wp]), fparse_value([2.0_wp]), fparse_value([-1.0_wp])], &
        str_list('a = if(x^2, y, z > 0)', 'b = if(a, z, x < y)', 'c = if(1, 2, x)'), ['x','y','z'])
    if (allocated(error)) return
    call compare_jacobians(error, ['x','y','z'], &  ! now the other branches
        [fparse_value([0.5_wp]), fparse_value([2.0_wp]), fparse_value([1.0_wp])], &
        str_list('a = if(x^2, y, z > 0)', 'b = if(a, z, x > y)', 'c = if(1, 2, x)'), ['x','y','z'])
    end subroutine test_conditionals_sparse

    subroutine test_user_sparse(error)
    !! user functions (finite-difference derivatives)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call compare_jacobians(error, ['a','b','c'], &
        [fparse_value([2.0_wp]), fparse_value([1.0_wp, 2.0_wp, 3.0_wp]), fparse_value([0.5_wp])], &
        str_list('f = fsum(a, b)', 'g = f + c', 'h = fsum(c, [c, 1])'), ['a','b','c'])
    end subroutine test_user_sparse

    subroutine test_wrt_subsets(error)
    !! only some of the inputs are derivative variables, in a different order
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    call compare_jacobians(error, ['a','b','c','d'], &
        [fparse_value([0.3_wp]), fparse_value([1.1_wp, 2.0_wp]), fparse_value([-0.7_wp]), fparse_value([2.0_wp])], &
        str_list('f = a*b', 'g = c + d', 'h = f*g'), ['d','b'])
    end subroutine test_wrt_subsets

    subroutine test_toggle(error)
    !! switching sparsity on and off between evaluations, and changing the wrt list
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    real(wp) :: d
    integer :: i
    call p%parse(['x','y'], str_list('f = x*y', 'g = f + x'))
    call p%set_variables([2.0_wp, 3.0_wp])
    do i = 1, 4
        call p%set_sparsity(mod(i, 2) == 0)
        call p%evaluate(wrt=['x','y'])
        call p%get_derivative('g', 'x', d)
        call check(error, d, 4.0_wp); if (allocated(error)) return
        call p%get_derivative('g', 'y', d)
        call check(error, d, 2.0_wp); if (allocated(error)) return
        call p%evaluate(wrt=['y'])
        call p%get_derivative('g', 'y', d)
        call check(error, d, 2.0_wp); if (allocated(error)) return
        call p%evaluate()
    end do
    end subroutine test_toggle

    end module test_sparsity
!*****************************************************************************************
