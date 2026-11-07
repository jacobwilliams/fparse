!*****************************************************************************************
!>
!  Helper routines for the fparse unit tests.

    module test_utils

    use fparse
    use testdrive, only: error_type, check, test_failed

    implicit none

    private

    public :: check_expr, check_vexpr, check_parse_error, check_eval_error, check_derivs_fd
    public :: str_list, close_to

    character(len=1), dimension(0), parameter :: no_vars = [character(len=1) ::]

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if `a` and `b` agree to within the tolerance (relative and absolute).

    pure logical function close_to(a, b, tol)
    real(wp), intent(in) :: a, b
    real(wp), intent(in), optional :: tol
    real(wp) :: t
    t = 1.0e-12_wp
    if (present(tol)) t = tol
    close_to = abs(a - b) <= t * max(1.0_wp, abs(a), abs(b))
    end function close_to
!*****************************************************************************************

!*****************************************************************************************
!>
!  Make a list of strings of a common length (avoids having to pad the
!  strings in array constructors).

    pure function str_list(s1, s2, s3, s4, s5, s6) result(list)
    character(len=*), intent(in) :: s1
    character(len=*), intent(in), optional :: s2, s3, s4, s5, s6
    character(len=200), dimension(:), allocatable :: list
    ! note: this uses plain assignment (which pads each string) rather than
    ! nested array constructors, because some compilers' runtime checks
    ! wrongly report "Different CHARACTER lengths" for those.
    character(len=200), dimension(6) :: tmp
    integer :: n
    n = 1
    tmp(1) = s1
    if (present(s2)) then; n = n + 1; tmp(n) = s2; end if
    if (present(s3)) then; n = n + 1; tmp(n) = s3; end if
    if (present(s4)) then; n = n + 1; tmp(n) = s4; end if
    if (present(s5)) then; n = n + 1; tmp(n) = s5; end if
    if (present(s6)) then; n = n + 1; tmp(n) = s6; end if
    list = tmp(1:n)
    end function str_list
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check that a single scalar expression of scalar variables evaluates to the expected value.

    subroutine check_expr(error, expr, expected, vars, vals, tol)

    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: expr
    real(wp), intent(in) :: expected
    character(len=*), dimension(:), intent(in), optional :: vars
    real(wp), dimension(:), intent(in), optional :: vals
    real(wp), intent(in), optional :: tol

    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg
    real(wp) :: res
    character(len=40) :: buf1, buf2

    if (present(vars)) then
        call p%parse(vars, [expr], status=status, errmsg=errmsg)
    else
        call p%parse(no_vars, [expr], status=status, errmsg=errmsg)
    end if
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//expr//'"', errmsg)
        return
    end if
    if (present(vals)) then
        call p%set_variables(vals, status=status, errmsg=errmsg)
        if (status /= 0) then
            call test_failed(error, 'set_variables failed for "'//expr//'"', errmsg)
            return
        end if
    end if
    call p%evaluate(status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'evaluate failed for "'//expr//'"', errmsg)
        return
    end if
    call p%get(1, res, status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'get failed for "'//expr//'"', errmsg)
        return
    end if
    if (.not. close_to(res, expected, tol)) then
        write(buf1, '(ES24.16)') res
        write(buf2, '(ES24.16)') expected
        call test_failed(error, 'wrong value for "'//expr//'"', &
                         'got '//trim(buf1)//', expected '//trim(buf2))
    end if

    end subroutine check_expr
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check that a single expression evaluates to the expected vector value.
!  Variables are given as a list of names and a list of values (each
!  possibly a vector).

    subroutine check_vexpr(error, expr, expected, vars, vals, tol)

    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: expr
    real(wp), dimension(:), intent(in) :: expected
    character(len=*), dimension(:), intent(in) :: vars
    type(fparse_value), dimension(:), intent(in) :: vals
    real(wp), intent(in), optional :: tol

    type(fparser) :: p
    integer :: status, i
    character(len=:), allocatable :: errmsg
    real(wp), dimension(:), allocatable :: res

    call p%parse(vars, [expr], status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//expr//'"', errmsg)
        return
    end if
    do i = 1, size(vals)
        call p%set_variable(i, vals(i)%v)
    end do
    call p%evaluate(status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'evaluate failed for "'//expr//'"', errmsg)
        return
    end if
    call p%get(1, res)
    if (size(res) /= size(expected)) then
        call test_failed(error, 'wrong size for "'//expr//'"')
        return
    end if
    do i = 1, size(res)
        if (.not. close_to(res(i), expected(i), tol)) then
            call test_failed(error, 'wrong value for "'//expr//'"')
            return
        end if
    end do

    end subroutine check_vexpr
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check that parsing fails, with an error message containing `contains`.

    subroutine check_parse_error(error, exprs, vars, contains)

    type(error_type), allocatable, intent(out) :: error
    character(len=*), dimension(:), intent(in) :: exprs
    character(len=*), dimension(:), intent(in) :: vars
    character(len=*), intent(in) :: contains

    type(fparser) :: p
    integer :: status
    character(len=:), allocatable :: errmsg

    call p%parse(vars, exprs, status=status, errmsg=errmsg)
    if (status == 0) then
        call test_failed(error, 'parse should have failed for "'//trim(exprs(1))//'"')
    else if (index(errmsg, contains) == 0) then
        call test_failed(error, 'unexpected error message for "'//trim(exprs(1))//'"', &
                         'got "'//errmsg//'", expected it to contain "'//contains//'"')
    else if (.not. p%failed()) then
        call test_failed(error, 'failed() should be true')
    end if

    end subroutine check_parse_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check that evaluating a single expression of scalar variables fails,
!  with an error message containing `contains`.

    subroutine check_eval_error(error, expr, vars, vals, contains)

    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: expr
    character(len=*), dimension(:), intent(in) :: vars
    type(fparse_value), dimension(:), intent(in) :: vals
    character(len=*), intent(in) :: contains

    type(fparser) :: p
    integer :: status, i
    character(len=:), allocatable :: errmsg

    call p%parse(vars, [expr], status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//expr//'"', errmsg)
        return
    end if
    do i = 1, size(vals)
        call p%set_variable(i, vals(i)%v)
    end do
    call p%evaluate(status=status, errmsg=errmsg)
    if (status == 0) then
        call test_failed(error, 'evaluate should have failed for "'//expr//'"')
    else if (index(errmsg, contains) == 0) then
        call test_failed(error, 'unexpected error message for "'//expr//'"', &
                         'got "'//errmsg//'", expected it to contain "'//contains//'"')
    end if

    end subroutine check_eval_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check the derivatives of all the outputs of a set of expressions with
!  respect to all the input variables, by comparing them to central
!  finite differences.

    subroutine check_derivs_fd(error, exprs, vars, vals, tol)

    type(error_type), allocatable, intent(out) :: error
    character(len=*), dimension(:), intent(in) :: exprs !! the expressions
    character(len=*), dimension(:), intent(in) :: vars !! the input variables
    type(fparse_value), dimension(:), intent(in) :: vals !! the variable values
    real(wp), intent(in), optional :: tol

    type(fparser) :: p
    integer :: status, i, k, j
    character(len=:), allocatable :: errmsg
    real(wp), dimension(:,:), allocatable :: jac
    real(wp), dimension(:), allocatable :: fp, fm, x
    real(wp) :: h, t
    character(len=200) :: buf

    t = 1.0e-6_wp
    if (present(tol)) t = tol

    call p%parse(vars, exprs, status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'parse failed for "'//trim(exprs(1))//'"', errmsg)
        return
    end if

    do i = 1, size(vals)
        call p%set_variable(i, vals(i)%v)
    end do
    call p%evaluate(wrt=vars, status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'evaluate failed for "'//trim(exprs(1))//'"', errmsg)
        return
    end if

    do i = 1, size(exprs)
        do k = 1, size(vars)
            call p%evaluate(wrt=vars)
            call p%get_derivative(i, vars(k), jac, status=status, errmsg=errmsg)
            if (status /= 0) then
                call test_failed(error, 'get_derivative failed', errmsg)
                return
            end if
            ! finite differences:
            do j = 1, size(vals(k)%v)
                x = vals(k)%v
                h = 1.0e-6_wp * max(1.0_wp, abs(x(j)))
                x(j) = vals(k)%v(j) + h
                call p%set_variable(k, x)
                call p%evaluate()
                call p%get(i, fp)
                x(j) = vals(k)%v(j) - h
                call p%set_variable(k, x)
                call p%evaluate()
                call p%get(i, fm)
                call p%set_variable(k, vals(k)%v)
                if (size(jac, 1) /= size(fp) .or. size(jac, 2) /= size(vals(k)%v)) then
                    call test_failed(error, 'wrong derivative shape for "'//trim(exprs(i))//'"')
                    return
                end if
                fp = (fp - fm) / (2.0_wp * h)
                if (any(abs(jac(:, j) - fp) > t * max(1.0_wp, abs(fp)))) then
                    write(buf, '(A,I0,A,*(ES12.4,1X))') 'd/d'//trim(vars(k))//'(', j, ') = ', jac(:, j)
                    write(buf, '(A,A,*(ES12.4,1X))') trim(buf), '  fd = ', fp
                    call test_failed(error, 'wrong derivative for "'//trim(exprs(i))//'"', trim(buf))
                    return
                end if
            end do
        end do
    end do

    end subroutine check_derivs_fd
!*****************************************************************************************

    end module test_utils
!*****************************************************************************************
