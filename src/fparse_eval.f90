!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Evaluation of expression trees, with optional forward-mode
!  automatic differentiation.
!
!  All values are stored as rank-1 arrays (a scalar is an array of size 1).
!  In binary operations, a value of size 1 is broadcast to the size of
!  the other operand. Otherwise, the sizes must match.
!
!  When derivatives are requested (`nd>0`), each value also carries a
!  jacobian matrix `d` with shape `[size(v), nd]`, which is the derivative
!  of the value with respect to the `nd` (concatenated) elements of
!  the variables being differentiated with respect to. When `nd=0`,
!  no derivative computations are performed.
!
!  Each node of the tree stores its result in its own buffer (`tree%buf`),
!  which is reused between evaluations, so that no memory allocation is
!  needed once the sizes of the values are known.

module fparse_eval

use fparse_common
use fparse_functions
use fparse_parser

implicit none

private

real(wp), parameter :: pi = acos(-1.0_wp)

type(val_t), dimension(0) :: no_dref !! used when no `dref` is given

integer, parameter, public :: fn_offset = 100 !! binary operation codes for the two-argument
                                              !! functions are `fn_offset + fn_*`.

! domain error codes:
integer, parameter :: err_div_zero   = 1
integer, parameter :: err_pow_zero   = 2
integer, parameter :: err_pow_neg    = 3
integer, parameter :: err_atan2      = 4
integer, parameter :: err_mod_zero   = 5
integer, parameter :: err_sqrt       = 6
integer, parameter :: err_log        = 7
integer, parameter :: err_log10      = 8
integer, parameter :: err_asin       = 9
integer, parameter :: err_acos       = 10
integer, parameter :: err_acosh      = 11
integer, parameter :: err_atanh      = 12

type, public :: eval_ctx
    !! Evaluation context.
    integer :: nd = 0 !! number of derivative columns (0 for no derivatives)
    logical :: check_domain = .true. !! check for domain errors (e.g., `log` of a
                                     !! negative number, division by zero)
    type(error_t) :: err !! error status
end type eval_ctx

public :: eval_tree
! scalar helper functions (also used by the bytecode evaluator):
public :: binary_value, binary_check, has_domain_check
public :: unary_value, unary_check, domain_message
public :: pow, is_integer
public :: binary_partials, unary_derivative

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate an expression tree. The result is in `tree%buf(tree%root)`.

subroutine eval_tree(tree, vars, funcs, ctx, dref)

    type(expr_tree), intent(inout) :: tree !! the expression tree
    type(val_t), dimension(:), intent(in) :: vars !! variable values (and derivatives if `nd>0`)
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), dimension(:), intent(in), optional :: dref !! derivatives of the referenced variables
                                                            !! (indexed like `tree%refs`). Where `dref(i)%d` is
                                                            !! allocated, it is used instead of the variable's `d`.

    if (present(dref)) then
        call eval_node(tree, tree%root, vars, funcs, ctx, dref)
    else
        call eval_node(tree, tree%root, vars, funcs, ctx, no_dref)
    end if

end subroutine eval_tree
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a node of an expression tree (recursively).
!  The result is stored in `tree%buf(inode)`.

recursive subroutine eval_node(tree, inode, vars, funcs, ctx, dref)

    type(expr_tree), intent(inout) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), dimension(:), intent(in) :: dref !! derivatives of the referenced variables (see [[eval_tree]])

    integer :: nd, kind, op, pos, k, c1, c2, i, m, n, nargs, ir

    nd = ctx%nd
    kind = tree%nodes(inode)%kind
    op = tree%nodes(inode)%op
    pos = tree%nodes(inode)%pos

    select case (kind)

    case (node_const)
        call ensure(tree%buf(inode), 1, nd)
        tree%buf(inode)%v(1) = tree%nodes(inode)%value
        if (nd > 0) tree%buf(inode)%d = 0.0_wp

    case (node_var)
        ir = tree%nodes(inode)%iref
        if (nd > 0 .and. ir <= size(dref)) then
            if (allocated(dref(ir)%d)) then
                call ensure(tree%buf(inode), size(vars(op)%v), nd)
                tree%buf(inode)%v = vars(op)%v
                tree%buf(inode)%d = dref(ir)%d
                return
            end if
        end if
        call copy_val(vars(op), tree%buf(inode), nd)

    case (node_unary)
        c1 = tree%nodes(inode)%args(1)
        call eval_node(tree, c1, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        associate (a => tree%buf(c1), r => tree%buf(inode))
            call ensure(r, size(a%v), nd)
            if (op == op_neg) then
                r%v = -a%v
                if (nd > 0) r%d = -a%d
            else ! op_not
                r%v = merge(1.0_wp, 0.0_wp, a%v == 0.0_wp)
                if (nd > 0) r%d = 0.0_wp
            end if
        end associate

    case (node_binary)
        c1 = tree%nodes(inode)%args(1)
        c2 = tree%nodes(inode)%args(2)
        call eval_node(tree, c1, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        call eval_node(tree, c2, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        call binary_op(op, tree%buf(c1), tree%buf(c2), ctx, pos, tree%buf(inode))

    case (node_builtin)
        call eval_builtin(tree, inode, vars, funcs, ctx, dref)

    case (node_user)
        call eval_user(tree, inode, vars, funcs, ctx, dref)

    case (node_vector)
        nargs = size(tree%nodes(inode)%args)
        n = 0
        do k = 1, nargs
            c1 = tree%nodes(inode)%args(k)
            call eval_node(tree, c1, vars, funcs, ctx, dref)
            if (ctx%err%failed) return
            n = n + size(tree%buf(c1)%v)
        end do
        call ensure(tree%buf(inode), n, nd)
        i = 0
        do k = 1, nargs
            c1 = tree%nodes(inode)%args(k)
            associate (a => tree%buf(c1), r => tree%buf(inode))
                m = size(a%v)
                r%v(i+1:i+m) = a%v
                if (nd > 0) r%d(i+1:i+m, :) = a%d
                i = i + m
            end associate
        end do

    case (node_index)
        c1 = tree%nodes(inode)%args(1)
        c2 = tree%nodes(inode)%args(2)
        call eval_node(tree, c1, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        call eval_node(tree, c2, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        associate (a => tree%buf(c1), b => tree%buf(c2), r => tree%buf(inode))
            if (size(b%v) /= 1) then
                call ctx%err%set('index must be a scalar', pos)
                return
            end if
            if (b%v(1) /= aint(b%v(1)) .or. abs(b%v(1)) > real(huge(1), wp)) then
                call ctx%err%set('index must be an integer', pos)
                return
            end if
            k = nint(b%v(1))
            if (k < 1 .or. k > size(a%v)) then
                call ctx%err%set('index '//int_to_str(k)//' is out of bounds (size is '//&
                                 int_to_str(size(a%v))//')', pos)
                return
            end if
            call ensure(r, 1, nd)
            r%v(1) = a%v(k)
            if (nd > 0) r%d(1, :) = a%d(k, :)
        end associate

    case default
        call ctx%err%set('invalid expression tree', pos)
    end select

end subroutine eval_node
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a built-in function.

recursive subroutine eval_builtin(tree, inode, vars, funcs, ctx, dref)

    type(expr_tree), intent(inout) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), dimension(:), intent(in) :: dref !! derivatives of the referenced variables (see [[eval_tree]])

    integer :: id, nd, k, i, m, n, pos, nargs, c, c1, c2, c3, na, nb, ia, ib
    real(wp) :: s
    real(wp), dimension(:), allocatable :: pre, suf

    nd = ctx%nd
    id = tree%nodes(inode)%op
    pos = tree%nodes(inode)%pos
    nargs = size(tree%nodes(inode)%args)

    ! evaluate the arguments:
    if (id == fn_if) then
        ! if(a, b, cond): only the required branch is evaluated when cond is a scalar
        c1 = tree%nodes(inode)%args(1)
        c2 = tree%nodes(inode)%args(2)
        c3 = tree%nodes(inode)%args(3)
        call eval_node(tree, c3, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        if (size(tree%buf(c3)%v) == 1) then
            if (tree%buf(c3)%v(1) /= 0.0_wp) then
                c = c1
            else
                c = c2
            end if
            call eval_node(tree, c, vars, funcs, ctx, dref)
            if (ctx%err%failed) return
            call copy_val(tree%buf(c), tree%buf(inode), nd)
            return
        end if
        call eval_node(tree, c1, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        call eval_node(tree, c2, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
    else
        do k = 1, nargs
            call eval_node(tree, tree%nodes(inode)%args(k), vars, funcs, ctx, dref)
            if (ctx%err%failed) return
        end do
        c1 = tree%nodes(inode)%args(1)
        c2 = c1
        if (nargs > 1) c2 = tree%nodes(inode)%args(2)
    end if

    if (id <= fn_last_elemental) then
        call unary_fn(id, tree%buf(c1), ctx, pos, tree%buf(inode))
        return
    end if

    associate (a => tree%buf(c1), b => tree%buf(c2), r => tree%buf(inode))

        n = size(a%v)

        select case (id)

        case (fn_if) ! with a vector condition
            associate (cond => tree%buf(c3))
                m = size(cond%v)
                na = size(a%v)
                nb = size(b%v)
                if ((na /= 1 .and. na /= m) .or. (nb /= 1 .and. nb /= m)) then
                    call ctx%err%set('incompatible sizes in if: condition has size '//int_to_str(m)//&
                                     ', values have sizes '//int_to_str(na)//' and '//int_to_str(nb), pos)
                    return
                end if
                call ensure(r, m, nd)
                do i = 1, m
                    if (cond%v(i) /= 0.0_wp) then
                        ia = merge(1, i, na == 1)
                        r%v(i) = a%v(ia)
                        if (nd > 0) r%d(i, :) = a%d(ia, :)
                    else
                        ib = merge(1, i, nb == 1)
                        r%v(i) = b%v(ib)
                        if (nd > 0) r%d(i, :) = b%d(ib, :)
                    end if
                end do
            end associate

        case (fn_max, fn_min)
            ! size of the result:
            m = n
            do k = 2, nargs
                nb = size(tree%buf(tree%nodes(inode)%args(k))%v)
                if (nb == m .or. nb == 1) cycle
                if (m == 1) then
                    m = nb
                else
                    call ctx%err%set('incompatible sizes ('//int_to_str(m)//' and '//int_to_str(nb)//')', pos)
                    return
                end if
            end do
            call ensure(r, m, nd)
            do i = 1, m
                ia = merge(1, i, n == 1)
                r%v(i) = a%v(ia)
                if (nd > 0) r%d(i, :) = a%d(ia, :)
            end do
            do k = 2, nargs
                associate (bk => tree%buf(tree%nodes(inode)%args(k)))
                    nb = size(bk%v)
                    do i = 1, m
                        ib = merge(1, i, nb == 1)
                        if ((id == fn_max .and. bk%v(ib) > r%v(i)) .or. &
                            (id == fn_min .and. bk%v(ib) < r%v(i))) then
                            r%v(i) = bk%v(ib)
                            if (nd > 0) r%d(i, :) = bk%d(ib, :)
                        end if
                    end do
                end associate
            end do

        case (fn_atan2, fn_mod, fn_modulo, fn_sign, fn_hypot)
            call binary_op(fn_offset + id, a, b, ctx, pos, r)

        case (fn_sum)
            call ensure(r, 1, nd)
            r%v(1) = sum(a%v)
            if (nd > 0) r%d(1, :) = sum(a%d, dim=1)

        case (fn_product)
            call ensure(r, 1, nd)
            r%v(1) = product(a%v)
            if (nd > 0) then
                ! d(prod)/d(a_i) = product of all a_j for j/=i
                allocate(pre(0:n), suf(1:n+1))
                pre(0) = 1.0_wp
                do i = 1, n
                    pre(i) = pre(i-1) * a%v(i)
                end do
                suf(n+1) = 1.0_wp
                do i = n, 1, -1
                    suf(i) = suf(i+1) * a%v(i)
                end do
                r%d = 0.0_wp
                do i = 1, n
                    r%d(1, :) = r%d(1, :) + pre(i-1) * suf(i+1) * a%d(i, :)
                end do
            end if

        case (fn_norm2)
            call ensure(r, 1, nd)
            s = norm2(a%v)
            r%v(1) = s
            if (nd > 0) then
                if (s > 0.0_wp) then
                    r%d(1, :) = matmul(a%v, a%d) / s
                else
                    r%d = 0.0_wp
                end if
            end if

        case (fn_maxval, fn_minval)
            if (n == 0) then
                call ctx%err%set('argument of maxval/minval has size 0', pos)
                return
            end if
            if (id == fn_maxval) then
                k = maxloc(a%v, dim=1)
            else
                k = minloc(a%v, dim=1)
            end if
            call ensure(r, 1, nd)
            r%v(1) = a%v(k)
            if (nd > 0) r%d(1, :) = a%d(k, :)

        case (fn_size)
            call ensure(r, 1, nd)
            r%v(1) = real(n, wp)
            if (nd > 0) r%d = 0.0_wp

        case (fn_dot)
            if (n /= size(b%v)) then
                call ctx%err%set('arguments of dot_product have different sizes ('//&
                                 int_to_str(n)//' and '//int_to_str(size(b%v))//')', pos)
                return
            end if
            call ensure(r, 1, nd)
            r%v(1) = dot_product(a%v, b%v)
            if (nd > 0) r%d(1, :) = matmul(b%v, a%d) + matmul(a%v, b%d)

        case (fn_cross)
            if (n /= 3 .or. size(b%v) /= 3) then
                call ctx%err%set('arguments of cross must have size 3 (sizes are '//&
                                 int_to_str(n)//' and '//int_to_str(size(b%v))//')', pos)
                return
            end if
            call ensure(r, 3, nd)
            r%v = cross3(a%v, b%v)
            if (nd > 0) then
                do k = 1, nd
                    r%d(:, k) = cross3(a%d(:, k), b%v) + cross3(a%v, b%d(:, k))
                end do
            end if

        case default
            call ctx%err%set('invalid function', pos)
        end select

    end associate

end subroutine eval_builtin
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a binary operation `r = a op b` (an operator, or a two-argument
!  elemental function if `code > fn_offset`), with broadcasting of size-1 operands.

subroutine binary_op(code, a, b, ctx, pos, r)

    integer, intent(in) :: code !! operation code (`op_*` or `fn_offset + fn_*`)
    type(val_t), intent(in) :: a !! left operand
    type(val_t), intent(in) :: b !! right operand
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(inout) :: r !! result

    integer :: na, nb, m, i, ia, ib, ierr, nd
    real(wp) :: da, db

    nd = ctx%nd
    na = size(a%v)
    nb = size(b%v)
    if (na == nb) then
        m = na
    else if (na == 1) then
        m = nb
    else if (nb == 1) then
        m = na
    else
        call ctx%err%set('incompatible sizes ('//int_to_str(na)//' and '//int_to_str(nb)//')', pos)
        return
    end if

    if (ctx%check_domain .and. has_domain_check(code)) then
        do i = 1, m
            ia = merge(1, i, na == 1)
            ib = merge(1, i, nb == 1)
            ierr = binary_check(code, a%v(ia), b%v(ib))
            if (ierr /= 0) then
                call ctx%err%set(domain_message(ierr), pos)
                return
            end if
        end do
    end if

    call ensure(r, m, nd)
    if (na == nb) then
        select case (code)
        case (op_add); r%v = a%v + b%v
        case (op_sub); r%v = a%v - b%v
        case (op_mul); r%v = a%v * b%v
        case (op_div); r%v = a%v / b%v
        case default;  r%v = binary_value(code, a%v, b%v)
        end select
    else if (na == 1) then
        select case (code)
        case (op_add); r%v = a%v(1) + b%v
        case (op_sub); r%v = a%v(1) - b%v
        case (op_mul); r%v = a%v(1) * b%v
        case (op_div); r%v = a%v(1) / b%v
        case default;  r%v = binary_value(code, a%v(1), b%v)
        end select
    else
        select case (code)
        case (op_add); r%v = a%v + b%v(1)
        case (op_sub); r%v = a%v - b%v(1)
        case (op_mul); r%v = a%v * b%v(1)
        case (op_div); r%v = a%v / b%v(1)
        case default;  r%v = binary_value(code, a%v, b%v(1))
        end select
    end if

    if (nd > 0) then
        do i = 1, m
            ia = merge(1, i, na == 1)
            ib = merge(1, i, nb == 1)
            call binary_partials(code, a%v(ia), b%v(ib), r%v(i), da, db)
            r%d(i, :) = da * a%d(ia, :) + db * b%d(ib, :)
        end do
    end if

end subroutine binary_op
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the binary operation has a domain check.

pure logical function has_domain_check(code)
    integer, intent(in) :: code !! the operation code
    select case (code)
    case (op_div, op_pow, fn_offset + fn_atan2, fn_offset + fn_mod, fn_offset + fn_modulo)
        has_domain_check = .true.
    case default
        has_domain_check = .false.
    end select
end function has_domain_check
!*****************************************************************************************

!*****************************************************************************************
!>
!  Domain check for a binary operation. Returns a nonzero error code if there is an error.

pure integer function binary_check(code, x, y) result(ierr)
    integer, intent(in) :: code !! the operation code
    real(wp), intent(in) :: x !! the first operand
    real(wp), intent(in) :: y !! the second operand
    ierr = 0
    select case (code)
    case (op_div)
        if (y == 0.0_wp) ierr = err_div_zero
    case (op_pow)
        if (x == 0.0_wp .and. y < 0.0_wp) then
            ierr = err_pow_zero
        else if (x < 0.0_wp .and. .not. is_integer(y)) then
            ierr = err_pow_neg
        end if
    case (fn_offset + fn_atan2)
        if (x == 0.0_wp .and. y == 0.0_wp) ierr = err_atan2
    case (fn_offset + fn_mod, fn_offset + fn_modulo)
        if (y == 0.0_wp) ierr = err_mod_zero
    end select
end function binary_check
!*****************************************************************************************

!*****************************************************************************************
!>
!  Value of a binary operation.

pure elemental real(wp) function binary_value(code, x, y) result(f)
    integer, intent(in) :: code !! the operation code
    real(wp), intent(in) :: x !! the first operand
    real(wp), intent(in) :: y !! the second operand
    select case (code)
    case (op_add); f = x + y
    case (op_sub); f = x - y
    case (op_mul); f = x * y
    case (op_div); f = x / y
    case (op_pow); f = pow(x, y)
    case (op_eq);  f = merge(1.0_wp, 0.0_wp, x == y)
    case (op_ne);  f = merge(1.0_wp, 0.0_wp, x /= y)
    case (op_lt);  f = merge(1.0_wp, 0.0_wp, x <  y)
    case (op_le);  f = merge(1.0_wp, 0.0_wp, x <= y)
    case (op_gt);  f = merge(1.0_wp, 0.0_wp, x >  y)
    case (op_ge);  f = merge(1.0_wp, 0.0_wp, x >= y)
    case (op_and); f = merge(1.0_wp, 0.0_wp, x /= 0.0_wp .and. y /= 0.0_wp)
    case (op_or);  f = merge(1.0_wp, 0.0_wp, x /= 0.0_wp .or. y /= 0.0_wp)
    case (fn_offset + fn_atan2);  f = atan2(x, y)
    case (fn_offset + fn_mod);    f = mod(x, y)
    case (fn_offset + fn_modulo); f = modulo(x, y)
    case (fn_offset + fn_sign);   f = sign(x, y)
    case (fn_offset + fn_hypot);  f = hypot(x, y)
    case default; f = 0.0_wp
    end select
end function binary_value
!*****************************************************************************************

!*****************************************************************************************
!>
!  Partial derivatives `df/dx` and `df/dy` of a binary operation `f(x,y)`.

pure subroutine binary_partials(code, x, y, f, dx, dy)
    integer, intent(in) :: code !! the operation code
    real(wp), intent(in) :: x, y !! the operands
    real(wp), intent(in) :: f !! the value `f(x,y)`
    real(wp), intent(out) :: dx, dy !! the partial derivatives
    real(wp) :: tmp
    select case (code)
    case (op_add); dx = 1.0_wp; dy = 1.0_wp
    case (op_sub); dx = 1.0_wp; dy = -1.0_wp
    case (op_mul); dx = y;      dy = x
    case (op_div); dx = 1.0_wp / y; dy = -f / y
    case (op_pow)
        if (y == 0.0_wp) then
            dx = 0.0_wp
        else
            dx = y * pow(x, y - 1.0_wp)
        end if
        if (x > 0.0_wp) then
            dy = f * log(x)
        else
            dy = 0.0_wp
        end if
    case (fn_offset + fn_atan2)
        tmp = x**2 + y**2
        dx = y / tmp
        dy = -x / tmp
    case (fn_offset + fn_mod)
        dx = 1.0_wp
        dy = -aint(x / y)
    case (fn_offset + fn_modulo)
        dx = 1.0_wp
        dy = -floor_r(x / y)
    case (fn_offset + fn_sign)
        dx = sign(1.0_wp, x) * sign(1.0_wp, y)
        dy = 0.0_wp
    case (fn_offset + fn_hypot)
        if (f > 0.0_wp) then
            dx = x / f
            dy = y / f
        else
            dx = 0.0_wp
            dy = 0.0_wp
        end if
    case default ! relational and logical operators
        dx = 0.0_wp
        dy = 0.0_wp
    end select
end subroutine binary_partials
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a one-argument elemental function `r = f(a)`.

subroutine unary_fn(id, a, ctx, pos, r)

    integer, intent(in) :: id !! function id
    type(val_t), intent(in) :: a !! argument
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(inout) :: r !! result

    integer :: i, n, ierr
    real(wp) :: fp

    n = size(a%v)

    if (ctx%check_domain .and. has_unary_check(id)) then
        do i = 1, n
            ierr = unary_check(id, a%v(i))
            if (ierr /= 0) then
                call ctx%err%set(domain_message(ierr), pos)
                return
            end if
        end do
    end if

    call ensure(r, n, ctx%nd)
    call unary_array(id, a%v, r%v)

    if (ctx%nd > 0) then
        do i = 1, n
            fp = unary_derivative(id, a%v(i), r%v(i))
            r%d(i, :) = fp * a%d(i, :)
        end do
    end if

end subroutine unary_fn
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the one-argument function has a domain check.

pure logical function has_unary_check(id)
    integer, intent(in) :: id !! the function id
    select case (id)
    case (fn_sqrt, fn_log, fn_log10, fn_asin, fn_acos, fn_acosh, fn_atanh)
        has_unary_check = .true.
    case default
        has_unary_check = .false.
    end select
end function has_unary_check
!*****************************************************************************************

!*****************************************************************************************
!>
!  Value of a one-argument elemental function for an array
!  (the function is selected once, outside the loop over the elements).

pure subroutine unary_array(id, x, f)
    integer, intent(in) :: id !! the function id
    real(wp), dimension(:), intent(in) :: x !! the argument
    real(wp), dimension(:), intent(out) :: f !! the function values `f(x)`
    select case (id)
    case (fn_abs);     f = abs(x)
    case (fn_sqrt);    f = sqrt(x)
    case (fn_exp);     f = exp(x)
    case (fn_log);     f = log(x)
    case (fn_log10);   f = log10(x)
    case (fn_sin);     f = sin(x)
    case (fn_cos);     f = cos(x)
    case (fn_tan);     f = tan(x)
    case (fn_asin);    f = asin(x)
    case (fn_acos);    f = acos(x)
    case (fn_atan);    f = atan(x)
    case (fn_sinh);    f = sinh(x)
    case (fn_cosh);    f = cosh(x)
    case (fn_tanh);    f = tanh(x)
    case default;      f = unary_value(id, x)
    end select
end subroutine unary_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  Domain check for a one-argument function. Returns a nonzero error code if there is an error.

pure integer function unary_check(id, x) result(ierr)
    integer, intent(in) :: id !! the function id
    real(wp), intent(in) :: x !! the argument
    ierr = 0
    select case (id)
    case (fn_sqrt);  if (x < 0.0_wp) ierr = err_sqrt
    case (fn_log);   if (x <= 0.0_wp) ierr = err_log
    case (fn_log10); if (x <= 0.0_wp) ierr = err_log10
    case (fn_asin);  if (abs(x) > 1.0_wp) ierr = err_asin
    case (fn_acos);  if (abs(x) > 1.0_wp) ierr = err_acos
    case (fn_acosh); if (x < 1.0_wp) ierr = err_acosh
    case (fn_atanh); if (abs(x) >= 1.0_wp) ierr = err_atanh
    end select
end function unary_check
!*****************************************************************************************

!*****************************************************************************************
!>
!  Value of a one-argument elemental function.

pure elemental real(wp) function unary_value(id, x) result(f)
    integer, intent(in) :: id !! the function id
    real(wp), intent(in) :: x !! the argument
    select case (id)
    case (fn_abs);     f = abs(x)
    case (fn_sqrt);    f = sqrt(x)
    case (fn_exp);     f = exp(x)
    case (fn_log);     f = log(x)
    case (fn_log10);   f = log10(x)
    case (fn_sin);     f = sin(x)
    case (fn_cos);     f = cos(x)
    case (fn_tan);     f = tan(x)
    case (fn_asin);    f = asin(x)
    case (fn_acos);    f = acos(x)
    case (fn_atan);    f = atan(x)
    case (fn_sinh);    f = sinh(x)
    case (fn_cosh);    f = cosh(x)
    case (fn_tanh);    f = tanh(x)
    case (fn_asinh);   f = asinh(x)
    case (fn_acosh);   f = acosh(x)
    case (fn_atanh);   f = atanh(x)
    case (fn_erf);     f = erf(x)
    case (fn_erfc);    f = erfc(x)
    case (fn_floor);   f = floor_r(x)
    case (fn_ceiling); f = -floor_r(-x)
    case (fn_nint);    f = anint(x)
    case (fn_aint);    f = aint(x)
    case default;      f = 0.0_wp
    end select
end function unary_value
!*****************************************************************************************

!*****************************************************************************************
!>
!  Derivative of a one-argument elemental function.

pure real(wp) function unary_derivative(id, x, f) result(fp)
    integer, intent(in) :: id !! the function id
    real(wp), intent(in) :: x !! the argument
    real(wp), intent(in) :: f !! the function value `f(x)`
    select case (id)
    case (fn_abs);   fp = sign(1.0_wp, x)
    case (fn_sqrt);  fp = 0.5_wp / f
    case (fn_exp);   fp = f
    case (fn_log);   fp = 1.0_wp / x
    case (fn_log10); fp = 1.0_wp / (x * log(10.0_wp))
    case (fn_sin);   fp = cos(x)
    case (fn_cos);   fp = -sin(x)
    case (fn_tan);   fp = 1.0_wp + f**2
    case (fn_asin);  fp = 1.0_wp / sqrt(1.0_wp - x**2)
    case (fn_acos);  fp = -1.0_wp / sqrt(1.0_wp - x**2)
    case (fn_atan);  fp = 1.0_wp / (1.0_wp + x**2)
    case (fn_sinh);  fp = cosh(x)
    case (fn_cosh);  fp = sinh(x)
    case (fn_tanh);  fp = 1.0_wp - f**2
    case (fn_asinh); fp = 1.0_wp / sqrt(x**2 + 1.0_wp)
    case (fn_acosh); fp = 1.0_wp / sqrt(x**2 - 1.0_wp)
    case (fn_atanh); fp = 1.0_wp / (1.0_wp - x**2)
    case (fn_erf);   fp = 2.0_wp / sqrt(pi) * exp(-x**2)
    case (fn_erfc);  fp = -2.0_wp / sqrt(pi) * exp(-x**2)
    case default;    fp = 0.0_wp ! piecewise constant functions
    end select
end function unary_derivative
!*****************************************************************************************

!*****************************************************************************************
!>
!  Error message for a domain error code.

pure function domain_message(ierr) result(msg)
    integer, intent(in) :: ierr !! the domain error code
    character(len=:), allocatable :: msg !! the error message
    select case (ierr)
    case (err_div_zero); msg = 'division by zero'
    case (err_pow_zero); msg = 'division by zero (zero raised to a negative power)'
    case (err_pow_neg);  msg = 'negative number raised to a non-integer power'
    case (err_atan2);    msg = 'atan2 arguments are both zero'
    case (err_mod_zero); msg = 'division by zero in mod/modulo'
    case (err_sqrt);     msg = 'sqrt of a negative number'
    case (err_log);      msg = 'log of a non-positive number'
    case (err_log10);    msg = 'log10 of a non-positive number'
    case (err_asin);     msg = 'asin argument outside [-1,1]'
    case (err_acos);     msg = 'acos argument outside [-1,1]'
    case (err_acosh);    msg = 'acosh argument less than 1'
    case (err_atanh);    msg = 'atanh argument outside (-1,1)'
    case default;        msg = 'domain error'
    end select
end function domain_message
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a user-defined function. If derivatives are requested and the
!  function does not have a derivative routine, central finite differences are used.

recursive subroutine eval_user(tree, inode, vars, funcs, ctx, dref)

    type(expr_tree), intent(inout) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), dimension(:), intent(in) :: dref !! derivatives of the referenced variables (see [[eval_tree]])

    type(fparse_value), dimension(:), allocatable :: uargs
    real(wp), dimension(:), allocatable :: res
    real(wp), dimension(:,:), allocatable :: jac
    integer :: k, nargs, ifunc, stat, m, pos, c

    ifunc = tree%nodes(inode)%op
    pos = tree%nodes(inode)%pos
    nargs = size(tree%nodes(inode)%args)
    allocate(uargs(nargs))
    do k = 1, nargs
        c = tree%nodes(inode)%args(k)
        call eval_node(tree, c, vars, funcs, ctx, dref)
        if (ctx%err%failed) return
        uargs(k)%v = tree%buf(c)%v
    end do

    associate (f => funcs(ifunc), r => tree%buf(inode))

        stat = 0
        call f%f(uargs, res, stat)
        if (stat /= 0) then
            call ctx%err%set('function "'//f%name//'" returned error code '//int_to_str(stat), pos)
            return
        end if
        if (.not. allocated(res)) then
            call ctx%err%set('function "'//f%name//'" did not return a result', pos)
            return
        end if
        m = size(res)
        call ensure(r, m, ctx%nd)
        r%v = res

        if (ctx%nd > 0) then
            r%d = 0.0_wp
            do k = 1, nargs
                associate (arg => tree%buf(tree%nodes(inode)%args(k)))
                    if (all(arg%d == 0.0_wp)) cycle ! no contribution
                    stat = 0
                    if (associated(f%df)) then
                        call f%df(uargs, k, jac, stat)
                        if (stat /= 0) then
                            call ctx%err%set('derivative of function "'//f%name//&
                                             '" returned error code '//int_to_str(stat), pos)
                            return
                        end if
                    else
                        call fd_jacobian(f, uargs, k, m, jac, stat)
                        if (stat /= 0) then
                            call ctx%err%set('error computing finite-difference derivative of function "'//&
                                             f%name//'"', pos)
                            return
                        end if
                    end if
                    if (.not. allocated(jac)) then
                        call ctx%err%set('derivative of function "'//f%name//'" did not return a result', pos)
                        return
                    end if
                    if (size(jac, 1) /= m .or. size(jac, 2) /= size(uargs(k)%v)) then
                        call ctx%err%set('derivative of function "'//f%name//&
                                         '" returned a jacobian with the wrong shape', pos)
                        return
                    end if
                    r%d = r%d + matmul(jac, arg%d)
                end associate
            end do
        end if

    end associate

end subroutine eval_user
!*****************************************************************************************

!*****************************************************************************************
!>
!  Compute the jacobian of a user function with respect to one of
!  its arguments using central finite differences.

subroutine fd_jacobian(f, args, iarg, m, jac, stat)

    type(user_function), intent(in) :: f !! the function
    type(fparse_value), dimension(:), intent(in) :: args !! the function arguments
    integer, intent(in) :: iarg !! the argument to differentiate with respect to
    integer, intent(in) :: m !! size of the function result
    real(wp), dimension(:,:), allocatable, intent(out) :: jac !! the jacobian `[m, size(args(iarg)%v)]`
    integer, intent(out) :: stat !! status code (nonzero if there was an error)

    type(fparse_value), dimension(:), allocatable :: work
    real(wp), dimension(:), allocatable :: rp, rm
    real(wp) :: h, x
    integer :: j, n

    real(wp), parameter :: h_scale = epsilon(1.0_wp)**(1.0_wp/3.0_wp)

    n = size(args(iarg)%v)
    allocate(jac(m, n))
    work = args
    stat = 0
    do j = 1, n
        x = args(iarg)%v(j)
        h = h_scale * max(1.0_wp, abs(x))
        work(iarg)%v(j) = x + h
        call f%f(work, rp, stat)
        if (stat /= 0) return
        work(iarg)%v(j) = x - h
        call f%f(work, rm, stat)
        if (stat /= 0) return
        work(iarg)%v(j) = x
        if (.not. allocated(rp) .or. .not. allocated(rm)) then
            stat = -1
            return
        end if
        if (size(rp) /= m .or. size(rm) /= m) then
            stat = -1
            return
        end if
        jac(:, j) = (rp - rm) / (2.0_wp * h)
    end do

end subroutine fd_jacobian
!*****************************************************************************************

!*****************************************************************************************
!>
!  Make sure a value buffer has size `m` (and derivative shape `[m, nd]` if `nd>0`).
!  It is only reallocated if the size has changed.

pure subroutine ensure(r, m, nd)

    type(val_t), intent(inout) :: r !! the value buffer
    integer, intent(in) :: m !! required size
    integer, intent(in) :: nd !! number of derivative columns

    if (allocated(r%v)) then
        if (size(r%v) /= m) then
            deallocate(r%v)
            allocate(r%v(m))
        end if
    else
        allocate(r%v(m))
    end if
    if (nd > 0) then
        if (allocated(r%d)) then
            if (size(r%d, 1) /= m .or. size(r%d, 2) /= nd) then
                deallocate(r%d)
                allocate(r%d(m, nd))
            end if
        else
            allocate(r%d(m, nd))
        end if
    end if

end subroutine ensure
!*****************************************************************************************

!*****************************************************************************************
!>
!  Copy a value (and its derivative if `nd>0`) into a buffer.

pure subroutine copy_val(src, dst, nd)

    type(val_t), intent(in) :: src !! the value to copy
    type(val_t), intent(inout) :: dst !! the buffer to copy into
    integer, intent(in) :: nd !! number of derivative columns (0 for no derivatives)

    call ensure(dst, size(src%v), nd)
    dst%v = src%v
    if (nd > 0) dst%d = src%d

end subroutine copy_val
!*****************************************************************************************

!*****************************************************************************************
!>
!  Scalar power function. Uses an integer power if the exponent is integer-valued,
!  so that negative bases are allowed (e.g., `(-2)^3 = -8`).

pure elemental real(wp) function pow(a, b)
    real(wp), intent(in) :: a !! the base
    real(wp), intent(in) :: b !! the exponent
    if (b == 2.0_wp) then
        pow = a * a
    else if (is_integer(b)) then
        pow = a**nint(b)
    else
        pow = a**b
    end if
end function pow
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if `x` is an integer value that fits in a default integer.

pure elemental logical function is_integer(x)
    real(wp), intent(in) :: x !! the number to check
    is_integer = abs(x) < real(huge(1), wp)
    if (is_integer) is_integer = x == aint(x)
end function is_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Floor function returning a real (no integer overflow for large values).

pure elemental real(wp) function floor_r(x)
    real(wp), intent(in) :: x !! the number
    floor_r = aint(x)
    if (floor_r > x) floor_r = floor_r - 1.0_wp
end function floor_r
!*****************************************************************************************

!*****************************************************************************************
!>
!  Cross product of two 3-element vectors.

pure function cross3(a, b) result(c)
    real(wp), dimension(3), intent(in) :: a !! the first vector
    real(wp), dimension(3), intent(in) :: b !! the second vector
    real(wp), dimension(3) :: c !! the cross product `a x b`
    c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]
end function cross3
!*****************************************************************************************

!*****************************************************************************************
end module fparse_eval
!*****************************************************************************************
