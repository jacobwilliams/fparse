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

    module fparse_eval

    use fparse_common
    use fparse_functions
    use fparse_parser

    implicit none

    private

    real(wp), parameter :: pi = acos(-1.0_wp)

    type, public :: val_t
        !! A value (and optionally its derivative).
        real(wp), dimension(:), allocatable :: v !! the value
        real(wp), dimension(:,:), allocatable :: d !! the derivative `[size(v), nd]`
                                                    !! (only allocated if `nd>0`)
    end type val_t

    type, public :: eval_ctx
        !! Evaluation context.
        integer :: nd = 0 !! number of derivative columns (0 for no derivatives)
        logical :: check_domain = .true. !! check for domain errors (e.g., `log` of a
                                         !! negative number, division by zero)
        type(error_t) :: err !! error status
    end type eval_ctx

    public :: eval_tree

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate an expression tree.

    subroutine eval_tree(tree, vars, funcs, ctx, res)

    type(expr_tree), intent(in) :: tree !! the expression tree
    type(val_t), dimension(:), intent(in) :: vars !! variable values (and derivatives if `nd>0`)
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), intent(out) :: res !! the result

    call eval_node(tree, tree%root, vars, funcs, ctx, res)

    end subroutine eval_tree
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a node of an expression tree (recursively).

    recursive subroutine eval_node(tree, inode, vars, funcs, ctx, res)

    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), intent(out) :: res !! the result

    type(val_t) :: a, b
    type(val_t), dimension(:), allocatable :: items
    integer :: nd, k, i, m, n

    nd = ctx%nd

    associate (node => tree%nodes(inode))

    select case (node%kind)

    case (node_const)
        call make_const(res, [node%value], nd)

    case (node_var)
        res = vars(node%op)

    case (node_unary)
        call eval_node(tree, node%args(1), vars, funcs, ctx, a)
        if (ctx%err%failed) return
        select case (node%op)
        case (op_neg)
            res%v = -a%v
            if (nd > 0) res%d = -a%d
        case (op_not)
            call make_const(res, merge(1.0_wp, 0.0_wp, a%v == 0.0_wp), nd)
        end select

    case (node_binary)
        call eval_node(tree, node%args(1), vars, funcs, ctx, a)
        if (ctx%err%failed) return
        call eval_node(tree, node%args(2), vars, funcs, ctx, b)
        if (ctx%err%failed) return
        call conform(a, b, ctx, node%pos)
        if (ctx%err%failed) return
        call binary_op(node%op, a, b, ctx, node%pos, res)

    case (node_builtin)
        call eval_builtin(tree, inode, vars, funcs, ctx, res)

    case (node_user)
        call eval_user(tree, inode, vars, funcs, ctx, res)

    case (node_vector)
        allocate(items(size(node%args)))
        n = 0
        do k = 1, size(items)
            call eval_node(tree, node%args(k), vars, funcs, ctx, items(k))
            if (ctx%err%failed) return
            n = n + size(items(k)%v)
        end do
        allocate(res%v(n))
        if (nd > 0) allocate(res%d(n, nd))
        i = 0
        do k = 1, size(items)
            m = size(items(k)%v)
            res%v(i+1:i+m) = items(k)%v
            if (nd > 0) res%d(i+1:i+m, :) = items(k)%d
            i = i + m
        end do

    case (node_index)
        call eval_node(tree, node%args(1), vars, funcs, ctx, a)
        if (ctx%err%failed) return
        call eval_node(tree, node%args(2), vars, funcs, ctx, b)
        if (ctx%err%failed) return
        if (size(b%v) /= 1) then
            call ctx%err%set('index must be a scalar', node%pos)
            return
        end if
        if (b%v(1) /= aint(b%v(1)) .or. abs(b%v(1)) > real(huge(1), wp)) then
            call ctx%err%set('index must be an integer', node%pos)
            return
        end if
        k = nint(b%v(1))
        if (k < 1 .or. k > size(a%v)) then
            call ctx%err%set('index '//int_to_str(k)//' is out of bounds (size is '//&
                             int_to_str(size(a%v))//')', node%pos)
            return
        end if
        res%v = a%v(k:k)
        if (nd > 0) res%d = a%d(k:k, :)

    case default
        call ctx%err%set('invalid expression tree', node%pos)
    end select

    end associate

    end subroutine eval_node
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a binary operator. The operands must already have conforming sizes.

    subroutine binary_op(op, a, b, ctx, pos, res)

    integer, intent(in) :: op !! the operator
    type(val_t), intent(in) :: a !! left operand
    type(val_t), intent(in) :: b !! right operand
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(out) :: res !! result

    integer :: nd

    nd = ctx%nd

    select case (op)
    case (op_add)
        res%v = a%v + b%v
        if (nd > 0) res%d = a%d + b%d
    case (op_sub)
        res%v = a%v - b%v
        if (nd > 0) res%d = a%d - b%d
    case (op_mul)
        res%v = a%v * b%v
        if (nd > 0) res%d = scale_rows(a%d, b%v) + scale_rows(b%d, a%v)
    case (op_div)
        if (ctx%check_domain .and. any(b%v == 0.0_wp)) then
            call ctx%err%set('division by zero', pos)
            return
        end if
        res%v = a%v / b%v
        if (nd > 0) res%d = scale_rows(a%d - scale_rows(b%d, res%v), 1.0_wp / b%v)
    case (op_pow)
        call power(a, b, ctx, pos, res)
    case (op_eq); call make_const(res, merge(1.0_wp, 0.0_wp, a%v == b%v), nd)
    case (op_ne); call make_const(res, merge(1.0_wp, 0.0_wp, a%v /= b%v), nd)
    case (op_lt); call make_const(res, merge(1.0_wp, 0.0_wp, a%v <  b%v), nd)
    case (op_le); call make_const(res, merge(1.0_wp, 0.0_wp, a%v <= b%v), nd)
    case (op_gt); call make_const(res, merge(1.0_wp, 0.0_wp, a%v >  b%v), nd)
    case (op_ge); call make_const(res, merge(1.0_wp, 0.0_wp, a%v >= b%v), nd)
    case (op_and)
        call make_const(res, merge(1.0_wp, 0.0_wp, a%v /= 0.0_wp .and. b%v /= 0.0_wp), nd)
    case (op_or)
        call make_const(res, merge(1.0_wp, 0.0_wp, a%v /= 0.0_wp .or. b%v /= 0.0_wp), nd)
    case default
        call ctx%err%set('invalid operator', pos)
    end select

    end subroutine binary_op
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate `a^b`. The operands must already have conforming sizes.
!  Integer-valued exponents are computed using integer powers, so that
!  negative bases are allowed (e.g., `(-2)^3 = -8`).

    subroutine power(a, b, ctx, pos, res)

    type(val_t), intent(in) :: a !! base
    type(val_t), intent(in) :: b !! exponent
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(out) :: res !! result

    integer :: i, m, nd
    real(wp) :: da, db

    nd = ctx%nd
    m = size(a%v)

    if (ctx%check_domain) then
        do i = 1, m
            if (a%v(i) == 0.0_wp .and. b%v(i) < 0.0_wp) then
                call ctx%err%set('division by zero (zero raised to a negative power)', pos)
                return
            else if (a%v(i) < 0.0_wp .and. .not. is_integer(b%v(i))) then
                call ctx%err%set('negative number raised to a non-integer power', pos)
                return
            end if
        end do
    end if

    allocate(res%v(m))
    do i = 1, m
        res%v(i) = pow(a%v(i), b%v(i))
    end do

    if (nd > 0) then
        allocate(res%d(m, nd))
        do i = 1, m
            if (b%v(i) == 0.0_wp) then
                da = 0.0_wp
            else
                da = b%v(i) * pow(a%v(i), b%v(i) - 1.0_wp)
            end if
            if (a%v(i) > 0.0_wp) then
                db = res%v(i) * log(a%v(i))
            else
                db = 0.0_wp
            end if
            res%d(i, :) = da * a%d(i, :) + db * b%d(i, :)
        end do
    end if

    end subroutine power
!*****************************************************************************************

!*****************************************************************************************
!>
!  Scalar power function. Uses an integer power if the exponent is integer-valued.

    pure elemental real(wp) function pow(a, b)
    real(wp), intent(in) :: a, b
    if (is_integer(b)) then
        pow = a ** nint(b)
    else
        pow = a ** b
    end if
    end function pow
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if `x` is an integer value that fits in a default integer.

    pure elemental logical function is_integer(x)
    real(wp), intent(in) :: x
    is_integer = abs(x) < real(huge(1), wp)
    if (is_integer) is_integer = x == aint(x)
    end function is_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a built-in function.

    recursive subroutine eval_builtin(tree, inode, vars, funcs, ctx, res)

    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), intent(out) :: res !! the result

    type(val_t) :: a, b, c
    integer :: id, nd, k, i, m, n, pos
    real(wp) :: r
    real(wp), dimension(:), allocatable :: pre, suf

    nd = ctx%nd

    associate (node => tree%nodes(inode))

    id = node%op
    pos = node%pos

    select case (id)

    case (fn_if)
        ! if(a, b, cond): only the required branch is evaluated when cond is a scalar
        call eval_node(tree, node%args(3), vars, funcs, ctx, c)
        if (ctx%err%failed) return
        if (size(c%v) == 1) then
            if (c%v(1) /= 0.0_wp) then
                call eval_node(tree, node%args(1), vars, funcs, ctx, res)
            else
                call eval_node(tree, node%args(2), vars, funcs, ctx, res)
            end if
        else
            m = size(c%v)
            call eval_node(tree, node%args(1), vars, funcs, ctx, a)
            if (ctx%err%failed) return
            call eval_node(tree, node%args(2), vars, funcs, ctx, b)
            if (ctx%err%failed) return
            call expand(a, m, nd)
            call expand(b, m, nd)
            if (size(a%v) /= m .or. size(b%v) /= m) then
                call ctx%err%set('incompatible sizes in if: condition has size '//int_to_str(m)//&
                                 ', values have sizes '//int_to_str(size(a%v))//' and '//&
                                 int_to_str(size(b%v)), pos)
                return
            end if
            res%v = merge(a%v, b%v, c%v /= 0.0_wp)
            if (nd > 0) then
                allocate(res%d(m, nd))
                do i = 1, m
                    if (c%v(i) /= 0.0_wp) then
                        res%d(i, :) = a%d(i, :)
                    else
                        res%d(i, :) = b%d(i, :)
                    end if
                end do
            end if
        end if
        return

    case (fn_max, fn_min)
        call eval_node(tree, node%args(1), vars, funcs, ctx, res)
        if (ctx%err%failed) return
        do k = 2, size(node%args)
            call eval_node(tree, node%args(k), vars, funcs, ctx, b)
            if (ctx%err%failed) return
            call conform(res, b, ctx, pos)
            if (ctx%err%failed) return
            do i = 1, size(res%v)
                if ((id == fn_max .and. b%v(i) > res%v(i)) .or. &
                    (id == fn_min .and. b%v(i) < res%v(i))) then
                    res%v(i) = b%v(i)
                    if (nd > 0) res%d(i, :) = b%d(i, :)
                end if
            end do
        end do
        return

    end select

    ! all other functions: evaluate the arguments first
    call eval_node(tree, node%args(1), vars, funcs, ctx, a)
    if (ctx%err%failed) return
    if (size(node%args) > 1) then
        call eval_node(tree, node%args(2), vars, funcs, ctx, b)
        if (ctx%err%failed) return
    end if

    end associate

    if (id <= fn_last_elemental) then
        call elemental_1(id, a, ctx, pos, res)
        return
    end if

    n = size(a%v)

    select case (id)

    case (fn_atan2, fn_mod, fn_modulo, fn_sign, fn_hypot)
        call conform(a, b, ctx, pos)
        if (ctx%err%failed) return
        call elemental_2(id, a, b, ctx, pos, res)

    case (fn_sum)
        res%v = [sum(a%v)]
        if (nd > 0) res%d = reshape(sum(a%d, dim=1), [1, nd])

    case (fn_product)
        res%v = [product(a%v)]
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
            allocate(res%d(1, nd), source=0.0_wp)
            do i = 1, n
                res%d(1, :) = res%d(1, :) + pre(i-1) * suf(i+1) * a%d(i, :)
            end do
        end if

    case (fn_norm2)
        r = norm2(a%v)
        res%v = [r]
        if (nd > 0) then
            allocate(res%d(1, nd), source=0.0_wp)
            if (r > 0.0_wp) res%d(1, :) = matmul(a%v, a%d) / r
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
        res%v = a%v(k:k)
        if (nd > 0) res%d = a%d(k:k, :)

    case (fn_size)
        call make_const(res, [real(n, wp)], nd)

    case (fn_dot)
        if (n /= size(b%v)) then
            call ctx%err%set('arguments of dot_product have different sizes ('//&
                             int_to_str(n)//' and '//int_to_str(size(b%v))//')', pos)
            return
        end if
        res%v = [dot_product(a%v, b%v)]
        if (nd > 0) res%d = reshape(matmul(b%v, a%d) + matmul(a%v, b%d), [1, nd])

    case (fn_cross)
        if (n /= 3 .or. size(b%v) /= 3) then
            call ctx%err%set('arguments of cross must have size 3 (sizes are '//&
                             int_to_str(n)//' and '//int_to_str(size(b%v))//')', pos)
            return
        end if
        res%v = cross3(a%v, b%v)
        if (nd > 0) then
            allocate(res%d(3, nd))
            do k = 1, nd
                res%d(:, k) = cross3(a%d(:, k), b%v) + cross3(a%v, b%d(:, k))
            end do
        end if

    case default
        call ctx%err%set('invalid function', pos)
    end select

    end subroutine eval_builtin
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a one-argument elemental function.

    subroutine elemental_1(id, a, ctx, pos, res)

    integer, intent(in) :: id !! function id
    type(val_t), intent(in) :: a !! argument
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(out) :: res !! result

    real(wp), dimension(:), allocatable :: fp !! derivative of the function

    if (ctx%check_domain) then
        select case (id)
        case (fn_sqrt)
            if (any(a%v < 0.0_wp)) call ctx%err%set('sqrt of a negative number', pos)
        case (fn_log)
            if (any(a%v <= 0.0_wp)) call ctx%err%set('log of a non-positive number', pos)
        case (fn_log10)
            if (any(a%v <= 0.0_wp)) call ctx%err%set('log10 of a non-positive number', pos)
        case (fn_asin)
            if (any(abs(a%v) > 1.0_wp)) call ctx%err%set('asin argument outside [-1,1]', pos)
        case (fn_acos)
            if (any(abs(a%v) > 1.0_wp)) call ctx%err%set('acos argument outside [-1,1]', pos)
        case (fn_acosh)
            if (any(a%v < 1.0_wp)) call ctx%err%set('acosh argument less than 1', pos)
        case (fn_atanh)
            if (any(abs(a%v) >= 1.0_wp)) call ctx%err%set('atanh argument outside (-1,1)', pos)
        end select
        if (ctx%err%failed) return
    end if

    select case (id)
    case (fn_abs);     res%v = abs(a%v)
    case (fn_sqrt);    res%v = sqrt(a%v)
    case (fn_exp);     res%v = exp(a%v)
    case (fn_log);     res%v = log(a%v)
    case (fn_log10);   res%v = log10(a%v)
    case (fn_sin);     res%v = sin(a%v)
    case (fn_cos);     res%v = cos(a%v)
    case (fn_tan);     res%v = tan(a%v)
    case (fn_asin);    res%v = asin(a%v)
    case (fn_acos);    res%v = acos(a%v)
    case (fn_atan);    res%v = atan(a%v)
    case (fn_sinh);    res%v = sinh(a%v)
    case (fn_cosh);    res%v = cosh(a%v)
    case (fn_tanh);    res%v = tanh(a%v)
    case (fn_asinh);   res%v = asinh(a%v)
    case (fn_acosh);   res%v = acosh(a%v)
    case (fn_atanh);   res%v = atanh(a%v)
    case (fn_erf);     res%v = erf(a%v)
    case (fn_erfc);    res%v = erfc(a%v)
    case (fn_floor);   res%v = floor_r(a%v)
    case (fn_ceiling); res%v = -floor_r(-a%v)
    case (fn_nint);    res%v = anint(a%v)
    case (fn_aint);    res%v = aint(a%v)
    end select

    if (ctx%nd > 0) then
        select case (id)
        case (fn_abs);     fp = sign(1.0_wp, a%v)
        case (fn_sqrt);    fp = 0.5_wp / res%v
        case (fn_exp);     fp = res%v
        case (fn_log);     fp = 1.0_wp / a%v
        case (fn_log10);   fp = 1.0_wp / (a%v * log(10.0_wp))
        case (fn_sin);     fp = cos(a%v)
        case (fn_cos);     fp = -sin(a%v)
        case (fn_tan);     fp = 1.0_wp + res%v**2
        case (fn_asin);    fp = 1.0_wp / sqrt(1.0_wp - a%v**2)
        case (fn_acos);    fp = -1.0_wp / sqrt(1.0_wp - a%v**2)
        case (fn_atan);    fp = 1.0_wp / (1.0_wp + a%v**2)
        case (fn_sinh);    fp = cosh(a%v)
        case (fn_cosh);    fp = sinh(a%v)
        case (fn_tanh);    fp = 1.0_wp - res%v**2
        case (fn_asinh);   fp = 1.0_wp / sqrt(a%v**2 + 1.0_wp)
        case (fn_acosh);   fp = 1.0_wp / sqrt(a%v**2 - 1.0_wp)
        case (fn_atanh);   fp = 1.0_wp / (1.0_wp - a%v**2)
        case (fn_erf);     fp = 2.0_wp / sqrt(pi) * exp(-a%v**2)
        case (fn_erfc);    fp = -2.0_wp / sqrt(pi) * exp(-a%v**2)
        case default;      fp = spread(0.0_wp, 1, size(a%v)) ! piecewise constant functions
        end select
        res%d = scale_rows(a%d, fp)
    end if

    end subroutine elemental_1
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a two-argument elemental function.
!  The arguments must already have conforming sizes.

    subroutine elemental_2(id, a, b, ctx, pos, res)

    integer, intent(in) :: id !! function id
    type(val_t), intent(in) :: a !! first argument
    type(val_t), intent(in) :: b !! second argument
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression
    type(val_t), intent(out) :: res !! result

    real(wp), dimension(:), allocatable :: tmp
    integer :: nd

    nd = ctx%nd

    select case (id)
    case (fn_atan2)
        if (ctx%check_domain .and. any(a%v == 0.0_wp .and. b%v == 0.0_wp)) then
            call ctx%err%set('atan2 arguments are both zero', pos)
            return
        end if
        res%v = atan2(a%v, b%v)
        if (nd > 0) then
            tmp = 1.0_wp / (a%v**2 + b%v**2)
            res%d = scale_rows(scale_rows(a%d, b%v) - scale_rows(b%d, a%v), tmp)
        end if
    case (fn_mod, fn_modulo)
        if (ctx%check_domain .and. any(b%v == 0.0_wp)) then
            call ctx%err%set('division by zero in mod/modulo', pos)
            return
        end if
        if (id == fn_mod) then
            res%v = mod(a%v, b%v)
            if (nd > 0) tmp = aint(a%v / b%v)
        else
            res%v = modulo(a%v, b%v)
            if (nd > 0) tmp = floor_r(a%v / b%v)
        end if
        if (nd > 0) res%d = a%d - scale_rows(b%d, tmp)
    case (fn_sign)
        res%v = sign(a%v, b%v)
        if (nd > 0) res%d = scale_rows(a%d, sign(1.0_wp, a%v) * sign(1.0_wp, b%v))
    case (fn_hypot)
        res%v = hypot(a%v, b%v)
        if (nd > 0) then
            tmp = merge(1.0_wp / res%v, 0.0_wp, res%v > 0.0_wp)
            res%d = scale_rows(scale_rows(a%d, a%v) + scale_rows(b%d, b%v), tmp)
        end if
    case default
        call ctx%err%set('invalid function', pos)
    end select

    end subroutine elemental_2
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate a user-defined function. If derivatives are requested and the
!  function does not have a derivative routine, central finite differences are used.

    recursive subroutine eval_user(tree, inode, vars, funcs, ctx, res)

    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: inode !! the node to evaluate
    type(val_t), dimension(:), intent(in) :: vars !! variable values
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), intent(out) :: res !! the result

    type(val_t), dimension(:), allocatable :: argv
    type(fparse_value), dimension(:), allocatable :: uargs
    real(wp), dimension(:), allocatable :: r
    real(wp), dimension(:,:), allocatable :: jac
    integer :: k, nargs, ifunc, stat, m, pos

    associate (node => tree%nodes(inode))

    ifunc = node%op
    pos = node%pos
    nargs = size(node%args)
    allocate(argv(nargs), uargs(nargs))
    do k = 1, nargs
        call eval_node(tree, node%args(k), vars, funcs, ctx, argv(k))
        if (ctx%err%failed) return
        uargs(k)%v = argv(k)%v
    end do

    end associate

    associate (f => funcs(ifunc))

    stat = 0
    call f%f(uargs, r, stat)
    if (stat /= 0) then
        call ctx%err%set('function "'//f%name//'" returned error code '//int_to_str(stat), pos)
        return
    end if
    if (.not. allocated(r)) then
        call ctx%err%set('function "'//f%name//'" did not return a result', pos)
        return
    end if
    m = size(r)
    call move_alloc(r, res%v)

    if (ctx%nd > 0) then
        allocate(res%d(m, ctx%nd), source=0.0_wp)
        do k = 1, nargs
            if (all(argv(k)%d == 0.0_wp)) cycle ! no contribution
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
            res%d = res%d + matmul(jac, argv(k)%d)
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
!  Create a constant value (with a zero derivative if `nd>0`).

    pure subroutine make_const(res, v, nd)

    type(val_t), intent(out) :: res !! the result
    real(wp), dimension(:), intent(in) :: v !! the value
    integer, intent(in) :: nd !! number of derivative columns

    res%v = v
    if (nd > 0) allocate(res%d(size(v), nd), source=0.0_wp)

    end subroutine make_const
!*****************************************************************************************

!*****************************************************************************************
!>
!  If the value has size 1, broadcast it to size `m`.

    pure subroutine expand(a, m, nd)

    type(val_t), intent(inout) :: a !! the value
    integer, intent(in) :: m !! the new size
    integer, intent(in) :: nd !! number of derivative columns

    real(wp) :: s
    real(wp), dimension(:), allocatable :: row

    if (size(a%v) /= 1 .or. m == 1) return
    s = a%v(1)
    a%v = spread(s, 1, m)
    if (nd > 0) then
        row = a%d(1, :)
        a%d = spread(row, 1, m)
    end if

    end subroutine expand
!*****************************************************************************************

!*****************************************************************************************
!>
!  Make the sizes of two values conform: a size-1 value is broadcast to
!  the size of the other. Otherwise the sizes must be equal.

    subroutine conform(a, b, ctx, pos)

    type(val_t), intent(inout) :: a !! first value
    type(val_t), intent(inout) :: b !! second value
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    integer, intent(in) :: pos !! position in the expression

    integer :: na, nb

    na = size(a%v)
    nb = size(b%v)
    if (na == nb) return
    if (na == 1) then
        call expand(a, nb, ctx%nd)
    else if (nb == 1) then
        call expand(b, na, ctx%nd)
    else
        call ctx%err%set('incompatible sizes ('//int_to_str(na)//' and '//int_to_str(nb)//')', pos)
    end if

    end subroutine conform
!*****************************************************************************************

!*****************************************************************************************
!>
!  Multiply each row `i` of the matrix `d` by `f(i)`.

    pure function scale_rows(d, f) result(r)

    real(wp), dimension(:,:), intent(in) :: d
    real(wp), dimension(:), intent(in) :: f
    real(wp), dimension(size(d,1), size(d,2)) :: r

    integer :: j

    do j = 1, size(d, 2)
        r(:, j) = d(:, j) * f
    end do

    end function scale_rows
!*****************************************************************************************

!*****************************************************************************************
!>
!  Floor function returning a real (no integer overflow for large values).

    pure elemental real(wp) function floor_r(x)
    real(wp), intent(in) :: x
    floor_r = aint(x)
    if (floor_r > x) floor_r = floor_r - 1.0_wp
    end function floor_r
!*****************************************************************************************

!*****************************************************************************************
!>
!  Cross product of two 3-element vectors.

    pure function cross3(a, b) result(c)
    real(wp), dimension(3), intent(in) :: a, b
    real(wp), dimension(3) :: c
    c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]
    end function cross3
!*****************************************************************************************

!*****************************************************************************************
    end module fparse_eval
!*****************************************************************************************
