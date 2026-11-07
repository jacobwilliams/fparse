!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  A fast evaluator for the (common) case where all the values are scalars and
!  no derivatives are needed.
!
!  An expression tree is compiled into a flat list of instructions that operate
!  on an array of scalar "slots" (one per tree node, with the constants preloaded).
!  The general tree evaluator in [[fparse_eval]] is used whenever this does not
!  apply (vector values, derivatives, user-defined functions, etc.). Both give
!  the same results and the same error messages.

    module fparse_bytecode

    use fparse_common
    use fparse_functions
    use fparse_parser
    use fparse_eval, only: eval_ctx, fn_offset, binary_value, binary_check, has_domain_check, &
                           unary_value, unary_check, domain_message, pow, &
                           binary_partials, unary_derivative

    implicit none

    private

    ! instruction codes. The binary operators use the `op_*` codes,
    ! the two-argument functions use `fn_offset + fn_*`, and the
    ! one-argument functions use `unary_offset + fn_*`.
    integer, parameter :: unary_offset = 300
    integer, parameter :: ins_var  = 401 !! `s(o) = vars(a)%v(1)`
    integer, parameter :: ins_neg  = 402 !! `s(o) = -s(a)`
    integer, parameter :: ins_not  = 403 !! `s(o) = .not. s(a)`
    integer, parameter :: ins_move = 404 !! `s(o) = s(a)`
    integer, parameter :: ins_jz   = 405 !! if `s(a) == 0`, jump to instruction `o`
    integer, parameter :: ins_jmp  = 406 !! jump to instruction `o`

    type, public :: scalar_program
        !! A compiled scalar expression.
        logical :: ok = .false. !! true if the expression could be compiled
        integer :: n = 0 !! number of instructions
        integer :: root = 0 !! slot containing the result
        integer, dimension(:), allocatable :: op !! instruction codes
        integer, dimension(:), allocatable :: a !! first operand slot (or variable index)
        integer, dimension(:), allocatable :: b !! second operand slot
        integer, dimension(:), allocatable :: o !! output slot (or jump target)
        integer, dimension(:), allocatable :: pos !! position in the expression (for error messages)
        real(wp), dimension(:), allocatable :: s !! the slots
        real(wp), dimension(:,:), allocatable :: sd !! the derivatives of the slots `[nd, n_slots]`
                                                    !! (only used by [[exec_scalar_d]])
    end type scalar_program

    public :: compile_scalar, exec_scalar, exec_scalar_d

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Compile an expression tree into a scalar program. If the expression contains
!  something that can't be evaluated by the scalar program, `prog%ok` is false.

    subroutine compile_scalar(tree, prog)

    type(expr_tree), intent(in) :: tree !! the expression tree
    type(scalar_program), intent(out) :: prog !! the compiled program

    integer :: nmax

    nmax = 2 * tree%n_nodes + 1 ! upper bound on the number of instructions
    allocate(prog%op(nmax), prog%a(nmax), prog%b(nmax), prog%o(nmax), prog%pos(nmax))
    allocate(prog%s(tree%n_nodes), source=0.0_wp)
    prog%ok = .true.
    call emit(tree%root)
    prog%root = tree%root
    if (prog%ok) then
        prog%op = prog%op(1:prog%n)
        prog%a = prog%a(1:prog%n)
        prog%b = prog%b(1:prog%n)
        prog%o = prog%o(1:prog%n)
        prog%pos = prog%pos(1:prog%n)
    end if

    contains

        recursive subroutine emit(inode)
        !! emit the instructions for a node. Its result goes in slot `inode`.
        integer, intent(in) :: inode
        integer :: k, c1, c2, c3, id, j1, j2, nargs

        if (.not. prog%ok) return
        associate (node => tree%nodes(inode))
        nargs = size(node%args)
        if (nargs > 0) c1 = node%args(1)
        if (nargs > 1) c2 = node%args(2)
        if (nargs > 2) c3 = node%args(3)

        select case (node%kind)

        case (node_const)
            prog%s(inode) = node%value

        case (node_var)
            call add(ins_var, node%op, 0, inode, node%pos)

        case (node_unary)
            call emit(c1)
            if (node%op == op_neg) then
                call add(ins_neg, c1, 0, inode, node%pos)
            else
                call add(ins_not, c1, 0, inode, node%pos)
            end if

        case (node_binary)
            call emit(c1)
            call emit(c2)
            call add(node%op, c1, c2, inode, node%pos)

        case (node_builtin)
            id = node%op
            select case (id)
            case (fn_if)
                call emit(c3)
                call add(ins_jz, c3, 0, 0, node%pos)
                j1 = prog%n
                call emit(c1)
                call add(ins_move, c1, 0, inode, node%pos)
                call add(ins_jmp, 0, 0, 0, node%pos)
                j2 = prog%n
                prog%o(j1) = prog%n + 1
                call emit(c2)
                call add(ins_move, c2, 0, inode, node%pos)
                prog%o(j2) = prog%n + 1
            case (fn_max, fn_min)
                do k = 1, nargs
                    call emit(node%args(k))
                end do
                call add(fn_offset + id, c1, c2, inode, node%pos)
                do k = 3, nargs
                    call add(fn_offset + id, inode, node%args(k), inode, node%pos)
                end do
            case (1:fn_last_elemental)
                call emit(c1)
                call add(unary_offset + id, c1, 0, inode, node%pos)
            case (fn_atan2, fn_mod, fn_modulo, fn_sign, fn_hypot)
                call emit(c1)
                call emit(c2)
                call add(fn_offset + id, c1, c2, inode, node%pos)
            case (fn_sum, fn_product, fn_maxval, fn_minval) ! of a scalar
                call emit(c1)
                call add(ins_move, c1, 0, inode, node%pos)
            case (fn_norm2)
                call emit(c1)
                call add(unary_offset + fn_abs, c1, 0, inode, node%pos)
            case (fn_size)
                call emit(c1)  ! evaluated for consistency of any errors
                prog%s(inode) = 1.0_wp
            case (fn_dot)
                call emit(c1)
                call emit(c2)
                call add(op_mul, c1, c2, inode, node%pos)
            case default ! e.g., cross
                prog%ok = .false.
            end select

        case (node_vector)
            if (nargs == 1) then
                call emit(c1)
                call add(ins_move, c1, 0, inode, node%pos)
            else
                prog%ok = .false.
            end if

        case default ! user functions, indexing
            prog%ok = .false.
        end select

        end associate

        end subroutine emit

        subroutine add(op, a, b, o, pos)
        !! add an instruction
        integer, intent(in) :: op, a, b, o, pos
        prog%n = prog%n + 1
        prog%op(prog%n) = op
        prog%a(prog%n) = a
        prog%b(prog%n) = b
        prog%o(prog%n) = o
        prog%pos(prog%n) = pos
        end subroutine add

    end subroutine compile_scalar
!*****************************************************************************************

!*****************************************************************************************
!>
!  Execute a scalar program. All the variables referenced by the program
!  must be scalars. The result is `prog%s(prog%root)`.

    subroutine exec_scalar(prog, vars, ctx)

    type(scalar_program), intent(inout) :: prog !! the program
    type(val_t), dimension(:), intent(in) :: vars !! variable values (all scalars)
    type(eval_ctx), intent(inout) :: ctx !! evaluation context

    integer :: k, op, a, b, o, ierr
    logical :: check

    check = ctx%check_domain

    associate (s => prog%s)

    k = 1
    do while (k <= prog%n)

        op = prog%op(k)
        a = prog%a(k)
        b = prog%b(k)
        o = prog%o(k)

        select case (op)
        case (ins_var);  s(o) = vars(a)%v(1)
        case (ins_neg);  s(o) = -s(a)
        case (ins_not);  s(o) = merge(1.0_wp, 0.0_wp, s(a) == 0.0_wp)
        case (ins_move); s(o) = s(a)
        case (ins_jz)
            if (s(a) == 0.0_wp) then
                k = o
                cycle
            end if
        case (ins_jmp)
            k = o
            cycle
        case (op_add); s(o) = s(a) + s(b)
        case (op_sub); s(o) = s(a) - s(b)
        case (op_mul); s(o) = s(a) * s(b)
        case (op_div)
            if (check .and. s(b) == 0.0_wp) then
                call ctx%err%set(domain_message(binary_check(op, s(a), s(b))), prog%pos(k))
                return
            end if
            s(o) = s(a) / s(b)
        case (op_pow)
            if (check) then
                ierr = binary_check(op, s(a), s(b))
                if (ierr /= 0) then
                    call ctx%err%set(domain_message(ierr), prog%pos(k))
                    return
                end if
            end if
            s(o) = pow(s(a), s(b))
        case (fn_offset + fn_max); s(o) = max(s(a), s(b))
        case (fn_offset + fn_min); s(o) = min(s(a), s(b))
        case (unary_offset + fn_sin); s(o) = sin(s(a))
        case (unary_offset + fn_cos); s(o) = cos(s(a))
        case (unary_offset + fn_exp); s(o) = exp(s(a))
        case (unary_offset + fn_abs); s(o) = abs(s(a))
        case default
            if (op > unary_offset) then ! other one-argument functions
                if (check) then
                    ierr = unary_check(op - unary_offset, s(a))
                    if (ierr /= 0) then
                        call ctx%err%set(domain_message(ierr), prog%pos(k))
                        return
                    end if
                end if
                s(o) = unary_value(op - unary_offset, s(a))
            else ! other binary operators and two-argument functions
                if (check .and. has_domain_check(op)) then
                    ierr = binary_check(op, s(a), s(b))
                    if (ierr /= 0) then
                        call ctx%err%set(domain_message(ierr), prog%pos(k))
                        return
                    end if
                end if
                s(o) = binary_value(op, s(a), s(b))
            end if
        end select

        k = k + 1

    end do

    end associate

    end subroutine exec_scalar
!*****************************************************************************************

!*****************************************************************************************
!>
!  Execute a scalar program, also computing the derivatives (gradient) of the
!  result with respect to the `nd` derivative variables. All the variables
!  referenced by the program must be scalars (with derivatives `vars(i)%d(1,:)`).
!  The result is `prog%s(prog%root)` and its gradient is `prog%sd(:,prog%root)`.

    subroutine exec_scalar_d(prog, vars, ctx)

    type(scalar_program), intent(inout) :: prog !! the program
    type(val_t), dimension(:), intent(in) :: vars !! variable values (all scalars)
    type(eval_ctx), intent(inout) :: ctx !! evaluation context

    integer :: k, op, a, b, o, ierr, nd
    real(wp) :: dx, dy

    nd = ctx%nd
    if (allocated(prog%sd)) then
        if (size(prog%sd, 1) /= nd) deallocate(prog%sd)
    end if
    ! note: the constant slots must have zero derivatives
    if (.not. allocated(prog%sd)) allocate(prog%sd(nd, size(prog%s)), source=0.0_wp)

    associate (s => prog%s, sd => prog%sd)

    k = 1
    do while (k <= prog%n)

        op = prog%op(k)
        a = prog%a(k)
        b = prog%b(k)
        o = prog%o(k)

        select case (op)
        case (ins_var)
            s(o) = vars(a)%v(1)
            sd(:, o) = vars(a)%d(1, :)
        case (ins_neg)
            s(o) = -s(a)
            sd(:, o) = -sd(:, a)
        case (ins_not)
            s(o) = merge(1.0_wp, 0.0_wp, s(a) == 0.0_wp)
            sd(:, o) = 0.0_wp
        case (ins_move)
            s(o) = s(a)
            sd(:, o) = sd(:, a)
        case (ins_jz)
            if (s(a) == 0.0_wp) then
                k = o
                cycle
            end if
        case (ins_jmp)
            k = o
            cycle
        case (fn_offset + fn_max, fn_offset + fn_min)
            ! the first argument is kept for ties (as in the general evaluator)
            if ((op == fn_offset + fn_max .and. s(b) > s(a)) .or. &
                (op == fn_offset + fn_min .and. s(b) < s(a))) then
                s(o) = s(b)
                sd(:, o) = sd(:, b)
            else
                s(o) = s(a)
                sd(:, o) = sd(:, a)
            end if
        case default
            if (op > unary_offset) then ! one-argument functions
                if (ctx%check_domain) then
                    ierr = unary_check(op - unary_offset, s(a))
                    if (ierr /= 0) then
                        call ctx%err%set(domain_message(ierr), prog%pos(k))
                        return
                    end if
                end if
                s(o) = unary_value(op - unary_offset, s(a))
                sd(:, o) = unary_derivative(op - unary_offset, s(a), s(o)) * sd(:, a)
            else ! binary operators and two-argument functions
                if (ctx%check_domain .and. has_domain_check(op)) then
                    ierr = binary_check(op, s(a), s(b))
                    if (ierr /= 0) then
                        call ctx%err%set(domain_message(ierr), prog%pos(k))
                        return
                    end if
                end if
                s(o) = binary_value(op, s(a), s(b))
                call binary_partials(op, s(a), s(b), s(o), dx, dy)
                sd(:, o) = dx * sd(:, a) + dy * sd(:, b)
            end if
        end select

        k = k + 1

    end do

    end associate

    end subroutine exec_scalar_d
!*****************************************************************************************

!*****************************************************************************************
    end module fparse_bytecode
!*****************************************************************************************
