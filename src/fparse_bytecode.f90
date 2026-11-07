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
    integer, parameter :: ins_neg  = 402 !! `s(o) = -s(a)`
    integer, parameter :: ins_not  = 403 !! `s(o) = .not. s(a)`
    integer, parameter :: ins_move = 404 !! `s(o) = s(a)`
    integer, parameter :: ins_jz   = 405 !! if `s(a) == 0`, jump to instruction `o`
    integer, parameter :: ins_jmp  = 406 !! jump to instruction `o`

    ! dense instruction codes used for dispatch in [[exec_scalar]]
    ! (so the `select case` compiles to a compact jump table):
    integer, parameter :: d_add = 1, d_sub = 2, d_mul = 3, d_div = 4, d_pow = 5, &
                          d_neg = 6, d_not = 7, d_move = 8, d_jz = 9, d_jmp = 10, &
                          d_max = 11, d_min = 12, d_sin = 13, d_cos = 14, d_exp = 15, &
                          d_abs = 16, d_sqrt = 17, d_unary = 18, d_binary = 19

    type, public :: scalar_program
        !! A compiled scalar expression.
        logical :: ok = .false. !! true if the expression could be compiled
        integer :: n = 0 !! number of instructions
        integer :: root = 0 !! slot containing the result
        integer, dimension(:,:), allocatable :: ins !! the instructions `[5, n]`. For each one:
                                                   !! the dense code (`d_*`), first operand slot,
                                                   !! second operand slot, output slot (or jump
                                                   !! target), and the full operation code
        integer, dimension(:), allocatable :: pos !! position in the expression (for error messages)
        integer :: nvar = 0 !! number of variable loads
        integer, dimension(:), allocatable :: var_slot !! slots to load the variables into
        integer, dimension(:), allocatable :: var_index !! the variables to load
        integer, dimension(:), allocatable :: var_ref !! position of each loaded variable in `expr_tree%refs`
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
    allocate(prog%ins(5, nmax), prog%pos(nmax))
    allocate(prog%var_slot(tree%n_nodes), prog%var_index(tree%n_nodes), prog%var_ref(tree%n_nodes))
    allocate(prog%s(tree%n_nodes), source=0.0_wp)
    prog%ok = .true.
    call emit(tree%root)
    prog%root = tree%root
    if (prog%ok) then
        prog%ins = prog%ins(:, 1:prog%n)
        prog%pos = prog%pos(1:prog%n)
        prog%var_slot = prog%var_slot(1:prog%nvar)
        prog%var_index = prog%var_index(1:prog%nvar)
        prog%var_ref = prog%var_ref(1:prog%nvar)
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
            ! variables are loaded before the instructions are executed
            prog%nvar = prog%nvar + 1
            prog%var_slot(prog%nvar) = inode
            prog%var_index(prog%nvar) = node%op
            prog%var_ref(prog%nvar) = node%iref

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
                prog%ins(4, j1) = prog%n + 1
                call emit(c2)
                call add(ins_move, c2, 0, inode, node%pos)
                prog%ins(4, j2) = prog%n + 1
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
        prog%ins(:, prog%n) = [dense_code(op), a, b, o, op]
        prog%pos(prog%n) = pos
        end subroutine add

        pure integer function dense_code(op)
        !! the dense instruction code for an operation code
        integer, intent(in) :: op
        select case (op)
        case (op_add);                 dense_code = d_add
        case (op_sub);                 dense_code = d_sub
        case (op_mul);                 dense_code = d_mul
        case (op_div);                 dense_code = d_div
        case (op_pow);                 dense_code = d_pow
        case (ins_neg);                dense_code = d_neg
        case (ins_not);                dense_code = d_not
        case (ins_move);               dense_code = d_move
        case (ins_jz);                 dense_code = d_jz
        case (ins_jmp);                dense_code = d_jmp
        case (fn_offset + fn_max);     dense_code = d_max
        case (fn_offset + fn_min);     dense_code = d_min
        case (unary_offset + fn_sin);  dense_code = d_sin
        case (unary_offset + fn_cos);  dense_code = d_cos
        case (unary_offset + fn_exp);  dense_code = d_exp
        case (unary_offset + fn_abs);  dense_code = d_abs
        case (unary_offset + fn_sqrt); dense_code = d_sqrt
        case default
            if (op > unary_offset) then
                dense_code = d_unary
            else
                dense_code = d_binary
            end if
        end select
        end function dense_code

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

    associate (s => prog%s, ins => prog%ins)

    do k = 1, prog%nvar
        s(prog%var_slot(k)) = vars(prog%var_index(k))%v(1)
    end do

    k = 1
    do while (k <= prog%n)

        a = ins(2, k)
        b = ins(3, k)
        o = ins(4, k)

        select case (ins(1, k))
        case (d_add);  s(o) = s(a) + s(b)
        case (d_sub);  s(o) = s(a) - s(b)
        case (d_mul);  s(o) = s(a) * s(b)
        case (d_div)
            if (check) then
                if (s(b) == 0.0_wp) then
                    call ctx%err%set(domain_message(binary_check(op_div, s(a), s(b))), prog%pos(k))
                    return
                end if
            end if
            s(o) = s(a) / s(b)
        case (d_pow)
            if (check) then
                ierr = binary_check(op_pow, s(a), s(b))
                if (ierr /= 0) then
                    call ctx%err%set(domain_message(ierr), prog%pos(k))
                    return
                end if
            end if
            s(o) = pow(s(a), s(b))
        case (d_neg);  s(o) = -s(a)
        case (d_not);  s(o) = merge(1.0_wp, 0.0_wp, s(a) == 0.0_wp)
        case (d_move); s(o) = s(a)
        case (d_jz)
            if (s(a) == 0.0_wp) then
                k = o
                cycle
            end if
        case (d_jmp)
            k = o
            cycle
        case (d_max);  s(o) = max(s(a), s(b))
        case (d_min);  s(o) = min(s(a), s(b))
        case (d_sin);  s(o) = sin(s(a))
        case (d_cos);  s(o) = cos(s(a))
        case (d_exp);  s(o) = exp(s(a))
        case (d_abs);  s(o) = abs(s(a))
        case (d_sqrt)
            if (check) then
                if (s(a) < 0.0_wp) then
                    call ctx%err%set(domain_message(unary_check(fn_sqrt, s(a))), prog%pos(k))
                    return
                end if
            end if
            s(o) = sqrt(s(a))
        case (d_unary) ! other one-argument functions
            op = ins(5, k) - unary_offset
            if (check) then
                ierr = unary_check(op, s(a))
                if (ierr /= 0) then
                    call ctx%err%set(domain_message(ierr), prog%pos(k))
                    return
                end if
            end if
            s(o) = unary_value(op, s(a))
        case default ! other binary operators and two-argument functions
            op = ins(5, k)
            if (check .and. has_domain_check(op)) then
                ierr = binary_check(op, s(a), s(b))
                if (ierr /= 0) then
                    call ctx%err%set(domain_message(ierr), prog%pos(k))
                    return
                end if
            end if
            s(o) = binary_value(op, s(a), s(b))
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

    subroutine exec_scalar_d(prog, vars, ctx, dref)

    type(scalar_program), intent(inout) :: prog !! the program
    type(val_t), dimension(:), intent(in) :: vars !! variable values (all scalars)
    type(eval_ctx), intent(inout) :: ctx !! evaluation context
    type(val_t), dimension(:), intent(in), optional :: dref !! derivatives of the referenced variables
                                                            !! (indexed like `expr_tree%refs`). Where `dref(i)%d`
                                                            !! is allocated, it is used instead of the variable's `d`.

    logical :: use_dref

    integer :: k, op, a, b, o, ierr, nd
    real(wp) :: dx, dy

    nd = ctx%nd
    if (allocated(prog%sd)) then
        if (size(prog%sd, 1) /= nd) deallocate(prog%sd)
    end if
    ! note: the constant slots must have zero derivatives
    if (.not. allocated(prog%sd)) allocate(prog%sd(nd, size(prog%s)), source=0.0_wp)

    associate (s => prog%s, sd => prog%sd)

    do k = 1, prog%nvar
        s(prog%var_slot(k)) = vars(prog%var_index(k))%v(1)
        use_dref = .false.
        if (present(dref)) use_dref = allocated(dref(prog%var_ref(k))%d)
        if (use_dref) then
            sd(:, prog%var_slot(k)) = dref(prog%var_ref(k))%d(1, :)
        else
            sd(:, prog%var_slot(k)) = vars(prog%var_index(k))%d(1, :)
        end if
    end do

    k = 1
    do while (k <= prog%n)

        op = prog%ins(5, k)
        a = prog%ins(2, k)
        b = prog%ins(3, k)
        o = prog%ins(4, k)

        select case (op)
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
