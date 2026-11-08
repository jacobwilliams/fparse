!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Generation of Fortran source code from parsed expressions (see
!  [[fparser:generate_fortran]]).
!
!  The generated code is a module with two subroutines:
!
!  * `<module>_eval(inputs..., outputs...)` computes the outputs.
!  * `<module>_jac(inputs..., outputs..., d_outputs...)` also computes the
!    derivatives of the outputs with respect to the selected inputs (forward mode,
!    written out as Fortran statements). The derivatives that are known to be zero
!    are not computed.
!
!  The code is specialized for the sizes of the input variables: scalars are
!  `real(wp)` arguments, and vectors are fixed-size arrays. The sizes of all the
!  intermediate values are determined from the expression trees.
!
!  The generated code does not check for domain errors (they give NaN or infinity
!  results, as in plain Fortran), or that indices are within bounds.

module fparse_codegen

use fparse_common
use fparse_functions
use fparse_parser
use fparse_hash

implicit none (type, external)

private

integer, parameter :: max_inline = 200 !! the longest expression text kept inline in the value routine
integer, parameter :: max_line = 100   !! longer lines are continued on the next line

real(wp), parameter :: pi = acos(-1.0_wp)
character(len=*), parameter :: nl = new_line('a') !! the newline character

type :: tree_info
    !! The sizes of the nodes of an expression tree, and which have zero derivatives.
    integer, dimension(:), allocatable :: nsz !! size of the value of each node
    logical, dimension(:), allocatable :: dz !! if the derivative of each node is zero
    logical, dimension(:), allocatable :: needs_temp !! if a node must be stored in a temporary
                                                     !! (the base of an index)
end type tree_info

type :: codegen_t
    !! The state of the code generator.
    type(string_t), dimension(:), allocatable :: idents !! all the identifiers used
    integer :: nid = 0 !! number of identifiers used
    type(name_table) :: table !! hash table for `idents`
    type(string_t), dimension(:), allocatable :: uname !! the Fortran names of the variables
    integer, dimension(:), allocatable :: vsize !! the sizes of the variables
    logical, dimension(:), allocatable :: vdz !! if the derivatives of the variables are zero
    integer, dimension(:), allocatable :: col !! column offset of each input in the jacobians
                                              !! (-1 if it is not a derivative variable)
    type(string_t), dimension(:), allocatable :: dname !! the names of the derivatives of the variables:
                                                       !! the seeds of the inputs, and the jacobian
                                                       !! arguments of the outputs
    integer :: nd = 0 !! number of derivative columns
    type(tree_info), dimension(:), allocatable :: info !! information about each expression tree
    character(len=:), allocatable :: fn_pow !! name of the power helper function
    character(len=:), allocatable :: fn_floor !! name of the floor helper function
    character(len=:), allocatable :: fn_cross !! name of the cross product helper function
    logical :: use_pow = .false. !! if the power helper is used
    logical :: use_floor = .false. !! if the floor helper is used
    logical :: use_cross = .false. !! if the cross product helper is used
    ! the routine being generated:
    type(string_builder) :: decl !! the declarations of the local variables
    type(string_builder) :: body !! the statements
    integer :: indent = 1 !! the current indentation level of the statements
    logical :: use_i = .false. !! if the loop variable `i` is used
    logical :: use_j = .false. !! if the index variable `j` is used
    integer :: ntemp = 0 !! number of temporaries in the routine
    type(error_t) :: err !! error status
end type codegen_t

! Fortran names that are used in the generated code (the variables can't have these names):
character(len=*), dimension(*), parameter :: reserved = [character(len=16) :: &
    'wp', 'real64', 'iso_fortran_env', 'i', 'j', &
    'abs', 'sqrt', 'exp', 'log', 'log10', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan', &
    'sinh', 'cosh', 'tanh', 'asinh', 'acosh', 'atanh', 'erf', 'erfc', 'anint', 'aint', &
    'sind', 'cosd', 'tand', 'asind', 'acosd', 'atand', 'atan2', 'atan2d', 'mod', 'modulo', &
    'sign', 'hypot', 'max', 'min', 'sum', 'product', 'norm2', 'maxval', 'minval', 'maxloc', &
    'minloc', 'dot_product', 'matmul', 'merge', 'nint', 'real', 'huge', 'size', 'spread']

! precedence levels of the Fortran expressions that are generated:
integer, parameter :: prec_add = 6     !! `a + b`, `a - b`, `-a`
integer, parameter :: prec_mul = 7     !! `a*b`, `a/b`
integer, parameter :: prec_pow = 9     !! `a**b`
integer, parameter :: prec_primary = 10 !! names, literals, function calls, parenthesized expressions

public :: generate_code

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate a Fortran module that evaluates a set of parsed expressions
!  (and optionally their derivatives).

subroutine generate_code(trees, names, nin, order, input_sizes, wrt, descriptions, module_name, code, err)

    type(expr_tree), dimension(:), intent(in) :: trees !! the expression trees
    type(string_t), dimension(:), intent(in) :: names !! the names of all the variables (inputs, then
                                                      !! outputs; empty for anonymous outputs)
    integer, intent(in) :: nin !! number of input variables
    integer, dimension(:), intent(in) :: order !! the evaluation order of the expressions
    integer, dimension(:), intent(in) :: input_sizes !! the sizes of the input variables
    integer, dimension(:), intent(in) :: wrt !! the input variables to differentiate with respect
                                             !! to (if empty, no jacobian routine is generated)
    type(string_t), dimension(:), intent(in) :: descriptions !! the expressions (for the comments)
    character(len=*), intent(in) :: module_name !! the name of the module
    character(len=:), allocatable, intent(out) :: code !! the generated code
    type(error_t), intent(inout) :: err !! error status

    type(codegen_t) :: g
    type(string_builder) :: out
    integer :: nout, i, k, e
    character(len=:), allocatable :: eval_name, jac_name, line

    nout = size(trees)
    code = ''

    if (.not. is_valid_name(module_name) .or. len(module_name) > 59) then
        call err%set('invalid module name "'//module_name//'"')
        return
    end if

    ! identifiers: reserved names, the module and routine names, the helpers, then the variables
    allocate(g%idents(64))
    call g%table%init(64)
    do i = 1, size(reserved)
        line = add_ident(g, trim(reserved(i)))
    end do
    line = add_ident(g, module_name)
    eval_name = unique_ident(g, module_name//'_eval')
    jac_name = unique_ident(g, module_name//'_jac')
    g%fn_pow = unique_ident(g, 'fp_pow')
    g%fn_floor = unique_ident(g, 'fp_floor')
    g%fn_cross = unique_ident(g, 'fp_cross')
    allocate(g%uname(nin + nout))
    do i = 1, nin + nout
        if (i > nin .and. len(names(i)%str) == 0) then
            g%uname(i)%str = unique_ident(g, 'out'//int_to_str(i - nin))
        else
            g%uname(i)%str = unique_ident(g, names(i)%str)
        end if
        if (len(g%uname(i)%str) > 63) then
            call err%set('the name "'//g%uname(i)%str//'" is too long for Fortran (63 characters at most)')
            return
        end if
    end do

    ! the derivative columns:
    allocate(g%col(nin), source=-1)
    allocate(g%vsize(nin + nout), source=0)
    allocate(g%vdz(nin + nout), source=.true.)
    allocate(g%dname(nin + nout))
    g%vsize(1:nin) = input_sizes
    g%nd = 0
    do k = 1, size(wrt)
        g%col(wrt(k)) = g%nd
        g%vdz(wrt(k)) = .false.
        g%dname(wrt(k))%str = unique_ident(g, 'ds_'//g%uname(wrt(k))%str)
        g%nd = g%nd + input_sizes(wrt(k))
    end do

    ! the sizes of all the values (in the evaluation order, so that the sizes of the outputs
    ! that an expression references are known):
    allocate(g%info(nout))
    do k = 1, nout
        e = order(k)
        call analyze_tree(g, trees(e), e)
        if (g%err%failed) then
            call err%set(g%err%message)
            return
        end if
        g%vsize(nin + e) = g%info(e)%nsz(trees(e)%root)
        g%vdz(nin + e) = g%info(e)%dz(trees(e)%root)
    end do
    do e = 1, nout
        g%dname(nin + e)%str = unique_ident(g, 'd_'//g%uname(nin + e)%str)
    end do

    ! the module:
    call comment(out, 0, 'Generated by fparse (https://github.com/jacobwilliams/fparse).')
    call out%add('!'//nl)
    call comment(out, 0, 'Inputs:')
    do i = 1, nin
        call comment(out, 0, '  '//g%uname(i)%str//size_note(g%vsize(i))//&
                     renamed_note(names(i)%str, g%uname(i)%str))
    end do
    call out%add('!'//nl)
    call comment(out, 0, 'Outputs:')
    do e = 1, nout
        call comment(out, 0, '  '//g%uname(nin + e)%str//size_note(g%vsize(nin + e))//' : '//&
                     descriptions(e)%str)
    end do
    if (size(wrt) > 0) then
        call out%add('!'//nl)
        call comment(out, 0, 'The jacobian of each output (`d_<name>`) has '//int_to_str(g%nd)//&
                     ' columns, for the derivatives with respect to:')
        do k = 1, size(wrt)
            call comment(out, 0, '  '//g%uname(wrt(k))%str//': column'//&
                         column_note(g%col(wrt(k)), input_sizes(wrt(k))))
        end do
        call comment(out, 0, 'For a scalar output it is a vector, and for a vector output it is a matrix '//&
                     '(one row for each element).')
    end if
    call out%add(nl//'module '//module_name//nl//nl)
    call out%add('use, intrinsic :: iso_fortran_env, only: wp => real64'//nl//nl)
    call out%add('implicit none'//nl//nl)
    call out%add('private'//nl//nl)
    if (size(wrt) > 0) then
        call out%add('public :: '//eval_name//', '//jac_name//nl//nl)
    else
        call out%add('public :: '//eval_name//nl//nl)
    end if
    call out%add('contains'//nl)

    call value_routine(g, trees, nin, order, eval_name, out)
    if (g%err%failed) then
        call err%set(g%err%message)
        return
    end if
    if (size(wrt) > 0) then
        call jacobian_routine(g, trees, nin, order, wrt, jac_name, out)
        if (g%err%failed) then
            call err%set(g%err%message)
            return
        end if
    end if
    call helpers(g, out)
    call out%add(nl//'end module '//module_name//nl)

    code = out%get()

end subroutine generate_code
!*****************************************************************************************

!*****************************************************************************************
!>
!  Determine the size of the value of each node of a tree, and which nodes have zero
!  derivatives (with respect to the derivative variables).

subroutine analyze_tree(g, tree, e)

    type(codegen_t), intent(inout) :: g !! the code generator
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: e !! the expression index

    integer :: k, i, j, n, m, c, na, nb
    integer, dimension(:), allocatable :: nsz
    logical, dimension(:), allocatable :: dz, needs_temp
    character(len=:), allocatable :: loc

    allocate(nsz(tree%n_nodes), source=1)
    allocate(dz(tree%n_nodes), source=.true.)
    allocate(needs_temp(tree%n_nodes), source=.false.)
    loc = ' (expression '//int_to_str(e)//')'

    do k = 1, tree%n_steps
        if (tree%sched(1, k) /= step_eval) cycle
        i = tree%sched(2, k)
        associate (node => tree%nodes(i))
            select case (node%kind)
            case (node_const)
                nsz(i) = 1
                dz(i) = .true.
            case (node_var)
                nsz(i) = g%vsize(node%op)
                dz(i) = g%vdz(node%op)
            case (node_unary)
                nsz(i) = nsz(arg(1))
                dz(i) = node%op == op_not .or. dz(arg(1))
            case (node_binary)
                nsz(i) = broadcast(nsz(arg(1)), nsz(arg(2)))
                if (g%err%failed) return
                select case (node%op)
                case (op_eq, op_ne, op_lt, op_le, op_gt, op_ge, op_and, op_or)
                    dz(i) = .true.
                case default
                    dz(i) = dz(arg(1)) .and. dz(arg(2))
                end select
            case (node_builtin)
                select case (node%op)
                case (1:fn_last_elemental)
                    nsz(i) = nsz(arg(1))
                    select case (node%op)
                    case (fn_floor, fn_ceiling, fn_nint, fn_aint) ! (piecewise constant)
                        dz(i) = .true.
                    case default
                        dz(i) = dz(arg(1))
                    end select
                case (fn_atan2, fn_mod, fn_modulo, fn_hypot, fn_atan2d)
                    nsz(i) = broadcast(nsz(arg(1)), nsz(arg(2)))
                    dz(i) = dz(arg(1)) .and. dz(arg(2))
                case (fn_sign)
                    nsz(i) = broadcast(nsz(arg(1)), nsz(arg(2)))
                    dz(i) = dz(arg(1))
                case (fn_max, fn_min)
                    n = nsz(arg(1))
                    dz(i) = dz(arg(1))
                    do j = 2, node%na
                        n = broadcast(n, nsz(arg(j)))
                        dz(i) = dz(i) .and. dz(arg(j))
                    end do
                    nsz(i) = n
                case (fn_sum, fn_product, fn_norm2, fn_maxval, fn_minval)
                    nsz(i) = 1
                    dz(i) = dz(arg(1))
                case (fn_size)
                    nsz(i) = 1
                    dz(i) = .true.
                case (fn_dot)
                    if (nsz(arg(1)) /= nsz(arg(2))) call g%err%set('arguments of dot_product have '//&
                        'different sizes ('//int_to_str(nsz(arg(1)))//' and '//int_to_str(nsz(arg(2)))//')'//loc)
                    nsz(i) = 1
                    dz(i) = dz(arg(1)) .and. dz(arg(2))
                case (fn_cross)
                    if (nsz(arg(1)) /= 3 .or. nsz(arg(2)) /= 3) call g%err%set('arguments of cross must '//&
                        'have size 3 (sizes are '//int_to_str(nsz(arg(1)))//' and '//int_to_str(nsz(arg(2)))//')'//loc)
                    nsz(i) = 3
                    dz(i) = dz(arg(1)) .and. dz(arg(2))
                case (fn_if)
                    c = nsz(arg(3))
                    na = nsz(arg(1))
                    nb = nsz(arg(2))
                    if (c == 1) then
                        if (na /= nb) call g%err%set('the values of "if" must have the same size for code '//&
                            'generation (sizes are '//int_to_str(na)//' and '//int_to_str(nb)//')'//loc)
                        nsz(i) = na
                    else
                        if ((na /= 1 .and. na /= c) .or. (nb /= 1 .and. nb /= c)) call g%err%set(&
                            'incompatible sizes in if'//loc)
                        nsz(i) = c
                    end if
                    dz(i) = dz(arg(1)) .and. dz(arg(2))
                case default
                    call g%err%set('unsupported function for code generation'//loc)
                end select
            case (node_vector)
                n = 0
                dz(i) = .true.
                do j = 1, node%na
                    n = n + nsz(arg(j))
                    dz(i) = dz(i) .and. dz(arg(j))
                end do
                nsz(i) = n
            case (node_index)
                nsz(i) = 1
                dz(i) = dz(arg(1))
                if (nsz(arg(1)) > 1 .and. tree%nodes(arg(1))%kind /= node_var) needs_temp(arg(1)) = .true.
            case (node_user)
                call g%err%set('user-defined functions are not supported by the code generator'//loc)
            case default
                call g%err%set('invalid expression tree'//loc)
            end select
        end associate
        if (g%err%failed) return
    end do

    call move_alloc(nsz, g%info(e)%nsz)
    call move_alloc(dz, g%info(e)%dz)
    call move_alloc(needs_temp, g%info(e)%needs_temp)

contains

    integer function arg(j)
        !! the index of argument `j` of node `i`
        integer, intent(in) :: j !! the argument number
        arg = tree%args(tree%nodes(i)%ia + j)
    end function arg

    integer function broadcast(a, b)
        !! the size of the result of an element-wise operation
        integer, intent(in) :: a !! the size of the first operand
        integer, intent(in) :: b !! the size of the second operand
        if (a == b .or. b == 1) then
            broadcast = a
        else if (a == 1) then
            broadcast = b
        else
            broadcast = a
            call g%err%set('incompatible sizes ('//int_to_str(a)//' and '//int_to_str(b)//')'//loc)
        end if
    end function broadcast

end subroutine analyze_tree
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate the routine that evaluates the outputs. Each expression is written as a
!  single Fortran expression where possible (temporaries are only used for long
!  expressions, the bases of indices, and conditionals with a scalar condition, which
!  are written as `if` blocks so that only the selected value is evaluated).

subroutine value_routine(g, trees, nin, order, rname, out)

    type(codegen_t), intent(inout) :: g !! the code generator
    type(expr_tree), dimension(:), intent(in) :: trees !! the expression trees
    integer, intent(in) :: nin !! number of input variables
    integer, dimension(:), intent(in) :: order !! the evaluation order
    character(len=*), intent(in) :: rname !! the name of the routine
    type(string_builder), intent(inout) :: out !! the output

    type(string_t), dimension(:), allocatable :: txt
    integer, dimension(:), allocatable :: prec
    integer :: k, s, e, i, nout
    character(len=:), allocatable :: tname

    nout = size(trees)
    call start_routine(g)
    call out%add(nl//wrap_text('pure subroutine '//rname//'('//argument_list(g, nin, nout, .false.)//')', 8)//nl)
    call out%add(nl//'    !! Compute the outputs.'//nl//nl)
    call declare_arguments(g, nin, nout, .false., out)

    do k = 1, nout
        e = order(k)
        associate (tree => trees(e), info => g%info(e))
            if (allocated(txt)) deallocate(txt)
            if (allocated(prec)) deallocate(prec)
            allocate(txt(tree%n_nodes), prec(tree%n_nodes))
            do s = 1, tree%n_steps
                i = tree%sched(2, s)
                select case (tree%sched(1, s))
                case (step_branch)
                    if (info%nsz(arg(tree, i, 3)) == 1) then
                        call statement(g, 'if ('//condition(tree, arg(tree, i, 3), txt)//') then')
                        g%indent = g%indent + 1
                    end if
                case (step_skip)
                    if (info%nsz(arg(tree, i, 3)) == 1) then
                        tname = new_temp(g, 't', info%nsz(i), 0)
                        call statement(g, tname//' = '//txt(arg(tree, i, 1))%str)
                        g%indent = g%indent - 1
                        call statement(g, 'else')
                        g%indent = g%indent + 1
                        txt(i)%str = tname
                    end if
                case (step_eval)
                    if (is_if(tree, i)) then
                        if (info%nsz(arg(tree, i, 3)) == 1) then
                            call statement(g, txt(i)%str//' = '//txt(arg(tree, i, 2))%str)
                            g%indent = g%indent - 1
                            call statement(g, 'end if')
                            prec(i) = prec_primary
                            cycle
                        end if
                    end if
                    call node_text(g, tree, info, i, txt, prec)
                    if (g%err%failed) return
                    if (tree%nodes(i)%kind /= node_const .and. tree%nodes(i)%kind /= node_var .and. &
                        (info%needs_temp(i) .or. len(txt(i)%str) > max_inline)) then
                        tname = new_temp(g, 't', info%nsz(i), 0)
                        call statement(g, tname//' = '//txt(i)%str)
                        txt(i)%str = tname
                        prec(i) = prec_primary
                    end if
                end select
            end do
            call statement(g, g%uname(nin + e)%str//' = '//txt(tree%root)%str)
        end associate
    end do

    call end_routine(g, out)
    call out%add(nl//'end subroutine '//rname//nl)

end subroutine value_routine
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate the routine that evaluates the outputs and their jacobians (forward mode).
!  Each node gets a temporary for its value, and one for its derivative (unless it is
!  known to be zero).

subroutine jacobian_routine(g, trees, nin, order, wrt, rname, out)

    type(codegen_t), intent(inout) :: g !! the code generator
    type(expr_tree), dimension(:), intent(in) :: trees !! the expression trees
    integer, intent(in) :: nin !! number of input variables
    integer, dimension(:), intent(in) :: order !! the evaluation order
    integer, dimension(:), intent(in) :: wrt !! the derivative variables
    character(len=*), intent(in) :: rname !! the name of the routine
    type(string_builder), intent(inout) :: out !! the output

    type(string_t), dimension(:), allocatable :: vb, db
    integer :: k, s, e, i, nout, iw, j, n, c3
    character(len=:), allocatable :: dout

    nout = size(trees)
    call start_routine(g)
    call out%add(nl//wrap_text('pure subroutine '//rname//'('//argument_list(g, nin, nout, .true.)//')', 8)//nl)
    call out%add(nl//'    !! Compute the outputs and their jacobians.'//nl//nl)
    call declare_arguments(g, nin, nout, .true., out)

    ! the seeds: the derivatives of the derivative variables
    do k = 1, size(wrt)
        iw = wrt(k)
        n = g%vsize(iw)
        if (n == 1) then
            call declare(g, g%dname(iw)%str, g%nd, 0, array=.true.)
            call statement(g, g%dname(iw)%str//' = 0.0_wp')
            call statement(g, g%dname(iw)%str//'('//int_to_str(g%col(iw) + 1)//') = 1.0_wp')
        else
            call declare(g, g%dname(iw)%str, n, g%nd)
            call statement(g, g%dname(iw)%str//' = 0.0_wp')
            g%use_i = .true.
            call statement(g, 'do i = 1, '//int_to_str(n))
            call statement(g, '    '//g%dname(iw)%str//'(i, '//int_to_str(g%col(iw))//' + i) = 1.0_wp')
            call statement(g, 'end do')
        end if
    end do

    do k = 1, nout
        e = order(k)
        associate (tree => trees(e), info => g%info(e))
            if (allocated(vb)) deallocate(vb)
            if (allocated(db)) deallocate(db)
            allocate(vb(tree%n_nodes), db(tree%n_nodes))
            do s = 1, tree%n_steps
                i = tree%sched(2, s)
                select case (tree%sched(1, s))
                case (step_branch)
                    c3 = arg(tree, i, 3)
                    if (info%nsz(c3) == 1) then
                        call statement(g, 'if ('//vb(c3)%str//' /= 0.0_wp) then')
                        g%indent = g%indent + 1
                    end if
                case (step_skip)
                    c3 = arg(tree, i, 3)
                    if (info%nsz(c3) == 1) then
                        call new_node_temps(i)
                        call copy_node(i, arg(tree, i, 1))
                        g%indent = g%indent - 1
                        call statement(g, 'else')
                        g%indent = g%indent + 1
                    end if
                case (step_eval)
                    select case (tree%nodes(i)%kind)
                    case (node_const)
                        vb(i)%str = num(tree%nodes(i)%value)
                        db(i)%str = ''
                    case (node_var)
                        j = tree%nodes(i)%op
                        vb(i)%str = g%uname(j)%str
                        if (g%vdz(j)) then
                            db(i)%str = ''
                        else
                            db(i)%str = g%dname(j)%str
                        end if
                    case default
                        if (is_if(tree, i)) then
                            if (info%nsz(arg(tree, i, 3)) == 1) then
                                call copy_node(i, arg(tree, i, 2))
                                g%indent = g%indent - 1
                                call statement(g, 'end if')
                                cycle
                            end if
                        end if
                        call new_node_temps(i)
                        call jacobian_node(g, tree, info, i, vb, db)
                        if (g%err%failed) return
                    end select
                end select
            end do
            ! the output and its jacobian:
            call statement(g, g%uname(nin + e)%str//' = '//vb(tree%root)%str)
            dout = g%dname(nin + e)%str
            if (len(db(tree%root)%str) > 0) then
                call statement(g, dout//' = '//db(tree%root)%str)
            else
                call statement(g, dout//' = 0.0_wp')
            end if
        end associate
    end do

    call end_routine(g, out)
    call out%add(nl//'end subroutine '//rname//nl)

contains

    subroutine new_node_temps(inode)
        !! create the temporaries for the value and derivative of a node
        integer, intent(in) :: inode !! the node
        integer :: nn
        nn = g%info(e)%nsz(inode)
        vb(inode)%str = new_temp(g, 'v', nn, 0)
        if (g%info(e)%dz(inode)) then
            db(inode)%str = ''
        else if (nn == 1) then
            db(inode)%str = new_temp(g, 'd', g%nd, 0, array=.true.)
        else
            db(inode)%str = new_temp(g, 'd', nn, g%nd)
        end if
    end subroutine new_node_temps

    subroutine copy_node(inode, src)
        !! copy the value and derivative of a node to another (with the same size)
        integer, intent(in) :: inode !! the destination node
        integer, intent(in) :: src !! the source node
        call statement(g, vb(inode)%str//' = '//vb(src)%str)
        if (len(db(inode)%str) > 0) then
            if (len(db(src)%str) > 0) then
                call statement(g, db(inode)%str//' = '//db(src)%str)
            else
                call statement(g, db(inode)%str//' = 0.0_wp')
            end if
        end if
    end subroutine copy_node

end subroutine jacobian_routine
!*****************************************************************************************

!*****************************************************************************************
!>
!  The Fortran expression for the value of a node, given those of its arguments
!  (`txt` and `prec`). For the value routine, and for the values in the jacobian routine.

subroutine node_text(g, tree, info, i, txt, prec)

    type(codegen_t), intent(inout) :: g !! the code generator
    type(expr_tree), intent(in) :: tree !! the expression tree
    type(tree_info), intent(in) :: info !! information about the tree
    integer, intent(in) :: i !! the node
    type(string_t), dimension(:), intent(inout) :: txt !! the text of each node
    integer, dimension(:), intent(inout) :: prec !! the precedence of the text of each node

    integer :: k, a, b, ie
    character(len=:), allocatable :: t

    associate (node => tree%nodes(i))
        if (node%na > 0) a = arg(tree, i, 1)
        if (node%na > 1) b = arg(tree, i, 2)
        prec(i) = prec_primary
        select case (node%kind)
        case (node_const)
            txt(i)%str = num(node%value)
        case (node_var)
            txt(i)%str = g%uname(node%op)%str
        case (node_unary)
            if (node%op == op_neg) then
                txt(i)%str = '-'//wrap(a, prec_mul)
                prec(i) = prec_add
            else
                txt(i)%str = 'merge(1.0_wp, 0.0_wp, '//txt(a)%str//' == 0.0_wp)'
            end if
        case (node_binary)
            select case (node%op)
            case (op_add)
                txt(i)%str = wrap(a, prec_add)//' + '//wrap(b, prec_add + 1)
                prec(i) = prec_add
            case (op_sub)
                txt(i)%str = wrap(a, prec_add)//' - '//wrap(b, prec_add + 1)
                prec(i) = prec_add
            case (op_mul)
                txt(i)%str = wrap(a, prec_mul)//'*'//wrap(b, prec_mul + 1)
                prec(i) = prec_mul
            case (op_div)
                txt(i)%str = wrap(a, prec_mul)//'/'//wrap(b, prec_mul + 1)
                prec(i) = prec_mul
            case (op_pow)
                if (int_constant(tree, b, ie)) then
                    txt(i)%str = wrap(a, prec_primary)//'**'//int_text(ie)
                    prec(i) = prec_pow
                else
                    g%use_pow = .true.
                    txt(i)%str = g%fn_pow//'('//txt(a)%str//', '//txt(b)%str//')'
                end if
            case (op_eq); txt(i)%str = relational('==')
            case (op_ne); txt(i)%str = relational('/=')
            case (op_lt); txt(i)%str = relational('<')
            case (op_le); txt(i)%str = relational('<=')
            case (op_gt); txt(i)%str = relational('>')
            case (op_ge); txt(i)%str = relational('>=')
            case (op_and)
                txt(i)%str = 'merge(1.0_wp, 0.0_wp, '//txt(a)%str//' /= 0.0_wp .and. '//txt(b)%str//' /= 0.0_wp)'
            case (op_or)
                txt(i)%str = 'merge(1.0_wp, 0.0_wp, '//txt(a)%str//' /= 0.0_wp .or. '//txt(b)%str//' /= 0.0_wp)'
            end select
        case (node_builtin)
            select case (node%op)
            case (1:fn_last_elemental)
                select case (node%op)
                case (fn_floor)
                    g%use_floor = .true.
                    txt(i)%str = g%fn_floor//'('//txt(a)%str//')'
                case (fn_ceiling)
                    g%use_floor = .true.
                    txt(i)%str = '-'//g%fn_floor//'(-'//wrap(a, prec_mul)//')'
                    prec(i) = prec_add
                case default
                    txt(i)%str = elemental_name(node%op)//'('//txt(a)%str//')'
                end select
            case (fn_atan2, fn_mod, fn_modulo, fn_sign, fn_hypot, fn_atan2d)
                txt(i)%str = elemental_name(node%op)//'('//txt(a)%str//', '//txt(b)%str//')'
            case (fn_max, fn_min)
                t = elemental_name(node%op)//'('//txt(a)%str
                do k = 2, node%na
                    t = t//', '//txt(arg(tree, i, k))%str
                end do
                txt(i)%str = t//')'
            case (fn_sum, fn_product, fn_maxval, fn_minval)
                if (info%nsz(a) == 1) then
                    txt(i)%str = txt(a)%str
                    prec(i) = prec(a)
                else
                    txt(i)%str = elemental_name(node%op)//'('//txt(a)%str//')'
                end if
            case (fn_norm2)
                if (info%nsz(a) == 1) then
                    txt(i)%str = 'abs('//txt(a)%str//')'
                else
                    txt(i)%str = 'norm2('//txt(a)%str//')'
                end if
            case (fn_size)
                txt(i)%str = num(real(info%nsz(a), wp))
            case (fn_dot)
                if (info%nsz(a) == 1) then
                    txt(i)%str = wrap(a, prec_mul)//'*'//wrap(b, prec_mul + 1)
                    prec(i) = prec_mul
                else
                    txt(i)%str = 'dot_product('//txt(a)%str//', '//txt(b)%str//')'
                end if
            case (fn_cross)
                g%use_cross = .true.
                txt(i)%str = g%fn_cross//'('//txt(a)%str//', '//txt(b)%str//')'
            case (fn_if) ! (with a vector condition)
                txt(i)%str = 'merge('//txt(a)%str//', '//txt(b)%str//', '//condition(tree, arg(tree, i, 3), txt)//')'
            end select
        case (node_vector)
            if (info%nsz(i) == 1) then
                txt(i)%str = txt(a)%str
                prec(i) = prec(a)
            else
                t = '['//txt(a)%str
                do k = 2, node%na
                    t = t//', '//txt(arg(tree, i, k))%str
                end do
                txt(i)%str = t//']'
            end if
        case (node_index)
            if (info%nsz(a) == 1) then
                txt(i)%str = txt(a)%str
                prec(i) = prec(a)
            else
                txt(i)%str = txt(a)%str//'('//index_text(tree, b, txt(b)%str)//')'
            end if
        end select
    end associate

contains

    function wrap(c, need) result(s)
        !! the text of a node, in parentheses if its precedence is lower than `need`
        integer, intent(in) :: c !! the node
        integer, intent(in) :: need !! the required precedence
        character(len=:), allocatable :: s !! the text
        if (prec(c) < need) then
            s = '('//txt(c)%str//')'
        else
            s = txt(c)%str
        end if
    end function wrap

    function relational(op) result(s)
        !! a relational operator (1 or 0)
        character(len=*), intent(in) :: op !! the Fortran operator
        character(len=:), allocatable :: s !! the text
        s = 'merge(1.0_wp, 0.0_wp, '//txt(a)%str//' '//op//' '//txt(b)%str//')'
    end function relational

end subroutine node_text
!*****************************************************************************************

!*****************************************************************************************
!>
!  The statements for the value and derivative of a node in the jacobian routine.
!  `vb` and `db` are the names (or texts) of the values and derivatives of the nodes
!  (`db` is empty for a zero derivative). The temporaries of the node have been created.

subroutine jacobian_node(g, tree, info, i, vb, db)

    type(codegen_t), intent(inout) :: g !! the code generator
    type(expr_tree), intent(in) :: tree !! the expression tree
    type(tree_info), intent(in) :: info !! information about the tree
    integer, intent(in) :: i !! the node
    type(string_t), dimension(:), intent(inout) :: vb !! the values of the nodes
    type(string_t), dimension(:), intent(inout) :: db !! the derivatives of the nodes

    integer :: n, a, b, c, k, o, m, ie
    type(string_t), dimension(:), allocatable :: txt
    integer, dimension(:), allocatable :: prec
    character(len=:), allocatable :: v, d, ix, t
    logical :: loop

    n = info%nsz(i)
    v = vb(i)%str
    d = db(i)%str
    associate (node => tree%nodes(i))
        if (node%na > 0) a = arg(tree, i, 1)
        if (node%na > 1) b = arg(tree, i, 2)

        select case (node%kind)

        case (node_unary, node_binary)
            call value_statement()
            if (len(d) == 0) return
            call elementwise_derivative()

        case (node_builtin)
            select case (node%op)
            case (1:fn_last_elemental, fn_atan2, fn_mod, fn_modulo, fn_sign, fn_hypot, fn_atan2d)
                call value_statement()
                if (len(d) == 0) return
                call elementwise_derivative()
            case (fn_max, fn_min)
                loop = n > 1
                if (loop) then
                    g%use_i = .true.
                    call statement(g, 'do i = 1, '//int_to_str(n))
                    g%indent = g%indent + 1
                end if
                call statement(g, el(v, n, 'i')//' = '//el(vb(a)%str, info%nsz(a), 'i'))
                if (len(d) > 0) call statement(g, row(d, n, 'i')//' = '//row_or_zero(a, 'i'))
                do k = 2, node%na
                    c = arg(tree, i, k)
                    if (node%op == fn_max) then
                        call statement(g, 'if ('//el(vb(c)%str, info%nsz(c), 'i')//' > '//el(v, n, 'i')//') then')
                    else
                        call statement(g, 'if ('//el(vb(c)%str, info%nsz(c), 'i')//' < '//el(v, n, 'i')//') then')
                    end if
                    g%indent = g%indent + 1
                    call statement(g, el(v, n, 'i')//' = '//el(vb(c)%str, info%nsz(c), 'i'))
                    if (len(d) > 0) call statement(g, row(d, n, 'i')//' = '//row_or_zero(c, 'i'))
                    g%indent = g%indent - 1
                    call statement(g, 'end if')
                end do
                if (loop) then
                    g%indent = g%indent - 1
                    call statement(g, 'end do')
                end if
            case (fn_sum)
                if (info%nsz(a) == 1) then
                    call copy(a)
                else
                    call statement(g, v//' = sum('//vb(a)%str//')')
                    if (len(d) > 0) call statement(g, d//' = sum('//db(a)%str//', dim=1)')
                end if
            case (fn_product)
                m = info%nsz(a)
                if (m == 1) then
                    call copy(a)
                else
                    call statement(g, v//' = product('//vb(a)%str//')')
                    if (len(d) > 0) then
                        g%use_i = .true.
                        call statement(g, d//' = 0.0_wp')
                        call statement(g, 'do i = 1, '//int_to_str(m))
                        call statement(g, '    '//d//' = '//d//' + product('//vb(a)%str//'(1:i-1))*product('//&
                                       vb(a)%str//'(i+1:'//int_to_str(m)//'))*'//db(a)%str//'(i,:)')
                        call statement(g, 'end do')
                    end if
                end if
            case (fn_norm2)
                if (info%nsz(a) == 1) then
                    call statement(g, v//' = abs('//vb(a)%str//')')
                    if (len(d) > 0) call statement(g, d//' = merge('//vb(a)%str//'/'//v//', 0.0_wp, '//v//&
                                                   ' > 0.0_wp)*'//db(a)%str)
                else
                    call statement(g, v//' = norm2('//vb(a)%str//')')
                    if (len(d) > 0) then
                        call statement(g, 'if ('//v//' > 0.0_wp) then')
                        call statement(g, '    '//d//' = matmul('//vb(a)%str//', '//db(a)%str//')/'//v)
                        call statement(g, 'else')
                        call statement(g, '    '//d//' = 0.0_wp')
                        call statement(g, 'end if')
                    end if
                end if
            case (fn_maxval, fn_minval)
                if (info%nsz(a) == 1) then
                    call copy(a)
                else
                    g%use_j = .true.
                    if (node%op == fn_maxval) then
                        call statement(g, 'j = maxloc('//vb(a)%str//', dim=1)')
                    else
                        call statement(g, 'j = minloc('//vb(a)%str//', dim=1)')
                    end if
                    call statement(g, v//' = '//vb(a)%str//'(j)')
                    if (len(d) > 0) call statement(g, d//' = '//db(a)%str//'(j,:)')
                end if
            case (fn_size)
                call statement(g, v//' = '//num(real(info%nsz(a), wp)))
            case (fn_dot)
                if (info%nsz(a) == 1) then
                    call statement(g, v//' = '//vb(a)%str//'*'//vb(b)%str)
                    if (len(d) > 0) call statement(g, d//' = '//sum_terms([term(vb(b)%str, db(a)%str), &
                                                                         term(vb(a)%str, db(b)%str)]))
                else
                    call statement(g, v//' = dot_product('//vb(a)%str//', '//vb(b)%str//')')
                    if (len(d) > 0) call statement(g, d//' = '//sum_terms([ &
                        matmul_term(vb(b)%str, db(a)%str), matmul_term(vb(a)%str, db(b)%str)]))
                end if
            case (fn_cross)
                g%use_cross = .true.
                call statement(g, v//' = '//g%fn_cross//'('//vb(a)%str//', '//vb(b)%str//')')
                if (len(d) > 0) then
                    call cross_row(1, 2, 3)
                    call cross_row(2, 3, 1)
                    call cross_row(3, 1, 2)
                end if
            case (fn_if) ! (with a vector condition)
                c = arg(tree, i, 3)
                g%use_i = .true.
                call statement(g, 'do i = 1, '//int_to_str(n))
                call statement(g, '    if ('//el(vb(c)%str, info%nsz(c), 'i')//' /= 0.0_wp) then')
                call statement(g, '        '//el(v, n, 'i')//' = '//el(vb(a)%str, info%nsz(a), 'i'))
                if (len(d) > 0) call statement(g, '        '//row(d, n, 'i')//' = '//row_or_zero(a, 'i'))
                call statement(g, '    else')
                call statement(g, '        '//el(v, n, 'i')//' = '//el(vb(b)%str, info%nsz(b), 'i'))
                if (len(d) > 0) call statement(g, '        '//row(d, n, 'i')//' = '//row_or_zero(b, 'i'))
                call statement(g, '    end if')
                call statement(g, 'end do')
            end select

        case (node_vector)
            if (n == 1) then
                call copy(a)
            else
                t = '['//vb(a)%str
                do k = 2, node%na
                    t = t//', '//vb(arg(tree, i, k))%str
                end do
                call statement(g, v//' = '//t//']')
                if (len(d) > 0) then
                    o = 0
                    do k = 1, node%na
                        c = arg(tree, i, k)
                        m = info%nsz(c)
                        if (m == 1) then
                            ix = int_to_str(o + 1)
                        else
                            ix = int_to_str(o + 1)//':'//int_to_str(o + m)
                        end if
                        if (len(db(c)%str) > 0) then
                            call statement(g, d//'('//ix//',:) = '//db(c)%str)
                        else
                            call statement(g, d//'('//ix//',:) = 0.0_wp')
                        end if
                        o = o + m
                    end do
                end if
            end if

        case (node_index)
            if (info%nsz(a) == 1) then
                call copy(a)
            else
                if (int_constant(tree, b, ie)) then
                    ix = int_to_str(ie)
                else
                    g%use_j = .true.
                    call statement(g, 'j = nint('//vb(b)%str//')')
                    ix = 'j'
                end if
                call statement(g, v//' = '//vb(a)%str//'('//ix//')')
                if (len(d) > 0) call statement(g, d//' = '//db(a)%str//'('//ix//',:)')
            end if

        end select
    end associate

contains

    subroutine value_statement()
        !! the value of an element-wise operation, using the values of the arguments
        integer :: j
        allocate(txt(size(vb)), prec(size(vb)))
        do j = 1, tree%nodes(i)%na
            c = arg(tree, i, j)
            txt(c)%str = vb(c)%str
            prec(c) = prec_primary
        end do
        call node_text(g, tree, info, i, txt, prec)
        call statement(g, v//' = '//txt(i)%str)
    end subroutine value_statement

    subroutine elementwise_derivative()
        !! the derivative of an element-wise operation (a loop over the elements for a vector)
        character(len=:), allocatable :: rhs, x, y, f
        loop = n > 1
        if (loop) then
            g%use_i = .true.
            call statement(g, 'do i = 1, '//int_to_str(n))
            g%indent = g%indent + 1
        end if
        x = el(vb(a)%str, info%nsz(a), 'i')
        f = el(v, n, 'i')
        if (tree%nodes(i)%na > 1) then
            y = el(vb(b)%str, info%nsz(b), 'i')
            rhs = sum_terms([term(partial(1, x, y, f), drow(a)), term(partial(2, x, y, f), drow(b))])
        else
            rhs = sum_terms([term(partial(1, x, x, f), drow(a))])
        end if
        call statement(g, row(d, n, 'i')//' = '//rhs)
        if (loop) then
            g%indent = g%indent - 1
            call statement(g, 'end do')
        end if
    end subroutine elementwise_derivative

    function partial(iarg, x, y, f) result(s)
        !! the partial derivative of node `i` with respect to its argument `iarg`
        !! (empty if it is zero)
        integer, intent(in) :: iarg !! the argument (1 or 2)
        character(len=*), intent(in) :: x !! the first argument
        character(len=*), intent(in) :: y !! the second argument
        character(len=*), intent(in) :: f !! the value of the node
        character(len=:), allocatable :: s !! the partial derivative
        integer :: k2
        s = ''
        associate (node => tree%nodes(i))
            select case (node%kind)
            case (node_unary) ! (op_neg; the derivative of op_not is zero)
                s = '-1.0_wp'
            case (node_binary)
                select case (node%op)
                case (op_add)
                    s = '1.0_wp'
                case (op_sub)
                    s = merge('1.0_wp ', '-1.0_wp', iarg == 1)
                    s = trim(s)
                case (op_mul)
                    if (iarg == 1) then
                        s = y
                    else
                        s = x
                    end if
                case (op_div)
                    if (iarg == 1) then
                        s = '1.0_wp/'//y
                    else
                        s = '-'//f//'/'//y
                    end if
                case (op_pow)
                    if (iarg == 1) then
                        if (int_constant(tree, b, k2)) then
                            if (k2 == 0) then
                                s = '0.0_wp'
                            else if (k2 == 1) then
                                s = '1.0_wp'
                            else if (k2 == 2) then
                                s = '2.0_wp*'//x
                            else
                                s = num(real(k2, wp))//'*'//x//'**'//int_text(k2 - 1)
                            end if
                        else
                            g%use_pow = .true.
                            s = 'merge(0.0_wp, '//y//'*'//g%fn_pow//'('//x//', '//y//' - 1.0_wp), '//y//' == 0.0_wp)'
                        end if
                    else
                        s = 'merge('//f//'*log('//x//'), 0.0_wp, '//x//' > 0.0_wp)'
                    end if
                end select
            case (node_builtin)
                select case (node%op)
                case (fn_abs);   s = 'sign(1.0_wp, '//x//')'
                case (fn_sqrt);  s = '0.5_wp/'//f
                case (fn_exp);   s = f
                case (fn_log);   s = '1.0_wp/'//x
                case (fn_log10); s = '1.0_wp/('//x//'*'//num(log(10.0_wp))//')'
                case (fn_sin);   s = 'cos('//x//')'
                case (fn_cos);   s = '-sin('//x//')'
                case (fn_tan);   s = '1.0_wp + '//f//'**2'
                case (fn_asin);  s = '1.0_wp/sqrt(1.0_wp - '//x//'**2)'
                case (fn_acos);  s = '-1.0_wp/sqrt(1.0_wp - '//x//'**2)'
                case (fn_atan);  s = '1.0_wp/(1.0_wp + '//x//'**2)'
                case (fn_sinh);  s = 'cosh('//x//')'
                case (fn_cosh);  s = 'sinh('//x//')'
                case (fn_tanh);  s = '1.0_wp - '//f//'**2'
                case (fn_asinh); s = '1.0_wp/sqrt('//x//'**2 + 1.0_wp)'
                case (fn_acosh); s = '1.0_wp/sqrt('//x//'**2 - 1.0_wp)'
                case (fn_atanh); s = '1.0_wp/(1.0_wp - '//x//'**2)'
                case (fn_erf);   s = num(2.0_wp/sqrt(pi))//'*exp(-'//x//'**2)'
                case (fn_erfc);  s = '-'//num(2.0_wp/sqrt(pi))//'*exp(-'//x//'**2)'
                case (fn_sind);  s = num(pi/180.0_wp)//'*cosd('//x//')'
                case (fn_cosd);  s = '-'//num(pi/180.0_wp)//'*sind('//x//')'
                case (fn_tand);  s = num(pi/180.0_wp)//'*(1.0_wp + '//f//'**2)'
                case (fn_asind); s = num(180.0_wp/pi)//'/sqrt(1.0_wp - '//x//'**2)'
                case (fn_acosd); s = '-'//num(180.0_wp/pi)//'/sqrt(1.0_wp - '//x//'**2)'
                case (fn_atand); s = num(180.0_wp/pi)//'/(1.0_wp + '//x//'**2)'
                case (fn_atan2)
                    if (iarg == 1) then
                        s = y//'/('//x//'**2 + '//y//'**2)'
                    else
                        s = '-'//x//'/('//x//'**2 + '//y//'**2)'
                    end if
                case (fn_atan2d)
                    if (iarg == 1) then
                        s = num(180.0_wp/pi)//'*'//y//'/('//x//'**2 + '//y//'**2)'
                    else
                        s = '-'//num(180.0_wp/pi)//'*'//x//'/('//x//'**2 + '//y//'**2)'
                    end if
                case (fn_mod)
                    if (iarg == 1) then
                        s = '1.0_wp'
                    else
                        s = '-aint('//x//'/'//y//')'
                    end if
                case (fn_modulo)
                    if (iarg == 1) then
                        s = '1.0_wp'
                    else
                        g%use_floor = .true.
                        s = '-'//g%fn_floor//'('//x//'/'//y//')'
                    end if
                case (fn_sign)
                    if (iarg == 1) s = 'sign(1.0_wp, '//x//')*sign(1.0_wp, '//y//')'
                case (fn_hypot)
                    if (iarg == 1) then
                        s = 'merge('//x//'/'//f//', 0.0_wp, '//f//' > 0.0_wp)'
                    else
                        s = 'merge('//y//'/'//f//', 0.0_wp, '//f//' > 0.0_wp)'
                    end if
                end select
            end select
        end associate
    end function partial

    function drow(cc) result(s)
        !! the derivative of argument `cc` for element `i` (empty if it is zero)
        integer, intent(in) :: cc !! the argument node
        character(len=:), allocatable :: s !! the text
        if (len(db(cc)%str) == 0) then
            s = ''
        else
            s = row(db(cc)%str, info%nsz(cc), 'i')
        end if
    end function drow

    function row_or_zero(cc, idx) result(s)
        !! the derivative of argument `cc` for element `idx`, or zero
        integer, intent(in) :: cc !! the argument node
        character(len=*), intent(in) :: idx !! the element index
        character(len=:), allocatable :: s !! the text
        if (len(db(cc)%str) == 0) then
            s = '0.0_wp'
        else
            s = row(db(cc)%str, info%nsz(cc), idx)
        end if
    end function row_or_zero

    subroutine copy(cc)
        !! the value and derivative of the node are those of argument `cc`
        integer, intent(in) :: cc !! the argument node
        call statement(g, v//' = '//vb(cc)%str)
        if (len(d) > 0) call statement(g, d//' = '//row_or_zero(cc, ''))
    end subroutine copy

    subroutine cross_row(r, p, q)
        !! row `r` of the derivative of the cross product: `da(p)*b(q) - da(q)*b(p) + a(p)*db(q) - a(q)*db(p)`
        integer, intent(in) :: r !! the row
        integer, intent(in) :: p !! the next component
        integer, intent(in) :: q !! the component after that
        character(len=:), allocatable :: sp, sq
        sp = int_to_str(p)
        sq = int_to_str(q)
        call statement(g, d//'('//int_to_str(r)//',:) = '//sum_terms([ &
            term(vb(b)%str//'('//sq//')', db_row(a, sp)), term('-'//vb(b)%str//'('//sp//')', db_row(a, sq)), &
            term(vb(a)%str//'('//sp//')', db_row(b, sq)), term('-'//vb(a)%str//'('//sq//')', db_row(b, sp))]))
    end subroutine cross_row

    function db_row(cc, r) result(s)
        !! row `r` of the derivative of argument `cc` (empty if it is zero)
        integer, intent(in) :: cc !! the argument node
        character(len=*), intent(in) :: r !! the row
        character(len=:), allocatable :: s !! the text
        if (len(db(cc)%str) == 0) then
            s = ''
        else
            s = db(cc)%str//'('//r//',:)'
        end if
    end function db_row

end subroutine jacobian_node
!*****************************************************************************************

!*****************************************************************************************
!>
!  The Fortran logical expression for a condition (a node that is true if nonzero):
!  the comparison itself for a relational operator, otherwise `value /= 0`.

function condition(tree, c, txt) result(s)
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: c !! the condition node
    type(string_t), dimension(:), intent(in) :: txt !! the text of each node
    character(len=:), allocatable :: s !! the logical expression
    character(len=:), allocatable :: op
    op = ''
    if (tree%nodes(c)%kind == node_binary) then
        select case (tree%nodes(c)%op)
        case (op_eq); op = ' == '
        case (op_ne); op = ' /= '
        case (op_lt); op = ' < '
        case (op_le); op = ' <= '
        case (op_gt); op = ' > '
        case (op_ge); op = ' >= '
        end select
    end if
    if (len(op) > 0) then
        s = txt(arg(tree, c, 1))%str//op//txt(arg(tree, c, 2))%str
    else
        s = txt(c)%str//' /= 0.0_wp'
    end if
end function condition
!*****************************************************************************************

!*****************************************************************************************
!>
!  A term `coefficient*derivative` (empty if either is empty or the coefficient is zero).

pure function term(coef, deriv) result(s)
    character(len=*), intent(in) :: coef !! the coefficient
    character(len=*), intent(in) :: deriv !! the derivative
    type(string_t) :: s !! the term
    if (len(coef) == 0 .or. len(deriv) == 0 .or. coef == '0.0_wp') then
        s%str = ''
    else if (coef == '1.0_wp') then
        s%str = deriv
    else if (coef == '-1.0_wp') then
        s%str = '-'//deriv
    else if (scan(coef, '+-*/ ') > 0) then
        s%str = '('//coef//')*'//deriv
    else
        s%str = coef//'*'//deriv
    end if
end function term
!*****************************************************************************************

!*****************************************************************************************
!>
!  A term `matmul(vector, derivative)` (empty if the derivative is empty).

pure function matmul_term(vec, deriv) result(s)
    character(len=*), intent(in) :: vec !! the vector
    character(len=*), intent(in) :: deriv !! the derivative
    type(string_t) :: s !! the term
    if (len(deriv) == 0) then
        s%str = ''
    else
        s%str = 'matmul('//vec//', '//deriv//')'
    end if
end function matmul_term
!*****************************************************************************************

!*****************************************************************************************
!>
!  The sum of a list of terms (`0.0_wp` if they are all empty).

pure function sum_terms(terms) result(s)
    type(string_t), dimension(:), intent(in) :: terms !! the terms
    character(len=:), allocatable :: s !! the sum
    integer :: k
    s = ''
    do k = 1, size(terms)
        if (len(terms(k)%str) == 0) cycle
        if (len(s) == 0) then
            s = terms(k)%str
        else if (terms(k)%str(1:1) == '-') then
            s = s//' - '//terms(k)%str(2:)
        else
            s = s//' + '//terms(k)%str
        end if
    end do
    if (len(s) == 0) s = '0.0_wp'
end function sum_terms
!*****************************************************************************************

!*****************************************************************************************
!>
!  Element `idx` of a value of size `n` (the value itself if it is a scalar).

pure function el(base, n, idx) result(s)
    character(len=*), intent(in) :: base !! the value
    integer, intent(in) :: n !! the size of the value
    character(len=*), intent(in) :: idx !! the element index
    character(len=:), allocatable :: s !! the text
    if (n == 1 .or. len(idx) == 0) then
        s = base
    else
        s = base//'('//idx//')'
    end if
end function el
!*****************************************************************************************

!*****************************************************************************************
!>
!  Row `idx` of the derivative of a value of size `n` (the whole derivative if the
!  value is a scalar).

pure function row(base, n, idx) result(s)
    character(len=*), intent(in) :: base !! the derivative
    integer, intent(in) :: n !! the size of the value
    character(len=*), intent(in) :: idx !! the element index
    character(len=:), allocatable :: s !! the text
    if (n == 1 .or. len(idx) == 0) then
        s = base
    else
        s = base//'('//idx//',:)'
    end if
end function row
!*****************************************************************************************

!*****************************************************************************************
!>
!  Argument `j` of node `i`.

pure integer function arg(tree, i, j)
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: i !! the node
    integer, intent(in) :: j !! the argument number
    arg = tree%args(tree%nodes(i)%ia + j)
end function arg
!*****************************************************************************************

!*****************************************************************************************
!>
!  If a node is the conditional `if(a, b, cond)`.

pure logical function is_if(tree, i)
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: i !! the node
    is_if = tree%nodes(i)%kind == node_builtin .and. tree%nodes(i)%op == fn_if
end function is_if
!*****************************************************************************************

!*****************************************************************************************
!>
!  If a node is a constant with an integer value (that fits in a default integer).

logical function int_constant(tree, i, k)
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: i !! the node
    integer, intent(out) :: k !! the value
    k = 0
    int_constant = .false.
    if (tree%nodes(i)%kind /= node_const) return
    associate (x => tree%nodes(i)%value)
        if (abs(x) < 1.0e9_wp .and. x == aint(x)) then
            k = nint(x)
            int_constant = .true.
        end if
    end associate
end function int_constant
!*****************************************************************************************

!*****************************************************************************************
!>
!  An integer, in parentheses if it is negative (for an exponent).

pure function int_text(k) result(s)
    integer, intent(in) :: k !! the integer
    character(len=:), allocatable :: s !! the text
    if (k < 0) then
        s = '('//int_to_str(k)//')'
    else
        s = int_to_str(k)
    end if
end function int_text
!*****************************************************************************************

!*****************************************************************************************
!>
!  The text of an index: an integer literal for a constant, otherwise `nint(...)`.

function index_text(tree, b, txt) result(s)
    type(expr_tree), intent(in) :: tree !! the expression tree
    integer, intent(in) :: b !! the index node
    character(len=*), intent(in) :: txt !! the text of the index node
    character(len=:), allocatable :: s !! the text
    integer :: k
    if (int_constant(tree, b, k)) then
        s = int_to_str(k)
    else
        s = 'nint('//txt//')'
    end if
end function index_text
!*****************************************************************************************

!*****************************************************************************************
!>
!  The Fortran name of an elemental function or reduction.

pure function elemental_name(id) result(name)
    integer, intent(in) :: id !! the function id
    character(len=:), allocatable :: name !! the Fortran name
    select case (id)
    case (fn_abs);     name = 'abs'
    case (fn_sqrt);    name = 'sqrt'
    case (fn_exp);     name = 'exp'
    case (fn_log);     name = 'log'
    case (fn_log10);   name = 'log10'
    case (fn_sin);     name = 'sin'
    case (fn_cos);     name = 'cos'
    case (fn_tan);     name = 'tan'
    case (fn_asin);    name = 'asin'
    case (fn_acos);    name = 'acos'
    case (fn_atan);    name = 'atan'
    case (fn_sinh);    name = 'sinh'
    case (fn_cosh);    name = 'cosh'
    case (fn_tanh);    name = 'tanh'
    case (fn_asinh);   name = 'asinh'
    case (fn_acosh);   name = 'acosh'
    case (fn_atanh);   name = 'atanh'
    case (fn_erf);     name = 'erf'
    case (fn_erfc);    name = 'erfc'
    case (fn_nint);    name = 'anint'
    case (fn_aint);    name = 'aint'
    case (fn_sind);    name = 'sind'
    case (fn_cosd);    name = 'cosd'
    case (fn_tand);    name = 'tand'
    case (fn_asind);   name = 'asind'
    case (fn_acosd);   name = 'acosd'
    case (fn_atand);   name = 'atand'
    case (fn_atan2);   name = 'atan2'
    case (fn_atan2d);  name = 'atan2d'
    case (fn_mod);     name = 'mod'
    case (fn_modulo);  name = 'modulo'
    case (fn_sign);    name = 'sign'
    case (fn_hypot);   name = 'hypot'
    case (fn_max);     name = 'max'
    case (fn_min);     name = 'min'
    case (fn_sum);     name = 'sum'
    case (fn_product); name = 'product'
    case (fn_maxval);  name = 'maxval'
    case (fn_minval);  name = 'minval'
    case default;      name = '?'
    end select
end function elemental_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  A real literal, with the fewest digits that give exactly the same value.

function num(x) result(s)

    real(wp), intent(in) :: x !! the value (finite)
    character(len=:), allocatable :: s !! the literal (with the `_wp` kind)

    character(len=40) :: buf
    character(len=:), allocatable :: digits, fmt
    real(wp) :: y
    integer :: d, iexp, ipos, ios

    if (x == 0.0_wp) then
        s = '0.0_wp'
        return
    end if
    do d = 1, 17
        fmt = '(ES30.'//int_to_str(d - 1)//'E3)'
        write(buf, fmt) abs(x)
        read(buf, *, iostat=ios) y
        if (ios == 0 .and. y == abs(x)) exit
    end do
    ! the digits and the decimal exponent (the value is 0.digits * 10**(iexp+1)):
    buf = adjustl(buf)
    ipos = index(buf, 'E')
    read(buf(ipos+1:), *) iexp
    digits = buf(1:ipos-1)
    ipos = index(digits, '.')
    if (ipos > 0) digits = digits(1:ipos-1)//digits(ipos+1:)
    do while (len(digits) > 1 .and. digits(len(digits):len(digits)) == '0')
        digits = digits(1:len(digits)-1)
    end do
    if (iexp >= 0 .and. iexp < 16) then
        if (len(digits) <= iexp + 1) then
            s = digits//repeat('0', iexp + 1 - len(digits))//'.0'
        else
            s = digits(1:iexp+1)//'.'//digits(iexp+2:)
        end if
    else if (iexp < 0 .and. iexp >= -5) then
        s = '0.'//repeat('0', -iexp - 1)//digits
    else
        if (len(digits) == 1) then
            s = digits//'.0e'//int_to_str(iexp)
        else
            s = digits(1:1)//'.'//digits(2:)//'e'//int_to_str(iexp)
        end if
    end if
    s = s//'_wp'
    if (x < 0.0_wp) s = '-'//s

end function num
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add an identifier (returns it).

function add_ident(g, name) result(s)
    type(codegen_t), intent(inout) :: g !! the code generator
    character(len=*), intent(in) :: name !! the identifier
    character(len=:), allocatable :: s !! the identifier
    type(string_t), dimension(:), allocatable :: tmp
    if (g%nid == size(g%idents)) then
        allocate(tmp(2*size(g%idents)))
        tmp(1:g%nid) = g%idents(1:g%nid)
        call move_alloc(tmp, g%idents)
    end if
    g%nid = g%nid + 1
    g%idents(g%nid)%str = name
    call g%table%insert(g%idents, g%nid)
    s = name
end function add_ident
!*****************************************************************************************

!*****************************************************************************************
!>
!  A unique identifier: the name, with underscores appended if it is already used.

function unique_ident(g, name) result(s)
    type(codegen_t), intent(inout) :: g !! the code generator
    character(len=*), intent(in) :: name !! the preferred identifier
    character(len=:), allocatable :: s !! the identifier
    s = name
    do while (g%table%find(g%idents, s) > 0)
        s = s//'_'
    end do
    s = add_ident(g, s)
end function unique_ident
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a temporary variable (of size `n`, or shape `[n, m]` if `m > 0`).

function new_temp(g, prefix, n, m, array) result(name)
    type(codegen_t), intent(inout) :: g !! the code generator
    character(len=*), intent(in) :: prefix !! the prefix of the name
    integer, intent(in) :: n !! the size (first dimension)
    integer, intent(in) :: m !! the second dimension (0 for a vector or scalar)
    logical, intent(in), optional :: array !! if it is an array even if `n` is 1 (a derivative)
    character(len=:), allocatable :: name !! the name of the temporary
    g%ntemp = g%ntemp + 1
    name = unique_ident(g, prefix//int_to_str(g%ntemp))
    call declare(g, name, n, m, array)
end function new_temp
!*****************************************************************************************

!*****************************************************************************************
!>
!  Declare a local variable.

subroutine declare(g, name, n, m, array)
    type(codegen_t), intent(inout) :: g !! the code generator
    character(len=*), intent(in) :: name !! the name
    integer, intent(in) :: n !! the size (first dimension)
    integer, intent(in) :: m !! the second dimension (0 for a vector or scalar)
    logical, intent(in), optional :: array !! if it is an array even if `n` is 1 (a derivative)
    call g%decl%add('    '//declaration(name, n, m, '', array)//nl)
end subroutine declare
!*****************************************************************************************

!*****************************************************************************************
!>
!  The declaration of a real variable.

pure function declaration(name, n, m, attr, array) result(s)
    character(len=*), intent(in) :: name !! the name
    integer, intent(in) :: n !! the size (first dimension)
    integer, intent(in) :: m !! the second dimension (0 for a vector or scalar)
    character(len=*), intent(in) :: attr !! other attributes (e.g., `, intent(in)`)
    logical, intent(in), optional :: array !! if it is an array even if `n` is 1 (a derivative)
    character(len=:), allocatable :: s !! the declaration
    logical :: is_array
    is_array = n > 1
    if (present(array)) is_array = is_array .or. array
    if (m > 0) then
        s = 'real(wp), dimension('//int_to_str(n)//', '//int_to_str(m)//')'//attr//' :: '//name
    else if (is_array) then
        s = 'real(wp), dimension('//int_to_str(n)//')'//attr//' :: '//name
    else
        s = 'real(wp)'//attr//' :: '//name
    end if
end function declaration
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a new routine.

subroutine start_routine(g)
    type(codegen_t), intent(inout) :: g !! the code generator
    g%decl = string_builder()
    g%body = string_builder()
    g%indent = 1
    g%use_i = .false.
    g%use_j = .false.
    g%ntemp = 0
end subroutine start_routine
!*****************************************************************************************

!*****************************************************************************************
!>
!  Finish a routine: write its local declarations and statements.

subroutine end_routine(g, out)
    type(codegen_t), intent(inout) :: g !! the code generator
    type(string_builder), intent(inout) :: out !! the output
    character(len=:), allocatable :: ints
    ints = ''
    if (g%use_i) ints = 'i'
    if (g%use_j) then
        if (len(ints) > 0) ints = ints//', '
        ints = ints//'j'
    end if
    if (len(ints) > 0) call out%add('    integer :: '//ints//nl)
    call out%add(g%decl%get())
    call out%add(nl)
    call out%add(g%body%get())
end subroutine end_routine
!*****************************************************************************************

!*****************************************************************************************
!>
!  The argument list of a routine.

function argument_list(g, nin, nout, jac) result(s)
    type(codegen_t), intent(in) :: g !! the code generator
    integer, intent(in) :: nin !! number of inputs
    integer, intent(in) :: nout !! number of outputs
    logical, intent(in) :: jac !! if the jacobians are included
    character(len=:), allocatable :: s !! the argument list
    integer :: i
    s = ''
    do i = 1, nin + nout
        if (i > 1) s = s//', '
        s = s//g%uname(i)%str
    end do
    if (jac) then
        do i = nin + 1, nin + nout
            s = s//', '//g%dname(i)%str
        end do
    end if
end function argument_list
!*****************************************************************************************

!*****************************************************************************************
!>
!  Declare the arguments of a routine.

subroutine declare_arguments(g, nin, nout, jac, out)
    type(codegen_t), intent(in) :: g !! the code generator
    integer, intent(in) :: nin !! number of inputs
    integer, intent(in) :: nout !! number of outputs
    logical, intent(in) :: jac !! if the jacobians are included
    type(string_builder), intent(inout) :: out !! the output
    integer :: i
    do i = 1, nin
        call out%add('    '//declaration(g%uname(i)%str, g%vsize(i), 0, ', intent(in)')//nl)
    end do
    do i = nin + 1, nin + nout
        call out%add('    '//declaration(g%uname(i)%str, g%vsize(i), 0, ', intent(out)')//nl)
    end do
    if (jac) then
        do i = nin + 1, nin + nout
            if (g%vsize(i) == 1) then
                call out%add('    '//declaration(g%dname(i)%str, g%nd, 0, ', intent(out)', array=.true.)//nl)
            else
                call out%add('    '//declaration(g%dname(i)%str, g%vsize(i), g%nd, ', intent(out)')//nl)
            end if
        end do
    end if
end subroutine declare_arguments
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a statement to the routine body (with continuation lines if it is long).

subroutine statement(g, text)
    type(codegen_t), intent(inout) :: g !! the code generator
    character(len=*), intent(in) :: text !! the statement
    call g%body%add(wrap_text(repeat('    ', g%indent)//text, 4*g%indent + 4)//nl)
end subroutine statement
!*****************************************************************************************

!*****************************************************************************************
!>
!  Split a long line of code into continuation lines (at spaces).

function wrap_text(text, indent) result(s)
    character(len=*), intent(in) :: text !! the line
    integer, intent(in) :: indent !! the indentation of the continuation lines
    character(len=:), allocatable :: s !! the line, with continuation lines
    character(len=:), allocatable :: rest
    integer :: k, lead
    s = ''
    rest = text
    do while (len(rest) > max_line)
        lead = verify(rest, ' ') - 1 ! (the indentation)
        k = index(rest(1:max_line), ' ', back=.true.)
        if (k <= lead + 1) then
            ! no space before the limit: split at the next one
            k = index(rest(max_line+1:), ' ')
            if (k == 0) exit
            k = k + max_line
        end if
        s = s//rest(1:k-1)//' &'//nl
        rest = repeat(' ', indent)//rest(k+1:)
    end do
    s = s//rest
end function wrap_text
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a comment (split into several lines if it is long).

subroutine comment(out, indent, text)
    type(string_builder), intent(inout) :: out !! the output
    integer, intent(in) :: indent !! the indentation
    character(len=*), intent(in) :: text !! the comment
    character(len=:), allocatable :: rest, prefix
    integer :: k
    prefix = repeat(' ', indent)//'!  '
    rest = text
    do while (len(prefix) + len(rest) > max_line)
        k = index(rest(1:max_line-len(prefix)), ' ', back=.true.)
        if (k <= 1) exit
        call out%add(prefix//rest(1:k-1)//nl)
        rest = '    '//rest(k+1:)
    end do
    call out%add(prefix//rest//nl)
end subroutine comment
!*****************************************************************************************

!*****************************************************************************************
!>
!  A note on the size of a variable (for the comments).

pure function size_note(n) result(s)
    integer, intent(in) :: n !! the size
    character(len=:), allocatable :: s !! the note
    if (n > 1) then
        s = '('//int_to_str(n)//')'
    else
        s = ''
    end if
end function size_note
!*****************************************************************************************

!*****************************************************************************************
!>
!  A note on a renamed variable (for the comments).

pure function renamed_note(name, uname) result(s)
    character(len=*), intent(in) :: name !! the variable name
    character(len=*), intent(in) :: uname !! the name in the generated code
    character(len=:), allocatable :: s !! the note
    if (name /= uname) then
        s = ' (the variable "'//name//'", renamed)'
    else
        s = ''
    end if
end function renamed_note
!*****************************************************************************************

!*****************************************************************************************
!>
!  A note on the columns of a derivative variable (for the comments).

pure function column_note(offset, n) result(s)
    integer, intent(in) :: offset !! the column offset
    integer, intent(in) :: n !! the size of the variable
    character(len=:), allocatable :: s !! the note
    if (n > 1) then
        s = 's '//int_to_str(offset + 1)//' to '//int_to_str(offset + n)
    else
        s = ' '//int_to_str(offset + 1)
    end if
end function column_note
!*****************************************************************************************

!*****************************************************************************************
!>
!  The helper functions used by the generated code.

subroutine helpers(g, out)
    type(codegen_t), intent(in) :: g !! the code generator
    type(string_builder), intent(inout) :: out !! the output
    if (g%use_pow) then
        call out%add(nl//'pure elemental real(wp) function '//g%fn_pow//'(a, b)'//nl)
        call out%add('    !! `a**b` (with an integer power if `b` is an integer)'//nl)
        call out%add('    real(wp), intent(in) :: a, b'//nl)
        call out%add('    if (b == 2.0_wp) then'//nl)
        call out%add('        '//g%fn_pow//' = a*a'//nl)
        call out%add('    else if (abs(b) < real(huge(1), wp) .and. b == aint(b)) then'//nl)
        call out%add('        '//g%fn_pow//' = a**nint(b)'//nl)
        call out%add('    else'//nl)
        call out%add('        '//g%fn_pow//' = a**b'//nl)
        call out%add('    end if'//nl)
        call out%add('end function '//g%fn_pow//nl)
    end if
    if (g%use_floor) then
        call out%add(nl//'pure elemental real(wp) function '//g%fn_floor//'(x)'//nl)
        call out%add('    !! the largest integer that is not greater than `x` (as a real)'//nl)
        call out%add('    real(wp), intent(in) :: x'//nl)
        call out%add('    '//g%fn_floor//' = aint(x)'//nl)
        call out%add('    if ('//g%fn_floor//' > x) '//g%fn_floor//' = '//g%fn_floor//' - 1.0_wp'//nl)
        call out%add('end function '//g%fn_floor//nl)
    end if
    if (g%use_cross) then
        call out%add(nl//'pure function '//g%fn_cross//'(a, b) result(c)'//nl)
        call out%add('    !! the cross product of two 3-element vectors'//nl)
        call out%add('    real(wp), dimension(3), intent(in) :: a, b'//nl)
        call out%add('    real(wp), dimension(3) :: c'//nl)
        call out%add('    c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]'//nl)
        call out%add('end function '//g%fn_cross//nl)
    end if
end subroutine helpers
!*****************************************************************************************

!*****************************************************************************************
end module fparse_codegen
!*****************************************************************************************
