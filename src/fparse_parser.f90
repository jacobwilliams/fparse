!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Recursive-descent parser that converts an expression string into
!  an expression tree (the internal representation used for evaluation).
!
!### Grammar
!
!  In order of increasing precedence:
!```
!  expr    := and_expr { ('||' | '.or.') and_expr }
!  and_expr:= not_expr { ('&&' | '.and.') not_expr }
!  not_expr:= ('!' | '.not.') not_expr | cmp_expr
!  cmp_expr:= add_expr { ('==' | '/=' | '<' | '<=' | '>' | '>=') add_expr }
!  add_expr:= mul_expr { ('+' | '-') mul_expr }
!  mul_expr:= unary { ('*' | '/') unary }
!  unary   := ('-' | '+' | '!') unary | power
!  power   := postfix [ ('^' | '**') unary ]           (right associative)
!  postfix := primary { '[' expr ']' }                 (indexing, 1-based)
!  primary := number | name | name '(' [expr {',' expr}] ')'
!           | '(' expr ')' | '[' expr {',' expr} ']'
!```
!
!  Note that, as in Fortran, `-x**2` is `-(x**2)`.

    module fparse_parser

    use fparse_common
    use fparse_lexer
    use fparse_functions

    implicit none

    private

    ! node kinds:
    integer, parameter, public :: node_const   = 1 !! a constant
    integer, parameter, public :: node_var     = 2 !! a variable (`op` is the variable index)
    integer, parameter, public :: node_unary   = 3 !! a unary operator (`op` is the operator)
    integer, parameter, public :: node_binary  = 4 !! a binary operator (`op` is the operator)
    integer, parameter, public :: node_builtin = 5 !! a built-in function (`op` is the function id)
    integer, parameter, public :: node_user    = 6 !! a user function (`op` is the function index)
    integer, parameter, public :: node_vector  = 7 !! a vector constructor `[a,b,...]`
    integer, parameter, public :: node_index   = 8 !! an indexing operation `a[i]`

    ! operators:
    integer, parameter, public :: op_neg = 1  !! unary `-`
    integer, parameter, public :: op_not = 2  !! logical not
    integer, parameter, public :: op_add = 3  !! `+`
    integer, parameter, public :: op_sub = 4  !! `-`
    integer, parameter, public :: op_mul = 5  !! `*`
    integer, parameter, public :: op_div = 6  !! `/`
    integer, parameter, public :: op_pow = 7  !! `^`
    integer, parameter, public :: op_eq  = 8  !! `==`
    integer, parameter, public :: op_ne  = 9  !! `/=`
    integer, parameter, public :: op_lt  = 10 !! `<`
    integer, parameter, public :: op_le  = 11 !! `<=`
    integer, parameter, public :: op_gt  = 12 !! `>`
    integer, parameter, public :: op_ge  = 13 !! `>=`
    integer, parameter, public :: op_and = 14 !! logical and
    integer, parameter, public :: op_or  = 15 !! logical or

    type, public :: node_t
        !! A node in an expression tree.
        integer :: kind = 0 !! node kind (`node_*` parameter)
        integer :: op = 0 !! operator, function id, or variable index (depending on `kind`)
        real(wp) :: value = 0.0_wp !! the value (for constants)
        integer, dimension(:), allocatable :: args !! indices of the child nodes
        integer :: pos = 0 !! position in the expression string (for error messages)
    end type node_t

    type, public :: expr_tree
        !! An expression tree. The nodes are stored in a flat array.
        type(node_t), dimension(:), allocatable :: nodes !! the nodes
        integer :: n_nodes = 0 !! number of nodes used
        integer :: root = 0 !! index of the root node
        integer, dimension(:), allocatable :: refs !! indices of the variables referenced by the expression
        type(val_t), dimension(:), allocatable :: buf !! work buffers: the value of each node
                                                      !! (reused between evaluations to avoid reallocation)
    end type expr_tree

    type :: parser_t
        !! Parser state.
        type(token_t), dimension(:), allocatable :: toks !! the tokens
        integer :: ip = 1 !! current token index
        type(expr_tree) :: tree !! the tree being built
        type(error_t) :: err !! error status
        type(string_t), dimension(:), allocatable :: vars !! variable names
        type(user_function), dimension(:), allocatable :: funcs !! user functions
    end type parser_t

    public :: parse_expression

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse an expression string into an expression tree.

    subroutine parse_expression(str, vars, funcs, tree, err)

    character(len=*), intent(in) :: str !! the expression
    type(string_t), dimension(:), intent(in) :: vars !! the variable names (lowercase)
    type(user_function), dimension(:), intent(in) :: funcs !! user-defined functions
    type(expr_tree), intent(out) :: tree !! the expression tree
    type(error_t), intent(inout) :: err !! error status

    type(parser_t) :: p
    integer :: root

    call tokenize(str, p%toks, p%err)
    if (p%err%failed) then
        err = p%err
        return
    end if
    p%vars = vars
    p%funcs = funcs
    allocate(p%tree%nodes(16))
    allocate(p%tree%refs(0))

    if (p%toks(1)%kind == tok_end) then
        call err%set('empty expression', 1)
        return
    end if

    root = parse_or(p)
    if (.not. p%err%failed) then
        if (peek(p) /= tok_end) call unexpected(p)
    end if
    if (p%err%failed) then
        err = p%err
        return
    end if

    p%tree%root = root
    p%tree%nodes = p%tree%nodes(1:p%tree%n_nodes)
    call move_alloc(p%tree%nodes, tree%nodes)
    call move_alloc(p%tree%refs, tree%refs)
    tree%n_nodes = p%tree%n_nodes
    tree%root = root
    allocate(tree%buf(tree%n_nodes))

    end subroutine parse_expression
!*****************************************************************************************

!*****************************************************************************************
!>
!  Kind of the current token.

    pure integer function peek(p)
    type(parser_t), intent(in) :: p
    peek = p%toks(p%ip)%kind
    end function peek
!*****************************************************************************************

!*****************************************************************************************
!>
!  Position of the current token.

    pure integer function cur_pos(p)
    type(parser_t), intent(in) :: p
    cur_pos = p%toks(p%ip)%pos
    end function cur_pos
!*****************************************************************************************

!*****************************************************************************************
!>
!  Raise an "unexpected token" error for the current token.

    subroutine unexpected(p)
    type(parser_t), intent(inout) :: p
    if (peek(p) == tok_end) then
        call p%err%set('unexpected end of expression', cur_pos(p))
    else
        call p%err%set('unexpected "'//p%toks(p%ip)%text//'"', cur_pos(p))
    end if
    end subroutine unexpected
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a node to the tree. Returns the node index.

    function add_node(p, kind, op, args, pos, value) result(idx)

    type(parser_t), intent(inout) :: p
    integer, intent(in) :: kind !! node kind
    integer, intent(in) :: op !! operator/function/variable
    integer, dimension(:), intent(in) :: args !! child nodes
    integer, intent(in) :: pos !! position in the string
    real(wp), intent(in), optional :: value !! constant value
    integer :: idx

    type(node_t), dimension(:), allocatable :: tmp

    if (p%tree%n_nodes == size(p%tree%nodes)) then
        allocate(tmp(2*size(p%tree%nodes)))
        tmp(1:p%tree%n_nodes) = p%tree%nodes(1:p%tree%n_nodes)
        call move_alloc(tmp, p%tree%nodes)
    end if
    p%tree%n_nodes = p%tree%n_nodes + 1
    idx = p%tree%n_nodes
    p%tree%nodes(idx)%kind = kind
    p%tree%nodes(idx)%op = op
    p%tree%nodes(idx)%args = args
    p%tree%nodes(idx)%pos = pos
    if (present(value)) p%tree%nodes(idx)%value = value

    end function add_node
!*****************************************************************************************

!*****************************************************************************************
!>
!  `expr := and_expr { '||' and_expr }`

    recursive function parse_or(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    n = parse_and(p)
    if (p%err%failed) return
    do while (peek(p) == tok_or)
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_and(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op_or, [n, m], pos)
    end do
    end function parse_or
!*****************************************************************************************

!*****************************************************************************************
!>
!  `and_expr := not_expr { '&&' not_expr }`

    recursive function parse_and(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    n = parse_not(p)
    if (p%err%failed) return
    do while (peek(p) == tok_and)
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_not(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op_and, [n, m], pos)
    end do
    end function parse_and
!*****************************************************************************************

!*****************************************************************************************
!>
!  `not_expr := '!' not_expr | cmp_expr`

    recursive function parse_not(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    if (peek(p) == tok_not) then
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_not(p)
        if (p%err%failed) return
        n = add_node(p, node_unary, op_not, [m], pos)
    else
        n = parse_cmp(p)
    end if
    end function parse_not
!*****************************************************************************************

!*****************************************************************************************
!>
!  `cmp_expr := add_expr { cmp_op add_expr }`

    recursive function parse_cmp(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos, op
    n = parse_add(p)
    if (p%err%failed) return
    do
        select case (peek(p))
        case (tok_eq); op = op_eq
        case (tok_ne); op = op_ne
        case (tok_lt); op = op_lt
        case (tok_le); op = op_le
        case (tok_gt); op = op_gt
        case (tok_ge); op = op_ge
        case default
            exit
        end select
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_add(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op, [n, m], pos)
    end do
    end function parse_cmp
!*****************************************************************************************

!*****************************************************************************************
!>
!  `add_expr := mul_expr { ('+'|'-') mul_expr }`

    recursive function parse_add(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos, op
    n = parse_mul(p)
    if (p%err%failed) return
    do
        select case (peek(p))
        case (tok_plus);  op = op_add
        case (tok_minus); op = op_sub
        case default
            exit
        end select
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_mul(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op, [n, m], pos)
    end do
    end function parse_add
!*****************************************************************************************

!*****************************************************************************************
!>
!  `mul_expr := unary { ('*'|'/') unary }`

    recursive function parse_mul(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos, op
    n = parse_unary(p)
    if (p%err%failed) return
    do
        select case (peek(p))
        case (tok_mul); op = op_mul
        case (tok_div); op = op_div
        case default
            exit
        end select
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_unary(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op, [n, m], pos)
    end do
    end function parse_mul
!*****************************************************************************************

!*****************************************************************************************
!>
!  `unary := ('-'|'+'|'!') unary | power`

    recursive function parse_unary(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    n = 0
    pos = cur_pos(p)
    select case (peek(p))
    case (tok_minus)
        p%ip = p%ip + 1
        m = parse_unary(p)
        if (p%err%failed) return
        if (p%tree%nodes(m)%kind == node_const) then
            ! fold the negation into the constant
            p%tree%nodes(m)%value = -p%tree%nodes(m)%value
            n = m
        else
            n = add_node(p, node_unary, op_neg, [m], pos)
        end if
    case (tok_plus)
        p%ip = p%ip + 1
        n = parse_unary(p)
    case (tok_not)
        p%ip = p%ip + 1
        m = parse_unary(p)
        if (p%err%failed) return
        n = add_node(p, node_unary, op_not, [m], pos)
    case default
        n = parse_power(p)
    end select
    end function parse_unary
!*****************************************************************************************

!*****************************************************************************************
!>
!  `power := postfix [ '^' unary ]`

    recursive function parse_power(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    n = parse_postfix(p)
    if (p%err%failed) return
    if (peek(p) == tok_pow) then
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_unary(p)
        if (p%err%failed) return
        n = add_node(p, node_binary, op_pow, [n, m], pos)
    end if
    end function parse_power
!*****************************************************************************************

!*****************************************************************************************
!>
!  `postfix := primary { '[' expr ']' }`

    recursive function parse_postfix(p) result(n)
    type(parser_t), intent(inout) :: p
    integer :: n
    integer :: m, pos
    n = parse_primary(p)
    if (p%err%failed) return
    do while (peek(p) == tok_lbrack)
        pos = cur_pos(p)
        p%ip = p%ip + 1
        m = parse_or(p)
        if (p%err%failed) return
        if (peek(p) /= tok_rbrack) then
            call p%err%set('expected "]"', cur_pos(p))
            return
        end if
        p%ip = p%ip + 1
        n = add_node(p, node_index, 0, [n, m], pos)
    end do
    end function parse_postfix
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse a primary expression: a number, variable, function call,
!  parenthesized expression, or vector constructor.

    recursive function parse_primary(p) result(n)

    type(parser_t), intent(inout) :: p
    integer :: n

    integer :: pos, k, id, min_args, max_args
    integer, dimension(:), allocatable :: args
    character(len=:), allocatable :: name
    logical :: found

    n = 0
    pos = cur_pos(p)

    select case (peek(p))

    case (tok_number)
        n = add_node(p, node_const, 0, [integer ::], pos, p%toks(p%ip)%value)
        p%ip = p%ip + 1

    case (tok_lparen)
        p%ip = p%ip + 1
        n = parse_or(p)
        if (p%err%failed) return
        if (peek(p) /= tok_rparen) then
            call p%err%set('expected ")"', cur_pos(p))
            return
        end if
        p%ip = p%ip + 1

    case (tok_lbrack)
        p%ip = p%ip + 1
        call parse_list(p, tok_rbrack, args)
        if (p%err%failed) return
        if (size(args) == 0) then
            call p%err%set('empty vector constructor', pos)
            return
        end if
        n = add_node(p, node_vector, 0, args, pos)

    case (tok_ident)
        name = p%toks(p%ip)%text
        p%ip = p%ip + 1

        if (peek(p) == tok_lparen) then
            ! function call
            p%ip = p%ip + 1
            call parse_list(p, tok_rparen, args)
            if (p%err%failed) return
            ! user functions take precedence over built-in functions
            do k = 1, size(p%funcs)
                if (p%funcs(k)%name == name) then
                    if (p%funcs(k)%nargs >= 0 .and. p%funcs(k)%nargs /= size(args)) then
                        call p%err%set('function "'//name//'" requires '//&
                                       int_to_str(p%funcs(k)%nargs)//' argument(s) but '//&
                                       int_to_str(size(args))//' were given', pos)
                        return
                    end if
                    n = add_node(p, node_user, k, args, pos)
                    return
                end if
            end do
            call builtin_lookup(name, found, id, min_args, max_args)
            if (.not. found) then
                call p%err%set('unknown function "'//name//'"', pos)
                return
            end if
            if (size(args) < min_args .or. (max_args >= 0 .and. size(args) > max_args)) then
                if (min_args == max_args) then
                    call p%err%set('function "'//name//'" requires '//int_to_str(min_args)//&
                                   ' argument(s) but '//int_to_str(size(args))//' were given', pos)
                else
                    call p%err%set('function "'//name//'" requires at least '//int_to_str(min_args)//&
                                   ' arguments but '//int_to_str(size(args))//' were given', pos)
                end if
                return
            end if
            n = add_node(p, node_builtin, id, args, pos)

        else
            ! variable or constant
            do k = 1, size(p%vars)
                if (p%vars(k)%str == name) then
                    n = add_node(p, node_var, k, [integer ::], pos)
                    if (.not. any(p%tree%refs == k)) p%tree%refs = [p%tree%refs, k]
                    return
                end if
            end do
            if (name == 'pi') then
                n = add_node(p, node_const, 0, [integer ::], pos, acos(-1.0_wp))
            else
                call p%err%set('undefined variable "'//name//'"', pos)
            end if
        end if

    case default
        call unexpected(p)
    end select

    end function parse_primary
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse a comma-separated list of expressions, terminated by
!  the token `close` (which is consumed). The opening token
!  must already have been consumed.

    recursive subroutine parse_list(p, close, args)

    type(parser_t), intent(inout) :: p
    integer, intent(in) :: close !! closing token kind
    integer, dimension(:), allocatable, intent(out) :: args !! the parsed expression nodes

    integer :: m

    allocate(args(0))
    if (peek(p) /= close) then
        do
            m = parse_or(p)
            if (p%err%failed) return
            args = [args, m]
            if (peek(p) == tok_comma) then
                p%ip = p%ip + 1
            else
                exit
            end if
        end do
    end if
    if (peek(p) /= close) then
        if (close == tok_rparen) then
            call p%err%set('expected ")" or ","', cur_pos(p))
        else
            call p%err%set('expected "]" or ","', cur_pos(p))
        end if
        return
    end if
    p%ip = p%ip + 1

    end subroutine parse_list
!*****************************************************************************************

!*****************************************************************************************
    end module fparse_parser
!*****************************************************************************************
