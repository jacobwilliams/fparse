!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  A Fortran function parser and evaluator for expressions involving
!  multiple scalar or vector variables, with user-defined functions
!  and optional first derivatives.
!
!### Example
!
!```fortran
!  use fparse
!  type(fparser) :: p
!  real(wp) :: w
!  call p%parse(['x','y'], ['z = sin(x) + 2.0*y', &
!                           'w = z + x          '])
!  call p%set_variable('x', 1.0_wp)
!  call p%set_variable('y', 2.0_wp)
!  call p%evaluate()
!  call p%get('w', w)
!```
!
!### Expressions
!
!  Each expression is either of the form `name = expression` (named),
!  or an expression with no name (anonymous). Named outputs may be
!  used as variables in the other expressions; the evaluation order
!  is determined automatically from the dependencies. Alternatively,
!  the output names can be given separately using the `names`
!  argument to [[fparser:parse]]. All names are case insensitive.
!
!  See [[fparse_parser]] for the expression syntax and [[fparse_functions]]
!  for the list of built-in functions.
!
!### Values
!
!  Variables can be scalars or vectors. Operations and elemental
!  functions apply element-wise, and scalars are broadcast to vectors.
!  Logical/relational operators return `1.0` for true and `0.0` for false,
!  and any nonzero value is considered true.
!
!### Derivatives
!
!  If the `wrt` argument is given to [[fparser:evaluate]], the derivatives
!  of all the outputs with respect to the listed input variables are also
!  computed (using forward-mode automatic differentiation), and can be
!  retrieved using [[fparser:get_derivative]]. The derivative of an output of size `m`
!  with respect to a variable of size `n` is a matrix of shape `[m, n]`.
!  When `wrt` is not given, no derivative computations are performed.

    module fparse

    use fparse_common
    use fparse_functions
    use fparse_parser
    use fparse_eval
    use fparse_bytecode

    implicit none

    private

    public :: wp
    public :: fparse_value, fparse_func, fparse_dfunc

    type :: deriv_layout
        !! The derivative columns stored for a variable. The columns are grouped in
        !! blocks, one for each derivative variable (the `wrt` argument of
        !! [[fparser:evaluate]]) that the variable depends on.
        integer :: nd = 0 !! number of columns
        integer, dimension(:), allocatable :: blocks !! indices (into `fparser%wrt`) of the blocks, in increasing order
        integer, dimension(:), allocatable :: offset !! column offset of each block
    end type deriv_layout

    type :: int_list
        integer, dimension(:), allocatable :: i
    end type int_list

    type :: expr_dsetup
        !! Derivative setup for one expression: the derivatives of its referenced
        !! variables, expressed in the expression's own columns.
        type(val_t), dimension(:), allocatable :: dref !! one per referenced variable (`expr_tree%refs`).
                                                       !! If `d` is not allocated, the variable's own
                                                       !! derivative is used directly (same columns).
        type(int_list), dimension(:), allocatable :: colmap !! for each referenced variable: the
                                                            !! expression's column for each of its columns
        logical, dimension(:), allocatable :: gather !! if `dref` must be refilled from an output
                                                     !! before each evaluation
    end type expr_dsetup

    type, public :: fparser
        !! The function parser class.
        private
        integer :: n_inputs = 0 !! number of input variables
        integer :: n_outputs = 0 !! number of expressions
        type(string_t), dimension(:), allocatable :: names !! all variable names (inputs, then outputs).
                                                            !! Anonymous outputs have empty names.
        type(string_t), dimension(:), allocatable :: expressions !! the expression strings
        type(expr_tree), dimension(:), allocatable :: trees !! the parsed expressions
        type(scalar_program), dimension(:), allocatable :: progs !! the expressions compiled for fast
                                                                 !! scalar evaluation
        integer, dimension(:), allocatable :: order !! evaluation order of the expressions
        type(val_t), dimension(:), allocatable :: vals !! values of all the variables (inputs, then outputs)
        logical, dimension(:), allocatable :: is_set !! if each input variable has been set
        type(user_function), dimension(:), allocatable :: funcs !! user-defined functions
        logical :: parsed = .false. !! if the expressions have been parsed
        logical :: evaluated = .false. !! if the expressions have been evaluated
        integer :: nd = 0 !! total number of derivative columns
        integer, dimension(:), allocatable :: wrt !! input variable indices for derivatives
        type(string_t), dimension(:), allocatable :: wrt_names !! the `wrt` argument of the last
                                                               !! call to [[fparser:evaluate]]
        integer, dimension(:), allocatable :: wrt_offset !! column offsets in the derivative matrices
        logical :: check_domain = .true. !! check for domain errors during evaluation
        logical :: use_scalar_programs = .true. !! use the fast scalar evaluator when possible
        logical :: use_sparsity = .true. !! only compute the derivatives that can be nonzero
        logical :: use_cache = .true. !! reuse the last results if the inputs haven't changed
        logical :: cache_ok = .true. !! false if the expressions use an impure user function
                                     !! (so the results can't be cached)
        logical :: sparse_setup = .false. !! if the current derivative setup used sparsity
        type(deriv_layout), dimension(:), allocatable :: layout !! derivative columns of each variable
        type(expr_dsetup), dimension(:), allocatable :: dsetup !! derivative setup of each expression
        integer :: size_version = 0 !! incremented whenever the size of any value changes
        integer, dimension(:), allocatable :: prog_version !! `size_version` when `prog_usable` was computed
        logical, dimension(:), allocatable :: prog_usable !! if each scalar program can currently be used
        type(error_t) :: err !! error status
    contains
        private
        procedure, public :: add_function
        procedure, public :: parse
        procedure, public :: evaluate
        procedure, public :: set_variables
        generic, public :: set_variable => set_scalar_by_name, set_vector_by_name, &
                                           set_scalar_by_index, set_vector_by_index
        generic, public :: get => get_scalar_by_name, get_vector_by_name, &
                                  get_scalar_by_index, get_vector_by_index
        generic, public :: get_derivative => get_derivative_matrix_by_name, &
                                             get_derivative_scalar_by_name, &
                                             get_derivative_matrix_by_index, &
                                             get_derivative_scalar_by_index
        procedure, public :: failed
        procedure, public :: error_message
        procedure, public :: clear_error
        procedure, public :: num_inputs
        procedure, public :: num_outputs
        procedure, public :: input_name
        procedure, public :: output_name
        procedure, public :: evaluation_order
        procedure, public :: set_domain_checking
        procedure, public :: set_scalar_optimization
        procedure, public :: set_sparsity
        procedure, public :: set_caching
        procedure, public :: destroy
        procedure :: set_scalar_by_name, set_vector_by_name, set_scalar_by_index, set_vector_by_index
        procedure :: get_scalar_by_name, get_vector_by_name, get_scalar_by_index, get_vector_by_index
        procedure :: get_derivative_matrix_by_name, get_derivative_scalar_by_name
        procedure :: get_derivative_matrix_by_index, get_derivative_scalar_by_index
        procedure :: set_value
        procedure :: find_variable
        procedure :: find_output
        procedure :: finish
        procedure :: sort_expressions
    end type fparser

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Destroy the parser and free all memory (including the user functions).

    subroutine destroy(me)

    class(fparser), intent(out) :: me ! intent(out) resets all components to their defaults

    end subroutine destroy
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if an error has occurred.

    pure logical function failed(me)

    class(fparser), intent(in) :: me

    failed = me%err%failed

    end function failed
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the last error message (an empty string if there was no error).

    pure function error_message(me) result(msg)

    class(fparser), intent(in) :: me
    character(len=:), allocatable :: msg

    if (me%err%failed .and. allocated(me%err%message)) then
        msg = me%err%message
    else
        msg = ''
    end if

    end function error_message
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear the error status.

    subroutine clear_error(me)

    class(fparser), intent(inout) :: me

    if (me%err%failed) call me%err%clear()

    end subroutine clear_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the optional status outputs of a method.

    subroutine finish(me, status, errmsg)

    class(fparser), intent(in) :: me
    integer, intent(out), optional :: status
    character(len=:), allocatable, intent(out), optional :: errmsg

    if (present(status)) status = merge(1, 0, me%err%failed)
    if (present(errmsg)) errmsg = error_message(me)

    end subroutine finish
!*****************************************************************************************

!*****************************************************************************************
!>
!  Enable or disable domain checking during evaluation (it is enabled by default).
!  If enabled, an error is raised for operations such as division by zero,
!  `sqrt` or `log` of a negative number, etc. If disabled, these operations
!  produce IEEE results (Inf or NaN) instead.

    subroutine set_domain_checking(me, check)

    class(fparser), intent(inout) :: me
    logical, intent(in) :: check

    if (me%check_domain .neqv. check) me%evaluated = .false.
    me%check_domain = check

    end subroutine set_domain_checking
!*****************************************************************************************

!*****************************************************************************************
!>
!  Enable or disable the use of sparsity in the derivative computations
!  (it is enabled by default).
!
!  When enabled, [[fparser:evaluate]] determines (from the expressions) which
!  derivative variables each output can depend on, directly or through other
!  outputs, and only computes those derivatives. The others are known to be zero.
!  This can be much faster when there are many derivative variables and each
!  output only depends on a few of them. The results are the same either way.

    subroutine set_sparsity(me, use)

    class(fparser), intent(inout) :: me
    logical, intent(in) :: use

    if (me%use_sparsity .neqv. use) me%evaluated = .false.
    me%use_sparsity = use

    end subroutine set_sparsity
!*****************************************************************************************

!*****************************************************************************************
!>
!  Enable or disable caching (it is enabled by default).
!
!  When enabled, [[fparser:evaluate]] returns immediately, keeping the results
!  of the previous evaluation, if:
!
!  * the previous evaluation was successful,
!  * none of the input variables have changed since then (the values must be exactly
!    the same, including the sign of zero; a NaN never compares equal), and
!  * the same derivatives are requested (the same `wrt` list, or no derivatives).
!    If no derivatives are requested after an evaluation with derivatives, the
!    values are reused and the derivatives are discarded.
!
!  Setting a variable to the value it already has does not count as a change.
!  Caching is never used if the expressions use a user-defined function that
!  was not added as pure (see [[fparser:add_function]]).

    subroutine set_caching(me, use)

    class(fparser), intent(inout) :: me
    logical, intent(in) :: use

    if (me%use_cache .neqv. use) me%evaluated = .false.
    me%use_cache = use

    end subroutine set_caching
!*****************************************************************************************

!*****************************************************************************************
!>
!  Enable or disable the fast scalar evaluator (it is enabled by default).
!  When enabled, expressions whose variables are all scalars are evaluated
!  with a compiled scalar program if no derivatives are requested. The
!  results are the same either way; this is mostly useful for testing.

    subroutine set_scalar_optimization(me, use)

    class(fparser), intent(inout) :: me
    logical, intent(in) :: use

    if (me%use_scalar_programs .neqv. use) me%evaluated = .false.
    me%use_scalar_programs = use

    end subroutine set_scalar_optimization
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a user-defined function. This must be called before [[fparser:parse]].
!  User-defined functions take precedence over built-in functions with the
!  same name. If a function with the same name has already been added, it
!  is replaced.
!
!  If `df` is not given and derivatives are requested, the derivative of
!  the function is computed using central finite differences.
!
!  A function is pure if its result only depends on its arguments (for example,
!  it doesn't use any saved state, random numbers, or the time). If the expressions
!  use any function that is not pure, the results are never cached (see
!  [[fparser:set_caching]]).

    subroutine add_function(me, name, f, is_pure, nargs, df, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! function name (case insensitive)
    procedure(fparse_func) :: f !! the function
    logical, intent(in) :: is_pure !! if the function (and its derivative) are pure: the
                                   !! results only depend on the arguments
    integer, intent(in), optional :: nargs !! number of arguments (default is -1, meaning any number)
    procedure(fparse_dfunc), optional :: df !! the derivative of the function
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    type(user_function) :: uf
    integer :: k

    if (me%err%failed) call me%err%clear()
    if (.not. allocated(me%funcs)) allocate(me%funcs(0))

    uf%name = to_lower(trim(adjustl(name)))
    if (.not. is_valid_name(uf%name)) then
        call me%err%set('invalid function name "'//trim(name)//'"')
    else
        uf%f => f
        uf%is_pure = is_pure
        if (present(df)) uf%df => df
        if (present(nargs)) uf%nargs = nargs
        do k = 1, size(me%funcs)
            if (me%funcs(k)%name == uf%name) then
                me%funcs(k) = uf
                exit
            end if
        end do
        if (k > size(me%funcs)) me%funcs = [me%funcs, uf]
        ! a replaced function may be used by the parsed expressions:
        me%evaluated = .false.
        call update_cache_ok(me)
    end if

    call finish(me, status, errmsg)

    end subroutine add_function
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse a list of expressions.
!
!  If `names` is not present, each expression may optionally be of the form
!  `name = expression`. Expressions without a name are anonymous: their results
!  can only be retrieved by index, and cannot be used in the other expressions.
!  If `names` is present, it gives the output name of each expression (a blank
!  name means the expression is anonymous).
!
!  Errors that are detected include: syntax errors, undefined variables and
!  functions, wrong number of function arguments, invalid or duplicate names,
!  and circular dependencies between the expressions.

    subroutine parse(me, variables, expressions, names, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), dimension(:), intent(in) :: variables !! names of the input variables (case insensitive)
    character(len=*), dimension(:), intent(in) :: expressions !! the expressions
    character(len=*), dimension(:), intent(in), optional :: names !! names of the outputs of the expressions
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: i, nin, nout, offset
    character(len=:), allocatable :: name, rhs
    type(string_t), dimension(:), allocatable :: rhs_list
    integer, dimension(:), allocatable :: offsets
    type(error_t) :: perr

    ! reset (keep the user functions and options)
    if (me%err%failed) call me%err%clear()
    me%parsed = .false.
    me%evaluated = .false.
    me%nd = 0
    if (allocated(me%names)) deallocate(me%names)
    if (allocated(me%expressions)) deallocate(me%expressions)
    if (allocated(me%trees)) deallocate(me%trees)
    if (allocated(me%progs)) deallocate(me%progs)
    if (allocated(me%order)) deallocate(me%order)
    if (allocated(me%vals)) deallocate(me%vals)
    if (allocated(me%is_set)) deallocate(me%is_set)
    if (allocated(me%wrt)) deallocate(me%wrt)
    if (allocated(me%wrt_offset)) deallocate(me%wrt_offset)
    if (allocated(me%wrt_names)) deallocate(me%wrt_names)
    if (allocated(me%layout)) deallocate(me%layout)
    if (allocated(me%dsetup)) deallocate(me%dsetup)
    if (.not. allocated(me%funcs)) allocate(me%funcs(0))

    nin = size(variables)
    nout = size(expressions)
    me%n_inputs = nin
    me%n_outputs = nout
    allocate(me%names(nin + nout), me%expressions(nout), rhs_list(nout), offsets(nout))

    main: block

        if (present(names)) then
            if (size(names) /= nout) then
                call me%err%set('size of names ('//int_to_str(size(names))//&
                                ') does not match the number of expressions ('//int_to_str(nout)//')')
                exit main
            end if
        end if

        ! input variables:
        do i = 1, nin
            name = to_lower(trim(adjustl(variables(i))))
            if (.not. is_valid_name(name)) then
                call me%err%set('invalid variable name "'//trim(variables(i))//'"')
                exit main
            end if
            if (find_variable(me, name, i-1) > 0) then
                call me%err%set('duplicate variable name "'//name//'"')
                exit main
            end if
            me%names(i)%str = name
        end do

        ! output names:
        do i = 1, nout
            me%expressions(i)%str = trim(expressions(i))
            if (present(names)) then
                name = to_lower(trim(adjustl(names(i))))
                rhs = trim(expressions(i))
                offset = 0
            else
                call split_assignment(trim(expressions(i)), name, rhs, offset)
            end if
            if (len(name) > 0) then
                if (.not. is_valid_name(name)) then
                    call me%err%set('invalid output name "'//name//'" for expression '//int_to_str(i))
                    exit main
                end if
                if (find_variable(me, name, nin + i - 1) > 0) then
                    call me%err%set('duplicate name "'//name//'" for expression '//int_to_str(i))
                    exit main
                end if
            end if
            me%names(nin + i)%str = name
            rhs_list(i)%str = rhs
            offsets(i) = offset
        end do

        ! parse the expressions:
        allocate(me%trees(nout))
        do i = 1, nout
            call parse_expression(rhs_list(i)%str, me%names, me%funcs, me%trees(i), perr)
            if (perr%failed) then
                call me%err%set('error parsing expression '//int_to_str(i)//' "'//&
                                me%expressions(i)%str//'": '//perr%message//&
                                ' at position '//int_to_str(perr%pos + offsets(i)), &
                                perr%pos + offsets(i))
                exit main
            end if
        end do

        ! determine the evaluation order:
        call sort_expressions(me)
        if (me%err%failed) exit main

        ! compile for fast scalar evaluation:
        allocate(me%progs(nout))
        do i = 1, nout
            call compile_scalar(me%trees(i), me%progs(i))
        end do
        me%size_version = 0
        me%prog_version = spread(-1, 1, nout)
        me%prog_usable = spread(.false., 1, nout)

        allocate(me%vals(nin + nout))
        allocate(me%is_set(nin), source=.false.)
        me%parsed = .true.
        call update_cache_ok(me)

    end block main

    call finish(me, status, errmsg)

    end subroutine parse
!*****************************************************************************************

!*****************************************************************************************
!>
!  Split a string of the form `name = expression`. If the string is not
!  of that form, `name` is returned blank and `rhs` is the full string.

    subroutine split_assignment(str, name, rhs, offset)

    character(len=*), intent(in) :: str !! the string
    character(len=:), allocatable, intent(out) :: name !! the name (lowercase)
    character(len=:), allocatable, intent(out) :: rhs !! the expression
    integer, intent(out) :: offset !! offset of `rhs` in `str`

    integer :: k, n
    character(len=1) :: prev, next

    n = len(str)
    do k = 1, n
        if (str(k:k) == '=') then
            prev = ' '
            next = ' '
            if (k > 1) prev = str(k-1:k-1)
            if (k < n) next = str(k+1:k+1)
            if (next /= '=' .and. index('=<>/!', prev) == 0) then
                ! this is an assignment
                name = to_lower(trim(adjustl(str(1:k-1))))
                rhs = str(k+1:n)
                offset = k
                if (len(name) == 0) name = '='  ! will be flagged as invalid
                return
            end if
        end if
    end do

    name = ''
    rhs = str
    offset = 0

    end subroutine split_assignment
!*****************************************************************************************

!*****************************************************************************************
!>
!  Determine the order to evaluate the expressions, based on their
!  dependencies on the outputs of the other expressions.
!  Raises an error if there is a circular dependency.

    subroutine sort_expressions(me)

    class(fparser), intent(inout) :: me

    logical, dimension(:), allocatable :: done
    logical :: ready, progress
    integer :: i, j, k, n, nin
    character(len=:), allocatable :: msg

    nin = me%n_inputs
    n = me%n_outputs
    allocate(me%order(0), done(n))
    done = .false.

    do while (size(me%order) < n)
        progress = .false.
        do i = 1, n
            if (done(i)) cycle
            ready = .true.
            do k = 1, size(me%trees(i)%refs)
                j = me%trees(i)%refs(k) - nin
                if (j > 0) then
                    if (.not. done(j)) then
                        ready = .false.
                        exit
                    end if
                end if
            end do
            if (ready) then
                done(i) = .true.
                me%order = [me%order, i]
                progress = .true.
            end if
        end do
        if (.not. progress) then
            msg = ''
            do i = 1, n
                if (.not. done(i)) then
                    if (len(msg) > 0) msg = msg//', '
                    msg = msg//me%names(nin + i)%str
                end if
            end do
            call me%err%set('circular dependency between expressions: '//msg)
            return
        end if
    end do

    end subroutine sort_expressions
!*****************************************************************************************

!*****************************************************************************************
!>
!  Find a variable (input or output) by name. Only the first `n` names are
!  searched (all the names if `n` is not present). Returns 0 if not found.

    pure integer function find_variable(me, name, n) result(idx)

    class(fparser), intent(in) :: me
    character(len=*), intent(in) :: name !! lowercase name
    integer, intent(in), optional :: n !! number of names to search

    integer :: i, nmax

    idx = 0
    if (.not. allocated(me%names) .or. len(name) == 0) return
    nmax = size(me%names)
    if (present(n)) nmax = min(n, nmax)
    do i = 1, nmax
        if (allocated(me%names(i)%str)) then
            if (me%names(i)%str == name) then
                idx = i
                return
            end if
        end if
    end do

    end function find_variable
!*****************************************************************************************

!*****************************************************************************************
!>
!  Find a variable by name, or raise an error if it is not found.
!  The returned index is into the full list of variables.

    integer function find_output(me, name) result(idx)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name

    idx = find_variable(me, to_lower(trim(adjustl(name))))
    if (idx == 0) call me%err%set('unknown variable "'//trim(name)//'"')

    end function find_output
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the value of an input variable (internal routine).

    subroutine set_value(me, idx, value)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! input variable index
    real(wp), dimension(:), intent(in) :: value !! the value

    if (.not. me%parsed) then
        call me%err%set('the expressions have not been parsed')
    else if (idx < 1 .or. idx > me%n_inputs) then
        call me%err%set('invalid input variable index '//int_to_str(idx))
    else
        if (allocated(me%vals(idx)%v)) then
            if (identical(me%vals(idx)%v, value)) return ! no change (the last results are still valid)
            if (size(me%vals(idx)%v) /= size(value)) me%size_version = me%size_version + 1
        else
            me%size_version = me%size_version + 1
        end if
        me%vals(idx)%v = value
        me%is_set(idx) = .true.
        me%evaluated = .false.
    end if

    end subroutine set_value
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if two values are exactly the same: the same size, and the same
!  elements (including the sign of zero). NaNs are never identical.

    pure logical function identical(a, b)

    real(wp), dimension(:), intent(in) :: a, b

    integer :: i

    identical = .false.
    if (size(a) /= size(b)) return
    do i = 1, size(a)
        if (.not. same_number(a(i), b(i))) return
    end do
    identical = .true.

    end function identical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if two numbers are exactly the same (including the sign of zero).
!  NaNs are never the same.

    pure elemental logical function same_number(a, b)

    real(wp), intent(in) :: a, b

    same_number = a == b
    if (same_number .and. a == 0.0_wp) same_number = sign(1.0_wp, a) == sign(1.0_wp, b)

    end function same_number
!*****************************************************************************************

!*****************************************************************************************
!>
!  Determine if the results can be cached (internal routine): not if any of the
!  parsed expressions use a user-defined function that isn't pure.

    subroutine update_cache_ok(me)

    class(fparser), intent(inout) :: me

    integer :: i, k

    me%cache_ok = .true.
    if (.not. allocated(me%trees)) return
    do i = 1, size(me%trees)
        do k = 1, me%trees(i)%n_nodes
            if (me%trees(i)%nodes(k)%kind == node_user) then
                if (.not. me%funcs(me%trees(i)%nodes(k)%op)%is_pure) then
                    me%cache_ok = .false.
                    return
                end if
            end if
        end do
    end do

    end subroutine update_cache_ok
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the value of a scalar input variable by name.

    subroutine set_scalar_by_name(me, name, value, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! variable name (case insensitive)
    real(wp), intent(in) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    call set_vector_by_name(me, name, [value], status, errmsg)

    end subroutine set_scalar_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the value of a vector input variable by name.

    subroutine set_vector_by_name(me, name, value, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! variable name (case insensitive)
    real(wp), dimension(:), intent(in) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: idx

    if (me%err%failed) call me%err%clear()
    idx = find_variable(me, to_lower(trim(adjustl(name))), me%n_inputs)
    if (idx == 0) then
        if (find_variable(me, to_lower(trim(adjustl(name)))) > 0) then
            call me%err%set('"'//trim(name)//'" is an output and cannot be set')
        else
            call me%err%set('unknown input variable "'//trim(name)//'"')
        end if
    else
        call set_value(me, idx, value)
    end if
    call finish(me, status, errmsg)

    end subroutine set_vector_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the value of a scalar input variable by index (in the order given to [[fparser:parse]]).

    subroutine set_scalar_by_index(me, idx, value, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! variable index
    real(wp), intent(in) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    call set_vector_by_index(me, idx, [value], status, errmsg)

    end subroutine set_scalar_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the value of a vector input variable by index (in the order given to [[fparser:parse]]).

    subroutine set_vector_by_index(me, idx, value, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! variable index
    real(wp), dimension(:), intent(in) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    if (me%err%failed) call me%err%clear()
    call set_value(me, idx, value)
    call finish(me, status, errmsg)

    end subroutine set_vector_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the values of all the input variables as scalars
!  (in the order given to [[fparser:parse]]).

    subroutine set_variables(me, values, status, errmsg)

    class(fparser), intent(inout) :: me
    real(wp), dimension(:), intent(in) :: values !! the values of all the input variables
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: i

    if (me%err%failed) call me%err%clear()
    if (size(values) /= me%n_inputs) then
        call me%err%set('number of values ('//int_to_str(size(values))//&
                        ') does not match the number of variables ('//int_to_str(me%n_inputs)//')')
    else
        if (.not. me%parsed) then
            call me%err%set('the expressions have not been parsed')
        else
            do i = 1, me%n_inputs
                associate (v => me%vals(i))
                    if (allocated(v%v)) then
                        if (size(v%v) == 1) then
                            if (same_number(v%v(1), values(i)) .and. me%is_set(i)) cycle ! no change
                        else
                            deallocate(v%v)
                        end if
                    end if
                    if (.not. allocated(v%v)) then
                        allocate(v%v(1))
                        me%size_version = me%size_version + 1
                    end if
                    v%v(1) = values(i)
                    me%is_set(i) = .true.
                    me%evaluated = .false.
                end associate
            end do
        end if
    end if
    call finish(me, status, errmsg)

    end subroutine set_variables
!*****************************************************************************************

!*****************************************************************************************
!>
!  Evaluate all the expressions, using the current values of the input variables.
!
!  If `wrt` is present, the first derivatives of all the outputs with respect
!  to the listed input variables are also computed. These can be retrieved
!  using [[fparser:get_derivative]].

    subroutine evaluate(me, wrt, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), dimension(:), intent(in), optional :: wrt !! names of the input variables
                                                                !! to compute derivatives with respect to
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    type(eval_ctx) :: ctx
    integer :: i, k, nin, idx, off, nwrt, nde

    if (me%err%failed) call me%err%clear()
    nin = me%n_inputs

    ! if nothing has changed since the last evaluation, use those results:
    if (me%evaluated .and. me%use_cache .and. me%cache_ok) then
        nwrt = 0
        if (present(wrt)) nwrt = size(wrt)
        if (nwrt == 0) then
            if (me%nd > 0) call clear_derivatives() ! the values are still valid
            call finish(me, status, errmsg)
            return
        else if (same_wrt()) then
            call finish(me, status, errmsg)
            return
        end if
    end if
    me%evaluated = .false.

    main: block

        if (.not. me%parsed) then
            call me%err%set('the expressions have not been parsed')
            exit main
        end if

        if (.not. all(me%is_set)) then
            i = findloc(me%is_set, .false., dim=1)
            call me%err%set('variable "'//me%names(i)%str//'" has not been set')
            exit main
        end if

        ! set up the derivatives:
        nwrt = 0
        if (present(wrt)) nwrt = size(wrt)
        if (nwrt > 0) then
            if (.not. same_wrt()) then
                if (allocated(me%wrt_names)) deallocate(me%wrt_names)
                me%nd = 0
                me%wrt = [integer ::]
                me%wrt_offset = [integer ::]
                off = 0
                do k = 1, nwrt
                    idx = find_variable(me, to_lower(trim(adjustl(wrt(k)))), nin)
                    if (idx == 0) then
                        call me%err%set('cannot compute derivative with respect to "'//trim(wrt(k))//&
                                        '": not an input variable')
                        exit main
                    end if
                    if (any(me%wrt == idx)) then
                        call me%err%set('duplicate derivative variable "'//trim(wrt(k))//'"')
                        exit main
                    end if
                    me%wrt = [me%wrt, idx]
                    me%wrt_offset = [me%wrt_offset, off]
                    off = off + size(me%vals(idx)%v)
                end do
                me%nd = off
                call setup_derivatives(me)
                allocate(me%wrt_names(nwrt))
                do k = 1, nwrt
                    me%wrt_names(k)%str = trim(wrt(k))
                end do
            end if
        else if (me%nd > 0 .or. .not. allocated(me%wrt)) then
            call clear_derivatives() ! from a previous evaluation
        end if

        ! evaluate the expressions:
        ctx%check_domain = me%check_domain
        do k = 1, me%n_outputs
            i = me%order(k)
            ! number of derivative columns for this expression:
            nde = 0
            if (me%nd > 0) nde = me%layout(nin + i)%nd
            ctx%nd = nde
            if (nde > 0) call gather_derivatives(me, i)
            if (use_scalar_program(i)) then
                if (nde > 0) then
                    call exec_scalar_d(me%progs(i), me%vals, ctx, me%dsetup(i)%dref)
                else
                    call exec_scalar(me%progs(i), me%vals, ctx)
                end if
                if (ctx%err%failed) exit
                associate (o => me%vals(nin + i), prog => me%progs(i))
                    if (allocated(o%v)) then
                        if (size(o%v) /= 1) deallocate(o%v)
                    end if
                    if (.not. allocated(o%v)) then
                        allocate(o%v(1))
                        me%size_version = me%size_version + 1
                    end if
                    o%v(1) = prog%s(prog%root)
                    if (me%nd > 0) then
                        if (allocated(o%d)) then
                            if (size(o%d, 1) /= 1 .or. size(o%d, 2) /= nde) deallocate(o%d)
                        end if
                        if (.not. allocated(o%d)) allocate(o%d(1, nde))
                        if (nde > 0) o%d(1, :) = prog%sd(:, prog%root)
                    end if
                end associate
            else
                if (nde > 0) then
                    call eval_tree(me%trees(i), me%vals, me%funcs, ctx, me%dsetup(i)%dref)
                else
                    call eval_tree(me%trees(i), me%vals, me%funcs, ctx)
                end if
                if (ctx%err%failed) exit
                associate (r => me%trees(i)%buf(me%trees(i)%root), o => me%vals(nin + i))
                    if (allocated(o%v)) then
                        if (size(o%v) /= size(r%v)) me%size_version = me%size_version + 1
                    else
                        me%size_version = me%size_version + 1
                    end if
                    o%v = r%v
                    if (nde > 0) then
                        o%d = r%d
                    else if (me%nd > 0) then
                        ! derivatives requested, but this output doesn't depend on any of them
                        if (allocated(o%d)) deallocate(o%d)
                        allocate(o%d(size(o%v), 0))
                    end if
                end associate
            end if
        end do
        if (ctx%err%failed) then
            call me%err%set('error evaluating expression '//int_to_str(i)//' "'//&
                            me%expressions(i)%str//'": '//ctx%err%message, ctx%err%pos)
            exit main
        end if

        me%evaluated = .true.

    end block main

    call finish(me, status, errmsg)

    contains

        subroutine clear_derivatives()
        !! no derivatives: clear any from a previous evaluation
        integer :: j
        me%nd = 0
        me%wrt = [integer ::]
        me%wrt_offset = [integer ::]
        if (allocated(me%wrt_names)) deallocate(me%wrt_names)
        if (allocated(me%layout)) deallocate(me%layout)
        if (allocated(me%dsetup)) deallocate(me%dsetup)
        do j = 1, size(me%vals)
            if (allocated(me%vals(j)%d)) deallocate(me%vals(j)%d)
        end do
        end subroutine clear_derivatives

        logical function use_scalar_program(iexpr)
        !! if the scalar program can be used for expression `iexpr`:
        !! all the referenced variables must be scalars. The result is
        !! cached until the size of any value changes.
        integer, intent(in) :: iexpr
        integer :: j
        use_scalar_program = .false.
        if (.not. me%use_scalar_programs .or. .not. me%progs(iexpr)%ok) return
        if (me%prog_version(iexpr) /= me%size_version) then
            me%prog_version(iexpr) = me%size_version
            me%prog_usable(iexpr) = .true.
            do j = 1, size(me%trees(iexpr)%refs)
                if (size(me%vals(me%trees(iexpr)%refs(j))%v) /= 1) then
                    me%prog_usable(iexpr) = .false.
                    exit
                end if
            end do
        end if
        use_scalar_program = me%prog_usable(iexpr)
        end function use_scalar_program

        logical function same_wrt()
        !! true if `wrt` is the same as in the previous call, and the
        !! input variable sizes haven't changed (so the derivative
        !! setup can be reused)
        integer :: j
        same_wrt = .false.
        if (.not. allocated(me%wrt_names) .or. me%nd == 0) return
        if (size(me%wrt_names) /= size(wrt)) return
        do j = 1, size(wrt)
            if (wrt(j) /= me%wrt_names(j)%str) return
        end do
        if (me%sparse_setup .neqv. me%use_sparsity) return
        if (.not. allocated(me%layout)) return
        do j = 1, nin
            if (.not. allocated(me%vals(j)%d)) return
            if (size(me%vals(j)%d, 1) /= size(me%vals(j)%v) .or. &
                size(me%vals(j)%d, 2) /= me%layout(j)%nd) return
        end do
        same_wrt = .true.
        end function same_wrt

    end subroutine evaluate
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set up the derivative computations (internal routine). This is called by
!  [[fparser:evaluate]] when the derivative variables (or the variable sizes) change.
!
!  For each variable, this determines which derivative variables (blocks of columns)
!  it can depend on. With sparsity enabled, an input only depends on itself (if it is
!  a derivative variable), and an output depends on the union of the variables it
!  references (directly, or through other outputs). This is a structural analysis: for
!  `if`, both branches are included. With sparsity disabled, every variable has all
!  the columns.
!
!  Each expression is evaluated using only its own columns. The derivatives of the
!  variables it references are mapped to those columns (`dsetup`).

    subroutine setup_derivatives(me)

    class(fparser), intent(inout) :: me

    integer :: nin, nout, nw, nvar, i, j, k, e, r, n, off, p, q
    logical, dimension(:,:), allocatable :: dep !! `dep(k,j)`: variable `j` depends on block `k`
    integer, dimension(:), allocatable :: bsize !! size of each block

    nin = me%n_inputs
    nout = me%n_outputs
    nvar = nin + nout
    nw = size(me%wrt)
    me%sparse_setup = me%use_sparsity

    bsize = [(size(me%vals(me%wrt(k))%v), k = 1, nw)]

    ! dependencies:
    allocate(dep(nw, nvar), source=.not. me%use_sparsity)
    if (me%use_sparsity) then
        do k = 1, nw
            dep(k, me%wrt(k)) = .true.
        end do
        do i = 1, nout
            e = me%order(i)
            do r = 1, size(me%trees(e)%refs)
                dep(:, nin + e) = dep(:, nin + e) .or. dep(:, me%trees(e)%refs(r))
            end do
        end do
    end if

    ! column layout of each variable:
    if (allocated(me%layout)) deallocate(me%layout)
    allocate(me%layout(nvar))
    do j = 1, nvar
        associate (L => me%layout(j))
            L%blocks = pack([(k, k = 1, nw)], dep(:, j))
            allocate(L%offset(size(L%blocks)))
            off = 0
            do p = 1, size(L%blocks)
                L%offset(p) = off
                off = off + bsize(L%blocks(p))
            end do
            L%nd = off
        end associate
    end do

    ! seed the input derivatives:
    do i = 1, nin
        n = size(me%vals(i)%v)
        if (allocated(me%vals(i)%d)) deallocate(me%vals(i)%d)
        allocate(me%vals(i)%d(n, me%layout(i)%nd), source=0.0_wp)
        k = findloc(me%wrt, i, dim=1)
        if (k > 0) then
            p = findloc(me%layout(i)%blocks, k, dim=1)
            do j = 1, n
                me%vals(i)%d(j, me%layout(i)%offset(p) + j) = 1.0_wp
            end do
        end if
    end do

    ! map the derivatives of the referenced variables to each expression's columns:
    if (allocated(me%dsetup)) deallocate(me%dsetup)
    allocate(me%dsetup(nout))
    do e = 1, nout
        associate (ds => me%dsetup(e), Le => me%layout(nin + e), refs => me%trees(e)%refs)
            allocate(ds%dref(size(refs)), ds%colmap(size(refs)))
            allocate(ds%gather(size(refs)), source=.false.)
            if (Le%nd == 0) cycle ! no derivatives needed
            do r = 1, size(refs)
                j = refs(r)
                associate (Lj => me%layout(j))
                    if (size(Lj%blocks) == size(Le%blocks)) then
                        if (all(Lj%blocks == Le%blocks)) cycle ! same columns: use directly
                    end if
                    allocate(ds%colmap(r)%i(Lj%nd))
                    do p = 1, size(Lj%blocks)
                        q = findloc(Le%blocks, Lj%blocks(p), dim=1) ! always found (dep is a superset)
                        do k = 1, bsize(Lj%blocks(p))
                            ds%colmap(r)%i(Lj%offset(p) + k) = Le%offset(q) + k
                        end do
                    end do
                    if (j <= nin) then
                        ! inputs: constant, so set it now
                        allocate(ds%dref(r)%d(size(me%vals(j)%v), Le%nd), source=0.0_wp)
                        ds%dref(r)%d(:, ds%colmap(r)%i) = me%vals(j)%d
                    else
                        ds%gather(r) = .true. ! outputs: set before each evaluation
                    end if
                end associate
            end do
        end associate
    end do

    end subroutine setup_derivatives
!*****************************************************************************************

!*****************************************************************************************
!>
!  Copy the derivatives of the outputs referenced by expression `e` into its
!  columns (internal routine). See [[setup_derivatives]].

    subroutine gather_derivatives(me, e)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: e !! expression index

    integer :: r, j, m, nd

    associate (ds => me%dsetup(e))
        nd = me%layout(me%n_inputs + e)%nd
        do r = 1, size(ds%gather)
            if (.not. ds%gather(r)) cycle
            j = me%trees(e)%refs(r)
            m = size(me%vals(j)%v)
            if (allocated(ds%dref(r)%d)) then
                if (size(ds%dref(r)%d, 1) /= m) deallocate(ds%dref(r)%d)
            end if
            ! the columns that aren't mapped are always zero:
            if (.not. allocated(ds%dref(r)%d)) allocate(ds%dref(r)%d(m, nd), source=0.0_wp)
            ds%dref(r)%d(:, ds%colmap(r)%i) = me%vals(j)%d
        end do
    end associate

    end subroutine gather_derivatives
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the value of a variable by name (an output, or an input).

    subroutine get_vector_by_name(me, name, value, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! variable name (case insensitive)
    real(wp), dimension(:), allocatable, intent(out) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: idx

    if (me%err%failed) call me%err%clear()
    idx = find_output(me, name)
    if (.not. me%err%failed) then
        if (check_value(me, idx)) value = me%vals(idx)%v
    end if
    call finish(me, status, errmsg)

    end subroutine get_vector_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the value of a scalar variable by name (an output, or an input).

    subroutine get_scalar_by_name(me, name, value, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! variable name (case insensitive)
    real(wp), intent(out) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: idx

    if (me%err%failed) call me%err%clear()
    value = 0.0_wp
    idx = find_output(me, name)
    if (.not. me%err%failed) call get_scalar(me, idx, value)
    call finish(me, status, errmsg)

    end subroutine get_scalar_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the value of the output of an expression by index (in the order given to [[fparser:parse]]).

    subroutine get_vector_by_index(me, iexpr, value, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: iexpr !! expression index
    real(wp), dimension(:), allocatable, intent(out) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    if (me%err%failed) call me%err%clear()
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        if (check_value(me, me%n_inputs + iexpr)) value = me%vals(me%n_inputs + iexpr)%v
    end if
    call finish(me, status, errmsg)

    end subroutine get_vector_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the value of the scalar output of an expression by index (in the order given to [[fparser:parse]]).

    subroutine get_scalar_by_index(me, iexpr, value, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: iexpr !! expression index
    real(wp), intent(out) :: value !! the value
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    if (me%err%failed) call me%err%clear()
    value = 0.0_wp
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_scalar(me, me%n_inputs + iexpr, value)
    end if
    call finish(me, status, errmsg)

    end subroutine get_scalar_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check that a variable (by index in the full variable list) has a value.
!  Raises an error and returns false if not.

    logical function check_value(me, idx) result(ok)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list

    ok = .false.
    if (idx <= me%n_inputs) then
        if (.not. me%is_set(idx)) then
            call me%err%set('variable "'//me%names(idx)%str//'" has not been set')
            return
        end if
    else if (.not. me%evaluated) then
        call me%err%set('the expressions have not been evaluated')
        return
    end if
    ok = .true.

    end function check_value
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a scalar value by variable index, or raise an error if it isn't a scalar.

    subroutine get_scalar(me, idx, value)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list
    real(wp), intent(out) :: value

    value = 0.0_wp
    if (.not. check_value(me, idx)) return
    associate (v => me%vals(idx)%v)
        if (size(v) /= 1) then
            call me%err%set('value is not a scalar (size is '//int_to_str(size(v))//')')
        else
            value = v(1)
        end if
    end associate

    end subroutine get_scalar
!*****************************************************************************************

!*****************************************************************************************
!>
!  Find the derivative columns for an input variable (internal routine).
!  Returns `n=-1` (and raises an error) if they are not available.

    subroutine find_deriv(me, idx, wrt, off, n)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index of the variable in the full variable list
    character(len=*), intent(in) :: wrt !! name of the input variable
    integer, intent(out) :: off !! column offset of the derivative in the variable's derivative
                                !! matrix, or -1 if the derivative is zero (the variable doesn't
                                !! depend on `wrt`)
    integer, intent(out) :: n !! number of columns (size of `wrt`)

    integer :: iwrt, k, p

    off = 0
    n = -1
    if (.not. me%evaluated) then
        call me%err%set('the expressions have not been evaluated')
        return
    end if
    iwrt = find_variable(me, to_lower(trim(adjustl(wrt))), me%n_inputs)
    k = 0
    if (iwrt > 0) k = findloc(me%wrt, iwrt, dim=1)
    if (k == 0) then
        call me%err%set('derivatives with respect to "'//trim(wrt)//'" were not computed')
        return
    end if
    n = size(me%vals(iwrt)%v)
    p = findloc(me%layout(idx)%blocks, k, dim=1)
    if (p == 0) then
        off = -1
    else
        off = me%layout(idx)%offset(p)
    end if

    end subroutine find_deriv
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative matrix of a variable (by index) with respect to an input variable.

    subroutine get_deriv(me, idx, wrt, jac)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list
    character(len=*), intent(in) :: wrt !! name of the input variable
    real(wp), dimension(:,:), allocatable, intent(out) :: jac

    integer :: off, n

    call find_deriv(me, idx, wrt, off, n)
    if (n < 0) return
    if (off < 0) then
        allocate(jac(size(me%vals(idx)%v), n), source=0.0_wp)
    else
        jac = me%vals(idx)%d(:, off+1:off+n)
    end if

    end subroutine get_deriv
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the scalar derivative of a variable (by index) with respect to a scalar input variable.

    subroutine get_deriv_scalar(me, idx, wrt, deriv)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list
    character(len=*), intent(in) :: wrt !! name of the input variable
    real(wp), intent(out) :: deriv

    integer :: off, n

    deriv = 0.0_wp
    call find_deriv(me, idx, wrt, off, n)
    if (n < 0) return
    if (size(me%vals(idx)%v) /= 1 .or. n /= 1) then
        call me%err%set('derivative is not a scalar (shape is ['//int_to_str(size(me%vals(idx)%v))//&
                        ','//int_to_str(n)//'])')
    else if (off >= 0) then
        deriv = me%vals(idx)%d(1, off+1)
    end if

    end subroutine get_deriv_scalar
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative of a variable (by name) with respect to an input variable.
!  The result has shape `[size(name), size(wrt)]`.
!  [[fparser:evaluate]] must have been called with `wrt` in its list.

    subroutine get_derivative_matrix_by_name(me, name, wrt, jac, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! the output (or input) variable name
    character(len=*), intent(in) :: wrt !! the input variable name
    real(wp), dimension(:,:), allocatable, intent(out) :: jac !! the derivative `d(name)/d(wrt)`
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: idx

    if (me%err%failed) call me%err%clear()
    idx = find_output(me, name)
    if (.not. me%err%failed) call get_deriv(me, idx, wrt, jac)
    call finish(me, status, errmsg)

    end subroutine get_derivative_matrix_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative of a scalar variable (by name) with respect to a scalar input variable.
!  [[fparser:evaluate]] must have been called with `wrt` in its list.

    subroutine get_derivative_scalar_by_name(me, name, wrt, deriv, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! the output (or input) variable name
    character(len=*), intent(in) :: wrt !! the input variable name
    real(wp), intent(out) :: deriv !! the derivative `d(name)/d(wrt)`
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    integer :: idx

    if (me%err%failed) call me%err%clear()
    deriv = 0.0_wp
    idx = find_output(me, name)
    if (.not. me%err%failed) call get_deriv_scalar(me, idx, wrt, deriv)
    call finish(me, status, errmsg)

    end subroutine get_derivative_scalar_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative of the output of an expression (by index) with respect to an
!  input variable. The result has shape `[size(output), size(wrt)]`.
!  [[fparser:evaluate]] must have been called with `wrt` in its list.

    subroutine get_derivative_matrix_by_index(me, iexpr, wrt, jac, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: iexpr !! expression index
    character(len=*), intent(in) :: wrt !! the input variable name
    real(wp), dimension(:,:), allocatable, intent(out) :: jac !! the derivative
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    if (me%err%failed) call me%err%clear()
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_deriv(me, me%n_inputs + iexpr, wrt, jac)
    end if
    call finish(me, status, errmsg)

    end subroutine get_derivative_matrix_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative of the scalar output of an expression (by index) with respect
!  to a scalar input variable.
!  [[fparser:evaluate]] must have been called with `wrt` in its list.

    subroutine get_derivative_scalar_by_index(me, iexpr, wrt, deriv, status, errmsg)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: iexpr !! expression index
    character(len=*), intent(in) :: wrt !! the input variable name
    real(wp), intent(out) :: deriv !! the derivative
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    if (me%err%failed) call me%err%clear()
    deriv = 0.0_wp
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_deriv_scalar(me, me%n_inputs + iexpr, wrt, deriv)
    end if
    call finish(me, status, errmsg)

    end subroutine get_derivative_scalar_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Number of input variables.

    pure integer function num_inputs(me)
    class(fparser), intent(in) :: me
    num_inputs = me%n_inputs
    end function num_inputs
!*****************************************************************************************

!*****************************************************************************************
!>
!  Number of expressions (outputs).

    pure integer function num_outputs(me)
    class(fparser), intent(in) :: me
    num_outputs = me%n_outputs
    end function num_outputs
!*****************************************************************************************

!*****************************************************************************************
!>
!  Name of an input variable (lowercase). Empty if the index is invalid.

    pure function input_name(me, i) result(name)
    class(fparser), intent(in) :: me
    integer, intent(in) :: i !! variable index
    character(len=:), allocatable :: name
    name = ''
    if (i >= 1 .and. i <= me%n_inputs .and. allocated(me%names)) name = me%names(i)%str
    end function input_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Name of the output of an expression (lowercase).
!  Empty if the expression is anonymous or the index is invalid.

    pure function output_name(me, i) result(name)
    class(fparser), intent(in) :: me
    integer, intent(in) :: i !! expression index
    character(len=:), allocatable :: name
    name = ''
    if (i >= 1 .and. i <= me%n_outputs .and. allocated(me%names)) name = me%names(me%n_inputs + i)%str
    end function output_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  The order in which the expressions are evaluated (expression indices).

    pure function evaluation_order(me) result(order)
    class(fparser), intent(in) :: me
    integer, dimension(:), allocatable :: order
    if (allocated(me%order)) then
        order = me%order
    else
        allocate(order(0))
    end if
    end function evaluation_order
!*****************************************************************************************

!*****************************************************************************************
    end module fparse
!*****************************************************************************************
