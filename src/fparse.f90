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

    implicit none

    private

    public :: wp
    public :: fparse_value, fparse_func, fparse_dfunc

    type, public :: fparser
        !! The function parser class.
        private
        integer :: n_inputs = 0 !! number of input variables
        integer :: n_outputs = 0 !! number of expressions
        type(string_t), dimension(:), allocatable :: names !! all variable names (inputs, then outputs).
                                                            !! Anonymous outputs have empty names.
        type(string_t), dimension(:), allocatable :: expressions !! the expression strings
        type(expr_tree), dimension(:), allocatable :: trees !! the parsed expressions
        integer, dimension(:), allocatable :: order !! evaluation order of the expressions
        type(val_t), dimension(:), allocatable :: vals !! values of all the variables (inputs, then outputs)
        logical, dimension(:), allocatable :: is_set !! if each input variable has been set
        type(user_function), dimension(:), allocatable :: funcs !! user-defined functions
        logical :: parsed = .false. !! if the expressions have been parsed
        logical :: evaluated = .false. !! if the expressions have been evaluated
        integer :: nd = 0 !! total number of derivative columns
        integer, dimension(:), allocatable :: wrt !! input variable indices for derivatives
        integer, dimension(:), allocatable :: wrt_offset !! column offsets in the derivative matrices
        logical :: check_domain = .true. !! check for domain errors during evaluation
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

    call me%err%clear()

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
    if (present(errmsg)) errmsg = me%error_message()

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

    me%check_domain = check

    end subroutine set_domain_checking
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

    subroutine add_function(me, name, f, nargs, df, status, errmsg)

    class(fparser), intent(inout) :: me
    character(len=*), intent(in) :: name !! function name (case insensitive)
    procedure(fparse_func) :: f !! the function
    integer, intent(in), optional :: nargs !! number of arguments (default is -1, meaning any number)
    procedure(fparse_dfunc), optional :: df !! the derivative of the function
    integer, intent(out), optional :: status !! status code (0 if no errors)
    character(len=:), allocatable, intent(out), optional :: errmsg !! error message (if any)

    type(user_function) :: uf
    integer :: k

    call me%err%clear()
    if (.not. allocated(me%funcs)) allocate(me%funcs(0))

    uf%name = to_lower(trim(adjustl(name)))
    if (.not. is_valid_name(uf%name)) then
        call me%err%set('invalid function name "'//trim(name)//'"')
    else
        uf%f => f
        if (present(df)) uf%df => df
        if (present(nargs)) uf%nargs = nargs
        do k = 1, size(me%funcs)
            if (me%funcs(k)%name == uf%name) then
                me%funcs(k) = uf
                exit
            end if
        end do
        if (k > size(me%funcs)) me%funcs = [me%funcs, uf]
    end if

    call me%finish(status, errmsg)

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
    call me%err%clear()
    me%parsed = .false.
    me%evaluated = .false.
    me%nd = 0
    if (allocated(me%names)) deallocate(me%names)
    if (allocated(me%expressions)) deallocate(me%expressions)
    if (allocated(me%trees)) deallocate(me%trees)
    if (allocated(me%order)) deallocate(me%order)
    if (allocated(me%vals)) deallocate(me%vals)
    if (allocated(me%is_set)) deallocate(me%is_set)
    if (allocated(me%wrt)) deallocate(me%wrt)
    if (allocated(me%wrt_offset)) deallocate(me%wrt_offset)
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
            if (me%find_variable(name, i-1) > 0) then
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
                if (me%find_variable(name, nin + i - 1) > 0) then
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
        call me%sort_expressions()
        if (me%err%failed) exit main

        allocate(me%vals(nin + nout))
        allocate(me%is_set(nin), source=.false.)
        me%parsed = .true.

    end block main

    call me%finish(status, errmsg)

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

    idx = me%find_variable(to_lower(trim(adjustl(name))))
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
        me%vals(idx)%v = value
        me%is_set(idx) = .true.
        me%evaluated = .false.
    end if

    end subroutine set_value
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

    call me%set_vector_by_name(name, [value], status, errmsg)

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

    call me%err%clear()
    idx = me%find_variable(to_lower(trim(adjustl(name))), me%n_inputs)
    if (idx == 0) then
        if (me%find_variable(to_lower(trim(adjustl(name)))) > 0) then
            call me%err%set('"'//trim(name)//'" is an output and cannot be set')
        else
            call me%err%set('unknown input variable "'//trim(name)//'"')
        end if
    else
        call me%set_value(idx, value)
    end if
    call me%finish(status, errmsg)

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

    call me%set_vector_by_index(idx, [value], status, errmsg)

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

    call me%err%clear()
    call me%set_value(idx, value)
    call me%finish(status, errmsg)

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

    call me%err%clear()
    if (size(values) /= me%n_inputs) then
        call me%err%set('number of values ('//int_to_str(size(values))//&
                        ') does not match the number of variables ('//int_to_str(me%n_inputs)//')')
    else
        do i = 1, me%n_inputs
            call me%set_value(i, values(i:i))
            if (me%err%failed) exit
        end do
    end if
    call me%finish(status, errmsg)

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
    type(val_t) :: res
    integer :: i, j, k, n, nin, idx, off

    call me%err%clear()
    me%evaluated = .false.
    me%nd = 0
    if (allocated(me%wrt)) deallocate(me%wrt)
    if (allocated(me%wrt_offset)) deallocate(me%wrt_offset)
    nin = me%n_inputs

    main: block

        if (.not. me%parsed) then
            call me%err%set('the expressions have not been parsed')
            exit main
        end if

        do i = 1, nin
            if (.not. me%is_set(i)) then
                call me%err%set('variable "'//me%names(i)%str//'" has not been set')
                exit main
            end if
        end do

        ! set up the derivatives:
        allocate(me%wrt(0), me%wrt_offset(0))
        if (present(wrt)) then
            off = 0
            do k = 1, size(wrt)
                idx = me%find_variable(to_lower(trim(adjustl(wrt(k)))), nin)
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
        end if

        do i = 1, nin
            if (allocated(me%vals(i)%d)) deallocate(me%vals(i)%d)
            if (size(me%wrt) > 0) then
                n = size(me%vals(i)%v)
                allocate(me%vals(i)%d(n, me%nd), source=0.0_wp)
                do k = 1, size(me%wrt)
                    if (me%wrt(k) == i) then
                        do j = 1, n
                            me%vals(i)%d(j, me%wrt_offset(k) + j) = 1.0_wp
                        end do
                    end if
                end do
            end if
        end do

        ! evaluate the expressions:
        ctx%nd = me%nd
        ctx%check_domain = me%check_domain
        do k = 1, me%n_outputs
            i = me%order(k)
            call eval_tree(me%trees(i), me%vals, me%funcs, ctx, res)
            if (ctx%err%failed) then
                call me%err%set('error evaluating expression '//int_to_str(i)//' "'//&
                                me%expressions(i)%str//'": '//ctx%err%message, ctx%err%pos)
                exit main
            end if
            call move_alloc(res%v, me%vals(nin + i)%v)
            if (allocated(me%vals(nin + i)%d)) deallocate(me%vals(nin + i)%d)
            if (allocated(res%d)) call move_alloc(res%d, me%vals(nin + i)%d)
        end do

        me%evaluated = .true.

    end block main

    call me%finish(status, errmsg)

    end subroutine evaluate
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

    call me%err%clear()
    idx = me%find_output(name)
    if (.not. me%err%failed) call get_value(me, idx, value)
    call me%finish(status, errmsg)

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
    real(wp), dimension(:), allocatable :: v

    call me%err%clear()
    value = 0.0_wp
    idx = me%find_output(name)
    if (.not. me%err%failed) call get_value(me, idx, v)
    if (.not. me%err%failed) call to_scalar(me, v, value)
    call me%finish(status, errmsg)

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

    call me%err%clear()
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_value(me, me%n_inputs + iexpr, value)
    end if
    call me%finish(status, errmsg)

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

    real(wp), dimension(:), allocatable :: v

    call me%err%clear()
    value = 0.0_wp
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_value(me, me%n_inputs + iexpr, v)
        if (.not. me%err%failed) call to_scalar(me, v, value)
    end if
    call me%finish(status, errmsg)

    end subroutine get_scalar_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a value by variable index (internal routine).

    subroutine get_value(me, idx, value)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list
    real(wp), dimension(:), allocatable, intent(out) :: value

    if (idx <= me%n_inputs) then
        if (.not. me%is_set(idx)) then
            call me%err%set('variable "'//me%names(idx)%str//'" has not been set')
            return
        end if
    else if (.not. me%evaluated) then
        call me%err%set('the expressions have not been evaluated')
        return
    end if
    value = me%vals(idx)%v

    end subroutine get_value
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a size-1 array to a scalar, or raise an error.

    subroutine to_scalar(me, v, value)

    class(fparser), intent(inout) :: me
    real(wp), dimension(:), intent(in) :: v
    real(wp), intent(out) :: value

    if (size(v) /= 1) then
        call me%err%set('value is not a scalar (size is '//int_to_str(size(v))//')')
        value = 0.0_wp
    else
        value = v(1)
    end if

    end subroutine to_scalar
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the derivative of a variable with respect to an input variable
!  (internal routine). Returns a matrix of shape `[size(var), size(wrt)]`.

    subroutine get_deriv(me, idx, wrt, jac)

    class(fparser), intent(inout) :: me
    integer, intent(in) :: idx !! index in the full variable list
    character(len=*), intent(in) :: wrt !! name of the input variable
    real(wp), dimension(:,:), allocatable, intent(out) :: jac

    integer :: iwrt, k, n

    if (.not. me%evaluated) then
        call me%err%set('the expressions have not been evaluated')
        return
    end if
    iwrt = me%find_variable(to_lower(trim(adjustl(wrt))), me%n_inputs)
    k = 0
    if (iwrt > 0) k = findloc(me%wrt, iwrt, dim=1)
    if (k == 0) then
        call me%err%set('derivatives with respect to "'//trim(wrt)//'" were not computed')
        return
    end if
    n = size(me%vals(iwrt)%v)
    jac = me%vals(idx)%d(:, me%wrt_offset(k)+1 : me%wrt_offset(k)+n)

    end subroutine get_deriv
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

    call me%err%clear()
    idx = me%find_output(name)
    if (.not. me%err%failed) call get_deriv(me, idx, wrt, jac)
    call me%finish(status, errmsg)

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

    real(wp), dimension(:,:), allocatable :: jac
    integer :: idx

    call me%err%clear()
    deriv = 0.0_wp
    idx = me%find_output(name)
    if (.not. me%err%failed) call get_deriv(me, idx, wrt, jac)
    if (.not. me%err%failed) call jac_to_scalar(me, jac, deriv)
    call me%finish(status, errmsg)

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

    call me%err%clear()
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_deriv(me, me%n_inputs + iexpr, wrt, jac)
    end if
    call me%finish(status, errmsg)

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

    real(wp), dimension(:,:), allocatable :: jac

    call me%err%clear()
    deriv = 0.0_wp
    if (iexpr < 1 .or. iexpr > me%n_outputs) then
        call me%err%set('invalid expression index '//int_to_str(iexpr))
    else
        call get_deriv(me, me%n_inputs + iexpr, wrt, jac)
        if (.not. me%err%failed) call jac_to_scalar(me, jac, deriv)
    end if
    call me%finish(status, errmsg)

    end subroutine get_derivative_scalar_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a 1x1 matrix to a scalar, or raise an error.

    subroutine jac_to_scalar(me, jac, deriv)

    class(fparser), intent(inout) :: me
    real(wp), dimension(:,:), intent(in) :: jac
    real(wp), intent(out) :: deriv

    if (size(jac, 1) /= 1 .or. size(jac, 2) /= 1) then
        call me%err%set('derivative is not a scalar (shape is ['//int_to_str(size(jac, 1))//&
                        ','//int_to_str(size(jac, 2))//'])')
        deriv = 0.0_wp
    else
        deriv = jac(1, 1)
    end if

    end subroutine jac_to_scalar
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
