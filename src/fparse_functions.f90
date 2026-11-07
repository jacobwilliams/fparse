!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Built-in function table and the interfaces for user-defined functions.
!
!### Built-in functions
!
!  Elemental functions of one argument (result has the size of the argument):
!
!  `abs`, `sqrt`, `exp`, `log` (`ln`), `log10`, `sin`, `cos`, `tan`, `asin`, `acos`,
!  `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `erf`, `erfc`,
!  `floor`, `ceiling` (`ceil`), `nint` (`anint`, `round`), `aint` (`trunc`)
!
!  Elemental functions of two arguments (scalars are broadcast to vectors):
!
!  `atan2(y,x)`, `mod(a,p)`, `modulo(a,p)`, `sign(a,b)`, `hypot(x,y)`
!
!  Elemental functions of two or more arguments: `max(a,b,...)`, `min(a,b,...)`
!
!  Reductions (vector argument, scalar result):
!
!  `sum`, `product`, `norm2` (`norm`), `maxval`, `minval`, `size`
!
!  Vector functions: `dot_product(a,b)` (`dot`), `cross(a,b)` (3-element vectors)
!
!  Conditional: `if(a, b, cond)` (`merge`). Returns `a` where `cond /= 0`,
!  and `b` where `cond == 0`.

module fparse_functions

use fparse_common

implicit none (type, external)

private

! built-in function ids:
integer, parameter, public :: fn_abs     = 1
integer, parameter, public :: fn_sqrt    = 2
integer, parameter, public :: fn_exp     = 3
integer, parameter, public :: fn_log     = 4
integer, parameter, public :: fn_log10   = 5
integer, parameter, public :: fn_sin     = 6
integer, parameter, public :: fn_cos     = 7
integer, parameter, public :: fn_tan     = 8
integer, parameter, public :: fn_asin    = 9
integer, parameter, public :: fn_acos    = 10
integer, parameter, public :: fn_atan    = 11
integer, parameter, public :: fn_sinh    = 12
integer, parameter, public :: fn_cosh    = 13
integer, parameter, public :: fn_tanh    = 14
integer, parameter, public :: fn_asinh   = 15
integer, parameter, public :: fn_acosh   = 16
integer, parameter, public :: fn_atanh   = 17
integer, parameter, public :: fn_erf     = 18
integer, parameter, public :: fn_erfc    = 19
integer, parameter, public :: fn_floor   = 20
integer, parameter, public :: fn_ceiling = 21
integer, parameter, public :: fn_nint    = 22
integer, parameter, public :: fn_aint    = 23
integer, parameter, public :: fn_last_elemental = fn_aint !! last of the one-argument elemental functions
integer, parameter, public :: fn_atan2   = 24
integer, parameter, public :: fn_mod     = 25
integer, parameter, public :: fn_modulo  = 26
integer, parameter, public :: fn_sign    = 27
integer, parameter, public :: fn_hypot   = 28
integer, parameter, public :: fn_max     = 29
integer, parameter, public :: fn_min     = 30
integer, parameter, public :: fn_sum     = 31
integer, parameter, public :: fn_product = 32
integer, parameter, public :: fn_norm2   = 33
integer, parameter, public :: fn_maxval  = 34
integer, parameter, public :: fn_minval  = 35
integer, parameter, public :: fn_size    = 36
integer, parameter, public :: fn_dot     = 37
integer, parameter, public :: fn_cross   = 38
integer, parameter, public :: fn_if      = 39

type :: builtin_t
    !! information about a built-in function
    character(len=12) :: name = '' !! function name
    integer :: id = 0 !! function id (`fn_*` parameter)
    integer :: min_args = 1 !! minimum number of arguments
    integer :: max_args = 1 !! maximum number of arguments (-1 for unlimited)
end type builtin_t

type(builtin_t), dimension(*), parameter :: builtins = [ &
    builtin_t('abs',         fn_abs,     1, 1), &
    builtin_t('sqrt',        fn_sqrt,    1, 1), &
    builtin_t('exp',         fn_exp,     1, 1), &
    builtin_t('log',         fn_log,     1, 1), &
    builtin_t('ln',          fn_log,     1, 1), &
    builtin_t('log10',       fn_log10,   1, 1), &
    builtin_t('sin',         fn_sin,     1, 1), &
    builtin_t('cos',         fn_cos,     1, 1), &
    builtin_t('tan',         fn_tan,     1, 1), &
    builtin_t('asin',        fn_asin,    1, 1), &
    builtin_t('acos',        fn_acos,    1, 1), &
    builtin_t('atan',        fn_atan,    1, 1), &
    builtin_t('sinh',        fn_sinh,    1, 1), &
    builtin_t('cosh',        fn_cosh,    1, 1), &
    builtin_t('tanh',        fn_tanh,    1, 1), &
    builtin_t('asinh',       fn_asinh,   1, 1), &
    builtin_t('acosh',       fn_acosh,   1, 1), &
    builtin_t('atanh',       fn_atanh,   1, 1), &
    builtin_t('erf',         fn_erf,     1, 1), &
    builtin_t('erfc',        fn_erfc,    1, 1), &
    builtin_t('floor',       fn_floor,   1, 1), &
    builtin_t('ceiling',     fn_ceiling, 1, 1), &
    builtin_t('ceil',        fn_ceiling, 1, 1), &
    builtin_t('nint',        fn_nint,    1, 1), &
    builtin_t('anint',       fn_nint,    1, 1), &
    builtin_t('round',       fn_nint,    1, 1), &
    builtin_t('aint',        fn_aint,    1, 1), &
    builtin_t('trunc',       fn_aint,    1, 1), &
    builtin_t('atan2',       fn_atan2,   2, 2), &
    builtin_t('mod',         fn_mod,     2, 2), &
    builtin_t('modulo',      fn_modulo,  2, 2), &
    builtin_t('sign',        fn_sign,    2, 2), &
    builtin_t('hypot',       fn_hypot,   2, 2), &
    builtin_t('max',         fn_max,     2, -1), &
    builtin_t('min',         fn_min,     2, -1), &
    builtin_t('sum',         fn_sum,     1, 1), &
    builtin_t('product',     fn_product, 1, 1), &
    builtin_t('norm2',       fn_norm2,   1, 1), &
    builtin_t('norm',        fn_norm2,   1, 1), &
    builtin_t('maxval',      fn_maxval,  1, 1), &
    builtin_t('minval',      fn_minval,  1, 1), &
    builtin_t('size',        fn_size,    1, 1), &
    builtin_t('dot_product', fn_dot,     2, 2), &
    builtin_t('dot',         fn_dot,     2, 2), &
    builtin_t('cross',       fn_cross,   2, 2), &
    builtin_t('if',          fn_if,      3, 3), &
    builtin_t('merge',       fn_if,      3, 3) ]

type, public :: fparse_value
    !! A value passed to a user-defined function.
    !! Scalars are represented as arrays of size 1.
    real(wp), dimension(:), allocatable :: v !! the value
end type fparse_value

abstract interface
    subroutine fparse_func(args, res, stat)
        !! Interface for a user-defined function.
        import :: wp, fparse_value
        implicit none
        type(fparse_value), dimension(:), intent(in) :: args !! the function arguments
        real(wp), dimension(:), allocatable, intent(out) :: res !! the function result
        integer, intent(inout) :: stat !! status code. It is zero on input.
                                       !! Set to a nonzero value to indicate an error.
    end subroutine fparse_func
    subroutine fparse_dfunc(args, iarg, jac, stat)
        !! Interface for the derivative of a user-defined function.
        import :: wp, fparse_value
        implicit none
        type(fparse_value), dimension(:), intent(in) :: args !! the function arguments
        integer, intent(in) :: iarg !! the index of the argument to differentiate with respect to
        real(wp), dimension(:,:), allocatable, intent(out) :: jac !! the jacobian `d(res)/d(args(iarg)%v)`,
                                                                  !! with shape `[size(res), size(args(iarg)%v)]`
        integer, intent(inout) :: stat !! status code. It is zero on input.
                                       !! Set to a nonzero value to indicate an error.
    end subroutine fparse_dfunc
end interface
public :: fparse_func, fparse_dfunc

type, public :: user_function
    !! A user-defined function.
    character(len=:), allocatable :: name !! function name (lowercase)
    integer :: nargs = -1 !! number of arguments (-1 for any number)
    logical :: is_pure = .false. !! if the function is pure: its result only depends on its
                                 !! arguments (so it can be cached)
    procedure(fparse_func), pointer, nopass :: f => null() !! the function
    procedure(fparse_dfunc), pointer, nopass :: df => null() !! the derivative function (optional).
                                                             !! If not associated, finite differences are used.
end type user_function

public :: builtin_lookup

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Look up a built-in function by name.

pure subroutine builtin_lookup(name, found, id, min_args, max_args)

    character(len=*), intent(in) :: name !! function name (lowercase)
    logical, intent(out) :: found !! true if the function was found
    integer, intent(out) :: id !! function id
    integer, intent(out) :: min_args !! minimum number of arguments
    integer, intent(out) :: max_args !! maximum number of arguments (-1 for unlimited)

    integer :: i

    found = .false.
    id = 0
    min_args = 0
    max_args = 0
    if (len(name) > len(builtins(1)%name)) return
    do i = 1, size(builtins)
        if (builtins(i)%name == name) then
            found = .true.
            id = builtins(i)%id
            min_args = builtins(i)%min_args
            max_args = builtins(i)%max_args
            return
        end if
    end do

end subroutine builtin_lookup
!*****************************************************************************************

!*****************************************************************************************
end module fparse_functions
!*****************************************************************************************
