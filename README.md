
A Fortran function parser and evaluator for expressions involving multiple variables, supporting vectorized operations, user-defined functions, and first derivatives.

### Features

* **Standard modern Fortran.** Object-oriented, with no compiler extensions. Documented with FORD-compatible doc comments. MIT licensed.
* **Expressions of any number of variables**, such as `sin(x) + 2.0*y`. Each expression is parsed once into an internal tree, and you can then evaluate it as many times as you like with new variable values.
* **Systems of named expressions.** You pass a list of expressions such as `['z = sin(x) + 2.0*y', 'w = z + x']`. An output can be used in other expressions, and the evaluation order is worked out from the dependencies automatically. Circular dependencies are reported as errors.
* **Scalars and vectors.** A variable can be a scalar or a vector, and operations and functions work element by element as in Fortran (`sin(x)` is a vector if `x` is a vector). Scalars are broadcast. You can build vectors with `[a, b, c]` and index them with `v[i]`.
* **Operators:** arithmetic (`+ - * / ^ **`), relational (`== /= < <= > >=`, plus the Fortran `.eq.`-style forms) and logical (`&& || !`, plus `.and. .or. .not.`).
* **Built-in functions:**
  * one argument (e.g. `sin(x)`, `exp`, `log`, `sqrt`, `abs`, `floor`)
  * two or more arguments (e.g. `max(x,y)`, `atan2`, `mod`, `hypot`)
  * reductions (e.g. `sum`, `norm2`, `maxval`)
  * vector functions (`dot_product(x,y)`, `cross(x,y)`)
* **Conditionals:** `if(value_if_true, value_if_false, condition)`, where any nonzero condition counts as true. Conditionals can be nested and work element by element on vectors. With a scalar condition, only the selected branch is evaluated.
* **User-defined functions** with any number of arguments. You can supply a derivative routine for each one; if you don't, finite differences are used.
* **Optional first derivatives** with respect to any of the input variables, using forward-mode automatic differentiation. Depending on the sizes involved, a derivative is a scalar, a vector or a matrix (Jacobian). Derivatives are computed only when you ask for them, so they don't slow down normal evaluation.
* **Error checking.**
  * At parse time: syntax errors, undefined variables and functions, wrong argument counts, and invalid or duplicate names.
  * At runtime: division by zero, domain errors, size mismatches, out-of-bounds indexes, and unset variables.
  * Errors are returned as status codes with messages, and never stop the program.
* **Case-insensitive names:** `x` and `X` refer to the same variable.
* **Built with fpm** and tested with gfortran, with a comprehensive unit test suite that uses test-drive. You can add it to another project as an fpm dependency.

### Usage

```fortran
use fparse

type(fparser) :: p
real(wp) :: w, dwdx
integer :: status
character(len=:), allocatable :: errmsg

! the order of the expressions doesn't matter; it is determined from the dependencies
call p%parse(['x', 'y'], [character(len=20) :: 'w = z + x', 'z = sin(x) + 2.0*y'], &
             status=status, errmsg=errmsg)
if (status /= 0) error stop errmsg

call p%set_variable('x', 1.0_wp)
call p%set_variable('y', 2.0_wp)          ! a vector can also be given: [1.0_wp, 2.0_wp]
call p%evaluate(wrt=['x'], status=status, errmsg=errmsg)  ! omit `wrt` for no derivatives
call p%get('w', w)                        ! or get a vector: real(wp), allocatable :: v(:)
call p%get_derivative('w', 'x', dwdx)     ! or a matrix:     real(wp), allocatable :: jac(:,:)
```

Every method takes the optional outputs `status` (0 means success) and `errmsg`. You can also call `p%failed()` and `p%error_message()`. Errors never stop the program.

#### Expressions

* An expression is written as `name = expr`, or as just `expr` (an anonymous expression, which you can only retrieve by index, e.g. `p%get(1, val)`). You can also give the output names in the `names=` argument of `parse`.
* Operators, from lowest to highest precedence:
  * `||` and `.or.`
  * `&&` and `.and.`
  * `!` and `.not.`
  * `==`, `/=` (or `!=`), `<`, `<=`, `>`, `>=`, and the forms `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, `.ge.`
  * `+` and `-`
  * `*` and `/`
  * unary `-`
  * `^` and `**` (right associative, so `-x**2` is `-(x**2)`)
* Relational and logical operators return `1.0` (true) or `0.0` (false). Any nonzero value counts as true.
* Vectors: `[a, b, 1]` builds a vector by concatenation, and `v[i]` indexes one (1-based). Scalars are broadcast in element-wise operations.
* The constant `pi` is predefined. A variable named `pi` takes precedence over it.
* Names are case insensitive.

#### Built-in functions

| Kind | Functions |
|---|---|
| one argument, element-wise | `abs sqrt exp log (ln) log10 sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh erf erfc floor ceiling (ceil) nint (anint, round) aint (trunc)` |
| two arguments, element-wise | `atan2(y,x) mod(a,p) modulo(a,p) sign(a,b) hypot(x,y)` |
| two or more arguments | `max(a,b,...) min(a,b,...)` |
| reductions (scalar result) | `sum product norm2 (norm) maxval minval size` |
| vector | `dot_product(a,b) (dot)`, `cross(a,b)` |
| conditional | `if(value_if_true, value_if_false, condition)` (alias `merge`, same as Fortran's `merge`) |

When the condition of an `if` is a scalar, only the selected branch is evaluated, so `if(log(x), 0, x > 0)` is safe for `x <= 0`. When the condition is a vector, the selection is element-wise and both branches are evaluated.

#### Runtime errors

Evaluation checks for division by zero, domain errors (`sqrt`/`log` of negative numbers, `asin(2)`, a negative number raised to a non-integer power, and so on), size mismatches, out-of-bounds indexes, and unset variables. To get IEEE Inf/NaN results instead of domain errors, call `p%set_domain_checking(.false.)`.

#### Derivatives

`p%evaluate(wrt=['x','y'])` also computes the first derivatives of every output (and every intermediate output) with respect to the listed input variables, using forward-mode automatic differentiation. For an output of size `m` and a variable of size `n`, the derivative is an `[m, n]` matrix. Without `wrt`, no derivative work is done. The derivatives of piecewise functions (`abs`, `max`, `if`, ...) are those of the active branch.

#### User-defined functions

```fortran
subroutine my_func(args, res, stat)   ! interface: fparse_func
    type(fparse_value), intent(in) :: args(:)       ! args(i)%v is the i-th argument (scalar = size 1)
    real(wp), allocatable, intent(out) :: res(:)
    integer, intent(inout) :: stat                  ! set nonzero to signal an error
    res = args(1)%v**2 + args(2)%v
end subroutine

call p%add_function('f', my_func, nargs=2, df=my_dfunc)  ! call before parse; df is optional
```

The optional derivative routine (interface `fparse_dfunc`) returns the Jacobian of the result with respect to `args(iarg)`. If you don't provide one, central finite differences are used. User functions take precedence over built-in functions with the same name.

### Building and testing

With [pixi](https://pixi.sh):

```
pixi run build     # fpm build
pixi run test      # fpm test
pixi run example   # run example/example.f90
pixi run docs      # FORD documentation in ./doc
```

To use fparse in another fpm project:

```toml
[dependencies]
fparse = { git = "https://github.com/jacobwilliams/fparse" }
```
