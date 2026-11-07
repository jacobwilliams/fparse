
A Fortran function parser and evaluator for expressions involving multiple variables, supporting vectorized operations, user-defined functions, and first derivatives.

### Features

* **Standard modern Fortran.** Object-oriented, with no compiler extensions. Documented with FORD-compatible doc comments. MIT licensed.
* **Expressions of any number of variables**, such as `sin(x) + 2.0*y`. Each expression is parsed once into an internal tree, and you can then evaluate it as many times as you like with new variable values. When all the variables are scalars, a compiled scalar instruction list is used instead, which is several times faster.
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
pixi run benchmark # run the speed benchmarks and update the table below
```

To use fparse in another fpm project:

```toml
[dependencies]
fparse = { git = "https://github.com/jacobwilliams/fparse" }
```

### Benchmarks

These benchmarks compare fparse with hand-written Fortran and with the other Fortran function parsers from [this list](https://github.com/sdm900/fortran_parser) that are available as fpm packages. Each parser is a dev-dependency used only by [test/benchmark.f90](test/benchmark.f90), which also checks every result against native Fortran. `pixi run benchmark` builds with the fpm `release` profile and rewrites the table below. Running `fpm test` does a short version of the same run, with very little timing.

<!-- BENCHMARK_START -->
Time per evaluation (lower is better). In parentheses: time relative to native Fortran (or relative to fparse for the parse benchmark). "—" means not supported, and "wrong result" means the result did not match native Fortran. Among the parsers (not native Fortran), ✅ marks the fastest correct result in each row and ❌ the slowest.

| Case | Native Fortran | fparse | [fortran_function_parser][ffp] | [feq-parse][feq] | [M_calculator][mcalc] |
|---|---:|---:|---:|---:|---:|
| Simple expression | 0.6 ns | 19.1 ns (33x) | 12.4 ns (21x) ✅ | 139.0 ns (239x) | 54.74 µs (94014x) ❌ |
| Trig expression | 4.3 ns | 25.1 ns (5.8x) ✅ | 25.5 ns (5.9x) | 172.9 ns (40x) | 102.45 µs (23781x) ❌ |
| Polynomial | 0.6 ns | 27.2 ns (43x) ✅ | 36.3 ns (57x) | 1.07 µs (1675x) | 149.28 µs (234755x) ❌ |
| Complex expression | 9.7 ns | 87.3 ns (9.0x) ✅ | 160.5 ns (16x) | 2.08 µs (214x) | 404.92 µs (41577x) ❌ |
| 100 equations (time per set) | 205.5 ns | 2.41 µs (12x) ✅ | 4.62 µs (23x) | 86.24 µs (420x) | 9.79 ms (47646x) ❌ |
| Vector, 1000 points (time per set) | 4.18 µs | 7.54 µs (1.8x) ✅ | 27.01 µs (6.5x) | 13.91 µs (3.3x) | 107.81 ms (25771x) ❌ |
| Parse complex expression | — | 11.10 µs | 105.49 µs (9.5x) ❌ | 7.33 µs (0.7x) ✅ | — |
| Trig expression + gradient | 12.5 ns | 198.3 ns (16x) ✅ | — | — | — |

Expressions (variables `x`, `y`, `z`):

* Simple: `x + y*z`
* Trig: `sin(x)*cos(y) + exp(-z)`
* Polynomial: `((((3*x + 2)*x - 5)*x + 1)*x - 7)*y + z`
* Complex: `sqrt(x^2 + y^2 + z^2)*exp(-x*y/(1 + z^2)) + log(1 + abs(sin(x)*cos(y))) - tan(z/3)^2 + (x - y)/(x + y + 3)`
* 100 equations: `sin(x + k)*y + z^2/k - x*k` for `k = 1, 2, ..., 100`
* Vector: the trig expression evaluated at 1000 points (fparse and feq-parse evaluate the whole array at once; the others loop over the points)
* Gradient: the trig expression and its derivatives with respect to `x`, `y` and `z`

Generated on 2026-11-07 by `pixi run benchmark` ([test/benchmark.f90](test/benchmark.f90)) using GCC version 16.2.0 with options `-mcpu=apple-m1 -mlittle-endian -mabi=lp64 -O3 -funroll-loops -fcoarray=single`.

[ffp]: https://github.com/jacobwilliams/fortran_function_parser
[feq]: https://github.com/FluidNumerics/feq-parse
[mcalc]: https://github.com/urbanjost/M_calculator
<!-- BENCHMARK_END -->

Notes:

* fparse compiles each expression to a scalar instruction list, which it uses whenever all the variables are scalars, including when gradients are requested. Otherwise it uses a tree evaluator that handles vectors and Jacobians, and whose buffers are reused between evaluations. For very short expressions, its fixed cost per call (setting variables, evaluating and getting results, with error checking) is the main cost.
* feq-parse is pinned to a commit on its `master` branch, because its latest release (v2.2.2) evaluates `a - b + c` as `a - (b + c)` (for example, `1 - 2 + 3` gives `-4`). That bug is fixed on `master` ([#44](https://github.com/FluidNumerics/feq-parse/issues/44)) but not in a release yet.
* M_calculator is an interpreter: it processes the expression string on every evaluation, and it has no separate parse step. Variables are passed through its `x(:)` array.
* Some parsers from the list are left out:
  * [ffp](https://github.com/jacobwilliams/ffp) has an `fpm.toml` but doesn't compile under fpm, because it relies on implicit typing.
  * The others ([fortran_parser](https://github.com/sdm900/fortran_parser), [FortranParser](https://github.com/jacopo-chevallard/FortranParser), [Fortran-Expression-Evaluator](https://github.com/ivomarsoares-personal/Fortran-Expression-Evaluator), [calmat](https://github.com/hassaniriad/calmat)) aren't fpm packages.








