!*****************************************************************************************
!>
!  Speed benchmark: fparse vs. native Fortran and other Fortran function parsers.
!
!  The other parsers (dev-dependencies in `fpm.toml`) are:
!
!  * [fortran_function_parser](https://github.com/jacobwilliams/fortran_function_parser)
!  * [feq-parse](https://github.com/FluidNumerics/feq-parse)
!  * [M_calculator](https://github.com/urbanjost/M_calculator)
!
!  Every result is also checked against the native Fortran result, so this
!  doubles as a test (it fails if any parser gives a wrong answer).
!
!  Usage:
!```
!  fpm test benchmark --profile release                    ! print the table
!  fpm test benchmark --profile release -- --update-readme ! also update README.md
!```
!  When run without `--update-readme` (e.g., as part of `fpm test`), a short
!  timing target is used so that it runs quickly.

    module benchmark_module

    use iso_fortran_env, only: int64, output_unit, compiler_version, compiler_options
    use fparse, only: fparser, wp
    use function_parser, only: ffp_parser => fparser, ffp_parser_array => fparser_array
    use FEQParse, only: EquationParser
    use M_calculator, only: dnum0, mc_x => x

    implicit none

    private

    public :: run_benchmarks

    ! implementations:
    integer, parameter :: i_native = 1
    integer, parameter :: i_fparse = 2
    integer, parameter :: i_ffp    = 3
    integer, parameter :: i_feq    = 4
    integer, parameter :: i_mcalc  = 5
    integer, parameter :: n_impl   = 5
    character(len=*), dimension(n_impl), parameter :: impl_names = [character(len=50) :: &
        'Native Fortran', 'fparse', '[fortran_function_parser][ffp]', '[feq-parse][feq]', '[M_calculator][mcalc]']

    ! benchmark cases:
    integer, parameter :: c_simple  = 1
    integer, parameter :: c_trig    = 2
    integer, parameter :: c_poly    = 3
    integer, parameter :: c_complex = 4
    integer, parameter :: c_many    = 5
    integer, parameter :: c_vector  = 6
    integer, parameter :: c_parse   = 7
    integer, parameter :: c_grad    = 8
    integer, parameter :: n_cases   = 8
    character(len=*), dimension(n_cases), parameter :: case_names = [character(len=60) :: &
        'Simple expression', &
        'Trig expression', &
        'Polynomial', &
        'Complex expression', &
        '100 equations (time per set)', &
        'Vector, 1000 points (time per set)', &
        'Parse complex expression', &
        'Trig expression + gradient' ]

    ! non-ASCII symbols used in the table, as UTF-8 byte sequences
    ! (so that this source file is pure ASCII):
    character(len=*), parameter :: sym_check = char(226)//char(156)//char(133) !! U+2705 (white heavy check mark)
    character(len=*), parameter :: sym_cross = char(226)//char(157)//char(140) !! U+274C (cross mark)
    character(len=*), parameter :: sym_dash  = char(226)//char(128)//char(148) !! U+2014 (em dash)
    character(len=*), parameter :: sym_micro = char(194)//char(181)            !! U+00B5 (micro sign)

    integer, parameter :: n_many = 100 !! number of equations in the "many" case
    integer, parameter :: n_pts = 1000 !! number of points in the "vector" case
    character(len=1), dimension(3), parameter :: vars = ['x', 'y', 'z']

    ! the expressions:
    character(len=*), parameter :: expr_simple = 'x + y*z'
    character(len=*), parameter :: expr_trig = 'sin(x)*cos(y) + exp(-z)'
    character(len=*), parameter :: expr_poly = '((((3*x + 2)*x - 5)*x + 1)*x - 7)*y + z'
    character(len=*), parameter :: expr_complex = &
        'sqrt(x^2 + y^2 + z^2)*exp(-x*y/(1 + z^2)) + log(1 + abs(sin(x)*cos(y))) - tan(z/3)^2 + (x - y)/(x + y + 3)'

    ! state for the current case:
    integer :: icase = 0
    real(wp), dimension(n_pts) :: xs, ys, zs
    type(fparser) :: fp
    type(ffp_parser) :: ffp1
    type(ffp_parser_array) :: ffpa
    type(EquationParser) :: feq1
    type(EquationParser), dimension(:), allocatable :: feqa
    character(len=:), allocatable :: mc_expr
    character(len=200), dimension(n_many) :: many_exprs
    real(wp) :: sink = 0.0_wp !! sum of all checksums (printed, so the compiler can't optimize the loops away)
    character(len=200), dimension(n_many) :: mc_many_exprs !! `many_exprs` in M_calculator syntax

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Run all the benchmarks and print the results table.

    subroutine run_benchmarks()

    real(wp), dimension(n_impl, n_cases) :: t !! time per evaluation [sec] (<0 if not applicable)
    logical, dimension(n_impl, n_cases) :: ok !! if the result was correct
    character(len=:), allocatable :: table
    real(wp) :: target_time, cs_native, cs
    integer :: impl, i
    logical :: update_readme
    character(len=100) :: arg

    update_readme = .false.
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        if (arg == '--update-readme') update_readme = .true.
    end do
    target_time = merge(0.5_wp, 0.01_wp, update_readme)

    do i = 1, n_pts
        xs(i) = 0.1_wp + 0.9_wp * i / n_pts
        ys(i) = 1.5_wp - real(i, wp) / n_pts
        zs(i) = 0.2_wp + 0.5_wp * i / n_pts
    end do
    do i = 1, n_many
        many_exprs(i) = 'sin(x + '//itoa(i)//')*y + z^2/'//itoa(i)//' - x*'//itoa(i)
    end do

    t = -1.0_wp
    ok = .true.
    do icase = 1, n_cases
        write(output_unit, '(A)') 'Running: '//trim(case_names(icase))
        do impl = 1, n_impl
            if (.not. supported(impl, icase)) cycle
            call setup(impl)
            ! check the result against the native one:
            if (icase /= c_parse) then
                call run(i_native, 64, cs_native)
                call run(impl, 64, cs)
                ok(impl, icase) = abs(cs - cs_native) <= 1.0e-9_wp * max(1.0_wp, abs(cs_native))
                if (.not. ok(impl, icase)) write(output_unit, '(A,2ES24.15)') &
                    '  WRONG RESULT for '//trim(impl_names(impl))//': ', cs, cs_native
            end if
            t(impl, icase) = time_it(impl, target_time)
        end do
    end do

    table = make_table(t, ok)
    write(output_unit, '(/A)') table
    write(output_unit, '(/A,ES24.15)') 'checksum: ', sink
    if (update_readme) call write_readme(table)

    ! only fparse is required to be correct (the others are reported in the table):
    if (.not. all(ok(i_fparse,:))) error stop 'fparse produced wrong results'

    end subroutine run_benchmarks
!*****************************************************************************************

!*****************************************************************************************
!>
!  If the implementation supports the case.

    pure logical function supported(impl, icase)
    integer, intent(in) :: impl, icase
    select case (icase)
    case (c_parse)
        supported = impl /= i_native .and. impl /= i_mcalc  ! M_calculator has no separate parse step
    case (c_grad)
        supported = impl == i_native .or. impl == i_fparse  ! only fparse has derivatives
    case default
        supported = .true.
    end select
    end function supported
!*****************************************************************************************

!*****************************************************************************************
!>
!  The expression for the current case (scalar cases).

    function case_expr() result(e)
    character(len=200) :: e  ! note: a deferred-length result causes an ICE in gfortran 16.2
    select case (icase)
    case (c_simple); e = expr_simple
    case (c_trig, c_vector, c_grad); e = expr_trig
    case (c_poly); e = expr_poly
    case (c_complex, c_parse); e = expr_complex
    case default; e = ''
    end select
    end function case_expr
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set up an implementation for the current case (parse the expressions).

    subroutine setup(impl)

    integer, intent(in) :: impl

    integer :: k

    select case (impl)
    case (i_fparse)
        if (icase == c_many) then
            call fp%parse(vars, many_exprs)
        else
            call fp%parse(vars, [trim(case_expr())])
        end if
        if (fp%failed()) error stop 'fparse: '//fp%error_message()
    case (i_ffp)
        if (icase == c_many) then
            call ffpa%parse(many_exprs, vars)
        else
            call ffp1%parse(trim(case_expr()), vars)
        end if
    case (i_feq)
        if (icase == c_many) then
            if (allocated(feqa)) deallocate(feqa)
            allocate(feqa(n_many))
            do k = 1, n_many
                feqa(k) = EquationParser('f = '//trim(many_exprs(k)), vars)
            end do
        else
            feq1 = EquationParser('f = '//trim(case_expr()), vars)
        end if
    case (i_mcalc)
        mc_expr = to_mcalc(trim(case_expr()))
        do k = 1, n_many
            mc_many_exprs(k) = to_mcalc(many_exprs(k))
        end do
    end select

    end subroutine setup
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert an expression to M_calculator syntax: it uses `**` rather than `^`,
!  and the variables are passed using its `x(:)` array, so `x`, `y`, `z`
!  become `x(1)`, `x(2)`, `x(3)`.

    pure function to_mcalc(str) result(s)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: s
    integer :: i, ivar
    logical :: isolated
    s = ''
    do i = 1, len_trim(str)
        ivar = index('xyz', str(i:i))
        isolated = .true.
        if (i > 1) isolated = .not. is_letter(str(i-1:i-1))
        if (i < len_trim(str)) isolated = isolated .and. .not. is_letter(str(i+1:i+1))
        if (str(i:i) == '^') then
            s = s//'**'
        else if (ivar > 0 .and. isolated) then
            s = s//'x('//itoa(ivar)//')'
        else
            s = s//str(i:i)
        end if
    end do
    end function to_mcalc

    pure logical function is_letter(c)
    character(len=1), intent(in) :: c
    is_letter = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
    end function is_letter
!*****************************************************************************************

!*****************************************************************************************
!>
!  Variable values for iteration `i`.

    pure subroutine inputs(i, x, y, z)
    integer, intent(in) :: i
    real(wp), intent(out) :: x, y, z
    real(wp) :: d
    d = 1.0e-7_wp * mod(i, 1000)
    x = 0.5_wp + d
    y = 1.3_wp - d
    z = 0.7_wp + 2.0_wp*d
    end subroutine inputs
!*****************************************************************************************

!*****************************************************************************************
!>
!  Time an implementation for the current case. Returns the time per iteration [sec].
!  The number of iterations is increased until the total time exceeds `target_time`.

    real(wp) function time_it(impl, target_time) result(t)

    integer, intent(in) :: impl
    real(wp), intent(in) :: target_time

    integer :: n
    integer(int64) :: c1, c2, rate
    real(wp) :: elapsed, cs

    n = 1
    do
        call system_clock(c1, rate)
        call run(impl, n, cs)
        call system_clock(c2)
        sink = sink + cs
        elapsed = real(c2 - c1, wp) / real(rate, wp)
        if (elapsed >= target_time .or. n >= huge(n)/4) exit
        ! estimate the number of iterations needed:
        if (elapsed <= 0.0_wp) then
            n = n * 10
        else
            n = int(min(real(n, wp) * 10.0_wp, 1.2_wp * real(n, wp) * target_time / elapsed)) + 1
        end if
    end do
    t = elapsed / n

    end function time_it
!*****************************************************************************************

!*****************************************************************************************
!>
!  Run `n` iterations of the current case with an implementation,
!  returning a checksum of all the results.

    subroutine run(impl, n, cs)

    integer, intent(in) :: impl
    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    select case (impl)
    case (i_native); call run_native(n, cs)
    case (i_fparse); call run_fparse(n, cs)
    case (i_ffp);    call run_ffp(n, cs)
    case (i_feq);    call run_feq(n, cs)
    case (i_mcalc);  call run_mcalc(n, cs)
    end select

    end subroutine run
!*****************************************************************************************

!*****************************************************************************************
!>
!  Native Fortran.

    subroutine run_native(n, cs)

    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    integer :: i, k
    real(wp) :: x, y, z, d

    cs = 0.0_wp
    select case (icase)
    case (c_simple)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + (x + y*z)
        end do
    case (c_trig)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + (sin(x)*cos(y) + exp(-z))
        end do
    case (c_poly)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + (((((3*x + 2)*x - 5)*x + 1)*x - 7)*y + z)
        end do
    case (c_complex)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + (sqrt(x**2 + y**2 + z**2)*exp(-x*y/(1 + z**2)) + log(1 + abs(sin(x)*cos(y))) &
                       - tan(z/3)**2 + (x - y)/(x + y + 3))
        end do
    case (c_many)
        do i = 1, n
            call inputs(i, x, y, z)
            do k = 1, n_many
                cs = cs + (sin(x + k)*y + z**2/k - x*k)
            end do
        end do
    case (c_vector)
        do i = 1, n
            d = 1.0e-7_wp * mod(i, 1000)
            do k = 1, n_pts
                cs = cs + (sin(xs(k)+d)*cos(ys(k)+d) + exp(-(zs(k)+d)))
            end do
        end do
    case (c_grad)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + (sin(x)*cos(y) + exp(-z)) + cos(x)*cos(y) - sin(x)*sin(y) - exp(-z)
        end do
    end select

    end subroutine run_native
!*****************************************************************************************

!*****************************************************************************************
!>
!  fparse.

    subroutine run_fparse(n, cs)

    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    integer :: i, k
    real(wp) :: x, y, z, r, d, dx, dy, dz
    real(wp), dimension(:), allocatable :: v

    cs = 0.0_wp
    select case (icase)
    case (c_simple, c_trig, c_poly, c_complex)
        do i = 1, n
            call inputs(i, x, y, z)
            call fp%set_variables([x, y, z])
            call fp%evaluate()
            call fp%get(1, r)
            cs = cs + r
        end do
    case (c_many)
        do i = 1, n
            call inputs(i, x, y, z)
            call fp%set_variables([x, y, z])
            call fp%evaluate()
            do k = 1, n_many
                call fp%get(k, r)
                cs = cs + r
            end do
        end do
    case (c_vector)
        do i = 1, n
            d = 1.0e-7_wp * mod(i, 1000)
            call fp%set_variable(1, xs + d)
            call fp%set_variable(2, ys + d)
            call fp%set_variable(3, zs + d)
            call fp%evaluate()
            call fp%get(1, v)
            cs = cs + sum(v)
        end do
    case (c_parse)
        do i = 1, n
            call fp%parse(vars, [expr_complex])
        end do
    case (c_grad)
        do i = 1, n
            call inputs(i, x, y, z)
            call fp%set_variables([x, y, z])
            call fp%evaluate(wrt=vars)
            call fp%get(1, r)
            call fp%get_derivative(1, 'x', dx)
            call fp%get_derivative(1, 'y', dy)
            call fp%get_derivative(1, 'z', dz)
            cs = cs + r + dx + dy + dz
        end do
    end select
    if (fp%failed()) error stop 'fparse: '//fp%error_message()

    end subroutine run_fparse
!*****************************************************************************************

!*****************************************************************************************
!>
!  fortran_function_parser.

    subroutine run_ffp(n, cs)

    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    integer :: i, k
    real(wp) :: x, y, z, r, d
    real(wp), dimension(n_many) :: res

    cs = 0.0_wp
    select case (icase)
    case (c_simple, c_trig, c_poly, c_complex)
        do i = 1, n
            call inputs(i, x, y, z)
            call ffp1%evaluate([x, y, z], r)
            cs = cs + r
        end do
    case (c_many)
        do i = 1, n
            call inputs(i, x, y, z)
            call ffpa%evaluate([x, y, z], res)
            cs = cs + sum(res)
        end do
    case (c_vector)
        do i = 1, n
            d = 1.0e-7_wp * mod(i, 1000)
            do k = 1, n_pts
                call ffp1%evaluate([xs(k)+d, ys(k)+d, zs(k)+d], r)
                cs = cs + r
            end do
        end do
    case (c_parse)
        do i = 1, n
            call ffp1%parse(expr_complex, vars)
        end do
    end select

    end subroutine run_ffp
!*****************************************************************************************

!*****************************************************************************************
!>
!  feq-parse.

    subroutine run_feq(n, cs)

    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    integer :: i, k
    real(wp) :: x, y, z, d
    real(wp), dimension(n_pts, 3) :: xv

    cs = 0.0_wp
    select case (icase)
    case (c_simple, c_trig, c_poly, c_complex)
        do i = 1, n
            call inputs(i, x, y, z)
            cs = cs + feq1%evaluate([x, y, z])
        end do
    case (c_many)
        do i = 1, n
            call inputs(i, x, y, z)
            do k = 1, n_many
                cs = cs + feqa(k)%evaluate([x, y, z])
            end do
        end do
    case (c_vector)
        do i = 1, n
            ! uses the feq-parse array evaluation
            d = 1.0e-7_wp * mod(i, 1000)
            xv(:,1) = xs + d
            xv(:,2) = ys + d
            xv(:,3) = zs + d
            cs = cs + sum(feq1%evaluate(xv))
        end do
    case (c_parse)
        do i = 1, n
            feq1 = EquationParser('f = '//expr_complex, vars)
        end do
    end select

    end subroutine run_feq
!*****************************************************************************************

!*****************************************************************************************
!>
!  M_calculator (an interpreter: the string is processed on every evaluation).

    subroutine run_mcalc(n, cs)

    integer, intent(in) :: n
    real(wp), intent(out) :: cs

    integer :: i, k
    real(wp) :: x, y, z, d

    cs = 0.0_wp
    select case (icase)
    case (c_simple, c_trig, c_poly, c_complex)
        do i = 1, n
            call inputs(i, x, y, z)
            mc_x(1:3) = [x, y, z]
            cs = cs + dnum0(mc_expr)
        end do
    case (c_many)
        do i = 1, n
            call inputs(i, x, y, z)
            mc_x(1:3) = [x, y, z]
            do k = 1, n_many
                cs = cs + dnum0(trim(mc_many_exprs(k)))
            end do
        end do
    case (c_vector)
        do i = 1, n
            d = 1.0e-7_wp * mod(i, 1000)
            do k = 1, n_pts
                mc_x(1:3) = [xs(k)+d, ys(k)+d, zs(k)+d]
                cs = cs + dnum0(mc_expr)
            end do
        end do
    end select

    end subroutine run_mcalc
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate the markdown results table.

    function make_table(t, ok) result(table)

    real(wp), dimension(n_impl, n_cases), intent(in) :: t
    logical, dimension(n_impl, n_cases), intent(in) :: ok
    character(len=:), allocatable :: table

    character(len=*), parameter :: nl = new_line('a')
    integer :: impl, ic, iref, ifast, islow, nparsers
    character(len=8) :: date
    character(len=:), allocatable :: cell

    call date_and_time(date=date)

    table = 'Time per evaluation (lower is better). In parentheses: time relative to native Fortran '//&
            '(or relative to fparse for the parse benchmark). "'//sym_dash//'" means not supported, and '//&
            '"wrong result" means the result did not match native Fortran. '//&
            'Among the parsers (not native Fortran), '//sym_check//' marks the fastest correct result '//&
            'in each row and '//sym_cross//' the slowest.'//nl//nl//'| Case |'
    do impl = 1, n_impl
        table = table//' '//trim(impl_names(impl))//' |'
    end do
    table = table//nl//'|---|'//repeat('---:|', n_impl)//nl
    do ic = 1, n_cases
        iref = merge(i_fparse, i_native, ic == c_parse)
        ! fastest (correct) and slowest parser for this case:
        ifast = 0
        islow = 0
        nparsers = 0
        do impl = 1, n_impl
            if (impl == i_native .or. t(impl, ic) < 0.0_wp) cycle
            nparsers = nparsers + 1
            if (ok(impl, ic)) then
                if (ifast == 0) then
                    ifast = impl
                else if (t(impl, ic) < t(ifast, ic)) then
                    ifast = impl
                end if
            end if
            if (islow == 0) then
                islow = impl
            else if (t(impl, ic) > t(islow, ic)) then
                islow = impl
            end if
        end do
        if (nparsers < 2) islow = 0 ! nothing to compare with
        table = table//'| '//trim(case_names(ic))//' |'
        do impl = 1, n_impl
            if (t(impl, ic) < 0.0_wp) then
                cell = sym_dash
            else
                cell = format_time(t(impl, ic))
                if (impl /= iref) cell = cell//' ('//format_ratio(t(impl, ic)/t(iref, ic))//')'
                if (.not. ok(impl, ic)) cell = cell//' **wrong result**'
                if (impl == ifast) cell = cell//' '//sym_check
                if (impl == islow) cell = cell//' '//sym_cross
            end if
            table = table//' '//cell//' |'
        end do
        table = table//nl
    end do

    table = table//nl//'Expressions (variables `x`, `y`, `z`):'//nl//nl//&
        '* Simple: `'//expr_simple//'`'//nl//&
        '* Trig: `'//expr_trig//'`'//nl//&
        '* Polynomial: `'//expr_poly//'`'//nl//&
        '* Complex: `'//expr_complex//'`'//nl//&
        '* 100 equations: `sin(x + k)*y + z^2/k - x*k` for `k = 1, 2, ..., 100`'//nl//&
        '* Vector: the trig expression evaluated at 1000 points (fparse and feq-parse '//&
        'evaluate the whole array at once; the others loop over the points)'//nl//&
        '* Gradient: the trig expression and its derivatives with respect to `x`, `y` and `z`'//nl//nl//&
        'Generated on '//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' by `pixi run benchmark` ('//&
        '[test/benchmark.f90](test/benchmark.f90)) using '//compiler_version()//' with options `'//&
        optimization_options()//'`.'//nl//nl//&
        '[ffp]: https://github.com/jacobwilliams/fortran_function_parser'//nl//&
        '[feq]: https://github.com/FluidNumerics/feq-parse'//nl//&
        '[mcalc]: https://github.com/urbanjost/M_calculator'

    end function make_table
!*****************************************************************************************

!*****************************************************************************************
!>
!  The compiler options relevant to performance (excludes paths, etc.).

    function optimization_options() result(opts)

    character(len=:), allocatable :: opts

    character(len=:), allocatable :: all_opts, tok
    integer :: i, j

    all_opts = trim(compiler_options())//' '
    opts = ''
    i = 1
    do while (i <= len(all_opts))
        j = index(all_opts(i:), ' ') + i - 1
        tok = all_opts(i:j-1)
        i = j + 1
        if (len(tok) < 2) cycle
        if (tok(1:2) == '-O' .or. tok(1:2) == '-m' .or. tok(1:2) == '-f') then
            if (index(tok, 'macosx') > 0 .or. index(tok, 'max-errors') > 0 .or. &
                tok == '-fPIC' .or. tok == '-ffree-form' .or. tok == '-fimplicit-none') cycle
            if (len(opts) > 0) opts = opts//' '
            opts = opts//tok
        end if
    end do

    end function optimization_options
!*****************************************************************************************

!*****************************************************************************************
!>
!  Format a time with appropriate units.

    function format_time(t) result(s)
    real(wp), intent(in) :: t !! time [sec]
    character(len=:), allocatable :: s
    character(len=20) :: buf
    if (t < 1.0e-6_wp) then
        write(buf, '(F0.1,A)') t*1.0e9_wp, ' ns'
    else if (t < 1.0e-3_wp) then
        write(buf, '(F0.2,A)') t*1.0e6_wp, ' '//sym_micro//'s'
    else
        write(buf, '(F0.2,A)') t*1.0e3_wp, ' ms'
    end if
    s = trim(adjustl(buf))
    if (s(1:1) == '.') s = '0'//s
    end function format_time
!*****************************************************************************************

!*****************************************************************************************
!>
!  Format a time ratio.

    function format_ratio(r) result(s)
    real(wp), intent(in) :: r
    character(len=:), allocatable :: s
    character(len=20) :: buf
    if (r < 10.0_wp) then
        write(buf, '(F0.1,A)') r, 'x'
    else
        write(buf, '(I0,A)') nint(r), 'x'
    end if
    s = trim(adjustl(buf))
    if (s(1:1) == '.') s = '0'//s
    end function format_ratio
!*****************************************************************************************

!*****************************************************************************************
!>
!  Integer to string.

    pure function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=20) :: buf
    write(buf, '(I0)') i
    s = trim(buf)
    end function itoa
!*****************************************************************************************

!*****************************************************************************************
!>
!  Replace the text between the benchmark markers in README.md with the table.

    subroutine write_readme(table)

    character(len=*), intent(in) :: table

    character(len=*), parameter :: filename = 'README.md'
    character(len=*), parameter :: start_marker = '<!-- BENCHMARK_START -->'
    character(len=*), parameter :: end_marker = '<!-- BENCHMARK_END -->'
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: text, line
    integer :: iunit, istat, i1, i2

    ! read the file:
    open(newunit=iunit, file=filename, status='old', action='read', iostat=istat)
    if (istat /= 0) error stop 'error opening '//filename
    text = ''
    do
        call read_line(iunit, line, istat)
        if (istat /= 0) exit
        text = text//line//nl
    end do
    close(iunit)

    i1 = index(text, start_marker)
    i2 = index(text, end_marker)
    if (i1 == 0 .or. i2 < i1) error stop 'benchmark markers not found in '//filename

    text = text(1:i1+len(start_marker)-1)//nl//table//nl//text(i2:)

    open(newunit=iunit, file=filename, status='replace', action='write', iostat=istat)
    if (istat /= 0) error stop 'error opening '//filename//' for writing'
    write(iunit, '(A)', advance='no') text
    close(iunit)
    write(output_unit, '(/A)') filename//' updated.'

    end subroutine write_readme
!*****************************************************************************************

!*****************************************************************************************
!>
!  Read a line of any length from a file.

    subroutine read_line(iunit, line, istat)
    integer, intent(in) :: iunit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: istat
    character(len=256) :: buf
    integer :: isize
    line = ''
    do
        read(iunit, '(A)', advance='no', size=isize, iostat=istat) buf
        line = line//buf(1:isize)
        if (istat /= 0) exit
    end do
    if (is_iostat_eor(istat)) istat = 0
    end subroutine read_line
!*****************************************************************************************

    end module benchmark_module
!*****************************************************************************************

!*****************************************************************************************
!>
!  Benchmark driver.

    program benchmark

    use benchmark_module, only: run_benchmarks

    implicit none

    call run_benchmarks()

    end program benchmark
!*****************************************************************************************
