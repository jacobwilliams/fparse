!*****************************************************************************************
!>
!  Tests for the Fortran code generator ([[fparser:generate_fortran]]).
!
!  The generated code is compiled and run, and its results (values and jacobians)
!  are compared with those of fparse. This needs a Fortran compiler: the `FPM_FC`
!  environment variable, or `gfortran`. If no compiler is available, the test that
!  compiles the code is skipped.

    module test_codegen

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
    use test_utils

    implicit none

    private

    public :: collect_codegen

    type :: gen_case
        !! a system of expressions, with the values of its inputs
        type(fparser) :: p !! the parser
        type(fparse_value), dimension(:), allocatable :: vals !! the values of the inputs
        character(len=10), dimension(:), allocatable :: wrt !! the derivative variables
        character(len=:), allocatable :: module_name !! the name of the generated module
    end type gen_case

    character(len=*), parameter :: nl = new_line('a')

    contains

    subroutine collect_codegen(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("compile_and_run", test_compile_and_run), &
        new_unittest("generated_text", test_generated_text), &
        new_unittest("errors", test_codegen_errors) ]
    end subroutine collect_codegen

    subroutine add_case(cases, n, vars, exprs, vals, wrt)
    !! add a system of expressions to the list of cases
    type(gen_case), dimension(:), intent(inout) :: cases !! the cases
    integer, intent(inout) :: n !! the number of cases
    character(len=*), dimension(:), intent(in) :: vars !! the input variables
    character(len=*), dimension(:), intent(in) :: exprs !! the expressions
    type(fparse_value), dimension(:), intent(in) :: vals !! the values of the inputs
    character(len=*), dimension(:), intent(in) :: wrt !! the derivative variables
    n = n + 1
    associate (c => cases(n))
        call c%p%parse(vars, exprs)
        c%vals = vals
        allocate(c%wrt(size(wrt)))
        c%wrt = wrt
        c%module_name = 'gen'//itoa(n)
    end associate
    end subroutine add_case

    subroutine test_compile_and_run(error)
    !! generate the code for several systems, compile and run it, and compare the results
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(gen_case), dimension(10) :: cases
    character(len=:), allocatable :: fc, code, src, driver, errmsg
    real(wp), dimension(:), allocatable :: expected, got
    integer :: k, status, ncases

    ncases = 0
    fc = compiler()
    if (.not. compiler_works(fc)) then
        call skip_test(error, 'no Fortran compiler ("'//fc//'") is available')
        return
    end if

    ! scalars, vectors, outputs used in other outputs, conditionals, powers:
    call add_case(cases, ncases, ['x', 'v'], [character(len=60) :: 'a = sin(x)*v + x^2', &
        'b = norm2(a) + if(x, -x, x > 0)', 'c = a[2]^x + max(x, 1, v[1])'], &
        [fparse_value([0.7_wp]), fparse_value([1.0_wp, 2.0_wp, 3.0_wp])], ['x', 'v'])
    ! all the one-argument functions:
    call add_case(cases, ncases, ['x'], [character(len=200) :: &
        'abs(sin(x)) + sqrt(x) + exp(x) + log(x) + log10(x) + tan(x) + asin(x) + acos(x) + atan(x)', &
        'sinh(x) + cosh(x) + tanh(x) + asinh(x) + acosh(x + 1.5) + atanh(x) + erf(x) + erfc(x)', &
        'floor(x*10) + ceiling(x*10) + ceil(-x*10) + nint(x*10) + aint(x*10) + trunc(-x*10) + round(x)', &
        'sind(x*100) + cosd(x*100) + tand(x*100) + asind(x) + acosd(x) + atand(x)', &
        '-x + --x + ln(x)'], [fparse_value([0.3_wp])], ['x'])
    ! two-argument functions, powers, relational and logical operators:
    call add_case(cases, ncases, ['x', 'y'], [character(len=200) :: &
        'atan2(y, x) + atan(y, x) + atan2d(y, x) + atand(-y, x) + hypot(x, y)', &
        'mod(x*7, y) + modulo(-x*7, y) + sign(x, -y) + sign(-x, y)', &
        'x^y + x^2 + x^-1 + (-y)^3 + y^x + x^0.5 + x**y**0.5 + 2^x + x^(y - y)', &
        '(x < y) + (x <= y) + (x > y) + (x >= y) + (x == x) + (x /= y) + (x != y)', &
        '(x && y) + (x || 0) + !x + !0 + (x .and. 0) + (.not. y)', &
        'if(1/(x - x), 2*x, 0) + if(x > y, x, y)*if(x, if(y, x*y, 1/(x - x)), 1)', &
        'x*y/(x - y)*(x + y)/2 - x/y/2 - (x - y) - -y'], &
        [fparse_value([0.6_wp]), fparse_value([1.7_wp])], ['x', 'y'])
    ! vectors:
    call add_case(cases, ncases, ['u', 'v', 's'], [character(len=200) :: &
        'n1 = sum(u) + product(v) + norm2(u) + maxval(v) + minval(u) + size(u)', &
        'n2 = dot(u, v) + dot_product(v, v)', &
        'c = cross(u, v)', &
        'w = [u, s, c[3], 2]', &
        'k = w[2] + w[nint(s*2)] + c[1]', &
        'm = max(u, s, v/2) + min(u, 0.5)', &
        'e = if(u, -u*s, u > 0) + if(v, u, s > 1)', &
        'r = u*s + v/s - u^2 + sin(v)*cos(u)', &
        'q = sum(r*u) + norm2(c) + sum(w)'], &
        [fparse_value([0.3_wp, -0.8_wp, 1.2_wp]), fparse_value([1.1_wp, 0.4_wp, -0.9_wp]), fparse_value([1.4_wp])], &
        ['u', 's', 'v'])
    ! names that clash with Fortran names, anonymous outputs, and only some derivative variables:
    call add_case(cases, ncases, ['sin', 'i  ', 'wp ', 'max'], [character(len=60) :: &
        'sin(sin) + i*wp', 'j = max(max, i) + sum(sin)', 'd_out1 = j*2', 'j + d_out1'], &
        [fparse_value([0.5_wp]), fparse_value([2.0_wp]), fparse_value([3.0_wp]), fparse_value([-1.0_wp])], &
        ['wp ', 'sin'])
    ! a long expression (continuation lines and temporaries), and no derivatives:
    block
        character(len=:), allocatable :: e
        e = 'x'
        do k = 1, 300
            e = e//' + sin('//itoa(k)//'*x)*'//itoa(k)
        end do
        call add_case(cases, ncases, ['x'], [e, 'x + 1' // repeat(' ', len(e) - 5)], [fparse_value([0.25_wp])], &
                      [character(len=10) :: ])
    end block

    ! generate the code and the driver, and the expected results:
    src = ''
    driver = 'program driver'//nl
    do k = 1, ncases
        driver = driver//'use '//cases(k)%module_name//nl
    end do
    driver = driver//'implicit none'//nl//'integer :: iu'//nl//&
             'open(newunit=iu, file="build/codegen_test.txt", status="replace")'//nl
    allocate(expected(0))
    do k = 1, ncases
        call set_inputs(cases(k))
        if (size(cases(k)%wrt) > 0) then
            call cases(k)%p%generate_fortran(code, cases(k)%module_name, wrt=cases(k)%wrt, &
                                             status=status, errmsg=errmsg)
        else
            call cases(k)%p%generate_fortran(code, cases(k)%module_name, status=status, errmsg=errmsg)
        end if
        if (status /= 0) then
            call test_failed(error, 'generate_fortran failed for case '//itoa(k), errmsg)
            return
        end if
        src = src//code//nl
        driver = driver//'call case'//itoa(k)//'()'//nl
        call add_expected(cases(k), expected, error)
        if (allocated(error)) return
    end do
    driver = driver//'close(iu)'//nl//'contains'//nl
    do k = 1, ncases
        driver = driver//driver_routine(cases(k), k)
    end do
    driver = driver//'end program driver'//nl

    call write_file('build/codegen_test.f90', src//driver)
    call execute_command_line(fc//' -std=f2023 -Wall -Werror -fimplicit-none -o build/codegen_test '//&
                              'build/codegen_test.f90 > build/codegen_test.log 2>&1', exitstat=status)
    if (status /= 0) then
        call test_failed(error, 'the generated code does not compile (see build/codegen_test.f90 and '//&
                         'build/codegen_test.log)')
        return
    end if
    call execute_command_line('./build/codegen_test', exitstat=status)
    if (status /= 0) then
        call test_failed(error, 'the generated code failed to run')
        return
    end if
    call read_numbers('build/codegen_test.txt', got)
    call check(error, size(got) == size(expected), 'wrong number of results: '//itoa(size(got))//&
               ' (expected '//itoa(size(expected))//')')
    if (allocated(error)) return
    do k = 1, size(got)
        if (.not. close_to(got(k), expected(k), 1.0e-12_wp)) then
            call test_failed(error, 'result '//itoa(k)//' is different: '//rtoa(got(k))//' (expected '//&
                             rtoa(expected(k))//')')
            return
        end if
    end do

    contains

        subroutine set_inputs(c)
        !! set the input values of a case
        type(gen_case), intent(inout) :: c !! the case
        integer :: j
        do j = 1, size(c%vals)
            call c%p%set_variable(j, c%vals(j)%v)
        end do
        end subroutine set_inputs

    end subroutine test_compile_and_run

    subroutine add_expected(c, expected, error)
    !! the results of a case computed by fparse, in the order the driver writes them:
    !! the outputs (from `_eval`), then if there are derivative variables, the outputs
    !! and their jacobians (from `_jac`)
    type(gen_case), intent(inout) :: c !! the case
    real(wp), dimension(:), allocatable, intent(inout) :: expected !! the expected results
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if fparse fails
    real(wp), dimension(:), allocatable :: v
    real(wp), dimension(:,:), allocatable :: jac
    integer :: k, j, status
    character(len=:), allocatable :: errmsg
    call c%p%evaluate(status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'fparse evaluation failed', errmsg)
        return
    end if
    do k = 1, c%p%num_outputs()
        call c%p%get(k, v)
        expected = [expected, v]
    end do
    if (size(c%wrt) == 0) return
    call c%p%evaluate(wrt=c%wrt, status=status, errmsg=errmsg)
    if (status /= 0) then
        call test_failed(error, 'fparse evaluation (with derivatives) failed', errmsg)
        return
    end if
    do k = 1, c%p%num_outputs()
        call c%p%get(k, v)
        expected = [expected, v]
    end do
    do k = 1, c%p%num_outputs()
        ! the jacobian, with the columns of all the derivative variables (column major):
        block
            real(wp), dimension(:,:), allocatable :: full
            call c%p%get(k, v)
            allocate(full(size(v), 0))
            do j = 1, size(c%wrt)
                call c%p%get_derivative(k, trim(c%wrt(j)), jac)
                full = reshape([full, jac], [size(v), size(full, 2) + size(jac, 2)])
            end do
            expected = [expected, reshape(full, [size(full)])]
        end block
    end do
    end subroutine add_expected

    function driver_routine(c, k) result(s)
    !! the driver routine for a case: calls the generated routines and writes the results
    type(gen_case), intent(inout) :: c !! the case
    integer, intent(in) :: k !! the case number
    character(len=:), allocatable :: s !! the code
    character(len=:), allocatable :: args, jargs
    real(wp), dimension(:), allocatable :: v
    integer :: j, n, nd
    s = 'subroutine case'//itoa(k)//'()'//nl//'use iso_fortran_env, only: wp => real64'//nl
    args = ''
    do j = 1, size(c%vals)
        s = s//decl('in'//itoa(j), size(c%vals(j)%v), 0)//' = '//literal(c%vals(j)%v)//nl
        args = args//'in'//itoa(j)//', '
    end do
    nd = 0
    do j = 1, size(c%wrt)
        nd = nd + size(c%vals(index_of(c, c%wrt(j)))%v)
    end do
    jargs = ''
    do j = 1, c%p%num_outputs()
        call c%p%get(j, v)
        n = size(v)
        s = s//decl('out'//itoa(j), n, 0)//nl
        if (nd > 0) then
            if (n == 1) then
                s = s//'real(wp), dimension('//itoa(nd)//') :: jac'//itoa(j)//nl
            else
                s = s//decl('jac'//itoa(j), n, nd)//nl
            end if
            jargs = jargs//', jac'//itoa(j)
        end if
        args = args//'out'//itoa(j)
        if (j < c%p%num_outputs()) args = args//', '
    end do
    s = s//'call '//c%module_name//'_eval('//args//')'//nl
    do j = 1, c%p%num_outputs()
        s = s//'write(iu, "(ES26.17E3)") out'//itoa(j)//nl
    end do
    if (nd > 0) then
        s = s//'call '//c%module_name//'_jac('//args//jargs//')'//nl
        do j = 1, c%p%num_outputs()
            s = s//'write(iu, "(ES26.17E3)") out'//itoa(j)//nl
        end do
        do j = 1, c%p%num_outputs()
            s = s//'write(iu, "(ES26.17E3)") jac'//itoa(j)//nl
        end do
    end if
    s = s//'end subroutine case'//itoa(k)//nl
    end function driver_routine

    integer function index_of(c, name)
    !! the index of an input variable
    type(gen_case), intent(in) :: c !! the case
    character(len=*), intent(in) :: name !! the variable name
    integer :: j
    index_of = 0
    do j = 1, c%p%num_inputs()
        if (c%p%input_name(j) == trim(name)) index_of = j
    end do
    end function index_of

    function decl(name, n, m) result(s)
    !! the declaration of a real variable (scalar, vector or matrix)
    character(len=*), intent(in) :: name !! the variable name
    integer, intent(in) :: n !! the first dimension
    integer, intent(in) :: m !! the second dimension (0 for a scalar or vector)
    character(len=:), allocatable :: s !! the declaration
    if (m > 0) then
        s = 'real(wp), dimension('//itoa(n)//', '//itoa(m)//') :: '//name
    else if (n > 1) then
        s = 'real(wp), dimension('//itoa(n)//') :: '//name
    else
        s = 'real(wp) :: '//name
    end if
    end function decl

    function literal(v) result(s)
    !! a real literal (or an array constructor) with the exact value
    real(wp), dimension(:), intent(in) :: v !! the value
    character(len=:), allocatable :: s !! the literal
    integer :: j
    if (size(v) == 1) then
        s = rtoa(v(1))
    else
        s = '['
        do j = 1, size(v)
            if (j > 1) s = s//', '
            s = s//rtoa(v(j))
        end do
        s = s//']'
    end if
    end function literal

    function rtoa(x) result(s)
    !! a real literal with the exact value
    real(wp), intent(in) :: x !! the value
    character(len=:), allocatable :: s !! the literal
    character(len=40) :: buf
    write(buf, '(ES26.17E3)') x
    s = trim(adjustl(buf))
    s = s(1:index(s, 'E')-1)//'e'//s(index(s, 'E')+1:)//'_wp'
    end function rtoa

    pure function itoa(i) result(s)
    !! integer to string
    integer, intent(in) :: i !! the integer
    character(len=:), allocatable :: s !! the integer as a string
    character(len=20) :: buf
    write(buf, '(I0)') i
    s = trim(buf)
    end function itoa

    subroutine write_file(filename, text)
    !! write a string to a file
    character(len=*), intent(in) :: filename !! the file name
    character(len=*), intent(in) :: text !! the contents of the file
    integer :: iunit
    open(newunit=iunit, file=filename, status='replace', access='stream', form='formatted')
    write(iunit, '(A)', advance='no') text
    close(iunit)
    end subroutine write_file

    subroutine read_numbers(filename, x)
    !! read the numbers in a file (one per line)
    character(len=*), intent(in) :: filename !! the file name
    real(wp), dimension(:), allocatable, intent(out) :: x !! the numbers
    integer :: iunit, istat
    real(wp) :: r
    allocate(x(0))
    open(newunit=iunit, file=filename, status='old', action='read', iostat=istat)
    if (istat /= 0) return
    do
        read(iunit, *, iostat=istat) r
        if (istat /= 0) exit
        x = [x, r]
    end do
    close(iunit)
    end subroutine read_numbers

    function compiler() result(fc)
    !! the Fortran compiler to use: `FPM_FC`, or `gfortran`
    character(len=:), allocatable :: fc !! the compiler command
    integer :: n, status
    call get_environment_variable('FPM_FC', length=n, status=status)
    if (status == 0 .and. n > 0) then
        allocate(character(len=n) :: fc)
        call get_environment_variable('FPM_FC', fc)
    else
        fc = 'gfortran'
    end if
    end function compiler

    logical function compiler_works(fc)
    !! if the compiler can be run
    character(len=*), intent(in) :: fc !! the compiler command
    integer :: exitstat, cmdstat
    call execute_command_line(fc//' --version > build/codegen_test.log 2>&1', exitstat=exitstat, cmdstat=cmdstat)
    compiler_works = cmdstat == 0 .and. exitstat == 0
    end function compiler_works

    subroutine test_generated_text(error)
    !! check some details of the generated code
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: code
    call p%parse(['x', 'y'], [character(len=40) :: 'f = x^2 + y', 'if(f, 1/0, x > y)'])
    call p%set_variables([1.0_wp, 2.0_wp])
    call p%generate_fortran(code, 'my_model', wrt=['y'])
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check(error, index(code, 'module my_model') > 0, 'module'); if (allocated(error)) return
    call check(error, index(code, 'pure subroutine my_model_eval(x, y, f, out2)') > 0, 'eval routine')
    if (allocated(error)) return
    call check(error, index(code, 'pure subroutine my_model_jac(x, y, f, out2, d_f, d_out2)') > 0, 'jac routine')
    if (allocated(error)) return
    call check(error, index(code, 'f = x**2 + y') > 0, 'expression'); if (allocated(error)) return
    call check(error, index(code, 'if (x > y) then') > 0, 'conditional'); if (allocated(error)) return
    call check(error, index(code, 'y: column 1') > 0, 'columns'); if (allocated(error)) return
    ! no jacobian routine without wrt:
    call p%generate_fortran(code)
    call check(error, index(code, 'module fparse_generated') > 0 .and. index(code, '_jac') == 0, 'no jac')
    end subroutine test_generated_text

    subroutine sq(args, res, stat)
    !! a user function
    type(fparse_value), dimension(:), intent(in) :: args !! the function arguments
    real(wp), dimension(:), allocatable, intent(out) :: res !! the function result
    integer, intent(inout) :: stat !! status code (nonzero for an error)
    res = args(1)%v**2
    end subroutine sq

    subroutine test_codegen_errors(error)
    !! errors in the code generation
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: code, errmsg
    integer :: status
    call p%generate_fortran(code, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not been parsed') > 0, 'not parsed'); if (allocated(error)) return
    call p%parse(['x', 'y'], ['x + y'])
    call p%set_variable('x', 1.0_wp)
    call p%generate_fortran(code, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, '"y" has not been set') > 0, errmsg); if (allocated(error)) return
    call p%set_variable('y', 1.0_wp)
    call p%generate_fortran(code, wrt=['z'], status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not an input variable') > 0, errmsg); if (allocated(error)) return
    call p%generate_fortran(code, module_name='1bad', status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'invalid module name') > 0, errmsg); if (allocated(error)) return
    call p%add_function('sq', sq, is_pure=.true., nargs=1)
    call p%parse(['x'], ['sq(x)'])
    call p%set_variable('x', 1.0_wp)
    call p%generate_fortran(code, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'user-defined functions are not supported') > 0, errmsg)
    if (allocated(error)) return
    call p%parse(['x', 'v'], ['if(x, v, 1)'])
    call p%set_variable('x', 1.0_wp)
    call p%set_variable('v', [1.0_wp, 2.0_wp])
    call p%generate_fortran(code, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'must have the same size') > 0, errmsg)
    end subroutine test_codegen_errors

    end module test_codegen
!*****************************************************************************************
