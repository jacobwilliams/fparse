!*****************************************************************************************
!>
!  Benchmark of the sparsity option for derivatives (see [[fparser:set_sparsity]]):
!  computes the jacobians of large systems of equations with sparsity enabled and
!  disabled, checks that the results are the same, and compares the speed.
!
!  Usage:
!```
!  fpm test benchmark_sparsity --profile release
!```

    program benchmark_sparsity

    use fparse
    use iso_fortran_env, only: int64, output_unit

    implicit none

    integer, parameter :: n_scalar = 1000 !! number of variables in the scalar problem
    integer, parameter :: n_vec = 50 !! number of vector variables in the vector problem
    integer, parameter :: vec_size = 20 !! size of each vector variable

    logical :: ok

    ok = .true.
    write(output_unit, '(A)') '| Problem | Equations | Derivative columns | Sparsity off | Sparsity on | Speedup | Max difference |'
    write(output_unit, '(A)') '|---|---:|---:|---:|---:|---:|---:|'
    call run_problem('Scalar', scalar_problem_setup)
    call run_problem('Vector', vector_problem_setup)
    if (.not. ok) error stop 'the jacobians with sparsity on and off are different'

    contains

    subroutine scalar_problem_setup(vars, exprs, names, vals)
        !! `f_i = sin(x_i)*x_(i+1) + exp(-x_(i+2))`, `g_i = f_i*f_(i+1) - x_i` (indices wrap around)
        character(len=:), dimension(:), allocatable, intent(out) :: vars !! the input variable names
        character(len=:), dimension(:), allocatable, intent(out) :: exprs !! the expressions
        character(len=:), dimension(:), allocatable, intent(out) :: names !! the output names
        type(fparse_value), dimension(:), allocatable, intent(out) :: vals !! the variable values
        integer :: i, n
        n = n_scalar
        allocate(character(len=10) :: vars(n), names(2*n))
        allocate(character(len=80) :: exprs(2*n))
        allocate(vals(n))
        do i = 1, n
            vars(i) = 'x'//itoa(i)
            vals(i)%v = [0.5_wp + 0.001_wp * i]
        end do
        do i = 1, n
            names(i) = 'f'//itoa(i)
            exprs(i) = 'sin('//trim(vars(i))//')*'//trim(vars(wrap(i+1, n)))//' + exp(-'//trim(vars(wrap(i+2, n)))//')'
            names(n+i) = 'g'//itoa(i)
            exprs(n+i) = trim(names(i))//'*f'//itoa(wrap(i+1, n))//' - '//trim(vars(i))
        end do
    end subroutine scalar_problem_setup

    subroutine vector_problem_setup(vars, exprs, names, vals)
        !! `w_i = sin(v_i)*v_(i+1) + sum(v_(i+2))`, `s_i = norm2(w_i) * w_i` (vectors; indices wrap around)
        character(len=:), dimension(:), allocatable, intent(out) :: vars !! the input variable names
        character(len=:), dimension(:), allocatable, intent(out) :: exprs !! the expressions
        character(len=:), dimension(:), allocatable, intent(out) :: names !! the output names
        type(fparse_value), dimension(:), allocatable, intent(out) :: vals !! the variable values
        integer :: i, j, n
        n = n_vec
        allocate(character(len=10) :: vars(n), names(2*n))
        allocate(character(len=80) :: exprs(2*n))
        allocate(vals(n))
        do i = 1, n
            vars(i) = 'v'//itoa(i)
            vals(i)%v = [(0.1_wp * i + 0.01_wp * j, j = 1, vec_size)]
        end do
        do i = 1, n
            names(i) = 'w'//itoa(i)
            exprs(i) = 'sin('//trim(vars(i))//')*'//trim(vars(wrap(i+1, n)))//' + sum('//trim(vars(wrap(i+2, n)))//')'
            names(n+i) = 's'//itoa(i)
            exprs(n+i) = 'norm2('//trim(names(i))//') * '//trim(names(i))
        end do
    end subroutine vector_problem_setup

    subroutine run_problem(label, setup)

        character(len=*), intent(in) :: label !! the problem name (for the table)
        interface
            subroutine setup(vars, exprs, names, vals)
                import :: fparse_value
                character(len=:), dimension(:), allocatable, intent(out) :: vars !! the input variable names
                character(len=:), dimension(:), allocatable, intent(out) :: exprs !! the expressions
                character(len=:), dimension(:), allocatable, intent(out) :: names !! the output names
                type(fparse_value), dimension(:), allocatable, intent(out) :: vals !! the variable values
            end subroutine setup
        end interface

        type(fparser) :: p, p2
        character(len=:), dimension(:), allocatable :: vars, exprs, names
        type(fparse_value), dimension(:), allocatable :: vals
        real(wp), dimension(:,:), allocatable :: j_dense, j_sparse
        real(wp) :: t_dense, t_sparse, maxdiff
        integer :: i, k, status, ncols
        character(len=:), allocatable :: errmsg
        character(len=200) :: line

        call setup(vars, exprs, names, vals)
        call p%parse(vars, exprs, names=names, status=status, errmsg=errmsg)
        if (status /= 0) error stop errmsg
        call p2%parse(vars, exprs, names=names)
        ! the inputs are the same for every evaluation, so turn off caching
        ! (otherwise only the first evaluation would compute anything):
        call p%set_caching(.false.)
        call p2%set_caching(.false.)
        do i = 1, size(vals)
            call p%set_variable(i, vals(i)%v)
            call p2%set_variable(i, vals(i)%v)
        end do
        ncols = sum([(size(vals(i)%v), i = 1, size(vals))])

        call p%set_sparsity(.false.)
        t_dense = time_evaluate(p, vars)
        call p%set_sparsity(.true.)
        t_sparse = time_evaluate(p, vars)

        ! compare the jacobians of all the outputs, with respect to the variables
        ! near them (where the derivatives are nonzero) and a few others:
        call p%set_sparsity(.false.)
        call p%evaluate(wrt=vars)
        call p2%set_sparsity(.true.)
        call p2%evaluate(wrt=vars)
        maxdiff = 0.0_wp
        do i = 1, size(exprs)
            do k = 1, size(vars)
                if (.not. (abs(k - mod(i-1, size(vars)) - 1) <= 3 .or. k == 1 .or. k == size(vars)/2)) cycle
                call p%get_derivative(i, trim(vars(k)), j_dense)
                call p2%get_derivative(i, trim(vars(k)), j_sparse)
                if (any(shape(j_dense) /= shape(j_sparse))) then
                    ok = .false.
                    maxdiff = huge(1.0_wp)
                else
                    maxdiff = max(maxdiff, maxval(abs(j_dense - j_sparse)))
                end if
            end do
        end do
        if (maxdiff > 1.0e-13_wp) ok = .false.

        write(line, '(A,I0,A,I0,A,A,A,A,A,F0.1,A,ES9.2,A)') '| '//label//' | ', size(exprs), ' | ', ncols, &
            ' | ', format_time(t_dense), ' | ', format_time(t_sparse), ' | ', t_dense / t_sparse, 'x | ', maxdiff, ' |'
        write(output_unit, '(A)') trim(line)

    end subroutine run_problem

    real(wp) function time_evaluate(p, vars) result(t)
        !! time per `evaluate(wrt=vars)` call [sec]
        type(fparser), intent(inout) :: p !! the parser
        character(len=*), dimension(:), intent(in) :: vars !! the variables to differentiate with respect to
        integer(int64) :: c1, c2, rate
        integer :: n, j, status
        character(len=:), allocatable :: errmsg
        real(wp) :: elapsed
        call p%evaluate(wrt=vars) ! warm up (and set up the derivatives)
        n = 1
        do
            call system_clock(c1, rate)
            do j = 1, n
                call p%evaluate(wrt=vars, status=status, errmsg=errmsg)
                if (status /= 0) error stop errmsg
            end do
            call system_clock(c2)
            elapsed = real(c2 - c1, wp) / real(rate, wp)
            if (elapsed > 0.2_wp) exit
            n = n * 2
        end do
        t = elapsed / n
    end function time_evaluate

    pure integer function wrap(i, n)
        integer, intent(in) :: i !! the index
        integer, intent(in) :: n !! the number of elements
        wrap = mod(i - 1, n) + 1
    end function wrap

    pure function itoa(i) result(s)
        integer, intent(in) :: i !! the integer
        character(len=:), allocatable :: s !! the integer as a string
        character(len=20) :: buf
        write(buf, '(I0)') i
        s = trim(buf)
    end function itoa

    function format_time(t) result(s)
        real(wp), intent(in) :: t !! time [sec]
        character(len=:), allocatable :: s !! the formatted time
        character(len=20) :: buf
        if (t < 1.0e-3_wp) then
            write(buf, '(F0.1,A)') t*1.0e6_wp, ' us'
        else
            write(buf, '(F0.2,A)') t*1.0e3_wp, ' ms'
        end if
        s = trim(adjustl(buf))
    end function format_time

    end program benchmark_sparsity
!*****************************************************************************************
