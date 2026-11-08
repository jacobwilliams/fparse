!*****************************************************************************************
!>
!  Example of the code generation feature of fparse.
!
!  A small projectile model is parsed and evaluated with fparse, then Fortran code
!  for it (the values, and the jacobians with respect to the launch speed and angle)
!  is generated with `generate_fortran`. The generated module and a small driver
!  program are written to a directory, compiled, and run, and their results are
!  printed next to those of fparse.
!
!  Usage (the files are written to the directory given as an argument, `build` by default):
!```
!  fpm run --example codegen
!  fpm run --example codegen -- my_directory
!```
!  The compiler is the `FPM_FC` environment variable, or `gfortran`. If it is not
!  available, the files are still written, and the command to compile them is printed.

    program codegen

    use fparse

    implicit none

    character(len=*), dimension(*), parameter :: inputs = [character(len=5) :: 'v0', 'angle', 'g', 't']
    character(len=*), dimension(*), parameter :: names = [character(len=7) :: &
        'vx', 'vy', 'tflight', 'range', 'hmax', 'x', 'y']
    character(len=*), dimension(*), parameter :: equations = [character(len=40) :: &
        'v0*cosd(angle)', &         ! horizontal velocity
        'v0*sind(angle)', &         ! vertical velocity
        '2*vy/g', &                 ! time of flight
        'vx*tflight', &             ! range
        'vy^2/(2*g)', &             ! maximum height
        'vx*t', &                   ! position at the times t (vectors)
        'max(vy*t - g*t^2/2, 0)']

    type(fparser) :: p
    character(len=:), allocatable :: code, errmsg, dir, fc, cmd
    real(wp), dimension(:), allocatable :: v
    real(wp), dimension(:,:), allocatable :: jac
    integer :: i, status, length

    ! the output directory:
    call get_command_argument(1, length=length)
    if (length > 0) then
        allocate(character(len=length) :: dir)
        call get_command_argument(1, dir)
    else
        dir = 'build'
    end if

    ! parse and evaluate the model with fparse:
    call p%parse(inputs, equations, names=names, status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    call p%set_variable('v0', 50.0_wp)                       ! [m/s]
    call p%set_variable('angle', 35.0_wp)                    ! [deg]
    call p%set_variable('g', 9.81_wp)                        ! [m/s^2]
    call p%set_variable('t', [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]) ! [s]
    call p%evaluate(wrt=['v0   ', 'angle'], status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg

    write(*, '(A)') 'Results from fparse:'
    do i = 1, size(names)
        call p%get(trim(names(i)), v)
        write(*, '(2X,A8,A,*(F11.4))') names(i)//' ', ' =', v
    end do
    call p%get_derivative('range', 'v0', jac)
    write(*, '(2X,A,F11.4)') 'd(range)/d(v0)    =', jac(1,1)
    call p%get_derivative('range', 'angle', jac)
    write(*, '(2X,A,F11.4)') 'd(range)/d(angle) =', jac(1,1)

    ! generate the code (it is specialized for the sizes of the inputs: t has 5 elements):
    call p%generate_fortran(code, module_name='projectile', wrt=['v0   ', 'angle'], status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    call write_file(dir//'/projectile.f90', code)
    call write_file(dir//'/projectile_driver.f90', driver())

    ! compile and run it:
    fc = compiler()
    cmd = fc//' -J '//dir//' -o '//dir//'/projectile '//dir//'/projectile.f90 '//dir//'/projectile_driver.f90'
    write(*, '(/A)') 'Compiling: '//cmd
    call execute_command_line(cmd, exitstat=status)
    if (status /= 0) then
        write(*, '(A)') 'The code could not be compiled here. To compile it yourself:'
        write(*, '(2X,A)') cmd
        stop
    end if
    write(*, '(/A)') 'Results from the generated code:'
    call execute_command_line(dir//'/projectile', exitstat=status)
    if (status /= 0) error stop 'error running the generated code'

    contains

    function driver() result(s)
        !! a program that calls the generated routines and prints the results
        character(len=:), allocatable :: s !! the program
        character(len=*), parameter :: nl = new_line('a')
        s = 'program projectile_driver'//nl//&
            'use projectile, only: projectile_eval, projectile_jac'//nl//&
            'use, intrinsic :: iso_fortran_env, only: wp => real64'//nl//&
            'implicit none'//nl//&
            'real(wp) :: vx, vy, tflight, range, hmax'//nl//&
            'real(wp), dimension(5) :: x, y'//nl//&
            'real(wp), dimension(2) :: d_vx, d_vy, d_tflight, d_range, d_hmax'//nl//&
            'real(wp), dimension(5, 2) :: d_x, d_y'//nl//&
            'real(wp), parameter :: t(5) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]'//nl//&
            nl//&
            'call projectile_eval(50.0_wp, 35.0_wp, 9.81_wp, t, vx, vy, tflight, range, hmax, x, y)'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''vx      '', '' ='', vx'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''vy      '', '' ='', vy'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''tflight '', '' ='', tflight'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''range   '', '' ='', range'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''hmax    '', '' ='', hmax'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''x       '', '' ='', x'//nl//&
            'write(*, ''(2X,A8,A,*(F11.4))'') ''y       '', '' ='', y'//nl//&
            nl//&
            '! the jacobian columns are: v0, angle'//nl//&
            'call projectile_jac(50.0_wp, 35.0_wp, 9.81_wp, t, vx, vy, tflight, range, hmax, x, y, &'//nl//&
            '                    d_vx, d_vy, d_tflight, d_range, d_hmax, d_x, d_y)'//nl//&
            'write(*, ''(2X,A,F11.4)'') ''d(range)/d(v0)    ='', d_range(1)'//nl//&
            'write(*, ''(2X,A,F11.4)'') ''d(range)/d(angle) ='', d_range(2)'//nl//&
            'end program projectile_driver'//nl
    end function driver

    function compiler() result(c)
        !! the Fortran compiler: `FPM_FC`, or `gfortran`
        character(len=:), allocatable :: c !! the compiler command
        integer :: n, stat
        call get_environment_variable('FPM_FC', length=n, status=stat)
        if (stat == 0 .and. n > 0) then
            allocate(character(len=n) :: c)
            call get_environment_variable('FPM_FC', c)
        else
            c = 'gfortran'
        end if
    end function compiler

    subroutine write_file(filename, text)
        !! write a string to a file
        character(len=*), intent(in) :: filename !! the file name
        character(len=*), intent(in) :: text !! the contents of the file
        integer :: iunit
        open(newunit=iunit, file=filename, status='replace', access='stream', form='formatted')
        write(iunit, '(A)', advance='no') text
        close(iunit)
        write(*, '(/A)') 'Wrote '//filename
    end subroutine write_file

    end program codegen
!*****************************************************************************************
