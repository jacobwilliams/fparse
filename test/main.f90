!*****************************************************************************************
!>
!  Test driver for the fparse library.

    program tester

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_basic, only: collect_basic
    use test_functions, only: collect_functions
    use test_vectors, only: collect_vectors
    use test_conditionals, only: collect_conditionals
    use test_multi, only: collect_multi
    use test_derivatives, only: collect_derivatives
    use test_user_functions, only: collect_user_functions
    use test_errors, only: collect_errors

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("basic", collect_basic), &
        new_testsuite("functions", collect_functions), &
        new_testsuite("vectors", collect_vectors), &
        new_testsuite("conditionals", collect_conditionals), &
        new_testsuite("multi", collect_multi), &
        new_testsuite("derivatives", collect_derivatives), &
        new_testsuite("user_functions", collect_user_functions), &
        new_testsuite("errors", collect_errors) ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

    end program tester
!*****************************************************************************************
