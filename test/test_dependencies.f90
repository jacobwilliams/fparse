!*****************************************************************************************
!>
!  Tests for the dependency queries and the export of the dependency graph.

    module test_dependencies

    use fparse
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_utils

    implicit none

    private

    public :: collect_dependencies

    contains

    subroutine collect_dependencies(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests in this suite
    testsuite = [ &
        new_unittest("direct", test_direct), &
        new_unittest("indirect", test_indirect), &
        new_unittest("dependents", test_dependents), &
        new_unittest("depends_on", test_depends_on), &
        new_unittest("matrix", test_matrix), &
        new_unittest("export_dot", test_export_dot), &
        new_unittest("export_json", test_export_json), &
        new_unittest("export_graphml", test_export_graphml), &
        new_unittest("export_mermaid", test_export_mermaid), &
        new_unittest("errors", test_dependency_errors) ]
    end subroutine collect_dependencies

    subroutine setup(p)
    !! The test system. The expressions are (by index): `d`, `a`, `b`, `c`, and
    !! an anonymous one (5). The input `u` is not used. The full variable list is
    !! `x, y, z, u, d, a, b, c, (5)`, and the evaluation order is `a, b, c, (5), d`.
    type(fparser), intent(out) :: p !! the parser
    call p%parse(['x', 'y', 'z', 'u'], str_list('d = b*c', 'a = x*y', 'b = a + z', 'c = sin(x)', 'x < y'))
    end subroutine setup

    logical function same(a, b)
    !! if two integer arrays are the same
    integer, dimension(:), intent(in) :: a !! the first array
    integer, dimension(:), intent(in) :: b !! the second array
    same = size(a) == size(b)
    if (same) same = all(a == b)
    end function same

    subroutine test_direct(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    integer, dimension(:), allocatable :: inputs, outputs
    call setup(p)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call p%get_dependencies('d', inputs, outputs)
    call check(error, same(inputs, [integer ::]) .and. same(outputs, [3, 4]), 'd'); if (allocated(error)) return
    call p%get_dependencies('B', inputs, outputs) ! case insensitive
    call check(error, same(inputs, [3]) .and. same(outputs, [2]), 'b'); if (allocated(error)) return
    call p%get_dependencies(5, inputs, outputs) ! anonymous, by index
    call check(error, same(inputs, [1, 2]) .and. same(outputs, [integer ::]), '(5)'); if (allocated(error)) return
    call p%get_dependencies('x', inputs, outputs) ! inputs have no dependencies
    call check(error, size(inputs) == 0 .and. size(outputs) == 0, 'x'); if (allocated(error)) return
    end subroutine test_direct

    subroutine test_indirect(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    integer, dimension(:), allocatable :: inputs, outputs
    call setup(p)
    call p%get_dependencies('d', inputs, outputs, indirect=.true.)
    call check(error, same(inputs, [1, 2, 3]) .and. same(outputs, [2, 3, 4]), 'd'); if (allocated(error)) return
    call p%get_dependencies(3, inputs, outputs, indirect=.true.)
    call check(error, same(inputs, [1, 2, 3]) .and. same(outputs, [2]), 'b'); if (allocated(error)) return
    call p%get_dependencies('c', inputs, outputs, indirect=.true.)
    call check(error, same(inputs, [1]) .and. same(outputs, [integer ::]), 'c'); if (allocated(error)) return
    end subroutine test_indirect

    subroutine test_dependents(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    integer, dimension(:), allocatable :: outputs
    call setup(p)
    call p%get_dependents('x', outputs)
    call check(error, same(outputs, [2, 4, 5]), 'x'); if (allocated(error)) return
    call p%get_dependents('x', outputs, indirect=.true.)
    call check(error, same(outputs, [1, 2, 3, 4, 5]), 'x indirect'); if (allocated(error)) return
    call p%get_dependents('a', outputs)
    call check(error, same(outputs, [3]), 'a'); if (allocated(error)) return
    call p%get_dependents('a', outputs, indirect=.true.)
    call check(error, same(outputs, [1, 3]), 'a indirect'); if (allocated(error)) return
    call p%get_dependents('u', outputs, indirect=.true.)
    call check(error, size(outputs) == 0, 'u'); if (allocated(error)) return
    call p%get_dependents('d', outputs, indirect=.true.)
    call check(error, size(outputs) == 0, 'd'); if (allocated(error)) return
    end subroutine test_dependents

    subroutine test_depends_on(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    call setup(p)
    call check(error, .not. p%depends_on('d', 'x'), 'd(x) direct'); if (allocated(error)) return
    call check(error, p%depends_on('d', 'x', indirect=.true.), 'd(x) indirect'); if (allocated(error)) return
    call check(error, p%depends_on('d', 'B'), 'd(b)'); if (allocated(error)) return
    call check(error, .not. p%depends_on('d', 'u', indirect=.true.), 'd(u)'); if (allocated(error)) return
    call check(error, .not. p%depends_on('x', 'x', indirect=.true.), 'x(x)'); if (allocated(error)) return
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check(error, .not. p%depends_on('d', 'w'), 'unknown'); if (allocated(error)) return
    call check(error, p%failed(), 'unknown variable should fail'); if (allocated(error)) return
    call check(error, index(p%error_message(), 'unknown variable "w"') > 0, p%error_message())
    end subroutine test_depends_on

    subroutine test_matrix(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    logical, dimension(:,:), allocatable :: dep
    integer :: i, j
    logical, dimension(5, 9) :: expected
    call setup(p)
    !                   x        y        z        u        d        a        b        c        (5)
    expected(1,:) = [.false., .false., .false., .false., .false., .false., .true.,  .true.,  .false.]
    expected(2,:) = [.true.,  .true.,  .false., .false., .false., .false., .false., .false., .false.]
    expected(3,:) = [.false., .false., .true.,  .false., .false., .true.,  .false., .false., .false.]
    expected(4,:) = [.true.,  .false., .false., .false., .false., .false., .false., .false., .false.]
    expected(5,:) = [.true.,  .true.,  .false., .false., .false., .false., .false., .false., .false.]
    call p%dependency_matrix(dep)
    call check(error, all(shape(dep) == [5, 9]), 'shape'); if (allocated(error)) return
    call check(error, all(dep .eqv. expected), 'direct'); if (allocated(error)) return
    expected(1,:) = [.true.,  .true.,  .true.,  .false., .false., .true.,  .true.,  .true.,  .false.]
    expected(3,:) = [.true.,  .true.,  .true.,  .false., .false., .true.,  .false., .false., .false.]
    call p%dependency_matrix(dep, indirect=.true.)
    call check(error, all(dep .eqv. expected), 'indirect'); if (allocated(error)) return
    ! consistent with the other queries:
    do i = 1, 5
        do j = 1, 9
            call check(error, dep(i, j) .eqv. depends_on_index(i, j), 'depends_on')
            if (allocated(error)) return
        end do
    end do
    contains
        logical function depends_on_index(ie, jv)
        !! `depends_on` for expression `ie` and variable `jv` (by index)
        integer, intent(in) :: ie !! the expression index
        integer, intent(in) :: jv !! the variable index
        character(len=1), dimension(9), parameter :: names = ['x','y','z','u','d','a','b','c',' ']
        integer, dimension(:), allocatable :: inputs, outputs
        call p%get_dependencies(ie, inputs, outputs, indirect=.true.)
        if (jv <= 4) then
            depends_on_index = any(inputs == jv)
        else
            depends_on_index = any(outputs == jv - 4)
        end if
        if (ie < 5 .and. jv < 9) then ! named: depends_on must agree (if not, return the wrong result)
            if (depends_on_index .neqv. p%depends_on(names(ie+4), names(jv), indirect=.true.)) &
                depends_on_index = .not. depends_on_index
        end if
        end function depends_on_index
    end subroutine test_matrix

    subroutine check_contains(error, str, parts)
    !! check that a string contains all the parts
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    character(len=*), intent(in) :: str !! the string
    character(len=*), dimension(:), intent(in) :: parts !! the text that must be in the string
    integer :: i
    do i = 1, size(parts)
        call check(error, index(str, trim(parts(i))) > 0, 'missing: '//trim(parts(i))//new_line('a')//str)
        if (allocated(error)) return
    end do
    end subroutine check_contains

    subroutine test_export_dot(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: str
    call setup(p)
    call p%export_dependencies('DOT', str)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check_contains(error, str, str_list( &
        'digraph dependencies {', &
        '"x" [label="x", shape=ellipse, class="input", tooltip="x"];', &
        '"d" [label="d", shape=box, class="output", tooltip="d = b*c"];', &
        '"#5" [label="x < y", shape=box'))
    if (allocated(error)) return
    call check_contains(error, str, str_list('"x" -> "a";', '"a" -> "b";', '"x" -> "#5";', '}'))
    if (allocated(error)) return
    call check(error, index(str, '"u" ->') == 0, 'u is not used')
    end subroutine test_export_dot

    subroutine test_export_json(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: str
    call setup(p)
    call p%export_dependencies('json', str)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check_contains(error, str, str_list( &
        '"directed": true,', &
        '{"id": "u", "kind": "input", "index": 4, "name": "u"}', &
        '{"id": "d", "kind": "output", "index": 1, "name": "d", "expression": "b*c", "order": 5}', &
        '{"id": "#5", "kind": "output", "index": 5, "name": "", "expression": "x < y", "order": 4}', &
        '{"source": "a", "target": "b"}', &
        '{"source": "c", "target": "d"}'))
    end subroutine test_export_json

    subroutine test_export_graphml(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: str
    call setup(p)
    call p%export_dependencies('graphml', str)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check_contains(error, str, str_list( &
        '<graph id="dependencies" edgedefault="directed">', &
        '<node id="#5"><data key="kind">output</data>', &
        '<data key="expression">x &lt; y</data>', &
        '<edge source="a" target="b"/>', &
        '</graphml>'))
    end subroutine test_export_graphml

    subroutine test_export_mermaid(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    character(len=:), allocatable :: str
    call setup(p)
    call p%export_dependencies('mermaid', str)
    call check(error, .not. p%failed(), p%error_message()); if (allocated(error)) return
    call check_contains(error, str, str_list( &
        'flowchart LR', 'v1(["x"])', 'v5["d"]', 'v9["x #lt; y"]', 'v6 --> v7', 'v1 --> v9'))
    end subroutine test_export_mermaid

    subroutine test_dependency_errors(error)
    type(error_type), allocatable, intent(out) :: error !! the error, allocated if the test fails
    type(fparser) :: p
    integer, dimension(:), allocatable :: inputs, outputs
    logical, dimension(:,:), allocatable :: dep
    character(len=:), allocatable :: str, errmsg
    integer :: status

    ! not parsed
    call p%get_dependencies(1, inputs, outputs, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'not been parsed') > 0, 'not parsed'); if (allocated(error)) return
    call p%dependency_matrix(dep, status=status)
    call check(error, status /= 0 .and. size(dep) == 0, 'not parsed (matrix)'); if (allocated(error)) return
    call p%export_dependencies('dot', str, status=status)
    call check(error, status /= 0, 'not parsed (export)'); if (allocated(error)) return

    call setup(p)
    call p%get_dependencies(6, inputs, outputs, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'invalid expression index 6') > 0, errmsg)
    if (allocated(error)) return
    call p%get_dependencies('w', inputs, outputs, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'unknown variable "w"') > 0, errmsg)
    if (allocated(error)) return
    call p%get_dependents('w', outputs, status=status)
    call check(error, status /= 0 .and. size(outputs) == 0, 'unknown variable (dependents)'); if (allocated(error)) return
    call p%export_dependencies('png', str, status=status, errmsg=errmsg)
    call check(error, status /= 0 .and. index(errmsg, 'unknown dependency export format "png"') > 0, errmsg)
    if (allocated(error)) return

    ! the errors are cleared by the next successful call:
    call p%get_dependencies('d', inputs, outputs, status=status)
    call check(error, status == 0 .and. .not. p%failed(), 'cleared')
    end subroutine test_dependency_errors

    end module test_dependencies
!*****************************************************************************************
