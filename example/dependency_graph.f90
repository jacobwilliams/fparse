!*****************************************************************************************
!>
!  Example of the dependency queries of fparse.
!
!  A set of equations that compute the classical orbital elements (and other quantities)
!  from a position and velocity vector is parsed, and its dependency graph is:
!
!  * drawn as an SVG image, using the dependency information and the
!    [fdot](https://github.com/jacobwilliams/fdot) graph layout library,
!  * exported in the formats that other tools can read: Graphviz DOT, JSON
!    (NetworkX node-link), GraphML, and Mermaid.
!
!  Usage (the files are written to the directory given as an argument,
!  the current directory by default):
!```
!  fpm run --example dependency_graph -- build
!```

    program dependency_graph

    use fparse
    use fdot, only: graph_t, RANK_LR

    implicit none

    ! equations: position `r` [km] and velocity `v` [km/s] to orbital elements
    character(len=*), dimension(*), parameter :: names = [character(len=6) :: &
        'rmag', 'vmag', 'rdotv', 'h', 'hmag', 'energy', 'a', 'evec', 'ecc', 'p', 'inc', &
        'nvec', 'nmag', 'raan', 'argp', 'nu', 'period', 'rp', 'ra', 'fpa', 'vcirc', 'vesc']
    character(len=*), dimension(*), parameter :: equations = [character(len=100) :: &
        'norm2(r)', &
        'norm2(v)', &
        'dot(r, v)', &
        'cross(r, v)', &
        'norm2(h)', &
        'vmag^2/2 - mu/rmag', &
        '-mu/(2*energy)', &
        'cross(v, h)/mu - r/rmag', &
        'norm2(evec)', &
        'hmag^2/mu', &
        'acos(h[3]/hmag)', &
        'cross([0, 0, 1], h)', &
        'norm2(nvec)', &
        'if(acos(nvec[1]/nmag), 2*pi - acos(nvec[1]/nmag), nvec[2] >= 0)', &
        'if(acos(dot(nvec, evec)/(nmag*ecc)), 2*pi - acos(dot(nvec, evec)/(nmag*ecc)), evec[3] >= 0)', &
        'if(acos(dot(evec, r)/(ecc*rmag)), 2*pi - acos(dot(evec, r)/(ecc*rmag)), rdotv >= 0)', &
        '2*pi*sqrt(a^3/mu)', &
        'a*(1 - ecc)', &
        'a*(1 + ecc)', &
        'atan2(rdotv/rmag, hmag/rmag)', &
        'sqrt(mu/rmag)', &
        'sqrt(2)*vcirc']

    character(len=*), parameter :: input_fill = '#3657c9' !! colour of the input variables
    character(len=*), parameter :: inner_fill = '#646d7e' !! colour of the intermediate outputs
    character(len=*), parameter :: final_fill = '#1d7a55' !! colour of the final outputs (not used
                                                          !! by any other expression)
    real(wp), parameter :: deg = 180.0_wp / acos(-1.0_wp) !! radians to degrees

    type(fparser) :: p
    type(graph_t) :: g
    integer, dimension(:), allocatable :: node, inputs, outputs, users
    character(len=:), allocatable :: str, errmsg, dir, svg
    character(len=*), dimension(*), parameter :: formats = ['dot    ', 'json   ', 'graphml', 'mermaid']
    character(len=*), dimension(*), parameter :: extensions = ['dot ', 'json', 'xml ', 'mmd ']
    real(wp) :: x
    integer :: i, k, nin, nout, status, length

    ! output directory:
    call get_command_argument(1, length=length)
    if (length > 0) then
        allocate(character(len=length) :: dir)
        call get_command_argument(1, dir)
    else
        dir = '.'
    end if

    call p%parse(['r ', 'v ', 'mu'], equations, names=names, status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    nin = p%num_inputs()
    nout = p%num_outputs()

    ! evaluate (Vallado, example 2-5):
    call p%set_variable('r', [6524.834_wp, 6862.875_wp, 6448.296_wp])
    call p%set_variable('v', [4.901327_wp, 5.533756_wp, -1.976341_wp])
    call p%set_variable('mu', 398600.4418_wp)
    call p%evaluate(status=status, errmsg=errmsg)
    if (status /= 0) error stop errmsg
    write(*, '(A)') 'Orbital elements:'
    call p%get('a', x);    write(*, '(A,F12.3,A)') '  a    = ', x, ' km'
    call p%get('ecc', x);  write(*, '(A,F12.6)')   '  ecc  = ', x
    call p%get('inc', x);  write(*, '(A,F12.4,A)') '  inc  = ', x*deg, ' deg'
    call p%get('raan', x); write(*, '(A,F12.4,A)') '  raan = ', x*deg, ' deg'
    call p%get('argp', x); write(*, '(A,F12.4,A)') '  argp = ', x*deg, ' deg'
    call p%get('nu', x);   write(*, '(A,F12.4,A)') '  nu   = ', x*deg, ' deg'

    ! some queries:
    write(*, '(/A)') 'Dependencies:'
    call p%get_dependencies('argp', inputs, outputs)
    write(*, '(A)') '  argp uses:             '//output_list(outputs)
    call p%get_dependencies('argp', inputs, outputs, indirect=.true.)
    write(*, '(A)') '  argp depends on:       '//input_list(inputs)//', '//output_list(outputs)
    call p%get_dependents('mu', outputs, indirect=.true.)
    write(*, '(A)') '  mu changes:            '//output_list(outputs)
    call p%get_dependents('hmag', outputs, indirect=.true.)
    write(*, '(A)') '  hmag changes:          '//output_list(outputs)
    write(*, '(A,L1)') '  inc depends on mu:     ', p%depends_on('inc', 'mu', indirect=.true.)

    ! draw the graph: one node for each variable, and an edge from each
    ! variable to each expression that references it.
    call g%initialize(direction=RANK_LR)
    allocate(node(nin + nout))
    do i = 1, nin
        call g%add_node(p%input_name(i), node(i), fill=input_fill)
    end do
    do i = 1, nout
        call p%get_dependents(p%output_name(i), users)
        if (size(users) == 0) then
            call g%add_node(p%output_name(i), node(nin+i), fill=final_fill)
        else
            call g%add_node(p%output_name(i), node(nin+i), fill=inner_fill)
        end if
    end do
    do i = 1, nout
        call p%get_dependencies(i, inputs, outputs)
        do k = 1, size(inputs)
            call g%add_edge(node(inputs(k)), node(nin+i), colour='#8a93a5')
        end do
        do k = 1, size(outputs)
            call g%add_edge(node(nin+outputs(k)), node(nin+i), colour='#8a93a5')
        end do
    end do
    call g%layout(status)
    if (status /= 0) error stop 'graph layout failed'
    call g%write_svg(svg)
    ! the text inherits the stroke of the edges from the enclosing group, which
    ! draws a grey outline around the white letters, so remove it:
    svg = replace_all(svg, '<text ', '<text stroke="none" ')
    call write_file(dir//'/dependencies.svg', svg)

    ! export in the other formats:
    do i = 1, size(formats)
        call p%export_dependencies(trim(formats(i)), str, status=status, errmsg=errmsg)
        if (status /= 0) error stop errmsg
        call write_file(dir//'/dependencies.'//trim(extensions(i)), str)
    end do

    contains

    function input_list(idx) result(s)
        !! the names of a list of input variables
        integer, dimension(:), intent(in) :: idx !! the input variable indices
        character(len=:), allocatable :: s !! the names, separated by commas
        integer :: j
        s = ''
        do j = 1, size(idx)
            if (j > 1) s = s//', '
            s = s//p%input_name(idx(j))
        end do
    end function input_list

    function output_list(idx) result(s)
        !! the names of a list of outputs
        integer, dimension(:), intent(in) :: idx !! the expression indices
        character(len=:), allocatable :: s !! the names, separated by commas
        integer :: j
        s = ''
        do j = 1, size(idx)
            if (j > 1) s = s//', '
            s = s//p%output_name(idx(j))
        end do
    end function output_list

    function replace_all(s, old, new) result(r)
        !! replace all the occurrences of a substring
        character(len=*), intent(in) :: s !! the string
        character(len=*), intent(in) :: old !! the substring to replace
        character(len=*), intent(in) :: new !! the replacement
        character(len=:), allocatable :: r !! the string with the replacements
        integer :: i, j
        r = ''
        i = 1
        do
            j = index(s(i:), old)
            if (j == 0) exit
            r = r//s(i:i+j-2)//new
            i = i + j - 1 + len(old)
        end do
        r = r//s(i:)
    end function replace_all

    subroutine write_file(filename, text)
        !! write a string to a file
        character(len=*), intent(in) :: filename !! the file name
        character(len=*), intent(in) :: text !! the contents of the file
        integer :: iunit
        open(newunit=iunit, file=filename, status='replace', access='stream', form='formatted')
        write(iunit, '(A)', advance='no') text
        close(iunit)
        write(*, '(A)') 'wrote '//filename
    end subroutine write_file

    end program dependency_graph
!*****************************************************************************************
