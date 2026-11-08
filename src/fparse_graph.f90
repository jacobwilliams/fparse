!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Export of the dependency graph of a set of expressions, in formats
!  that can be read by other tools:
!
!  * `dot`: the [Graphviz](https://graphviz.org) DOT language.
!  * `json`: the node-link format of [NetworkX](https://networkx.org)
!    (`networkx.node_link_graph`) and D3.js.
!  * `graphml`: [GraphML](http://graphml.graphdrawing.org) (yEd, Gephi, Cytoscape,
!    NetworkX, igraph).
!  * `mermaid`: a [Mermaid](https://mermaid.js.org) flowchart (rendered by GitHub
!    in markdown files).
!
!  The nodes are the variables (the inputs, and the outputs of the expressions),
!  and there is an edge from each variable to each expression that references
!  it, in the direction of the data flow.

module fparse_graph

use fparse_common

implicit none (type, external)

private

integer, parameter, public :: node_input = 1 !! node kind: an input variable
integer, parameter, public :: node_output = 2 !! node kind: the output of an expression

character(len=*), parameter :: nl = new_line('a') !! the newline character

type, public :: graph_node
    !! A node of the dependency graph.
    integer :: kind = node_input !! `node_input` or `node_output`
    integer :: index = 0 !! index of the input variable, or of the expression
    character(len=:), allocatable :: name !! the variable name (empty for an anonymous output)
    character(len=:), allocatable :: expression !! the expression (outputs only)
    integer :: order = 0 !! position of the expression in the evaluation order (outputs only)
end type graph_node

type, public :: dependency_graph
    !! A dependency graph: the nodes, and the edges between them.
    type(graph_node), dimension(:), allocatable :: nodes !! the nodes
    integer, dimension(:,:), allocatable :: edges !! the edges `[from, to]` (node indices),
                                                  !! with shape `[2, number of edges]`
end type dependency_graph

type :: string_builder
    !! A string that grows efficiently as text is appended.
    character(len=:), allocatable :: buf !! the buffer
    integer :: n = 0 !! the length of the string in the buffer
end type string_builder

public :: export_graph

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Export a dependency graph. `ok` is false if the format is not recognized.

subroutine export_graph(g, format, str, ok)

    type(dependency_graph), intent(in) :: g !! the graph
    character(len=*), intent(in) :: format !! `dot`, `json`, `graphml` or `mermaid` (case insensitive)
    character(len=:), allocatable, intent(out) :: str !! the exported graph
    logical, intent(out) :: ok !! if the format is valid

    type(string_builder) :: b

    ok = .true.
    select case (to_lower(trim(adjustl(format))))
    case ('dot', 'gv', 'graphviz')
        call write_dot(g, b)
    case ('json')
        call write_json(g, b)
    case ('graphml')
        call write_graphml(g, b)
    case ('mermaid', 'mmd')
        call write_mermaid(g, b)
    case default
        ok = .false.
        str = ''
        return
    end select
    str = b%buf(1:b%n)

end subroutine export_graph
!*****************************************************************************************

!*****************************************************************************************
!>
!  The identifier of a node: its name, or `#k` for the anonymous output of expression `k`
!  (that cannot be the same as a name).

pure function node_id(node) result(id)

    type(graph_node), intent(in) :: node !! the node
    character(len=:), allocatable :: id !! the identifier

    if (len(node%name) > 0) then
        id = node%name
    else
        id = '#'//int_to_str(node%index)
    end if

end function node_id
!*****************************************************************************************

!*****************************************************************************************
!>
!  The text to display for a node: its name, or the expression for an anonymous output.

pure function node_label(node) result(label)

    type(graph_node), intent(in) :: node !! the node
    character(len=:), allocatable :: label !! the label

    if (len(node%name) > 0) then
        label = node%name
    else
        label = node%expression
    end if

end function node_label
!*****************************************************************************************

!*****************************************************************************************
!>
!  The full description of a node: `name = expression` for an output.

pure function node_description(node) result(text)

    type(graph_node), intent(in) :: node !! the node
    character(len=:), allocatable :: text !! the description

    if (node%kind == node_input) then
        text = node%name
    else if (len(node%name) > 0) then
        text = node%name//' = '//node%expression
    else
        text = node%expression
    end if

end function node_description
!*****************************************************************************************

!*****************************************************************************************
!>
!  The kind of a node, as a string.

pure function kind_name(node) result(kind)

    type(graph_node), intent(in) :: node !! the node
    character(len=:), allocatable :: kind !! `input` or `output`

    if (node%kind == node_input) then
        kind = 'input'
    else
        kind = 'output'
    end if

end function kind_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Graphviz DOT language.

subroutine write_dot(g, b)

    type(dependency_graph), intent(in) :: g !! the graph
    type(string_builder), intent(inout) :: b !! the output

    integer :: i

    call add(b, 'digraph dependencies {'//nl)
    call add(b, '  rankdir=LR;'//nl)
    do i = 1, size(g%nodes)
        associate (node => g%nodes(i))
            call add(b, '  '//dot_str(node_id(node))//' [label='//dot_str(node_label(node)))
            if (node%kind == node_input) then
                call add(b, ', shape=ellipse')
            else
                call add(b, ', shape=box')
            end if
            call add(b, ', class='//dot_str(kind_name(node))//', tooltip='//dot_str(node_description(node))//'];'//nl)
        end associate
    end do
    do i = 1, size(g%edges, 2)
        call add(b, '  '//dot_str(node_id(g%nodes(g%edges(1, i))))//' -> '//&
                 dot_str(node_id(g%nodes(g%edges(2, i))))//';'//nl)
    end do
    call add(b, '}'//nl)

end subroutine write_dot
!*****************************************************************************************

!*****************************************************************************************
!>
!  JSON, in the node-link format of NetworkX and D3.js.

subroutine write_json(g, b)

    type(dependency_graph), intent(in) :: g !! the graph
    type(string_builder), intent(inout) :: b !! the output

    integer :: i

    call add(b, '{'//nl)
    call add(b, '  "directed": true,'//nl)
    call add(b, '  "multigraph": false,'//nl)
    call add(b, '  "graph": {"name": "dependencies"},'//nl)
    call add(b, '  "nodes": [')
    do i = 1, size(g%nodes)
        associate (node => g%nodes(i))
            if (i > 1) call add(b, ',')
            call add(b, nl//'    {"id": '//json_str(node_id(node))//', "kind": '//json_str(kind_name(node))//&
                     ', "index": '//int_to_str(node%index)//', "name": '//json_str(node%name))
            if (node%kind == node_output) then
                call add(b, ', "expression": '//json_str(node%expression)//', "order": '//int_to_str(node%order))
            end if
            call add(b, '}')
        end associate
    end do
    call add(b, nl//'  ],'//nl)
    call add(b, '  "links": [')
    do i = 1, size(g%edges, 2)
        if (i > 1) call add(b, ',')
        call add(b, nl//'    {"source": '//json_str(node_id(g%nodes(g%edges(1, i))))//&
                 ', "target": '//json_str(node_id(g%nodes(g%edges(2, i))))//'}')
    end do
    call add(b, nl//'  ]'//nl)
    call add(b, '}'//nl)

end subroutine write_json
!*****************************************************************************************

!*****************************************************************************************
!>
!  GraphML.

subroutine write_graphml(g, b)

    type(dependency_graph), intent(in) :: g !! the graph
    type(string_builder), intent(inout) :: b !! the output

    integer :: i

    call add(b, '<?xml version="1.0" encoding="UTF-8"?>'//nl)
    call add(b, '<graphml xmlns="http://graphml.graphdrawing.org/xmlns">'//nl)
    call add(b, '  <key id="kind" for="node" attr.name="kind" attr.type="string"/>'//nl)
    call add(b, '  <key id="index" for="node" attr.name="index" attr.type="int"/>'//nl)
    call add(b, '  <key id="name" for="node" attr.name="name" attr.type="string"/>'//nl)
    call add(b, '  <key id="expression" for="node" attr.name="expression" attr.type="string"/>'//nl)
    call add(b, '  <key id="order" for="node" attr.name="order" attr.type="int"/>'//nl)
    call add(b, '  <graph id="dependencies" edgedefault="directed">'//nl)
    do i = 1, size(g%nodes)
        associate (node => g%nodes(i))
            call add(b, '    <node id="'//xml_str(node_id(node))//'">')
            call add(b, '<data key="kind">'//kind_name(node)//'</data>')
            call add(b, '<data key="index">'//int_to_str(node%index)//'</data>')
            call add(b, '<data key="name">'//xml_str(node%name)//'</data>')
            if (node%kind == node_output) then
                call add(b, '<data key="expression">'//xml_str(node%expression)//'</data>')
                call add(b, '<data key="order">'//int_to_str(node%order)//'</data>')
            end if
            call add(b, '</node>'//nl)
        end associate
    end do
    do i = 1, size(g%edges, 2)
        call add(b, '    <edge source="'//xml_str(node_id(g%nodes(g%edges(1, i))))//&
                 '" target="'//xml_str(node_id(g%nodes(g%edges(2, i))))//'"/>'//nl)
    end do
    call add(b, '  </graph>'//nl)
    call add(b, '</graphml>'//nl)

end subroutine write_graphml
!*****************************************************************************************

!*****************************************************************************************
!>
!  Mermaid flowchart. The node identifiers are `v1`, `v2`, ... (node indices),
!  since Mermaid identifiers can't contain all characters.

subroutine write_mermaid(g, b)

    type(dependency_graph), intent(in) :: g !! the graph
    type(string_builder), intent(inout) :: b !! the output

    integer :: i

    call add(b, 'flowchart LR'//nl)
    do i = 1, size(g%nodes)
        associate (node => g%nodes(i))
            if (node%kind == node_input) then
                call add(b, '  v'//int_to_str(i)//'(["'//mermaid_str(node_label(node))//'"])'//nl)
            else
                call add(b, '  v'//int_to_str(i)//'["'//mermaid_str(node_label(node))//'"]'//nl)
            end if
        end associate
    end do
    do i = 1, size(g%edges, 2)
        call add(b, '  v'//int_to_str(g%edges(1, i))//' --> v'//int_to_str(g%edges(2, i))//nl)
    end do

end subroutine write_mermaid
!*****************************************************************************************

!*****************************************************************************************
!>
!  A quoted string for the DOT language.

pure function dot_str(s) result(q)

    character(len=*), intent(in) :: s !! the string
    character(len=:), allocatable :: q !! the quoted string

    integer :: i

    q = '"'
    do i = 1, len(s)
        select case (s(i:i))
        case ('"', '\')
            q = q//'\'//s(i:i)
        case default
            q = q//s(i:i)
        end select
    end do
    q = q//'"'

end function dot_str
!*****************************************************************************************

!*****************************************************************************************
!>
!  A quoted JSON string.

pure function json_str(s) result(q)

    character(len=*), intent(in) :: s !! the string
    character(len=:), allocatable :: q !! the quoted string

    integer :: i, c
    character(len=*), parameter :: hex = '0123456789abcdef'

    q = '"'
    do i = 1, len(s)
        c = iachar(s(i:i))
        select case (c)
        case (iachar('"'), iachar('\'))
            q = q//'\'//s(i:i)
        case (0:31)
            q = q//'\u00'//hex(c/16+1:c/16+1)//hex(mod(c,16)+1:mod(c,16)+1)
        case default
            q = q//s(i:i)
        end select
    end do
    q = q//'"'

end function json_str
!*****************************************************************************************

!*****************************************************************************************
!>
!  A string with the XML special characters replaced by entities.

pure function xml_str(s) result(q)

    character(len=*), intent(in) :: s !! the string
    character(len=:), allocatable :: q !! the escaped string

    integer :: i

    q = ''
    do i = 1, len(s)
        select case (s(i:i))
        case ('&'); q = q//'&amp;'
        case ('<'); q = q//'&lt;'
        case ('>'); q = q//'&gt;'
        case ('"'); q = q//'&quot;'
        case (''''); q = q//'&apos;'
        case default; q = q//s(i:i)
        end select
    end do

end function xml_str
!*****************************************************************************************

!*****************************************************************************************
!>
!  A string with the special characters of a Mermaid label replaced by entity codes.

pure function mermaid_str(s) result(q)

    character(len=*), intent(in) :: s !! the string
    character(len=:), allocatable :: q !! the escaped string

    integer :: i

    q = ''
    do i = 1, len(s)
        select case (s(i:i))
        case ('"'); q = q//'#quot;'
        case ('<'); q = q//'#lt;'
        case ('>'); q = q//'#gt;'
        case ('#'); q = q//'#35;'
        case default; q = q//s(i:i)
        end select
    end do

end function mermaid_str
!*****************************************************************************************

!*****************************************************************************************
!>
!  Append text to a string builder.

pure subroutine add(b, s)

    type(string_builder), intent(inout) :: b !! the string builder
    character(len=*), intent(in) :: s !! the text to append

    character(len=:), allocatable :: tmp

    if (.not. allocated(b%buf)) allocate(character(len=max(1024, 2*len(s))) :: b%buf)
    if (b%n + len(s) > len(b%buf)) then
        allocate(character(len=max(2*len(b%buf), b%n + len(s))) :: tmp)
        tmp(1:b%n) = b%buf(1:b%n)
        call move_alloc(tmp, b%buf)
    end if
    b%buf(b%n+1:b%n+len(s)) = s
    b%n = b%n + len(s)

end subroutine add
!*****************************************************************************************

!*****************************************************************************************
end module fparse_graph
!*****************************************************************************************
