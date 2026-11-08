"""
Generate the fparse logo (media/logo.svg).

A rounded square with the text "f(x)" and, below it, a small expression tree
(the parsed form of the expression). The SVG is written directly, so no
packages are needed.

Usage:

    python media/make_logo.py [output_file] [size]
"""

import sys
from pathlib import Path

# colors (based on the purple of the Fortran logo, #734f96):
BG_TOP = '#8a63b3'       # background gradient (top left): a lighter Fortran purple
BG_BOTTOM = '#5c3f7a'    # background gradient (bottom right): a darker Fortran purple
TEXT = '#ffffff'         # the "f(x)" text
TREE_LINE = '#cbb6e6'    # the edges of the tree
TREE_NODE = '#ffb547'    # the nodes of the tree
TREE_LEAF = '#ffffff'    # the leaves of the tree

VIEW = 512               # size of the drawing (viewBox units)
RADIUS = 95              # corner radius of the square (the same proportion as the Fortran logo)


def tree():
    """The expression tree, as (nodes, edges). Each node is (x, y, kind)."""
    nodes = [
        (276, 318, 'op'),    # 0: root
        (196, 378, 'op'),    # 1
        (356, 378, 'leaf'),  # 2
        (156, 436, 'leaf'),  # 3
        (236, 436, 'leaf'),  # 4
    ]
    edges = [(0, 1), (0, 2), (1, 3), (1, 4)]
    return nodes, edges


def logo_svg(size):
    """The logo, as an SVG string, with a width and height of `size` pixels."""
    nodes, edges = tree()
    lines = []
    for i, j in edges:
        (x1, y1, _), (x2, y2, _) = nodes[i], nodes[j]
        lines.append(f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}"/>')
    circles = []
    for x, y, kind in nodes:
        if kind == 'op':
            circles.append(f'<circle cx="{x}" cy="{y}" r="19" fill="{TREE_NODE}"/>')
        else:
            circles.append(f'<circle cx="{x}" cy="{y}" r="14" fill="{TREE_LEAF}"/>')
    edge_svg = '\n    '.join(lines)
    node_svg = '\n    '.join(circles)
    return f'''<svg xmlns="http://www.w3.org/2000/svg" width="{size}" height="{size}" viewBox="0 0 {VIEW} {VIEW}">
  <title>fparse</title>
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="{BG_TOP}"/>
      <stop offset="1" stop-color="{BG_BOTTOM}"/>
    </linearGradient>
  </defs>
  <rect width="{VIEW}" height="{VIEW}" rx="{RADIUS}" ry="{RADIUS}" fill="url(#bg)"/>
  <text x="256" y="236" text-anchor="middle" fill="{TEXT}"
        font-family="Menlo, Consolas, 'DejaVu Sans Mono', 'Courier New', monospace"
        font-size="150" font-weight="bold">f(x)</text>
  <g stroke="{TREE_LINE}" stroke-width="9" stroke-linecap="round">
    {edge_svg}
  </g>
  <g>
    {node_svg}
  </g>
</svg>
'''


def main():
    output = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent / 'logo.svg'
    size = int(sys.argv[2]) if len(sys.argv) > 2 else VIEW
    output.write_text(logo_svg(size))
    print(f'wrote {output}')


if __name__ == '__main__':
    main()
