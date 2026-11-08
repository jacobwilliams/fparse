"""
Python version of the benchmark cases in test/benchmark.f90.

Each case is written by hand in Python (with the math module, and numpy for the
vector case), the same way the "Native Fortran" column is written in Fortran:
the same inputs for each iteration, the same checksum, and the same way of
choosing the number of iterations. benchmark.f90 runs this script and adds the
results to its table as the "Python" column.

Usage:

    python test/benchmark.py TARGET_TIME OUTPUT_FILE

The output file has one line with the Python and numpy versions, then one line
per case: the case number, the time per iteration [sec], and the checksum of
64 iterations (which benchmark.f90 compares with native Fortran).
"""

import math
import platform
import sys
import time

import numpy as np

N_MANY = 100  # number of equations in the "many" case
N_PTS = 1000  # number of points in the "vector" case

EXPR_COMPLEX = ('sqrt(x**2 + y**2 + z**2)*exp(-x*y/(1 + z**2)) + log(1 + abs(sin(x)*cos(y)))'
                ' - tan(z/3)**2 + (x - y)/(x + y + 3)')

# the vector case points (as in benchmark.f90):
_i = np.arange(1, N_PTS + 1, dtype=np.float64)
XS = 0.1 + 0.9 * _i / N_PTS
YS = 1.5 - _i / N_PTS
ZS = 0.2 + 0.5 * _i / N_PTS

sin, cos, exp, sqrt, log, tan = math.sin, math.cos, math.exp, math.sqrt, math.log, math.tan


def run_simple(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        cs += x + y * z
    return cs


def run_trig(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        cs += sin(x) * cos(y) + exp(-z)
    return cs


def run_poly(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        cs += ((((3 * x + 2) * x - 5) * x + 1) * x - 7) * y + z
    return cs


def run_complex(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        cs += (sqrt(x**2 + y**2 + z**2) * exp(-x * y / (1 + z**2)) + log(1 + abs(sin(x) * cos(y)))
               - tan(z / 3)**2 + (x - y) / (x + y + 3))
    return cs


def run_many(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        for k in range(1, N_MANY + 1):
            cs += sin(x + k) * y + z**2 / k - x * k
    return cs


def run_vector(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        cs += float(np.sum(np.sin(XS + d) * np.cos(YS + d) + np.exp(-(ZS + d))))
    return cs


def run_parse(n):
    for _ in range(n):
        compile(EXPR_COMPLEX, '<expr>', 'eval')
    return 0.0


def run_grad(n):
    cs = 0.0
    for i in range(1, n + 1):
        d = 1.0e-7 * (i % 1000)
        x, y, z = 0.5 + d, 1.3 - d, 0.7 + 2.0 * d
        cs += (sin(x) * cos(y) + exp(-z)) + cos(x) * cos(y) - sin(x) * sin(y) - exp(-z)
    return cs


# in the order of the cases in benchmark.f90:
CASES = [run_simple, run_trig, run_poly, run_complex, run_many, run_vector, run_parse, run_grad]


def time_it(run, target_time):
    """Time per iteration [sec], increasing the number of iterations until
    the total time exceeds `target_time` (as `time_it` in benchmark.f90)."""
    n = 1
    while True:
        t0 = time.perf_counter()
        run(n)
        elapsed = time.perf_counter() - t0
        if elapsed >= target_time:
            return elapsed / n
        if elapsed <= 0.0:
            n *= 10
        else:
            n = int(min(n * 10.0, 1.2 * n * target_time / elapsed)) + 1


def main():
    target_time = float(sys.argv[1])
    output_file = sys.argv[2]
    lines = [f'Python {platform.python_version()}, numpy {np.__version__}']
    for icase, run in enumerate(CASES, start=1):
        checksum = run(64)
        t = time_it(run, target_time)
        lines.append(f'{icase} {t:.17e} {checksum:.17e}')
    with open(output_file, 'w') as f:
        f.write('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main()
