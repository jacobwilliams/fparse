
### Requirements

* pure Fortran, standard with no compiler extensions.
* MIT license
* evaluate expression strings that are functions of many variables (e.g., "sin(x)+2.0*y")
* be written in modern fortran, object-oriented, well documented (using FORD-compatible doc strings),
* support basic mathematical functions (e.g., sin, cos, exp, log, etc)
* support single-input function (e.g. 'sin(x)')
* support two-variable functions (e.g., 'max(x,y)')
* support three-variable if statements (e.g., "if(value if true, value if false, expression)"). for the expressions, 0 of false, /=1 for true.
* support vector-valued function (e.g., 'dot_product(x,y)', 'cross(x,y)')
* support nested if statements and complex expressions
* values of variables can be provided as input to the expressions at runtime
* equations are parsed into an internal representation suitable for evaluation at runtime
* error checking (at parse time, and runtime)
* support optional user-defined functions within expressions
* support vectorized operations (e.g., operations on arrays of values). a variable can be a scalar or vector. functions can return vectors or scalars like normal fortran (e.g., sin(x) is a vector if x is a vector).
* the expressions are an array of strings, each representing a separate equation or function. each can contain expressions of many variables. the output of the expressions also have defined names which can be in other expresssions in the same list. (so the code has to determine the proper order to evaluate them). example: [z: 'sin(x) + 2.0*y', w: 'z + x'].
* the parser should determine the correct evaluation order based on variable dependencies within the expressions.
* the parser should also be able to evaluate the first derivative of the expressions with respect to any of the variables at runtime. this should be an option and not using it should not affect the normal evaluation of the expressions (or slow it down). Only first derivatives are needed. should work for scalars and vectors (so the derivative can be a scalar, vector, or matrix).
* Compiled and tested with gfortran, using the Fortran package manager (fpm). the library should be easy to integrate into other Fortran projects (using FPM).
* variables not not case sensitive (e.g., "x" and "X" refer to the same variable)
* comprehensive unit testing

### See also

Other fortran parsers are listed here: https://github.com/sdm900/fortran_parser