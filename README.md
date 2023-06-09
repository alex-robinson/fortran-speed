# fortran-speed
Testing different fortran coding paradigms for speed

## Looping over types

This test compares looping over individual arrays
of variables with multiple dimensions to looping
over arrays of derived types, where e.g., the last dimension
is contained within the derived type.

Case 1 (arrays): a single derived type contains 3D arrays inside it.

Case 2 (columns): a 2D array of derived types contains 1D arrays inside them.

Case 3 (points): a 3D array of derived types contains points inside them.

To perform the test, compile the code, then run it:

```
gfortran looping_over_types.f90
./a.out
```

Example output:

```
Time using:   arrays:      2.45     s
Time using:  columns:      2.08     s
Time using:   points:      1.99     s
```

However, using different compiler flags can change the comparison:

```
gfortran -O3 -flto -march=native looping_over_types.f90
./a.out
```

```
Time using:   arrays:      1.90     s
Time using:  columns:      1.53     s
Time using:   points:      1.53     s
```

Conclusions

*gfortran 12.2.0, ifort 19.1.3.304*

Using arrays inside the derived type is a little slower than
using a 2D array of derived types with a column vector inside, 
and the fastest is actually to use a 3D array of derived types
with points inside.

The right compiler flags can greatly reduce differences between these methods.
