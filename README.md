# fortran-speed
Testing different fortran coding paradigms for speed

## Looping over types

This test compares looping over individual arrays
of variables with multiple dimensions to looping
over arrays of derived types, where the last dimension
is contained within the derived type.

To perform the test, compile the code, then run it:

```
gfortran looping_over_types.f90
./a.out
```

Example output:

```
Time using:   arrays:      1.21     s
Time using:  columns:      1.05     s
```

Conclusions

- gfortran 12.2.0: columns inside derived type runs slightly faster.
- ifort 19.1.3.304: to do.
