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
Time using:   arrays:      1.30     s
Time using:  columns:      1.30     s
```

Conclusions

gfortran 12.2.0: both methods give the same computational speed.
ifort 19.1.3.304: both methods give the same computational speed.
