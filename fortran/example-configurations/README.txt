The values of NRECL, FSIZER, and (depending on FSIZER) KSIZE must be configured
before compiling the fortran programs.

These patch files are example changes to the fortran files that are necesary
before you can compile them.  These changes will work for most UNIX
implementations, but see ../userguide.txt for complete documentation to make
sure they are right for your OS and hardware.

Run `make fortran` to build the fortran tools after patching them.
