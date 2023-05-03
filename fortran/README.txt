The Fortran program asc2eph.f may be used to convert the ASCII versions
of the planetary ephemerides into a binary format. The program testeph.f
checks the binary files against the test printouts stored with the ASCII
ephemeris files. The instructions are given in userguide.txt .

The binary files are platform-dependent, and the programs requires the user
to choose custom options within the source code before running.

The binary SPK files, with extensions .bsp are portable across platforms
so are recommended for new users. See the report documenting the format at:
http://arxiv.org/abs/1507.04291

The program testeph1.s in this directory is a preliminary version of a
replacement for testeph.f. The new version allows the user to choose
the units returns, with the coice of positions in the astronomical unit
used in the construction of the particular ephpemris; the standard
astronomical unit adopted by the IAU in 2012; or in kilometers.
The preliminary version also allows reading of the TT-TDB time difference.
