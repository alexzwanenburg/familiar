Update familiar to version 1.4.7

Vignettes are pre-compiled to avoid long compilation times on build (several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN, but are performed locally as part of the release process. Running the full test suite takes several hours. Locally run unit and integrated tests did not produce errors or (unexpected) warnings.



## R CMD check results

R CMD check was run on R-hub using rhub::check_for_cran(), producing the following results:

----------------------------------
Windows Server 2022, R-devel, 64 bit:
0 errors | 0 warnings | 4 notes

* checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    R      3.5Mb
    doc    1.0Mb
    help   1.2Mb

**Maintainer**: The size of the R-directory is due to source code. Addressing the size of the R directory would require creating a set of mutually dependent packages. This would likely result in a larger footprint across the packages.  

* checking HTML version of manual ... [39s] NOTE
Skipping checking math rendering: package 'V8' unavailable

**Maintainer**: I am not sure what the cause is, but it might be because the V8 package was not available on rhub.

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

**Maintainer**: I am not sure what the cause is. However, NULL is not present as a directory or file.  

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

**Maintainer**: This note only appeared on the Windows server test. It didn't appear locally (Windows 10) or the other builds.



----------------------------------
Fedora Linux, R-devel, clang, gfortran:
0 errors | 0 warnings | 1 notes

* checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    R      3.5Mb
    doc    1.0Mb
    help   1.2Mb



----------------------------------
Ubuntu Linux 20.04.1 LTS, R-release, GCC:
0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    help   1.1Mb
    R      3.6Mb



## Downstream dependencies

There are currently no downstream dependencies for this package.
