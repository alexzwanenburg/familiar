Update familiar to version 1.3.0

Vignettes are pre-compiled to avoid long compilation times on build (several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN, but are performed locally as part of the release process. Running the full test suite takes several hours. Locally run unit and integrated tests did not produce errors or (unexpected) warnings.



## R CMD check results

R CMD check was run on R-hub using rhub::check_for_cran(), producing the following results:

----------------------------------
Windows Server 2022, R-devel, 64 bit:
0 errors | 0 warnings | 3 notes

* checking package dependencies ... NOTE
  Package suggested but not available for checking: 'qvalue'
  
**Maintainer**: The current Bioconductor release does not seem to be compiled against R-devel. This note does not appear when build against R-release.

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    R      3.4Mb
    doc    1.0Mb
    help   1.1Mb
    
**Maintainer**: The size of the R-directory is due to source code. Addressing the size of the R directory would require creating a set of mutually dependent  packages. This would likely result in a larger footprint across the packages.  
    
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

**Maintainer**: This note only appeared on the Windows server test. It didn't appear locally (Windows 10) or the other builds.


----------------------------------
Fedora Linux, R-devel, clang, gfortran:
0 errors | 0 warnings | 3 notes

* checking package dependencies ... NOTE
Packages suggested but not available for checking: 'fastglm', 'qvalue'

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    R      3.4Mb
    doc    1.0Mb
    help   1.1Mb
    
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable


**Maintainer**: I am unsure what causes this note. It doesn't appear locally, or for the other builds.



----------------------------------
Ubuntu Linux 20.04.1 LTS, R-release, GCC:
0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    R      3.5Mb
    doc    1.0Mb
    help   1.1Mb




## Downstream dependencies

There are currently no downstream dependencies for this package.
