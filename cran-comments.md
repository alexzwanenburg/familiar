Update familiar to version 1.1.0

Vignettes are pre-compiled to avoid long compilation times on build (several
minutes).

Longer tests and tests involving parallel processing are not performed on CRAN,
but are performed locally as part of the release process. Running the full test
suite takes several hours. Locally run unit and integrated tests did not produce
errors or (unexpected) warnings.

## R CMD check results

R CMD check was run on R-hub using rhub::check_for_cran(), producing the
following results:

Windows Server 2022, R-devel, 64 bit: 0 errors | 0 warnings | 2 notes

Ubuntu Linux 20.04.1 LTS, R-release, GCC: 0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    R      3.0Mb
    help   1.0Mb

  **Maintainer**: The size of the R-directory is due to source code. Addressing
  the size of the R directory would require creating a set of mutually dependent
  packages. This would likely result in a larger footprint across the packages.

* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
  'lastMiKTeXException'

  **Maintainer**: This note only appeared on the Windows server test. It didn't
  appear locally (Windows 10) or the Ubuntu Linux distribution.

## Downstream dependencies

There are currently no downstream dependencies for this package.
