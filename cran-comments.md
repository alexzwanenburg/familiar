This is a resubmission.

## Resubmission

* CRAN: Please omit the redundant "A Package for" from your title.
 
  **Maintainer**: Done.

* CRAN: Please do not start the description with "This package", package name, title or similar.

  **Maintainer**: Done. The description now starts with a sentence describing the package.
 
* CRAN: If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

  **Maintainer**: A manuscript is in preparation. Methods are detailed in the
  vignettes.

* CRAN: Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir().

  **Maintainer**: None of the functions in the package write to the user's filespace, unless explicitly indicated by the user. I removed a left-over file in vignettes used to pre-compile the vignettes prior to building.

* CRAN: Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.

  **Maintainer**: None of the examples, vignettes or tests use more than 2 cores.
  
* CRAN: Please do not set a seed to a specific number within a function. e.g. 
TestDataCreators.R

  **Maintainer**: set.seed was removed from all functions to avoid interfering with seeds set by the user.

## R CMD check results

Vignettes are pre-compiled to avoid long compilation times on build (several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN, but are performed locally as part of the release process. Running the full test
suite takes several hours. Locally run unit and integrated tests did not produce errors or (unexpected) warnings.

R CMD check was run on Rhub using rhub::check_for_cran(), producing the following results:

Windows Server 2022, R-devel, 64 bit: 0 errors | 0 warnings | 2 notes
Ubuntu Linux 20.04.1 LTS, R-release, GCC: 0 errors | 0 warnings | 2 notes
Fedora Linux, R-devel, clang, gfortran: 0 errors | 0 warnings | 2 notes

* NOTE: checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Alex Zwanenburg ’
  New submission

  **Maintainer**: Yes, this my first package.

* NOTE: checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    R     2.9Mb

  **Maintainer**: I checked for spurious data, but could not find any. The
  size of the R-directory is due to source code. Addressing the size of the R
  directory would require creating a set of mutually dependent packages. This
  would likely result in a larger footprint across the packages.
  
  Also, this note only appeared on the Ubuntu and Fedora builds.
  
* NOTE: checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
  **Maintainer**: This note only appeared on the Windows server test. It didn't
  appear locally (Windows 10) or the Ubunu Linux and Fedora Linux tests.

## Downstream dependencies

There are currently no downstream dependencies for this package.
