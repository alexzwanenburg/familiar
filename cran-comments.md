This is the first release of the package.

Vignettes are pre-compiled to avoid long compilation times on build (several
minutes).

Longer tests and tests involving parallel processing are not performed on CRAN,
but are performed locally as part of the release process. Running the full test
suite takes several hours. Locally run unit and integrated tests did not produce
errors or (unexpected) warnings.

R CMD check was run on Rhub using rhub::check_for_cran().

## R CMD check results

Windows Server 2022, R-devel, 64 bit: 0 errors | 0 warnings | 3 notes
Ubuntu Linux 20.04.1 LTS, R-release, GCC: 0 errors | 0 warnings | 2 notes
Fedora Linux, R-devel, clang, gfortran: 0 errors | 0 warnings | 2 notes

* NOTE: Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1177/001316446002000104
    From: inst/doc/performance_metrics_precompiled.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0272989X06295361
    From: inst/doc/evaluation_and_explanation_precompiled.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.7326/0003-4819-122-5-199503010-00001
    From: inst/doc/evaluation_and_explanation_precompiled.html
    Status: 503
    Message: Service Unavailable

  **Maintainer**: I manually checked the flagged URL. These DOI refer to the
  proper papers, and can be accessed.

* NOTE: checking installed package size ... NOTE
  installed size is  5.0Mb
  sub-directories of 1Mb or more:
    R     2.9Mb
    doc   1.0Mb
    
  **Maintainer**: I checked for spurious data, but could not find any. The
  size of the R-directory is due to source code. Addressing the size of the R
  directory would require creating a set of mutually dependent packages. This
  would likely result in a larger footprint across the packages.
  
  The (six) HTML vignettes contain images and math.

* NOTE: checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
  **Maintainer**: This note only appeared on the Windows server test. It didn't
  appear locally (Windows 10) or the Ubunu Linux and Fedora Linux tests.

## Downstream dependencies

There are currently no downstream dependencies for this package.
