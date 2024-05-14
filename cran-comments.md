Update familiar to version 1.4.7

Vignettes are pre-compiled to avoid long compilation times on build (several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN, but are performed locally as part of the release process. Running the full test suite takes several hours. Locally run unit and integrated tests did not produce errors or (unexpected) warnings.



## R CMD check results

R CMD check was run on GitHub against R-release using r-lib/actions/check-r-package:

----------------------------------
window-latest:
0 errors | 0 warnings | 0 notes

----------------------------------
macos-latest
0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    help   1.2Mb
    R      4.1Mb

----------------------------------
ubuntu-latest:
0 errors | 0 warnings | 0 notes



## Downstream dependencies

There are currently no downstream dependencies for this package.
