Update familiar to version 1.5.0

Vignettes are pre-compiled to avoid long compilation times on build 
(several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN,
but are performed locally as part of the release process. Running the full test 
suite takes several hours. Locally run unit and integrated tests did not produce
errors or (unexpected) warnings.



# R CMD check results

R CMD check was run on GitHub using 
https://github.com/alexzwanenburg/familiar/actions/workflows/auto-test-package_pull.yml

----------------------------------
windows-latest:
0 errors | 0 warnings | 0 notes

----------------------------------
macos-latest
0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    help   1.2Mb
    R      4.0Mb

----------------------------------
ubuntu-latest:
0 errors | 0 warnings | 0 notes



# R CMD check noSuggests

R CMD check (noSuggests) was run on GitHub using 
https://github.com/alexzwanenburg/familiar/actions/workflows/auto-test-no-suggests-pull.yml
(ubuntu-latest) and locally (windows-latest).

----------------------------------
windows-latest:

* checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
      R      3.5Mb
      help   1.2Mb

----------------------------------
ubuntu-latest:
0 errors | 2 warnings | 2 note

* checking package dependencies ... WARNING
  Warning: Skipping vignette re-building
  Packages suggested but not available for checking:
    'BART', 'cluster', 'CORElearn', 'coro', 'dynamicTreeCut', 'e1071',
    'Ecdat', 'fastcluster', 'fastglm', 'ggplot2', 'glmnet', 'gtable',
    'harmonicmeanp', 'isotree', 'knitr', 'labeling', 'laGP', 'MASS',
    'maxstat', 'mboost', 'microbenchmark', 'nnet', 'partykit',
    'power.transform', 'praznik', 'proxy', 'qvalue', 'randomForestSRC',
    'ranger', 'rmarkdown', 'scales', 'xml2', 'VGAM', 'xgboost'
  VignetteBuilder package required for checking but not installed: 'knitr'

* checking Rd cross-references ... NOTE
  Package unavailable to check Rd xrefs: 'ggplot2'

* checking files in 'vignettes' ... WARNING
  Warning: Files in the 'vignettes' directory but no files in 'inst/doc':
    'CC4_0_BY_88x31.png' 'familiar.svg' 'familiar_logo.html'
    'license.html' 'refs.bib'

* checking package vignettes ... NOTE
  Package has 'vignettes' subdirectory but apparently no vignettes.
  Perhaps the 'VignetteBuilder' information is missing from the
  DESCRIPTION file?


AZ: Warnings and notes are directly due to missing packages (noSuggests).



# Downstream dependencies

There are currently no downstream dependencies for this package.
