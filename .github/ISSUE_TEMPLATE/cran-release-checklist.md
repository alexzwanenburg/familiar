---
name: CRAN release checklist
about: Prepare for CRAN release
title: Release CRAN version
labels: release checklist
assignees: ''

---

**Prior to merge with master**
- [ ] increment package version
- [ ] create release name
- [ ] update NEWS.md
- [ ] code check: run devtools::check(args=c("--no-examples", "--no-tests"), vignettes=FALSE)
- [ ] test check: run devtools::test(): set options("testthat.progress.max_fails"=Inf) and options("Ncpus"=10)
- [ ] create archives for testing backward compatibility against future versions: run tests/old_experiments/archiveExperiments.R
- [ ] test backward compatibility against archived experiments: manually run tests/testthat/test-update_object.R
- [ ] pre-compile vignettes: run vignettes/compile.R
- [ ] code and vignette check: run devtools::check(args=c("--no-examples", "--no-tests"))
- [ ] reverse dependencies: run revdepcheck::revdep_check()
- [ ] reverse dependencies: mail maintainers using revdepcheck::revdep_email()
- [ ] run check: rhub::check_for_cran()
- [ ] update cran_comments.md
- [ ] merge dev branch into master

**Post-merge**
- [ ] Check for merging errors.
  - [ ] Run a post-merge code check: devtools::check(env_vars = c("NOT_CRAN"="false"), args=c("--as-cran"))
  - [ ] Run post-merge test suite, if required.
  - [ ] Pre-compile vignettes, if required.

**CRAN**
- [ ] Build source tarball using devtools::build().
- [ ] Check CRAN-policies on https://cran.r-project.org/web/packages/policies.html
- [ ] Upload source tarball to https://cran.r-project.org/submit.html
- [ ] Check CRAN checks

**Github release**
- [ ] prepare a GithHub release
  - [ ] create source tarball using devtools::build and add as binary.
  - [ ] copy news from NEWS.md
