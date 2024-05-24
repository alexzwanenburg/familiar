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
- [ ] code check: run `devtools::check(args=c("--no-examples", "--no-tests"), vignettes=FALSE)`, alternatively `rcmdcheck::rcmdcheck(args=c("--no-examples", "--no-tests"))`
- [ ] test check: run `devtools::test()`: set `options("testthat.progress.max_fails"=Inf)` and `options("Ncpus"=10)`
- [ ] create archives for testing backward compatibility against future versions: run `tests/old_experiments/archiveExperiments.R`
- [ ] test backward compatibility against archived experiments: manually run `tests/testthat/test-update_object.R`
- [ ] pre-compile vignettes: run `vignettes/compile.R`
- [ ] code and vignette check: `rcmdcheck::rcmdcheck(args=c("--no-examples", "--as-cran"))`
- [ ] check reverse dependencies
- [ ] create pull request
- [ ] check continuous integration tests
- [ ] Build source tarball using `devtools::build()`.
- [ ] Run `devtools::check_built(path=pkg, env_vars = list("_R_CHECK_DEPENDS_ONLY_" = TRUE))`, with pkg the path to the tarball.
- [ ] update cran_comments.md
- [ ] merge dev branch into master

**Post-merge**
- [ ] Check for merging errors.
  - [ ] Run a post-merge code check: `rcmdcheck::rcmdcheck(args=c("--no-examples", "--as-cran"))`
  - [ ] Run post-merge test suite, if required.
  - [ ] Pre-compile vignettes, if required.
  - [ ] Rebuild source tarball using `devtools::build()`, if required.

**CRAN**
- [ ] Check CRAN-policies on https://cran.r-project.org/web/packages/policies.html
- [ ] Upload source tarball to https://cran.r-project.org/submit.html
- [ ] Check CRAN checks

**Github release**
- [ ] prepare a GithHub release
  - [ ] copy news from NEWS.md
  - [ ] add source tarball as data
