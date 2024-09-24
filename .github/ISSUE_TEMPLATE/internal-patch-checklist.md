---
name: Internal patch checklist
about: Prepare for internal patches
title: Release patch version
labels: release checklist
assignees: ''

---

**Prior to merge with master**
- [ ] code check: run `devtools::check(args=c("--no-examples", "--no-tests"), vignettes=FALSE)`; alternatively `rcmdcheck::rcmdcheck(args=c("--no-examples", "--no-tests"))`
- [ ] test check: run `devtools::test()`: set options("testthat.progress.max_fails"=Inf) and options("Ncpus"=10)
- [ ] create archives for testing backward compatibility against future versions: run tests/old_experiments/archiveExperiments.R
- [ ] test backward compatibility against archived experiments: manually run tests/testthat/test-update_object.R
- [ ] increment minor package version
- [ ] create release name
- [ ] update NEWS.md
- [ ] create pull request
- [ ] check continuous integration tests
- [ ] merge patches into master

**Post-merge**
- [ ] Check for merging errors.
  - [ ] Run a post-merge code check: `rcmdcheck::rcmdcheck(args=c("--no-examples", "--as-cran"))`
  - [ ] Run post-merge test suite, if required.

**Github release**
- [ ] prepare a GithHub release
  - [ ] create source tarball using devtools::build and add as binary.
  - [ ] copy news from NEWS.md
