library(testthat)
library(familiar)

suppressWarnings(
  testthat::test_check("familiar"),
  classes = c("deprecation_warning")
)
