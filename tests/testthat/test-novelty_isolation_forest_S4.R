# Test if detectors are available.
familiar:::test_all_novelty_detectors_available(
  detectors = familiar:::.get_available_isolation_forest_detectors())

# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_all_novelty_detectors(
  detectors = familiar:::.get_available_isolation_forest_detectors())

familiar:::test_all_novelty_detectors_parallel(
  detectors = familiar:::.get_available_isolation_forest_detectors())
