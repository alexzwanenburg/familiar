# Test if detectors are available.
familiar:::test_all_novelty_detectors_available(detectors=familiar:::.get_available_none_detectors())

# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()

familiar:::test_all_novelty_detectors(detectors=familiar:::.get_available_none_detectors(),
                                      except_train=familiar:::.get_available_none_detectors(),
                                      can_trim=FALSE)
