# Default
results <- familiar:::test_export_specific(export_function=familiar:::export_ice_data,
                                           data_element="ice_data",
                                           sample_limit=20L,
                                           create_novelty_detector=FALSE,
                                           debug=TRUE)


testthat::test_that("Sample limit is correctly propagated", {
  
  for(data_set in results){
    
    # Get sample names.
    sample_names <- unique(c(data_set[[1]]@data$sample))
    
    testthat::expect_equal(length(sample_names), 20)
  }
})


# Test that test_export_specific works when specifying a single feature.
results <- familiar:::test_export_specific(export_function=familiar:::export_ice_data,
                                           outcome_type_available="survival",
                                           data_element="ice_data",
                                           features=c("nodes"),
                                           sample_limit=20L,
                                           create_novelty_detector=FALSE,
                                           debug=TRUE)


testthat::test_that("A single features can be specified.", {
  
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_x, "nodes")
  testthat::expect_equal(length(results$survival), 1L)
})


# Test that test_export_specific works when specifying two features.
results <- familiar:::test_export_specific(export_function=familiar:::export_ice_data,
                                           outcome_type_available="survival",
                                           data_element="ice_data",
                                           features=c("rx", "nodes"),
                                           sample_limit=20L,
                                           create_novelty_detector=FALSE,
                                           debug=TRUE)


testthat::test_that("Two features can be specified.", {
  
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_x, "rx")
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_y, "nodes")
  testthat::expect_equal(length(results$survival), 1L)
})
