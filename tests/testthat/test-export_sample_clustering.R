# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

# Default
results <- familiar:::test_export_specific(export_function=familiar:::export_sample_similarity,
                                           data_element="sample_similarity",
                                           sample_limit=20L,
                                           debug=FALSE)


testthat::test_that("Sample limit is correctly propagated", {
  
  for(data_set in results){
    
    # Get sample names.
    sample_names <- unique(c(data_set[[1]]@data$sample_1,
                             data_set[[1]]@data$sample_2))
    
    testthat::expect_equal(length(sample_names), 20)
  }
})


results <- familiar:::test_export_specific(export_function=familiar:::export_sample_similarity,
                                           data_element="sample_similarity",
                                           export_args=list("sample_limit"=20L),
                                           debug=FALSE)

testthat::test_that("Sample limit is correctly propagated", {
  
  for(data_set in results){
    
    # Get sample names.
    sample_names <- unique(c(data_set[[1]]@data$sample_1,
                             data_set[[1]]@data$sample_2))
    
    testthat::expect_equal(length(sample_names), 20)
  }
})
