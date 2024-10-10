# Avoid warnings due to non-standard evaluation in data.table.
outcome <- outcome_time <- NULL

for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
  # Generate data.
  data <- familiar:::test_create_good_data(outcome_type = outcome_type)
  data@data[, "batch_id" := "A"]
  data_b <- familiar:::test_create_good_data(outcome_type = outcome_type)
  data_b@data[, "batch_id" := "B"]
  data_c <- familiar:::test_create_good_data(outcome_type = outcome_type)
  data_c@data[, "batch_id" := "C"]
  data@data <- rbind(data@data, data_b@data, data_c@data)
  
  # Introduce data with offset.
  data_offset <- data
  data_offset@data <- data.table::copy(data_offset@data)
  if (outcome_type == "continuous") {
    data_offset@data[batch_id == "A", "outcome" := outcome + 50.0]
    
  } else if (outcome_type %in% c("binomial", "multinomial")) {
    data_offset@data[batch_id == "A", "outcome" := "red"]
    
  } else if (outcome_type == "survival") {
    data_offset@data[batch_id == "A", "outcome_time" := outcome_time + 1000.0]
  }
  
  # Test results
  testthat::expect_no_condition(
    familiar:::.check_batch_normalisation_assumptions(
      data = data,
      normalisation_method = "combat"
    )
  )
  
  testthat::expect_warning(
    familiar:::.check_batch_normalisation_assumptions(
      data = data_offset,
      normalisation_method = "combat"
    ),
    class = "familiar_batch_outcome_difference"
  )
}
