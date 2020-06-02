library(data.table)

# Load colon data.
data <- as.data.table(survival::colon)

# Recurrence
data <- data[etype == 1]

# Keep only first 100 samples for speed and only id, nodes, rx, extent and outcome.
data_1 <- data[1:100, c("id", "nodes", "rx", "time", "status")]

# Convert data to dataObject.

fam_model <- familiar:::train(data=data_1,
                              sample_id_column="id",
                              outcome_column=c("time", "status"),
                              outcome_type="survival",
                              cluster_method="none",
                              hyperparameter_list=list("sign_size"=2),
                              learner="cox")

testthat::test_that("Cox model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(fam_model), TRUE)
  
  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(fam_model), TRUE)
})

testthat::test_that("Cox model has variable importance", {
  testthat::expect_success(familiar:::..vimp(fam_model))
  testthat::expect_equal(nrow(familiar:::..vimp(fam_model)), 2)
})


testthat::test_that("Cox model can predict", {
  prediction_table <- familiar:::.predict(fam_model, data=data_1)
})
