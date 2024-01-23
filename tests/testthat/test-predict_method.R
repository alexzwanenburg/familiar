# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

# Survival data with multiple instances ----------------------------------------

# Create a dataset using the good dataset.
data <- familiar:::test_create_good_data("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox",
  create_novelty_detector = TRUE
)

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data("survival", to_data_object = FALSE),
  .as_prediction_table = TRUE
)

# Survival probability.
predictions_surv <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "survival_probability",
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data("survival", to_data_object = FALSE),
  .as_prediction_table = TRUE
)

# Risk stratification.
predictions_risk <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "risk_stratification",
  .as_prediction_table = TRUE
)

testthat::test_that("Survival models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_surv@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_risk@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_surv))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_risk))
})

# Survival data with one instance ----------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("survival")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "survival", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Survival probability.
predictions_surv <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "survival_probability",
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "survival", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Risk stratification.
predictions_risk <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "risk_stratification",
  .as_prediction_table = TRUE
)

testthat::test_that("Surival models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_surv@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_risk@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_surv))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_risk))
})

# Binomial data with multiple instances ----------------------------------------

# Create a dataset using the good dataset.
data <- familiar:::test_create_good_data("binomial")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "glm_logistic",
  create_novelty_detector = TRUE
)

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "binomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "binomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Binomial models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
})

# Binomial data with one instance ----------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("binomial")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "binomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "binomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Binomial models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
})

# Multinomial data with multiple instances -------------------------------------

# Create a dataset using the good dataset.
data <- familiar:::test_create_good_data("multinomial")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "lasso_multinomial",
  create_novelty_detector = TRUE
)

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "multinomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "multinomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Multinomial models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
})

# Multinomial data with one instance -------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("multinomial")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "multinomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "multinomial", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Multinomial models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1$novelty))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2$novelty))
})

# Continuous data with multiple instances --------------------------------------

# Create a dataset using the good dataset.
data <- familiar:::test_create_good_data("continuous")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "lasso_gaussian",
  create_novelty_detector = TRUE
)

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "continuous", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "continuous", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Continuous models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2))
})

# Continuous data with one instance --------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("continuous")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  .as_prediction_table = TRUE
)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "continuous", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty",
  .as_prediction_table = TRUE
)

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "continuous", to_data_object = FALSE
  ),
  .as_prediction_table = TRUE
)

testthat::test_that("Continuous models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1@data), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2@data), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_true(familiar:::any_predictions_valid(predictions_1))
  testthat::expect_true(familiar:::any_predictions_valid(predictions_2))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_1$novelty))
  testthat::expect_true(familiar:::all_predictions_valid(predictions_novelty_2$novelty))
})
