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
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data("survival", to_data_object = FALSE))

# Survival probability.
predictions_surv <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "survival_probability")

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data("survival", to_data_object = FALSE))

# Risk stratification.
predictions_risk <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "risk_stratification")

testthat::test_that("Survival models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_surv), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_risk), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "survival"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "survival"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_surv, "survival"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_risk, "survival"), TRUE)
})

# Survival data with one instance ----------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("survival")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "survival", to_data_object = FALSE))

# Survival probability.
predictions_surv <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "survival_probability")

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "survival", to_data_object = FALSE))

# Risk stratification.
predictions_risk <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "risk_stratification")

testthat::test_that("Surival models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_surv), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_risk), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "survival"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "survival"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_surv, "survival"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_risk, "survival"), TRUE)
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
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "binomial", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "binomial", to_data_object = FALSE))

testthat::test_that("Binomial models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "binomial"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "binomial"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
})

# Binomial data with one instance ----------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("binomial")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "binomial", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "binomial", to_data_object = FALSE))

testthat::test_that("Binomial models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "binomial"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "binomial"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
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
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "multinomial", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "multinomial", to_data_object = FALSE))

testthat::test_that("Multinomial models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "multinomial"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "multinomial"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
})

# Multinomial data with one instance -------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("multinomial")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "multinomial", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "multinomial", to_data_object = FALSE))

testthat::test_that("Multinomial models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "multinomial"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "multinomial"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
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
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_good_data(
    "continuous", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_good_data(
    "continuous", to_data_object = FALSE))

testthat::test_that("Continuous models can predict using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "continuous"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "continuous"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
})

# Continuous data with one instance --------------------------------------------

# Create one-sample dataset.
data <- familiar:::test_create_one_sample_data("continuous")

# Default predictions using data object.
predictions_1 <- familiar::predict(
  object = fam_model,
  newdata = data)

# Default predictions using data.table.
predictions_2 <- familiar::predict(
  object = fam_model,
  newdata = familiar:::test_create_one_sample_data(
    "continuous", to_data_object = FALSE))

# Novelty.
predictions_novelty_1 <- familiar::predict(
  object = fam_model,
  newdata = data,
  type = "novelty")

# Novelty predictions using data.table.
predictions_novelty_2 <- familiar::predict(
  object = fam_model@novelty_detector,
  newdata = familiar:::test_create_one_sample_data(
    "continuous", to_data_object = FALSE))

testthat::test_that("Continuous models can predict single instances using external data.", {
  # Check that predictions 1 and 2 are equal.
  testthat::expect_equal(predictions_1, predictions_2, ignore_attr = TRUE)
  testthat::expect_equal(predictions_novelty_1, predictions_novelty_2, ignore_attr = TRUE)

  # Check that the number of rows is equal to the input data.
  testthat::expect_equal(nrow(predictions_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_2), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_1), nrow(data@data))
  testthat::expect_equal(nrow(predictions_novelty_2), nrow(data@data))

  # Check that all predictions are valid.
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_1, "continuous"), TRUE)
  testthat::expect_equal(familiar:::any_predictions_valid(predictions_2, "continuous"), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_1$novelty)), TRUE)
  testthat::expect_equal(all(is.finite(predictions_novelty_2$novelty)), TRUE)
})
