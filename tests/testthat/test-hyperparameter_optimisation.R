# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()

verbose <- FALSE

# Test acquisition functions for all hyperparameter learners -------------------
for (hyperparameter_learner in familiar:::.get_available_hyperparameter_learners()) {
  for (acquisition_function in familiar:::.get_available_acquisition_functions()) {
    familiar:::test_hyperparameter_optimisation(
      learners = "glm_logistic",
      outcome_type_available = "binomial",
      acquisition_function = acquisition_function,
      hyperparameter_learner = hyperparameter_learner,
      debug = FALSE,
      parallel = FALSE)
  }
}

# Test optimisation functions for one metric -----------------------------------
for (optimisation_function in familiar:::.get_available_optimisation_functions()) {
  familiar:::test_hyperparameter_optimisation(
    learners = "glm_logistic",
    outcome_type_available = "binomial",
    optimisation_function = optimisation_function,
    debug = FALSE,
    parallel = FALSE)
}

# Test optimisation functions for multiple metrics -----------------------------
for (optimisation_function in familiar:::.get_available_optimisation_functions()) {
  familiar:::test_hyperparameter_optimisation(
    learners = "glm_logistic",
    outcome_type_available = "binomial",
    optimisation_function = optimisation_function,
    metric = c("auc", "brier", "balanced_accuracy"),
    debug = FALSE,
    parallel = FALSE)
}

# Test hyperparameter learners for learner with only one hyperparameter --------
for (hyperparameter_learner in familiar:::.get_available_hyperparameter_learners()) {
  familiar:::test_hyperparameter_optimisation(
    learners = "cox",
    outcome_type_available = "survival",
    hyperparameter_learner = hyperparameter_learner,
    debug = FALSE,
    parallel = FALSE)
}


# Test without measuring time --------------------------------------------------
familiar:::test_hyperparameter_optimisation(
  learners = "glm_logistic",
  outcome_type_available = "binomial",
  measure_time = FALSE,
  debug = FALSE,
  parallel = FALSE)

# Create dataset.
data <- familiar:::test_create_good_data(outcome_type = "binomial")

# Test that "none" feature selection keeps all features ------------------------

# Create object.
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "none",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = FALSE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 3L,
  n_max_intensify_steps = 2L,
  n_random_sets = 20L,
  n_challengers = 10L,
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Test that \"none\" feature selection keeps all features.", {
  testthat::expect_equal(
    all(new_object@hyperparameter_data$parameter_table$sign_size == familiar:::get_n_features(data)),
    TRUE)
})



# Test that "random" feature selection can select up to the maximum number of features ------
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "random",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = FALSE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 3L,
  n_max_intensify_steps = 2L,
  n_random_sets = 20L,
  n_challengers = 10L,
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Test that \"random\" feature selection can select up to the maximum number of features.", {
  testthat::expect_equal(
    all(new_object@hyperparameter_data$parameter_table$sign_size >= 1L &
          new_object@hyperparameter_data$parameter_table$sign_size <= familiar:::get_n_features(data)),
    TRUE)
})


# Test that "signature_only" keeps only signature features ---------------------
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "signature_only",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = TRUE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 3L,
  n_max_intensify_steps = 2L,
  n_random_sets = 20L,
  n_challengers = 10L,
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.", {
  testthat::expect_equal(
    all(new_object@hyperparameter_data$parameter_table$sign_size == 2L),
    TRUE)
})


# Test that a range of signature sizes can be provided -------------------------
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "mim",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = TRUE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  user_list = list("sign_size" = c(2, 5)),
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 3L,
  n_max_intensify_steps = 2L,
  n_random_sets = 20L,
  n_challengers = 10L,
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.", {
  testthat::expect_equal(
    all(new_object@hyperparameter_data$parameter_table$sign_size >= 2L &
          new_object@hyperparameter_data$parameter_table$sign_size <= 5L),
    TRUE)
  testthat::expect_equal(
    all(new_object@hyperparameter_data$parameter_table$sign_size %in% 2:5),
    TRUE)
  testthat::expect_equal(
    length(setdiff(unique(new_object@hyperparameter_data$parameter_table$sign_size), c(2, 5))) >= 1,
    TRUE)
})


# Test that a range of signature sizes can be provided -------------------------
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "mim",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = FALSE)


# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  user_list = list("sign_size" = c(1, 4, 6)),
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 3L,
  n_max_intensify_steps = 2L,
  n_random_sets = 20L,
  n_challengers = 10L,
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.", {
  testthat::expect_setequal(
    unique(new_object@hyperparameter_data$parameter_table$sign_size),
    c(1, 4, 6))
})


# Test exploration methods -----------------------------------------------------

# Create dataset.
data <- familiar:::test_create_good_data(outcome_type = "binomial")

# Create object.
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "mim",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = FALSE)

# Hyperparameter optimisation without pruning.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 4L,
  n_intensify_step_bootstraps = 1L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "none",
  is_vimp = FALSE,
  verbose = verbose)

# Set expected range of rows. Upper and lower boundary are the same, as all runs
# are executed simultaneously.
expected_rows_lower <- expected_rows_upper <- (16 + 10 * 4 + 1 * 4) * 2

testthat::test_that(paste0(
  "Test that \"none\" exploration method does not prune any hyperparameter sets ",
  "during intensification"), {
    testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
    testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
  }
)


# Hyperparameter optimisation using successive_halving for pruning. Note that
# n_max_intensify_steps is 5, but only 4 will be steps are possible. Just as a
# test.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 5L,
  n_intensify_step_bootstraps = 1L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "successive_halving",
  is_vimp = FALSE,
  verbose = verbose)

# Set expected range of rows. 10 initial challengers decrease to 5, 2 and 1 in
# subsequent rounds. Upper and lower boundary are the same because here
# n_intensify_step_bootstraps = 1, and only one new run will be assessed for
# each parameter set.
expected_rows_lower <- expected_rows_upper <- (16 + 10 + 5 + 2 + 1 + 4) * 2

testthat::test_that(paste0(
  "Test that \"successive_halving\" exploration method may prune any ",
  "hyperparameter sets during intensification"), {
    testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
    testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
  }
)

# Hyperparameter optimisation using stochastic_reject for pruning.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 4L,
  n_initial_bootstraps = 2L,
  n_intensify_step_bootstraps = 5L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "stochastic_reject",
  is_vimp = FALSE,
  verbose = verbose)

# Set expected range of rows. The lowest boundary occurs when all challengers
# are rejected after one round, and only one new run is sampled. The upper
# boundary occurs when no challengers are rejected at all and 5 new runs are
# sampled.
expected_rows_lower <- (16 * 2 + 10 + 1) * 2 # initial + step 1 + incumbent
expected_rows_upper <- (16 * 2 + 10 * 5 * 4 + 1 * 5 * 4) * 2 # initial + steps 1-4 + incumbent

testthat::test_that(paste0(
  "Test that \"stochastic_reject\" exploration method may prune any ",
  "hyperparameter sets during intensification"), {
    testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
    testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
  }
)

# Single-shot hyperparameter optimisation. Note that n_intensify_step_bootstraps
# and n_max_intensify_steps should be set to 1L internally.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 4L,
  n_intensify_step_bootstraps = 5L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "single_shot",
  is_vimp = FALSE,
  verbose = verbose)

# Set expected range of rows. Upper and lower boundary are the same, as all runs
# are executed simultaneously.
expected_rows_lower <- expected_rows_upper <- (16 + 10 * 1 + 1 * 1) * 2

testthat::test_that(paste0(
  "Test that \"single_shot\" exploration method does not prune any ",
  "hyperparameter sets during intensification"), {
    testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
    testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
  }
)


# Test time truncation ---------------------------------------------------------

# Create dataset.
data <- familiar:::test_create_good_data(outcome_type = "binomial")

# Create object.
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "mim",
  learner = "elastic_net",
  is_vimp = FALSE,
  set_signature_feature = FALSE)

# Hyperparameter optimisation without pruning and marginal time limit. This
# should just complete the initial step.
new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  time_limit = 0.000001,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 4L,
  n_intensify_step_bootstraps = 1L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "none",
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("Time limits are respected and only the initial bootstraps are run.", {
  testthat::expect_gte(new_object@hyperparameter_data$time_taken, 0.000001)
  testthat::expect_equal(all(new_object@hyperparameter_data$score_table$iteration_id == 0), TRUE)
})



# Test that clustered data are correctly handled -------------------------------
# Create data,
data <- familiar:::test_create_synthetic_correlated_data(
  outcome_type = "continuous",
  n_numeric = 4,
  cluster_size = c(3, 3, 3, 3))

# Create object.
object <- familiar:::.test_create_hyperparameter_object(
  data = data,
  vimp_method = "mim",
  learner = "elastic_net",
  is_vimp = FALSE,
  cluster_method = "hclust",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.90,
  set_signature_feature = FALSE)

new_object <- familiar:::optimise_hyperparameters(
  object = object,
  data = data,
  n_max_bootstraps = 25L,
  n_max_optimisation_steps = 1L,
  n_max_intensify_steps = 5L,
  n_intensify_step_bootstraps = 1L,
  n_random_sets = 16L,
  n_challengers = 10L,
  exploration_method = "successive_halving",
  is_vimp = FALSE,
  verbose = verbose)

testthat::test_that("One to four features are assessed for clustered features.", {
  testthat::expect(
    all(new_object@hyperparameter_data$parameter_table$sign_size >= 1 &
          new_object@hyperparameter_data$parameter_table$sign_size <= 4),
    TRUE)
  testthat::expect(any(new_object@hyperparameter_data$parameter_table$sign_size == 1), TRUE)
  testthat::expect(any(new_object@hyperparameter_data$parameter_table$sign_size == 4), TRUE)
})
