# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()

verbose <- FALSE

##### Test acquisition functions for all hyperparameter learners ###############
for(hyperparameter_learner in familiar:::.get_available_hyperparameter_learners()){
  for(acquisition_function in familiar:::.get_available_acquisition_functions()){
    familiar:::test_hyperparameter_optimisation(learners="glm_logistic",
                                                outcome_type_available="binomial",
                                                acquisition_function=acquisition_function,
                                                hyperparameter_learner=hyperparameter_learner,
                                                debug=FALSE,
                                                parallel=FALSE)
  }
}


##### Test optimisation functions for one metric ###############################
for(optimisation_function in familiar:::.get_available_optimisation_functions()){
  familiar:::test_hyperparameter_optimisation(learners="glm_logistic",
                                              outcome_type_available="binomial",
                                              optimisation_function=optimisation_function,
                                              debug=FALSE,
                                              parallel=FALSE)
}

##### Test optimisation functions for multiple metrics #########################
for(optimisation_function in familiar:::.get_available_optimisation_functions()){
  familiar:::test_hyperparameter_optimisation(learners="glm_logistic",
                                              outcome_type_available="binomial",
                                              optimisation_function=optimisation_function,
                                              metric=c("auc", "brier", "balanced_accuracy"),
                                              debug=FALSE,
                                              parallel=FALSE)
}


##### Test hyperparameter learners for learner with only one hyperparameter. #######
for(hyperparameter_learner in familiar:::.get_available_hyperparameter_learners()){
  familiar:::test_hyperparameter_optimisation(learners="cox",
                                              outcome_type_available="survival",
                                              hyperparameter_learner=hyperparameter_learner,
                                              debug=FALSE,
                                              parallel=FALSE)
}


##### Test without measuring time ##############################################
familiar:::test_hyperparameter_optimisation(learners="glm_logistic",
                                            outcome_type_available="binomial",
                                            measure_time=FALSE,
                                            debug=FALSE,
                                            parallel=FALSE)


# Create dataset.
data <- familiar:::test.create_good_data_set(outcome_type="binomial")

##### Test that "none" feature selection keeps all features. ###################

# Create object.
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="none",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=FALSE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=3L,
                                                  n_max_intensify_steps=2L,
                                                  n_random_sets=20L,
                                                  n_challengers=10L,
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

testthat::test_that("Test that \"none\" feature selection keeps all features.",{
  testthat::expect_equal(all(new_object@hyperparameter_data$parameter_table$sign_size == familiar:::get_n_features(data)), TRUE)
})



##### Test that "random" feature selection can select up to the maximum number of features. #################
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="random",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=FALSE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=3L,
                                                  n_max_intensify_steps=2L,
                                                  n_random_sets=20L,
                                                  n_challengers=10L,
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

testthat::test_that("Test that \"random\" feature selection can select up to the maximum number of features.",{
  testthat::expect_equal(all(new_object@hyperparameter_data$parameter_table$sign_size >= 1L &
                               new_object@hyperparameter_data$parameter_table$sign_size <= familiar:::get_n_features(data)), TRUE)
})



##### Test that "signature_only" keeps only signature features. ################
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="signature_only",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=TRUE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=3L,
                                                  n_max_intensify_steps=2L,
                                                  n_random_sets=20L,
                                                  n_challengers=10L,
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.",{
  testthat::expect_equal(all(new_object@hyperparameter_data$parameter_table$sign_size == 2L), TRUE)
})



##### Test that a range of signature sizes can be provided. ####################
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="mim",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=TRUE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  user_list=list("sign_size"=c(2,5)),
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=3L,
                                                  n_max_intensify_steps=2L,
                                                  n_random_sets=20L,
                                                  n_challengers=10L,
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.",{
  testthat::expect_equal(all(new_object@hyperparameter_data$parameter_table$sign_size >= 2L &
                               new_object@hyperparameter_data$parameter_table$sign_size <= 5L), TRUE)
  testthat::expect_equal(all(new_object@hyperparameter_data$parameter_table$sign_size %in% 2:5), TRUE)
  testthat::expect_equal(length(setdiff(unique(new_object@hyperparameter_data$parameter_table$sign_size), c(2, 5))) >= 1, TRUE)
})


##### Test that a range of signature sizes can be provided. ####################
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="mim",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=TRUE)

# Hyperparameter optimisation.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  user_list=list("sign_size"=c(1, 4, 8)),
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=3L,
                                                  n_max_intensify_steps=2L,
                                                  n_random_sets=20L,
                                                  n_challengers=10L,
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

testthat::test_that("Test that \"signature_only\" feature selection keeps only signature features.",{
  testthat::expect_setequal(unique(new_object@hyperparameter_data$parameter_table$sign_size), c(1, 4, 8))
})



##### Test exploration methods #################################################

# Create dataset.
data <- familiar:::test.create_good_data_set(outcome_type="binomial")

# Create object.
object <- familiar:::.test_create_hyperparameter_object(data=data,
                                                        vimp_method="mim",
                                                        learner="elastic_net",
                                                        is_vimp=FALSE,
                                                        set_signature_feature=FALSE)

# Hyperparameter optimisation without pruning.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=1L,
                                                  n_max_intensify_steps=4L,
                                                  n_intensify_step_bootstraps=1L,
                                                  n_random_sets=16L,
                                                  n_challengers=10L,
                                                  exploration_method="none",
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

# Set expected range of rows. The lowest boundary occurs when the incumbent does
# not change during intensification, whereas the upper boundary is expected when
# it does.
expected_rows_lower <- 2 * 16 + 10 * 4 * 2
expected_rows_upper <- expected_rows_lower + 2 * 4

testthat::test_that("Test that \"none\" exploration method does not prune any hyperparameter sets during intensification",{
  testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
  testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
})


# Hyperparameter optimisation using successive_halving for pruning. Note that
# n_max_intensify_steps is 5, but only 4 will be steps are possible. Just as a
# test.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=1L,
                                                  n_max_intensify_steps=5L,
                                                  n_intensify_step_bootstraps=1L,
                                                  n_random_sets=16L,
                                                  n_challengers=10L,
                                                  exploration_method="successive_halving",
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

# Set expected range of rows. 10 initial challengers decrease to 5, 2 and 1 in
# subsequent rounds. The lowest boundary occurs when the incumbent does not
# change during intensification, whereas the upper boundary is expected when it
# does.
expected_rows_lower <- 2 * 16 + 10 * 2 + 5 * 2 + 2 * 2 + 1 * 2
expected_rows_upper <- expected_rows_lower + 2 * 4

testthat::test_that("Test that \"successive_halving\" exploration method may prune any hyperparameter sets during intensification",{
  testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
  testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
})


# Hyperparameter optimisation using stochastic_reject for pruning.
new_object <- familiar:::optimise_hyperparameters(object=object,
                                                  data=data,
                                                  n_max_bootstraps=25L,
                                                  n_max_optimisation_steps=1L,
                                                  n_max_intensify_steps=4L,
                                                  n_intensify_step_bootstraps=5L,
                                                  n_random_sets=16L,
                                                  n_challengers=10L,
                                                  exploration_method="stochastic_reject",
                                                  is_vimp=FALSE,
                                                  verbose=verbose)

# Set expected range of rows. The lowest boundary occurs when all challengers
# are rejected after one round. The upper boundary occurs when no challengers
# are rejected at all, and the incumbent changes during intensification.
expected_rows_lower <- 2 * 16 * 5 + 10 * 2 * 5
expected_rows_upper <- 2 * 16 * 5 + 4 * 10 * 2 * 5 + 2 * 4 * 5

testthat::test_that("Test that \"stochastic_reject\" exploration method may prune any hyperparameter sets during intensification",{
  testthat::expect_lte(nrow(new_object@hyperparameter_data$score_table), expected_rows_upper)
  testthat::expect_gte(nrow(new_object@hyperparameter_data$score_table), expected_rows_lower)
})
