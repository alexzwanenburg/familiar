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
  testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == familiar:::get_n_features(data)), TRUE)
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
  testthat::expect_equal(all(new_object@hyperparameter_data$sign_size >= 1L &
                               new_object@hyperparameter_data$sign_size <= familiar:::get_n_features(data)), TRUE)
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
  testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2L), TRUE)
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
  testthat::expect_equal(all(new_object@hyperparameter_data$sign_size >= 2L & new_object@hyperparameter_data$sign_size <= 5L), TRUE)
  testthat::expect_equal(all(new_object@hyperparameter_data$sign_size %in% 2:5), TRUE)
  testthat::expect_equal(length(setdiff(unique(new_object@hyperparameter_data$sign_size), c(2, 5))) >= 1, TRUE)
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
  testthat::expect_setequal(unique(new_object@hyperparameter_data$sign_size), c(1, 4, 8))
})
