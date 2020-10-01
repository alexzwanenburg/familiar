# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

# Create a dataset using the good dataset.
data <- familiar:::test.create_small_good_data_set("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::train(data=data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                              learner="cox",
                              hpo_metric="concordance_index",
                              vimp_aggregation_method="mean",
                              vimp_aggregation_rank_threshold=5)

#####familiarModel-based test###################################################
testthat::test_that("Conversion of single familiarModel to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=fam_model), "familiarEnsemble")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=list(fam_model, fam_model)), "familiarEnsemble")
})

testthat::test_that("Conversion of single familiarModel to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_model,
                                                       data=data,
                                                       compute_ensemble_ci=FALSE,
                                                       verbose=FALSE),
                            "familiarData")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=list(fam_model, fam_model),
                                                       data=data,
                                                       compute_ensemble_ci=FALSE,
                                                       verbose=FALSE),
                            "familiarData")
})

testthat::test_that("Conversion of single familiarModel to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_model,
                                                             data=data,
                                                             compute_ensemble_ci=FALSE,
                                                             verbose=FALSE),
                            "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=list(fam_model, fam_model),
                                                             data=data,
                                                             compute_ensemble_ci=FALSE,
                                                             verbose=FALSE),
                            "familiarCollection")
})

#####familiarEnsemble-based test################################################

# Create familiarEnsemble from fam_model objects.
fam_ensemble <- familiar::as_familiar_ensemble(object=list(fam_model, fam_model))

testthat::test_that("Conversion of familiarEnsemble to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=fam_ensemble), "familiarEnsemble")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_ensemble, fam_ensemble)))
})

testthat::test_that("Conversion of familiarEnsemble to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_ensemble,
                                                       data=data,
                                                       compute_ensemble_ci=FALSE,
                                                       verbose=FALSE),
                            "familiarData")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_ensemble, fam_ensemble), data=data, verbose=FALSE))
})

testthat::test_that("Conversion of familiarEnsemble to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_ensemble,
                                                             data=data,
                                                             compute_ensemble_ci=FALSE,
                                                             verbose=FALSE),
                            "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_ensemble, fam_ensemble, verbose=FALSE), data=data))
})


#####familiarData-based test####################################################

# Create familiarData from fam_model objects.
fam_data_1 <- familiar::as_familiar_data(object=list(fam_model, fam_model),
                                         compute_ensemble_ci=FALSE,
                                         data=data)
fam_data_2 <- familiar::as_familiar_data(object=list(fam_model, fam_model),
                                         compute_ensemble_ci=FALSE,
                                         data=data)

testthat::test_that("Conversion of familiarData to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=fam_data_1))
})

testthat::test_that("Conversion of multiple familiarData objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_data_1, fam_data_2)))
})

testthat::test_that("Conversion of familiarData to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_data_1), "familiarData")
})

testthat::test_that("Conversion of multiple familiarData objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_data_1, fam_data_2)))
})

testthat::test_that("Conversion of familiarData to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_data_1), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarData objects to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=list(fam_data_1, fam_data_2)), "familiarCollection")
})

testthat::test_that("Conversion of multiple identical familiarEnsemble objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_data_1, fam_data_1)), "familiarCollection")
})

#####familiarCollection-based test##############################################
# Create familiarCollection from fam_data objects.
fam_collection <- familiar::as_familiar_collection(object=list(fam_data_1, fam_data_2))

testthat::test_that("Conversion of familiarCollection to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=fam_collection))
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_collection, fam_collection)))
})

testthat::test_that("Conversion of familiarCollection to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=fam_collection))
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_collection, fam_collection)))
})

testthat::test_that("Conversion of familiarCollection to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_collection), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_collection, fam_collection)))
})

#####Mixed elements test########################################################
testthat::test_that("Conversion of mixed objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_model, fam_ensemble)))
})

testthat::test_that("Conversion of mixed objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_model, fam_ensemble)))
})

testthat::test_that("Conversion of mixed objects to to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_model, fam_ensemble)))
})

