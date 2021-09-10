testthat::skip_on_cran()

# Train a model to transfer settings.
data <- familiar:::test.create_good_data_set("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(data=data,
                                   cluster_method="none",
                                   imputation_method="simple",
                                   hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                   learner="cox",
                                   create_novelty_detector=TRUE)

##### Feature levels are correctly ordered -------------------------------------
data <- familiar:::test.create_good_data_set("survival", to_data_object=FALSE)

# Change order of features.
data$rx <- factor(data$rx, levels=c("Lev", "Lev+5FU", "Obs"))

testthat::test_that("Feature levels are correctly ordered", {
  for(strictness in c("strict", "external_warn", "external")){
    parsed_data <- familiar::as_data_object(data=data.table::copy(data),
                                            object=fam_model,
                                            check_stringency=strictness)
    
    testthat::expect_equal(levels(parsed_data@data$rx), c("Obs", "Lev", "Lev+5FU"))
  }  
})


data <- familiar:::test.create_good_data_set("survival", to_data_object=FALSE)

# Change order of features.
data$rx <- as.character(data$rx)

testthat::test_that("Feature levels are correctly set", {
  for(strictness in c("strict", "external_warn", "external")){
    parsed_data <- familiar::as_data_object(data=data.table::copy(data),
                                            object=fam_model,
                                            check_stringency=strictness)
    
    testthat::expect_equal(levels(parsed_data@data$rx), c("Obs", "Lev", "Lev+5FU"))
  }  
})



##### Ordered features are correctly set ---------------------------------------


##### Missing levels in categorical features -----------------------------------


##### Additional levels in categorical features --------------------------------


##### Censoring and event identifiers are set ----------------------------------


##### Manual censoring and event identifiers -----------------------------------


##### Unknown censoring and event identifiers ----------------------------------


##### Class levels are correctly ordered ---------------------------------------


##### Missing class levels -----------------------------------------------------


##### Additional class levels --------------------------------------------------
