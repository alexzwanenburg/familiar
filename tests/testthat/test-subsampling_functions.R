set.seed(1844)

##### Cross-validation #########################################################

for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, rare_outcome=TRUE)
  n_rep <- 3L
  n_folds <- 3L
  
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    available_stratify_options <- c(FALSE, TRUE)
    
  } else {
    available_stratify_options <- FALSE
  }
  
  for(stratify in available_stratify_options){
    
    testthat::test_that(paste0("Cross-validation ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " functions correctly."), {
                                 
                                 # Create subsample.
                                 subsample_data <- familiar:::.create_cv(data=data@data,
                                                                         n_folds=n_folds,
                                                                         stratify=stratify,
                                                                         outcome_type=outcome_type)
                                 
                                 # Check that none of the training folds are the same.
                                 for(ii in 1:(length(subsample_data$train_list) - 1)){
                                   for(jj in (ii+1):length(subsample_data$train_list)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data$train_list[[ii]],
                                                                                  subsample_data$train_list[[jj]]),
                                                            FALSE)
                                   }
                                 }
                                 
                                 for(ii in seq_along(subsample_data$train_list)){
                                   # Check that there is no overlap between training folds and the validation fold.
                                   testthat::expect_equal(nrow(data.table::fintersect(unique(subsample_data$train_list[[ii]]),
                                                                                      unique(subsample_data$valid_list[[ii]]))),
                                                          0L)
                                   
                                   # Check that the union of training and validation folds is the input dataset.
                                   testthat::expect_equal(data.table::fsetequal(unique(rbind(subsample_data$train_list[[ii]], subsample_data$valid_list[[ii]])),
                                                                                unique(data@data[, mget(familiar:::get_id_columns(id_depth="series"))])),
                                                          TRUE)
                                   
                                   # Assert that all samples in the training and validation folds are unique (not
                                   # duplicated).
                                   testthat::expect_equal(anyDuplicated(subsample_data$train_list[[ii]]), 0)
                                   testthat::expect_equal(anyDuplicated(subsample_data$valid_list[[ii]]), 0)
                                   
                                   # Check that sampling creates a dataset identical to the development subsample.
                                   train_data <- familiar:::select_data_from_samples(data=data,
                                                                                     samples=subsample_data$train_list[[ii]])
                                   
                                   # Test that the samples and series are selected.
                                   testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                                subsample_data$train_list[[ii]]),
                                                          TRUE)
                                   
                                   # Test that repetitions are likewise selected.
                                   testthat::expect_equal(nrow(train_data@data),
                                                          n_rep * nrow(subsample_data$train_list[[ii]]))
                                   
                                   # Test that the size of the training folds is about (n_folds - 1) / n_folds of
                                   # the complete set.
                                   testthat::expect_gte(nrow(train_data@data), nrow(data@data) * (n_folds - 1) / n_folds - 9)
                                   testthat::expect_lte(nrow(train_data@data), nrow(data@data) * (n_folds - 1) / n_folds + 9)
                                   
                                   # Check that sampling creates a dataset identical to the validation subsample.
                                   validation_data <- familiar:::select_data_from_samples(data=data,
                                                                                          samples=subsample_data$valid_list[[ii]])
                                   
                                   # Test that the samples and series are selected.
                                   testthat::expect_equal(data.table::fsetequal(validation_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                                subsample_data$valid_list[[ii]]),
                                                          TRUE)
                                   
                                   # Test that repetitions are likewise selected.
                                   testthat::expect_equal(nrow(validation_data@data),
                                                          n_rep * nrow(subsample_data$valid_list[[ii]]))
                                   
                                   # Test that the size of the validation fold is about 1 / n_folds of
                                   # the complete set.
                                   testthat::expect_gte(nrow(validation_data@data), nrow(data@data) * 1 / n_folds - 9)
                                   testthat::expect_lte(nrow(validation_data@data), nrow(data@data) * 1 / n_folds + 9)
                                   
                                   # Assert that data are correctly stratified.
                                   if(stratify & outcome_type %in% c("binomial", "multinomial")){
                                     # Determine the frequency of outcome classes in the original dataset.
                                     input_frequency <- data@data[, list("frequency_original"=.N / nrow(data@data)), by="outcome"]
                                     train_frequency <- train_data@data[, list("frequency_bootstrap"=.N / nrow(train_data@data)), by="outcome"]
                                     
                                     # Update the frequency table.
                                     frequency_table <- merge(x=input_frequency,
                                                              y=train_frequency,
                                                              by="outcome")
                                     
                                     # Check that the data is correctly stratified.
                                     frequency_table[, "similar":=data.table::between(frequency_bootstrap,
                                                                                      lower=frequency_original-0.05,
                                                                                      upper=frequency_original+0.05)]
                                     
                                     testthat::expect_equal(all(frequency_table$similar), TRUE)
                                     
                                   } else if(stratify & outcome_type %in% c("survival")){
                                     # Determine the frequency of censored data points and events in classes in
                                     # the original dataset.
                                     input_frequency <- data@data[, list("frequency_original"=.N / nrow(data@data)), by="outcome_event"]
                                     train_frequency <- train_data@data[, list("frequency_bootstrap"=.N / nrow(train_data@data)), by="outcome_event"]
                                     
                                     # Update the frequency table.
                                     frequency_table <- merge(x=input_frequency,
                                                              y=train_frequency,
                                                              by="outcome_event")
                                     
                                     # Check that the data is correctly stratified.
                                     frequency_table[, "similar":=data.table::between(frequency_bootstrap,
                                                                                      lower=frequency_original-0.05,
                                                                                      upper=frequency_original+0.05)]
                                     
                                     testthat::expect_equal(all(frequency_table$similar), TRUE)
                                   }
                                   
                                   # Check that the rare outcome is found in the
                                   # training data. This prevent issues with
                                   # training data.
                                   if(outcome_type == "multinomial"){
                                     testthat::expect_equal(nrow(train_data@data[outcome == "3"]) > 0, TRUE)
                                   }
                                   
                                   # Assert that all outcome levels in the
                                   # validation folds also appear in the
                                   # training folds.
                                   if(outcome_type %in% c("binomial", "multinomial", "survival")){
                                     testthat::expect_equal(length(setdiff(unique(validation_data@data$outcome),
                                                                           unique(train_data@data$outcome))),
                                                            0)
                                   }
                                 }
                               })
  }  
}



##### Bootstraps ###############################################################
for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, rare_outcome=TRUE)
  n_rep <- 3L
  
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    available_stratify_options <- c(FALSE, TRUE)
    
  } else {
    available_stratify_options <- FALSE
  }
  
  for(stratify in available_stratify_options){
    
    testthat::test_that(paste0("Bootstrap resampling ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " functions correctly."), {
                                 
                                 # Create subsample.
                                 subsample_data <- familiar:::.create_bootstraps(data=data@data,
                                                                                 n_iter=20,
                                                                                 stratify=stratify,
                                                                                 outcome_type=outcome_type)
                                 
                                 # Check that none of in-bag datasets is the same.
                                 for(ii in 1:(length(subsample_data$train_list) - 1)){
                                   for(jj in (ii+1):length(subsample_data$train_list)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data$train_list[[ii]],
                                                                                  subsample_data$train_list[[jj]]),
                                                            FALSE)
                                   }
                                 }
                                 
                                 
                                 for(ii in seq_along(subsample_data$train_list)){
                                   # Check that there is no overlap between in-bag and out-of-bag data.
                                   testthat::expect_equal(nrow(data.table::fintersect(unique(subsample_data$train_list[[ii]]),
                                                                                      unique(subsample_data$valid_list[[ii]]))),
                                                          0L)
                                   
                                   # Check that the combination of in-bag and out-of-bag data is the same as
                                   # the input dataset.
                                   testthat::expect_equal(data.table::fsetequal(unique(rbind(subsample_data$train_list[[ii]], subsample_data$valid_list[[ii]])),
                                                                                unique(data@data[, mget(familiar:::get_id_columns(id_depth="series"))])),
                                                          TRUE)
                                   
                                   # Check that sampling creates a dataset identical to the development dataset.
                                   train_data <- familiar:::select_data_from_samples(data=data,
                                                                                     samples=subsample_data$train_list[[ii]])
                                   
                                   # Test that the samples and series are selected.
                                   testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                                subsample_data$train_list[[ii]]),
                                                          TRUE)
                                   
                                   # Test that repetitions are likewise selected.
                                   testthat::expect_equal(nrow(train_data@data),
                                                          n_rep * nrow(subsample_data$train_list[[ii]]))
                                   
                                   # Test that the subsample has the same size
                                   # (or is slightly larger) than the original
                                   # dataset. It can be slightly larger because
                                   # samples with rare outcomes are added to the
                                   # in-bag data.
                                   testthat::expect_gte(nrow(train_data@data), nrow(data@data) - 30)
                                   testthat::expect_lte(nrow(train_data@data), nrow(data@data) + 9)
                                   
                                   # Check that sampling creates a dataset identical to the validation dataset.
                                   validation_data <- familiar:::select_data_from_samples(data=data,
                                                                                          samples=subsample_data$valid_list[[ii]])
                                   
                                   # Test that the samples and series are selected.
                                   testthat::expect_equal(data.table::fsetequal(validation_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                                subsample_data$valid_list[[ii]]),
                                                          TRUE)
                                   
                                   # Test that repetitions are likewise selected.
                                   testthat::expect_equal(nrow(validation_data@data),
                                                          n_rep * nrow(subsample_data$valid_list[[ii]]))
                                   
                                   
                                   # If stratified, check that occurrence of event or categories is similar between
                                   # discovery and the entire dataset.
                                   if(stratify & outcome_type %in% c("binomial", "multinomial")){
                                     # Determine the frequency of outcome classes in the original dataset.
                                     input_frequency <- data@data[, list("frequency_original"=.N / nrow(data@data)), by="outcome"]
                                     train_frequency <- train_data@data[, list("frequency_bootstrap"=.N / nrow(train_data@data)), by="outcome"]
                                     
                                     # Update the frequency table.
                                     frequency_table <- merge(x=input_frequency,
                                                              y=train_frequency,
                                                              by="outcome")
                                     
                                     # Check that the data is correctly stratified.
                                     frequency_table[, "similar":=data.table::between(frequency_bootstrap,
                                                                                      lower=frequency_original-0.05,
                                                                                      upper=frequency_original+0.05)]
                                     
                                     testthat::expect_equal(all(frequency_table$similar), TRUE)
                                     
                                   } else if(stratify & outcome_type %in% c("survival")){
                                     # Determine the frequency of censored data points and events in classes in
                                     # the original dataset.
                                     input_frequency <- data@data[, list("frequency_original"=.N / nrow(data@data)), by="outcome_event"]
                                     train_frequency <- train_data@data[, list("frequency_bootstrap"=.N / nrow(train_data@data)), by="outcome_event"]
                                     
                                     # Update the frequency table.
                                     frequency_table <- merge(x=input_frequency,
                                                              y=train_frequency,
                                                              by="outcome_event")
                                     
                                     # Check that the data is correctly stratified.
                                     frequency_table[, "similar":=data.table::between(frequency_bootstrap,
                                                                                      lower=frequency_original-0.05,
                                                                                      upper=frequency_original+0.05)]
                                     
                                     testthat::expect_equal(all(frequency_table$similar), TRUE)
                                   }
                                   
                                   # Check that the rare outcome is found in the training data. This prevent
                                   # issues with training data.
                                   if(outcome_type == "multinomial"){
                                     testthat::expect_equal(nrow(train_data@data[outcome == "3"]) > 0, TRUE)
                                   }
                                   
                                   # Assert that all outcome levels in the
                                   # validation folds also appear in the
                                   # training folds.
                                   if(outcome_type %in% c("binomial", "multinomial", "survival")){
                                     testthat::expect_equal(length(setdiff(unique(validation_data@data$outcome),
                                                                           unique(train_data@data$outcome))),
                                                            0)
                                   }
                                 }
                               })
  }
}
