set.seed(1844)


##### Full undersampling #######################################################
for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset.
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, rare_outcome=FALSE)
  n_rep <- 3L
  
  testthat::test_that(paste0("Full undersampling for correcting outcome imbalances for ", 
                             outcome_type, " functions correctly."), {
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_method="full_undersampling"))
                               
                               # Check that none of the training folds are the same.
                               if(length(subsample_data) > 1){
                                 for(ii in 1:(length(subsample_data) - 1)){
                                   for(jj in (ii+1):length(subsample_data)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data[[ii]],
                                                                                  subsample_data[[jj]]),
                                                            FALSE)
                                   }
                                 }
                               }
                               
                               # Determine the minority class.
                               
                               
                               for(ii in seq_along(subsample_data)){
                                 # Assert that all samples in the subsample are unique (not duplicated).
                                 testthat::expect_equal(anyDuplicated(subsample_data[[ii]]), 0)
                                 
                                 # Check that sampling creates a dataset identical to the development subsample.
                                 train_data <- familiar:::select_data_from_samples(data=data,
                                                                                   samples=subsample_data[[ii]])
                                 
                                 # Test that the samples and series are selected.
                                 testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(train_data@data),
                                                        n_rep * nrow(subsample_data[[ii]]))
                                 
                                 # Assert that outcomes the minority class is
                                 # now selected as least as often as other
                                 # classes.
                                 original_table <- unique(data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("n"=.N), by="outcome"][order(n)]
                                 minority_class_n <- min(original_table$n)
                                 minority_class <- original_table[n == minority_class_n]$outcome[1]
                                 
                                 frequency_table <- unique(train_data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("partition_occurrence"=.N), by="outcome"]
                                 
                                 # Assert that all instances of the minority
                                 # class are selected.
                                 testthat::expect_equal(frequency_table[outcome == minority_class]$partition_occurrence, minority_class_n)
                                 
                                 # Assert that all instances similar to the
                                 # minority class are selected.
                                 testthat::expect_equal(all(frequency_table$partition_occurrence <= minority_class_n), TRUE)
                               }
                             })
}


for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset.
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, n_series=1L, n_samples=30, rare_outcome=FALSE)
  n_rep <- 3L
  
  testthat::test_that(paste0("Full undersampling for correcting outcome imbalances for ", 
                             outcome_type, " without multiple series functions correctly."), {
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_method="full_undersampling"))
                               
                               # Check that none of the training folds are the same.
                               if(length(subsample_data) > 1){
                                 for(ii in 1:(length(subsample_data) - 1)){
                                   for(jj in (ii+1):length(subsample_data)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data[[ii]],
                                                                                  subsample_data[[jj]]),
                                                            FALSE)
                                   }
                                 }
                               }
                               
                               # The union of the datasets is the original
                               # dataset.
                               testthat::expect_equal(data.table::fsetequal(unique(data.table::rbindlist(subsample_data)),
                                                                            unique(data@data[, mget(familiar:::get_id_columns(id_depth="series"))])),
                                                      TRUE)
                               
                               
                               for(ii in seq_along(subsample_data)){
                                 # Assert that all samples in the subsample are unique (not duplicated).
                                 testthat::expect_equal(anyDuplicated(subsample_data[[ii]]), 0)
                                 
                                 # Check that sampling creates a dataset identical to the development subsample.
                                 train_data <- familiar:::select_data_from_samples(data=data,
                                                                                   samples=subsample_data[[ii]])
                                 
                                 # Test that the samples and series are selected.
                                 testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(train_data@data),
                                                        n_rep * nrow(subsample_data[[ii]]))
                                 
                                 # Assert that outcomes the minority class is
                                 # now selected as least as often as other
                                 # classes.
                                 original_table <- unique(data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("n"=.N), by="outcome"][order(n)]
                                 minority_class_n <- min(original_table$n)
                                 minority_class <- original_table[n == minority_class_n]$outcome[1]
                                 
                                 frequency_table <- unique(train_data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("partition_occurrence"=.N), by="outcome"]
                                 
                                 # Assert that all instances of the minority
                                 # class are selected.
                                 testthat::expect_equal(frequency_table[outcome == minority_class]$partition_occurrence, minority_class_n)
                                 
                                 # Assert that all instances similar to the
                                 # minority class are selected.
                                 testthat::expect_equal(all(frequency_table$partition_occurrence == minority_class_n), TRUE)
                               }
                             })
}


for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset with one outcome.
  
  testthat::test_that(paste0("Full undersampling for correcting outcome imbalances for ", 
                             outcome_type, " with odd data functions correctly."), {
                               
                               # One outcome-data
                               data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_method="full_undersampling"))
                               # Expect a list. This is sort of a placeholder
                               # because the partitioning should work.
                               testthat::expect_type(subsample_data, "list")
                               
                               # One sample data.
                               data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                               
                               # Create subsample
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_method="full_undersampling"))
                               
                               # Expect a list. This is sort of a placeholder
                               # because the partitioning should work.
                               testthat::expect_type(subsample_data, "list")
                             })
}


##### Random undersampling #######################################################
for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset.
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, rare_outcome=FALSE)
  n_rep <- 3L
  
  testthat::test_that(paste0("Random undersampling for correcting outcome imbalances for ", 
                             outcome_type, " functions correctly."), {
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_n_partitions=3L,
                                                                                                         imbalance_method="random_undersampling"))
                               
                               # Check that none of the training folds are the same.
                               if(length(subsample_data) > 1){
                                 for(ii in 1:(length(subsample_data) - 1)){
                                   for(jj in (ii+1):length(subsample_data)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data[[ii]],
                                                                                  subsample_data[[jj]]),
                                                            FALSE)
                                   }
                                 }
                               }
                               
                               # Check that at most 3 (the number specified) partitions are created.
                               testthat::expect_lte(length(subsample_data), 3L)
                               
                               for(ii in seq_along(subsample_data)){
                                 # Assert that all samples in the subsample are unique (not duplicated).
                                 testthat::expect_equal(anyDuplicated(subsample_data[[ii]]), 0)
                                 
                                 # Check that sampling creates a dataset identical to the development subsample.
                                 train_data <- familiar:::select_data_from_samples(data=data,
                                                                                   samples=subsample_data[[ii]])
                                 
                                 # Test that the samples and series are selected.
                                 testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(train_data@data),
                                                        n_rep * nrow(subsample_data[[ii]]))
                                 
                                 # Assert that outcomes the minority class is
                                 # now selected as least as often as other
                                 # classes.
                                 original_table <- unique(data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("n"=.N), by="outcome"][order(n)]
                                 minority_class_n <- min(original_table$n)
                                 minority_class <- original_table[n == minority_class_n]$outcome[1]
                                 
                                 frequency_table <- unique(train_data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("partition_occurrence"=.N), by="outcome"]
                                 
                                 # Assert that all instances of the minority
                                 # class are selected.
                                 testthat::expect_equal(frequency_table[outcome == minority_class]$partition_occurrence, minority_class_n)
                                 
                                 # Assert that all instances similar to the
                                 # minority class are selected.
                                 testthat::expect_equal(all(frequency_table$partition_occurrence <= minority_class_n), TRUE)
                               }
                             })
}



for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset.
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, n_series=1L, n_samples=30, rare_outcome=FALSE)
  n_rep <- 3L
  
  testthat::test_that(paste0("Random undersampling for correcting outcome imbalances for ", 
                             outcome_type, " without multiple series functions correctly."), {
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_n_partitions=3L,
                                                                                                         imbalance_method="random_undersampling"))
                               
                               # Check that none of the training folds are the same.
                               if(length(subsample_data) > 1){
                                 for(ii in 1:(length(subsample_data) - 1)){
                                   for(jj in (ii+1):length(subsample_data)){
                                     testthat::expect_equal(data.table::fsetequal(subsample_data[[ii]],
                                                                                  subsample_data[[jj]]),
                                                            FALSE)
                                   }
                                 }
                               }

                               for(ii in seq_along(subsample_data)){
                                 # Assert that all samples in the subsample are unique (not duplicated).
                                 testthat::expect_equal(anyDuplicated(subsample_data[[ii]]), 0)
                                 
                                 # Check that sampling creates a dataset identical to the development subsample.
                                 train_data <- familiar:::select_data_from_samples(data=data,
                                                                                   samples=subsample_data[[ii]])
                                 
                                 # Test that the samples and series are selected.
                                 testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(train_data@data),
                                                        n_rep * nrow(subsample_data[[ii]]))
                                 
                                 # Assert that outcomes the minority class is
                                 # now selected as least as often as other
                                 # classes.
                                 original_table <- unique(data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("n"=.N), by="outcome"][order(n)]
                                 minority_class_n <- min(original_table$n)
                                 minority_class <- original_table[n == minority_class_n]$outcome[1]
                                 
                                 frequency_table <- unique(train_data@data, by=familiar:::get_id_columns(id_depth="series"))[, list("partition_occurrence"=.N), by="outcome"]
                                 
                                 # Assert that all instances of the minority
                                 # class are selected.
                                 testthat::expect_equal(frequency_table[outcome == minority_class]$partition_occurrence, minority_class_n)
                                 
                                 # Assert that all instances similar to the
                                 # minority class are selected.
                                 testthat::expect_equal(all(frequency_table$partition_occurrence == minority_class_n), TRUE)
                               }
                             })
}


for(outcome_type in c("binomial", "multinomial")){
  # Create synthetic dataset with one outcome.
  
  testthat::test_that(paste0("Random undersampling for correcting outcome imbalances for ", 
                             outcome_type, " with odd data functions correctly."), {
                               
                               # One outcome-data
                               data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                               
                               # Create subsample.
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_n_partitions=3L,
                                                                                                         imbalance_method="random_undersampling"))
                               # Expect a list. This is sort of a placeholder
                               # because the partitioning should work.
                               testthat::expect_type(subsample_data, "list")
                               
                               # One sample data.
                               data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                               
                               # Create subsample
                               subsample_data <- suppressWarnings(familiar:::.create_balanced_partitions(data=data@data,
                                                                                                         outcome_type=outcome_type,
                                                                                                         imbalance_n_partitions=3L,
                                                                                                         imbalance_method="random_undersampling"))
                               
                               # Expect a list. This is sort of a placeholder
                               # because the partitioning should work.
                               testthat::expect_type(subsample_data, "list")
                             })
}



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


for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  # Create synthetic dataset with one outcome.
  
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    available_stratify_options <- c(FALSE, TRUE)
    
  } else {
    available_stratify_options <- FALSE
  }
  
  n_folds <- 3L
  
  for(stratify in available_stratify_options){
    
    testthat::test_that(paste0("Cross-validation ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " with odd data functions correctly."), {
                                 
                                 # One outcome-data
                                 data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                                 
                                 # Create subsample.
                                 subsample_data <- suppressWarnings(familiar:::.create_cv(data=data@data,
                                                                                          n_folds=n_folds,
                                                                                          stratify=stratify,
                                                                                          outcome_type=outcome_type))
                                 # Expect a list. This is sort of a placeholder
                                 # because cross-validation should work even
                                 # when the outcome value is singular.
                                 testthat::expect_type(subsample_data, "list")
                                 
                                 # One sample data.
                                 data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                                 
                                 # Create subsample. We expect an error because
                                 # you can't do cross-validation with a single
                                 # sample.
                                 subsample_data <- testthat::expect_error(suppressWarnings(familiar:::.create_cv(data=data@data,
                                                                                                                 n_folds=n_folds,
                                                                                                                 stratify=stratify,
                                                                                                                 outcome_type=outcome_type)))
                               })
  }
}
  
  

##### Repeated cross-validation ################################################
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
    
    testthat::test_that(paste0("Repeated cross-validation ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " functions correctly."), {
                                 
                                 # Create subsample.
                                 subsample_data <- familiar:::.create_repeated_cv(data=data@data,
                                                                                  n_rep=3L,
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


for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  # Create synthetic dataset with one outcome.
  
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    available_stratify_options <- c(FALSE, TRUE)
    
  } else {
    available_stratify_options <- FALSE
  }
  
  n_folds <- 3L

  for(stratify in available_stratify_options){
    
    testthat::test_that(paste0("Repeated cross-validation ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " with odd data functions correctly."), {
                                 
                                 # One outcome-data
                                 data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                                 
                                 # Create subsample.
                                 subsample_data <- suppressWarnings(familiar:::.create_repeated_cv(data=data@data,
                                                                                                   n_rep=3L,
                                                                                                   n_folds=n_folds,
                                                                                                   stratify=stratify,
                                                                                                   outcome_type=outcome_type))
                                 # Expect a list. This is sort of a placeholder
                                 # because cross-validation should work even
                                 # when the outcome value is singular.
                                 testthat::expect_type(subsample_data, "list")
                                 
                                 # One sample data.
                                 data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                                 
                                 # Create subsample. We expect an error because
                                 # you can't do cross-validation with a single
                                 # sample.
                                 subsample_data <- testthat::expect_error(suppressWarnings(familiar:::.create_repeated_cv(data=data@data,
                                                                                                                          n_rep=3L,
                                                                                                                          n_folds=n_folds,
                                                                                                                          stratify=stratify,
                                                                                                                          outcome_type=outcome_type)))
                               })
  }
}



##### Leave-one-out cross-validation ###########################################
for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  
  
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, rare_outcome=TRUE)
  n_rep <- 3L
  
  testthat::test_that(paste0("Leave-one-out cross-validation for ",
                             outcome_type, " functions correctly."), {
                               
                               # Create subsample
                               subsample_data <- familiar:::.create_loocv(data=data@data,
                                                                          outcome_type=outcome_type)
                               
                               # Check that none of the training folds are the same.
                               for(ii in 1:(length(subsample_data$train_list) - 1)){
                                 for(jj in (ii+1):length(subsample_data$train_list)){
                                   testthat::expect_equal(data.table::fsetequal(subsample_data$train_list[[ii]],
                                                                                subsample_data$train_list[[jj]]),
                                                          FALSE)
                                 }
                               }
                               
                               # Assert (for LOOCV) that the number of run pairs
                               # is the number of samples, or the number of
                               # samples - 1 for multinomial (due to
                               # pre-assignment of a sample with the rare
                               # outcome level).
                               if(outcome_type == "multinomial"){
                                 testthat::expect_equal(length(subsample_data$train_list),
                                                        data.table::uniqueN(data@data, by=familiar:::get_id_columns(id_depth="sample")) - 1L)
                                 
                               } else {
                                 testthat::expect_equal(length(subsample_data$train_list),
                                                        data.table::uniqueN(data@data, by=familiar:::get_id_columns(id_depth="sample")))
                               }
                               
                               # Iterate over the subsamples.
                               for(ii in seq_along(subsample_data$train_list)){
                                 # Check that there is no overlap between
                                 # training folds and the validation fold.
                                 testthat::expect_equal(nrow(data.table::fintersect(unique(subsample_data$train_list[[ii]]),
                                                                                    unique(subsample_data$valid_list[[ii]]))),
                                                        0L)
                                 
                                 # Check that the union of training and
                                 # validation folds is the input dataset.
                                 testthat::expect_equal(data.table::fsetequal(unique(rbind(subsample_data$train_list[[ii]], subsample_data$valid_list[[ii]])),
                                                                              unique(data@data[, mget(familiar:::get_id_columns(id_depth="series"))])),
                                                        TRUE)
                                 
                                 # Assert that all samples in the training fold
                                 # are unique (not duplicated).
                                 testthat::expect_equal(anyDuplicated(subsample_data$train_list[[ii]]), 0)
                                 
                                 # Assert that there is only one sample in the
                                 # validation fold.
                                 testthat::expect_equal(data.table::uniqueN(subsample_data$valid_list[[ii]],
                                                                            by=familiar:::get_id_columns(id_depth="sample")),
                                                        1)
                                 
                                 # Check that sampling creates a dataset
                                 # identical to the development subsample.
                                 train_data <- familiar:::select_data_from_samples(data=data,
                                                                                   samples=subsample_data$train_list[[ii]])
                                 
                                 # Test that the samples and series are
                                 # selected.
                                 testthat::expect_equal(data.table::fsetequal(train_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data$train_list[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(train_data@data),
                                                        n_rep * nrow(subsample_data$train_list[[ii]]))
                                 
                                 # Assert that the size of the training set is
                                 # equal to the number of samples - 1.
                                 testthat::expect_equal(data.table::uniqueN(train_data@data,
                                                                            by=familiar:::get_id_columns(id_depth="sample")),
                                                        data.table::uniqueN(data@data,
                                                                            by=familiar:::get_id_columns(id_depth="sample")) - 1L)
                                 
                                 # Check that sampling creates a dataset
                                 # identical to the validation subsample.
                                 validation_data <- familiar:::select_data_from_samples(data=data,
                                                                                        samples=subsample_data$valid_list[[ii]])
                                 
                                 # Test that the samples and series are
                                 # selected.
                                 testthat::expect_equal(data.table::fsetequal(validation_data@data[repetition_id==1L, mget(familiar:::get_id_columns(id_depth="series"))],
                                                                              subsample_data$valid_list[[ii]]),
                                                        TRUE)
                                 
                                 # Test that repetitions are likewise selected.
                                 testthat::expect_equal(nrow(validation_data@data),
                                                        n_rep * nrow(subsample_data$valid_list[[ii]]))
                                 
                                 # Test that the number of samples in the the
                                 # validation fold is 1.
                                 testthat::expect_equal(data.table::uniqueN(validation_data@data,
                                                                            by=familiar:::get_id_columns(id_depth="sample")),
                                                        1L)
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


for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  
    testthat::test_that(paste0("Repeated cross-validation for ",
                               outcome_type, " with odd data functions correctly."), {
                                 
                                 # One outcome-data
                                 data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                                 
                                 # Create subsample.
                                 subsample_data <- suppressWarnings(familiar:::.create_loocv(data=data@data,
                                                                                             outcome_type=outcome_type))
                                 # Expect a list. This is sort of a placeholder
                                 # because cross-validation should work even
                                 # when the outcome value is singular.
                                 testthat::expect_type(subsample_data, "list")
                                 
                                 # One sample data.
                                 data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                                 
                                 # Create subsample. We expect an error because
                                 # you can't do cross-validation with a single
                                 # sample.
                                 subsample_data <- testthat::expect_error(suppressWarnings(familiar:::.create_loocv(data=data@data,
                                                                                                                    outcome_type=outcome_type)))
                               })
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


for(outcome_type in c("binomial", "multinomial", "continuous", "count", "survival")){
  # Create synthetic dataset with one outcome.
  
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    available_stratify_options <- c(FALSE, TRUE)
    
  } else {
    available_stratify_options <- FALSE
  }
  
  for(stratify in available_stratify_options){
    
    testthat::test_that(paste0("Bootstrap ", ifelse(stratify, "(stratified) ", ""),
                               "for ", outcome_type, " with odd data functions correctly."), {
                                 
                                 # One outcome-data
                                 data <- familiar:::test_create_synthetic_series_one_outcome(outcome_type=outcome_type)
                                 
                                 # Create subsample.
                                 subsample_data <- familiar:::.create_bootstraps(data=data@data,
                                                                                 n_iter=20,
                                                                                 stratify=stratify,
                                                                                 outcome_type=outcome_type)
                                 
                                 # Expect a list. This is sort of a placeholder
                                 # because cross-validation should work even
                                 # when the outcome value is singular.
                                 testthat::expect_type(subsample_data, "list")
                                 
                                 # One sample data.
                                 data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type)
                                 
                                 # Create subsample. We expect an error because
                                 # you can't do cross-validation with a single
                                 # sample.
                                 subsample_data <- testthat::expect_error(familiar:::.create_bootstraps(data=data@data,
                                                                                                        n_iter=20,
                                                                                                        stratify=stratify,
                                                                                                        outcome_type=outcome_type))
                               })
  }
}
