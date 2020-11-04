test.create_good_data_set <- function(outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  etype <- median_house_value <- NULL
  
  if(outcome_type == "survival"){
    # Load colon dataset from the survival package.
    data <- data.table::as.data.table(survival::colon)
    
    # Focus on recurrence.
    data <- data[etype == 1]
    
    # Keep only first 100 samples for speed and only id, nodes, rx, extent and
    # outcome.
    data <- as_data_object(data=data[1:100, ],
                           sample_id_column="id",
                           outcome_column=c("time", "status"),
                           outcome_type=outcome_type,
                           include_features=c("nodes", "rx"))
    
  } else if(outcome_type == "multinomial"){
    # Load iris data set.
    data <- data.table::as.data.table(datasets::iris)
    
    # Add sample identifier.
    data[,":="("sample_id"=.I)]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="Species",
                           outcome_type=outcome_type)
    
  } else if(outcome_type == "binomial"){
    # Load the cancer breast biopsy data set.
    data <- data.table::as.data.table(MASS::biopsy)
    
    # Rename columns.
    data.table::setnames(data,
                         old=c("ID", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class"),
                         new=c("id", "clump_thickness", "cell_size_uniformity", "cell_shape_uniformity",
                               "marginal_adhesion", "epithelial_cell_size", "bare_nuclei",
                               "bland_chromatin", "normal_nucleoli", "mitoses", "cell_malignancy"))
    
    # Keep unique samples. Some samples have the same id, but a different
    # outcome.
    data <- unique(data, by="id")
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Convert to a data object. Exclude cell_size_uniformity, as these are
    # correlated and make it difficult to stable establish variable importance.
    data <- as_data_object(data=data,
                           sample_id_column="id",
                           outcome_column="cell_malignancy",
                           outcome_type=outcome_type,
                           exclude_features="cell_size_uniformity",
                           class_levels=c("benign", "malignant"))
    
  } else if(outcome_type == "continuous"){
    # Load the California Test Score Data Set
    data <- data.table::data.table(Ecdat::Caschool)
    
    # Drop distcod, district, county, readscr, mathscr
    data[, ":="("distcod"=NULL, "district"=NULL, "county"=NULL, "readscr"=NULL, "mathscr"=NULL)]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 150 samples
    data <- data[271:420, ]
    
    # Convert to a data object. Exclude mealpct, as this feature is correlated
    # to avginc.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="testscr",
                           outcome_type=outcome_type,
                           exclude_features="mealpct")
    
  } else if(outcome_type == "count"){
    # Load the Boston Housing data set
    data <- data.table::as.data.table(MASS::Boston)
    
    # Rename columns
    data.table::setnames(data,
                         old=c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv"),
                         new=c("per_capita_crime", "large_residence_proportion", "industry", "by_charles_river",
                               "nox_concentration", "avg_rooms", "residence_before_1940_proportion",
                               "distance_to_employment_centres", "radial_highway_accessibility", "property_tax_rate",
                               "pupil_teacher_ratio", "african_american_metric", "lower_status_percentage", "median_house_value"))
    
    # Convert by_charles_river to a factor.
    data$by_charles_river <- factor(x=data$by_charles_river, levels=c(0, 1), labels=c("no", "yes"))
    
    # Convert the median_house_value to the actual value.
    data[, "median_house_value":=median_house_value * 1000.0]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="median_house_value",
                           outcome_type=outcome_type)
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}



test.create_small_good_data_set <- function(outcome_type){
  
  # Set random seed so that the same numbers are produced every time.
  set.seed(1844)
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now select a subset of the data.
  data@data <- data@data[sample(30)]
  
  return(data)
}


test.create_good_data_invariant_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type == "survival"){
    data@data[, "nodes":=4.0]
    
  } else if(outcome_type == "binomial"){
    data@data[, "clump_thickness":=4.0]
    
  } else if(outcome_type == "multinomial"){
    data@data[, "Sepal_Length":=3.0]
    
  } else if(outcome_type == "continuous"){
    data@data[, "calwpct":=5.0]
    
  } else if(outcome_type == "count"){
    data@data[, "industry":=3.0]
    
  }
  
  return(data)
}



test.create_empty_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now empty the data.
  data@data <- head(data@data, n=0)
  
  return(data)
}



test.create_one_sample_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now keep only the first sample.
  data@data <- head(data@data, n=1)
  
  return(data)
}



test.create_all_identical_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now keep only the first sample.
  data@data <- head(data@data, n=1)
  
  # Fill the dataset with the same sample.
  data@data <- data@data[rep.int(1L, 10)]
  
  # Set unique subject ids.
  data@data[, "sample_id":=.I]
  
  return(data)
}



test.create_one_feature_data_set <- function(outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  etype <- median_house_value <- NULL
  
  if(outcome_type == "survival"){
    # Load colon dataset from the survival package
    data <- data.table::as.data.table(survival::colon)
    
    # Recurrence
    data <- data[etype == 1]
    
    # Keep only first 100 samples for speed and only id, nodes, rx, extent and outcome.
    data <- as_data_object(data=data[1:100, ],
                           sample_id_column="id",
                           outcome_column=c("time", "status"),
                           outcome_type=outcome_type,
                           include_features=c("nodes"))
    
  } else if(outcome_type == "multinomial") {
    # Load iris data set.
    data <- data.table::as.data.table(datasets::iris)
    
    # Add sample identifier.
    data[,":="("sample_id"=.I)]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="Species",
                           outcome_type=outcome_type,
                           include_features=c("Petal.Length"))
    
  } else if(outcome_type == "binomial"){
    # Load the cancer breast biopsy data set.
    data <- data.table::as.data.table(MASS::biopsy)
    
    # Rename columns.
    data.table::setnames(data,
                         old=c("ID", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class"),
                         new=c("id", "clump_thickness", "cell_size_uniformity", "cell_shape_uniformity",
                               "marginal_adhesion", "epithelial_cell_size", "bare_nuclei",
                               "bland_chromatin", "normal_nucleoli", "mitoses", "cell_malignancy"))
    
    # Keep unique samples. Some samples have the same id, but a different
    # outcome.
    data <- unique(data, by="id")
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="id",
                           outcome_column="cell_malignancy",
                           outcome_type=outcome_type,
                           class_levels=c("benign", "malignant"),
                           include_features="cell_size_uniformity")
    
  } else if(outcome_type == "continuous"){
    # Load the California Test Score Data Set
    data <- data.table::data.table(Ecdat::Caschool)
    
    # Drop distcod, district, county, readscr, mathscr
    data[, ":="("distcod"=NULL, "district"=NULL, "county"=NULL, "readscr"=NULL, "mathscr"=NULL)]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 150 samples
    data <- data[271:420, ]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="testscr",
                           outcome_type=outcome_type,
                           include_features="avginc")
    
  } else if(outcome_type == "count"){
    # Load the Boston Housing data set
    data <- data.table::as.data.table(MASS::Boston)
    
    # Rename columns
    data.table::setnames(data,
                         old=c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv"),
                         new=c("per_capita_crime", "large_residence_proportion", "industry", "by_charles_river",
                               "nox_concentration", "avg_rooms", "residence_before_1940_proportion",
                               "distance_to_employment_centres", "radial_highway_accessibility", "property_tax_rate",
                               "pupil_teacher_ratio", "african_american_metric", "lower_status_percentage", "median_house_value"))
    
    # Convert by_charles_river to a factor.
    data$by_charles_river <- factor(x=data$by_charles_river, levels=c(0, 1), labels=c("no", "yes"))
    
    # Convert the median_house_value to the actual value.
    data[, "median_house_value":=median_house_value * 1000.0]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="median_house_value",
                           outcome_type=outcome_type,
                           include_features="lower_status_percentage")
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}



test.create_one_feature_one_sample_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_one_feature_data_set(outcome_type=outcome_type)
  
  # Now keep only the first sample.
  data@data <- head(data@data, n=1)
  
  return(data)
}



test.create_one_feature_invariant_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_one_feature_data_set(outcome_type=outcome_type)
  
  # Get the feature column
  feature_column <- get_feature_columns(data)
  
  # Set the feature to a fixed value.
  data@data[, (feature_column):=data@data[[feature_column]][1]]
  
  return(data)
}



test.create_wide_data_set <- function(outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  etype <- median_house_value <- NULL
  
  # Set random seed so that the same numbers are produced every time.
  set.seed(1844)
  
  if(outcome_type == "survival"){
    
    # Load colon dataset from the survival package
    data <- data.table::as.data.table(survival::colon)
    
    # Recurrence
    data <- data[etype == 1]
    
    # Remove superfluous columns
    data[, ":="("study"=NULL, "node4"=NULL, "etype"=NULL)]
    
    # Refactor columns
    data$sex <- factor(x=data$sex, levels=c(0, 1), labels=c("female", "male"))
    data$obstruct <- factor(data$obstruct, levels=c(0, 1), labels=c(FALSE, TRUE))
    data$perfor <- factor(data$perfor, levels=c(0, 1), labels=c(FALSE, TRUE))
    data$adhere <- factor(data$adhere, levels=c(0, 1), labels=c(FALSE, TRUE))
    data$differ <- factor(data$differ, levels=c(1, 2, 3), labels=c("well", "moderate", "poor"), ordered=TRUE)
    data$extent <- factor(data$extent, levels=c(1, 2, 3, 4), labels=c("submucosa", "muscle",  "serosa", "contiguous_structures"), ordered=TRUE)
    data$surg <- factor(data$surg, levels=c(0, 1), labels=c("short", "long"))
    
    # Make the dataset small and wide (10 features)
    data <- data[1:5, ]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) stats::rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Keep only first 100 samples for speed and only id, nodes, rx, extent and outcome.
    data <- as_data_object(data=data,
                           sample_id_column="id",
                           outcome_column=c("time", "status"),
                           outcome_type=outcome_type)
    
  } else if(outcome_type == "multinomial"){
    # Load iris data set.
    data <- data.table::as.data.table(datasets::iris)
    
    # Squeeze data
    data <- data[c(1, 2, 3, 80, 81, 82, 148, 149, 150)]
    
    # Add sample identifier.
    data[,":="("sample_id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) stats::rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n) factor(sample(c("red", "green", "blue"), size=n, replace=TRUE)), n=nrow(data))
    names(random_data) <- paste0("random_categorical_", seq_len(3))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="Species",
                           outcome_type=outcome_type)
    
  } else if(outcome_type == "binomial"){
    # Load the cancer breast biopsy data set.
    data <- data.table::as.data.table(MASS::biopsy)
    
    # Rename columns.
    data.table::setnames(data,
                         old=c("ID", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class"),
                         new=c("id", "clump_thickness", "cell_size_uniformity", "cell_shape_uniformity",
                               "marginal_adhesion", "epithelial_cell_size", "bare_nuclei",
                               "bland_chromatin", "normal_nucleoli", "mitoses", "cell_malignancy"))
    
    # Keep unique samples. Some samples have the same id, but a different
    # outcome.
    data <- unique(data, by="id")
    
    # Limit to 10 samples
    data <- data[11:20, ]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) stats::rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n) factor(sample(c("red", "green", "blue"), size=n, replace=TRUE)), n=nrow(data))
    names(random_data) <- paste0("random_categorical_", seq_len(3))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="id",
                           outcome_column="cell_malignancy",
                           outcome_type=outcome_type,
                           class_levels=c("benign", "malignant"))
    
  } else if(outcome_type == "continuous"){
    # Load the California Test Score Data Set
    data <- data.table::data.table(Ecdat::Caschool)
    
    # Drop distcod, district, county, readscr, mathscr
    data[, ":="("distcod"=NULL, "district"=NULL, "county"=NULL, "readscr"=NULL, "mathscr"=NULL)]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 10 samples
    data <- data[411:420, ]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) stats::rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="testscr",
                           outcome_type=outcome_type)
    
  } else if(outcome_type == "count"){
    # Load the Boston Housing data set
    data <- data.table::as.data.table(MASS::Boston)
    
    # Rename columns
    data.table::setnames(data,
                         old=c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv"),
                         new=c("per_capita_crime", "large_residence_proportion", "industry", "by_charles_river",
                               "nox_concentration", "avg_rooms", "residence_before_1940_proportion",
                               "distance_to_employment_centres", "radial_highway_accessibility", "property_tax_rate",
                               "pupil_teacher_ratio", "african_american_metric", "lower_status_percentage", "median_house_value"))
    
    # Convert by_charles_river to a factor.
    data$by_charles_river <- factor(x=data$by_charles_river, levels=c(0, 1), labels=c("no", "yes"))
    
    # Convert the median_house_value to the actual value.
    data[, "median_house_value":=median_house_value * 1000.0]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Limit to 10 samples
    data <- data[1:10, ]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) stats::rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n) factor(sample(c("red", "green", "blue"), size=n, replace=TRUE)), n=nrow(data))
    names(random_data) <- paste0("random_categorical_", seq_len(3))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Convert to a data object.
    data <- as_data_object(data=data,
                           sample_id_column="sample_id",
                           outcome_column="median_house_value",
                           outcome_type=outcome_type)
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}



test.create_bad_data_set <- function(outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type == "survival"){
    # For survival data it would be really bad if all data are censored.
    data@data[, "outcome_event":=0]
    
  } else if(outcome_type == "multinomial"){
    # For multinomial data, having not all classes is bad.
    data@data <- data@data[outcome %in% c("setosa", "versicolor"), ]
    
  } else if(outcome_type == "binomial" ){
    # For binomial data, having a single class is bad.
    data@data[, "outcome":="benign"]
    
  } else if(outcome_type == "continuous"){
    # For continuous data, it would be bad if all outcome values are invariant.
    data@data[, "outcome":=500.0]
    
  } else if(outcome_type == "count"){
    # For count data it would be bad if all outcome values are invariant
    data@data[, "outcome":=50000]
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}


test.create_small_bad_data_set <- function(outcome_type){
  
  # Set random seed so that the same numbers are produced every time.
  set.seed(1844)
  
  # Create good dataset first and work from there.
  data <- test.create_bad_data_set(outcome_type=outcome_type)
  
  # Now select a subset of the data.
  data@data <- data@data[sample(30)]
  
  return(data)
}



test_create_synthetic_series_data <- function(outcome_type, n_batch=3, n_samples=10, n_series=3, n_rep=3){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  batch_id <- NULL
  
  # Set random seed so that the same numbers are produced every time.
  set.seed(1844)
  browser()
  # Determine the number of series instances.
  n_series_instances <- n_batch * n_samples * n_series
  
  # Draw random numbers for three features.
  feature_1 <- stats::runif(n=n_series_instances, min=0.0, max=1.0)
  feature_2 <- stats::runif(n=n_series_instances, min=0.0, max=2.0)
  feature_3 <- stats::runif(n=n_series_instances, min=0.0, max=3.0)
  
  # Determine the raw outcome.
  outcome_raw <- feature_1 + feature_2 + feature_3
  
  if(outcome_type == "binomial"){
    # Convert to 0, 1
    outcome_value <- outcome_raw > 3.0
    outcome_value <- factor(x=outcome_value,
                            levels=c(FALSE, TRUE),
                            labels=c("0", "1"))
    
  } else if(outcome_type == "multinomial"){
    # Convert to 0 (x < 2), 1 (2 < x 4), 2 (4 < x < 6)
    outcome_value <- floor(outcome_raw / 2)
    outcome_value[outcome_value==3.0] <- 2.0
    outcome_value <- factor(x=outcome_value,
                            levels=c(0.0, 1.0, 2.0),
                            labels=c("0", "1", "2"))
    
  } else if(outcome_type == "continuous"){
    outcome_value <- outcome_raw
    
  } else if(outcome_type == "count"){
    outcome_value <- round(outcome_raw * 100)
    
  } else if(outcome_type == "survival"){
    # Outcome follows an exponential distribution.
    outcome_time <- exp(outcome_value)
    outcome_event <- rep_len(1, length.out=n_series_instances)
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  # Create basic table. Sample identifiers are explicitly repeated for different
  # batches.
  data <- data.table::data.table("batch_id"=rep(seq_len(n_batch), each=n_samples * n_series),
                                 "sample_id"=rep(seq_len(n_samples), each=n_series, times=n_batch),
                                 "series_id"=rep(seq_len(n_series), times=n_batch * n_samples),
                                 "feature_1"=feature_1,
                                 "feature_2"=feature_2,
                                 "feature_3"=feature_3)
  
  # Add outcome.
  if(outcome_type %in% "survival"){
    data[, ":="("outcome_time"=outcome_time, "outcome_event"=outcome_event)]
    outcome_column <- c("outcome_time", "outcome_event")
  } else {
    data[, ":="("outcome"=outcome_value)]
    outcome_column <- "outcome"
  }
  
  # Create batch-offsets
  data[,":="("feature_1"=feature_1 + batch_id - 1.0,
             "feature_2"=feature_2 + batch_id - 1.0,
             "feature_3"=feature_3 + batch_id - 1.0)]
  
  # Create repetitions.
  if(n_rep > 1){
    repeated_rows <- rep(seq_len(n_series_instances), each=n_rep)
    data <- data[repeated_rows, ]
    
    # Add some noise to features.
    data[,":="("feature_1"=feature_1 + stats::rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125),
               "feature_2"=feature_2 + stats::rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125),
               "feature_3"=feature_3 + stats::rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125))]
  }
  
  # Convert to a data object.
  data <- as_data_object(data=data,
                         batch_id_column="batch_id",
                         sample_id_column="sample_id",
                         series_id_column="series_id",
                         outcome_column=outcome_column,
                         outcome_type=outcome_type)
  
  return(data)
}
