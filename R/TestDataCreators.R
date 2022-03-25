test.create_good_data_set <- function(outcome_type, to_data_object=TRUE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  etype <- median_house_value <- NULL
  
  if(outcome_type == "survival"){
    # Load colon dataset from the survival package.
    data <- data.table::as.data.table(survival::colon)
    
    # Focus on recurrence.
    data <- data[etype == 1]
    data$adhere <- factor(data$adhere, levels=c(0, 1), labels=c(FALSE, TRUE), ordered=TRUE)
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
    if(to_data_object){
      data <- as_data_object(data=data,
                             sample_id_column="id",
                             outcome_column=c("time", "status"),
                             outcome_type=outcome_type,
                             include_features=c("nodes", "rx", "adhere"))
    }
    
    
  } else if(outcome_type == "multinomial"){
    # Load iris data set.
    data <- data.table::as.data.table(datasets::iris)
    
    # Add sample identifier.
    data[,":="("sample_id"=.I)]
    
    # Convert to a data object.
    if(to_data_object){
      data <- as_data_object(data=data,
                             sample_id_column="sample_id",
                             outcome_column="Species",
                             outcome_type=outcome_type)
    }
    
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
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
    # Convert to a data object. Exclude cell_size_uniformity, as these are
    # correlated and make it difficult to stable establish variable importance.
    if(to_data_object){
      data <- as_data_object(data=data,
                             sample_id_column="id",
                             outcome_column="cell_malignancy",
                             outcome_type=outcome_type,
                             exclude_features="cell_size_uniformity",
                             class_levels=c("benign", "malignant"))
    }
    
  } else if(outcome_type == "continuous"){
    # Load the California Test Score Data Set
    data <- data.table::data.table(Ecdat::Caschool)
    
    # Drop distcod, district, county, readscr, mathscr
    data[, ":="("distcod"=NULL, "district"=NULL, "county"=NULL, "readscr"=NULL, "mathscr"=NULL)]
    
    # Limit to 150 samples
    data <- data[271:420, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Convert to a data object. Exclude mealpct, as this feature is correlated
    # to avginc.
    if(to_data_object){
      data <- as_data_object(data=data,
                             sample_id_column="sample_id",
                             outcome_column="testscr",
                             outcome_type=outcome_type,
                             exclude_features="mealpct")
    }
    
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
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Convert to a data object.
    if(to_data_object){
      data <- as_data_object(data=data,
                             sample_id_column="sample_id",
                             outcome_column="median_house_value",
                             outcome_type=outcome_type)
    }
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}



test.create_small_good_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now select a subset of the data.
  data@data <- data@data[fam_sample(seq_len(nrow(data@data)),
                                    size=30,
                                    replace=FALSE,
                                    seed=1844)]
  
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



test.create_good_data_no_censoring_set <- function(outcome_type){
  
  if(!outcome_type %in% c("survival", "competing_risk")) return(NULL)
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Set all data to event.
  data@data[, "outcome_event":=1]
  
  return(data)
}



test.create_good_data_one_censored_set <- function(outcome_type){
  
  if(!outcome_type %in% c("survival", "competing_risk")) return(NULL)
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Set all data to event.
  data@data[, "outcome_event":=1]
  
  # Set one instance to censored.
  data@data[1L, "outcome_event":=0]
  
  return(data)
}



test.create_good_data_few_censored_set <- function(outcome_type){
  
  if(!outcome_type %in% c("survival", "competing_risk")) return(NULL)
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Set all data to event.
  data@data[, "outcome_event":=1]
  
  # Set a few instances to censored.
  data@data[seq_len(4), "outcome_event":=0]
  
  return(data)
}



test.create_empty_data_set <- function(outcome_type){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  # Now empty the data.
  data@data <- head(data@data, n=0)
  
  return(data)
}



test.create_bootstrapped_data_set <- function(outcome_type, to_data_object=TRUE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- sample_id <- NULL
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type,
                                    to_data_object=to_data_object)
  
  # Now keep only the first sample.
  if(to_data_object){
    data@data[fam_sample(data@data, replace=TRUE), on=.NATURAL][order(sample_id)]
    
  } else {
    data[fam_sample(data, replace=TRUE), on=.NATURAL][order(sample_id)]
  }
  
  return(data)
  
}



test.create_one_sample_data_set <- function(outcome_type, to_data_object=TRUE){
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type,
                                    to_data_object=to_data_object)
  
  # Now keep only the first sample.
  if(to_data_object){
    data@data <- head(data@data, n=1)
    
  } else {
    data <- head(data, n=1)
  }
  
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
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
    # Keep only first 150 samples for speed and only id, nodes, rx, extent,
    # adhere and outcome.
    data <- as_data_object(data=data,
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
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
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
    
    # Limit to 150 samples
    data <- data[271:420, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
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
    
    # Limit to 150 samples
    data <- data[1:150, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
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



test.create_one_feature_two_values_data_set <- function(outcome_type){
  # Create good dataset first.
  data <- test.create_one_feature_data_set(outcome_type=outcome_type)
  
  # Get the feature columns
  feature_column <- get_feature_columns(data)
  
  # Find unique values of the feature and use the first 2.
  feature_values <- head(unique(data@data[[feature_column]]), n=2L)
  
  # Fill all the rows while alternating the value.
  data@data[, (feature_column):=rep_len(feature_values, nrow(data@data))]
  
  return(data)
}



test.create_wide_data_set <- function(outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  etype <- median_house_value <- NULL
  
  # Create random stream object so that the same numbers are produced every
  # time.
  r <- .start_random_number_stream(seed=1844)
  
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
    data$status <- 1
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n, r) fam_rnorm(n=n, rstream_object=r), n=nrow(data), r=r)
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
    random_data <- lapply(seq_len(20), function(ii, n, r) fam_rnorm(n=n, rstream_object=r), n=nrow(data), r=r)
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n, r) factor(fam_sample(c("red", "green", "blue"), size=n, replace=TRUE, rstream_object=r)), n=nrow(data), r=r)
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
    
    # update sample identifier.
    data[, ":="("id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n, r) fam_rnorm(n=n, rstream_object=r), n=nrow(data), r=r)
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n, r) factor(fam_sample(c("red", "green", "blue"), size=n, replace=TRUE, rstream_object=r)), n=nrow(data), r=r)
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
    
    # Limit to 10 samples
    data <- data[411:420, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n, r) fam_rnorm(n=n, rstream_object=r), n=nrow(data), r=r)
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
    
    # Limit to 10 samples
    data <- data[1:10, ]
    
    # Add sample identifier.
    data[, ":="("sample_id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n, r) fam_rnorm(n=n, rstream_object=r), n=nrow(data), r=r)
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Add another 3 random features
    random_data <- lapply(seq_len(3), function(ii, n, r) factor(fam_sample(c("red", "green", "blue"), size=n, replace=TRUE, rstream_object=r)), n=nrow(data), r=r)
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



test.create_bad_data_set <- function(outcome_type, add_na_data=FALSE){
  # add_na_data argument is intended for integration tests, where we have to
  # circumvent a check on the outcome classes. We do this by keeping these
  # classes in, and assigning NA to rows of one class, causing the data to pass
  # the check, but have the rows be removed afterwards.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL
  
  # Create good dataset first and work from there.
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type == "survival"){
    # For survival data it would be really bad if all data are censored.
    data@data[, "outcome_event":=0]
    
  } else if(outcome_type == "multinomial"){
    # For multinomial data, having not all classes is bad.
    
    if(add_na_data){
      # Assign NA to the rows containing the virginica class.
      
      # Identify the feature columns.
      feature_columns <- get_feature_columns(data)
      
      # Update feature columns.
      for(feature in feature_columns){
        if(is.factor(data@data[[feature]])){
          data@data[outcome == "virginica", (feature):=NA]
          
        } else {
          data@data[outcome == "virginica", (feature):=NA_real_]
        }
      }
      
    } else {
      # Select 2 of 3 classes by leaving virginica out.
      data@data <- data@data[outcome %in% c("setosa", "versicolor"), ]
    }
    
  } else if(outcome_type == "binomial" ){
    # For binomial data, having a single class is bad.
    
    if(add_na_data){
      # Assign NA to the rows containing the malignant class.
      
      # Identify the feature columns.
      feature_columns <- get_feature_columns(data)
      
      # Update feature columns.
      for(feature in feature_columns){
        if(is.factor(data@data[[feature]])){
          data@data[outcome == "malignant", (feature):=NA]
          
        } else {
          data@data[outcome == "malignant", (feature):=NA_real_]
        }
      }
      
    } else {
      # Assign everything to the benign class.
      data@data[, "outcome":="benign"]
    }
    
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
  
  # Create good dataset first and work from there.
  data <- test.create_bad_data_set(outcome_type=outcome_type)
  
  # Now select a subset of the data.
  data@data <- data@data[fam_sample(seq_len(nrow(data@data)),
                                    size=30,
                                    replace=FALSE,
                                    seed=1844)]
  
  return(data)
}



test.create_prospective_data_set <- function(outcome_type){
  # Prospective data has NA for outcome.
  
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type %in% c("survival", "competing_risk")){
    data@data[, ":="("outcome_time"=NA,
                     "outcome_event"=NA)]
  } else {
    data@data[, ":="("outcome"=NA)]
  }
  
  return(data)  
}



test.create_partially_prospective_data_set <- function(outcome_type){
  # Prospective data has NA for outcome for a few samples, but not all.
  
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type %in% c("survival", "competing_risk")){
    data@data[c(1,2), ":="("outcome_time"=NA,
                           "outcome_event"=NA)]
  } else {
    data@data[c(1,2), ":="("outcome"=NA)]
  }
  
  return(data)  
}



test.create_mostly_prospective_data_set <- function(outcome_type){
  # Prospective data has NA for all but one sample.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  sample_id <- NULL
  
  data <- test.create_good_data_set(outcome_type=outcome_type)
  
  if(outcome_type %in% c("survival", "competing_risk")){
    data@data[sample_id > 1L, ":="("outcome_time"=NA,
                                   "outcome_event"=NA)]
  } else {
    data@data[sample_id > 1L, ":="("outcome"=NA)]
  }
  
  return(data)  
}



test_create_synthetic_series_data <- function(outcome_type,
                                              n_batch=3,
                                              n_samples=10,
                                              n_series=3,
                                              n_rep=3,
                                              n_numeric=4L,
                                              rare_outcome=FALSE,
                                              seed=1844,
                                              rstream_object=NULL){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  batch_id <- feature_1 <- feature_2 <- feature_3 <- feature_4 <- NULL
  
  # Create random stream object so that the same numbers are produced every
  # time.
  if(is.null(rstream_object)){
    r <- .start_random_number_stream(seed=seed)
    
  } else {
    r <- rstream_object
  }
  
  # Determine the number of series instances.
  n_series_instances <- n_batch * n_samples * n_series
  
  # Draw random numbers for three features.
  feature_1 <- fam_runif(n=n_series_instances, min=0.0, max=1.0, rstream_object=r)
  feature_2 <- fam_runif(n=n_series_instances, min=0.0, max=2.0, rstream_object=r)
  feature_3 <- fam_runif(n=n_series_instances, min=0.0, max=2.0, rstream_object=r)
  feature_4 <- fam_runif(n=n_series_instances, min=0.0, max=1.0, rstream_object=r)
  
  # Determine the raw outcome.
  outcome_raw <- feature_1 + feature_2 + feature_3 + feature_4
  
  if(outcome_type == "binomial"){
    # Convert to 0, 1
    outcome_value <- outcome_raw > 3.0
    outcome_value <- factor(x=outcome_value,
                            levels=c(FALSE, TRUE),
                            labels=c("0", "1"))
    
  } else if(outcome_type == "multinomial"){
    outcome_value <- numeric(n_series_instances)
    
    # Convert to 0 (x < 2.59), 1 (2.59 < x < 3.41), 2 (3.41 < x < 6)
    outcome_value[outcome_raw < 2.59] <- 0.0
    outcome_value[outcome_raw >= 2.59 & outcome_raw < 3.41] <- 1.0
    outcome_value[outcome_raw >= 3.41 & outcome_raw < 6.00] <- 2.0
    
    if(rare_outcome){
      outcome_value[length(outcome_value)] <- 3.0
      outcome_value <- factor(x=outcome_value,
                              levels=c(0.0, 1.0, 2.0, 3.0),
                              labels=c("0", "1", "2", "3"))
      
    } else {
      outcome_value <- factor(x=outcome_value,
                              levels=c(0.0, 1.0, 2.0),
                              labels=c("0", "1", "2"))
    }
    
  } else if(outcome_type == "continuous"){
    outcome_value <- outcome_raw
    
  } else if(outcome_type == "count"){
    outcome_value <- round(outcome_raw * 100)
    
  } else if(outcome_type == "survival"){
    # Outcome follows an exponential distribution.
    outcome_time <- exp(outcome_raw)
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
                                 "feature_3"=feature_3,
                                 "feature_4"=feature_4)
  
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
             "feature_3"=feature_3 + batch_id - 1.0,
             "feature_4"=feature_4 + batch_id - 1.0)]
  
  # Create repetitions.
  if(n_rep > 1){
    repeated_rows <- rep(seq_len(n_series_instances), each=n_rep)
    data <- data[repeated_rows, ]
    
    # Add some noise to features.
    data[,":="("feature_1"=feature_1 + fam_rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125, rstream_object=r),
               "feature_2"=feature_2 + fam_rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125, rstream_object=r),
               "feature_3"=feature_3 + fam_rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125, rstream_object=r),
               "feature_4"=feature_4 + fam_rnorm(n=n_rep * n_series_instances, mean=0.0, sd=0.125, rstream_object=r))]
    
    
    data[feature_1 <= 0.0, "feature_1":=0.01]
    data[feature_2 <= 0.0, "feature_2":=0.01]
    data[feature_3 <= 0.0, "feature_3":=0.01]
    data[feature_4 <= 0.0, "feature_4":=0.01]
  }
  
  if(n_numeric < 4) data$feature_1 <- factor(floor(data$feature_1))
  if(n_numeric < 3) data$feature_2 <- factor(floor(data$feature_2))
  if(n_numeric < 2) data$feature_3 <- factor(floor(data$feature_3))
  if(n_numeric < 1) data$feature_4 <- factor(floor(data$feature_4))
  
  # Convert to a data object.
  data <- as_data_object(data=data,
                         batch_id_column="batch_id",
                         sample_id_column="sample_id",
                         series_id_column="series_id",
                         outcome_column=outcome_column,
                         outcome_type=outcome_type)
  
  return(data)
}



test_create_synthetic_series_one_outcome <- function(outcome_type,
                                                     n_numeric=4L,
                                                     seed=1844,
                                                     rstream_object=NULL){
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  if(outcome_type %in% c("binomial", "multinomial")){
    data@data[, "outcome":="0"]
    
  } else if(outcome_type %in% c("count", "continuous")){
    data@data[, "outcome":=1]
    
  } else if(outcome_type == "survival"){
    data@data[, ":="("outcome_time"=1.25, "outcome_event"=1)]
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  return(data)
}



test_create_synthetic_series_one_sample_data <- function(outcome_type,
                                                         n_numeric=4L,
                                                         seed=1844,
                                                         rstream_object=NULL){
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  # Select the first instance
  data@data <- head(data@data, n=1L)
  
  return(data)
}



test_create_synthetic_series_invariant_feature_data <- function(outcome_type,
                                                                n_numeric=4L,
                                                                seed=1844,
                                                                rstream_object=NULL){
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  # Select the first instance
  data@data$feature_1 <- data@data$feature_1[1]
  data@data$feature_2 <- data@data$feature_2[1]
  data@data$feature_3 <- data@data$feature_3[1]
  data@data$feature_4 <- data@data$feature_4[1]
  
  return(data)
}



test_create_synthetic_series_one_feature_invariant_data <- function(outcome_type,
                                                                    n_numeric=4L,
                                                                    seed=1844,
                                                                    rstream_object=NULL){
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  # Select the first instance for feature 2.
  data@data$feature_2 <- data@data$feature_2[1]
  
  return(data)
}



test_create_synthetic_series_na_data <- function(outcome_type,
                                                 n_numeric=4L,
                                                 n_missing_frac=0.1,
                                                 seed=1844,
                                                 rstream_object=NULL){
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  # Select which rows will be updated.
  n_rows <- nrow(data@data)
  na_rows <- fam_sample(seq_len(n_rows),
                        size=ceiling(n_missing_frac * n_rows),
                        replace=FALSE,
                        seed=seed,
                        rstream_object=rstream_object)
  
  # Identify the feature columns.
  feature_columns <- get_feature_columns(data)
  
  # Update feature columns.
  for(feature in feature_columns){
    if(is.factor(data@data[[feature]])){
      data@data[na_rows, (feature):=NA]
      
    } else {
      data@data[na_rows, (feature):=NA_real_]
    }
  }
  
  return(data)
}



test_create_synthetic_series_one_feature_all_na_data <- function(outcome_type,
                                                                 n_numeric=4L,
                                                                 seed=1844,
                                                                 rstream_object=NULL){
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_2 <- NULL
  
  # Create test data.
  data <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                            n_numeric=n_numeric,
                                            seed=seed,
                                            rstream_object=rstream_object)
  
  # Set the first feature column to NA.
  if(is.factor(data@data[["feature_2"]])){
    data@data[, feature_2:=NA]
    
  } else {
    data@data[, feature_2:=NA_real_]
  }
  
  return(data)
}


test_create_multiple_synthetic_series <- function(outcome_type){
  # The idea here is to create multiple synthetic datasets that together
  # represent extreme variation in data composition.
  
  ..extend_feature_set <- function(data){
    # Get feature columns.
    original_feature_columns <- get_feature_columns(data)
    
    # Find new feature columns for the correlated features
    new_feature_columns <- paste0("feature_", seq_along(original_feature_columns) + length(original_feature_columns))
    
    # Add in correlated features.
    for(ii in seq_along(original_feature_columns)){
      data@data[, (new_feature_columns[ii]):=get(original_feature_columns[ii])]
    }
    
    return(data)
  }
  
  # Draw the first dataset.
  data_1 <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                              n_numeric=3L,
                                              n_samples=20,
                                              seed=1)
  
  # Add correlated features.
  data_1 <- ..extend_feature_set(data_1)
  
  # Draw the second dataset.
  data_2 <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                              n_numeric=3L,
                                              n_samples=20,
                                              seed=2)
  # Do not add correlated features to dataset 2.
  
  # Draw a third dataset.
  data_3 <- test_create_synthetic_series_data(outcome_type=outcome_type,
                                              n_numeric=3L,
                                              n_samples=20,
                                              seed=3)
  
  # Add correlated features, but remove the original features.
  data_3 <- ..extend_feature_set(data_3)
  data_3@data[, ":="("feature_1"=NULL,
                     "feature_2"=NULL,
                     "feature_3"=NULL,
                     "feature_4"=NULL)]
  
  # Draw a fourth dataset that cannot be used for training, e.g. contains just
  # one sample.
  data_4 <- test_create_synthetic_series_one_outcome(outcome_type=outcome_type,
                                                     n_numeric=3L,
                                                     seed=4)
  
  # Add correlated features.
  data_4 <- ..extend_feature_set(data_4)
  
  return(list("set_1"=data_1,
              "set_2"=data_2,
              "set_3"=data_3,
              "set_4"=data_4))
}
