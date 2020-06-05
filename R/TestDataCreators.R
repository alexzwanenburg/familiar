test.create_good_data_set <- function(outcome_type){
  
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
    data <- data.table::as.data.table(iris)
    
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
                           exclude_features="cell_shape_uniformity",
                           class_levels=c("benign", "malignant"))
    
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



test.create_one_feature_data_set <- function(outcome_type){
  
  if(outcome_type == "survival"){
    # Load colon dataset from the survival package
    data <- as.data.table(survival::colon)
    
    # Recurrence
    data <- data[etype == 1]
    
    # Keep only first 100 samples for speed and only id, nodes, rx, extent and outcome.
    data <- familiar:::as_data_object(data=data[1:100, ],
                                      sample_id_column="id",
                                      outcome_column=c("time", "status"),
                                      outcome_type=outcome_type,
                                      include_features=c("nodes"))
    
  } else if(outcome_type == "multinomial") {
    # Load iris data set.
    data <- data.table::as.data.table(iris)
    
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
    
  }
  
  return(data)
}



test.create_wide_data_set <- function(outcome_type){
  
  # Set random seed so that the same numbers are produced every time.
  set.seed(1844)
  
  if(outcome_type == "survival"){
    
    # Load colon dataset from the survival package
    data <- as.data.table(survival::colon)
    
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
    random_data <- lapply(seq_len(20), function(ii, n) rnorm(n=n), n=nrow(data))
    names(random_data) <- paste0("random_", seq_len(20))
    
    # Add to dataset
    data <- cbind(data, data.table::as.data.table(random_data))
    
    # Keep only first 100 samples for speed and only id, nodes, rx, extent and outcome.
    data <- familiar:::as_data_object(data=data,
                                      sample_id_column="id",
                                      outcome_column=c("time", "status"),
                                      outcome_type=outcome_type)
    
  } else if(outcome_type == "multinomial"){
    # Load iris data set.
    data <- data.table::as.data.table(iris)
    
    # Squeeze data
    data <- data[c(1, 2, 3, 80, 81, 82, 148, 149, 150)]
    
    # Add sample identifier.
    data[,":="("sample_id"=.I)]
    
    # Add twenty random features
    random_data <- lapply(seq_len(20), function(ii, n) rnorm(n=n), n=nrow(data))
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
    random_data <- lapply(seq_len(20), function(ii, n) rnorm(n=n), n=nrow(data))
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
    
  }
  
  return(data)
}



test.create_bad_data_set <- function(outcome_type){
  
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
    data@data[, "outcome":="setosa"]
    
  }
  
  return(data)
}
