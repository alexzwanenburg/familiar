.assess_performance <- function(object, data, metric, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, as_objective=FALSE, na.rm=FALSE){
  # This is an internal function for assessing discriminatory performance of a model or ensemble
  
  # Check if the input classes are as expected
  if(!(is(object, "familiarModel") | is(object, "familiarEnsemble"))){
    ..error_reached_unreachable_code(".assess_performance: object is not a familiarModel or familiarEnsemble.")
  }
  
  # First, make predictions for the data
  prediction_table <- .predict(object=object,
                               data=data,
                               allow_recalibration=allow_recalibration,
                               is_pre_processed=is_pre_processed,
                               time=time)
  
  # Calculate the metric
  score <- metric.main(metric=metric,
                       object=object,
                       purpose="score",
                       dt=prediction_table,
                       outcome_type=object@outcome_type,
                       na.rm=na.rm)
  
  # Convert to an objective score if required
  if(as_objective){
    
    # Try to get a (baseline) mean score. This is used by some regression
    # metrics for normalisation of the objective score. It is the score achieved
    # by assuming a model that will always produce the mean value of the
    # development cohort.
    
    if(!is.null(object@outcome_info@distribution$mean)){
      score_mean <- metric.main(metric=metric, object=object, purpose="score",
                                dt=data.table::copy(prediction_table)[, "predicted_outcome":=object@outcome_info@distribution$mean],
                                outcome_type=object@outcome_type, na.rm=na.rm)
    } else {
      score_mean <- NULL
    }
    
    # Calculate objective score
    score <- metric.main(metric=metric, object=object, purpose="objective_score",
                         metric_score=score, score_mean=score_mean, outcome_type=object@outcome_type)
  }
  
  # Return score or objective score
  return(score)
}



.assess_calibration <- function(object, data, eval_times=NULL, is_pre_processed=FALSE) {
  # This is an internal function for assessing calibration of a model or ensemble

  # Check if the input classes are as expected
  if(!is_any(object, c("familiarModel", "familiarEnsemble"))){
    stop("The \".assess_calibration\" function is only applicable to objects of the \"familiarModel\" or \"familiarEnsemble\" classes.")
  }
  
  # Check data consistency of external data. We cannot transfer pre-processing state beyond this function
  if(!is(data, "dataObject")){
    data <- create_data_object(object=object,
                               data=data,
                               is_pre_processed=is_pre_processed)
  }
  
  # Check if eval_times are provided, and load from settings attribute otherwise.
  if(is.null(eval_times)){
    eval_times <- object@settings$eval_times
  }
  
  
  if(object@outcome_type %in% c("binomial", "multinomial")){
    
    # Compute calibration data
    calibration_data <- compute_calibration_data(object=object, data=data)
    
    # Calibration-in-the-large and calibration slope
    calibration_at_large <- data.table::rbindlist(lapply(split(calibration_data, by="pos_class"),
                                                         test.calibration.model, outcome_type=object@outcome_type))
    
    # Hosmer-Lemeshow tests
    calibration_gof_test <- test.calibration.hosmer_lemeshow(calibration_data=calibration_data, outcome_type=object@outcome_type)
    
    # Store to list
    calibration_list <- list("data"= calibration_data, "linear_test"=calibration_at_large, "gof_test"=calibration_gof_test)
    
  } else if(object@outcome_type %in% c("continuous", "count")) {
    
    # Compute calibration data
    calibration_data <- compute_calibration_data(object=object, data=data)
    
    # Calibration-in-the-large and calibration slope
    calibration_at_large <- test.calibration.model(calibration_data=calibration_data, outcome_type=object@outcome_type)
  
    # Hosmer-Lemeshow tests
    calibration_gof_test <- test.calibration.hosmer_lemeshow(calibration_data=calibration_data, outcome_type=object@outcome_type)
    
    # Store to list
    calibration_list <- list("data"= calibration_data, "linear_test"=calibration_at_large, "gof_test"=calibration_gof_test)
    
  } else if(object@outcome_type %in% "survival"){
    
    # Sort evaluation times.
    eval_times <- sort(eval_times)
    
    # Extract calibration data for each eval_time.
    calibration_data_list <- lapply(eval_times, function(current_eval_time, object, data){

      # Compute calibration data
      calibration_data <- compute_calibration_data(object=object, data=data, time=current_eval_time)
      
      # Calibration-in-the-large and calibration slope
      calibration_at_large <- test.calibration.model(calibration_data=calibration_data, outcome_type=object@outcome_type, eval_time=current_eval_time)
      
      # Nam-D'Agostino tests
      calibration_gof_test <- test.calibration.nam_dagostino(calibration_data=calibration_data, eval_time=current_eval_time)
      
      # Store to list
      return(list("data"= calibration_data, "linear_test"=calibration_at_large, "gof_test"=calibration_gof_test))
      
    }, object=object, data=data)
    
    # Add evaluation times as factor
    calibration_data <- rbind_list_list(calibration_data_list, list_elem="data")
    calibration_data$evaluation_time <- factor(calibration_data$evaluation_time, levels=eval_times)
    
    linear_test <- rbind_list_list(calibration_data_list, list_elem="linear_test")
    linear_test$evaluation_time <- factor(linear_test$evaluation_time, levels=eval_times)
    
    gof_test <- rbind_list_list(calibration_data_list, list_elem="gof_test")
    gof_test$evaluation_time <- factor(gof_test$evaluation_time, levels=eval_times)
    
    # Reorganise calibration_list by concatenating the underlying data sets.
    calibration_list <- list("data" = calibration_data,
                             "linear_test" = linear_test, 
                             "gof_test" = gof_test)
    
  } else {
    ..error_no_known_outcome_type(object@outcome_type)
  }
  
  return(calibration_list)
}


.compute_calibration_data <- function(object, data, time=NULL){
  # This is an internal function for computing calibration data for later use in statistical tests
  
  if(!is_any(object, c("familiarModel", "familiarEnsemble"))){
    stop("The \".compute_calibration_data\" function is only applicable to objects of the \"familiarModel\" or \"familiarEnsemble\" classes.")
  }
  
  # Check if there is any data present
  if(is(data, "dataObject")){
    if(is_empty(data)){
      return(create_empty_calibration_table(outcome_type=object@outcome_type))
    }
  }
  
  # Aggregate data.
  data <- aggregate_data(data=data)

  # Extract data
  if(object@outcome_type %in% c("survival")){
    # Calibration grouping data for survival outcomes.
    calibration_data <- learner.calibration.survival(object=object, data=data, time=time)
    
  } else if(object@outcome_type %in% c("binomial", "multinomial")){
    # Calibration grouping data for categorical outcomes.
    calibration_data <- learner.calibration.categorical(object=object, data=data)
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    # Calibration grouping data for numerical outcomes.
    calibration_data <- learner.calibration.regression(object=object, data=data)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }

  # Check for unsuccessful attempts
  if(is.null(calibration_data)){
    calibration_data <- create_empty_calibration_table(outcome_type=object@outcome_type)
    
  } else {
    # Clean up the calibration table.
    calibration_data <- strip_calibration_table(calibration_data=calibration_data, outcome_type=object@outcome_type)
  }
  
  return(calibration_data)
}



.add_model_name <- function(data, object){
  # Adds a model identifier column
  
  if(is.null(data)) return(NULL)
  
  if(!data.table::is.data.table(data)) return(data)
  
  if(nrow(data)>=1){
    
    # Get the model name
    model_name <- get_object_name(object=object, abbreviated=TRUE)
    
    # Insert "model_name" column
    data[, "model_name":=model_name]
    
    # Reorder columns and move model_name to the front
    data.table::setcolorder(data, neworder="model_name")
    
    return(data)
    
  } else {
    # In case the table is empty, return an empty table with the model name attached.
    return(cbind(data.table::data.table("model_name"=character(0)), data))
  }
}



.add_package_version <- function(object){
  # Adds the version of the familiar package used to generate the object. This allows for backward compatibility.
  
  object@familiar_version <- utils::packageVersion("familiar")
  
  return(object)
}



.save <- function(object, dir_path){
  # Saves the object to the disk. Applicable to familiarModel, familiarEnsemble,
  # and familiarData objects.
  
  # Generate directory path
  if(is(object, "familiarModel")){
    object_type <- "familiarModel"
    
  } else if(is(object, "familiarEnsemble")){
    object_type <- "familiarEnsemble"
    
  } else if(is(object, "familiarData")){
    object_type <- "familiarData"
    
  } else if(is(object, "familiarCollection")){
    object_type <- "familiarCollection"
    
  } else {
    ..error_reached_unreachable_code(".save: unknown type of object encountered.")
  }
  
  # Obtain the directory path
  dir_path  <- get_object_dir_path(dir_path=dir_path,
                                   object_type=object_type,
                                   learner=object@learner,
                                   fs_method=object@fs_method)
  
  # Generate file name
  file_name <- get_object_name(object=object)
  file_name <- paste0(file_name, ".RDS")
  
  # Check if the directory exists, and create anew if not
  if(!dir.exists(dir_path)){ dir.create(dir_path, recursive=TRUE) }
  
  # Add package version
  object <- add_package_version(object=object)
  
  # Save to disk
  saveRDS(object, file=file.path(dir_path, file_name))
}



#####has_bad_training_data#####
setMethod("has_bad_training_data", signature(object="ANY", data="dataObject"),
          function(object, data, ...){
            
            if(!(is(object, "familiarModel") | is(object, "familiarVimpMethod"))){
              ..error_reached_unreachable_code("has_bad_training_data: object is not a familiarModel or familiarVimpMethod.")
            }
            
            # Retrieve outcomeInfo object.
            if(!is.null(object@outcome_info)){
              outcome_info <- object@outcome_info
              
            } else if(!is.null(data@outcome_info)){
              outcome_info <- data@outcome_info
              
            } else {
              ..error_reached_unreachable_code("has_bad_training_data: could not find outcomeInfo object attached to familiarModel/familiarVimpMethod or dataObject.")
            }
            
            # One cannot train without data or on a single sample.
            if(is_empty(data)) return(TRUE)
            
            if(nrow(data@data) < 2) return(TRUE)
            
            if(object@outcome_type == "survival"){
              
              # Check that not all data are censored.
              censoring_variable <- outcome_info@censored
              if(all(data@data$outcome_event == censoring_variable)) return(TRUE)
              if(all(data@data$outcome_event == 0)) return(TRUE)
              
              # Check that not all data have the same survival time.
              if(all(data@data$outcome_time == data@data$outcome_time[1])) return(TRUE)
              
            } else if(object@outcome_type %in% c("binomial", "multinomial")){
              
              # Check that not all data have the same class.
              if(data.table::uniqueN(data@data$outcome) == 1) return(TRUE)
              
              # Check that all classes are present at least once.
              if(data.table::uniqueN(data@data$outcome) < nlevels(data@data$outcome)) return(TRUE)
              
            } else if(object@outcome_type %in% c("count", "continuous")){
              
              # Check that not all data have the same outcome value.
              if(all(data@data$outcome == data@data$outcome[1])) return(TRUE)
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(FALSE)
          })
