.add_package_version <- function(object){
  # Adds the version of the familiar package used to generate the object. This
  # allows for backward compatibility.
  
  if(is.null(object@familiar_version)){
    # Set package version.
    object@familiar_version <- utils::packageVersion("familiar")
  
  } else if(tail(object@familiar_version, n=1) < utils::packageVersion("familiar") &
            head(object@familiar_version, n=1) == "0.0.0"){
    
    # Replace version.
    object@familiar_version <- utils::packageVersion("familiar")
    
  } else if(tail(object@familiar_version, n=1) < utils::packageVersion("familiar")){
    # Check if package version differs from the currently installed version.
    # This is usually done when updating the object.
    object@familiar_version <- c(object@familiar_version,
                                 utils::packageVersion("familiar"))
  }
  
  return(object)
}



.familiar_version_string <- function(object){
  
  # Set version string.
  version_string <- paste0("v", head(object@familiar_version, n=1L))
  
  # Add version string if the object was updated from a previous version.
  if(length(object@familiar_version) > 1){
    version_string <- paste0(version_string,
                             " -> ",
                             tail(object@familiar_version, n=1L))
  }
  
  return(version_string)
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
  
  # Add file name.
  if(object@project_id == 0 & length(object@name) > 0){
    file_name <- object@name
    
  } else {
    # Generate file name
    file_name <- get_object_name(object=object)
  }
  
  # Add file extension
  file_name <- paste0(file_name, ".RDS")
  
  # Obtain the directory path
  dir_path  <- get_object_dir_path(dir_path=dir_path,
                                   object_type=object_type,
                                   learner=object@learner,
                                   fs_method=object@fs_method)
  
  
  
  # Check if the directory exists, and create anew if not
  if(!dir.exists(dir_path)){ dir.create(dir_path, recursive=TRUE) }
  
  # Add package version
  object <- add_package_version(object=object)
  
  # Save to disk
  saveRDS(object, file=file.path(dir_path, file_name))
}



#####has_bad_training_data######################################################
setMethod("has_bad_training_data", signature(object="ANY", data="dataObject"),
          function(object, data, allow_no_features=FALSE, ...){
            # Checks the data for consistency and usability. Any errors are passed as attributes
            
            if(!(is(object, "familiarModel") | is(object, "familiarVimpMethod") | is(object, "familiarNoveltyDetector"))){
              ..error_reached_unreachable_code("has_bad_training_data: object is not a familiarModel, familiarVimpMethod or familiarNoveltyDetector.")
            }
            
            # One cannot train without data or on a single sample.
            if(is_empty(data, allow_no_features=allow_no_features)){
              return_value <- TRUE
              attr(return_value, "error") <- ..error_message_no_training_data_available()
              
              return(return_value)
            } 
            
            if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample")) < 2){
              return_value <- TRUE
              attr(return_value, "error") <- "Only one sample was available to train the model."
              
              return(return_value)
            }
            
            # For familiarNoveltyDetector objects outcome is not important.
            if(is(object, "familiarNoveltyDetector")) return(FALSE)
            
            
            # Retrieve outcomeInfo object.
            if(!is.null(object@outcome_info)){
              outcome_info <- object@outcome_info
              
            } else if(!is.null(data@outcome_info)){
              outcome_info <- data@outcome_info
              
            } else {
              ..error_reached_unreachable_code("has_bad_training_data: could not find outcomeInfo object attached to familiarModel/familiarVimpMethod or dataObject.")
            }
            
            if(object@outcome_type == "survival"){
              
              # Check that not all data are censored.
              censoring_variable <- outcome_info@censored
              if(length(censoring_variable) > 0){
                if(all(data@data$outcome_event == censoring_variable)){
                  return_value <- TRUE
                  attr(return_value, "error") <- "All instances in the data set were censored. Events are required for training the model."
                  
                  return(return_value)
                } 
              }
              
              # The same as above.
              if(all(data@data$outcome_event == 0)){
                return_value <- TRUE
                attr(return_value, "error") <- "All instances in the data set were censored. Events are required for training the model."
                
                return(return_value)
              }
              
              # Check that not all data have the same survival time.
              if(all(data@data$outcome_time == data@data$outcome_time[1])){
                return_value <- TRUE
                attr(return_value, "error") <- "All instances in the data set had the same recorded survival time."
                
                return(return_value)
              } 
              
            } else if(object@outcome_type %in% c("binomial", "multinomial")){
              
              # Check that not all data have the same class.
              if(data.table::uniqueN(data@data$outcome) == 1){
                return_value <- TRUE
                attr(return_value, "error") <- "All instances in the data set were of the same class."
                
                return(return_value)
              }
              
              # Check that all classes are present at least once.
              if(data.table::uniqueN(data@data$outcome) < nlevels(data@data$outcome)){
                return_value <- TRUE
                attr(return_value, "error") <- "Some expected classes were not found in the data set."
                
                return(return_value)
              }
              
            } else if(object@outcome_type %in% c("count", "continuous")){
              
              # Check that not all data have the same outcome value.
              if(all(data@data$outcome == data@data$outcome[1])){
                return_value <- TRUE
                attr(return_value, "error") <- "All instances in the data set had the same outcome value."
                
                return(return_value)
              }
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(FALSE)
          })



.why_bad_training_data <- function(object, reason){
  # Determine the reason why training data was bad.
  
  # If the reason was FALSE, return the object as is.
  if(!reason) return(object)
  
  # Check that object is a familiarModel, because otherwise we have nowhere to
  # store the reason.
  if(!(is(object, "familiarModel"))){
    ..error_reached_unreachable_code(".why_bad_training_data: object is not a familiarModel.")
  }
  
  # Store error messages and return the object.
  return(..update_errors(object=object, attr(reason, "error")))
}
