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
          function(object, data, ...){
            
            if(!(is(object, "familiarModel") | is(object, "familiarVimpMethod") | is(object, "familiarNoveltyDetector"))){
              ..error_reached_unreachable_code("has_bad_training_data: object is not a familiarModel, familiarVimpMethod or familiarNoveltyDetector.")
            }
            
            # One cannot train without data or on a single sample.
            if(is_empty(data)) return(TRUE)
            if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample")) < 2) return(TRUE)
            
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
                if(all(data@data$outcome_event == censoring_variable)) return(TRUE)
              }
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
