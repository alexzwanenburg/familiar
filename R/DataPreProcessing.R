get_feature_info_file <- function(file_paths, project_id){
  # Generate file name of pre-processing file
  file_name <- paste0(project_id, "_feature_info.RDS")
  
  # Add file path and normalise according to the OS
  file_name <- normalizePath(file.path(file_paths$process_data_dir, file_name), mustWork=FALSE)
  
  return(file_name)
}



.assign_feature_info_to_global <- function(feature_info_list){
  # Put feature_info_list into the familiar environment
  assign("feature_info_list", feature_info_list, envir=familiar_global_env)
}



get_feature_info_from_backend <- function(){
  
  # Retrieve the paths to files and directories
  if(exists("familiar_global_env")){
    if(exists("feature_info_list", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("feature_info_list", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("Feature info was not found in backend.")
    }
  } else if (exists("feature_info_list", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("Feature info was not found in backend.")
  }
  
  return(get("feature_info_list", envir=data_env))
}



get_feature_info_list_name <- function(data_id, run_id){
  return(paste0(data_id, ".", run_id))
}



get_feature_info_list <- function(run){
  
  # Find pre-processing control element for the current run
  pre_proc_id_list <- getPreprocessingID(run=run)
  
  # Load feature info list from backend
  feature_info_list <- get_feature_info_from_backend()[[get_feature_info_list_name(data_id=pre_proc_id_list$data,
                                                                                   run_id=pre_proc_id_list$run)]]
  
  return(feature_info_list)
}


check_pre_processing <- function(cl, data_id, file_paths, project_id){

  # Get the feature info list from the backend
  feature_info_list <- get_feature_info_from_backend()

  # Identify the data_id for pre-processing
  pre_process_data_id <- getPreprocessingID(run=getRunList(iter_list=get_project_list()$iter_list, data_id=data_id, run_id=1))$data
  
  # Find the iteration list
  iter_list <- getRunList(iter_list=get_project_list()$iter_list, data_id=pre_process_data_id)
  
  # Determine all available runs
  all_runs <- names(iter_list)
  
  # Determine file name
  feature_info_file <- get_feature_info_file(file_paths=file_paths, project_id=project_id)
  
  # Iterate over runs
  for(run_id in all_runs){
    
    # Define the name of the feature_info_list entry
    list_name <- get_feature_info_list_name(data_id=pre_process_data_id, run_id=run_id)
    
    # Determine if the pre-processing data has already been stored
    if(!is.null(feature_info_list[[list_name]])){
      next()
    }
    
    logger.message(paste0("\nPre-processing: Starting preprocessing for run ", run_id, " of ", length(all_runs), "."))
    
    # Find pre-processing parameters
    feature_info_list[[list_name]] <- determine_pre_processing_parameters(cl=cl, data_id=pre_process_data_id, run_id=run_id)

    # Write to file
    saveRDS(feature_info_list, file=feature_info_file)
    
    # Set to backend
    .assign_feature_info_to_global(feature_info_list=feature_info_list)
    
  }
}


apply_pre_processing <- function(cl=NULL, run, train_or_validate, selected_features=NULL, exclude_signature=FALSE){

  # Get settings file
  settings <- get_settings()
  
  # Identify samples
  run_id_list <- getIterID(run=run)
  sample_id <- getSubjectIDs(iter_list=get_project_list()$iter_list, data_id=run_id_list$data,
                             run_id=run_id_list$run, train_or_validate=train_or_validate)
  
  
  # Return an empty data set in case no samples are found.
  if(is.null(sample_id)){
    return(methods::new("dataObject", data=NULL, is_pre_processed=FALSE, outcome_type=settings$data$outcome_type))
  } else {
    unique_samples <- unique(sample_id)
  }
  
  # Get the feature information list
  feature_info_list <- get_feature_info_list(run=run)
  
  # Find currently available features
  if(is.null(selected_features)){
    selected_features <- get_available_features(feature_info_list=feature_info_list, exclude_signature=exclude_signature)
    selected_features <- features_after_clustering(features=selected_features, feature_info_list=feature_info_list)
  }
  
  # Find all features to generate the selected features
  required_features <- find_required_features(features=selected_features, feature_info_list=feature_info_list)
  
  # Find non-feature columns
  non_feature_cols <- get_non_feature_columns(x=settings$data$outcome_type)
  
  # Create a dataObject
  data <- methods::new("dataObject", data=NULL, is_pre_processed=TRUE, outcome_type=settings$data$outcome_type)
  
  # Read data from external data server. A slice is requested for the relevant subjects (rows) and columns
  data@data <- getDataFromBackend(subj_id=unique_samples, col_names=c(non_feature_cols, required_features), settings=settings)

  # Check whether the dataset has any features.
  if(!has_feature_data(x=data)){
    
    # Recreate iteration prior to passing a mostly empty data set.
    data <- select_data_from_samples(data=data, samples=sample_id)
    
    return(data)
  }
  
  # Remove unavailable features from the data object, as precaution.
  data <- filter_features(data=data, available_features=required_features)
  
  # Transform features
  data <- transform_features(data=data, feature_info_list=feature_info_list)
  
  # Normalise feature values
  data <- normalise_features(data=data, feature_info_list=feature_info_list)
  
  # Batch-normalise feature values
  data <- batch_normalise_features(data=data, feature_info_list=feature_info_list)
  
  # Impute missing values
  data <- impute_features(data=data, feature_info_list=feature_info_list)
  
  # Cluster features
  data <- cluster_features(data=data, feature_info_list=feature_info_list)
  
  # Recreate iteration
  data <- select_data_from_samples(data=data, samples=sample_id)
  
  # Remove features that were required for processing, but are no longer necessary
  data <- filter_features(data=data, available_features=selected_features)
  
  return(data)
}


determine_pre_processing_parameters <- function(cl, data_id, run_id){

  # Get settings file
  settings <- get_settings()
    
  # Get the generic feature information list
  feature_info_list <- get_feature_info_from_backend()[["generic"]]
  
  # Add workflow control info
  feature_info_list  <- add_control_info(feature_info_list=feature_info_list, data_id=data_id, run_id=run_id)
  
  # Add signature feature info
  feature_info_list  <- add_signature_info(feature_info_list=feature_info_list, signature=settings$data$signature)

  # Find the run list
  run_list     <- getRunList(iter_list=get_project_list()$iter_list, data_id=data_id, run_id=run_id)
  run_subj_id  <- unique(getSubjectIDs(run=run_list, train_or_validate="train"))

  # Find currently available features
  available_features <- get_available_features(feature_info_list=feature_info_list)

  # Create a dataObject
  data_obj           <- methods::new("dataObject", data=NULL, is_pre_processed=TRUE, outcome_type=settings$data$outcome_type)
  
  # Read data from external data server. A slice is requested for the relevant subjects (rows) and columns
  data_obj@data      <- getDataFromBackend(subj_id=run_subj_id, settings=settings)
  
  # Remove unavailable features from the data object.
  data_obj           <- filter_features(data=data_obj, available_features=available_features)
   
  # Unload cluster locally, if it is not required
  if(!settings$prep$do_parallel) cl <- NULL 
  
  ##### Remove samples with missing outcome data #####
  n_samples_current  <- length(run_subj_id)
  logger.message(paste0("Pre-processing: ", n_samples_current, " samples were initially available."))
  
  # Remove all samples with missing outcome data
  data_obj           <- filter_missing_outcome(data=data_obj, is_validation=FALSE)
  n_samples_remain   <- uniqueN(data_obj@data, by=c("subject_id", "cohort_id"))
  
  n_samples_removed  <- n_samples_current - n_samples_remain
  logger.message(paste0("Pre-processing: ", n_samples_removed, " samples were removed because of missing outcome data. ",
                        n_samples_remain, " samples remain."))
  rm("n_samples_removed", "n_samples_current", "run_subj_id")

  
  
  ##### Remove features with a large fraction of missing values #####
  n_features_current <- length(available_features)
  logger.message(paste0("Pre-processing: ", n_features_current, " features were initially available."))
  
  # Determine the fraction of missing values
  feature_info_list  <- add_missing_value_fractions(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data=data_obj,
                                                    threshold=settings$prep$feat_max_fract_missing)
  
  # Find features that are not missing too many values.
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove features with a high fraction of missing values
  data_obj           <- filter_features(data=data_obj, available_features=available_features)
  
  # Message how many features were removed
  logger.message(paste0("Pre-processing: ", n_features_current-length(available_features), " features were removed because of a high fraction of missing values. ",
                        length(available_features), " features remain."))
  
  rm("n_features_current")
  
  
  
  ##### Remove training samples with a large fraction of missing values #####
  n_samples_current  <- n_samples_remain
  
  # Remove samples with a large fraction of missing values
  data_obj           <- filter_bad_samples(data=data_obj, threshold=settings$prep$subj_max_fract_missing)
  
  # Message how many subjects were removed
  n_samples_remain   <- uniqueN(data_obj@data, by=c("subject_id", "cohort_id"))
  logger.message(paste0("Pre-processing: ", n_samples_current-n_samples_remain, " samples were removed because of missing feature data. ",
                        n_samples_remain, " samples remain."))
  
  rm("n_samples_current", "n_samples_remain")
  
  
  
  ##### Remove invariant features #####
  n_features_current <- length(available_features)
  
  # Filter features that are invariant.
  feature_info_list  <- find_invariant_features(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj)
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove invariant features from the data
  data_obj           <- filter_features(data=data_obj, available_features=available_features)
  
  # Message number of features removed by the no-variance filter.
  logger.message(paste0("Pre-processing: ", n_features_current - length(available_features), " features were removed due to invariance. ",
                        length(available_features), " features remain."))
  rm("n_features_current")
  
  
  
  ##### Add feature distribution data ------------------------------------------
  logger.message(paste0("Pre-processing: Adding value distribution statistics to features."))
  
  # Add feature distribution data
  feature_info_list <- compute_feature_distribution_data(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj)
  
  
  
  ##### Transform features #####
  if(settings$prep$transform_method!="none"){
    logger.message("Pre-processing: Performing transformations to normalise feature value distributions.")
  }
  
  # Add transformation parameters to the feature information list
  feature_info_list <- add_transformation_parameters(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)
  
  # Apply transformation before normalisation
  data_obj          <- transform_features(data=data_obj, feature_info_list=feature_info_list)
  
  if(settings$prep$transform_method!="none"){
    logger.message("Pre-processing: Feature distributions have been transformed for normalisation.")
  }
  
  
  
  ##### Remove low-variance features #####
  if("low_variance" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are invariant.
    feature_info_list  <- find_low_variance_features(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove invariant features from the data
    data_obj           <- filter_features(data=data_obj, available_features=available_features)
    
    # Message number of features removed by the low-variance filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features), " features were removed due to low variance. ",
                          length(available_features), " features remain."))
    rm("n_features_current")
  }
  
  
  
  ##### Normalise features #####
  if(settings$prep$normalisation_method!="none"){
    logger.message("Pre-processing: Extracting normalisation parameters from feature data.")
  }
  
  # Add normalisation parameters to the feature information list
  feature_info_list <- add_normalisation_parameters(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)

  # Apply normalisation to data before clustering
  data_obj          <- normalise_features(data=data_obj, feature_info_list=feature_info_list)
  
  if(settings$prep$normalisation_method!="none"){
    logger.message("Pre-processing: Feature data were normalised.")
  }
  
  
  ##### Batch normalise features #####
  if(settings$prep$batch_normalisation_method!="none"){
    logger.message("Pre-processing: Extracting batch normalisation parameters from feature data.")
  }
  
  # Add batch normalisation parameters to the feature information list.
  feature_info_list <- add_batch_normalisation_parameters(cl=cl, feature_info_list=feature_info_list,
                                                          data_obj=data_obj, settings=settings)
  
  # Batch-normalise feature values
  data_obj <- batch_normalise_features(data=data_obj, feature_info_list=feature_info_list)
  
  if(settings$prep$batch_normalisation_method!="none"){
    logger.message("Pre-processing: Feature data were batch-normalised.")
  }
  
  
  
  ##### Remove non-robust features #####
  if("robustness" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are not robust
    feature_info_list  <- find_non_robust_features(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove non-robust features from the data
    data_obj           <- filter_features(data=data_obj, available_features=available_features)
    
    # Message number of features removed by the robustness filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features), " features were removed due to low robustness. ",
                          length(available_features), " features remain."))
    rm("n_features_current")
  }
  
  
  
  ##### Remove unimportant features #####
  if("univariate_test" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are not relevant
    feature_info_list  <- find_unimportant_features(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove unimportant features from the data
    data_obj           <- filter_features(data=data_obj, available_features=available_features)
    
    # Message number of features removed by the importance filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features), " features were removed due to low importance. ",
                          length(available_features), " features remain."))
    rm("n_features_current")
  }
  
  
  ##### Impute missing values #####
  
  logger.message("Pre-processing: Adding imputation information to features.")
  
  # Add imputation info
  feature_info_list   <- add_imputation_info(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)
  
  # Impute features with censored data prior to clustering
  data_obj            <- impute_features(data=data_obj, feature_info_list=feature_info_list)
  
  
  ##### Cluster features #####

  logger.message("Pre-processing: Starting clustering of redundant clusters.")
  
  # Extract clustering information
  feature_info_list   <- add_cluster_info(cl=cl, feature_info_list=feature_info_list, data_obj=data_obj, settings=settings)

  # Add required features
  feature_info_list   <- add_required_features(feature_info_list=feature_info_list)
  
  # Return list of featureInfo objects
  return(feature_info_list)
}
