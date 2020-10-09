get_feature_info_file <- function(file_paths, project_id){
  # Generate file name of pre-processing file
  file_name <- paste0(project_id, "_feature_info.RDS")
  
  # Add file path and normalise according to the OS
  file_name <- normalizePath(file.path(file_paths$process_data_dir, file_name), mustWork=FALSE)
  
  return(file_name)
}


get_feature_info_list <- function(run){
  
  # Find pre-processing control element for the current run
  pre_proc_id_list <- getPreprocessingID(run=run)
  
  # Load feature info list from backend
  feature_info_list <- get_feature_info_from_backend(data_id=pre_proc_id_list$data,
                                                     run_id=pre_proc_id_list$run)
  
  return(feature_info_list)
}


check_pre_processing <- function(cl, data_id, file_paths, project_id){

  # Get the feature info list from the backend
  feature_info_list <- get_feature_info_from_backend(data_id=waiver(),
                                                     run_id=waiver())
  
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
    list_name <- .get_feature_info_list_name(data_id=pre_process_data_id, run_id=run_id)
    
    # Determine if the pre-processing data has already been stored
    if(!is.null(feature_info_list[[list_name]])) next()
    
    logger.message(paste0("\nPre-processing: Starting preprocessing for run ", run_id, " of ", length(all_runs), "."))
    
    # Find pre-processing parameters
    feature_info_list[[list_name]] <- determine_pre_processing_parameters(cl=cl, data_id=pre_process_data_id, run_id=run_id)

    # Write to file
    saveRDS(feature_info_list, file=feature_info_file)
    
    # Set to backend
    .assign_feature_info_to_backend(feature_info_list=feature_info_list)
  }
}



determine_pre_processing_parameters <- function(cl, data_id, run_id){

  # Get settings file
  settings <- get_settings()
    
  # Get the generic feature information list
  feature_info_list <- get_feature_info_from_backend()
  
  # Add workflow control info
  feature_info_list <- add_control_info(feature_info_list=feature_info_list, data_id=data_id, run_id=run_id)
  
  # Add signature feature info
  feature_info_list <- add_signature_info(feature_info_list=feature_info_list, signature=settings$data$signature)

  # Add novelty feature info
  feature_info_list <- add_novelty_info(feature_info_list=feature_info_list,
                                        novelty_features=settings$data$novelty_features)
  
  # Find the run list
  run_list     <- getRunList(iter_list=get_project_list()$iter_list, data_id=data_id, run_id=run_id)
  run_subj_id  <- unique(getSubjectIDs(run=run_list, train_or_validate="train"))

  # Find currently available features
  available_features <- get_available_features(feature_info_list=feature_info_list)

  # Create a dataObject
  data_obj <- methods::new("dataObject",
                           data=get_data_from_backend(sample_identifiers=run_subj_id),
                           preprocessing_level="none",
                           outcome_type=settings$data$outcome_type)
  
  # Remove unavailable features from the data object.
  data_obj <- filter_features(data=data_obj, available_features=available_features)
  
  # Unload cluster locally, if it is not required
  if(!settings$prep$do_parallel) cl <- NULL 
  
  return(.determine_preprocessing_parameters(cl=cl,
                                             data=data_obj,
                                             feature_info_list = feature_info_list,
                                             settings=settings,
                                             verbose=TRUE))
}

.determine_preprocessing_parameters <- function(cl=NULL,
                                                data,
                                                feature_info_list,
                                                settings,
                                                verbose=FALSE){

  if(!is(data, "dataObject")) ..error_reached_unreachable_code(".determine_preprocessing_parameters: data is not a dataObject.")
  if(is_empty(data)) stop("The provided dataset does not contain any samples.")
  if(!has_feature_data(data)) stop("The provided dataset does not contain any features.")
  
  
  ##### Remove samples with missing outcome data #####
  if(verbose){
    n_samples_current <- uniqueN(data@data, by=c("subject_id", "cohort_id"))
    logger.message(paste0("Pre-processing: ", n_samples_current, " samples were initially available."))
  }
  
  # Remove all samples with missing outcome data
  data <- filter_missing_outcome(data=data, is_validation=FALSE)
  if(is_empty(data)) stop("The provided training dataset lacks outcome data.")
  
  if(verbose){
    n_samples_remain <- uniqueN(data@data, by=c("subject_id", "cohort_id"))
    
    n_samples_removed <- n_samples_current - n_samples_remain
    logger.message(paste0("Pre-processing: ", n_samples_removed,
                          " samples were removed because of missing outcome data. ",
                          n_samples_remain, " samples remain."))
  }
  
  
  ##### Remove features with a large fraction of missing values #####
  if(verbose){
    n_features_current <- get_n_features(data)
    logger.message(paste0("Pre-processing: ", n_features_current, " features were initially available."))
  }
  
  # Determine the fraction of missing values
  feature_info_list <- add_missing_value_fractions(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data=data,
                                                    threshold=settings$prep$feature_max_fraction_missing)
  
  # Find features that are not missing too many values.
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove features with a high fraction of missing values
  data <- filter_features(data=data, available_features=available_features)
  if(!has_feature_data(data)) stop(paste0("The provided dataset lacks features with sufficient available values. ",
                                          "Please investigate missing values in the dataset or increase the missingness ",
                                          "threshold by increasing the feature_max_fraction_missing configuration parameter."))
  
  if(verbose){
    # Message how many features were removed
    logger.message(paste0("Pre-processing: ", n_features_current-length(available_features),
                          " features were removed because of a high fraction of missing values. ",
                          length(available_features), " features remain."))
    
    n_samples_current <- n_samples_remain
  }
  
  
  ##### Remove training samples with a large fraction of missing values #####
  
  # Remove samples with a large fraction of missing values
  data <- filter_bad_samples(data=data, threshold=settings$prep$sample_max_fraction_missing)
  if(is_empty(data)) stop(paste0("The provided dataset lacks samples with sufficient available feature values. ",
                                 "Please investigate missing values in the dataset or increase the missingness ",
                                 "threshold by increasing the sample_max_fraction_missing configuration parameter."))
  
  # Message how many subjects were removed
  if(verbose){
    n_samples_remain <- uniqueN(data@data, by=c("subject_id", "cohort_id"))
    logger.message(paste0("Pre-processing: ", n_samples_current-n_samples_remain,
                          " samples were removed because of missing feature data. ",
                          n_samples_remain, " samples remain."))
    
    n_features_current <- length(available_features)
  }
  
  
  ##### Remove invariant features #####
  
  # Filter features that are invariant.
  feature_info_list  <- find_invariant_features(cl=cl, feature_info_list=feature_info_list, data_obj=data)
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove invariant features from the data
  data <- filter_features(data=data, available_features=available_features)
  if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset only have a single value for all samples and cannot be used for training."))
  
  if(verbose){
    # Message number of features removed by the no-variance filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                          " features were removed due to invariance. ",
                          length(available_features), " features remain."))
  }
  
  
  ##### Add feature distribution data ------------------------------------------
  if(verbose){
    logger.message(paste0("Pre-processing: Adding value distribution statistics to features."))
  }
    
  # Add feature distribution data
  feature_info_list <- compute_feature_distribution_data(cl=cl, feature_info_list=feature_info_list, data_obj=data)
  
  
  
  ##### Transform features #####
  if(settings$prep$transform_method!="none" & verbose){
    logger.message("Pre-processing: Performing transformations to normalise feature value distributions.")
  }
  
  # Add transformation parameters to the feature information list
  feature_info_list <- add_transformation_parameters(cl=cl, feature_info_list=feature_info_list, data_obj=data, settings=settings)
  
  # Apply transformation before normalisation
  data <- transform_features(data=data, feature_info_list=feature_info_list)
  
  if(settings$prep$transform_method!="none" & verbose){
    logger.message("Pre-processing: Feature distributions have been transformed for normalisation.")
  }
  
  
  
  ##### Remove low-variance features #####
  if("low_variance" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are invariant.
    feature_info_list  <- find_low_variance_features(cl=cl,
                                                     feature_info_list=feature_info_list,
                                                     data_obj=data,
                                                     settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove invariant features from the data
    data <- filter_features(data=data, available_features=available_features)
    if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset have a variance that is lower than the threshold ",
                                            "and were therefore all removed. Please investigate your data, or increase the threshold ",
                                            "through the low_var_minimum_variance_threshold configuration parameter."))
    
    if(verbose){
      # Message number of features removed by the low-variance filter.
      logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                            " features were removed due to low variance. ",
                            length(available_features), " features remain."))
    }
  }
  
  
  
  ##### Normalise features #####
  if(settings$prep$normalisation_method!="none" & verbose){
    logger.message("Pre-processing: Extracting normalisation parameters from feature data.")
  }
  
  # Add normalisation parameters to the feature information list
  feature_info_list <- add_normalisation_parameters(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data_obj=data,
                                                    settings=settings)

  # Apply normalisation to data before clustering
  data          <- normalise_features(data=data,
                                      feature_info_list=feature_info_list)
  
  if(settings$prep$normalisation_method!="none" & verbose){
    logger.message("Pre-processing: Feature data were normalised.")
  }
  
  
  ##### Batch normalise features #####
  if(settings$prep$batch_normalisation_method!="none" & verbose){
    logger.message("Pre-processing: Extracting batch normalisation parameters from feature data.")
  }
  
  # Add batch normalisation parameters to the feature information list.
  feature_info_list <- add_batch_normalisation_parameters(cl=cl, 
                                                          feature_info_list=feature_info_list,
                                                          data_obj=data,
                                                          settings=settings)
  
  # Batch-normalise feature values
  data <- batch_normalise_features(data=data,
                                   feature_info_list=feature_info_list)
  
  if(settings$prep$batch_normalisation_method!="none" & verbose){
    logger.message("Pre-processing: Feature data were batch-normalised.")
  }
  
  
  
  ##### Remove non-robust features #####
  if("robustness" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are not robust
    feature_info_list  <- find_non_robust_features(cl=cl,
                                                   feature_info_list=feature_info_list,
                                                   data_obj=data,
                                                   settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove non-robust features from the data
    data <- filter_features(data=data,
                            available_features=available_features)
    if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset have a robustness that is lower than the threshold ",
                                            "and were therefore all removed. Please investigate your data, or decrease the threshold ",
                                            "through the robustness_threshold_value configuration parameter."))
    
    if(verbose){
    # Message number of features removed by the robustness filter.
      logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                            " features were removed due to low robustness. ",
                            length(available_features), " features remain."))
    }
  }
  
  
  
  ##### Remove unimportant features #####
  if("univariate_test" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are not relevant
    feature_info_list  <- find_unimportant_features(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data_obj=data,
                                                    settings=settings)
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove unimportant features from the data
    data <- filter_features(data=data, available_features=available_features)
    if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset have a p-value that is higher than the threshold ",
                                            "and were therefore all removed. Please investigate your data, or increase the threshold ",
                                            "through the univariate_test_threshold configuration parameter."))
    
    if(verbose){
      # Message number of features removed by the importance filter.
      logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                            " features were removed due to low importance. ",
                            length(available_features), " features remain."))
    }
  }
  
  
  ##### Impute missing values #####
  if(verbose) logger.message("Pre-processing: Adding imputation information to features.")
  
  # Add imputation info
  feature_info_list <- add_imputation_info(cl=cl,
                                           feature_info_list=feature_info_list,
                                           data_obj=data,
                                           settings=settings,
                                           verbose=verbose)
  
  # Impute features with censored data prior to clustering
  data <- impute_features(data=data,
                          feature_info_list=feature_info_list)
  
  
  ##### Cluster features #####
  if(verbose & settings$prep$cluster_method != "none") logger.message("Pre-processing: Starting clustering of redundant clusters.")
  
  # Extract clustering information
  feature_info_list  <- add_cluster_info(cl=cl,
                                         feature_info_list=feature_info_list,
                                         data_obj=data,
                                         settings=settings,
                                         verbose=verbose)

  # Add required features
  feature_info_list <- add_required_features(feature_info_list=feature_info_list)
  
  # Filter features that are not required from the list
  feature_info_list <- trim_unused_features_from_list(feature_info_list=feature_info_list)
  
  # Return list of featureInfo objects
  return(feature_info_list)
}
