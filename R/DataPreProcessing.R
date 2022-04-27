run_preprocessing <- function(cl,
                              feature_info_list=NULL,
                              project_info,
                              settings,
                              file_paths,
                              message_indent=0L,
                              verbose){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- list_name <- complete <- NULL
  
  # Determine how parallel processing takes place.
  if(settings$prep$do_parallel %in% c("TRUE", "inner")){
    # Parallel processing in inner function, i.e. within each data subset.
    cl_inner <- cl
    cl_outer <- NULL
    
  } else if(settings$prep$do_parallel %in% c("outer")){
    # Parallel processing in outer loop, i.e. over all data subsets.
    cl_inner <- NULL
    cl_outer <- cl
    
    if(!is.null(cl_outer)) logger.message(paste0("\nPre-processing: Load-balanced parallel processing is done in the outer loop. ",
                                                 "No progress can be displayed."),
                                          indent=message_indent,
                                          verbose=verbose)
  } else {
    # No parallel processing.
    cl_inner <- cl_outer <- NULL
  }
  
  # Check if a feature info list was already created. This will typically
  # generate a generic feature info list when called from summon_familiar.
  if(is.null(feature_info_list)) feature_info_list <- .get_feature_info_data(data=data,
                                                                             file_paths=file_paths,
                                                                             project_id=project_info$project_id,
                                                                             outcome_type=settings$data$outcome_type)
  
  # TODO: Check if the generic contains all the required data -- particularly
  # for externally provided feature information.
  
  # Create a list of runs for which pre-processing information should be
  # obtained. First find the data ids over which should be iterated.
  data_id <- c(.get_process_step_data_identifier(project_info=project_info,
                                                 process_step="fs"),
               .get_process_step_data_identifier(project_info=project_info,
                                                 process_step="mb"))
  
  # Create a list of runs, with data_id and run_id.
  run_list <- data.table::rbindlist(lapply(unique(data_id), function(data_id, project_info){
    # Find the current data identifier for pre-processing. This may or may not
    # be data_id.
    pre_process_data_id <- .get_preprocessing_iteration_identifiers(run=.get_run_list(iteration_list=project_info$iter_list,
                                                                                      data_id=data_id,
                                                                                      run_id=1))$data
    
    # Find data and run ids.
    iteration_list <- .get_run_list(iteration_list=project_info$iter_list,
                                    data_id=pre_process_data_id)
    
    
    # Iterate over the iteration list, extract the run-table and return it.
    return(data.table::rbindlist(lapply(iteration_list, function(x) (tail(x$run_table, n=1)))))
    
  },
  project_info=project_info))
  
  # Remove duplicates.
  run_list <- unique(run_list)
  
  # Add list names and check for completeness.
  run_list[, ":="("list_name"=.get_feature_info_list_name(data_id=data_id, run_id=run_id),
                  "complete"=FALSE)]
  
  # Iterate over the runs and check which feature information lists are already
  # fully complete.
  run_list[, "complete":=feature_info_complete(feature_info_list[[list_name]]), by="list_name"]
 
  # Get all runs which are not (fully) complete, and add some additional data.
  run_list <- run_list[complete == FALSE, ]
  
  if(!is_empty(run_list)){
    # Set preprocessing run identifier and total number of datasets.
    run_list[, ":="("preprocessing_run_id"=.I, "n_preprocessing_runs"=nrow(run_list))]
    
    # Iterate over data subsets for which parameters have not yet been set.
    new_feature_info_list <- fam_mapply_lb(cl=cl_outer,
                                           assign="data",
                                           FUN=.run_preprocessing,
                                           progress_bar=!is.null(cl_outer),
                                           run=split(run_list, by=c("preprocessing_run_id")),
                                           MoreArgs=list("cl"=cl_inner,
                                                         "feature_info_list"=feature_info_list,
                                                         "project_info"=project_info,
                                                         "settings"=settings,
                                                         "message_indent"=message_indent,
                                                         "verbose"=verbose & is.null(cl_outer)))
    
    # Set names of the new feature list.
    names(new_feature_info_list) <- run_list$list_name
    
    # Update lists with feature information.
    feature_info_list[run_list$list_name] <- new_feature_info_list 
  }
  
  # Save to file, if necessary.
  if(!is.null(file_paths)){
    # Determine file name
    feature_info_file <- .get_feature_info_file_name(file_paths=file_paths,
                                                     project_id=project_info$project_id)
    
    # Write to file
    saveRDS(feature_info_list, file=feature_info_file)
  }
  
  # Attach the feature info file to the backend.
  .assign_feature_info_to_backend(feature_info_list=feature_info_list)
  
  return(feature_info_list)
}


.run_preprocessing <- function(cl=NULL,
                               run,
                               feature_info_list,
                               project_info,
                               settings,
                               message_indent,
                               verbose){
  
  
  logger.message(paste0("\nPre-processing: Starting preprocessing for run ",
                        run$preprocessing_run_id, " of ",
                        run$n_preprocessing_runs, "."),
                 indent=message_indent,
                 verbose=verbose)
  
  # Selected feature information list.
  template_feature_info <- combine_feature_info_list(preferred=feature_info_list[[run$list_name]],
                                                     custom=feature_info_list[["custom"]],
                                                     generic=feature_info_list[["generic"]])
  
  # Find pre-processing parameters
  feature_info_list <- determine_preprocessing_parameters(cl=cl,
                                                          feature_info_list=template_feature_info,
                                                          data_id=run$data_id,
                                                          run_id=run$run_id,
                                                          project_info=project_info,
                                                          settings=settings,
                                                          message_indent=message_indent+1L,
                                                          verbose=verbose)
  
  return(feature_info_list)
}




determine_preprocessing_parameters <- function(cl=NULL,
                                               feature_info_list,
                                               data_id,
                                               run_id,
                                               project_info,
                                               settings,
                                               message_indent,
                                               verbose){
  
  # Add workflow control info.
  feature_info_list <- add_control_info(feature_info_list=feature_info_list,
                                        data_id=data_id,
                                        run_id=run_id)
  
  # Add signature feature info.
  feature_info_list <- add_signature_info(feature_info_list=feature_info_list,
                                          signature=settings$data$signature)
  
  # Add novelty feature info.
  feature_info_list <- add_novelty_info(feature_info_list=feature_info_list,
                                        novelty_features=settings$data$novelty_features)
  
  # Find the run list.
  run_list <- .get_run_list(iteration_list=project_info$iter_list,
                            data_id=data_id,
                            run_id=run_id)
  
  # Select unique samples.
  sample_identifiers <- .get_sample_identifiers(run=run_list,
                                                train_or_validate="train")
  sample_identifiers <- unique(sample_identifiers)
  
  # Find currently available features.
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Create a dataObject.
  data <- methods::new("dataObject",
                       data = get_data_from_backend(sample_identifiers=sample_identifiers),
                       preprocessing_level = "none",
                       outcome_type = settings$data$outcome_type)
  
  # Remove unavailable features from the data object.
  data <- filter_features(data=data,
                          available_features=available_features)
  
  return(.determine_preprocessing_parameters(cl=cl,
                                             data=data,
                                             feature_info_list=feature_info_list,
                                             settings=settings,
                                             message_indent=message_indent,
                                             verbose=verbose))
}



.get_feature_info_file_name <- function(file_paths, project_id){
  # Generate file name of pre-processing file
  file_name <- paste0(project_id, "_feature_info.RDS")
  
  # Add file path and normalise according to the OS
  file_name <- normalizePath(file.path(file_paths$process_data_dir, file_name), mustWork=FALSE)
  
  return(file_name)
}



.get_feature_info_list_name <- function(data_id, run_id){
  return(paste0(data_id, ".", run_id))
}



.get_feature_info_list <- function(run){
  
  # Find pre-processing control element for the current run
  pre_proc_id_list <- .get_preprocessing_iteration_identifiers(run=run)
  
  # Load feature info list from backend
  feature_info_list <- get_feature_info_from_backend(data_id=pre_proc_id_list$data,
                                                     run_id=pre_proc_id_list$run)
  
  return(feature_info_list)
}



.determine_preprocessing_parameters <- function(cl=NULL,
                                                data,
                                                feature_info_list,
                                                settings,
                                                message_indent=0L,
                                                verbose=FALSE){

  if(!is(data, "dataObject")) ..error_reached_unreachable_code(".determine_preprocessing_parameters: data is not a dataObject.")
  if(is_empty(data)) stop("The provided dataset does not contain any samples.")
  if(!has_feature_data(data)) stop("The provided dataset does not contain any features.")
  
  
  ##### Remove samples with missing outcome data -------------------------------
  n_samples_current <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample"))
  logger.message(paste0("Pre-processing: ", n_samples_current, " samples were initially available."),
                 indent=message_indent,
                 verbose=verbose)
  
  # Remove all samples with missing outcome data
  data <- filter_missing_outcome(data=data, is_validation=FALSE)
  if(is_empty(data)) stop("The provided training dataset lacks outcome data.")
  
  n_samples_remain <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample"))
  n_samples_removed <- n_samples_current - n_samples_remain
  
  logger.message(paste0("Pre-processing: ", n_samples_removed,
                        " samples were removed because of missing outcome data. ",
                        n_samples_remain, " samples remain."),
                 indent=message_indent,
                 verbose=verbose)
  
  
  ##### Remove features with a large fraction of missing values ----------------
  n_features_current <- get_n_features(data)
  logger.message(paste0("Pre-processing: ", n_features_current, " features were initially available."),
                 indent=message_indent,
                 verbose=verbose)
  
  # Determine the fraction of missing values
  feature_info_list <- add_missing_value_fractions(cl=cl,
                                                   feature_info_list=feature_info_list,
                                                   data=data,
                                                   threshold=settings$prep$feature_max_fraction_missing)
  
  # Find features that are not missing too many values.
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove features with a high fraction of missing values
  data <- filter_features(data=data,
                          available_features=available_features)
  if(!has_feature_data(data)) stop(paste0("The provided dataset lacks features with sufficient available values. ",
                                          "Please investigate missing values in the dataset or increase the missingness ",
                                          "threshold by increasing the feature_max_fraction_missing configuration parameter."))
  
  # Message how many features were removed
  logger.message(paste0("Pre-processing: ", n_features_current-length(available_features),
                        " features were removed because of a high fraction of missing values. ",
                        length(available_features), " features remain."),
                 indent=message_indent,
                 verbose=verbose)
  
  n_samples_current <- n_samples_remain
  
  
  ##### Remove training samples with a large fraction of missing values --------
  
  # Remove samples with a large fraction of missing values
  data <- filter_bad_samples(data=data,
                             threshold=settings$prep$sample_max_fraction_missing)
  if(is_empty(data)) stop(paste0("The provided dataset lacks samples with sufficient available feature values. ",
                                 "Please investigate missing values in the dataset or increase the missingness ",
                                 "threshold by increasing the sample_max_fraction_missing configuration parameter."))
  
  # Message how many subjects were removed
  n_samples_remain <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample"))
  logger.message(paste0("Pre-processing: ", n_samples_current-n_samples_remain,
                        " samples were removed because of missing feature data. ",
                        n_samples_remain, " samples remain."),
                 indent=message_indent,
                 verbose=verbose)
  
  n_features_current <- length(available_features)
  
  
  ##### Remove invariant features #####
  
  # Filter features that are invariant.
  feature_info_list  <- find_invariant_features(cl=cl,
                                                feature_info_list=feature_info_list,
                                                data=data)
  # Find available features.
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Remove invariant features from the data
  data <- filter_features(data=data,
                          available_features=available_features)
  if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset only have a single value for all samples and cannot be used for training."))
  
  # Message number of features removed by the no-variance filter.
  logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                        " features were removed due to invariance. ",
                        length(available_features), " features remain."),
                 indent=message_indent,
                 verbose=verbose)
  
  
  ##### Add feature distribution data ------------------------------------------
  logger.message(paste0("Pre-processing: Adding value distribution statistics to features."),
                 indent=message_indent,
                 verbose=verbose)
    
  # Add feature distribution data
  feature_info_list <- compute_feature_distribution_data(cl=cl,
                                                         feature_info_list=feature_info_list,
                                                         data=data)
  
  
  
  ##### Transform features -----------------------------------------------------
  if(settings$prep$transform_method!="none"){
    logger.message("Pre-processing: Performing transformations to normalise feature value distributions.",
                   indent=message_indent,
                   verbose=verbose)
  }
  
  # Add skeletons to the feature information list.
  feature_info_list <- create_transformation_parameter_skeleton(feature_info_list=feature_info_list,
                                                                transformation_method=settings$prep$transform_method)
  
  # Add transformation parameters to the feature information list
  feature_info_list <- add_transformation_parameters(cl=cl,
                                                     feature_info_list=feature_info_list,
                                                     data=data,
                                                     verbose=verbose)
  
  # Apply transformation.
  data <- transform_features(data=data,
                             feature_info_list=feature_info_list)
  
  if(settings$prep$transform_method!="none"){
    logger.message("Pre-processing: Feature distributions have been transformed for normalisation.",
                   indent=message_indent,
                   verbose=verbose)
  }
  
  
  
  ##### Remove low-variance features -------------------------------------------
  if("low_variance" %in% settings$prep$filter_method){
    n_features_current <- length(available_features)
    
    # Filter features that are invariant.
    feature_info_list <- find_low_variance_features(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data=data,
                                                    settings=settings)
    
    # Check available features.
    available_features <- get_available_features(feature_info_list=feature_info_list)
    
    # Remove invariant features from the data
    data <- filter_features(data=data,
                            available_features=available_features)
    
    if(!has_feature_data(data)) stop(paste0("Remaining features in the dataset have a variance that is lower than the threshold ",
                                            "and were therefore all removed. Please investigate your data, or increase the threshold ",
                                            "through the low_var_minimum_variance_threshold configuration parameter."))
    
    # Message number of features removed by the low-variance filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                          " features were removed due to low variance. ",
                          length(available_features), " features remain."),
                   indent=message_indent,
                   verbose=verbose)
  }
  
  
  
  ##### Normalise features -----------------------------------------------------
  if(settings$prep$normalisation_method!="none"){
    logger.message("Pre-processing: Extracting normalisation parameters from feature data.",
                   indent=message_indent,
                   verbose=verbose)
  }
  
  # Add skeletons to the feature information list.
  feature_info_list <- create_normalisation_parameter_skeleton(feature_info_list=feature_info_list,
                                                               normalisation_method=settings$prep$normalisation_method)
  
  # Add normalisation parameters to the feature information list.
  feature_info_list <- add_normalisation_parameters(cl=cl,
                                                    feature_info_list=feature_info_list,
                                                    data=data,
                                                    verbose=verbose)

  # Apply normalisation to data before clustering
  data <- normalise_features(data=data,
                             feature_info_list=feature_info_list)
  
  if(settings$prep$normalisation_method!="none"){
    logger.message("Pre-processing: Feature data were normalised.",
                   indent=message_indent,
                   verbose=verbose)
  }
  
  
  ##### Batch normalise features -----------------------------------------------
  if(settings$prep$batch_normalisation_method!="none"){
    logger.message("Pre-processing: Extracting batch normalisation parameters from feature data.",
                   indent=message_indent,
                   verbose=verbose)
  }
  
  # Add batch normalisation parameters to the feature information list.
  feature_info_list <- add_batch_normalisation_parameters(cl=cl, 
                                                          feature_info_list=feature_info_list,
                                                          data_obj=data,
                                                          settings=settings,
                                                          progress_bar=verbose)
  
  # Batch-normalise feature values
  data <- batch_normalise_features(data=data,
                                   feature_info_list=feature_info_list)
  
  if(settings$prep$batch_normalisation_method!="none"){
    logger.message("Pre-processing: Feature data were batch-normalised.",
                   indent=message_indent,
                   verbose=verbose)
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
    
    # Message number of features removed by the robustness filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                          " features were removed due to low robustness. ",
                          length(available_features), " features remain."),
                   indent=message_indent,
                   verbose=verbose)
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
    
    # Message number of features removed by the importance filter.
    logger.message(paste0("Pre-processing: ", n_features_current - length(available_features),
                          " features were removed due to low importance. ",
                          length(available_features), " features remain."),
                   indent=message_indent,
                   verbose=verbose)
  }
  
  
  ##### Impute missing values #####
  logger.message("Pre-processing: Adding imputation information to features.",
                 indent=message_indent,
                 verbose=verbose)
  
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
  if(settings$prep$cluster_method != "none") logger.message("Pre-processing: Starting clustering of redundant clusters.",
                                                            indent=message_indent,
                                                            verbose=verbose)
  
  # Extract clustering information
  feature_info_list  <- add_cluster_info(cl=cl,
                                         feature_info_list=feature_info_list,
                                         data_obj=data,
                                         settings=settings,
                                         message_indent=message_indent,
                                         verbose=verbose)

  # Add required features
  feature_info_list <- add_required_features(feature_info_list=feature_info_list)
  
  # Filter features that are not required from the list
  feature_info_list <- trim_unused_features_from_list(feature_info_list=feature_info_list)
  
  # Return list of featureInfo objects
  return(feature_info_list)
}


combine_feature_info_list <- function(preferred=NULL,
                                      custom=NULL,
                                      generic=NULL){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- present <- list_name <- complete <- NULL
  
  # Get all features.
  feature_names <- c(names(preferred),
                     names(custom),
                     names(generic))
  
  # Identify which features appear where.
  data <- mapply(FUN=function(x, x_name, feature_names){
    
    # Default dataset.
    data <- data.table::data.table("list_name"=x_name,
                                   "name"=feature_names,
                                   "present"=FALSE,
                                   "complete"=FALSE)
    
    # Mark feature names that are present in the current dataset.
    data[name %in% names(x), "present":=TRUE]
    
    # Check whether data are present, are complete.
    data[present==TRUE, "complete":=feature_info_complete(object=x[[name]]), by="name"]
    
    return(data)
    
  },
  x=list("preferred"=preferred, "custom"=custom, "generic"=generic),
  x_name=c("preferred", "custom", "generic"),
  MoreArgs=list("feature_names"=feature_names),
  SIMPLIFY=FALSE)
  
  # Combine list.
  data <- data.table::rbindlist(data)
  
  # Start new feature list.
  new_feature_list <- NULL
  
  # Preference to get features with complete information first from the
  # preferred set.
  selected_feature_names <- data[list_name == "preferred" & complete == TRUE]$name
  
  # Add to feature list and remove from set of features.
  if(length(selected_feature_names) > 0){
    new_feature_list <- c(new_feature_list, preferred[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get complete information from the custom set.
  selected_feature_names <- data[name %in% feature_names & list_name == "custom" & complete == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if(length(selected_feature_names) > 0){
    new_feature_list <- c(new_feature_list, custom[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the preferred set.
  selected_feature_names <- data[name %in% feature_names & list_name == "preferred" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if(length(selected_feature_names) > 0){
    new_feature_list <- c(new_feature_list, preferred[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the custom set.
  selected_feature_names <- data[name %in% feature_names & list_name == "custom" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if(length(selected_feature_names) > 0){
    new_feature_list <- c(new_feature_list, custom[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the generic set.
  selected_feature_names <- data[name %in% feature_names & list_name == "generic" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if(length(selected_feature_names) > 0){
    new_feature_list <- c(new_feature_list, generic[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  return(new_feature_list)
}
