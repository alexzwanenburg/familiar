run_feature_selection <- function(cl, project_list, settings, file_paths, message_indent=0L){

  # Check which data object is required for performing feature selection
  fs_data_id <- .get_preprocessing_iteration_identifiers(project_list=project_list,
                                                         process_step="fs")
  
  # Get feature selection methods that still need to be checked
  run_fs_methods <- .find_missing_feature_selection_data(proj_list=project_list,
                                                         settings=settings,
                                                         file_paths=file_paths)

  # Check whether pre-processing has been conducted
  check_pre_processing(cl=cl,
                       data_id=fs_data_id,
                       file_paths=file_paths,
                       project_id=project_list$project_id)

  # Get runs
  run_list <- .get_run_list(iteration_list=project_list$iter_list,
                            data_id=fs_data_id)
  
  # Remove cluster information in case no parallelisation is provided.
  if(!settings$fs$do_parallel){
    cl_fs <- NULL
  } else {
    cl_fs <- cl
  }
  
  # Create variable importance matrices by iterating over feature selection methods
  for(curr_fs_method in run_fs_methods){

    # Message
    logger.message(paste0("\nFeature selection: starting feature selection using \"", curr_fs_method, "\" method."),
                   indent=message_indent)

    # Optimise models used for feature selection
    hpo_list <- run_hyperparameter_optimisation(cl=cl,
                                                project_list=project_list,
                                                data_id=fs_data_id,
                                                settings=settings,
                                                file_paths=file_paths,
                                                fs_method=curr_fs_method,
                                                message_indent=message_indent + 1L)
    
    # Create variable importance information by iterating over the list of runs.
    vimp_list <- fam_mapply_lb(cl=cl_fs,
                               assign="all",
                               FUN=compute_variable_importance,
                               run=run_list,
                               progress_bar=TRUE,
                               MoreArgs=list("fs_method"=curr_fs_method,
                                             "hpo_list"=hpo_list,
                                             "proj_list"=project_list,
                                             "settings"=settings,
                                             "file_paths"=file_paths))
    
    # Save to file
    saveRDS(vimp_list, file=.get_feature_selection_data_filename(proj_list=project_list,
                                                                 fs_method=curr_fs_method,
                                                                 file_paths=file_paths))

    # Message that feature selection has been completed.
    logger.message(paste0("Feature selection: feature selection using \"", curr_fs_method, "\" method has been completed."),
                   indent=message_indent)
  }
}



compute_variable_importance <- function(run, fs_method, hpo_list, proj_list, settings, file_paths){
  # Function for calculating variable importance
  
  ############### Data preparation ################################################################
  # Pre-process data
  
  # Data will be loaded at run time in .vimp.
  data <- methods::new("dataObject",
                       data = NULL,
                       preprocessing_level="none",
                       outcome_type = settings$data$outcome_type,
                       delay_loading = TRUE,
                       perturb_level = tail(run$run_table, n=1)$perturb_level,
                       load_validation =FALSE,
                       aggregate_on_load = FALSE,
                       outcome_info = create_outcome_info(settings=settings))
  
  ############## Select parameters ################################################################
  parameter_list <- .find_hyperparameters_for_run(run=run, hpo_list=hpo_list)

  ############## Variable importance calculation ##################################################
  
  # Load feature_info_list
  feature_info_list <- get_feature_info_list(run=run)
  
  # Find required features
  required_features <- find_required_features(features=get_available_features(feature_info_list=feature_info_list,
                                                                              exclude_signature=TRUE),
                                              feature_info_list=feature_info_list)
  
  # Limit to required features. In principle, this removes signature features
  # which are not assessed through variable importance.
  feature_info_list <- feature_info_list[required_features]
  
  # Create the variable importance method object or familiar model object to
  # compute variable importance with.
  vimp_object <- methods::new("familiarVimpMethod",
                              outcome_type = data@outcome_type,
                              hyperparameters = parameter_list,
                              vimp_method = fs_method,
                              outcome_info = .get_outcome_info(),
                              feature_info = feature_info_list,
                              required_features = required_features,
                              run_table = run$run_table)

  # Compute variable importance.
  vimp_table <- .vimp(object=vimp_object, data=data)
  
  # Post-processing on the variable importance data table
  if(nrow(vimp_table) == 0){
    # If the variable importance data table is empty, return an empty table
    vimp_table$data_id   <- numeric(0)
    vimp_table$run_id    <- numeric(0)
    vimp_table$fs_method <- character(0)
    
  } else {
    # Obtain the list of identifiers.
    id_list <- .get_iteration_identifiers(run=run)

    # Add identifiers to variable importance data table
    vimp_table$data_id   <- id_list$data
    vimp_table$run_id    <- id_list$run
    vimp_table$fs_method <- fs_method
  }

  # Generate the translation table for the selected set of features.
  translation_table <- rank.get_decluster_translation_table(features=vimp_table$name,
                                                            feature_info_list=feature_info_list)

  return(list("run_table"=run$run_table, "fs_method"=fs_method, "vimp"=vimp_table, "translation_table"=translation_table))
}



.find_missing_feature_selection_data <- function(proj_list, settings, file_paths){

  # Suppress NOTES due to non-standard evaluation in data.table
  fs_method <- fs_file <- NULL

  # All feature selection methods
  dt_fs <- data.table::data.table("fs_method"=settings$fs$fs_methods)

  # Add expected feature selection file names
  dt_fs[,"fs_file":=.get_feature_selection_data_filename(proj_list=proj_list, fs_method=fs_method, file_paths=file_paths)]

  # List files in directory
  file_list <- normalizePath(list.files(path=file_paths$fs_dir, pattern=paste0(proj_list$project_id, "_fs_"), full.names=TRUE, recursive=TRUE), mustWork=FALSE)

  # Remove files which have already been selected
  dt_fs <- dt_fs[!fs_file %in% file_list, ]

  return(dt_fs$fs_method)

}



.get_feature_selection_data_filename <- function(fs_method, proj_list, file_paths){
  return(normalizePath(file.path(file_paths$fs_dir, paste0(proj_list$project_id, "_fs_", fs_method, ".RDS")), mustWork=FALSE))
}
