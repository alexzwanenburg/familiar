run_feature_selection <- function(cl,
                                  project_list,
                                  settings,
                                  file_paths,
                                  message_indent=0L,
                                  verbose=TRUE){

  # Check which data object is required for performing feature selection.
  fs_data_id <- .get_process_step_data_identifier(project_info=project_list,
                                                  process_step="fs")
  
  # Get feature selection methods that still need to be checked.
  run_fs_methods <- .find_missing_feature_selection_data(project_list=project_list,
                                                         settings=settings,
                                                         file_paths=file_paths)

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
    
    logger.message(paste0("\nFeature selection: starting feature selection using \"", curr_fs_method, "\" method."),
                   indent=message_indent,
                   verbose=verbose)
    
    # Optimise models used for feature selection
    hpo_list <- run_hyperparameter_optimisation(cl=cl,
                                                project_list=project_list,
                                                data_id=fs_data_id,
                                                settings=settings,
                                                file_paths=file_paths,
                                                vimp_method=curr_fs_method,
                                                message_indent=message_indent + 1L,
                                                verbose=verbose)
    
    # Create variable importance information by iterating over the list of runs.
    vimp_list <- fam_mapply_lb(cl=cl_fs,
                               assign="all",
                               FUN=compute_variable_importance,
                               run=run_list,
                               progress_bar=verbose,
                               MoreArgs=list("fs_method"=curr_fs_method,
                                             "hpo_list"=hpo_list,
                                             "settings"=settings))
    
    # Save to file
    saveRDS(vimp_list, file=.get_feature_selection_data_filename(project_list=project_list,
                                                                 fs_method=curr_fs_method,
                                                                 file_paths=file_paths))

    # Message that feature selection has been completed.
    logger.message(paste0("Feature selection: feature selection using \"", curr_fs_method, "\" method has been completed."),
                   indent=message_indent,
                   verbose=verbose)
  }
}



compute_variable_importance <- function(run, fs_method, hpo_list, settings){
  # Function for calculating variable importance
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- NULL
  
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
  parameter_list <- .find_hyperparameters_for_run(run=run,
                                                  hpo_list=hpo_list,
                                                  as_list=TRUE)

  ############## Variable importance calculation ##################################################
  
  # Load feature_info_list
  feature_info_list <- .get_feature_info_list(run=run)
  
  # Create the variable importance method object or familiar model object to
  # compute variable importance with.
  vimp_object <- methods::new("familiarVimpMethod",
                              outcome_type = data@outcome_type,
                              hyperparameters = parameter_list,
                              vimp_method = fs_method,
                              outcome_info = .get_outcome_info(),
                              run_table = run$run_table)
  
  # Promote to the correct subclass.
  vimp_object <- promote_vimp_method(object=vimp_object)
  
  # Set multivariate methods.
  if(is(vimp_object, "familiarModel")) is_multivariate <- TRUE
  if(is(vimp_object, "familiarVimpMethod")) is_multivariate <- vimp_object@multivariate
  
  # Find required features. Exclude the signature features at this point, as
  # these will have been dropped from the variable importance table.
  browser()
  required_features <- get_required_features(x=data,
                                             feature_info_list=feature_info_list,
                                             exclude_signature=!is_multivariate)
  # required_features <- find_required_features(features=get_available_features(feature_info_list=feature_info_list,
  #                                                                             exclude_signature=!is_multivariate),
  #                                             feature_info_list=feature_info_list)
  
  # Limit to required features.
  vimp_object@required_features <- required_features
  vimp_object@feature_info <- feature_info_list[required_features]
  
  # Compute variable importance.
  vimp_object <- .vimp(object=vimp_object, data=data)
  
  return(vimp_object)
}
  # 
  # if(is_multivariate){
  #   vimp_table <- remove_signature_features(vimp_table,
  #                                           features=names(feature_info_list)[sapply(feature_info_list, is_in_signature)])
  # }
  # # 
  # # Post-processing on the variable importance data table
  # if(nrow(vimp_table) == 0){
  #   # If the variable importance data table is empty, return an empty table
  #   vimp_table$data_id   <- numeric(0)
  #   vimp_table$run_id    <- numeric(0)
  #   vimp_table$fs_method <- character(0)
  #   
  # } else {
  #   # Obtain the list of identifiers.
  #   id_list <- .get_iteration_identifiers(run=run)
  #   
  #   # Add identifiers to variable importance data table
  #   vimp_table$data_id   <- id_list$data
  #   vimp_table$run_id    <- id_list$run
  #   vimp_table$fs_method <- fs_method
  # }
  # 
  # # Remove signature features from the table, and re-rank.
  # if(is_multivariate & nrow(vimp_table) > 0){
  #   
  #   # Identify signature features, if any.
  #   signature_features <- names(feature_info_list)[sapply(feature_info_list, is_in_signature)]
  #   
  #   if(length(signature_features) > 0){
  #     # Remove signature features.
  #     vimp_table <- vimp_table[!name %in% signature_features]
  #     
  #     if(nrow(vimp_table) > 0){
  #       # Re-rank features after removing signature features.
  #       vimp_table[, "rank":=data.table::frank(rank, ties.method="min")]
  #     }
  #   }
  # }
# 
#   # Generate the translation table for the selected set of features.
#   translation_table <- rank.get_decluster_translation_table(features=vimp_table$name,
#                                                             feature_info_list=feature_info_list[required_features])
#   
#   return(list("run_table"=run$run_table,
#               "fs_method"=fs_method,
#               "vimp"=vimp_table,
#               "translation_table"=translation_table))



.find_missing_feature_selection_data <- function(project_list, settings, file_paths){

  # Suppress NOTES due to non-standard evaluation in data.table
  fs_method <- fs_file <- NULL

  # All feature selection methods
  file_table <- data.table::data.table("fs_method"=settings$fs$fs_methods)

  # Add expected feature selection file names
  file_table[, "fs_file":=.get_feature_selection_data_filename(project_list=project_list, 
                                                               fs_method=fs_method,
                                                               file_paths=file_paths)]

  # List files in directory
  file_list <- normalizePath(list.files(path=file_paths$fs_dir,
                                        pattern=paste0(project_list$project_id, "_fs_"),
                                        full.names=TRUE,
                                        recursive=TRUE),
                             mustWork=FALSE)

  # Remove files which have already been selected
  file_table <- file_table[!fs_file %in% file_list, ]

  return(file_table$fs_method)
}



.get_feature_selection_data_filename <- function(fs_method, project_list, file_paths){
  
  return(normalizePath(file.path(file_paths$fs_dir,
                                 paste0(project_list$project_id, "_fs_", fs_method, ".RDS")),
                       mustWork=FALSE))
}
