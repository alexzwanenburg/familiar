run_feature_selection <- function(cl, proj_list, settings, file_paths){

  # Check which data object is required for performing feature selection
  fs_data_id <- getProcessDataID(proj_list=proj_list, process_step="fs")

  # Get feature selection methods that still need to be checked
  run_fs_methods <- .find_missing_feature_selection_data(proj_list=proj_list, settings=settings, file_paths=file_paths)

  # Check whether pre-processing has been conducted
  check_pre_processing(cl=cl, data_id=fs_data_id, file_paths=file_paths, project_id=proj_list$project_id)

  # Get runs
  run_list <- getRunList(iter_list=proj_list$iter_list, data_id=fs_data_id)

  # Start cluster for parallel processing
  if(settings$fs$do_parallel){
    parallel::clusterExport(cl=cl, varlist=c("compute_variable_importance"), envir=environment())
  }

  # Create variable importance matrices by iterating over feature selection methods
  for(curr_fs_method in run_fs_methods){

    # Message
    logger.message(paste0("\nFeature selection: starting feature selection using \"", curr_fs_method, "\" method."))

    # Optimise models used for feature selection
    hpo_list <- run_hyperparameter_optimisation(cl=cl, proj_list=proj_list, data_id=fs_data_id, settings=settings, file_paths=file_paths, fs_method=curr_fs_method)

    # Extract variable importance lists
    if(settings$fs$do_parallel){
      # Calculate variable importance
      vimp_list <- parallel::parLapply(cl=cl, X=run_list, fun=compute_variable_importance, fs_method=curr_fs_method,
                                       hpo_list=hpo_list, proj_list=proj_list, settings=settings, file_paths=file_paths)
    } else{
      # Start text progress bar
      pb_conn   <- utils::txtProgressBar(min=0, max=length(run_list), style=3)

      # Add iteration number for progress bar
      run_list  <- lapply(seq_len(length(run_list)), function(ii, run_list) (append(run_list[[ii]], c("iter_nr"=ii))), run_list=run_list)

      # Calculate variable importance
      vimp_list <- lapply(run_list, compute_variable_importance, fs_method=curr_fs_method,
                          hpo_list=hpo_list, proj_list=proj_list, settings=settings, file_paths=file_paths, pb_conn=pb_conn)

      # Close progress bar
      close(pb_conn)
    }

    # Save to file
    saveRDS(vimp_list, file=getFSFileName(proj_list=proj_list, fs_method=curr_fs_method, file_paths=file_paths))

    # Message that feature selection has been completed.
    logger.message(paste0("Feature selection: feature selection using \"", curr_fs_method, "\" method has been completed."))
  }
}



compute_variable_importance <- function(run, fs_method, hpo_list, proj_list, settings, file_paths, pb_conn=NULL){
  # Function for calculating variable importance


  ############### Data preparation ################################################################
  # Pre-process data
  data_obj <- apply_pre_processing(run=run, train_or_validate="train")
  
  ############## Select parameters ################################################################
  param_list <- .find_hyperparameters_for_run(run=run, hpo_list=hpo_list)

  ############## Variable importance calculation ##################################################
  vimp_table <- vimp.assess_variable_importance(data_obj=data_obj, method=fs_method, param=param_list)

  # Post-processing on the variable importance data table
  if(nrow(vimp_table)==0){
    # If the variable importance data table is empty, return an empty table
    vimp_table$data_id   <- numeric(0)
    vimp_table$run_id    <- numeric(0)
    vimp_table$fs_method <- character(0)
  } else {
    id_list <- getIterID(run=run)

    # Add identifiers to variable importance data table
    vimp_table$data_id   <- id_list$data
    vimp_table$run_id    <- id_list$run
    vimp_table$fs_method <- fs_method
  }

  # Generate the translation table for the selected set of features.
  translation_table <- rank.get_decluster_translation_table(features=vimp_table$name,
                                                            feature_info_list=get_feature_info_list(run=run))

  # Update progress bar
  if(!is.null(pb_conn)){
    utils::setTxtProgressBar(pb=pb_conn, value=run$iter_nr)
  }

  return(list("run_table"=run$run_table, "fs_method"=fs_method, "vimp"=vimp_table, "translation_table"=translation_table))
}



.find_missing_feature_selection_data <- function(proj_list, settings, file_paths){

  # Suppress NOTES due to non-standard evaluation in data.table
  fs_method <- fs_file <- NULL

  # All feature selection methods
  dt_fs <- data.table::data.table("fs_method"=settings$fs$fs_methods)

  # Add expected feature selection file names
  dt_fs[,"fs_file":=getFSFileName(proj_list=proj_list, fs_method=fs_method, file_paths=file_paths)]

  # List files in directory
  file_list <- normalizePath(list.files(path=file_paths$fs_dir, pattern=paste0(proj_list$project_id, "_fs_"), full.names=TRUE, recursive=TRUE), mustWork=FALSE)

  # Remove files which have already been selected
  dt_fs <- dt_fs[!fs_file %in% file_list, ]

  return(dt_fs$fs_method)

}



getFSFileName <- function(fs_method, proj_list, file_paths){
  return(normalizePath(file.path(file_paths$fs_dir, paste0(proj_list$project_id, "_fs_", fs_method, ".RDS")), mustWork=FALSE))
}
