run_model_development <- function(cl, proj_list, settings, file_paths){
  # Model building

  # Check which data object is required for performing model building
  mb_data_id <- getProcessDataID(proj_list=proj_list, process_step="mb")

  # Check whether pre-processing has been conducted
  check_pre_processing(cl=cl, data_id=mb_data_id, file_paths=file_paths, project_id=proj_list$project_id)

  # Get runs
  run_list <- getRunList(iter_list=proj_list$iter_list, data_id=mb_data_id)

  # Identify combinations of feature selection methods and learners
  run_methods <- get_fs_learner_combinations(settings=settings)

  # Remove parallel clusters for model building in case these are not required.
  if(!settings$mb$do_parallel){
    cl_mb <- NULL
  } else {
    cl_mb <- cl
  }

  # Create models by iterating over combination of feature selection methods and learners
  for(iter_methods in run_methods){

    # Check which runs have been completed and skip if all methods were analysed
    iter_run_list <- add_model_data_to_run_list(methods=iter_methods, run_list=run_list,
                                                proj_list=proj_list, settings=settings,
                                                file_paths=file_paths, filter_existing=TRUE)
    
    # Skip if all learners were assessed
    if(length(iter_run_list)==0) {
      next()
    }

    # Message
    logger.message(paste0("\nModel building: starting model building using \"",
                          iter_methods$learner, "\" learner, based on \"",
                          iter_methods$fs_method, "\" feature selection."))
    
    # Optimise hyperparameters of models used for model building
    hpo_list      <- run_hyperparameter_optimisation(cl=cl,
                                                     proj_list=proj_list,
                                                     data_id=mb_data_id,
                                                     settings=settings,
                                                     file_paths=file_paths,
                                                     fs_method=iter_methods$fs_method,
                                                     learner=iter_methods$learner)
    
    # Build models
    fam_lapply_lb(cl=cl_mb,
                  assign="all",
                  X=iter_run_list,
                  FUN=build_model,
                  progress_bar=TRUE,
                  hpo_list=hpo_list)

    logger.message(paste0("Model building: model building using \"",
                          iter_methods$learner, "\" learner, based on \"",
                          iter_methods$fs_method, "\" feature selection, has been completed."))
  }
}


build_model <- function(run, hpo_list){
  # Function for model building and data extraction
  
  # Load from the familiar or global environment
  proj_list         <- get_project_list()
  settings          <- get_settings()
  file_paths        <- get_file_paths()

  # Data will be loaded at run time in .train
  data <- methods::new("dataObject",
                       data = NULL,
                       is_pre_processed = FALSE,
                       outcome_type = settings$data$outcome_type,
                       delay_loading = TRUE,
                       perturb_level = tail(run$run_table, n=1)$perturb_level,
                       load_validation =FALSE,
                       aggregate_on_load = FALSE,
                       outcome_info = create_outcome_info(settings=settings))
  
  ############### Initialisation ##################################################################

  # Get hyper-parameters
  param_list        <- .find_hyperparameters_for_run(run=run, hpo_list=hpo_list)

  # Get feature ranks
  dt_ranks          <- rank.get_feature_ranks(run=run, fs_method=run$fs_method, settings=settings, proj_list=proj_list, file_paths=file_paths)


  ############### Data preparation ################################################################

  # Load feature_info_list
  feature_info_list <- get_feature_info_list(run=run)
  
  # Select features
  selected_features <- get_signature(feature_info_list=feature_info_list, dt_ranks=dt_ranks, fs_method=run$fs_method, param=param_list, settings=settings)
  
  # Generate data object
  # data_obj          <- apply_pre_processing(run=run, train_or_validate="train", selected_features=selected_features)
  # 
  # Find features that are required for processing the data
  required_features <- find_required_features(features=selected_features, feature_info_list=feature_info_list)

  # Find important features, i.e. those that constitute the signature either individually or as part of a cluster
  important_features <- find_important_features(features=selected_features, feature_info_list=feature_info_list)
  
  # Find pre-processing data
  feature_info_list <- feature_info_list[required_features]

  ############### Model building ################################################################

  # Create familiar model
  fam_model <- methods::new("familiarModel",
                            outcome_type = settings$data$outcome_type,
                            learner = run$learner,
                            fs_method = run$fs_method,
                            run_table = run$run_table,
                            hyperparameters = param_list,
                            hyperparameter_data = NULL,
                            signature = selected_features,
                            req_feature_cols =  required_features,
                            important_features = important_features,
                            feature_info = feature_info_list,
                            outcome_info = .get_outcome_info(),
                            project_id = proj_list$project_id,
                            settings = settings$eval)
  
  # Add package version
  fam_model <- add_package_version(object=fam_model)

  # Train model
  fam_model <- .train(object=fam_model, data=data, get_additional_info=TRUE)

  # Save model
  save(list=fam_model, file=file_paths$mb_dir)
}


add_model_data_to_run_list <- function(methods, run_list, proj_list, settings, file_paths, filter_existing){

  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL

  # Extract learner and fs_method from methods
  fs_method <- methods$fs_method
  learner   <- methods$learner

  # Get table with data and run ids from run_list
  dt_run    <- data.table::rbindlist(lapply(run_list, function(run) (tail(run$run_table, n=1))))

  # Construct file names
  dt_run[, "mb_file":=get_object_file_name(learner=learner, fs_method=fs_method, project_id=proj_list$project_id,
                                           data_id=data_id, run_id=run_id, object_type="familiarModel")]

  # Add file names and methods to run_list
  run_list  <- lapply(seq_len(nrow(dt_run)), function(ii, dt, run_list, fs_method, learner)
    (append(run_list[[ii]], c("mb_file"=dt$mb_file[ii], "fs_method"=fs_method, "learner"=learner))),
    dt=dt_run, run_list=run_list, fs_method=fs_method, learner=learner)

  # # Determine if the directory where the model building files will be stored exists, and create it otherwise
  # mb_dir    <- unique(dirname(dt_run$mb_file))
  # if(!dir.exists(mb_dir)){ dir.create(mb_dir, recursive=TRUE) }

  # Filter runs corresponding to existing models from the run list
  if(filter_existing == TRUE) {
    # Find the model directory
    mb_dir    <- get_object_dir_path(dir_path=file_paths$mb_dir, object_type="familiarModel", learner=learner, fs_method=fs_method)

    # Determine which files already exist in the model building directory
    file_list <- list.files(path=mb_dir, pattern=paste0(proj_list$project_id, "_", learner), full.names=FALSE)

    # Select only those runs which have not been completed
    run_list <- run_list[!dt_run$mb_file %in% file_list]
  }

  return(run_list)
}



get_fs_learner_combinations <- function(settings) {

  # Generate all combinations
  dt <- data.table::as.data.table(expand.grid(fs_method=settings$fs$fs_methods, learner=settings$mb$learners, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE))

  # Parse methods to a list of lists
  run_methods <- lapply(seq_len(nrow(dt)), function(ii, dt) (list("fs_method"=dt$fs_method[ii], "learner"=dt$learner[ii])), dt=dt)

  return(run_methods)
}
