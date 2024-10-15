run_model_development <- function(
    cl,
    project_list,
    settings,
    file_paths,
    message_indent = 0L,
    verbose = TRUE
) {
  # Model building
  
  # Check which data object is required for performing model building
  mb_data_id <- .get_process_step_data_identifier(
    project_info = project_list,
    process_step = "mb"
  )
  
  # Get runs
  run_list <- .get_run_list(
    iteration_list = project_list$iter_list,
    data_id = mb_data_id
  )
  
  # Identify combinations of variable importance methods and learners
  run_methods <- get_fs_learner_combinations(settings = settings)
  
  # Remove parallel clusters for model building in case these are not required.
  cl_mb <- NULL
  if (settings$mb$do_parallel) {
    cl_mb <- cl
  }
  
  # Create models by iterating over combination of variable importance methods
  # and learners
  for (iter_methods in run_methods) {
    # Check which runs have been completed and skip if all methods were
    # analysed.
    iter_run_list <- add_model_data_to_run_list(
      methods = iter_methods,
      run_list = run_list,
      project_list = project_list,
      settings = settings,
      file_paths = file_paths,
      filter_existing = TRUE
    )
    
    # Skip if all learners were assessed
    if (length(iter_run_list) == 0L) next
    
    # Message
    logger_message(
      paste0(
        "\nModel building: starting model building using \"",
        iter_methods$learner, "\" learner, based on \"",
        iter_methods$vimp_method, "\" variable importances."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
    # Optimise hyperparameters of models used for model building
    hpo_list <- run_hyperparameter_optimisation(
      cl = cl,
      project_list = project_list,
      data_id = mb_data_id,
      settings = settings,
      file_paths = file_paths,
      vimp_method = iter_methods$vimp_method,
      learner = iter_methods$learner,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    # Build models
    fam_lapply_lb(
      cl = cl_mb,
      assign = "all",
      X = iter_run_list,
      FUN = build_model,
      progress_bar = verbose,
      hpo_list = hpo_list
    )
    
    logger_message(
      paste0(
        "Model building: model building using \"",
        iter_methods$learner, "\" learner, based on \"",
        iter_methods$vimp_method, "\" variable importances, has been completed."
      ),
      indent = message_indent,
      verbose = verbose
    )
  }
}


build_model <- function(run, hpo_list) {
  # Function for model building and data extraction
  
  # Load from the familiar or global environment
  project_list <- get_project_list()
  settings <- get_settings()
  file_paths <- get_file_paths()
  
  # Data will be loaded at run time in .train
  data <- methods::new(
    "dataObject",
    data = NULL,
    preprocessing_level = "none",
    outcome_type = settings$data$outcome_type,
    delay_loading = TRUE,
    perturb_level = tail(run$run_table, n = 1L)$perturb_level,
    load_validation = FALSE,
    aggregate_on_load = FALSE,
    outcome_info = create_outcome_info(settings = settings)
  )
  
  # Load feature_info_list
  feature_info_list <- .get_feature_info_list(run = run)
  
  # Get hyper-parameters
  hyperparameter_object <- .find_hyperparameters_for_run(
    run = run,
    hpo_list = hpo_list
  )
  
  # Read variable importance file and retrieve the variable importance table
  # objects.
  vimp_table_list <- .retrieve_variable_importance_data(
    vimp_method = run$vimp_method,
    project_list = project_list,
    file_paths = file_paths
  )[[run$vimp_method]]
  
  # Collect all relevant variable importance
  vimp_table_list <- collect_vimp_table(
    x = vimp_table_list,
    run_table = run$run_table
  )
  
  # Update using reference cluster table to ensure that the data are correct
  # locally.
  vimp_table_list <- update_vimp_table_to_reference(
    x = vimp_table_list,
    reference_cluster_table = .create_clustering_table(
      feature_info_list = feature_info_list
    )
  )
  
  # Recluster the data according to the clustering table corresponding to the
  # model.
  vimp_table_list <- recluster_vimp_table(vimp_table_list)
  
  # Get feature ranks
  vimp_table <- aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = settings$vimp$aggregation,
    rank_threshold = settings$vimp$aggr_rank_threshold
  )
  
  # Extract rank table.
  rank_table <- get_vimp_table(vimp_table)
  
  # Create familiar model
  fam_model <- methods::new(
    "familiarModel",
    outcome_type = settings$data$outcome_type,
    learner = run$learner,
    vimp_method = run$vimp_method,
    run_table = run$run_table,
    hyperparameters = hyperparameter_object@hyperparameters,
    hyperparameter_data = hyperparameter_object@hyperparameter_data,
    feature_info = feature_info_list,
    outcome_info = .get_outcome_info(),
    project_id = project_list$project_id,
    settings = settings$eval
  )
  
  # Select features
  fam_model <- set_signature(
    object = fam_model,
    rank_table = rank_table,
    minimise_footprint = FALSE
  )
  
  # Add package version
  fam_model <- add_package_version(object = fam_model)
  
  # Train model
  fam_model <- .train(
    object = fam_model,
    data = data,
    get_additional_info = TRUE
  )
  
  # Add novelty detector
  fam_model <- .train_novelty_detector(
    object = fam_model,
    data = data,
    detector = settings$mb$novelty_detector,
    user_list = settings$mb$detector_parameters[[settings$mb$novelty_detector]],
    get_additional_info = TRUE
  )
  
  # Add model name
  fam_model <- set_object_name(fam_model)
  
  # Save model
  save(
    list = fam_model,
    file = file_paths$mb_dir
  )
}



add_model_data_to_run_list <- function(
    methods,
    run_list,
    project_list, 
    settings, 
    file_paths,
    filter_existing
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL
  
  # Extract learner and vimp_method from methods
  vimp_method <- methods$vimp_method
  learner <- methods$learner
  
  # Get table with data and run ids from run_list
  run_data <- data.table::rbindlist(lapply(
    run_list,
    function(run) (tail(run$run_table, n = 1L))
  ))
  
  # Construct file names
  run_data[, "mb_file" := get_object_file_name(
    learner = learner,
    vimp_method = vimp_method,
    project_id = project_list$project_id,
    data_id = data_id, 
    run_id = run_id,
    object_type = "familiarModel"
  )]
  
  # Add file names and methods to run_list
  run_list <- mapply(
    function(run_list, file_path, vimp_method, learner) {
      return(c(
        run_list,
        list(
          "mb_file" = file_path,
          "vimp_method" = vimp_method,
          "learner" = learner
        )
      ))
    },
    run_list,
    run_data$mb_file,
    MoreArgs = list(
      "vimp_method" = vimp_method,
      "learner" = learner
    ),
    SIMPLIFY = FALSE
  )
  
  # Filter runs corresponding to existing models from the run list
  if (filter_existing == TRUE) {
    # Find the model directory
    mb_dir <- get_object_dir_path(
      dir_path = file_paths$mb_dir, 
      object_type = "familiarModel",
      learner = learner, 
      vimp_method = vimp_method
    )
    
    # Determine which files already exist in the model building directory
    file_list <- list.files(
      path = mb_dir,
      pattern = paste0(project_list$project_id, "_", learner),
      full.names = FALSE
    )
    
    # Select only those runs which have not been completed
    run_list <- run_list[!run_data$mb_file %in% file_list]
  }
  
  return(run_list)
}



get_fs_learner_combinations <- function(settings) {
  # Generate all combinations
  combination_data <- data.table::as.data.table(expand.grid(
    vimp_method = settings$vimp$vimp_methods,
    learner = settings$mb$learners,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ))
  
  # Parse methods to a list of lists
  run_methods <- lapply(
    seq_len(nrow(combination_data)),
    function(ii, data) {
      list(
        "vimp_method" = data$vimp_method[ii],
        "learner" = data$learner[ii]
      )
    }, 
    data = combination_data
  )
  
  return(run_methods)
}
