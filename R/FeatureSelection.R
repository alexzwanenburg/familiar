run_variable_importance_computation <- function(
    cl,
    project_list,
    settings,
    file_paths,
    message_indent = 0L,
    verbose = TRUE
) {
  # Check which data object is required for computing variable importances.
  vimp_data_id <- .get_process_step_data_identifier(
    project_info = project_list,
    process_step = "vimp"
  )

  # Get variable importance methods that still need to be checked.
  run_vimp_methods <- .find_missing_variable_importance_data(
    project_list = project_list,
    settings = settings,
    file_paths = file_paths
  )

  # Get runs
  run_list <- .get_run_list(
    iteration_list = project_list$iter_list,
    data_id = vimp_data_id
  )

  # Remove cluster information in case no parallelisation is provided.
  cl_fs <- NULL
  if (settings$vimp$do_parallel) cl_fs <- cl

  # Create variable importance matrices by iterating over variable importance
  # methods
  for (curr_vimp_method in run_vimp_methods) {
    logger_message(
      paste0(
        "\nVariable importance: starting variable importance computation using \"",
        curr_vimp_method, "\" method."
      ),
      indent = message_indent,
      verbose = verbose
    )

    # Optimise models used for variable importance
    hpo_list <- run_hyperparameter_optimisation(
      cl = cl,
      project_list = project_list,
      data_id = vimp_data_id,
      settings = settings,
      file_paths = file_paths,
      vimp_method = curr_vimp_method,
      message_indent = message_indent + 1L,
      verbose = verbose
    )

    # Create variable importance information by iterating over the list of runs.
    vimp_list <- fam_mapply_lb(
      cl = cl_fs,
      assign = "all",
      FUN = compute_variable_importance,
      run = run_list,
      progress_bar = verbose,
      MoreArgs = list(
        "vimp_method" = curr_vimp_method,
        "hpo_list" = hpo_list,
        "settings" = settings
      )
    )

    # Save to file
    saveRDS(vimp_list, file = .get_variable_importance_data_filename(
      project_id = project_list$project_id,
      vimp_method = curr_vimp_method,
      file_paths = file_paths
    ))

    # Message that variable importances have been computed.
    logger_message(
      paste0(
        "Variable importance: variable importance have been computed using the \"",
        curr_vimp_method, "\" method."
      ),
      indent = message_indent,
      verbose = verbose
    )
  }

  invisible(TRUE)
}



compute_variable_importance <- function(
    run,
    vimp_method,
    hpo_list,
    settings
) {
  # Function for calculating variable importance

  # Data will be loaded at run time in .vimp.
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

  # Load hyperparameters, if any.
  parameter_list <- .find_hyperparameters_for_run(
    run = run,
    hpo_list = hpo_list,
    as_list = TRUE
  )

  # Load feature_info_list
  feature_info_list <- .get_feature_info_list(run = run)

  # Create the variable importance method object or familiar model object to
  # compute variable importance with.
  vimp_object <- methods::new("familiarVimpMethod",
    outcome_type = data@outcome_type,
    hyperparameters = parameter_list,
    vimp_method = vimp_method,
    outcome_info = .get_outcome_info(),
    run_table = run$run_table
  )

  # Promote to the correct subclass.
  vimp_object <- promote_vimp_method(object = vimp_object)

  # Set multivariate methods.
  if (is(vimp_object, "familiarModel")) is_multivariate <- TRUE
  if (is(vimp_object, "familiarVimpMethod")) is_multivariate <- vimp_object@multivariate

  # Find required features. Exclude the signature features at this point, as
  # these will have been dropped from the variable importance table.
  required_features <- get_required_features(
    x = data,
    feature_info_list = feature_info_list,
    exclude_signature = !is_multivariate
  )

  # Limit to required features.
  vimp_object@required_features <- required_features
  vimp_object@feature_info <- feature_info_list[required_features]

  # Compute variable importance.
  vimp_object <- .vimp(
    object = vimp_object, 
    data = data
  )

  return(vimp_object)
}



.find_missing_variable_importance_data <- function(
    project_list, 
    settings, 
    file_paths
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  vimp_method <- fs_file <- NULL

  # All variable importance methods
  file_table <- data.table::data.table("vimp_method" = settings$vimp$vimp_methods)

  # Add expected variable importance file names
  file_table[, "fs_file" := .get_variable_importance_data_filename(
    project_id = project_list$project_id,
    vimp_method = vimp_method,
    file_paths = file_paths
  )]

  # List files in directory
  file_list <- normalizePath(
    list.files(
      path = file_paths$vimp_dir,
      pattern = paste0(project_list$project_id, "_vimp_"),
      full.names = TRUE,
      recursive = TRUE
    ),
    mustWork = FALSE
  )

  # Remove files which have already been selected
  file_table <- file_table[!fs_file %in% file_list, ]

  return(file_table$vimp_method)
}



.get_variable_importance_data_filename <- function(
    vimp_method, 
    project_id, 
    file_paths,
    old = FALSE
) {
  if (old) {
    return(normalizePath(
      file.path(
        file_paths$vimp_dir,
        paste0(project_id, "_fs_", vimp_method, ".RDS")
      ),
      mustWork = FALSE
    ))
  } else {
    return(normalizePath(
      file.path(
        file_paths$vimp_dir,
        paste0(project_id, "_vimp_", vimp_method, ".RDS")
      ),
      mustWork = FALSE
    )) 
  }
}



.retrieve_variable_importance_data <- function(
    vimp_method, 
    project_list, 
    file_paths
) {
  # Iterate over the variable importance methods to select the correct files.
  vimp_table_list <- lapply(
    vimp_method, 
    function(x, project_list, file_paths) {
      # Attempt to read the object. This should produce a list of variable
      # importance tables.
      vimp_table_sub_list <- tryCatch(
        readRDS(.get_variable_importance_data_filename(
          vimp_method = x,
          project_id = project_list$project_id,
          file_paths = file_paths
        )),
        error = identity
      )
      
      # Check if there were issues reading the file.
      if (inherits(vimp_table_sub_list, "error")) {
        # Check if the file used an older style.
        vimp_table_sub_list <- tryCatch(
          readRDS(.get_variable_importance_data_filename(
            vimp_method = x,
            project_id = project_list$project_id,
            file_paths = file_paths,
            old = TRUE
          )),
          error = identity
        )
        
        # Check if there were issues reading the file.
        if (inherits(vimp_table_sub_list, "error")) return(NULL)
      } 
      
      # Iterate over contents of the variable importance table list to update
      # these to the latest definitions.
      vimp_table_sub_list <- lapply(
        vimp_table_sub_list, 
        function(x, project_id) {
          # Upgrade the object for backward compatibility.
          if (!is(x, "vimpTable")) x <- as_vimp_table_object(x, project_id = project_id)
          
          # Update variable importance table objects to the most recent
          # definition.
          return(update_object(x))
        },
        project_id = project_list$project_id
      )
      
      # Return list of vimpTable objects.
      return(vimp_table_sub_list)
    },
    project_list = project_list,
    file_paths = file_paths
  )

  # Update names
  names(vimp_table_list) <- vimp_method

  return(vimp_table_list)
}
