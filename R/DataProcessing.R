.get_run_list <- function(
    iteration_list,
    data_id,
    run_id = NULL
) {
  
  # Check if data_id has any length.
  if (length(data_id) == 0L) return(list())
  
  # Return an empty list if data_id equals 0.
  if (data_id == 0L) return(list())
  
  # Return part of the run list.
  if (is.null(run_id)) {
    return(iteration_list[[as.character(data_id)]]$run)
    
  } else {
    return(iteration_list[[as.character(data_id)]]$run[[as.character(run_id)]])
  }
}



.get_sample_identifiers <- function(
    run = NULL,
    iteration_list = NULL,
    data_id = NULL,
    run_id = NULL,
    train_or_validate
) {
  
  # Get run from iter_list if not provided directly.
  if (is.null(run)) {
    run <- .get_run_list(
      iteration_list = iteration_list,
      data_id = data_id,
      run_id = run_id
    )
  }
  
  # Extract training or validation data - note that run$valid_samples can be
  # NULL.
  if (train_or_validate == "train") {
    samples <- run$train_samples
    
  } else {
    samples <- run$valid_samples
  }
  
  if (!data.table::is.data.table(samples)) {
    samples <- data.table::data.table("sample_id" = samples)
  }
  
  return(samples)
}



.get_iteration_identifiers <- function(run, perturb_level = NULL) {
  # Extract the runs
  run_table <- run$run_table
  
  # Set the perturbation level that will be returned.
  if (is.null(perturb_level)) {
    indicated_perturb_level <- tail(run_table, n = 1L)$perturb_level
    
  } else {
    indicated_perturb_level <- perturb_level
  }
  
  # Get the corresponding row
  run_table <- run_table[perturb_level == indicated_perturb_level, ]
  
  # Extract data and run identifiers.
  return(list(
    "data" = run_table$data_id[1L],
    "run" = run_table$run_id[1L],
    "perturb_level" = run_table$perturb_level[1L],
    "perturbation" = run_table$perturbation[1L]
  ))
}



.get_preprocessing_iteration_identifiers <- function(run) {
  # Find the identifiers for the data and run identifiers that allow for
  # pre-processing.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  can_pre_process <- NULL
  
  # Extract the run table
  run_table <- run$run_table
  
  # Find the last entry that is available for pre-processing
  run_table <- tail(run_table[can_pre_process == TRUE, ], n = 1L)
  
  # Extract data and run identifiers.
  return(list(
    "data" = run_table$data_id[1L],
    "run" = run_table$run_id[1L],
    "perturb_level" = run_table$perturb_level[1L],
    "perturbation" = run_table$perturbation[1L]
  ))
}



.get_process_step_data_identifier <- function(project_info, process_step) {
  # Get the main data id for a step in the overall modelling process.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  feat_sel <- model_building <- external_validation <- NULL
  
  # Load experiment data table
  experiment_table <- project_info$experiment_setup
  
  if (process_step == "fs") {
    # Find row on where feature selection takes place and extract the main data
    # id.
    main_data_id <- experiment_table[feat_sel == TRUE, ]$main_data_id[1L]
    
  } else if (process_step %in% c("mb")) {
    # Find row where model building takes place and extract the main data id.
    main_data_id <- experiment_table[model_building == TRUE, ]$main_data_id[1L]
    
  } else if (process_step == "ev") {
    # Check if external validation is present; otherwise return an illegal main
    # data id.
    if (any(experiment_table$external_validation)) {
      main_data_id <- experiment_table[external_validation == TRUE, ]$main_data_id[1L]
      
    } else {
      main_data_id <- -1L
    }
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".get_process_step_data_identifier: encountered unknown process step code: ",
      process_step
    ))
  }
  
  return(main_data_id)
}
