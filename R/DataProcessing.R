.get_run_list <- function(iteration_list, data_id, run_id=NULL){
  
  # Return an empty list if data_id equals 0.
  if(data_id == 0) return(list())
  
  # Return part of the run list.
  if(is.null(run_id)){
    return(iteration_list[[as.character(data_id)]]$run)
    
  } else {
    return(iteration_list[[as.character(data_id)]]$run[[as.character(run_id)]])
  }
}



.get_sample_identifiers <- function(run=NULL, iteration_list=NULL, data_id=NULL, run_id=NULL, train_or_validate){
  
  # Get run from iter_list if not provided directly.
  if(is.null(run)){
    run <- .get_run_list(iteration_list=iteration_list,
                         data_id=data_id,
                         run_id=run_id)
  }
  
  # Extract training or validation data - note that run$valid_samples can be NULL.
  if(train_or_validate == "train"){
    samples <- run$train_samples
    
  } else {
    samples <- run$valid_samples
  }
  
  if(!data.table::is.data.table(samples)){
    samples <- data.table::data.table("sample_id"=samples)
  }
  
  return(samples)
}



.get_iteration_identifiers <- function(run, perturb_level=NULL){
  # Extract the runs
  run_table <- run$run_table
  
  # Set the perturbation level that will be returned.
  if(is.null(perturb_level)){
    perturb_level_ <- tail(run_table, n=1)$perturb_level
    
  } else {
    perturb_level_ <- perturb_level
  }
  
  # Get the corresponding row
  run_table <- run_table[perturb_level==perturb_level_, ]
  
  # Extract data and run identifiers.
  id_list <- list("data"=run_table$data_id[1],
                  "run"=run_table$run_id[1],
                  "perturb_level"=run_table$perturb_level[1],
                  "perturbation"=run_table$perturbation[1])
  
  return(id_list)
}


.get_preprocessing_iteration_identifiers <- function(run){
  # Find the identifiers for the data and run identifiers that allow for
  # pre-processing.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  can_pre_process <- NULL
  
  # Extract the run table
  run_table <- run$run_table
  
  # Find the last entry that is available for pre-processing
  run_table <- tail(run_table[can_pre_process==TRUE, ],n=1)
  
  # Extract data and run identifiers.
  id_list <- list("data"=run_table$data_id[1],
                  "run"=run_table$run_id[1],
                  "perturb_level"=run_table$perturb_level[1],
                  "perturbation"=run_table$perturbation[1])
  
  return(id_list)
}



.get_process_step_data_identifier <- function(project_list, process_step){
  # Get the main data id for a step in the overall modelling process.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  feat_sel <- model_building <- external_validation <- NULL
  
  # Load experiment data table
  experiment_table <- project_list$experiment_setup
  
  if(process_step=="fs"){
    # Find row on where feature selection takes place and extract the main data
    # id.
    main_data_id <- experiment_table[feat_sel==TRUE, ]$main_data_id[1]
    
  } else if(process_step %in% c("mb")) {
    # Find row where model building takes place and extract the main data id.
    main_data_id <- experiment_table[model_building==TRUE, ]$main_data_id[1]
    
  } else if(process_step=="ev") {
    # Check if external validation is present; otherwise return an illegal main
    # data id.
    if(any(experiment_table$external_validation)){
      main_data_id <- experiment_table[external_validation==TRUE, ]$main_data_id[1]
      
    } else {
      main_data_id <- -1L
    }
    
  } else {
    ..error_reached_unreachable_code(paste0(".get_process_step_data_identifier: encountered unknown process step code: ", process_step))
  }
  
  return(main_data_id)
}



.find_hyperparameters_for_run <- function(run, hpo_list, allow_random_selection=FALSE){

  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL

  # Identify the right entry on hpo_list
  for(ii in rev(run$run_table$perturb_level)){
    run_id_list <- .get_iteration_identifiers(run=run, perturb_level=ii)

    # Check whether there are any matching data and run ids by determining the number of rows in the table after matching
    match_hpo <- sapply(hpo_list, function(iter_hpo, run_id_list){
      
      # Determine if there are any rows in the run_table of the parameter list that match the data and run identifiers
      # of the current level.
      match_size <- nrow(iter_hpo$run_table[data_id==run_id_list$data & run_id==run_id_list$run])
      
      # Return TRUE if any matching rows are found.
      return(match_size > 0)
      
    }, run_id_list=run_id_list)

    # If there is a match, we step out of the loop
    if(any(match_hpo)) break()
  }

  # Extract the table of parameters
  if(allow_random_selection & sum(match_hpo) > 1){
    random_set <- sample(which(match_hpo), size=1)
    
    parameter_table <- hpo_list[[random_set]]$param
    
  } else {
    parameter_table <- hpo_list[match_hpo][[1]]$param
  }

  # The table of parameter may be empty
  if(is.null(parameter_table)){
    # There are no parameters, and NULL is returned.
    return(NULL)
    
  } else {
    # Return as a list
    return(as.list(parameter_table))
  }
}



