selectData <- function(dt, proj_list, main_data_id, main_run_id, process_step){
  # Selects data from data table dt based on current run

  if(process_step %in% c("fs", "mb", "model_eval_train")){
    iter_subjects <- proj_list$iter_list[[as.character(main_data_id)]]$train_id[[main_run_id]]
  } else if (process_step == "model_eval_valid") {
    iter_subjects <- proj_list$iter_list[[as.character(main_data_id)]]$valid_id[[main_run_id]]
  }

  # Make subselection based on iter_subjects
  dt <- selectDataFromSubjects(dt=dt, subj_id=iter_subjects)

  return(dt)
}


selectDataFromSubjects <- function(dt, subj_id){

  # Suppress NOTES due to non-standard evaluation in data.table
  cohort_id <- NULL

  # Check if any subjects are returned, otherwise return an empty data set dt
  if(is.null(subj_id)) { return( head(dt,0) ) }

  # Select subjects by merging on subject_id - this allows the same subjects to be present multiple times when bootstrapping
  dt_iter <- data.table::data.table("subject_id"=subj_id)
  dt      <- merge(dt_iter, dt, by="subject_id", all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)

  # Some subjects may have been previously removed due to lack of data: therefore we only use data with a valid cohort_id.
  dt <- dt[!is.na(cohort_id), ]

  return(dt)
}


getRunList <- function(iter_list, data_id, run_id=NULL){
  # If data_id is 0, an empty list is returned
  if(data_id==0){
    return(list())
  }

  if(is.null(run_id)){
    return(iter_list[[as.character(data_id)]]$run)
  } else {
    return(iter_list[[as.character(data_id)]]$run[[as.character(run_id)]])
  }
}


getIterID <- function(run, perturb_level=NULL){
  # Extract the runs
  dt <- run$run_table

  # Set the perturbation level
  if(is.null(perturb_level)){
    perturb_level_ <- tail(dt, n=1)$perturb_level
  } else {
    perturb_level_ <- perturb_level
  }

  # Get the corresponding row
  dt <- dt[perturb_level==perturb_level_,]

  # Extract data and run ids
  id_list <- list("data"=dt$data_id[1], "run"=dt$run_id[1], "perturb_level"=dt$perturb_level[1], "perturbation"=dt$perturbation[1])

  return(id_list)
}


getSubjectIDs <- function(run=NULL, iter_list=NULL, data_id=NULL, run_id=NULL, train_or_validate){

  # Get run from iter_list if not provided directly.
  if(is.null(run)){
    run <- getRunList(iter_list=iter_list, data_id=data_id, run_id=run_id)
  }

  # Extract training or validation data - note that run$valid_samples can be NULL.
  if(train_or_validate=="train"){
    return(run$train_samples)
  } else {
    return(run$valid_samples)
  }
}



getPreprocessingID <- function(run){
  # Find the identifiers for the data and run that may be pre-processed

  # Suppress NOTES due to non-standard evaluation in data.table
  can_pre_process <- NULL

  # Extract the runs
  dt <- run$run_table

  # Find the last entry that is available for pre-processing
  dt <- tail(dt[can_pre_process==TRUE, ],n=1)

  # Extract data and run ids
  id_list <- list("data"=dt$data_id[1], "run"=dt$run_id[1], "perturb_level"=dt$perturb_level[1])

  return(id_list)

}



getProcessDataID <- function(proj_list, process_step){
  # Get the main data id for a process

  # Suppress NOTES due to non-standard evaluation in data.table
  feat_sel <- model_building <- external_validation <- NULL

  # Load experiment data table
  dt_exp <- proj_list$experiment_setup

  if(process_step=="fs"){
    # Find row on dt_exp where feature selection takes place and extract the main data id
    main_data_id <- dt_exp[feat_sel==TRUE, ]$main_data_id[1]
  }
  if(process_step %in% c("mb")) {
    # Find row where model building takes place and extract the main data id
    main_data_id <- dt_exp[model_building==TRUE, ]$main_data_id[1]
  }
  if(process_step=="ev") {
    # Check if external validation is present; otherwise return an illegal main data id
    if(any(dt_exp$external_validation)){
      main_data_id <- dt_exp[external_validation==TRUE, ]$main_data_id[1]
    } else {
      main_data_id <- -1
    }
  }

  return(main_data_id)
}


.find_hyperparameters_for_run <- function(run, hpo_list){

  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL

  # Identify the right entry on hpo_list
  for(ii in rev(run$run_table$perturb_level)){
    run_id_list <- getIterID(run=run, perturb_level=ii)

    # Check whether there are any matching data and run ids by determining the number of rows in the table after matching
    match_hpo <- sapply(hpo_list, function(iter_hpo, run_id_list){
      
      # Determine if there are any rows in the run_table of the parameter list that match the data and run identifiers
      # of the current level.
      match_size <- nrow(iter_hpo$run_table[data_id==run_id_list$data & run_id==run_id_list$run])
      
      # Return TRUE if any matching rows are found.
      return(match_size > 0)
      
    }, run_id_list=run_id_list)

    # If there is a match, we step out of the loop
    if(any(match_hpo)){
      break()
    }
  }

  # Extract the table of parameters
  parameter_table <- hpo_list[match_hpo][[1]]$param

  # The table of parameter may be empty
  if(is.null(parameter_table)){
    # There are no parameters, and NULL is returned.
    return(NULL)
    
  } else {
    # Return as a list
    return(as.list(parameter_table))
  }
}



get_signature <- function(feature_info_list, dt_ranks, fs_method, param, settings){

  # Suppress NOTES due to non-standard evaluation in data.table
  name <- aggr_rank <- NULL

  # Get signature size
  if(length(param$sign_size) > 0){
    sign_size <- param$sign_size
    
  } else {
    sign_size <- 0
  }

  # Find non-feature columns
  sign_feat_cols <- check_column_name(settings$data$signature)

  if(fs_method=="signature_only"){
    # Only select signature
    if(is.null(settings$data$signature)){ stop("No signature was provided.")}

    sel_feat_cols <- sign_feat_cols

  } else if(fs_method=="none"){
    # Select all features
    sel_feat_cols <- features_after_clustering(features=get_available_features(feature_info_list=feature_info_list),
                                               feature_info_list=feature_info_list)
    
    # Order randomly so that there is no accidental dependency on order
    sel_feat_cols <- fam_sample(x=sel_feat_cols, size=length(sel_feat_cols), replace=FALSE)

  } else if(fs_method=="random"){
    # Randomise selection
    features <- features_after_clustering(features=get_available_features(feature_info_list=feature_info_list),
                                          feature_info_list=feature_info_list)

    sel_feat_cols <- fam_sample(x=features, size=sign_size, replace=FALSE)

  } else {
    # Select non-feature columns and signature feature columns
    sel_feat_cols <- sign_feat_cols

    # Get number remaining available features
    rem_feat_count <- sign_size - length(sign_feat_cols)
    if(rem_feat_count > 0){

      # Get available
      features <- features_after_clustering(features=get_available_features(feature_info_list=feature_info_list),
                                            feature_info_list=feature_info_list)
      
      # Remove signature features, if any.
      features <- setdiff(features, sign_feat_cols)
      
      # Keep only feature ranks of feature corresponding to available features,
      # and order by rank.
      dt_ranks <- dt_ranks[name %in% features,][order(aggr_rank)]

      # Add good features (low rank) to the selection
      sel_feat_cols <- c(sel_feat_cols, head(x=dt_ranks, n=rem_feat_count)$name)
    }
  }

  return(sel_feat_cols)
}


apply_signature <- function(data_obj, selected_feat) {
  # Get non-feature columns
  non_feat_cols  <- get_non_feature_columns(x=data_obj)

  # Create slice and maintain only selected features
  data_obj@data <- data_obj@data[, c(non_feat_cols, selected_feat), with=FALSE]

  return(data_obj)
}


# set_categorical_data <- function(dt, outcome_type){
#   # Replace columns types so that only numeric and categorical features remain
# 
#   # Get feature columns
#   feature_columns <- get_feature_columns(x=dt, outcome_type=outcome_type)
#   
#   # Find column classes
#   column_class <- sapply(feature_columns, function(ii, dt) (class(dt[[ii]])), dt=dt)
#   
#   # All "logical" columns should be categorical
#   logical_columns <- feature_columns[column_class == "logical"]
#   
#   # All "character" columns should be categorical
#   character_columns <- feature_columns[column_class == "character"]
#   
#   # Combine
#   categorical_columns <- c(logical_columns, character_columns)
#   
#   # Update columns
#   for(ii in categorical_columns) data.table::set(dt, j=ii, value=factor(dt[[ii]]))
#   
#   return(dt)
# }
