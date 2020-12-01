run_evaluation <- function(cl, proj_list, settings, file_paths){
  # performs evaluation of the data
  
  if(!settings$eval$do_parallel){
    cl <- NULL
  }
  
  # Extract data from ensembles
  data_set_list <- .prepare_familiar_data_sets(cl=cl, only_pooling=settings$eval$pool_only)

  # Form collections (all individual ensembles with train and validation data combined)
  collection_list <- .prepare_familiar_collections(data_set_list=data_set_list)
  
  # Create and save collections and export data
  if(!file_paths$is_temporary){
    lapply(collection_list, .process_collections, file_paths=file_paths)
  }
}


#' @title Prepare familiarData objects for evaluation at runtime.
#'
#' @description Information concerning models, features and the experiment is
#'   processed and stored in familiarData objects. Information can be extracted
#'   from these objects as csv files, or by plotting, or multiple objects can be
#'   combined into familiarCollection objects, which allows aggregated exports.
#'
#' @details This function generates the names of familiarData object files, and
#'   their corresponding generating ensemble, which allows the familiarData
#'   objects to be created.
#'
#' @param cl Cluster for parallel processing.
#' @param only_pooling Flag that, if set, forces evaluation of only the
#'   top-level data, and not e.g. ensembles.
#'
#' @return A data.table with created links to created data objects.
#'
#' @md
.prepare_familiar_data_sets <- function(cl=NULL, only_pooling=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  fam_ensemble_exists <- fam_ensemble <- fam_data_exists <- fam_data <- NULL
  learner <- fs_method <- NULL
  data_dir_path <- model_dir_path <- NULL
  model_data_id <- model_run_id <- pool_data_id <- pool_run_id <- NULL
  ensemble_data_id <- ensemble_run_id <- is_ensemble <- is_validation <- NULL
  
  # Determine the models that should be generated.
  # Load project list, file_paths and settings if required.
  settings <- get_settings()
  file_paths <- get_file_paths()
  project_list <- get_project_list()
  
  # Get project_id
  project_id <- project_list$project_id
  
  # Check which data object is required for performing model building
  mb_data_id <- .get_process_step_data_identifier(project_list=project_list,
                                                  process_step="mb")
  
  # Get runs
  run_list <- .get_run_list(iteration_list=project_list$iter_list,
                            data_id=mb_data_id)
  
  # Get list of data collection pools
  data_sets <- data.table::rbindlist(.get_ensemble_structure_info(run_list=run_list,
                                                                  proj_list=project_list,
                                                                  only_pooling=only_pooling),
                                     use.names=TRUE)

  # Identify combinations of feature selection methods and learners
  run_methods <- data.table::rbindlist(get_fs_learner_combinations(settings=settings), use.names=TRUE)
  
  # Perform a cartesian join of the data sets and the run methods.
  data_set_list <- run_methods[, as.list(data_sets), by=c("fs_method", "learner")]
  
  # Add model file names
  data_set_list[, "fam_model":=get_object_file_name(learner=learner,
                                                    fs_method=fs_method,
                                                    project_id=project_id,
                                                    data_id=model_data_id,
                                                    run_id=model_run_id,
                                                    object_type="familiarModel")]
  
  # Add paths to model directories
  data_set_list[, "model_dir_path":=get_object_dir_path(dir_path=file_paths$mb_dir,
                                                        object_type="familiarModel",
                                                        learner=learner,
                                                        fs_method=fs_method),
                by=c("learner", "fs_method")]
  
  # Add paths to data
  data_set_list[, "data_dir_path":=get_object_dir_path(dir_path=file_paths$fam_data_dir,
                                                       object_type="familiarData")]
  
  # Set paths to familiar ensembles
  data_set_list[, "fam_ensemble":=get_object_file_name(dir_path=model_dir_path,
                                                       learner=learner,
                                                       fs_method=fs_method,
                                                       project_id=project_id,
                                                       data_id=ensemble_data_id,
                                                       run_id=ensemble_run_id,
                                                       is_ensemble=TRUE,
                                                       object_type="familiarEnsemble"),
                by=c("model_dir_path", "fs_method", "learner", "ensemble_data_id", "ensemble_run_id")]
  
  # Add data file directory + names
  data_set_list[, "fam_data":=get_object_file_name(dir_path=data_dir_path,
                                                   learner=learner,
                                                   fs_method=fs_method,
                                                   project_id=project_id,
                                                   data_id=ensemble_data_id,
                                                   run_id=ensemble_run_id,
                                                   pool_data_id=pool_data_id,
                                                   pool_run_id=pool_run_id,
                                                   is_ensemble=is_ensemble,
                                                   is_validation=is_validation,
                                                   object_type="familiarData")]

  # Remove model_dir_path and data_dir_path
  data_set_list[, ":="("model_dir_path"=NULL, "data_dir_path"=NULL)]
  
  # Check which ensembles exist
  data_set_list[, "fam_ensemble_exists":=file.exists(fam_ensemble)]
  
  # Check which data objects already exist
  data_set_list[, "fam_data_exists":=file.exists(fam_data)]
  
  # Find any new ensembles that may have to be created
  new_ensemble_table <- data.table::copy(data_set_list[fam_ensemble_exists==FALSE,
                                                       c("ensemble_data_id", "ensemble_run_id",
                                                         "learner", "fs_method", "fam_model", "fam_ensemble"),
                                                       with=FALSE])
  
  # Determine if there any ensembles that need to be processed.
  if(!is_empty(new_ensemble_table)){
    
    # Select unique entries.
    new_ensemble_table <- unique(new_ensemble_table)
    
    logger.message("\nEvaluation: Creating ensemble models from individual models.")
    
    # Create familiarEnsemble objects
    fam_lapply_lb(cl=cl,
                  assign=NULL,
                  X=split(new_ensemble_table, by="fam_ensemble"),
                  FUN=.create_familiar_ensemble_runtime,
                  progress_bar=TRUE,
                  dir_path=file_paths$mb_dir)
  }
  
  # Re-check which ensembles exist
  data_set_list[, "fam_ensemble_exists":=file.exists(fam_ensemble)]
  if(!all(data_set_list$fam_ensemble_exists)){
    ..error_reached_unreachable_code(".prepare_familiar_data_sets: not all familiarEnsemble objects were created.")
  }

  # Find any new familiarData objects that may have to be created.
  new_data_table <- data.table::copy(data_set_list[fam_data_exists==FALSE, c("fam_ensemble", "fam_data", "data_perturb_level",
                                                                             "pool_data_id", "pool_run_id", "pool_perturb_level",
                                                                             "is_validation")])
  
  if(!is_empty(new_data_table)){
    
    # Select unique entries.
    new_data_table <- unique(new_data_table)
    
    # Add iteration_id and total number of iterations
    new_data_table[, ":="("iteration_id"=.I, "n_sets"=nrow(new_data_table))]
    
    logger.message("\nEvaluation: Processing data to create familiarData objects.")
    
    # Allow automated switching between internal and external loops.
    if(settings$eval$do_parallel & nrow(new_data_table) > length(cl)){
      # Perform parallelisation of the outer loop.
      outer_parallel <- show_progress_bar <- TRUE
      cl_outer <- cl
      cl_inner <- NULL
     
      logger.message(paste0("Evaluation: Parallel processing is done in the outer loop. ",
                            "No progress can be displayed."))
      
    } else if(settings$eval$do_parallel){
      # Perform parallelisation in the internal processes.
      outer_parallel <- show_progress_bar <- FALSE
      cl_outer <- NULL
      cl_inner <- cl
      
    } else {
      # No parallelisation is conducted.
      outer_parallel <- show_progress_bar <- FALSE
      cl_outer <- NULL
      cl_inner <- NULL
    }
    
    # Perform the necessary computations to create familiarData objects.
    fam_mapply_lb(cl=cl_outer,
                  assign="all",
                  FUN=.create_familiar_data_runtime,
                  pool_data_table=split(new_data_table, by="fam_data"),
                  progress_bar=show_progress_bar,
                  MoreArgs=list("cl"=cl_inner,
                                "dir_path"=file_paths$fam_data_dir))
  }  
    
  # Re-check if all familiarData objects exist
  data_set_list[, "fam_data_exists":=file.exists(fam_data)]
  if(!all(data_set_list$fam_data_exists)){
    ..error_reached_unreachable_code(".prepare_familiar_data_sets: not all familiarData_objects were created.")
  }

  return(data_set_list)
}



.prepare_familiar_collections <- function(data_set_list){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  descriptor <- NULL

  # Drop superfluous columns and select unique entries.
  data_set_list <- data.table::copy(data_set_list[, c("descriptor", "ensemble_data_id", "ensemble_run_id",
                                                      "pool_data_id", "pool_run_id", "fam_data"), with=FALSE])
  data_set_list <- unique(data_set_list)
  
  # Ensemble-based collections
  ensemble_descriptors <- c("internal_development_ensemble", "internal_validation_ensemble", "external_validation_ensemble")
  pool_descriptors <- c("internal_development_pool", "internal_validation_pool", "external_validation_pool")
  
  # Split by type
  ensemble_collections <- split(data_set_list[descriptor %in% ensemble_descriptors], by=c("ensemble_data_id", "ensemble_run_id"))
  pool_collections <- list(data_set_list[descriptor %in% pool_descriptors])
  
  # Combine
  collections <- c(ensemble_collections, pool_collections)
  
  # Process to extract collection_name, familiar_data objects and familiar data set names.
  collections <- lapply(collections, .collect_collection_info)
  
  return(collections)
}


.get_ensemble_structure_info <- function(run_list, proj_list, only_pooling=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  perturb_level <- data_id <- run_id <- has_validation <- pool_perturb_level <- data_perturb_level <- can_pre_process <- NULL
  
  # Create empty ensemble run list
  ensemble_run_list <- list()

  # Determine perturbation level for model building
  model_perturb_level <- tail(run_list[[1]]$run_table, n=1)$perturb_level

  # Iterate upwards to perturb_level 1
  for(curr_perturb_level in rev(seq_len(model_perturb_level))){

    # Generate a run table index
    ensemble_run_list <- append(ensemble_run_list, lapply(seq_len(length(run_list)), function(ii, run_list, curr_perturb_level){

      # Extract the table row corresponding to the current perturbation level for each entry in the run list
      dt <- run_list[[ii]]$run_table[perturb_level>=curr_perturb_level]

      # Extract whether the current table row has validation data
      dt[, "has_validation":=!is.null(proj_list$iter_list[[as.character(data_id)]]$run[[as.character(run_id)]]$valid_samples), by=c("data_id", "run_id")]

      # Get model data
      dt_model <- tail(run_list[[ii]]$run_table, n=1)

      # Extract the data_id and run_id of each entry
      dt[, ":="("model_data_id"=dt_model$data_id[1], "model_run_id"=dt_model$run_id[1])]

      # Get pooling data
      dt_pool <- run_list[[ii]]$run_table[perturb_level==curr_perturb_level]

      # Add a pool_data_id and pool_run_id entry, which determines at which level data is pooled
      dt[, ":="("pool_data_id"=dt_pool$data_id[1], "pool_run_id"=dt_pool$run_id[1], "pool_perturb_level"=curr_perturb_level)]

      data.table::setnames(dt, old=c("data_id", "run_id", "perturb_level"),
                           new=c("ensemble_data_id", "ensemble_run_id", "data_perturb_level"))
      
    }, curr_perturb_level=curr_perturb_level, run_list=run_list))
  }

  # Combine different ensembles
  ensemble_table <- data.table::rbindlist(ensemble_run_list)

  # Select the perturbation levels which have associated validation data, if
  # any. Only itmes that can process are considered. This excludes bootstraps.
  perturbation_levels <- head(sort(unique(ensemble_table[has_validation==TRUE & can_pre_process==TRUE]$data_perturb_level)), n=2)
  
  # Create the resulting data sets.
  if(length(perturbation_levels) == 0){
    # Development pool
    internal_development_pool <- ensemble_table[pool_perturb_level==1 & can_pre_process==TRUE]
    internal_development_pool[, ":="("descriptor"="internal_development_pool",
                                     "is_ensemble"=ifelse(data_perturb_level==pool_perturb_level, TRUE, FALSE),
                                     "is_validation"=FALSE)]
    
    data_sets <- list(internal_development_pool)
  } else {
    # Internal development ensemble
    internal_development_ensemble <- ensemble_table[data_perturb_level==tail(perturbation_levels, n=1) & pool_perturb_level!=1]
    internal_development_ensemble[, ":="("descriptor"="internal_development_ensemble",
                                         "is_ensemble"=ifelse(data_perturb_level==pool_perturb_level, TRUE, FALSE),
                                         "is_validation"=FALSE)]
    
    # Internal validation ensemble -- check if internal validation exists
    if(!is_empty(ensemble_table[data_perturb_level==tail(perturbation_levels, n=1) & data_perturb_level!=1L & has_validation==TRUE & pool_perturb_level!=1])){
      internal_validation_ensemble <- data.table::copy(internal_development_ensemble)
      
      # Update descriptors
      internal_validation_ensemble[, ":="("descriptor"="internal_validation_ensemble",
                                          "is_validation"=TRUE)]
    } else {
      internal_validation_ensemble <- head(internal_development_ensemble, n=0)
    }

    # External validation ensemble -- check if external validation data exists
    if(!is_empty(ensemble_table[data_perturb_level==1L & has_validation==TRUE & pool_perturb_level==1])){
      external_validation_ensemble <- data.table::copy(internal_development_ensemble)
      
      # Update descriptors
      external_validation_ensemble[, ":="("descriptor"="external_validation_ensemble",
                                          "data_perturb_level"=1L,
                                          "pool_data_id"=1L, "pool_run_id"=1L, "pool_perturb_level"=1L,
                                          "is_ensemble"=TRUE,
                                          "is_validation"=TRUE)]
    } else {
      external_validation_ensemble <- head(internal_development_ensemble, n=0)
    }

    # Development pool
    internal_development_pool <- ensemble_table[data_perturb_level==tail(perturbation_levels, n=1) & pool_perturb_level==1]
    internal_development_pool[, ":="("descriptor"="internal_development_pool",
                                     "ensemble_data_id"=1L,
                                     "ensemble_run_id"=1,
                                     "is_ensemble"=ifelse(data_perturb_level==pool_perturb_level, TRUE, FALSE),
                                     "is_validation"=FALSE)]
    

    
    # Internal validation pool
    internal_validation_pool <- ensemble_table[data_perturb_level==tail(perturbation_levels, n=1) & data_perturb_level!=1L & has_validation==TRUE & pool_perturb_level==1]
    internal_validation_pool[, ":="("descriptor"="internal_validation_pool",
                                    "ensemble_data_id"=1L,
                                    "ensemble_run_id"=1,
                                    "is_ensemble"=ifelse(data_perturb_level==pool_perturb_level, TRUE, FALSE),
                                    "is_validation"=TRUE)]

    # External validation pool
    external_validation_pool <- ensemble_table[data_perturb_level==1L & has_validation==TRUE & pool_perturb_level==1]
    external_validation_pool[, ":="("descriptor"="external_validation_pool",
                                    "is_ensemble"=ifelse(data_perturb_level==pool_perturb_level, TRUE, FALSE),
                                    "is_validation"=TRUE)]

    if(only_pooling){
      # List of datasets that operate on the top level (pooling level).
      data_sets <- c(list(internal_development_pool),
                     list(internal_validation_pool),
                     list(external_validation_pool))
      
    } else {
      # List of datasets
      data_sets <- c(split(internal_development_ensemble, by="ensemble_run_id"),
                     split(internal_validation_ensemble, by="ensemble_run_id"),
                     split(external_validation_ensemble, by="ensemble_run_id"),
                     list(internal_development_pool),
                     list(internal_validation_pool),
                     list(external_validation_pool))
    }
  }

  return(data_sets)
}


.create_familiar_ensemble_runtime <- function(ensemble_table, dir_path){
  # Creates a familiarEnsemble and extracts the corresponding data
  # Note that data for development and validation data is extracted separately.
  # This function is called during the validation step.

  # Generate a skeleton familiarEnsemble
  fam_ensemble <- methods::new("familiarEnsemble",
                               model_list=as.list(ensemble_table$fam_model),
                               learner=ensemble_table$learner[1],
                               fs_method=ensemble_table$fs_method[1])
  
  # Load models and prevent auto-detaching.
  fam_ensemble <- load_models(object=fam_ensemble, dir_path=dir_path, suppress_auto_detach=TRUE)

  # Create a run table
  fam_ensemble@run_table <- list("run_table"=lapply(fam_ensemble@model_list, function(fam_model) fam_model@run_table),
                                 "ensemble_data_id"=ensemble_table$ensemble_data_id[1],
                                 "ensemble_run_id"=ensemble_table$ensemble_run_id[1])
  
  # Complete the ensemble using information provided by the model
  fam_ensemble <- complete_familiar_ensemble(object=fam_ensemble)
  
  # Detach models
  fam_ensemble <- detach_models(object=fam_ensemble)
  
  save(list=fam_ensemble, file=dir_path)
}



.create_familiar_data_runtime <- function(cl=NULL, pool_data_table, dir_path){
  # Creates a familiarData object.

  logger.message(paste0("\nEvaluation: Processing dataset ", pool_data_table$iteration_id,
                        " of ", pool_data_table$n_sets, "."))
  
  # Load the familiarEnsemble
  fam_ensemble <- load_familiar_object(pool_data_table$fam_ensemble)
  
  # Define a dataObject with delayed reading. This enables the proper selection
  # of development and training data for each familiarModel used in the
  # ensemble.
  data_obj <- methods::new("dataObject",
                           data = NULL,
                           preprocessing_level="none",
                           outcome_type = fam_ensemble@outcome_type,
                           delay_loading = TRUE,
                           perturb_level = pool_data_table$data_perturb_level[1],
                           load_validation = pool_data_table$is_validation,
                           aggregate_on_load = FALSE)
  
  # Retrieve settings from the backend
  settings <- get_settings()
  
  # Create a familiarData object
  fam_data <- extract_data(object = fam_ensemble,
                           data = data_obj,
                           cl=cl,
                           data_element = settings$eval$evaluation_data_elements,
                           time_max = settings$eval$time_max,
                           eval_times = settings$eval$eval_times,
                           aggregation_method = settings$eval$aggregation,
                           rank_threshold = settings$eval$aggr_rank_threshold,
                           ensemble_method = settings$eval$ensemble_method,
                           stratification_ensemble_method = settings$eval$strat_ensemble_method,
                           metric = settings$eval$metric,
                           feature_cluster_method = settings$eval$feature_cluster_method,
                           feature_cluster_cut_method=settings$eval$feature_cluster_cut_method,
                           feature_linkage_method=settings$eval$feature_linkage_method,
                           feature_similarity_metric=settings$eval$feature_similarity_metric,
                           feature_similarity_threshold=settings$eval$feature_similarity_threshold,
                           sample_cluster_method=settings$eval$sample_cluster_method,
                           sample_linkage_method=settings$eval$sample_linkage_method,
                           sample_similarity_metric=settings$eval$sample_similarity_metric,
                           confidence_level = settings$eval$confidence_level,
                           bootstrap_ci_method = settings$eval$bootstrap_ci_method,
                           compute_model_data = settings$eval$compute_model_data,
                           compute_model_ci = settings$eval$compute_model_ci,
                           compute_ensemble_ci = settings$eval$compute_ensemble_ci,
                           aggregate_ci = settings$eval$aggregate_ci,
                           dynamic_model_loading = settings$eval$auto_detach,
                           icc_type= settings$eval$icc_type,
                           message_indent=1L,
                           verbose=TRUE)
  
  # Update the pooling table
  fam_data@pooling_table <- fam_data@pooling_table[, ":="("pool_data_id"=pool_data_table$pool_data_id,
                                                          "pool_run_id"=pool_data_table$pool_run_id,
                                                          "pool_perturb_level"=pool_data_table$pool_perturb_level)]
  
  # Set a placeholder name for the familiarData object
  fam_data <- set_data_set_names(x=fam_data)
  
  # Save the familiarData object
  save(list=fam_data, file=dir_path)
  
  logger.message(paste0("Evaluation: familiarData object ", get_object_name(object=fam_data), " was created."))
}



.collect_collection_info <- function(data_set){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  descriptor <- NULL
  
  # Determine whether any internal and external validation is present.
  has_internal_validation <- any(data_set$descriptor %in% c("internal_validation_ensemble", "internal_validation_pool"))
  has_external_validation <- any(data_set$descriptor %in% c("external_validation_ensemble", "external_validation_pool"))
  
  # Set development name
  data_set[descriptor %in% c("internal_development_ensemble", "internal_development_pool"), "fam_data_name":="development"]
  
  # Set internal validation name
  if(has_internal_validation & !has_external_validation){
    data_set[descriptor %in% c("internal_validation_ensemble", "internal_validation_pool"), "fam_data_name":="validation"]
    
  } else if(has_internal_validation & has_external_validation){
    data_set[descriptor %in% c("internal_validation_ensemble", "internal_validation_pool"), "fam_data_name":="int. validation"]
  }
  
  # External validation name
  if(!has_internal_validation & has_external_validation){
    data_set[descriptor %in% c("external_validation_ensemble", "external_validation_pool"), "fam_data_name":="validation"]
    
  } else if(has_internal_validation & has_external_validation){
    data_set[descriptor %in% c("external_validation_ensemble", "external_validation_pool"), "fam_data_name":="ext. validation"]
  }
  
  # Determine whether the collection represents a pool or an ensemble.
  is_pool <- any(data_set$descriptor == "internal_development_pool")
  
  # Set the collection name.
  if(is_pool){
    collection_name <- "pooled_data"
    
  } else {
    collection_name <- paste0("ensemble_data_", data_set$ensemble_run_id[1])
  }
  
  # Return collection info that is required to form a collection.
  return(list("collection_name"=collection_name,
              "fam_data"=as.list(data_set$fam_data),
              "fam_data_names"=droplevels(factor(x=data_set$fam_data_name,
                                                 levels=c("development", "int. validation", "ext. validation", "validation")))))
}



.process_collections <- function(collection_info, file_paths){

  # Create the expected file path to the familiarCollection object.
  fam_collection_file <- file.path(file_paths$fam_coll_dir, paste0(collection_info$collection_name, ".RDS"))

  # Check if the familiarCollection already exists.
  if(!file.exists(fam_collection_file)){
    logger.message(paste0("\nEvaluation: Creating collection ", collection_info$collection_name))
    # Create a collection using the available input data
    fam_collection <- as_familiar_collection(object=collection_info$fam_data,
                                             familiar_data_names=collection_info$fam_data_names,
                                             collection_name=collection_info$collection_name)
    
    # Save to drive.
    save(list=fam_collection, file=file_paths$fam_coll_dir)
    
  } else {
    # Read from drive.
    fam_collection <- load_familiar_object(fam_collection_file)
  }
  
  logger.message(paste0("\nEvaluation: Exporting data from collection ", collection_info$collection_name))
  
  # Export to csv
  export_all(object=fam_collection, dir_path=file_paths$results_dir)
  
  # Export to plot
  plot_all(object=fam_collection, dir_path=file_paths$results_dir)
}
