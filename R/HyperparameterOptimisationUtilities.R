..create_initial_hyperparameter_set <- function(parameter_list,
                                                grid_initialisation_method,
                                                n_random_sets=200L){
  if(length(parameter_list) == 0) return(NULL)
  
  if(!.any_randomised_hyperparameters(parameter_list=parameter_list)){
    # All variables have been fixed. No optimisation is required.
    
    # Get values for each parameter.
    value_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config))
    
    # Convert to a data table.
    parameter_table <- data.table::as.data.table(value_list)
    
  } else {
    
    if(grid_initialisation_method %in% "random"){
      # Create completely random hyperparameter sets.

      # Create random hyperparameter sets.
      random_list <- lapply(seq_len(n_random_sets),
                            function(ii, parameter_list) (..randomise_hyperparameter_set(parameter_list=parameter_list,
                                                                                         local=FALSE)),
                            parameter_list=parameter_list)
      
      # Combine list into a single data table.
      parameter_table <- data.table::rbindlist(random_list, use.names=TRUE)
      
      # Allow only unique parameter sets.
      parameter_table <- unique(parameter_table, by=names(parameter_list))
    }
    
    if(grid_initialisation_method %in% c("fixed", "fixed_subsample")){
      # Create a fixed grid for the parameters. This may have the advantage of
      # spanning a number of suitable configurations.
      
      # Get initial values for each parameter.
      value_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config))
      
      # Generate a table with all permutations of the grid points.
      parameter_table <- expand.grid(value_list, stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
      parameter_table <- data.table::as.data.table(parameter_table)
      
      # Allow only unique parameter sets.
      parameter_table <- unique(parameter_table, by=names(parameter_list))
    }
    
    if(grid_initialisation_method %in% "fixed_subsample"){
      # Subsample the fixed grid.
      
      # Randomly select up to n_random_sets initial parameter sets from the
      # fixed grid.
      selected_row_id <- sample(x = seq_len(nrow(parameter_table)),
                                size = min(c(nrow(parameter_table), n_random_sets)),
                                replace = FALSE)
      
      parameter_table <- parameter_table[selected_row_id, ]
    }
  }
  
  # Add parameter id.
  parameter_table[, "param_id":=.I]
  
  return(parameter_table)
}



..randomise_hyperparameter_set <- function(parameter_table=NULL,
                                           parameter_list,
                                           local=TRUE){
  
  if(local & is_empty(parameter_table)){
    ..error_reached_unreachable_code("..randomise_hyperparameter_set: no values for local search.")
  }
  
  if(local){
    # Parameters are locally randomised.
    
    # Create the updated list from the parameter table.
    updated_list <- as.list(parameter_table)
    
    # Remove param_id and expected improvement from updated list.
    updated_list$param_id <- NULL
    updated_list$expected_improvement <- NULL
    
    # Select one hyperparameter from the list of randomisable hyperparameters to
    # randomly update.
    random_parameters <- sapply(parameter_list, function(list_entry) (list_entry$randomise))
    selected_parameter <- fam_sample(names(parameter_list)[random_parameters], size=1)
    
    # Get current value of the selected hyperparameter from the table.
    current_parameter_value <- parameter_table[[selected_parameter]]
    
    # Get type and range of current hyperparameter
    parameter_type <- parameter_list[[selected_parameter]]$type
    parameter_range <- parameter_list[[selected_parameter]]$range
    
    if(parameter_type %in% c("integer", "numeric")){
      
      if(length(parameter_range) == 2){
        
        # Check that the range is not 0.
        if(max(parameter_range) != min(parameter_range)){
          # Convert to float in [0,1] range
          hp_val_float <- (current_parameter_value - min(parameter_range)) / (max(parameter_range) - min(parameter_range))
          
          # Draw a new random position in [0,1]
          rand_valid <- FALSE
          while(!rand_valid){
            # Draw 20 random numbers from a normal distribution with mean=hp_val_float and sd=0.2
            hp_rand    <- stats::rnorm(20, mean=hp_val_float, sd=0.1)
            rand_valid <- any(hp_rand>=0.0 & hp_rand<=1.0)
          }
          
          # Select new value in [0,1] and convert to original range
          new_hp_val_float <- hp_rand[hp_rand>=0 & hp_rand<=1.0][1]
          new_hp_val_float <- new_hp_val_float * (max(parameter_range) - min(parameter_range)) + min(parameter_range)
          
        } else {
          # Range is 0. Just copy the current parameter value.
          new_hp_val_float <- current_parameter_value
        }
        
      } else {
        # Treat a range such as c(0,1,3) as if only these values can be selected.
        new_hp_val_float <- fam_sample(parameter_range, size=1)
      }
      
      # Set new value as integer or numeric float
      if(parameter_type=="integer"){
        updated_list[[selected_parameter]] <- as.integer(round(new_hp_val_float))
      } else {
        updated_list[[selected_parameter]] <- new_hp_val_float
      }
      
    } else if(parameter_type %in% c("factor")){
      # Find range of available options
      available_parameter_values <- parameter_range[parameter_range != current_parameter_value]
      
      # Randomly select one option
      updated_list[[selected_parameter]] <- factor(fam_sample(available_parameter_values, size=1), levels=parameter_range)
      
    } else if(parameter_type %in% c("logical")){
      # Find range of available options
      available_parameter_values <- parameter_range[parameter_range != current_parameter_value]
      
      # Randomly select one option
      updated_list[[selected_parameter]] <- fam_sample(available_parameter_values, size=1)
      
    } else {
      ..error_reached_unreachable_code("randomise_hyperparameter_set_unknown_type")
    }
    
  } else {
    # Parameters are globally randomised.
    
    # Generate an updated list from parameter_list for randomisation
    updated_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config[1]))
    
    # Select hyperparameters to be randomised
    random_parameters <- sapply(parameter_list, function(list_entry) (list_entry$randomise))
    random_parameters <- names(parameter_list)[random_parameters]
    
    # Iterate over hyperparameters
    for(selected_parameter in random_parameters){
      # Get type and range of current hyperparameter
      parameter_type <- parameter_list[[selected_parameter]]$type
      parameter_range <- parameter_list[[selected_parameter]]$range
      parameter_distribution <- parameter_list[[selected_parameter]]$rand_distr
      
      if(parameter_type %in% c("integer", "numeric")){
        if(length(parameter_range) == 2){
          if(is.null(parameter_distribution)){
            # Select new value in [0,1] and convert to original range
            hp_rand          <- stats::runif(1)
            new_hp_val_float <- hp_rand * (max(parameter_range) - min(parameter_range)) + min(parameter_range)
            
          } else if(parameter_distribution=="log"){
            # Select new value in [0,1] and convert to log transform of original range
            hp_log_rand      <- stats::runif(1) * (log(max(parameter_range)) - log(min(parameter_range))) + log(min(parameter_range))
            
            # Transform back from log transformation: note this emphasises smaller values
            new_hp_val_float <- exp(hp_log_rand)
          }
          
        } else {
          # Treat a range such as c(0,1,3) as if only these values can be selected.
          new_hp_val_float <- fam_sample(parameter_range, size=1)
        }
        
        # Set new value as integer or numeric float
        if(parameter_type=="integer"){
          updated_list[[selected_parameter]] <- as.integer(round(new_hp_val_float))
        } else {
          updated_list[[selected_parameter]] <- new_hp_val_float
        }
        
      } else if(parameter_type %in% c("factor")){
        # Randomly select one option
        updated_list[[selected_parameter]] <- factor(fam_sample(parameter_range, size=1), levels=parameter_range)
        
      } else if(parameter_type %in% c("logical")){
        updated_list[[selected_parameter]] <- fam_sample(parameter_range, size=1)
      }
    }
  }

  # Return a randomised configuration as a data.table
  return(data.table::as.data.table(updated_list))
}



..create_hyperparameter_run_table <- function(run_ids,
                                              parameter_ids=NULL,
                                              measure_time=FALSE,
                                              score_table=NULL,
                                              parameter_table=NULL,
                                              optimisation_model=NULL,
                                              n_max_bootstraps=NULL){
  # Creates a run table from the run identifiers and parameter identifiers.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  time_taken <- NULL
  
  if(measure_time){
    # Filter by time. This filters out the hyperparameter sets that took very
    # long to complete, while not delivering good enough performance.
    
    # Compute optimisation score based on the presented scores.
    optimisation_score_table <- .compute_hyperparameter_optimisation_score(score_table=score_table,
                                                                           optimisation_function=optimisation_model@optimisation_function)
    
    # Find data related to the most promising set of hyperparameters.
    incumbent_set <- get_best_hyperparameter_set(score_table=optimisation_score_table,
                                                 parameter_table=parameter_table,
                                                 optimisation_model=optimisation_model,
                                                 n_max_bootstraps=n_max_bootstraps)
    
    # Select fitting hyperparameters.
    parameter_ids <- optimisation_score_table[time_taken <= incumbent_set$max_time]$param_id
  }
  
  if(is.null(parameter_ids)) ..error_reached_unreachable_code("..create_hyperparameter_run_table: parameter_ids cannot be NULL.")
  
  # Create a run table consisting of parameter and run identifiers.
  run_table <- data.table::as.data.table(expand.grid(param_id=parameter_ids,
                                                     run_id=run_ids,
                                                     KEEP.OUT.ATTRS=FALSE,
                                                     stringsAsFactors=FALSE))
  
  return(run_table)
}



.optimisation_process_time_available <- function(process_clock,
                                                 time_limit=NULL,
                                                 message_indent=0L,
                                                 verbose=FALSE){
  
  # Check that there is time-limit to obey.
  if(is.null(time_limit)) return(TRUE)
  
  # Compute time spent optimising.
  optimisation_time <- process_clock$time(units="mins")
  
  # Check if there still is time left.
  if(optimisation_time < time_limit) return(TRUE)
  
  logger.message(paste0("Hyperparameter optimisation: Optimisation stopped because the optimisation process exceeded the allotted time: time spent: ",
                        optimisation_time, " mins; time allotted: ", time_limit, " mins."),
                 indent=message_indent,
                 verbose=verbose)
  
  return(FALSE)
}



.compute_hyperparameter_variable_importance <- function(cl=NULL,
                                                        determine_vimp=TRUE,
                                                        object,
                                                        data,
                                                        bootstraps,
                                                        verbose,
                                                        message_indent,
                                                        ...){
  
  if(object@fs_method %in% .get_available_random_vimp_methods() |
     object@fs_method %in% .get_available_none_vimp_methods() | 
     object@fs_method %in% .get_available_signature_only_vimp_methods()) return(NULL)
  
  # Check if the code is called downstream from summon_familiar.
  is_main_process <- !inherits(tryCatch(get_file_paths(), error=identity), "error")
  
  if(determine_vimp | !is_main_process){
    # Variable importance has to be computed for the bootstraps if expressly
    # requested using determine_vimp or if the code is called outside of
    # summon_familiar.
    
    # Set up variable importance object.
    vimp_object <- promote_vimp_method(methods::new("familiarVimpMethod",
                                                    outcome_type = object@outcome_type,
                                                    vimp_method = object@fs_method,
                                                    outcome_info = object@outcome_info,
                                                    feature_info = object@feature_info,
                                                    required_features = object@required_features,
                                                    run_table = object@run_table))
    
    if(is_main_process){
      # Load hyperparameters.
      vimp_object <- run_hyperparameter_optimisation(vimp_method=object@fs_method,
                                                     data_id = tail(object@run_table, n=1)$data_id,
                                                     verbose=FALSE)
      
      # Select a suitable sets of hyperparameters.
      vimp_object <- .find_hyperparameters_for_run(run=list("run_table"=object@run_table),
                                                   hpo_list=vimp_object,
                                                   allow_random_selection=TRUE,
                                                   as_list=FALSE)
      
      
    } else {
      # Optimise hyperparameters.
      vimp_object <- optimise_hyperparameters(object=vimp_object,
                                              data=data,
                                              cl=cl,
                                              determine_vimp=determine_vimp,
                                              verbose=verbose,
                                              message_indent=message_indent + 1L,
                                              ...)
    }
    
    logger.message(paste0("Computing variable importance for ", length(bootstraps), " bootstraps."),
                   indent=message_indent + 1L,
                   verbose=verbose)
    
    # Iterate over data.
    vimp_list <- fam_lapply(cl=cl,
                            assign="all",
                            X=bootstraps,
                            FUN=..compute_hyperparameter_variable_importance,
                            object=vimp_object,
                            data=data,
                            progress_bar=verbose,
                            chopchop=TRUE)
  }
  
  return(vimp_list) 
}



..compute_hyperparameter_variable_importance <- function(train_samples, object, data){
  
  # Select bootstrap data.
  data <- select_data_from_samples(data=data,
                                   samples=train_samples)
  
  # Select a familiarVimpMethod object if multiple are present.
  if(is.list(object)) object <- object[[sample(x=seq_along(object), size=1L)]]
  
  # Compute variable importance.
  vimp_table <- .vimp(object=object,
                      data=data)
  
  # Form clusters.
  vimp_table <- recluster_vimp_table(vimp_table)
  
  # Compute variable importance.
  vimp_table <- get_vimp_table(vimp_table)
  
  return(vimp_table)
}



.compute_hyperparameter_model_performance <- function(cl=NULL,
                                                      object,
                                                      run_table,
                                                      parameter_table,
                                                      bootstraps,
                                                      data,
                                                      rank_table_list,
                                                      metric_objects,
                                                      iteration_id,
                                                      time_optimisation_model=NULL,
                                                      overhead_time=NULL,
                                                      verbose=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL
  
  # Check that the run table is not empty. This may occur if, for instance, the
  # initial search turned up nothing.
  if(is_empty(run_table)) return(NULL)
  
  # Prepare training and validation samples for the separate runs.
  training_list <- lapply(run_table$run_id,
                          function(ii, training_list) (training_list[[ii]]),
                          training_list=bootstraps$train_list)
  
  validation_list <- lapply(run_table$run_id,
                            function(ii, validation_list) (validation_list[[ii]]),
                            validation_list=bootstraps$valid_list)
  
  # Generate parameter sets.
  parameter_list <- lapply(run_table$param_id,
                           function(ii, parameter_table) (parameter_table[param_id==ii, ]),
                           parameter_table=parameter_table)
  
  # Generate variable importance sets (if any)
  rank_table_list <- lapply(run_table$run_id,
                            function(ii, rank_table_list) (rank_table_list[[ii]]),
                            rank_table_list=rank_table_list)
  
  if(is.null(time_optimisation_model)){
    # Create a scoring table, with accompanying information.
    score_results <- fam_mapply_lb(cl = cl,
                                   assign = NULL,
                                   FUN = ..compute_hyperparameter_model_performance,
                                   run_id = run_table$run_id,
                                   training_samples = training_list,
                                   validation_samples = validation_list,
                                   rank_table=rank_table_list,
                                   parameter_table=parameter_list,
                                   progress_bar=verbose,
                                   MEASURE.TIME=TRUE,
                                   MoreArgs=list("object"=object,
                                                 "data"=data,
                                                 "metric_objects"=metric_objects))
    
  } else {
  
    # Predict expected process time for each parameter set.
    process_time <- sapply(parameter_list,
                           .compute_expected_train_time,
                           time_model=time_optimisation_model)
    
    # Create a scoring table, with accompanying information.
    score_results <- fam_mapply(cl = cl,
                                assign = NULL,
                                FUN = ..compute_hyperparameter_model_performance,
                                run_id = run_table$run_id,
                                training_samples = training_list,
                                validation_samples = validation_list,
                                rank_table=rank_table_list,
                                parameter_table=parameter_list,
                                progress_bar=verbose,
                                process_time=process_time,
                                overhead_time=overhead_time,
                                chopchop=TRUE,
                                MEASURE.TIME=TRUE,
                                MoreArgs=list("object"=object,
                                              "data"=data,
                                              "metric_objects"=metric_objects))
  }
  
  # Aggregate the results, and the score table
  aggregated_results <- list("results"=data.table::rbindlist(lapply(score_results$results, function(x)(x$score_table)), use.names=TRUE))
  
  # Add in process times to the results.
  aggregated_results$results[, "time_taken":=rep(score_results$process_time, each=2*length(metric_objects))]
  
  # Add in iteration id.
  aggregated_results$results[, "iteration_id":=iteration_id]
  
  # Aggregate errors.
  aggregated_results$error <- unlist(lapply(score_results$results, function(x)(x$error)))
  
  # Return aggregated results.
  return(aggregated_results)
}


..compute_hyperparameter_model_performance <- function(object,
                                                       data,
                                                       run_id,
                                                       training_samples,
                                                       validation_samples,
                                                       parameter_table,
                                                       rank_table,
                                                       metric_objects,
                                                       signature_features=NULL){
  if(!is(object, "familiarModel")){
    ..error_reached_unreachable_code("..compute_hyperparameter_model_performance: object is not a familiarModel object.")
  }
  
  # Find parameter id and run id for the current run
  parameter_list <- as.list(parameter_table[, -c("param_id")])
  param_id <- parameter_table$param_id[1]
  
  # Select training (in-bag) and validation (out-of-bag) data for current run,
  data_training <- select_data_from_samples(data=data,
                                            samples=training_samples)
  
  data_validation <- select_data_from_samples(data=data,
                                              samples=validation_samples)
  
  # Update the familiar model (for variable importance)
  object@hyperparameters <- parameter_list
  
  # Set signature.
  object <- set_signature(object=object,
                          rank_table=rank_table,
                          minimise_footprint=TRUE)
  
  # Train model with the set of hyperparameters.
  object <- .train(object=object,
                   data=data_training,
                   get_additional_info=FALSE,
                   trim_model=FALSE)
  
  # Generate scores.
  score_table <- mapply(function(data, data_set, object, metric_objects, settings){
    
    # Get metric names.
    metric_names <- sapply(metric_objects, function(metric_object) metric_object@metric)

    # Set time_max.
    if(!is.null(object@settings$time_max)){
      # Derive from evaluation settings attached to the familiarModel object.
      time_max <- object@settings$time_max
      
    } else {
      time_max <- object@outcome_info@time
    }
    
    # Predict for the in-bag and out-of-bag datasets.
    prediction_table <- .predict(object=object,
                                 data=data,
                                 time=time_max)
    
    # Compute metric scores.
    metrics_values <- sapply(metric_objects,
                             compute_metric_score,
                             data=prediction_table)
    
    # Compute objective scores.
    metrics_objective_score <- mapply(compute_objective_score,
                                      metric=metric_objects,
                                      value=metrics_values,
                                      SIMPLIFY=TRUE)
    
    # Return as data.table.
    return(data.table::data.table("metric"=metric_names,
                                  "data_set"=data_set,
                                  "value"=metrics_values,
                                  "objective_score"=metrics_objective_score))
  },
  data=list(data_training, data_validation),
  data_set=c("training", "validation"),
  MoreArgs=list("object"=object,
                "metric_objects"=metric_objects),
  SIMPLIFY=FALSE)
  
  # Aggregate to a single table.
  score_table <- data.table::rbindlist(score_table, use.names=TRUE)
  
  # Add parameter id and run id.
  score_table[, ":="("param_id"=param_id,
                     "run_id"=run_id)]
  
  # Set the column order.
  data.table::setcolorder(score_table, neworder=c("param_id", "run_id"))
  
  return(list("score_table"=score_table,
              "error"=object@messages$error))
}



.compute_hyperparameter_optimisation_score <- function(score_table,
                                                       optimisation_function){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- NULL
  
  if(is_empty(score_table)) return(NULL)
  
  # Compute optimisation score.
  optimisation_score_table <- metric.compute_optimisation_score(score_table=score_table,
                                                                optimisation_function=optimisation_function)
  
  # Reinsert time_taken.
  optimisation_score_table <- optimisation_score_table[unique(score_table[, c("param_id", "run_id", "time_taken")]), on=.NATURAL]
  
  return(optimisation_score_table)
}



.compute_hyperparameter_summary_score <- function(score_table, parameter_table, optimisation_model){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- time_taken <- .NATURAL <- mu <- sigma <- summary_score <- score_estimate <- NULL
  
  # Check that the table is not empty.
  if(is_empty(score_table)) return(NULL)
  
  # Check if summary score has already been created.
  if("summary_score" %in% colnames(score_table)) return(score_table)
  
  # Extract the optimisation function.
  optimisation_function <- optimisation_model@optimisation_function
  
  # Check if the score table already contains optimisation scores.
  if(!"optimisation_score" %in% colnames(score_table)){
    score_table <- .compute_hyperparameter_optimisation_score(score_table=score_table,
                                                              optimisation_function=optimisation_function)
  }
  
  # Make sure to return both the summary score, as well as the mean optimisation
  # score (or its model estimate if the optimisation function allows it). The
  # mean score is important for acquisition functions, where we work with
  # predictions of the optimisation score -- not the summary score. In addition,
  # compute time_taken.
  if(optimisation_function %in% c("validation", "max_validation", "balanced", "stronger_balance")){
    # Here we just use the mean score.
    data <- score_table[, list("summary_score"=mean(optimisation_score, na.rm=TRUE),
                               "score_estimate"=mean(optimisation_score, na.rm=TRUE),
                               "time_taken"=stats::median(time_taken)), by="param_id"]
    
  } else if(optimisation_function %in% c("validation_minus_sd")){
    # Here we need to compute the standard deviation, with an exception for
    # single subsamples.
    data <- score_table[, list("summary_score"=..optimisation_function_validation_minus_sd(optimisation_score),
                               "score_estimate"=mean(optimisation_score, na.rm=TRUE),
                               "time_taken"=stats::median(time_taken)), by="param_id"]
    
    
  } else if(optimisation_function %in% c("validation_25th_percentile")){
    # Summary score is formed by the 25th percentile of the optimisation scores.
    data <- score_table[, list("summary_score"=stats::quantile(optimisation_score, probs=0.25, names=FALSE, na.rm=TRUE),
                               "score_estimate"=mean(optimisation_score, na.rm=TRUE),
                               "time_taken"=stats::median(time_taken)), by="param_id"]
    
  } else if(optimisation_function %in% c("model_estimate", "model_estimate_minus_sd")){
    # Model based summary scores. The main difference with other optimisation
    # functions is that a hyperparameter model is used to both infer the summary
    # scores and the score estimate.
    
    # Check if the model is trained.
    if(!model_is_trained(optimisation_model)) optimisation_model <- .train(object=optimisation_model,
                                                                           data=score_table,
                                                                           parameter_data=parameter_table)
    
    # Get parameter data that is used in the score table.
    parameter_data <- parameter_table[unique(score_table[, mget("param_id")]), on=.NATURAL]
    
    # Model was successfully trained.
    prediction_table <- .predict(object=optimisation_model,
                                 data=parameter_data,
                                 type=ifelse(optimisation_function=="model_estimate", "default", "sd"))
    
    # Check the prediction table has returned sensible information.
    if(any(is.finite(prediction_table$mu))){
      # Prepare the score estimate and time taken.
      data <-  score_table[, list("time_taken"=stats::median(time_taken)), by="param_id"]
      
      # Fold the prediction table into data.
      data <- merge(x=data,
                    y=prediction_table,
                    all.x=TRUE,
                    all.y=FALSE,
                    by="param_id")
      
      if(optimisation_function == "model_estimate"){
        # Copy mu as score estimate.
        data[, "score_estimate":=mu]
        
        # Rename mu to summary_score.
        data.table::setnames(data, old="mu", new="summary_score")
        
      } else if(optimisation_function == "model_estimate_minus_sd"){
        # Compute summary score.
        data[, "summary_score":=mu - sigma]
        
        # Rename mu to score_estimate.
        data.table::setnames(data, old="mu", new="score_estimate")
        
      } else {
        ..error_reached_unreachable_code(paste0(".compute_hyperparameter_summary_score: encountered unknown optimisation function: ", optimisation_function))
      }
      
      # Check that any predictions make sense at all.
      if(all(!is.finite(score_table$optimisation_score))){
        data[, ":="(summary_score=NA_real_,
                    score_estimate=NA_real_)]
      }
      
    } else if(optimisation_function == "model_estimate"){
      # In absence of suitable data, use the model-less equivalent of
      # model_estimate, namely "validation".
      data <- score_table[, list("summary_score"=mean(optimisation_score, na.rm=TRUE),
                                 "score_estimate"=mean(optimisation_score, na.rm=TRUE),
                                 "time_taken"=stats::median(time_taken)), by="param_id"]
      
    } else if(optimisation_function == "model_estimate_minus_sd"){
      # In absence of suitable data, use the model-less equivalent of
      # model_estimate_minus_sd, namely "validation_minus_sd".
      data <- score_table[, list("summary_score"=..optimisation_function_validation_minus_sd(optimisation_score),
                                 "score_estimate"=mean(optimisation_score, na.rm=TRUE),
                                 "time_taken"=stats::median(time_taken)), by="param_id"]
      
    } else {
      ..error_reached_unreachable_code(paste0(".compute_hyperparameter_summary_score: encountered unknown optimisation function: ", optimisation_function))
    }

  } else {
    ..error_reached_unreachable_code(".compute_hyperparameter_summary_score: encountered an unknown optimisation function")
  }
  
  # Format data by selecting only relevant columns. This also orders the data.
  data <- data[, mget(c("param_id", "summary_score", "score_estimate", "time_taken"))]
  
  # Update missing scores.
  data[!is.finite(summary_score), "summary_score":=-1.0]
  data[!is.finite(score_estimate), "score_estimate":=-1.0]
  
  return(data)
}



..optimisation_function_validation_minus_sd <- function(x){
  # Here we need to switch based on the length of x.
  
  # The default behaviour is when multiple subsamples exist, and the standard
  # deviation can be computed.
  if(length(x) > 1) return(mean(x, na.rm=TRUE) - stats::sd(x, na.rm=TRUE))
  
  # The exception for x with length 1, where the standard deviation does not
  # exist.
  return(mean(x, na.rm=TRUE))
}



get_best_hyperparameter_set <- function(score_table,
                                        parameter_table,
                                        optimisation_model,
                                        n_max_bootstraps,
                                        n=1L,
                                        parameter_id_set=NULL){
  # The best set has several interesting values that can be computed and used
  # for acquisition functions, information for the user etc.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  summary_score <- param_id <- NULL
  
  # Compute summary scores.
  data <- .compute_hyperparameter_summary_score(score_table=score_table,
                                                parameter_table=parameter_table,
                                                optimisation_model=optimisation_model)
  
  # Select only the parameter sets of interest. This happens during the run-off
  # between the incumbent and challenger hyperparameter sets.
  if(!is.null(parameter_id_set)){
    data <- data[param_id %in% parameter_id_set]
  }
  
  # Find the best hyperparameter set based on summary scores in data.
  data <- data[order(-summary_score)]
  data <- head(data, n=n)
  
  # Compute round t, i.e. the highest number of bootstraps observed across the
  # parameters.
  t <- max(unique(score_table[, mget(c("param_id", "run_id"))])[, list("n"=.N), by="param_id"]$n)
  
  # Compute the maximum allowed time.
  max_time <- (5.0 - 4.0 * t / n_max_bootstraps) * data$time_taken[1]
  
  # Check that the maximum time is not trivially small (i.e. less than 10
  # seconds).
  if(is.na(max_time)){
    max_time <- Inf
    
  } else if(max_time < 10.0) {
    max_time <- 10.0
  }
  
  # Extract and update summary score and score estimate
  summary_score <- data$summary_score
  if(!is.finite(summary_score)) summary_score <- -1.0
  
  score_estimate <- data$score_estimate
  if(!is.finite(score_estimate)) score_estimate <- -1.0
  
  # Extract the summary score, score estimate, and time taken, and return with
  # other information.
  return(list("param_id"=data$param_id,
              "t"=t,
              "time"=data$time_taken,
              "max_time"=max_time,
              "summary_score"=summary_score,
              "score_estimate"=score_estimate,
              "n"=n_max_bootstraps))
}



# ..get_acquisition_function_parameters <- function(optimisation_score_table,
#                                                   acquisition_function,
#                                                   n_max_bootstraps){
#   # Determine optimal score.
#   best_hyperparameter_set <- ..get_best_hyperparameter_set(optimisation_score_table=optimisation_score_table,
#                                                            acquisition_function=acquisition_function,
#                                                            n=1L)
#   
#   # Determine the optimal parameter score tau.
#   tau <- best_hyperparameter_set$optimisation_score
#   
#   # Determine time corresponding to the best parameter set.
#   time <- best_hyperparameter_set$time_taken
#   
#   # Determine round t.
#   t <- max(optimisation_score_table[, list("n"=.N), by="param_id"]$n)
#   
#   # Determine maximum time that can be countenanced.
#   max_time <- (5.0 - 4.0 * t / n_max_bootstraps) * time
#   
#   # Check that the maximum time is not trivially small (i.e. less than 10
#   # seconds).
#   if(is.na(max_time)){
#     max_time <- Inf
#   } else if(max_time < 10.0) {
#     max_time <- 10.0
#   }
#   
#   return(list("t"=t,
#               "time"=time,
#               "max_time"=max_time,
#               "tau"=tau,
#               "n"=n_max_bootstraps))
# }



# ..get_best_hyperparameter_set <- function(optimisation_score_table,
#                                           acquisition_function,
#                                           n=1L){
#   # Find the best configurations based on the optimisation score
#   
#   # Suppress NOTES due to non-standard evaluation in data.table
#   optimisation_score <- time_taken <- .NATURAL <- NULL
#   
#   # Compute time taken.
#   time_table <- optimisation_score_table[, list("time_taken"=stats::median(time_taken, na.rm=TRUE)), by="param_id"]
#   
#   # Compute the summary score per parameter id.
#   summary_table <- metric.summarise_optimisation_score(score_table=optimisation_score_table,
#                                                        method=acquisition_function)
#   
#   # Join with time table.
#   summary_table <- summary_table[time_table, on=.NATURAL]
#   
#   # Sort by decreasing optimisation score.
#   summary_table <- summary_table[order(-optimisation_score)]
#   
#   # Average objective score over known available in the score table.
#   best_parameter_data <- head(summary_table, n=n)
#   
#   return(best_parameter_data)
# }



..parse_hyperparameters_to_string <- function(id, parameter_table, parameter_list){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL
  
  # Initialise an empty string
  parameter_string <- character(0)
  
  # Iterate through parameter list and identify parameters that are being optimised
  for(current_parameter in names(parameter_list)){
    
    # Check if the current parameter is randomised.
    if(parameter_list[[current_parameter]]$randomise==TRUE){
      # Determine the value of the parameter for the set identified by the id
      # variable.
      optimal_value <- parameter_table[param_id==id, ][[current_parameter]][1]
      
      # Append to string.
      parameter_string <- c(parameter_string,
                            paste0(current_parameter, ": ", optimal_value))
    }
  }
  
  # Concatenate all separate strings into one string.
  parameter_string <- paste(parameter_string, collapse="; ")
  
  return(parameter_string)
}



..update_hyperparameter_optimisation_stopping_criteria <- function(set_data,
                                                                   stop_data=NULL,
                                                                   tolerance=1E-2){
  
  # Store the summary score. Note that the latest score is always appended.
  summary_scores <- c(stop_data$score,
                      set_data$summary_score)
  
  # Store the parameter id of the incumbent set. Note that the most recent
  # hyperparameter set identifier is always appended.
  incumbent_parameter_id <- c(stop_data$parameter_id,
                              set_data$param_id)
  
  # Read the convergence counters. Can be NULL initially.
  convergence_counter_score <- stop_data$convergence_counter_score
  if(is.null(convergence_counter_score)) convergence_counter_score <- 0L
  
  # Convergence counter for parameter identifiers.
  convergence_counter_parameter_id <- stop_data$convergence_counter_parameter_id
  if(is.null(convergence_counter_parameter_id)) convergence_counter_parameter_id <- 0L
  
  # Update convergence counter for parameter identifiers if there is no change
  # in the incumbent parameter set. Skip on the first run-through.
  if(length(incumbent_parameter_id) >= 2){
    if(all(tail(incumbent_parameter_id, n=3L) == set_data$param_id)){
      
      # Update the convergence counter.
      convergence_counter_parameter_id <- convergence_counter_parameter_id + 1L
      
    } else {
      # Reset the convergence counter.
      convergence_counter_parameter_id <- 0L
    }
  }
  
  # Assess convergence of the summary scores. This is skipped on the first
  # run-through.
  if(length(summary_scores) >= 2){
    # Compute absolute deviation from the mean.
    max_abs_deviation <- max(abs(tail(summary_scores, n=3L) - mean(tail(summary_scores, n=3L))))
    
    if(is.na(max_abs_deviation)){
      convergence_counter_score <- 0L
      
    } else if(max_abs_deviation <= tolerance){
      convergence_counter_score <- convergence_counter_score + 1L
      
    } else {
      convergence_counter_score <- 0L
    }
  }
  
  # Return list with stopping parameters.
  return(list("score"=summary_scores,
              "parameter_id"=incumbent_parameter_id,
              "convergence_counter_score" = convergence_counter_score,
              "convergence_counter_parameter_id" = convergence_counter_parameter_id))
}



..create_hyperparameter_intensify_run_table <- function(parameter_id_incumbent,
                                                        parameter_id_challenger,
                                                        score_table,
                                                        n_max_bootstraps,
                                                        n_intensify_step_bootstraps,
                                                        exploration_method){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- sampled <- run_id <- to_sample <- sample_id <- n <- NULL
  
  # Combine all parameter ids.
  parameter_ids <- c(parameter_id_incumbent, parameter_id_challenger)
  
  # Make a copy of the score table and keep only relevant parameter and run
  # identifiers.
  score_table <- unique(data.table::copy(score_table[param_id %in% parameter_ids,
                                                     c("param_id","run_id")]))
  
  # Add a sampled column that marks those elements that have been previously
  # sampled.
  score_table[, "sampled":=TRUE]
  
  # Generate run data table
  run_table <- data.table::as.data.table(expand.grid(param_id=parameter_ids,
                                                     run_id=seq_len(n_max_bootstraps),
                                                     KEEP.OUT.ATTRS=FALSE,
                                                     stringsAsFactors=FALSE))
  
  # Mark those runs that have not been sampled yet. These have an NA-value for
  # the sampled column.
  run_table <- merge(x=run_table,
                     y=score_table,
                     by=c("param_id", "run_id"),
                     all.x=TRUE)
  run_table[is.na(sampled), "sampled":=FALSE]
  
  # Add a to_sample column to mark new samples to be made.
  run_table[, "to_sample":=FALSE]
  
  # Find runs that have only been fully, partially or not sampled by the hyperparameter sets.
  sampled_runs <- run_table[sampled == TRUE, list("n"=.N), by="run_id"]
  
  # Identify run identifiers.
  fully_sampled_runs <- sampled_runs[n == length(parameter_ids)]$run_id
  partially_sampled_runs <- sampled_runs[n > 0 & n < length(parameter_ids)]$run_id
  unsampled_runs <- setdiff(seq_len(n_max_bootstraps), sampled_runs[n > 0]$run_id)
  
  # Check that there are any runs to be sampled.
  if(length(fully_sampled_runs) == n_max_bootstraps) return(NULL)
  
  # Compute the number of runs that could be completely new. This is 1/3rd of
  # n_max_bootstraps, with a minimum of 1. This makes the selection of new runs
  # more conservative, saving time and resources. When the exploration method
  # equals none, we select up to n_intensify_step_bootstraps of new runs.
  if(exploration_method == "none"){
    n_new_runs <- n_intensify_step_bootstraps
    
  } else {
    n_new_runs <- max(c(1L, floor(n_intensify_step_bootstraps / 3)))
  }
  
  # An exception should be made if there are no partially sampled runs. Then up
  # to n_intensify_step_bootstraps should be sampled.
  if(length(partially_sampled_runs) == 0) n_new_runs <- n_intensify_step_bootstraps
  
  # Iterate over the parameter identifiers and identify what runs should be
  # sampled.
  for(parameter_id in parameter_ids){
    
    # Find partially sampled runs that have not been sampled for the current
    # hyperparameter set.
    current_partially_sampled_runs <- setdiff(partially_sampled_runs,
                                              run_table[sampled==TRUE & param_id==parameter_id, ]$run_id)
    
    # Select runs to be sampled from among those that have been sampled by other
    # hyperparameter sets.
    if(length(current_partially_sampled_runs) > 0){
      run_table[run_id %in% head(current_partially_sampled_runs, n=n_intensify_step_bootstraps) & param_id==parameter_id, "to_sample":=TRUE]
    }
    
    # Add completely new runs if there is room.
    if(length(current_partially_sampled_runs) < n_intensify_step_bootstraps &
       length(unsampled_runs) > 0){
      
      # Determine the number of new runs that should be added.
      n_current_new_runs <- min(c(n_new_runs,
                                  n_intensify_step_bootstraps - length(current_partially_sampled_runs)))
      
      # Select up to current_new_runs to sample.
      run_table[run_id %in% head(unsampled_runs, n=n_current_new_runs) & param_id==parameter_id, "to_sample":=TRUE]
    }
  }
  
  # Select only runs that should be sampled, and return run_id and param_id
  # columns.
  run_table <- run_table[to_sample==TRUE , c("param_id", "run_id")]
  
  return(run_table)
}



.compare_hyperparameter_sets <- function(score_table,
                                         parameter_table,
                                         optimisation_model,
                                         parameter_id_incumbent,
                                         parameter_id_challenger,
                                         exploration_method="successive_halving",
                                         intensify_stop_p_value){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- p_value <- summary_score <- NULL
  
  if(exploration_method == "successive_halving"){
    # Compute summary scores.
    data <- .compute_hyperparameter_summary_score(score_table=score_table,
                                                  parameter_table=parameter_table,
                                                  optimisation_model=optimisation_model)
    
    # Select only data concerning the incumbent and challenger sets
    data <- data[param_id %in% c(parameter_id_incumbent, parameter_id_challenger)]
    
    # Order the relevant data according to summary score.
    data <- data[order(-summary_score)]
    
    # Remove the worst half of performers. For simplicity we increase the number
    # of selectable hyperparameters by one to account for the challenger.
    n_selectable <- floor((length(parameter_id_challenger)) / 2)
    
    # Select the new incumbent, which may be identical to the old incumbent.
    param_id_incumbent_new <- head(data, n=1L)$param_id
    
    # Select the remaining challengers, which temporarily includes the new
    # incumbent.
    param_id_challenger_new <- head(data, n=n_selectable+1L)$param_id
    
  } else if(exploration_method == "stochastic_reject"){
    # Compute hyperparameter optimisation scores. First check if the score table
    # already contains optimisation scores.
    if(!"optimisation_score" %in% colnames(score_table)){
      data <- .compute_hyperparameter_optimisation_score(score_table=score_table[param_id %in% c(parameter_id_incumbent, parameter_id_challenger)],
                                                         optimisation_function=optimisation_model@optimisation_function)
      
    } else {
      data <- data.table::copy(score_table)
    }

    # Compute summary scores.
    summary_data <- .compute_hyperparameter_summary_score(score_table=data,
                                                          parameter_table=parameter_table,
                                                          optimisation_model=optimisation_model)
    
    # Select only data concerning the incumbent and challenger sets
    summary_data <- summary_data[param_id %in% c(parameter_id_incumbent, parameter_id_challenger)]
    
    # Order the relevant data according to summary score.
    summary_data <- summary_data[order(-summary_score)]
    
    # Select the new incumbent, which may be identical to the old incumbent.
    param_id_incumbent_new <- head(summary_data, n=1L)$param_id
    
    # Select the preliminary set of challengers.
    param_id_challenger_new <- setdiff(union(parameter_id_incumbent, parameter_id_challenger),
                                       param_id_incumbent_new)
    
    # Compare optimisation scores and compute p-values.
    p_value_table <-  data[param_id %in% param_id_challenger_new,
                           ..compare_hyperparameter_optimisation_scores(challenger_score_table=.SD, 
                                                                        incumbent_score_table=data[param_id==param_id_incumbent_new]),
                           by=c("param_id")]
    
    # Select parameter identifiers with p-values above the thresholds.
    param_id_challenger_new <- p_value_table[p_value > intensify_stop_p_value, ]$param_id
    
  } else if(exploration_method == "none"){
    # Keep the incumbent set.
    param_id_incumbent_new <- parameter_id_incumbent
    
    # Keep the challengers.
    param_id_challenger_new <- parameter_id_challenger
    
  } else {
    ..error_reached_unreachable_code(paste0(".compare_hyperparameter_sets: encountered unknown exploration method: ", exploration_method))
  }
  
  # Update param_id_challenger_new by removing the incumbent set.
  param_id_challenger_new <- setdiff(union(param_id_incumbent_new, param_id_challenger_new),
                                     param_id_incumbent_new)
  
  return(list("parameter_id_incumbent"=param_id_incumbent_new,
              "parameter_id_challenger"=param_id_challenger_new))
}



..compare_hyperparameter_optimisation_scores <- function(challenger_score_table,
                                                         incumbent_score_table){
  # Suppress NOTES due to non-standard evaluation in data.table
  run_id <- optimisation_score <- NULL
  
  # Find matching bootstrap ids
  run_id_match    <- intersect(challenger_score_table$run_id,
                               incumbent_score_table$run_id)
  
  # Select rows that match. Note that optimisation score is updated locally to
  # avoid update by reference on protected slices of the score table in
  # .compare_hyperparameter_optimisation_scores.
  challenger_score_table <- data.table::copy(challenger_score_table[run_id %in% run_id_match, ])[is.na(optimisation_score), "optimisation_score":=-1.0]
  incumbent_score_table <- data.table::copy(incumbent_score_table[run_id %in% run_id_match, ])[is.na(optimisation_score), "optimisation_score":=-1.0]
  
  # Find p-value for matching means using paired wilcoxon rank test.
  p_value <- suppressWarnings(stats::wilcox.test(x=challenger_score_table[order(run_id_match)]$optimisation_score,
                                                 y=incumbent_score_table[order(run_id_match)]$optimisation_score,
                                                 paired=TRUE,
                                                 alternative="less")$p.value)
  # Return list with p-values.
  return(list("p_value"=p_value))
}



.encode_categorical_hyperparameters <- function(parameter_list){
  
  # Iterate over hyperparameters.
  parameter_list <- lapply(parameter_list, ..encode_categorical_hyperparameters)
  
  return(parameter_list)
}



..encode_categorical_hyperparameters <- function(x){
  # Encodes the init_config element of x in case x is a categorical
  # hyperparameter.
  
  # Skip if x is not categorical.
  if(x$type != "factor") return(x)
  
  if(x$randomise){
    # Assign default range + initial value as levels, in case the hyperparameter
    # is randomised.
    x$init_config <- factor(x$init_config, levels=c(union(x$range, x$init_config)))
    
  } else {
    # Assign only the selected level as factor.
    x$init_config <- factor(x$init_config, levels=x$init_config)
  }
  
  return(x)
}



.set_signature_size <- function(object,
                                rank_table_list,
                                suggested_range=NULL){
  
  if(is.null(suggested_range)) suggested_range <- c(1, Inf)
  
  # Update minimum signature size in object.
  object@hyperparameters$sign_size <- min(suggested_range)
  
  # Some variable importance methods fail to produce a list (e.g. none,
  # signature_only, random). We create a single element list in that case.
  if(is_empty(rank_table_list)) rank_table_list <- list(NULL)
  
  # Determine the lower range of the signature.
  signature_list <- lapply(rank_table_list,
                           function(rank_table, object){
                             get_signature(object=object,
                                           rank_table=rank_table)
                           },
                           object=object)
  
  # Find the minimum number of 
  min_signature_size <- min(sapply(signature_list, length))
  
  # Update maximum signature size in the list.
  object@hyperparameters$sign_size <- max(suggested_range)
  
  # Determine the lower range of the signature.
  signature_list <- lapply(rank_table_list,
                           function(rank_table, object){
                             get_signature(object=object,
                                           rank_table=rank_table)
                           },
                           object=object)
  
  # Update maximum signature size in the list.
  max_signature_size <- max(sapply(signature_list, length))
  
  # Add minimum and maximum signature size, if necessary.
  if(any(suggested_range < min_signature_size)) suggested_range <- c(suggested_range, min_signature_size)
  if(any(suggested_range > max_signature_size)) suggested_range <- c(suggested_range, max_signature_size)
  
  # Limit range to unique values within the range.
  suggested_range <- suggested_range[suggested_range >= min_signature_size & suggested_range <= max_signature_size]
  suggested_range <- unique(suggested_range)
  
  return(suggested_range)
}
