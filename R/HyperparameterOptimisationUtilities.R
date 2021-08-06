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
                                              optimisation_score_table=NULL,
                                              acquisition_function=NULL,
                                              n_max_bootstraps=NULL){
  # Creates a run table from the run identifiers and parameter identifiers.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  time_taken <- NULL
  
  if(measure_time){
    # Filter by time. This filters out the hyperparameter sets that took very
    # long to complete, while not delivering good enough performance.
    
    # Get acquisition details.
    acquisition_data <- ..get_acquisition_function_parameters(optimisation_score_table=optimisation_score_table,
                                                              acquisition_function=acquisition_function,
                                                              n_max_bootstraps=n_max_bootstraps)
    
    # Select fitting hyperparameters.
    parameter_ids <- optimisation_score_table[time_taken <= acquisition_data$max_time]$param_id
  }
  
  if(is.null(parameter_ids)) ..error_reached_unreachable_code("..create_hyperparameter_run_table: parameter_ids cannot be NULL.")
  
  run_table <- data.table::as.data.table(expand.grid(param_id=parameter_ids,
                                                     run_id=run_ids,
                                                     KEEP.OUT.ATTRS=FALSE,
                                                     stringsAsFactors=FALSE))
  
  return(run_table)
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
    
    if(verbose){
      logger.message(paste0("Computing variable importance for ", length(bootstraps), " bootstraps."),
                     indent=message_indent)
    }
    
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
  vimp_table <- .vimp(object=object, data=data)
  
  # Rename columns.
  data.table::setnames(vimp_table,
                       old=c("score", "rank"),
                       new=c("aggr_score", "aggr_rank"))
  
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
    # Create a scoring table.
    score_table <- fam_mapply_lb(cl = cl,
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
    
    # Create a scoring table.
    score_table <- fam_mapply(cl = cl,
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
  
  # Aggregate the table.
  score_table$results <- data.table::rbindlist(score_table$results, use.names=TRUE)
  
  # Add in process times to the results.
  score_table$results[, "time_taken":=rep(score_table$process_time, each=2*length(metric_objects))]
  
  # Return scores.
  return(score_table)
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
  if(!is_empty(rank_table)){
    object <- set_signature(object=object,
                            rank_table=rank_table,
                            minimise_footprint=TRUE)
    
  } else {
    object <- set_signature(object=object,
                            signature_features=get_feature_columns(data),
                            minimise_footprint=TRUE)
    
  }

  # Train model with the set of hyperparameters.
  object <- .train(object=object,
                   data=data_training,
                   get_additional_info=FALSE)
  
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
  
  return(score_table)
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



..get_acquisition_function_parameters <- function(optimisation_score_table,
                                                  acquisition_function,
                                                  n_max_bootstraps){
  # Determine optimal score.
  best_hyperparameter_set <- ..get_best_hyperparameter_set(optimisation_score_table=optimisation_score_table,
                                                           acquisition_function=acquisition_function,
                                                           n=1L)
  
  # Determine the optimal parameter score tau.
  tau <- best_hyperparameter_set$optimisation_score
  
  # Determine time corresponding to the best parameter set.
  time <- best_hyperparameter_set$time_taken
  
  # Determine round t.
  t <- max(optimisation_score_table[, list("n"=.N), by="param_id"]$n)
  
  # Determine maximum time that can be countenanced.
  max_time <- (5.0 - 4.0 * t / n_max_bootstraps) * time
  
  # Check that the maximum time is not trivially small (i.e. less than 10
  # seconds).
  if(is.na(max_time)){
    max_time <- Inf
  } else if(max_time < 10.0) {
    max_time <- 10.0
  }
  
  return(list("t"=t,
              "time"=time,
              "max_time"=max_time,
              "tau"=tau,
              "n"=n_max_bootstraps))
}



..get_best_hyperparameter_set <- function(optimisation_score_table,
                                          acquisition_function,
                                          n=1L){
  # Find the best configurations based on the optimisation score
  
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- time_taken <- .NATURAL <- NULL
  
  # Compute time taken.
  time_table <- optimisation_score_table[, list("time_taken"=stats::median(time_taken, na.rm=TRUE)), by="param_id"]
  
  # Compute the summary score per parameter id.
  summary_table <- metric.summarise_optimisation_score(score_table=optimisation_score_table,
                                                       method=acquisition_function)
  
  # Join with time table.
  summary_table <- summary_table[time_table, on=.NATURAL]
  
  # Sort by decreasing optimisation score.
  summary_table <- summary_table[order(-optimisation_score)]
  
  # Average objective score over known available in the score table.
  best_parameter_data <- head(summary_table, n=n)
  
  return(best_parameter_data)
}



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



..initialise_hyperparameter_optimisation_stopping_criteria <- function(){
  # Initialise the stop list.
  return(list("score"=numeric(0),
              "parameter_id"=integer(0),
              "convergence_counter"= 0L))
}



..update_hyperparameter_optimisation_stopping_criteria <- function(score_table,
                                                                   parameter_id_incumbent,
                                                                   stop_list,
                                                                   tolerance=1E-2,
                                                                   acquisition_function){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL
  
  # Compute aggregate optimisation score.
  summary_score_table <- metric.summarise_optimisation_score(score_table=score_table[param_id==parameter_id_incumbent, ],
                                                             method=acquisition_function,
                                                             replace_na=FALSE)
  
  # Read the convergence counter.
  convergence_counter <- stop_list$convergence_counter
  
  # Assess convergence and stability
  if(length(stop_list$score) >= 4){
    
    # Calculate mean over last 4 incumbent scores and compare with incumbent
    # Note that if the series is converging, the difference between the moving
    # average over the last 4 incumbents and the current incumbent should be
    # positive or 0.
    recent_scores <- tail(stop_list$score, n=4L)
    recent_parameter_id <- tail(stop_list$parameter_id, n=4L)
    
    # Determine the max absolute deviation from the mean.
    max_abs_deviation <- max(recent_scores) - min(recent_scores)
    
    # Start counting convergence if:
    #
    # 1. The maximum absolute deviation is below the tolerance.
    #
    # 2. the param_id of the incumbent dataset has not changed over the last
    # iterations.
    if(is.na(max_abs_deviation)){
      # This means that a combination of parameters that leads to a correctly
      # predicting model was not found.
      convergence_counter <- 0L
      
    } else if(max_abs_deviation <= tolerance | all(recent_parameter_id == parameter_id_incumbent)){
      # Check if all recent optimal parameter sets are the same or the result is
      # relatively stable.
      convergence_counter <- convergence_counter + 1L
      
    } else {
      # This means that there is no convergence.
      convergence_counter <- 0L
    }
  } else {
    convergence_counter <- 0L
  }
  
  # Append new items to the stop list.
  return(list("score"=c(stop_list$score, summary_score_table$optimisation_score),
              "parameter_id"=c(stop_list$parameter_id, parameter_id_incumbent),
              "convergence_counter"= convergence_counter))
}



..update_hyperparameter_optimisation_stopping_criteria <- function(score_table,
                                                                   parameter_id_incumbent,
                                                                   stop_list, tolerance=1E-2,
                                                                   acquisition_function){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL
  
  # Compute aggregate optimisation score.
  summary_score_table <- metric.summarise_optimisation_score(score_table=score_table[param_id==parameter_id_incumbent, ],
                                                             method=acquisition_function,
                                                             replace_na=FALSE)
  
  # Read the convergence counter.
  convergence_counter <- stop_list$convergence_counter
  
  # Assess convergence and stability
  if(length(stop_list$score) >= 4){
    
    # Calculate mean over last 4 incumbent scores and compare with incumbent
    # Note that if the series is converging, the difference between the moving
    # average over the last 4 incumbents and the current incumbent should be
    # positive or 0.
    recent_scores <- tail(stop_list$score, n=4L)
    recent_parameter_id <- tail(stop_list$parameter_id, n=4L)
    
    # Determine the max absolute deviation from the mean.
    max_abs_deviation <- max(recent_scores) - min(recent_scores)
    
    # Start counting convergence if:
    #
    # 1. The maximum absolute deviation is below the tolerance.
    #
    # 2. the param_id of the incumbent dataset has not changed over the last
    # iterations.
    if(is.na(max_abs_deviation)){
      # This means that a combination of parameters that leads to a correctly
      # predicting model was not found.
      convergence_counter <- 0L
      
    } else if(max_abs_deviation <= tolerance | all(recent_parameter_id == parameter_id_incumbent)){
      # Check if all recent optimal parameter sets are the same or the result is
      # relatively stable.
      convergence_counter <- convergence_counter + 1L
      
    } else {
      # This means that there is no convergence.
      convergence_counter <- 0L
    }
  } else {
    convergence_counter <- 0L
  }
  
  # Append new items to the stop list.
  return(list("score"=c(stop_list$score, summary_score_table$optimisation_score),
              "parameter_id"=c(stop_list$parameter_id, parameter_id_incumbent),
              "convergence_counter"= convergence_counter))
}



..create_hyperparameter_intensify_run_table <- function(parameter_id_incumbent,
                                                        parameter_id_challenger,
                                                        score_table,
                                                        n_max_bootstraps,
                                                        n_intensify_step_bootstraps){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- sampled <- run_id <- to_sample <- sample_id <- NULL
  
  # Combine all parameter ids.
  parameter_ids <- c(parameter_id_incumbent, parameter_id_challenger)
  
  # Make a copy of the score table and keep only relevant parameter and run
  # identifiers.
  score_table <- data.table::copy(score_table[param_id %in% parameter_ids,
                                              c("param_id","run_id")])
  
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
  
  # Find all run ids that have been sampled using the incumbent hyperparameter
  # set.
  run_id_incumbent <- unique(run_table[param_id==parameter_id_incumbent & sampled==TRUE, ]$run_id)
  
  # Find all run ids that have been sampled using any of the challenger
  # hyperparameter sets.
  run_id_challenger <- unique(run_table[param_id %in% parameter_id_challenger & sampled==TRUE, ]$run_id)
  
  # Determine run ids that have been sampled by challengers, but not by the
  # incumbent.
  run_id_incumbent_new <- setdiff(run_id_challenger, run_id_incumbent)
  
  # Identify if additional new bootstraps should be made for the incumbent.
  if(length(run_id_incumbent_new) < n_intensify_step_bootstraps){
    # Sample n_sample new runs
    n_sample <- n_intensify_step_bootstraps - length(run_id_incumbent_new)
    
    # Determine which run ids have not been sampled by the incumbent.
    run_id_incumbent_new_samples <- unique(run_table[param_id==parameter_id_incumbent & sampled==FALSE, ]$run_id)
    
    # Update n_sample to be the smallest of itself or the number of unsampled
    # runs. This prevents an overflow beyond n_max_bootstraps.
    n_sample <- min(c(n_sample, length(run_id_incumbent_new_samples)))
    
    # Sample and add to run_id_incumbent_new
    if(n_sample > 0){
      run_id_incumbent_new <- c(run_id_incumbent_new,
                                fam_sample(x=run_id_incumbent_new_samples,
                                           size=n_sample,
                                           replace=FALSE))
    }
    
  } else {
    # Sample up to n_intensify_step_bootstraps from run_id_incumbent_new
    run_id_incumbent_new <- fam_sample(x=run_id_incumbent_new, 
                                       size=n_intensify_step_bootstraps,
                                       replace=FALSE)
  }
  
  # Add new runs to the incumbent and applicable challengers.
  if(length(run_id_incumbent_new) > 0){
    run_table[run_id %in% run_id_incumbent_new & sampled==FALSE,
              "to_sample":=TRUE]
  }
  
  # Add incumbent runs that have not been performed by the challengers to the
  # sample.
  run_table[param_id %in% parameter_id_challenger & run_id %in% run_id_incumbent & sampled==FALSE,
            "to_sample":=TRUE]
  
  # Select only those runs that are to be sampled
  run_table <- run_table[to_sample==TRUE, ]
  
  # Do not sample more than n_intensify_step_bootstraps for challengers. We
  # first create a randomly ordered sample identifier, and than select up to
  # n_intensify_step_bootstraps runs from that list.
  run_table[param_id %in% parameter_id_challenger,
            "sample_id":=fam_sample(x=seq_len(.N), size=.N), by="param_id"]
  
  run_table <- run_table[(param_id == parameter_id_incumbent) |
                           (param_id %in% parameter_id_challenger & sample_id <= n_intensify_step_bootstraps),
                         c("param_id", "run_id")]
  
  return(run_table)
}



.compare_hyperparameter_optimisation_scores <- function(score_table,
                                                        parameter_id_incumbent,
                                                        parameter_id_challenger,
                                                        acquisition_function,
                                                        exploration_method="successive_halving",
                                                        intensify_stop_p_value){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- challenger_score <- p_value <- NULL
  
  # Determine scores and comparison p-values
  comparison_table <- score_table[param_id %in% parameter_id_challenger,
                                  ..compare_hyperparameter_optimisation_scores(challenger_score_table=.SD, 
                                                                               incumbent_score_table=score_table[param_id==parameter_id_incumbent, ],
                                                                               exploration_method=exploration_method,
                                                                               acquisition_function=acquisition_function),
                                  by=c("param_id")]
  
  if(any(comparison_table$challenger_score > comparison_table$incumbent_score)){
    # If a challenger beats the incumbent, replace the incumbent.
    param_id_incumbent_new  <- head(comparison_table[order(-challenger_score)], n=1L)$param_id
    
  } else {
    # Do not replace the incumbent.
    param_id_incumbent_new <- parameter_id_incumbent
  }
  
  if(exploration_method == "stochastic_reject"){
    # Remove new incumbent from the challengers and determine remaining
    # challengers by against significance threshold alpha
    param_id_challenger_new <- comparison_table[param_id != param_id_incumbent_new &
                                                  p_value > intensify_stop_p_value, ]$param_id
    
  } else if(exploration_method == "successive_halving"){
    # Remove the worst half of performers.
    n_selectable <- floor(length(parameter_id_challenger + 1) / 2)
    
    # Order by decreasing challenger score and keep the best half.
    param_id_challenger_new <- head(comparison_table[order(-challenger_score)], n=n_selectable)$param_id
    
  } else if(exploration_method == "none"){
    # Do not remove anything.
    param_id_challenger_new <- parameter_id_challenger
  }
  
  # Add incumbent to the challengers.
  param_id_challenger_new <- union(param_id_challenger_new, parameter_id_incumbent)
  
  # Remove the new incumbent from the challengers.
  param_id_challenger_new <- setdiff(param_id_challenger_new, param_id_incumbent_new)
  
  return(list("parameter_id_incumbent"=param_id_incumbent_new,
              "parameter_id_challenger"=param_id_challenger_new))
}



..compare_hyperparameter_optimisation_scores <- function(challenger_score_table,
                                                         incumbent_score_table,
                                                         exploration_method,
                                                         acquisition_function){
  # Suppress NOTES due to non-standard evaluation in data.table
  run_id <- optimisation_score <- NULL
  
  # Find matching bootstrap ids
  run_id_match    <- intersect(challenger_score_table$run_id,
                               incumbent_score_table$run_id)
  
  # Select rows that match.
  challenger_score_table <- challenger_score_table[run_id %in% run_id_match, ][is.na(optimisation_score), "optimisation_score":=-1.0]
  incumbent_score_table <- incumbent_score_table[run_id %in% run_id_match, ][is.na(optimisation_score), "optimisation_score":=-1.0]
  
  # Calculate the aggregate optimisation score for challenger and incumbent for
  # the matching bootstraps.
  challenger_score <- metric.summarise_optimisation_score(score_table=challenger_score_table,
                                                          method=acquisition_function)$optimisation_score
  incumbent_score <- metric.summarise_optimisation_score(score_table=incumbent_score_table,
                                                         method=acquisition_function)$optimisation_score
  
  if(exploration_method == "stochastic_reject"){
    # Find p-value for matching means using paired wilcoxon rank test
    p_value <- suppressWarnings(stats::wilcox.test(x=challenger_score_table[order(run_id_match)]$optimisation_score,
                                                   y=incumbent_score_table[order(run_id_match)]$optimisation_score,
                                                   paired=TRUE,
                                                   alternative="less")$p.value)
    # Return list with data
    return(list("challenger_score"=challenger_score,
                "incumbent_score"=incumbent_score,
                "p_value"=p_value))
    
  } else {
    # Return list with data
    return(list("challenger_score"=challenger_score,
                "incumbent_score"=incumbent_score))
  }
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
    # Assign valid range as levels, in case the hyperparameter is randomised.
    x$init_config <- factor(x$init_config, levels=x$valid_range)
    
  } else {
    # Assign only the selected level as factor.
    x$init_config <- factor(x$init_config, levels=x$init_config)
  }
  
  return(x)
}
