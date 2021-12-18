.create_hyperparameter_challenger_sets <- function(parameter_table,
                                                   score_table,
                                                   parameter_list,
                                                   time_optimisation_model,
                                                   smbo_iter,
                                                   hyperparameter_learner,
                                                   optimisation_function,
                                                   acquisition_function,
                                                   n_max_bootstraps,
                                                   n_challengers=20L,
                                                   measure_time,
                                                   random_admixture_fraction=0.20){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- expected_train_time <- expected_improvement <- optimisation_score <- weight <- NULL 
  is_observed <- .NATURAL <- i.param_id <- NULL
  
  # Set the maximum number of local steps based on the number of randomisable
  # hyperparameters.
  n_max_local_steps <- 2 * sum(sapply(parameter_list, function(x) (x$randomise)))
  
  # Compute the optimisation score. This creates a table with optimisation
  # scores per bootstrap and parameter identifier.
  optimisation_score_table <- .compute_hyperparameter_optimisation_score(score_table=score_table,
                                                                         optimisation_function=optimisation_function)
  
  # Check for minimal size of optimisation_score_table to prevent errors in
  # laGP.
  if(hyperparameter_learner == "gaussian_process" & nrow(score_table) < 8){
    logger.warning(paste0("Insufficient instances (<8) in the score table to derive an approximate Gaussian Process. ",
                          "Switching to random search until suffcient instances are created."))
    
    hyperparameter_learner <- "random"
  }
  
  # Check if all hyperparameters are invariant.
  if(!hyperparameter_learner %in% c("random", "random_search")){
    
    # Get parameter names.
    parameter_names <- setdiff(colnames(parameter_table),
                               c("param_id", "run_id", "optimisation_score", "time_taken"))
    
    if(all(sapply(parameter_table[, mget(parameter_names)], is_singular_data))){
      logger.warning(paste0("All hyperparameters are currently invariant. Switching to random search until non-invariant instances are created."))
      
      hyperparameter_learner <- "random"
    }
  }
  
  # Create a model to predict the optimisation score for a given parameter set.
  score_optimisation_model <- .create_hyperparameter_score_optimisation_model(hyperparameter_learner=hyperparameter_learner,
                                                                              score_table=optimisation_score_table,
                                                                              parameter_table=parameter_table)
  
  # Find information regarding the dataset that has the highest optimisation
  # score.
  incumbent_set_data <- ..get_best_hyperparameter_set(optimisation_score_table=optimisation_score_table,
                                                      acquisition_function=acquisition_function,
                                                      n=1)
  
  # Get acquisition data.
  acquisition_data <- ..get_acquisition_function_parameters(optimisation_score_table=optimisation_score_table,
                                                            acquisition_function=acquisition_function,
                                                            n_max_bootstraps=n_max_bootstraps)
  
  ##### Generate random sets----------------------------------------------------
  if(hyperparameter_learner %in% c("random", "random_search")) random_admixture_fraction <- 1.0
  
  # In general, draw about 10 times as much random sets then required.
  n_random_sets <- ceiling(n_challengers * random_admixture_fraction)
  
  # Generate random sets
  random_sets <- lapply(seq_len(n_random_sets * 10), function(ii,
                                                              parameter_list){
    
    # Create random configuration.
    random_set <- ..randomise_hyperparameter_set(parameter_list=parameter_list, local=FALSE)
    
  },
  parameter_list=parameter_list)
  
  # Combine random sets into a data.table.
  random_sets <- unique(data.table::rbindlist(random_sets,
                                              use.names=TRUE))
  
  # Compute expected improvement.
  if(!is_empty(random_sets) & !hyperparameter_learner %in% c("random", "random_search")){
    random_sets[, "expected_improvement":=.compute_utility_value(parameter_set=.SD,
                                                                 score_model=score_optimisation_model,
                                                                 hyperparameter_learner=hyperparameter_learner,
                                                                 acquisition_data=acquisition_data,
                                                                 acquisition_function=acquisition_function)]
  }
  
  # Compute expect train time.
  if(!is_empty(random_sets)){
    random_sets[, "expected_train_time":=.compute_expected_train_time(parameter_set=.SD,
                                                                      time_model=time_optimisation_model)]
    
  }
  
  ##### Identify local sets ----------------------------------------------------
  n_local_sets <- ceiling(n_challengers * (1.0 - random_admixture_fraction))
  
  # Find local sets
  local_sets <- list()
  
  if(n_local_sets > 0 & !hyperparameter_learner %in% c("random", "random_search")){
    
    # Create a table of summary scores to identify the best sets.
    summary_score_table <- metric.summarise_optimisation_score(score_table=optimisation_score_table, method=acquisition_function)
    
    # Find the most promising samples by comparing against the median.
    summary_score_table[, "weight":=optimisation_score - stats::median(optimisation_score)]
    
    # If all parameters have the same optimisation score (an edge case), assign
    # weight 1 to each, that is, all hyperparameter sets are as likely to be
    # sampled.
    if(all(summary_score_table$weight == 0.0)) summary_score_table[, "weight":=1.0]
    
    # Remove 0 and negative weights.
    summary_score_table <- summary_score_table[weight > 0.0]
    
    # Select the hyperparameter ids to sample, starting with the most promising
    # samples first.
    summary_score_table <- summary_score_table[, "n_select":=round(2 * n_local_sets * weight^2/sum(weight^2))][order(-optimisation_score )]
    selected_best_parameter_id <- unlist(mapply(function(x, n) (rep(x, times=n)), x=summary_score_table$param_id, n=summary_score_table$n_select))
    
    # Perform a local search.
    for(best_param_id in selected_best_parameter_id){
      
      # Initialise iterator variable
      iter_local_step <- 0
      
      # Select initial configuration for local search
      selected_set <- parameter_table[param_id==best_param_id, ]
      
      while(iter_local_step < n_max_local_steps){
        
        # Randomise configuration
        local_random_set <- ..randomise_hyperparameter_set(parameter_table=selected_set,
                                                           parameter_list=parameter_list,
                                                           local=TRUE)
        
        # Add configuration to list
        local_sets <- c(local_sets,
                        list(local_random_set))
        
        # Continue search from current set
        selected_set <- local_random_set
        
        # Update iterator
        iter_local_step <- iter_local_step + 1
      }
    }
  }
  
  # Combine local sets.
  local_sets <- unique(data.table::rbindlist(local_sets))
  
  if(!is_empty(local_sets)){
    # Compute expected improvement.
    local_sets[, "expected_improvement":=.compute_utility_value(parameter_set=.SD,
                                                                 score_model=score_optimisation_model,
                                                                 hyperparameter_learner=hyperparameter_learner,
                                                                 acquisition_data=acquisition_data,
                                                                 acquisition_function=acquisition_function)]
    
    # Compute expected train time.
    local_sets[, "expected_train_time":=.compute_expected_train_time(parameter_set=.SD,
                                                                     time_model=time_optimisation_model)]
  }
  
  
  ##### Select challengers -----------------------------------------------------
  
  local_challenger_sets <- list()
  random_challenger_sets <- list()
  
  if(hyperparameter_learner %in% c("random", "random_search")){
    # Preferentially select in the following order:
    # 1. Random sets that were not observed before.
    # 2. Random sets that were observed before.
    
    # Remove incumbent set.
    if(!is_empty(random_sets)) random_sets <- random_sets[!parameter_table[param_id == incumbent_set_data$param_id], on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(random_sets)) random_sets <- random_sets[expected_train_time < acquisition_data$max_time]
    
    # Consider random sets.
    if(!is_empty(random_sets)){
      
      # Set observed status
      random_sets[, "is_observed":=FALSE]
      random_sets[parameter_table, "is_observed":=TRUE, on=.NATURAL]
      
      # Random sets that were not observed before.
      if(any(!random_sets$is_observed) & n_random_sets > 0){
        # Select up to n_random_sets of random samples sets that were not
        # selected previously.
        selected_sets <- head(random_sets[is_observed==FALSE], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- list(selected_sets)
        n_challengers <- n_challengers - nrow(selected_sets)
        n_random_sets <- min(c(n_random_sets - nrow(selected_sets), n_challengers))
      }
      
      # Random sets that were observed before.
      if(any(random_sets$is_observed) & n_random_sets > 0){
        # Select up to n_random_sets of random samples sets that were observed
        # selected previously.
        selected_sets <- head(random_sets[is_observed==TRUE], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- c(random_challenger_sets, list(selected_sets))
        n_challengers <- n_challengers - nrow(selected_sets)
      }
    }
    
    if(!is_empty(random_challenger_sets)){
      # Concatenate challenger sets.
      challenger_sets <- data.table::rbindlist(random_challenger_sets, use.names=TRUE)
      
      # Remove expected time from challenger sets.
      challenger_sets[, ":="("expected_train_time"=NULL,
                             "is_observed"=NULL)]
      
    } else {
      challenger_sets <- NULL
    }
    
    
  } else {
    # Preferentially select in the following order:
    # 1. Local sets that were not observed before, with highest utility first.
    # 2. Local sets that were observed before, with highest utility first.
    # 3. Random sets that were not observed before, with highest utility first.
    # 4. Random sets that were observed before, with highest utility first.
    
    # First, remove incumbent.
    if(!is_empty(local_sets)) local_sets <- local_sets[!parameter_table[param_id == incumbent_set_data$param_id], on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(local_sets)) local_sets <- local_sets[expected_train_time < acquisition_data$max_time]
    
    # Consider local sets.
    if(!is_empty(local_sets)){
      
      # Set observed status
      local_sets[, "is_observed":=FALSE]
      local_sets[parameter_table, "is_observed":=TRUE, on=.NATURAL]
      
      # Local sets that were not observed before, with highest utility first.
      if(any(!local_sets$is_observed)){
        # Select up to n_local_sets of locally sampled sets that were not
        # selected previously.
        selected_sets <- head(local_sets[is_observed==FALSE][order(-expected_improvement)], n=n_local_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        local_challenger_sets <- list(selected_sets)
        n_challengers <- n_challengers - nrow(selected_sets)
        n_local_sets <- min(c(n_local_sets - nrow(selected_sets), n_challengers))
      }
      
      # Local sets that were observed before, with highest utility first.
      if(any(local_sets$is_observed) & n_local_sets > 0){
        # Select up to n_local_sets of locally samples sets that were observed
        # selected previously.
        selected_sets <- head(local_sets[is_observed==TRUE][order(-expected_improvement)], n=n_local_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        local_challenger_sets <- c(local_challenger_sets, list(selected_sets))
        n_challengers <- n_challengers - nrow(selected_sets)
      }
    }
    
    if(!is_empty(local_challenger_sets)){
      # Concatenate challenger sets.
      local_challenger_sets <- data.table::rbindlist(local_challenger_sets, use.names=TRUE)
      
      # Remove expected improvement and time from challenger sets.
      local_challenger_sets[, ":="("expected_improvement"=NULL,
                                   "expected_train_time"=NULL,
                                   "is_observed"=NULL)]
    }
    
    # Remove incumbent and already selected challenger sets.
    if(!is_empty(random_sets)) random_sets <- random_sets[!parameter_table[param_id == incumbent_set_data$param_id], on=.NATURAL]
    if(!is_empty(random_sets) & !is_empty(local_challenger_sets) > 0) random_sets <- random_sets[!local_challenger_sets, on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(random_sets)) random_sets <- random_sets[expected_train_time < acquisition_data$max_time]
    
    # Update n_random_sets to compensate for selected challengers.
    n_random_sets <- min(c(n_challengers, n_random_sets))
    
    # Consider random sets.
    if(!is_empty(random_sets)){
      
      # Set observed status
      random_sets[, "is_observed":=FALSE]
      random_sets[parameter_table, "is_observed":=TRUE, on=.NATURAL]
      
      # Random sets that were not observed before, with highest utility first.
      if(any(!random_sets$is_observed) & n_random_sets > 0){
        # Select up to n_random_sets of random samples sets that were not
        # selected previously.
        selected_sets <- head(random_sets[is_observed==FALSE][order(-expected_improvement)], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- list(selected_sets)
        n_challengers <- n_challengers - nrow(selected_sets)
        n_random_sets <- min(c(n_random_sets - nrow(selected_sets), n_challengers))
      }
      
      # Random sets that were observed before, with highest utility first.
      if(any(random_sets$is_observed) & n_random_sets > 0){
        # Select up to n_random_sets of random samples sets that were observed
        # selected previously.
        selected_sets <- head(random_sets[is_observed==TRUE][order(-expected_improvement)], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- c(random_challenger_sets, list(selected_sets))
        n_challengers <- n_challengers - nrow(selected_sets)
      }
    }
    
    if(!is_empty(random_challenger_sets)){
      # Concatenate challenger sets.
      random_challenger_sets <- data.table::rbindlist(random_challenger_sets, use.names=TRUE)
      
      # Remove expected improvement and time from challenger sets.
      random_challenger_sets[, ":="("expected_improvement"=NULL,
                                    "expected_train_time"=NULL,
                                    "is_observed"=NULL)]
    }
    
    # Combine to challenger sets.
    challenger_sets <- data.table::rbindlist(list(local_challenger_sets, random_challenger_sets),
                                             use.names=TRUE)
  }
  
  # Add parameter identifiers for known sets.
  if(!is_empty(challenger_sets)){
    # Left-join with parameter table to add in known parameters
    challenger_sets <- challenger_sets[parameter_table, "param_id":=i.param_id, on=.NATURAL]
    
    # Fill out missing parameter ids.
    if(any(is.na(challenger_sets$param_id))){
      # Find the largest existing parameter id.
      max_param_id <- max(parameter_table$param_id)
      
      # Generate parameter ids for new parameter sets. 
      challenger_sets[is.na(param_id), "param_id":=max_param_id + .I]
    }
    
  } else {
    challenger_sets <- NULL
  }
  
  return(challenger_sets)
}



.create_hyperparameter_score_optimisation_model <- function(hyperparameter_learner,
                                                            score_table,
                                                            parameter_table){
  
  if(hyperparameter_learner == "random_forest"){
    model <- ..hyperparameter_random_forest_optimisation_score_learner(score_table=score_table,
                                                                       parameter_table=parameter_table)
    
  } else if(hyperparameter_learner == "gaussian_process"){
    model <- ..hyperparameter_gaussian_process_optimisation_score_learner(score_table=score_table,
                                                                          parameter_table=parameter_table)
    
  } else if(hyperparameter_learner %in% c("bayesian_additive_regression_trees", "bart")){
    model <- ..hyperparameter_bart_optimisation_score_learner(score_table=score_table,
                                                              parameter_table=parameter_table)
    
  } else if(hyperparameter_learner %in% c("random", "random_search")){
    model <- NULL
    
  } else {
    ..error_reached_unreachable_code(paste0(".create_hyperparameter_score_optimisation_model: hyperparameter_learner was not recognised: ", hyperparameter_learner))
  }
  
  return(model)
}
  


.create_hyperparameter_time_optimisation_model <- function(hyperparameter_learner="random_forest",
                                                           score_table,
                                                           parameter_table){
  
  model <- ..hyperparameter_random_forest_time_learner(score_table=score_table,
                                                       parameter_table=parameter_table)
  
  # if(hyperparameter_learner == "random_forest"){
  #   model <- ..hyperparameter_random_forest_time_learner(score_table=score_table,
  #                                                        parameter_table=parameter_table)
  # } else {
  #   ..error_reached_unreachable_code(paste0(".create_hyperparameter_time_optimisation_model: hyperparameter_learner was not recognised: ", hyperparameter_learner))
  # }
  
  return(model)
}



..hyperparameter_random_forest_optimisation_score_learner <- function(score_table, parameter_table){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- NULL
  
  # Check if package is installed
  require_package(x="ranger", purpose="to perform model-based hyperparameter optimisation")
  
  # Merge score and parameter data tables on param id.
  joint_table <- merge(x=score_table,
                       y=parameter_table,
                       by="param_id",
                       all=FALSE)
  
  # Replace NA entries with the minimum optimisation score.
  joint_table[is.na(optimisation_score), optimisation_score:=-1.0]
  
  # Get parameter names.
  parameter_names <- setdiff(colnames(parameter_table),
                             c("param_id", "run_id", "optimisation_score", "time_taken"))
  
  # Hyperparameters for the random forest.
  n_tree <- 400
  n_train <- nrow(joint_table)
  sample_fraction <- max(c(0.3, min(c(1, 1/(0.025*n_train)))))
  
  # Parse formula.
  formula <- stats::reformulate(termlabels=parameter_names,
                                response="optimisation_score")
  
  # Train random forest. Note that ranger is imported through the NAMESPACE.
  rf_model <- ranger::ranger(formula,
                             data=joint_table,
                             num.trees=n_tree,
                             num.threads=1L,
                             sample.fraction=sample_fraction,
                             verbose=FALSE)
  
  return(rf_model)
}



..hyperparameter_random_forest_time_learner <- function(score_table, parameter_table){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  time_taken <- NULL
  
  # Check if package is installed
  require_package(x="ranger", purpose="to predict process time of hyperparameter sets")
  
  # Fill NA values with large, but finite values. Determine the maximum time.
  if(!all(is.na(score_table$time_taken))){
    max_time_taken <- max(score_table$time_taken, na.rm=TRUE)
    
  } else {
    # Set max time to an improbable 86400 seconds, in case all time values are NA.
    # A single model trained in familiar should never take a day to finish.
    max_time_taken <- 86400.0
  }
  
  # Merge score and parameter data tables on param id.
  joint_table <- merge(x=score_table,
                       y=parameter_table,
                       by="param_id",
                       all=FALSE)
  
  # Replace NA entries with the minimum optimisation score.
  joint_table[is.na(time_taken), time_taken:=10*max_time_taken]
  
  # Get parameter names.
  parameter_names <- setdiff(colnames(parameter_table),
                             c("param_id", "run_id", "optimisation_score", "time_taken"))
  
  # Hyperparameters for the random forest.
  n_tree <- 400
  n_train <- nrow(joint_table)
  sample_fraction <- max(c(0.3, min(c(1, 1/(0.025*n_train)))))
  
  # Parse formula.
  formula <- stats::reformulate(termlabels=parameter_names,
                                response="time_taken")
  
  # Train random forest.
  rf_model <- ranger::ranger(formula,
                             data=joint_table,
                             num.trees=n_tree,
                             num.threads=1L,
                             sample.fraction=sample_fraction,
                             verbose=FALSE)
  
  return(rf_model)
}



..hyperparameter_gaussian_process_optimisation_score_learner <- function(score_table, parameter_table){
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- NULL
  
  # Merge score and parameter data tables on param id.
  joint_table <- merge(x=score_table,
                       y=parameter_table,
                       by="param_id",
                       all=FALSE)
  
  # Replace NA entries with the minimum optimisation score.
  joint_table[is.na(optimisation_score), optimisation_score:=-1.0]
  
  # Get parameter names.
  parameter_names <- setdiff(colnames(parameter_table),
                             c("param_id", "run_id", "optimisation_score", "time_taken"))
  
  # Get training data and response.
  x <- joint_table[, mget(parameter_names)]
  
  # Encode categorical variables.
  x_encoded <- encode_categorical_variables(data=x,
                                            object=NULL,
                                            encoding_method="dummy",
                                            drop_levels=FALSE)$encoded_data
  
  # Get optimisation score as response.
  y <- joint_table$optimisation_score
  
  return(list("X"=as.matrix(x_encoded),
              "Z"=y))
}



..hyperparameter_bart_optimisation_score_learner <- function(score_table, parameter_table){
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- NULL
  
  # Check if package is installed
  require_package(x="BART", purpose="to predict process time of hyperparameter sets")
  
  # Merge score and parameter data tables on param id.
  joint_table <- merge(x=score_table,
                       y=parameter_table,
                       by="param_id",
                       all=FALSE)
  
  # Replace NA entries with the minimum optimisation score.
  joint_table[is.na(optimisation_score), optimisation_score:=-1.0]
  
  # Get parameter names.
  parameter_names <- setdiff(colnames(parameter_table),
                             c("param_id", "run_id", "optimisation_score", "time_taken"))
  
  # Get training data and response.
  x <- joint_table[, mget(parameter_names)]
  
  # Drop invariant columns.
  invariant_columns <- sapply(x, is_singular_data)
  parameter_names <- parameter_names[!invariant_columns]
  x <- joint_table[, mget(parameter_names)]
  
  # Encode categorical variables.
  x_encoded <- encode_categorical_variables(data=x,
                                            object=NULL,
                                            encoding_method="dummy",
                                            drop_levels=FALSE)$encoded_data
  
  # Get optimisation score as response.
  y <- joint_table$optimisation_score
  
  # Create model. Note that prediction by BART is expensive for large ndpost.
  quiet(bart_model <- BART::wbart(x.train=data.frame(x_encoded), y.train=y, ntree=100, ndpost=50))
  
  return(bart_model)
}


.compute_utility_value <- function(parameter_set,
                                   score_model,
                                   hyperparameter_learner,
                                   acquisition_data,
                                   acquisition_function){
  # Predict score and compute .
  if(hyperparameter_learner == "random_forest"){
    # Check if package is installed
    require_package(x="ranger", purpose="to perform model-based hyperparameter optimisation")
    
    # Compute score per decision tree in the random forest.
    predicted_scores <- predict(score_model,
                                data=parameter_set,
                                predict.all=TRUE,
                                num.threads=1L,
                                verbose=FALSE)$predictions
    
    # Compute utility scores.
    utility_scores <- apply(predicted_scores,
                            MARGIN=1,
                            ..compute_utility_score,
                            acquisition_function=acquisition_function,
                            acquisition_data=acquisition_data,
                            n_parameters=ncol(parameter_set))
    
  } else if(hyperparameter_learner == "gaussian_process") {
    # Encode categorical variables.
    x_encoded <- encode_categorical_variables(data=parameter_set,
                                              object=NULL,
                                              encoding_method="dummy",
                                              drop_levels=FALSE)$encoded_data
    
    # Update the end-size parameter, if the number of instances is smaller than
    # the default of 51. Note that we already capture end_size of 7 or smaller
    # in the parent function.
    end_size <- ifelse(nrow(score_model$X) < 51, nrow(score_model$X) - 1, 50)
    
    # Infer scores for the hyperparameters.
    quiet(predicted_scores <- laGP::aGP(X=score_model$X,
                                        Z=score_model$Z,
                                        XX=as.matrix(x_encoded),
                                        end=end_size))
    
    # Compute utility_scores
    utility_scores <- mapply(..compute_utility_score,
                             mu=predicted_scores$mean,
                             sigma=sqrt(predicted_scores$var),
                             MoreArgs=list("acquisition_function"=acquisition_function,
                                           "acquisition_data"=acquisition_data,
                                           "n_parameters"=ncol(parameter_set)))
    
  } else if(hyperparameter_learner %in% c("bayesian_additive_regression_trees", "bart")){
    
    # Check if package is installed
    require_package(x="BART", purpose="to perform model-based hyperparameter optimisation")
    
    # Drop columns not in the model object.
    modelled_parameters <- colnames(score_model$varcount)
    
    # Encode categorical variables.
    x_encoded <- encode_categorical_variables(data=parameter_set,
                                              object=NULL,
                                              encoding_method="dummy",
                                              drop_levels=FALSE)$encoded_data
    
    # Get predicted values.
    quiet(predicted_scores <- predict(score_model, data.frame(x_encoded[, mget(modelled_parameters)])))
    
    # Compute utility scores.
    utility_scores <- apply(predicted_scores,
                            MARGIN=2,
                            ..compute_utility_score,
                            acquisition_function=acquisition_function,
                            acquisition_data=acquisition_data,
                            n_parameters=ncol(parameter_set))
    
  } else if(hyperparameter_learner %in% c("random", "random_search")){
    # Utility scores are not used for utility scores.
    utility_scores <- rep_len(NA_real_, nrow(parameter_set))
    
  } else {
    ..error_reached_unreachable_code(paste0(".compute_utility_value: hyperparameter_learner was not recognised: ", hyperparameter_learner))
  }
  
  return(utility_scores)
}



..compute_utility_score <- function(x=NULL, mu=NULL, sigma=NULL, acquisition_function, acquisition_data, n_parameters){
  
  if(is.null(mu)) mu <- mean(x)
  if(is.null(sigma)) sigma <- stats::sd(x)
  
  if(!is.null(x)) if(all(!is.finite(x))) return(0.0)
  if(!is.finite(mu)) return(0.0)
  if(!is.finite(sigma)) return(0.0)
  
  if(acquisition_function == "improvement_probability"){
    # The definition of probability of improvement is found in Shahriari, B.,
    # Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out
    # of the Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175
    # (2016)
    
    # Get the incumbent score.
    tau <- acquisition_data$tau
    
    # Compute the probability of improvement.
    alpha <- stats::pnorm((mu - tau) / sigma)
    
  } else if(acquisition_function == "improvement_empirical_probability"){
    
    # Get the incumbent score.
    tau <- acquisition_data$tau
    
    if(!is.null(x)){
      # Compute the empirical probability of improvement.
      alpha <- sum(x > tau) / length(x)
      
    } else {
      # Compute the stochastic probability of improvement.
      alpha <- stats::pnorm((mu - tau) / sigma)
    }
    
  } else if(acquisition_function == "expected_improvement"){
    # The definition of expected improvement is found in Shahriari, B., Swersky,
    # K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out of the
    # Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175 (2016).
    
    # Get the incumbent score.
    tau <- acquisition_data$tau
    
    # Compute a z-score.
    if(sigma > 0){
      z <- (mu - tau) / sigma
    } else {
      return(0.0)
    }
    
    # Compute the expected improvement.
    alpha <- (mu - tau) * stats::pnorm(z) + sigma * stats::dnorm(z)
    
  } else if(acquisition_function == "upper_confidence_bound"){
    # The definition of upper confidence bound is adapted from Srinivas, N.,
    # Krause, A., Kakade, S. M. & Seeger, M. W. Information-Theoretic Regret
    # Bounds for Gaussian Process Optimization in the Bandit Setting. IEEE
    # Trans. Inf. Theory 58, 3250–3265 (2012).
    
    # Compute the beta parameter.
    beta <- 2/5 * log(5/3 * n_parameters * (acquisition_data$t + 1)^2 * pi^2)
    
    # Compute alpha
    alpha <- mu + beta * sigma
    
  } else if(acquisition_function == "bayes_upper_confidence_bound"){
    # The definition of the Bayesian upper confidence bound is adapted from
    # Kaufmann, E., Cappé, O. & Garivier, A. On Bayesian upper confidence bounds
    # for bandit problems. in Artificial intelligence and statistics 592–600
    # (2012).
    
    # Compute the quantile we are interested in.
    q <- 1.0 - 1.0 / (acquisition_data$t + 1)
    
    if(!is.null(x)){
      # Compute empiric alpha.
      alpha <- stats::quantile(x=x,
                               probs=q,
                               names=FALSE)
    } else {
      # Stochastic alpha.
      alpha <- stats::qnorm(q, mean=mu, sd=sigma)
    }
    
  } else {
    ..error_reached_unreachable_code(paste0("hpo.acquisition_function: unknown acquisition function: ", acquisition_function))
  }
  
  return(alpha)
}



.compute_expected_train_time <- function(parameter_set,
                                         time_model){
  
  # If no time model is present, return NA.
  if(is.null(time_model)) return(NA_real_)
  
  if(!data.table::is.data.table(parameter_set)) parameter_set <- data.table::as.data.table(parameter_set)
  
  # Check if package is installed
  require_package(x="ranger", purpose="to predict process time of hyperparameter sets")
  
  predictions <- predict(time_model,
                         data=parameter_set,
                         type="response",
                         num.threads=1L,
                         verbose=FALSE)
  
  return(predictions$predictions)
}



.get_available_acquisition_functions <- function(){
  return(c("improvement_probability",
           "improvement_empirical_probability",
           "expected_improvement",
           "upper_confidence_bound",
           "bayes_upper_confidence_bound"))
}



.get_available_hyperparameter_learners <- function(){
  return(c("random_forest",
           "gaussian_process",
           "bayesian_additive_regression_trees", 
           "bart",
           "random",
           "random_search"))
}



.get_available_hyperparameter_exploration_methods <- function(){
  return(c("successive_halving",
           "stochastic_reject",
           "none"))
}
