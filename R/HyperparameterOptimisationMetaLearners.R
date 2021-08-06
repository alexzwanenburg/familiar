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
  param_id <- expected_train_time <- expected_improvement <- NULL
  
  # Set max number of local steps and max number of failure to improve expected
  # improvement.
  n_local_steps <- 10
  n_max_no_improv <- 5
  
  # Compute the optimisation score. This creates a table with optimisation
  # scores per bootstrap and parameter identifier.
  optimisation_score_table <- .compute_hyperparameter_optimisation_score(score_table=score_table,
                                                                         optimisation_function=optimisation_function)
  
  # Create a model to predict the optimisation score for a given parameter set.
  score_optimisation_model <- .create_hyperparameter_score_optimisation_model(hyperparameter_learner=hyperparameter_learner,
                                                                              score_table=optimisation_score_table,
                                                                              parameter_table=parameter_table)
  
  # Find information regarding the dataset that has the highest optimisation
  # score.
  incumbent_set_data <- ..get_best_hyperparameter_set(optimisation_score_table=optimisation_score_table,
                                                      acquisition_function=acquisition_function,
                                                      n=1)
  
  # Set a flag for local search, vs. random search.
  # local_search <- smbo_iter %% 2 == 0
  
  # Get acquisition data.
  acquisition_data <- ..get_acquisition_function_parameters(optimisation_score_table=optimisation_score_table,
                                                            acquisition_function=acquisition_function,
                                                            n_max_bootstraps=n_max_bootstraps)
  
  ##### Generate random sets----------------------------------------------------
  # In general, draw about 10 times as much random sets then required.
  n_random_sets <- ceiling(n_challengers * random_admixture_fraction)
  browser()
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
  
  if(!is_empty(random_sets) & !hyperparameter_learner %in% c("random", "random_search")){
    random_sets[, "expected_improvement":=.compute_utility_value(parameter_set=.SD,
                                                                 score_model=score_optimisation_model,
                                                                 hyperparameter_learner=hyperparameter_learner,
                                                                 acquisition_data=acquisition_data,
                                                                 acquisition_function=acquisition_function)]
  }
  
  ##### Identify local sets ----------------------------------------------------
  n_local_sets <- ceiling(n_challengers * (1.0 - random_admixture_fraction))
  
  # Find local sets
  local_sets <- list()
  
  if(n_local_sets > 0){
    
    # Make local copy of parameter table.
    temp_parameter_table <- data.table::copy(parameter_table)
    
    # Compute expected improvement for the sets in the parameter table
    temp_parameter_table[, "expected_improvement":=.compute_utility_value(parameter_set=.SD,
                                                                          score_model=score_optimisation_model,
                                                                          hyperparameter_learner=hyperparameter_learner,
                                                                          acquisition_data=acquisition_data,
                                                                          acquisition_function=acquisition_function),
                         by=param_id]
    
    # Add the expected training time.
    temp_parameter_table[, "expected_train_time":=.compute_expected_train_time(parameter_set=.SD,
                                                                               time_model=time_optimisation_model),
                         by=param_id]
    
    # Select only parameters with allowable time.
    if(measure_time) temp_parameter_table <- temp_parameter_table[expected_train_time < acquisition_data$max_time]
    
    # Select up n_local_sets best parameter sets as starting point
    best_sets <- head(temp_parameter_table[order(-expected_improvement)], n=n_local_sets)
    
    # Perform a local search
    for(best_param_id in best_sets$param_id){
      
      # Initialise iterator variables
      iter_local_step <- iter_no_improv  <- 0
      
      # Select initial configuration for local search
      selected_set <- temp_parameter_table[param_id==best_param_id, ]
      
      # Determine the expected improvement for the currently selected set
      selected_set_expected_improvement <- selected_set$expected_improvement[1]
      
      while(iter_local_step < n_local_steps & iter_no_improv < n_max_no_improv & length(local_sets) < n_local_sets){
        
        # Randomise configuration
        local_random_set <- ..randomise_hyperparameter_set(parameter_table=selected_set,
                                                           parameter_list=parameter_list,
                                                           local=TRUE)
        
        # Compute the expected improvement for the local set
        local_random_set[, "expected_improvement":=.compute_utility_value(parameter_set=local_random_set,
                                                                          score_model=score_optimisation_model,
                                                                          hyperparameter_learner=hyperparameter_learner,
                                                                          acquisition_data=acquisition_data,
                                                                          acquisition_function=acquisition_function)]
        
        local_random_set_expected_improvement <- local_random_set$expected_improvement
        
        # Accept new configuration if there is an expected improvement
        if(local_random_set_expected_improvement > selected_set_expected_improvement){
          
          # Add time estimate.
          local_random_set[, "expected_train_time":=.compute_expected_train_time(parameter_set=.SD,
                                                                                 time_model=time_optimisation_model)]
          
          # Add configuration to list
          local_sets <- c(local_sets,
                          list(local_random_set))
          
          # Continue search from current set
          selected_set <- local_random_set
          selected_set_expected_improvement <- local_random_set_expected_improvement
          
          # Update iterator
          iter_local_step <- iter_local_step + 1
          
          # Reset iterator for no improvements in expected improvement.
          iter_no_improv  <- 0
          
        } else {
          # Increment the failure to improve iterator
          iter_no_improv  <- iter_no_improv + 1
        }
      }
    }
  }
  
  # Combine local sets
  local_sets <- unique(data.table::rbindlist(local_sets))
  
  browser()
  ##### Select challengers -----------------------------------------------------
  
  # Preferentially select in the following order:
  # 1. Local sets that were not observed before, with highest utility first.
  # 2. Local sets that were observed before, with highest utility first..
  # 3. Random sets that were not observed before, with highest utility first..
  # 4. Random sets that were observed before, with highest utility first..
  
  
  if(local_search){
    
    # Combine original, local and random sets.
    new_sets <- data.table::rbindlist(c(list(temp_parameter_table[, -c("param_id")]),
                                        c(local_sets, random_sets)),
                                      use.names=TRUE)
    
    # Select only unique parameter sets
    new_sets <- unique(new_sets,
                       by=names(parameter_list))
    
    # Merge in original parameter-ids
    new_sets <- merge(x=new_sets,
                      y=parameter_table,
                      by=names(parameter_list),
                      all.x=TRUE,
                      all.y=FALSE)
    
    # Keep all sets except the incumbent, as it cannot have a run-off against
    # itself.
    new_sets <- new_sets[param_id != incumbent_set_data$param_id | is.na(param_id)]
    
    # Select only parameters with allowable time.
    if(measure_time) new_sets <- new_sets[expected_train_time < acquisition_data$max_time]
    
    # Sort by expected improvement and select up to 20 best
    challenger_sets <- head(new_sets[(order(-expected_improvement))], n_challengers)
    
  } else {
    
    # Combine random sets into a data.table, but only select unique sets
    new_sets <- unique(data.table::rbindlist(random_sets),
                       by=names(parameter_list))
    
    # Merge in original parameter set ids
    new_sets <- merge(x=new_sets,
                      y=parameter_table,
                      by=names(parameter_list),
                      all.x=TRUE,
                      all.y=FALSE)
    
    # Keep all sets except the incumbent, as it cannot have a run-off against
    # itself.
    new_sets <- new_sets[param_id != incumbent_set_data$param_id | is.na(param_id)]
    
    # Select only parameters with allowable time.
    if(measure_time) new_sets <- new_sets[expected_train_time < acquisition_data$max_time]
    
    # Prefer challengers to be parameters that have not been seen.
    challenger_sets <- head(new_sets[is.na(param_id)], n_challengers)
    
    # Pad with challengers that contain parameter sets that were previously
    # visited.
    if(nrow(challenger_sets) < n_challengers){
      additional_challengers <- new_sets[!is.na(param_id)]
      
      n_additional_challengers <- n_challengers - nrow(challenger_sets)
      if(n_additional_challengers > nrow(additional_challengers)) n_additional_challengers <- nrow(additional_challengers)
      
      if(n_additional_challengers > 0){
        additional_challengers <- additional_challengers[fam_sample(x=seq_len(nrow(additional_challengers)),
                                                                    size=n_additional_challengers)]
        
      } else {
        additional_challengers <- NULL
      }
      
      # Add challenger sets.
      challenger_sets <- data.table::rbindlist(list(challenger_sets, additional_challengers), use.names=TRUE)
    }
  }
  
  # Remove expected improvement
  challenger_sets[, ":="("expected_improvement"=NULL,
                         "expected_train_time"=NULL)]
  
  # Provide a new parameter id for new configurations
  if(any(is.na(challenger_sets$param_id))){
    
    # Find the largest existing parameter id
    max_param_id <- max(parameter_table$param_id)
    
    # Generate parameter ids for new parameter sets. 
    challenger_sets[is.na(param_id), "param_id":=max_param_id + .I]
  }
  
  return(challenger_sets)
}



.create_hyperparameter_score_optimisation_model <-  function(hyperparameter_learner,
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
  
  # Train random forest. Note that ranger is imported through the NAMESPACE.
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
  
  # TODO: finish
  browser()
}



..hyperparameter_bart_optimisation_score_learner <- function(score_table, parameter_table){
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
    browser()
    
  } else if(hyperparameter_learner %in% c("bayesian_additive_regression_trees", "bart")){
    browser()
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



..compute_utility_score <- function(x, acquisition_function, acquisition_data, n_parameters){
  
  if(acquisition_function == "improvement_probability"){
    # The definition of probability of improvement is found in Shahriari, B.,
    # Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out
    # of the Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175
    # (2016)
    
    # Calculate predicted mean objective score and its standard deviation over
    # the trees.
    mu <- mean(x)
    sigma <- stats::sd(x)
    
    # Get the incumbent score.
    tau <- acquisition_data$tau
    
    # Compute the probability of improvement.
    alpha <- stats::pnorm((mu - tau) / sigma)
    
  } else if(acquisition_function == "improvement_empirical_probability"){
    
    # Compute the empirical probability of improvement.
    alpha <- sum(x > acquisition_data$tau) / length(x)
    
  } else if(acquisition_function == "expected_improvement"){
    # The definition of expected improvement is found in Shahriari, B., Swersky,
    # K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out of the
    # Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175 (2016).
    
    # Calculate predicted mean objective score and its standard deviation over
    # the trees.
    mu <- mean(x)
    sigma <- stats::sd(x)
    
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
    
    # Calculate predicted mean objective score and its standard deviation over
    # the trees.
    mu <- mean(x)
    sigma <- stats::sd(x)
    
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
    
    # Compute alpha
    alpha <- stats::quantile(x=x,
                             probs=q,
                             names=FALSE)
    
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
