.create_hyperparameter_challenger_sets <- function(score_table,
                                                   parameter_list,
                                                   parameter_table,
                                                   time_optimisation_model,
                                                   score_optimisation_model,
                                                   acquisition_function,
                                                   n_max_bootstraps,
                                                   n_challengers=20L,
                                                   smbo_iter,
                                                   measure_time,
                                                   random_admixture_fraction=0.20){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- expected_train_time <- utility <- summary_score <- weight <- NULL 
  is_observed <- .NATURAL <- i.param_id <- NULL
  
  # Set the maximum number of local steps based on the number of randomisable
  # hyperparameters.
  n_max_local_steps <- 2 * sum(sapply(parameter_list, function(x) (x$randomise)))
  
  # Train hyperparameter optimisation model.
  if(!model_is_trained(score_optimisation_model)){
    score_optimisation_model <- .train(object=score_optimisation_model,
                                       data=score_table,
                                       parameter_data=parameter_table)
  }
  
  # Find information regarding the dataset that has the highest
  # optimisation score.
  incumbent_set <- get_best_hyperparameter_set(score_table=score_table,
                                               parameter_table=parameter_table,
                                               optimisation_model=score_optimisation_model,
                                               n_max_bootstraps=n_max_bootstraps,
                                               n=1L)
  
  ##### Generate random sets----------------------------------------------------
  
  # Increase random admixture for random search.
  if(is(score_optimisation_model, "familiarHyperparameterRandomSearch")) random_admixture_fraction <- 1.0
  
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
  if(!is_empty(random_sets) & !is(score_optimisation_model, "familiarHyperparameterRandomSearch")){
    random_sets[, "utility":=.compute_utility_value(parameter_set=.SD,
                                                    score_model=score_optimisation_model,
                                                    acquisition_data=incumbent_set,
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
  
  if(n_local_sets > 0 & !is(score_optimisation_model, "familiarHyperparameterRandomSearch")){
    
    # Create a table of summary scores to identify the best sets.
    summary_score_table <- .compute_hyperparameter_summary_score(score_table=score_table,
                                                                 parameter_table=parameter_table,
                                                                 optimisation_model=score_optimisation_model)
                                                                 
    # Find the most promising samples by comparing against the median.
    summary_score_table[, "weight":=summary_score - stats::median(summary_score)]
    
    # If all parameters have the same optimisation score (an edge case), assign
    # weight 1 to each, that is, all hyperparameter sets are as likely to be
    # sampled.
    if(all(summary_score_table$weight == 0.0)) summary_score_table[, "weight":=1.0]
    
    # Remove 0 and negative weights.
    summary_score_table <- summary_score_table[weight > 0.0]
    
    # Select the hyperparameter ids to sample, starting with the most promising
    # samples first.
    summary_score_table <- summary_score_table[, "n_select":=round(2 * n_local_sets * weight^2/sum(weight^2))][order(-summary_score )]
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
    local_sets[, "utility":=.compute_utility_value(parameter_set=.SD,
                                                   score_model=score_optimisation_model,
                                                   acquisition_data=incumbent_set,
                                                   acquisition_function=acquisition_function)]
    
    # Compute expected train time.
    local_sets[, "expected_train_time":=.compute_expected_train_time(parameter_set=.SD,
                                                                     time_model=time_optimisation_model)]
  }
  
  
  ##### Select challengers -----------------------------------------------------
  
  local_challenger_sets <- list()
  random_challenger_sets <- list()
  
  if(is(score_optimisation_model, "familiarHyperparameterRandomSearch")){
    # Preferentially select in the following order:
    # 1. Random sets that were not observed before.
    # 2. Random sets that were observed before.
    
    # Remove incumbent set.
    if(!is_empty(random_sets)) random_sets <- random_sets[!parameter_table[param_id == incumbent_set$param_id], on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(random_sets)) random_sets <- random_sets[expected_train_time < incumbent_set$max_time]
    
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
    if(!is_empty(local_sets)) local_sets <- local_sets[!parameter_table[param_id == incumbent_set$param_id], on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(local_sets)) local_sets <- local_sets[expected_train_time < incumbent_set$max_time]
    
    # Consider local sets.
    if(!is_empty(local_sets)){
      
      # Set observed status
      local_sets[, "is_observed":=FALSE]
      local_sets[parameter_table, "is_observed":=TRUE, on=.NATURAL]
      
      # Local sets that were not observed before, with highest utility first.
      if(any(!local_sets$is_observed)){
        # Select up to n_local_sets of locally sampled sets that were not
        # selected previously.
        selected_sets <- head(local_sets[is_observed==FALSE][order(-utility)], n=n_local_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        local_challenger_sets <- list(selected_sets)
        n_challengers <- n_challengers - nrow(selected_sets)
        n_local_sets <- min(c(n_local_sets - nrow(selected_sets), n_challengers))
      }
      
      # Local sets that were observed before, with highest utility first.
      if(any(local_sets$is_observed) & n_local_sets > 0){
        # Select up to n_local_sets of locally samples sets that were observed
        # selected previously.
        selected_sets <- head(local_sets[is_observed==TRUE][order(-utility)], n=n_local_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        local_challenger_sets <- c(local_challenger_sets, list(selected_sets))
        n_challengers <- n_challengers - nrow(selected_sets)
      }
    }
    
    if(!is_empty(local_challenger_sets)){
      # Concatenate challenger sets.
      local_challenger_sets <- data.table::rbindlist(local_challenger_sets, use.names=TRUE)
      
      # Remove expected improvement and time from challenger sets.
      local_challenger_sets[, ":="("utility"=NULL,
                                   "expected_train_time"=NULL,
                                   "is_observed"=NULL)]
    }
    
    # Remove incumbent and already selected challenger sets.
    if(!is_empty(random_sets)) random_sets <- random_sets[!parameter_table[param_id == incumbent_set$param_id], on=.NATURAL]
    if(!is_empty(random_sets) & !is_empty(local_challenger_sets) > 0) random_sets <- random_sets[!local_challenger_sets, on=.NATURAL]
    
    # Select only parameter sets that can be evaluated within the allowed time.
    if(measure_time & !is_empty(random_sets)) random_sets <- random_sets[expected_train_time < incumbent_set$max_time]
    
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
        selected_sets <- head(random_sets[is_observed==FALSE][order(-utility)], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- list(selected_sets)
        n_challengers <- n_challengers - nrow(selected_sets)
        n_random_sets <- min(c(n_random_sets - nrow(selected_sets), n_challengers))
      }
      
      # Random sets that were observed before, with highest utility first.
      if(any(random_sets$is_observed) & n_random_sets > 0){
        # Select up to n_random_sets of random samples sets that were observed
        # selected previously.
        selected_sets <- head(random_sets[is_observed==TRUE][order(-utility)], n=n_random_sets)
        
        # Add to challenger sets and reduce the number of challengers to select.
        random_challenger_sets <- c(random_challenger_sets, list(selected_sets))
        n_challengers <- n_challengers - nrow(selected_sets)
      }
    }
    
    if(!is_empty(random_challenger_sets)){
      # Concatenate challenger sets.
      random_challenger_sets <- data.table::rbindlist(random_challenger_sets, use.names=TRUE)
      
      # Remove expected improvement and time from challenger sets.
      random_challenger_sets[, ":="("utility"=NULL,
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



.create_hyperparameter_time_optimisation_model <- function(hyperparameter_learner="random_forest",
                                                           score_table,
                                                           parameter_table,
                                                           optimisation_function){
  
  # Check that score_table contains optimisation scores.
  if(!"optimisation_score" %in% colnames(score_table)){
    score_table <- .compute_hyperparameter_optimisation_score(score_table=score_table,
                                                              optimisation_function=optimisation_function)
  }
  
  # Train model to predict process time.
  model <- ..hyperparameter_random_forest_time_learner(score_table=score_table,
                                                       parameter_table=parameter_table)
  
  return(model)
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



.compute_utility_value <- function(parameter_set,
                                   score_model,
                                   acquisition_data,
                                   acquisition_function){
  
  # Check that the score_model is trained, and return NA if not.
  if(!model_is_trained(score_model)) return(rep_len(NA_real_, nrow(parameter_set)))
  
  # Dispatch to acquisition functions.
  acquisition_FUN <- switch(acquisition_function,
                            "improvement_probability" = acquisition_improvement_probability,
                            "improvement_empirical_probability" = acquisition_improvement_empirical_probability,
                            "expected_improvement" = acquisition_expected_improvement,
                            "upper_confidence_bound" = acquisition_upper_confidence_bound,
                            "bayes_upper_confidence_bound" = acquisition_bayes_upper_confidence_bound)
  
  
  utility_scores <- acquisition_FUN(object=score_model,
                                    parameter_data=parameter_set,
                                    acquisition_data=acquisition_data)
  
  return(utility_scores)
}



acquisition_improvement_probability <- function(object,
                                                parameter_data,
                                                acquisition_data){
  # The definition of probability of improvement is found in Shahriari, B.,
  # Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out
  # of the Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175
  # (2016)
  browser()
  # Get the score estimate from the incumbent hyperparameter set.
  tau <- acquisition_data$score_estimate
  
  # Predict the mean and standard deviation for the parameter sets in parameter
  # data.
  prediction_table <- .predict(object=object,
                               data=parameter_data,
                               type="sd")
  
  # Compute the probability of improvement.
  alpha <- stats::pnorm((prediction_table$mu - tau) / prediction_table$sigma)
  
  return(alpha)
}



acquisition_improvement_empirical_probability <- function(object,
                                                          parameter_data,
                                                          acquisition_data){
  # The definition of probability of improvement is found in Shahriari, B.,
  # Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out
  # of the Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175
  # (2016)
  
  # Get the incumbent score.
  tau <- acquisition_data$score_estimate
  
  if("raw" %in% get_prediction_type(object)){
    # Compute the empirical probability of improvement.
    
    # Define helper function.
    .acquisition_improvement_empirical_probability <- function(x, tau){
      return(sum(x > tau) / length(x))
    }
    
    # Predict raw data for the hyperparameter sets.
    prediction_table <- .predict(object=object,
                                 data=parameter_data,
                                 type="raw")
    
    # Drop parameter id. This should leave only raw data.
    prediction_table[, "param_id":=NULL]
    
    # Compute utility.
    alpha <- apply(prediction_table,
                   MARGIN=1,
                   .acquisition_improvement_empirical_probability,
                   tau=tau)
    
  } else {
    # Compute the stochastic probability of improvement.
    
    # Predict the mean and standard deviation for the parameter sets in parameter
    # data.
    prediction_table <- .predict(object=object,
                                 data=parameter_data,
                                 type="sd")
    
    # Stochastic values
    alpha <- stats::pnorm((prediction_table$mu - tau) / prediction_table$sigma)
  }
  
  return(alpha)
}



acquisition_expected_improvement <- function(object,
                                             parameter_data,
                                             acquisition_data){
  # The definition of expected improvement is found in Shahriari, B., Swersky,
  # K., Wang, Z., Adams, R. P. & de Freitas, N. Taking the Human Out of the
  # Loop: A Review of Bayesian Optimization. Proc. IEEE 104, 148–175 (2016).
  browser()
  # Get the incumbent score.
  tau <- acquisition_data$score_estimate
  
  # Predict the mean and standard deviation for the parameter sets in parameter
  # data.
  prediction_table <- .predict(object=object,
                               data=parameter_data,
                               type="sd")
  
  # Compute a z-score.
  z <- (prediction_table$mu - tau) / prediction_table$sigma

  # Compute the expected improvement.
  alpha <- (prediction_table$mu - tau) * stats::pnorm(z) + prediction_table$sigma * stats::dnorm(z)
  
  return(alpha)
}



acquisition_upper_confidence_bound <- function(object,
                                               parameter_data,
                                               acquisition_data){
  # The definition of upper confidence bound is adapted from Srinivas, N.,
  # Krause, A., Kakade, S. M. & Seeger, M. W. Information-Theoretic Regret
  # Bounds for Gaussian Process Optimization in the Bandit Setting. IEEE
  # Trans. Inf. Theory 58, 3250–3265 (2012).
  browser()
  # Find the number hyperparameters.
  n_parameters <- length(object@target_hyperparameters)
  
  # Predict the mean and standard deviation for the parameter sets in parameter
  # data.
  prediction_table <- .predict(object=object,
                               data=parameter_data,
                               type="sd")
  
  # Compute the beta parameter.
  beta <- 2/5 * log(5/3 * n_parameters * (acquisition_data$t + 1)^2 * pi^2)
  
  # Compute alpha
  alpha <- prediction_table$mu + beta * prediction_table$sigma
  
  return(alpha)
}
  


acquisition_bayes_upper_confidence_bound <- function(object,
                                                     parameter_data,
                                                     acquisition_data){
  # The definition of the Bayesian upper confidence bound is adapted from
  # Kaufmann, E., Cappé, O. & Garivier, A. On Bayesian upper confidence bounds
  # for bandit problems. in Artificial intelligence and statistics 592–600
  # (2012).
  
  # Compute the quantile we are interested in.
  q <- 1.0 - 1.0 / (acquisition_data$t + 1)
  browser()
  if("percentile" %in% get_prediction_type(object)){
    # Compute empiric alpha.
    alpha <- .predict(object=object,
                      data=data,
                      type="percentile",
                      percentile=q)$percentile
    
  } else {
    # Predict the mean and standard deviation for the parameter sets in parameter
    # data.
    prediction_table <- .predict(object=object,
                                 data=parameter_data,
                                 type="sd")
    
    # Stochastic alpha.
    alpha <- stats::qnorm(q, mean=prediction_table$mu, sd=prediction_table$sigma)
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
  return(c(.get_available_ranger_hyperparameter_learners(),
           .get_available_lagp_hyperparameter_learners(),
           .get_available_bart_hyperparameter_learners(),
           .get_available_random_search_hyperparameter_learners()))
}



.get_available_hyperparameter_exploration_methods <- function(){
  return(c("successive_halving",
           "stochastic_reject",
           "none"))
}
