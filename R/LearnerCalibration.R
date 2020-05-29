learner.calibration.survival <- function(object, data, time){
  # Generate baseline calibration data for survival models from an input table
  # (dt) containing survival probabilities at time_max for each sample and
  # corresponding time and event status.

  # Suppress NOTES due to non-standard evaluation in data.table
  exp_prob <- NULL
  
  # Set outcome type
  outcome_type <- object@outcome_type
  
  # Create a probability table. Note that the ensemble_method argument is
  # ignored unless object is a familiarEnsemble.
  probability_table <- .predict(object=object,
                                data=data,
                                time=time,
                                type="survival_probability",
                                allow_recalibration=TRUE,
                                ensemble_method="median")
  
  # Check for empty predictions.
  if(is_empty(probability_table)){
    return(create_empty_calibration_table(outcome_type=outcome_type))
  }
  
  # Check for failed predictions.
  if(!any_predictions_valid(probability_table, outcome_type=outcome_type)){
    return(create_empty_calibration_table(outcome_type=outcome_type))
  } 
  
  # Rename the survival column to standard name.
  data.table::setnames(probability_table,
                       old=c("outcome_time", "outcome_event", "predicted_outcome"),
                       new=c("time", "event", "exp_prob"))
  
  # Sort by survival probability.
  probability_table <- probability_table[order(exp_prob)]
  
  # Repeatedly split into groups. The number of groups is determined using
  # sturges rule.
  repeated_groups <- lapply(seq_len(20), function(ii, x, y, sample_id){
    return(create_randomised_groups(x=x, y=y, sample_id=sample_id, n_min_y_in_group=4))
    
  }, x=probability_table$exp_prob, y=probability_table$event, sample_id=probability_table$subject_id)
  
  # Iterate over groups and add details by comparing the kaplan-meier survival
  # curve within each group at time_max with the mean survival probability in
  # the group.
  calibration_table <- lapply(seq_along(repeated_groups),
                              function(ii, groups, probability_table, time_max, outcome_type){
                                return(learner.calibration.survival.prepare_data(probability_table=probability_table,
                                                                                 groups=repeated_groups[[ii]],
                                                                                 outcome_type=outcome_type,
                                                                                 time_max=time_max,
                                                                                 ii=ii))
                                
                              }, probability_table=probability_table, outcome_type=outcome_type, time_max=time)
  
  # Concatenate to table.
  calibration_table <- data.table::rbindlist(calibration_table)
  
  # Prevent processing of empty tables.
  if(is_empty(calibration_table)) return(create_empty_calibration_table(outcome_type=outcome_type))
  
  # Set the evaluation time.
  calibration_table[, "evaluation_time":=time]
  
  # Set column order.
  data.table::setcolorder(calibration_table, c("evaluation_time", "expected", "observed", "km_var", "n_g", "rep_id"))
  
  return(calibration_table)
}



learner.calibration.survival.prepare_data <- function(groups, probability_table, outcome_type, time_max, ii){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  subject_id <- NULL
  
  # Placeholder variables
  obs_prob <- exp_prob <- n_g <- km_var <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get oberved and expected probabilities over the groups
  for(jj in seq_along(groups)){
    
    # Find data for the current group
    group_data <- probability_table[subject_id %in% groups[[jj]]]
    
    if(nrow(group_data) >= 2){
    
      # Fit a Kaplan-Meier curve for the current group
      km_fit <- survival::survfit(Surv(time, event)~1, data=group_data)
      
      # Get observed probability
      obs_prob[jj] <- stats::approx(x=km_fit$time,
                                    y=km_fit$surv,
                                    xout=time_max,
                                    method="linear",
                                    rule=2)$y
      
      # Get expected probability
      exp_prob[jj] <- mean(group_data$exp_prob)
      
      # Get group size
      n_g[jj] <- length(groups[[jj]])
      
      # Get greenwood variance estimate.
      km_var[jj] <- stats::approx(x=km_fit$time,
                                  y=km_fit$std.err,
                                  xout=time_max,
                                  method="linear",
                                  rule=2)$y^2
    } else {
      # Set NA values.
      obs_prob[jj] <- exp_prob[jj] <- km_var[jj] <- NA_real_
      n_g[jj] <- NA_integer_
    }
  }
  
  # Create table.
  calibration_table <- data.table::data.table("expected"=exp_prob,
                                              "observed"=obs_prob,
                                              "n_g"=n_g,
                                              "km_var"=km_var,
                                              "rep_id"=ii)
  
  return(calibration_table)
}



learner.calibration.categorical <- function(object, data){
  # For assessing the calibration of categorical outcomes, we require expected
  # and observed probabilities for 1 (binomial) or all classes (multinomial).
  # Expected probabilities are easy, as the model predicts them. Observed
  # probabilities are however based on class proportion within a group. This
  # means that we need to define groups, ordered by predicted probability. The
  # same groups need to be defined for the Hosmer-Lemeshow test, so we might as
  # well obtain all the data here.
  #
  # Groups are defined for each class level because the samples should be
  # ordered according to predicted class probability to create the groups.
  
  # Set outcome type
  outcome_type <- object@outcome_type
  class_levels <- get_outcome_class_levels(x=object)

  browser()
  # Get prediction table. Note that the ensemble method is ignored unless object
  # is a familiarEnsemble.
  probability_table <- .predict(object=object,
                                data=data,
                                allow_recalibration=TRUE,
                                ensemble_method="median")

  if(is_empty(probability_table)){
    return(create_empty_calibration_table(outcome_type=outcome_type))
  }

  # Calibrate binomial and multinomial scores
  if(outcome_type == "binomial"){
    # Identify the positive outcome class
    pos_class <- class_levels[2]

    # Calculate the calibration curve
    calibration_table <- learner.calibration.categorical.assess_class(pos_class=pos_class,
                                                                      probability_table=probability_table,
                                                                      outcome_type=outcome_type)
    
    # Set pos_class as factor.
    calibration_table$pos_class <- factor(calibration_table$pos_class, levels=pos_class)

  } else if(outcome_type == "multinomial") {
    # Iterate over class levels and get the calibration curves
    calibration_table <- lapply(class_levels, learner.calibration.categorical.assess_class, probability_table=probability_table, outcome_type=outcome_type)
    
    # Concatenate to list
    calibration_table <- data.table::rbindlist(calibration_table)
    
    # Set pos_class as factor.
    calibration_table$pos_class <- factor(calibration_table$pos_class, levels=class_levels)
    
  } else {
    ..error_reached_unreachable_code("learner.calibration.categorical: calibration is not available for this outcome type.")
  }
  
  return(calibration_table)
}



learner.calibration.categorical.assess_class <- function(pos_class, probability_table, outcome_type){
  # This function performs the analysis for the provided class.

  # Suppress NOTES due to non-standard evaluation in data.table
  obs_class <- exp_prob <- NULL

  # Determine outcome column
  pos_col_name <- get_class_probability_name(x=pos_class)

  # Identify the real outcome columns
  outc_col <- get_outcome_columns(x=outcome_type)

  # Create a local copy of the data table
  probability_table <- data.table::copy(probability_table[,c("subject_id", outc_col, pos_col_name), with=FALSE])

  # Only select instances with known outcomes and probabilities
  probability_table <- probability_table[!(is.na(get(outc_col)) | is.na(get(pos_col_name))), ]

  # Check that there is actually any data to work with
  if(is_empty(probability_table)){
    return(create_empty_calibration_table(outcome_type=outcome_type))
  }

  # Rename columns to standard names
  data.table::setnames(probability_table, c(outc_col, pos_col_name), c("obs_class", "exp_prob"))

  # Mask class so that positive outcomes are TRUE and the rest FALSE
  probability_table[, "obs_class":=obs_class==pos_class]

  # Sort by probability
  probability_table <- probability_table[order(exp_prob)]

  # Repeatedly split into groups. The number of groups is determined using sturges rule
  repeated_groups <- lapply(seq_len(20), function(ii, x, sample_id) (create_randomised_groups(x=x, sample_id=sample_id)),
                            x=probability_table$exp_prob, sample_id=probability_table$subject_id)

  # Iterate over groups
  calibration_table <- lapply(seq_len(length(repeated_groups)),
                              function(ii, groups, probability_table, outcome_type){
                                return(learner.calibration.categorical.prepare_data(probability_table=probability_table,
                                                                                    groups=repeated_groups[[ii]],
                                                                                    outcome_type=outcome_type,
                                                                                    ii=ii))
                                
                              }, probability_table=probability_table, outcome_type=outcome_type)
  
  # Combine to a single list.
  calibration_table <- data.table::rbindlist(calibration_table)
  
  # Check that any calibration data was generated.
  if(is_empty(calibration_table)) return(create_empty_calibration_table(outcome_type=outcome_type))
  
  # Add in the positive class.
  calibration_table[, "pos_class":=pos_class]
  
  # Set column order
  data.table::setcolorder(calibration_table, c("pos_class", "expected", "observed", "n_g", "n_pos", "n_neg", "rep_id"))
  
  return(calibration_table)
}



learner.calibration.categorical.prepare_data <- function(groups, probability_table, outcome_type, ii){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  subject_id <- NULL
  
  obs_prob <- exp_prob <- n_g <- n_pos <- n_neg <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get oberved and expected probabilities over the groups
  for(jj in seq_len(length(groups))){
    # Mean expected probability in a group.
    exp_prob[jj] <- mean(probability_table[subject_id %in% groups[[jj]]]$exp_prob)
    
    # Observed proportion of positive class in a group.
    obs_prob[jj] <- mean(probability_table[subject_id %in% groups[[jj]]]$obs_class)
    
    # Number of samples in the group
    n_g[jj] <- length(groups[[jj]])
    
    # Number of samples with the positive class in each group.
    n_pos[jj] <- sum(probability_table[subject_id %in% groups[[jj]]]$obs_class)
    
    # Number of samples with the negative class in each group.
    n_neg[jj] <- sum(!probability_table[subject_id %in% groups[[jj]]]$obs_class)
  }
  
  # Create table
  calibration_table <- data.table::data.table("expected"=exp_prob,
                                              "observed"=obs_prob,
                                              "n_g"=n_g,
                                              "n_pos"=n_pos,
                                              "n_neg"=n_neg,
                                              "rep_id"=ii)
  
  return(calibration_table)
}



learner.calibration.regression <- function(object, data_obj){
  # Calibration for regression problems is pretty straightforward. However, for
  # goodness-of-fit tests, we need to constrain expected and observed value
  # ranges to [0, 1]. To do so, we use the range of outcome values from the
  # development data, as stored in the calibration_info slot of the input
  # object.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- predicted_outcome <- NULL
  
  # Set outcome type
  outcome_type <- object@outcome_type

  # Get prediction table. Note that the ensemble_method argument may be ignored
  # unless object is a familiarEnsemble.
  prediction_table <- .predict(object=object,
                               data=data_obj,
                               allow_recalibration=TRUE,
                               ensemble_method="median")

  # Remove non-finite predicted values.
  prediction_table <- prediction_table[is.finite(outcome) & is.finite(predicted_outcome), ]
  
  # Check for empty required data.
  if(is_empty(prediction_table)) return(create_empty_calibration_table(outcome_type=outcome_type))
  if(is.null(object@calibration_info)) return(create_empty_calibration_table(outcome_type=outcome_type))
  
  # Normalise predicted and observed outcomes. The same range is used for
  # predicted and observed outcomes.
  norm_range <- c(object@calibration_info$min_value,
                  object@calibration_info$max_value)
  
  # Get the shift and scale parameters.
  norm_shift <- norm_range[1]
  norm_scale <- diff(norm_range)
  
  # Set scale parameters equal to 0.0 to 1.0 to avoid division by 0.
  if(norm_scale == 0.0) norm_scale <- 1.0
  
  # Apply normalisation.
  prediction_table[, ":="("expected"=(predicted_outcome - norm_shift) / norm_scale,
                          "observed"=(outcome - norm_shift) / norm_scale)]
  
  # Repeatedly split into groups. The number of groups is determined using sturges rule
  repeated_groups <- lapply(seq_len(20), function(ii, x, sample_id) (create_randomised_groups(x=x, sample_id=sample_id)),
                            x=prediction_table$expected, sample_id=prediction_table$subject_id)
  
  # Iterate over groups
  calibration_table <- lapply(seq_len(length(repeated_groups)),
                              function(ii, groups, probability_table, outcome_type){
                                return(learner.calibration.regression.prepare_data(probability_table=probability_table,
                                                                                   groups=repeated_groups[[ii]],
                                                                                   outcome_type=outcome_type,
                                                                                   ii=ii))
                                
                                } , probability_table=prediction_table, outcome_type=outcome_type)
  
  # Concatenate to a single table
  calibration_table <- data.table::rbindlist(calibration_table)
  
  # Check if the resulting table contains data.
  if(is_empty(calibration_table)) return(create_empty_calibration_table(outcome_type=outcome_type))
  
  # Make columns match the expected order
  data.table::setcolorder(calibration_table, c("expected", "observed", "n_g", "rep_id"))

  return(calibration_table)
}



learner.calibration.regression.prepare_data <- function(groups, probability_table, outcome_type, ii){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  subject_id <- NULL
  
  obs_prob <- exp_prob <- n_g <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get oberved and expected probabilities over the groups
  for(jj in seq_len(length(groups))){
    # Mean expected probability in a group.
    exp_prob[jj] <- mean(probability_table[subject_id %in% groups[[jj]]]$expected)
    
    # Observed proportion of positive class in a group.
    obs_prob[jj] <- mean(probability_table[subject_id %in% groups[[jj]]]$observed)
    
    # Number of samples in the group
    n_g[jj] <- length(groups[[jj]])
  }
  
  # Create table
  calibration_table <- data.table::data.table("expected"=exp_prob,
                                              "observed"=obs_prob,
                                              "n_g"=n_g,
                                              "rep_id"=ii)
  
  return(calibration_table)
}


learner.calibration.regression.outcome_range <- function(data_obj){
  
  # Check for empty dataset.
  if(is_empty(data_obj@data)){
    return(NULL)
  }
  
  # Acquire range of observed outcome values
  outcome_range <- range(data_obj@data$outcome, na.rm=TRUE)
  
  if(!all(is.finite(outcome_range))){
    return(NULL)
    
  } else {
    return(data.table::data.table("min_value"=outcome_range[1],
                                  "max_value"=outcome_range[2]))
  }
}
