#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementCalibrationData",
         contains="familiarDataElement")

setClass("familiarDataElementCalibrationLinearFit",
         contains="familiarDataElement")

setClass("familiarDataElementCalibrationGoodnessOfFit",
         contains="familiarDataElement")

setClass("familiarDataElementCalibrationDensity",
         contains="familiarDataElement")

#'@title Internal function to extract calibration data.
#'
#'@description Computes calibration data from a `familiarEnsemble` object.
#'  Calibration tests are performed based on expected (predicted) and observed
#'  outcomes. For all outcomes, calibration-at-the-large and calibration slopes
#'  are determined. Furthermore, for all but survival outcomes, a repeated,
#'  randomised grouping Hosmer-Lemeshow test is performed. For survival
#'  outcomes, the Nam-D'Agostino and Greenwood-Nam-D'Agostino tests are
#'  performed.
#'
#'@inheritParams extract_data
#'
#'@return A list with data.tables containing calibration test information for
#'  the ensemble model.
#'@md
#'@keywords internal
setGeneric("extract_calibration_data",
           function(object,
                    data,
                    cl=NULL,
                    ensemble_method=waiver(),
                    eval_times=waiver(),
                    detail_level=waiver(),
                    estimation_type=waiver(),
                    aggregate_results=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    is_pre_processed=FALSE,
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_calibration_data"))


#####extract_calibration_data#####
setMethod("extract_calibration_data", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   ensemble_method=waiver(),
                   eval_times=waiver(),
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Assessing model calibration."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)) eval_times <- object@settings$eval_times
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Load confidence alpha from object settings attribute if not
            # provided externally.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check confidence_level input argument
            .check_number_in_valid_range(x=confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_method",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            # Check the level detail.
            detail_level <- .parse_detail_level(x = detail_level,
                                                default = "hybrid",
                                                data_element = "calibration_data")
            
            # Check the estimation type.
            estimation_type <- .parse_estimation_type(x = estimation_type,
                                                      default = "bootstrap_confidence_interval",
                                                      data_element = "calibration_data",
                                                      detail_level = detail_level,
                                                      has_internal_bootstrap = TRUE)
            
            # Check whether results should be aggregated.
            aggregate_results <- .parse_aggregate_results(x = aggregate_results,
                                                          default = TRUE,
                                                          data_element = "calibration_data")
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Test if any model in the ensemble was successfully trained.
            if(!model_is_trained(object=object)) return(NULL)
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementCalibrationData",
                                      detail_level = detail_level,
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method)
            
            # Generate elements to send to dispatch.
            calibration_data <- extract_dispatcher(FUN=.extract_calibration_data,
                                                   has_internal_bootstrap=TRUE,
                                                   cl=cl,
                                                   object=object,
                                                   data=data,
                                                   proto_data_element=proto_data_element,
                                                   is_pre_processed=is_pre_processed,
                                                   ensemble_method=ensemble_method,
                                                   eval_times=eval_times,
                                                   aggregate_results=aggregate_results,
                                                   message_indent=message_indent + 1L,
                                                   verbose=verbose)
            
            return(calibration_data)
          })



.extract_calibration_data <- function(object,
                                      proto_data_element,
                                      eval_times=NULL,
                                      aggregate_results,
                                      cl,
                                      ...){
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  # Add evaluation time as a identifier to the data element.
  if(length(eval_times) > 0 & object@outcome_type == "survival"){
    data_elements <- add_data_element_identifier(x=proto_data_element, evaluation_time=eval_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  # Iterate over data elements.
  calibration_data <- lapply(data_elements,
                             ..extract_calibration_data,
                             object=object,
                             aggregate_results=aggregate_results,
                             cl=cl,
                             ...)
  
  return(calibration_data)
}



..extract_calibration_data <- function(data_element,
                                       object,
                                       data,
                                       cl=NULL,
                                       is_pre_processed,
                                       ensemble_method,
                                       metric,
                                       aggregate_results,
                                       progress_bar=FALSE,
                                       verbose=FALSE,
                                       message_indent,
                                       ...){
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Message the user concerning the time at which metrics are computed. This is
  # only relevant for survival analysis.
  if(length(data_element@identifiers$evaluation_time) > 0 & progress_bar){
    logger.message(paste0("Computing metric value at time ", data_element@identifiers$evaluation_time, "."),
                   indent=message_indent)
  }
  
  # Aggregate data.
  data <- aggregate_data(data)
  
  if(object@outcome_type %in% c("survival", "competing_risk")){
    # Predict class probabilities or regression values.
    prediction_data <- .predict(object=object,
                                data=data,
                                type="survival_probability",
                                time=data_element@identifiers$evaluation_time,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
    
  } else {
    # Predict class probabilities or regression values.
    prediction_data <- .predict(object=object,
                                data=data,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
  }
  
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Add positive class as an identifier.
  if(object@outcome_type %in% c("binomial")){
    data_element <- add_data_element_identifier(x=data_element,
                                                positive_class= get_outcome_class_levels(object)[2])
    
  } else if(object@outcome_type %in% c("multinomial")){
    data_element <- add_data_element_identifier(x=data_element,
                                                positive_class= get_outcome_class_levels(object))
  }
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x=data_element,
                                               ...)
  
  # Add distribution data.
  if(!is.list(data_element)) data_element <- list(data_element)
  density_data <- lapply(data_element,
                         .compute_calibration_data_density,
                         data=prediction_data,
                         object=object)
  
  # Iterate over elements.
  data_elements <- fam_mapply(cl=cl,
                              assign=NULL,
                              FUN=.compute_calibration_data,
                              data_element=bootstrap_data$data_element,
                              bootstrap=bootstrap_data$bootstrap,
                              bootstrap_seed = bootstrap_data$seed,
                              MoreArgs=list("object"=object,
                                            "data"=prediction_data),
                              progress_bar=progress_bar,
                              .chopchop=TRUE)
  
  # Flatten list of data elements.
  data_elements <- unlist(data_elements)
  if(!is.list(data_elements)) data_elements <- list(data_elements)
  
  # Add in density data elements.
  data_elements <- c(data_elements, density_data)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



.compute_calibration_data <- function(data_element,
                                      object,
                                      data,
                                      bootstrap,
                                      bootstrap_seed){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  observed <- NULL
  
  # Bootstrap the data.
  if(bootstrap) data <- get_bootstrap_sample(data=data,
                                             seed=bootstrap_seed)
  
  # Extract data
  if(object@outcome_type %in% c("survival")){
    # Calibration grouping data for survival outcomes.
    calibration_data <- .compute_calibration_data_survival(data=data,
                                                           time=data_element@identifiers$evaluation_time,
                                                           n_groups=ifelse(data_element@estimation_type == "point", 20L, 1L))
    
    # Calibration-in-the-large and calibration slope
    calibration_at_large <- .compute_calibration_linear_fit(calibration_data=calibration_data,
                                                            outcome_type=object@outcome_type)
    
    # Nam-D'Agostino tests
    calibration_gof_test <- .compute_calibration_nam_dagostino(calibration_data=calibration_data)
    
  } else if(object@outcome_type %in% c("binomial", "multinomial")){
    # Calibration grouping data for categorical outcomes.
    calibration_data <- .compute_calibration_data_categorical(data=data,
                                                              positive_class=data_element@identifiers$positive_class,
                                                              n_groups=ifelse(data_element@estimation_type == "point", 20L, 1L))
    
    # Calibration-in-the-large and calibration slope
    calibration_at_large <- .compute_calibration_linear_fit(calibration_data=calibration_data,
                                                            outcome_type=object@outcome_type)
    
    # Hosmer-Lemeshow tests
    calibration_gof_test <- .compute_calibration_hosmer_lemeshow(calibration_data=calibration_data)
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    # Calibration grouping data for numerical outcomes.
    calibration_data <- .compute_calibration_data_regression(object=object,
                                                             data=data,
                                                             n_groups=ifelse(data_element@estimation_type == "point", 20L, 1L))
    
    # Calibration-in-the-large and calibration slope
    calibration_at_large <- .compute_calibration_linear_fit(calibration_data=calibration_data,
                                                            outcome_type=object@outcome_type)
    
    # Hosmer-Lemeshow tests
    calibration_gof_test <- .compute_calibration_hosmer_lemeshow(calibration_data=calibration_data)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  if(!is_empty(calibration_data) & data_element@estimation_type != "point"){
    # Interpolate data to regular expected values, unless point data is used.
    calibration_data <- calibration_data[, ..compute_calibration_data_interpolated(.SD),
                                         by=c("rep_id"),
                                         .SDcols=c("expected", "observed")]
    
    # Keep only finite predictions,
    calibration_data <- calibration_data[is.finite(observed)]
  }
  
  # Create additional data elements to contain linear fit data and GoF test
  # data.
  linear_fit_data_element <- methods::new("familiarDataElementCalibrationLinearFit", data_element)  
  gof_data_element <- methods::new("familiarDataElementCalibrationGoodnessOfFit", data_element)
  
  # Set for calibration data.
  data_element@data <- calibration_data
  data_element@value_column <- "observed"
  data_element@grouping_column <- "expected"
  
  # Set linear fit data, i.e. calibration-in-the-large and calibration slope.
  linear_fit_data_element@data <- calibration_at_large
  linear_fit_data_element@value_column <- "value"
  linear_fit_data_element@grouping_column <- "type"
  
  # Set goodness of fit data.
  gof_data_element@data <- calibration_gof_test
  gof_data_element@value_column <- "p_value"
  gof_data_element@grouping_column <- "type"
  
  return(list(data_element,
              linear_fit_data_element,
              gof_data_element))
}



.compute_calibration_data_density <- function(data_element,
                                              data,
                                              object){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  predicted_outcome <- NULL
  
  # Copy data element.
  data_element <- methods::new("familiarDataElementCalibrationDensity",
                               data_element)
  
  # Copy data
  data <- data.table::copy(data)
  
  if(object@outcome_type %in% c("survival", "competing_risk")){
    # Rename the survival column to standard name.
    data.table::setnames(data,
                         old="survival_probability",
                         new="expected")
    
  } else if(object@outcome_type %in% c("binomial", "multinomial")) {
    # Rename class probability columns to standard names.
    data.table::setnames(data,
                         old=get_class_probability_name(x=data_element@identifiers$positive_class),
                         new="expected")
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    # Determine the outcome range
    outcome_range <- range(object@outcome_info@distribution$fivenum)
    
    # Get the shift and scale parameters.
    norm_shift <- outcome_range[1]
    norm_scale <- diff(outcome_range)
    
    # Set scale parameters equal to 0.0 to 1.0 to avoid division by 0.
    if(norm_scale == 0.0) norm_scale <- 1.0
    
    # Apply normalisation.
    data[, ":="("expected"=(predicted_outcome - norm_shift) / norm_scale)]
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  # Get expected values.
  expected <- data$expected
  
  # Set interpolation range for expected values. This allows for aggregating
  # observed values based on the expected value.
  expected_interpolated <- seq(from=0.000, to=1.000, by=0.005)
  
  # Determine the bin for expected.
  n <- length(expected_interpolated)
  
  # Discretise bins.
  group_bin <- floor((n - 1) * expected) + 1
  
  # Check if the group_bin still falls within the valid range.
  group_bin <- ifelse(group_bin > n, n, group_bin)
  group_bin <- ifelse(group_bin < 1, 1, group_bin)
  
  # Compute the frequency of each entry.
  data <- data.table::data.table("expected"=expected_interpolated[group_bin])
  data <- data[, list("frequency"=.N / length(group_bin)), by="expected"]
  
  # Set linear fit data, i.e. calibration-in-the-large and calibration slope.
  data_element@data <- data
  data_element@value_column <- "frequency"
  data_element@grouping_column <- "expected"
  
  return(data_element)
}



.compute_calibration_data_survival <- function(data, time, n_groups=1L){
  # Generate baseline calibration data for survival models from an input table
  # (data) containing survival probabilities for each sample and
  # corresponding time and event status.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  exp_prob <- NULL
  
  # Rename the survival column to standard name.
  data.table::setnames(data,
                       old=c("outcome_time", "outcome_event", "survival_probability"),
                       new=c("time", "event", "exp_prob"))
  
  # Sort by survival probability.
  data <- data[order(exp_prob)]
  
  # Repeatedly split into groups. The number of groups is determined using
  # sturges rule.
  repeated_groups <- lapply(seq_len(n_groups), function(ii, x, y, sample_identifiers){
    return(create_randomised_groups(x=x,
                                    y=y,
                                    sample_identifiers=sample_identifiers,
                                    n_min_y_in_group=4))
    
  },
  x=data$exp_prob,
  y=data$event,
  sample_identifiers=data[, mget(get_id_columns(id_depth="series"))])
  
  # Iterate over groups and add details by comparing the kaplan-meier survival
  # curve within each group at time with the mean survival probability in
  # the group.
  calibration_table <- lapply(seq_along(repeated_groups),
                              function(ii, groups, data, time){
                                return(..compute_calibration_data_survival(data=data,
                                                                           groups=groups[[ii]],
                                                                           time=time,
                                                                           ii=ii))
                                
                              },
                              groups=repeated_groups,
                              data=data,
                              time=time)
  
  # Concatenate to table.
  calibration_table <- data.table::rbindlist(calibration_table, use.names=TRUE)
  
  # Prevent processing of empty tables.
  if(is_empty(calibration_table)) return(NULL)
  
  # Set column order.
  data.table::setcolorder(calibration_table, c("expected", "observed", "km_var", "n_g", "rep_id"))
  
  return(calibration_table)
}



..compute_calibration_data_survival <- function(groups, data, time, ii){
  
  # Placeholder variables
  obs_prob <- exp_prob <- n_g <- km_var <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get observed and expected probabilities over the groups
  for(jj in seq_along(groups)){
    
    # Find data for the current group
    group_data <- data[unique(groups[[jj]]), on=.NATURAL]
    
    if(nrow(group_data) >= 2){
      
      # Fit a Kaplan-Meier curve for the current group
      km_fit <- survival::survfit(Surv(time, event)~1, data=group_data)
      
      if(length(km_fit$time) >= 2){
        
        # Get observed probability
        obs_prob[jj] <- stats::approx(x=km_fit$time,
                                      y=km_fit$surv,
                                      xout=time,
                                      method="linear",
                                      rule=2)$y
        
        # Get expected probability
        exp_prob[jj] <- mean(group_data$exp_prob)
        
        # Get group size
        n_g[jj] <- length(groups[[jj]])
        
        # Get greenwood variance estimate.
        km_var[jj] <- stats::approx(x=km_fit$time,
                                    y=km_fit$std.err,
                                    xout=time,
                                    method="linear",
                                    rule=2)$y^2
      } else {
        # Set NA values.
        obs_prob[jj] <- exp_prob[jj] <- km_var[jj] <- NA_real_
        n_g[jj] <- NA_integer_
      }
      
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



.compute_calibration_data_categorical<- function(data, positive_class, n_groups=1L){
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
  
  # Suppress NOTES due to non-standard evaluation in data.table
  observed_class <- exp_prob <- NULL
  
  # Determine outcome column
  positive_class_column_name <- get_class_probability_name(x=positive_class)
  
  # Identify the real outcome columns
  outcome_column <- get_outcome_columns(x="binomial")
  
  # Find sample identifiers.
  sample_identifiers <- get_id_columns(id_depth="series")
  
  # Create a local copy of the data table
  data <- data.table::copy(data[, mget(c(sample_identifiers, outcome_column, positive_class_column_name))])
  
  # Only select instances with known outcomes and probabilities
  data <- data[!(is.na(get(outcome_column)) | is.na(get(positive_class_column_name))), ]
  
  # Check that there is actually any data to work with
  if(is_empty(data)) return(NULL)
  
  # Rename columns to standard names
  data.table::setnames(data,
                       old=c(outcome_column, positive_class_column_name),
                       new=c("observed_class", "exp_prob"))
  
  # Mask class so that positive outcomes are TRUE and the rest FALSE
  data[, "observed_class":=observed_class==positive_class]
  
  # Sort by probability
  data <- data[order(exp_prob)]
  
  # Repeatedly split into groups. The number of groups is determined using sturges rule
  repeated_groups <- lapply(seq_len(n_groups), function(ii, x, sample_identifiers){
    return(create_randomised_groups(x=x, sample_identifiers=sample_identifiers))
    
  },
  x=data$exp_prob,
  sample_identifiers=data[, mget(sample_identifiers)])
  
  # Iterate over groups
  calibration_table <- lapply(seq_len(length(repeated_groups)),
                              function(ii, groups, data){
                                return(..compute_calibration_data_categorical(data=data,
                                                                              groups=groups[[ii]],
                                                                              ii=ii))
                                
                              },
                              data=data,
                              groups=repeated_groups)
  
  # Combine to a single list.
  calibration_table <- data.table::rbindlist(calibration_table, use.names=TRUE)
  
  # Check that any calibration data was generated.
  if(is_empty(calibration_table)) return(NULL)

  # Set column order
  data.table::setcolorder(calibration_table, c("expected", "observed", "n_g", "n_pos", "n_neg", "rep_id"))
  
  return(calibration_table)
}




..compute_calibration_data_categorical <- function(groups, data, ii){
  
  obs_prob <- exp_prob <- n_g <- n_pos <- n_neg <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get oberved and expected probabilities over the groups
  for(jj in seq_along(groups)){
    # Find data for the current group
    group_data <- data[unique(groups[[jj]]), on=.NATURAL]
    
    # Mean expected probability in a group.
    exp_prob[jj] <- mean(group_data$exp_prob)
    
    # Observed proportion of positive class in a group.
    obs_prob[jj] <- mean(group_data$observed_class)
    
    # Number of samples in the group
    n_g[jj] <- nrow(group_data)
    
    # Number of samples with the positive class in each group.
    n_pos[jj] <- sum(group_data$observed_class)
    
    # Number of samples with the negative class in each group.
    n_neg[jj] <- sum(!group_data$observed_class)
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



.compute_calibration_data_regression <- function(object, data, n_groups=1L){
  # Calibration for regression problems is pretty straightforward. However, for
  # goodness-of-fit tests, we need to constrain expected and observed value
  # ranges to [0, 1]. To do so, we use the range of outcome values from the
  # development data, as stored in the calibration_info slot of the input
  # object.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- predicted_outcome <- NULL
  
  # Remove non-finite predicted values.
  data <- data[is.finite(outcome) & is.finite(predicted_outcome), ]
  
  # Check for empty required data.
  if(is_empty(data)) return(NULL)
  if(is.null(object@outcome_info)) return(NULL)
  
  # Determine the outcome range
  outcome_range <- range(object@outcome_info@distribution$fivenum)
  
  # Get the shift and scale parameters.
  norm_shift <- outcome_range[1]
  norm_scale <- diff(outcome_range)
  
  # Set scale parameters equal to 0.0 to 1.0 to avoid division by 0.
  if(norm_scale == 0.0) norm_scale <- 1.0
  
  # Apply normalisation.
  data[, ":="("expected"=(predicted_outcome - norm_shift) / norm_scale,
              "observed"=(outcome - norm_shift) / norm_scale)]
  
  # Repeatedly split into groups. The number of groups is determined using
  # sturges rule.
  repeated_groups <- lapply(seq_len(n_groups), function(ii, x, sample_identifiers){
    return(create_randomised_groups(x=x, sample_identifiers=sample_identifiers))
    
  },
  x=data$expected,
  sample_identifiers=data[, mget(get_id_columns(id_depth="series"))])
  
  # Iterate over groups
  calibration_table <- lapply(seq_len(length(repeated_groups)),
                              function(ii, groups, data){
                                return(..compute_calibration_data_regression(data=data,
                                                                             groups=groups[[ii]],
                                                                             ii=ii))
                                
                              },
                              data=data,
                              groups=repeated_groups)
  
  # Concatenate to a single table
  calibration_table <- data.table::rbindlist(calibration_table, use.names=TRUE)
  
  # Check if the resulting table contains data.
  if(is_empty(calibration_table)) return(NULL)
  
  # Make columns match the expected order
  data.table::setcolorder(calibration_table, c("expected", "observed", "n_g", "rep_id"))
  
  return(calibration_table)
}



..compute_calibration_data_regression <- function(groups, data, ii){
  
  obs_prob <- exp_prob <- n_g <- numeric(length(groups))
  
  # Check that the groups list contains at least one entry.
  if(is_empty(groups)) return(NULL)
  
  # Get observed and expected probabilities over the groups
  for(jj in seq_along(groups)){
    # Find data for the current group
    group_data <- data[unique(groups[[jj]]), on=.NATURAL]
    
    # Mean expected probability in a group.
    exp_prob[jj] <- mean(group_data$expected)
    
    # Observed proportion of positive class in a group.
    obs_prob[jj] <- mean(group_data$observed)
    
    # Number of samples in the group
    n_g[jj] <- nrow(group_data)
  }
  
  # Create table
  calibration_table <- data.table::data.table("expected"=exp_prob,
                                              "observed"=obs_prob,
                                              "n_g"=n_g,
                                              "rep_id"=ii)
  
  return(calibration_table)
}



.compute_calibration_linear_fit <- function(calibration_data, outcome_type){
  # Tests calibration-at-the-large and calibration slope
  
  # Suppress NOTES due to non-standard evaluation in data.table.
  type <- value <- ci_low <- ci_up <- expected <- observed <- NULL
  
  # Check that calibration data are not empty.
  if(is_empty(calibration_data)) return(NULL)
  
  # Filter out non-finite expected and observed values.
  calibration_data <- calibration_data[is.finite(expected) & is.finite(observed)]
  
  # Check if the input is empty or only contains one entry
  if(nrow(calibration_data) < 2) return(NULL)
  
  # Perform a linear fit. Note that the slope is offset by 1*expected to
  # directly compare with the expected slope of 1.
  fit <- stats::lm(observed~expected+1+offset(expected),
                   data=calibration_data)
  
  # Get coefficients for intercept and slope
  fit_coef <- stats::coef(fit)
  
  # Get confidence intervals
  fit_conf_int <- suppressWarnings(stats::confint(fit))
  
  # Get summary
  fit_summary <- suppressWarnings(stats::summary.lm(fit))$coefficients
  
  # Correct for having multiple overlapping groups in binomial and multinomial
  # tests.
  if(outcome_type %in% c("binomial", "multinomial", "survival")){
    
    # Determine the number of groups.
    n_groups <- data.table::uniqueN(calibration_data, by="rep_id")
    
    # Recompute the standard deviation
    fit_summary[, 2] <- fit_summary[, 2] * sqrt(n_groups)
    
    # Recompute the t-score
    fit_summary[, 3] <- fit_summary[, 1] / fit_summary[, 2]
    
    # Recompute the p-value
    fit_summary[, 4] <- 2.0 * stats::pt(abs(fit_summary[, 3]), fit$df.residual, lower.tail = FALSE)
    
    # Adapt the confidence intervals
    fit_conf_int[, 1] <- fit_summary[, 1] + fit_summary[, 2] * stats::qnorm(0.025)
    fit_conf_int[, 2] <- fit_summary[, 1] + fit_summary[, 2] * stats::qnorm(0.975)
  }
  
  # Construct data table
  calibration_at_large <- data.table::data.table("type"=c("offset", "slope"),
                                                 "value"=fit_coef,
                                                 "ci_low"=fit_conf_int[,1],
                                                 "ci_up"=fit_conf_int[,2],
                                                 "p_value"=fit_summary[, 4])
  
  # Update the slope so that the expected slope (slope+1) is shown instead
  calibration_at_large[type=="slope", ":="("value"=value+1,
                                           "ci_low"=ci_low+1,
                                           "ci_up"=ci_up+1)]
  
  return(calibration_at_large)
}



.compute_calibration_hosmer_lemeshow <- function(calibration_data){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  observed <- expected <- n_g <- hm_group <- statistic <- n_groups <- p_value <- NULL
  
  # Check for empty calibration tables
  if(is_empty(calibration_data)) return(NULL)
  
  # Make local copy of the calibration data.
  calibration_data <- data.table::copy(calibration_data)
  
  # Compute test statistic for each group
  calibration_data[, "hm_group":=(observed-expected)^2*n_g / (expected * (1-expected))]
  
  # Compute test statistic for each time point
  gof_table <- calibration_data[, list(statistic=sum(hm_group, na.rm=TRUE), n_groups=.N),
                                by="rep_id"]
  
  # Remove all entries with n_groups < 3
  gof_table <- gof_table[n_groups >= 3]
  
  if(is_empty(gof_table)) return(NULL)
  
  # Perform chi-square test
  gof_table <- gof_table[, list(p_value=stats::pchisq(q=statistic, df=n_groups-2, lower.tail=FALSE)),
                         by="rep_id"]
  
  # Since samples overlap, combination methods that assume independence cannot
  # be used (e.g. Fisher's method). Hence we calculate a harmonic mean p value
  # according to Wilson (2019), 10.1073/pnas.1814092116.
  gof_table <- gof_table[, list("p_value"= 1 / sum(1/.N * 1/p_value))]
  
  # Add test type
  gof_table[, "type":="hosmer_lemeshow"]
  
  # Reorder columns
  data.table::setcolorder(gof_table, c("type", "p_value"))
  
  return(gof_table)
}



.compute_calibration_nam_dagostino <- function(calibration_data){
  # Nam-D'Agostino and Greenwood-Nam-D'Agostino tests. See Demler et al. 2015.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  observed <- expected <- n_g <- km_var <- nd_group <- gnd_group <- n_groups <- NULL
  nam_dagostino <- greenwood_nam_dagostino <- p_value <- NULL
  
  # Make local copy of calibration_data to avoid updating the main copy by
  # reference.
  calibration_data <- data.table::copy(calibration_data)
  
  # Check for empty calibration tables
  if(is_empty(calibration_data)) return(NULL)
  
  # Compute test statistic for each group.
  calibration_data[, ":="("nd_group"=(observed-expected)^2*n_g / (expected * (1-expected)),
                          "gnd_group"=(observed-expected)^2 / km_var)]
  
  # Remove gnd_group that are infinite (which occurs when observed==0.000)
  calibration_data[!is.finite(gnd_group), "gnd_group":=0]
  
  # Compute test statistic for each time point
  gof_table <- calibration_data[, list("nam_dagostino"=sum(nd_group, na.rm=TRUE),
                                       "greenwood_nam_dagostino"=sum(gnd_group, na.rm=TRUE),
                                       "n_groups"=.N),
                                by="rep_id"]
  
  # Compute test score for both test tests.
  gof_table <- gof_table[n_groups > 1,
                         list("nam_dagostino"=stats::pchisq(q=nam_dagostino, df=n_groups-1, lower.tail=FALSE),
                              "greenwood_nam_dagostino"=stats::pchisq(q=greenwood_nam_dagostino, df=n_groups-1, lower.tail=FALSE)),
                         by="rep_id"]
  
  # Check for an empty goodness-of-fit table.
  if(is_empty(gof_table)) return(NULL)
  
  # Melt so that each test is on a separate row
  gof_table <- data.table::melt(gof_table,
                                id.vars="rep_id",
                                variable.name="type",
                                value.name="p_value",
                                variable.factor=FALSE)
  
  # Since samples overlap, combination methods that assume independence cannot
  # be used (e.g. Fisher's method). Hence we calculate a harmonic mean p value
  # according to Wilson (2019), 10.1073/pnas.1814092116.
  gof_table <- gof_table[, harmonic_p_value(.SD), by="type", .SDcols="p_value"]

  # Reorder columns
  data.table::setcolorder(gof_table, c("type", "p_value"))
  
  return(gof_table)
}



..compute_calibration_data_interpolated <- function(data, method="loess"){
  
  # Set interpolation range for expected values. This allows for aggregating
  # observed values based on the expected value.
  expected_interpolated <- seq(from=0.000, to=1.000, by=0.005)
  
  # Select only unique entries.
  data <- unique(data)
  
  # Determine the number of unique expected values.
  n_instances <- data.table::uniqueN(data, by="expected")
  
  # Switch interpolation method for small number of predictions.
  if(method == "loess"){
    if(n_instances >= 4){
      degree <- 2
      
    } else if(n_instances == 3){
      degree <- 1
      
    } else if(n_instances == 2){
      method <- "linear"
      
    } else {
      return(list("expected"=NA_real_,
                  "observed"=NA_real_))
    }
  }
  
  if(method == "linear"){
    # Interpolate observed values. Return NA values outside the known expected
    # range.
    observed_interpolated <- stats::approx(x=data$expected,
                                           y=data$observed,
                                           xout=expected_interpolated,
                                           method="linear",
                                           rule=1)$y
    
  } else if(method == "loess"){
    # Set up the loess model.
    loess_predictor <- suppressWarnings(stats::loess(observed ~ expected,
                                                     data=data,
                                                     span=1.0,
                                                     degree=degree,
                                                     normalize=FALSE,
                                                     control=stats::loess.control(surface="direct")))
    
    # Predicted interpolated values.
    observed_interpolated <- suppressWarnings(predict(loess_predictor,
                                                      newdata=expected_interpolated))
  }
  
  # Set expected values and the corresponding interpolated observed values.
  return(list("expected"=expected_interpolated,
              "observed"=observed_interpolated))
}



##### ..compute_data_element_estimates (familiarDataElementCalibrationData) #####
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementCalibrationData"),
          function(x, x_list=NULL, ...){
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Identify the estimation types of the current data elements.
            estimation_type <- sapply(x, function(x) (x@estimation_type))
            
            # Determine the bootstrap_ci_method and the aggregation function
            if(any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))){
              
              # Point estimates need to be for all expected probability values,
              # which means that we have to compute it from the data.
              x <- .add_point_estimate_from_elements(x[estimation_type %in% c("bci", "bootstrap_confidence_interval")])
            }
            
            # Check that x is not empty.
            if(is_empty(x)) return(NULL)
            
            return(callNextMethod(x_list=x))
          })

##### ..compute_data_element_estimates (familiarDataElementCalibrationLinearFit) #####
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementCalibrationLinearFit"),
          function(x, x_list=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            p_value <- NULL
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Process data normally.
            y <- callNextMethod()
            
            # Identify the estimation types of the current data elements.
            estimation_type <- sapply(x, function(x) (x@estimation_type))
            
            # Determine the bootstrap_ci_method and the aggregation function
            if(any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))){
              
              # Obtain data from bootstrapped data.
              data <- x[estimation_type %in% c("bci", "bootstrap_confidence_interval")][[1]]@data
              
            } else {
              data <- x[[1]]@data
            }
            
            # Get the grouping column.
            grouping_column <- x[[1]]@grouping_column
            
            # Compute p-value by grouping column.
            p_value <- data[, list("p_value"=stats::median(p_value)),
                            by=c(grouping_column)]
            
            # Merge data into 
            y@data <- merge(x=y@data,
                            y=p_value,
                            by=grouping_column)
            
            # Update value column
            y@value_column <- setdiff(names(y@data),
                                      y@grouping_column)
            
            return(y)
          })


##### ..compute_data_element_estimates (familiarDataElementCalibrationGoodnessOfFit) #####
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementCalibrationGoodnessOfFit"),
          function(x, x_list=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            p_value <- NULL
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Identify the estimation types of the current data elements.
            estimation_type <- sapply(x, function(x) (x@estimation_type))
            
            # Determine the bootstrap_ci_method and the aggregation function
            if(any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))){
              
              # Obtain data from bootstrapped data.
              x <- x[estimation_type %in% c("bci", "bootstrap_confidence_interval")][[1]]
              
            } else {
              x <- x[[1]]
            }
            
            # Get the grouping column.
            grouping_column <- x@grouping_column
            
            # Compute p-value by grouping column.
            x@data <- x@data[, list("p_value"=stats::median(p_value)),
                             by=c(grouping_column)]
            
            # Update value column
            x@value_column <- setdiff(names(x@data),
                                      x@grouping_column)
            
            return(x)
          })


##### ..compute_data_element_estimates (familiarDataElementCalibrationDensity) #####
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementCalibrationDensity"),
          function(x, x_list=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            frequency <- expected <- NULL
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Identify the estimation types of the current data elements.
            estimation_type <- sapply(x, function(x) (x@estimation_type))
            
            # Determine the bootstrap_ci_method and the aggregation function
            if(any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))){
              
              # Obtain data from bootstrapped data.
              x <- x[estimation_type %in% c("bci", "bootstrap_confidence_interval")][[1]]
              
            } else {
              x <- x[[1]]
            }
            
            # Get the grouping column.
            grouping_column <- x@grouping_column
            
            # Compute frequency by grouping column.
            x@data <- x@data[, list("frequency"=sum(frequency)),
                             by=c(grouping_column)]
            
            # Normalise frequency by all grouping columns minus expected.
            grouping_column <- setdiff(grouping_column, "expected")
            
            if(length(grouping_column) > 0){
              x@data <- x@data[, "frequency":=frequency / sum(frequency), by=c(grouping_column)][order(expected)]
            } else {
              x@data <- x@data[, "frequency":=frequency / sum(frequency)][order(expected)]
            }
            
            # Update value column
            x@value_column <- setdiff(names(x@data),
                                      x@grouping_column)
            
            return(x)
          })



#####export_calibration_data#####

#'@title Extract and export calibration and goodness-of-fit tests.
#'
#'@description Extract and export calibration and goodness-of-fit tests for data
#'  in a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_calibration_data
#'@inheritDotParams as_familiar_collection
#'
#'@details Data is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previous files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Calibration tests are performed based on expected (predicted) and observed
#'  outcomes. For all outcomes, calibration-at-the-large and calibration slopes
#'  are determined. Furthermore, for all but survival outcomes, a repeated,
#'  randomised grouping Hosmer-Lemeshow test is performed. For survival
#'  outcomes, the Nam-D'Agostino and Greenwood-Nam-D'Agostino tests are
#'  performed.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_calibration_data
#'@md
#'@rdname export_calibration_data-methods
setGeneric("export_calibration_data", function(object, dir_path=NULL, aggregate_results=TRUE, ...) standardGeneric("export_calibration_data"))

#####export_calibration_data (collection)#####

#'@rdname export_calibration_data-methods
setMethod("export_calibration_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Obtain calibration data.
            calibration_data <- .export(x=object,
                                        data_slot="calibration_data",
                                        dir_path=dir_path,
                                        aggregate_results=aggregate_results,
                                        type="calibration",
                                        subtype="data",
                                        object_class="familiarDataElementCalibrationData")
            
            # Obtain linear fit data
            linear_fit_data <- .export(x=object,
                                       data_slot="calibration_data",
                                       dir_path=dir_path,
                                       aggregate_results=aggregate_results,
                                       type="calibration",
                                       subtype="at_large",
                                       object_class="familiarDataElementCalibrationLinearFit")
            
            # Obtain goodness of fit data
            gof_data <- .export(x=object,
                                data_slot="calibration_data",
                                dir_path=dir_path,
                                aggregate_results=aggregate_results,
                                type="calibration",
                                subtype="gof_test",
                                object_class="familiarDataElementCalibrationGoodnessOfFit")
            
            # Obtain density data
            density_data <- .export(x=object,
                                    data_slot="calibration_data",
                                    dir_path=dir_path,
                                    aggregate_results=aggregate_results,
                                    type="calibration",
                                    subtype="density",
                                    object_class="familiarDataElementCalibrationDensity")
            
            
            if(is.null(dir_path)){
              return(list("data"=calibration_data,
                          "density"=density_data,
                          "linear_test"=linear_fit_data,
                          "gof_test"=gof_data))
              
            } else {
              return(NULL)
            }
          })

#####export_calibration_data (generic)#####

#'@rdname export_calibration_data-methods
setMethod("export_calibration_data", signature(object="ANY"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="calibration_data",
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_calibration_data,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results),
                                  list(...))))
          })
