#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include FamiliarDataComputation.R
NULL

#'@title Internal function to extract decision curve analysis data.
#'
#'@description Computes decision curve analysis data from a `familiarEnsemble` object.
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
setGeneric("extract_decision_curve_data",
           function(object,
                    data,
                    cl=NULL,
                    ensemble_method=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    aggregate_ci=waiver(),
                    eval_times=waiver(),
                    is_pre_processed=FALSE,
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_decision_curve_data"))

setMethod("extract_decision_curve_data", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   ensemble_method=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   aggregate_ci=waiver(),
                   eval_times=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Decision curve analysis is only available for categorical and
            # survival outcomes.
            if(object@outcome_type %in% c("count", "continuous")) return(NULL)
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing data for decision curve analysis."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)){
              eval_times <- object@settings$eval_times
            }
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)){
              ensemble_method <- object@settings$ensemble_method
            }
            
            # Load confidence alpha from object settings attribute if not
            # provided externally.
            if(is.waive(confidence_level)){
              confidence_level <- object@settings$confidence_level
            }
            
            # Check confidence_level input argument
            .check_number_in_valid_range(x=confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)){
              bootstrap_ci_method <- object@settings$bootstrap_ci_method
            }
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_methpd",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            if(is.waive(aggregate_ci)){
              aggregate_ci <- object@settings$aggregate_ci
            }
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Extract data for the individual models and the ensemble.
            dca_data <- universal_extractor(object=object,
                                            cl=cl,
                                            FUN=.extract_decision_curve_data,
                                            individual_model_ci=FALSE,
                                            data=data,
                                            is_pre_processed=is_pre_processed,
                                            eval_times=eval_times,
                                            confidence_level=confidence_level,
                                            aggregate_ci=any(c("all", "decision_curve_analyis") %in% aggregate_ci),
                                            bootstrap_ci_method=bootstrap_ci_method,
                                            message_indent=message_indent + 1L,
                                            verbose=verbose)
            
            return(dca_data)
          })


.extract_decision_curve_data <- function(object, data, cl=NULL, is_pre_processed, eval_times, confidence_level,
                                         aggregate_ci, determine_ci, bootstrap_ci_method, verbose, message_indent=0L){
  
  # Ensemble model.
  if(object@outcome_type %in% c("binomial", "multinomial")){
    # Iterate over outcome classes.
    
    # Predict class probabilities.
    prediction_data <- .predict(object=object,
                                data=data,
                                is_pre_processed=is_pre_processed)
    
    # Check if any predictions are valid.
    if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
    
    # Iterate over class levels.
    dca_data <- lapply(get_outcome_class_levels(object),
                       .compute_dca_data_categorical,
                       data=prediction_data,
                       object=object,
                       confidence_level=confidence_level,
                       determine_ci=determine_ci,
                       cl=cl,
                       message_indent=message_indent,
                       verbose=verbose)
    
    # Concatenate lists.
    dca_data <- list("model_data"=rbind_list_list(dca_data, "model_data"),
                     "intervention_data"=rbind_list_list(dca_data, "intervention_data"),
                     "bootstrap_data"=rbind_list_list(dca_data, "bootstrap_data"),
                     "confidence_level"=confidence_level)
    
  } else if(object@outcome_type %in% c("survival")){
    # Iterate over evaluation times.
    dca_data <- lapply(eval_times,
                       .compute_dca_data_survival,
                       data=data,
                       object=object,
                       confidence_level=confidence_level,
                       determine_ci=determine_ci,
                       cl=cl,
                       message_indent=message_indent,
                       verbose=verbose)
    
    # Concatenate lists.
    dca_data <- list("model_data"=rbind_list_list(dca_data, "model_data"),
                     "intervention_data"=rbind_list_list(dca_data, "intervention_data"),
                     "bootstrap_data"=rbind_list_list(dca_data, "bootstrap_data"),
                     "confidence_level"=confidence_level)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  if(determine_ci & aggregate_ci){
    browser()
    # Aggregate the data by computing the bootstrap confidence intervals.
    dca_data$model_data <- .compute_bootstrap_ci(x0=dca_data$model_data,
                                                 xb=dca_data$bootstrap_data,
                                                 target_column="net_benefit",
                                                 bootstrap_ci_method="bc",
                                                 additional_splitting_variable="threshold_probability",
                                                 confidence_level=confidence_level)
    
    # Set the bootstrap_data to NULL.
    dca_data$bootstrap_data <- NULL
  }
  
  return(dca_data)
}


.compute_dca_data_categorical <- function(positive_class, data, object, confidence_level,
                                          determine_ci=FALSE, cl=NULL, verbose=FALSE,
                                          message_indent=0L){
  
  # Check if the data has more than 1 row.
  if(nrow(data) <= 1) return(list("confidence_level"=confidence_level))
 
  # Set test probabilities
  test_probabilities <- seq(from=0.00, to=1.00, by=0.005)
  
  # Compute data from the model.
  model_data <- .compute_dca_data_categorical_model(data=data,
                                                    x=test_probabilities,
                                                    positive_class=positive_class)

  # Compute intervention for all data.
  intervention_data <- .compute_dca_data_categorical_model(data=data,
                                                           x=test_probabilities,
                                                           positive_class=positive_class,
                                                           return_intervention=TRUE)
  
  if(determine_ci){
    if(verbose) logger.message(paste0("Computing bootstrap confidence interval data for the \"", positive_class, "\" class."),
                               indent=message_indent)
    
    # Bootstrap the data.
    bootstrap_data <- bootstrapper(data=data,
                                   alpha= 1.0 - confidence_level,
                                   FUN=.compute_dca_data_categorical_model,
                                   positive_class=positive_class,
                                   x=test_probabilities,
                                   cl=cl,
                                   verbose=verbose)
    
  } else {
    bootstrap_data <- NULL
  }

  # Add to list and return
  return(list("model_data"=model_data,
              "intervention_data"=intervention_data,
              "bootstrap_data"=bootstrap_data,
              "confidence_level"=confidence_level))
}



.compute_dca_data_categorical_model <- function(data, x, positive_class, return_intervention=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- probability <- is_positive <- NULL
  
  # Make a local copy
  data <- data.table::copy(data)
  data.table::setnames(data, old=get_class_probability_name(positive_class), new="probability")
  
  # Determine positive output.
  data[, "is_positive":=outcome == positive_class]
  
  # Keep only probability and is_positive
  data <- data[, c("probability", "is_positive")]
  
  # Order by inverse probability.
  data <- data[order(-probability)]
  
  # Determine the number of samples.
  n <- nrow(data)
  
  if(return_intervention){
    # Determine maximum number of true and false positives.
    n_max_true_positive <- sum(data$is_positive)
    n_max_false_positive <- sum(!data$is_positive)
    
    # Compute benefit for the situation an intervention always happens.
    net_benefit <- n_max_true_positive / n - n_max_false_positive / n * (x / (1.0 - x))
    
  } else {
    # Determine the number of true and false positives.
    data[, ":="("n_true_positive"=cumsum(is_positive),
                "n_false_positive"=cumsum(!is_positive))]
    
    # Compute benefit for the model.
    net_benefit <- ..compute_dca_data_net_benefit(data, x)
  }
  
  # Store data.
  model_data <- data.table::data.table("pos_class"=positive_class,
                                       "threshold_probability"=x,
                                       "net_benefit"=net_benefit)
  
  return(model_data)
}



.compute_dca_data_survival <- function(evaluation_time, data, object, confidence_level,
                                       determine_ci=TRUE, cl=NULL, verbose=FALSE, message_indent=0L){
  
  # Predict survival probabilities.
  data <- .predict(object=object,
                   data=data,
                   time=evaluation_time,
                   is_pre_processed=is_pre_processed,
                   type="survival_probability")

  # Check if any predictions are valid.
  if(!any_predictions_valid(data, outcome_type=object@outcome_type)) return(list("confidence_level"=confidence_level))
  
  # Check if the data has more than 1 row.
  if(nrow(data) <= 1) return(list("confidence_level"=confidence_level))
  
  # Set test probabilities
  test_probabilities <- seq(from=0.00, to=1.00, by=0.005)
  
  # Compute data from the model.
  model_data <- .compute_dca_data_survival_model(data=data,
                                                 x=test_probabilities,
                                                 evaluation_time=evaluation_time)
  
  # Compute intervention for all data.
  intervention_data <- .compute_dca_data_survival_model(data=data,
                                                        x=test_probabilities,
                                                        evaluation_time=evaluation_time,
                                                        return_intervention=TRUE)
  
  if(determine_ci){
    if(verbose) logger.message(paste0("Computing bootstrap confidence interval data at a time of ", evaluation_time, "."),
                               indent=message_indent)
    
    # Bootstrap the data.
    bootstrap_data <- bootstrapper(data=data,
                                   alpha= 1.0 - confidence_level,
                                   FUN=.compute_dca_data_survival_model,
                                   evaluation_time=evaluation_time,
                                   x=test_probabilities,
                                   cl=cl,
                                   verbose=verbose)
    
  } else {
    bootstrap_data <- NULL
  }
  
  # Add to list and return
  return(list("model_data"=model_data,
              "intervention_data"=intervention_data,
              "bootstrap_data"=bootstrap_data,
              "confidence_level"=confidence_level))
}


.compute_dca_data_survival_model <- function(data, x, evaluation_time, return_intervention=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  predicted_outcome <- outcome_event <- death <- censored <- n <- NULL
  
  # Prepare net benefit.
  net_benefit <- numeric(length(x))
  
  # Determine the total group size.
  n_group_size <- nrow(data)
  
  # Define probability thresholds that will be evaluated. When the intervention
  # curve is required, only one threshold (0.0) is used.
  if(return_intervention){
    p_threshold <- 0.0
  } else {
    p_threshold <- x
  }
  
  # We want to avoid running too many computations. Therefore, we will only
  # compute the number of true positives and false positives when the group size
  # changes.
  previous_group_size <- n_group_size + 1
  for(ii in seq_along(p_threshold)){
    # Select the group of patients
    surv_group <- data.table::copy(data[predicted_outcome >= x[ii]])
    
    # Get the total group size of the group where predicted survival probability
    # exceeds the threshold probability.
    n_surv_group <- nrow(surv_group)
    
    if(n_surv_group == previous_group_size){
      # True positive and false positive numbers did not change. This is always
      # skipped in the first iteration, because the previous group size is set
      # to a value that lies beyond the possible survival group size.
      n_true_positive <- n_true_positive
      n_false_positive <- n_false_positive
      
    } else if(n_surv_group == 0) {
      # There are no true and false positives, because there are no positives
      # with survival probability greater than the threshold.
      n_true_positive <- n_false_positive <- 0
      
    } else {
      # Create the basic part of the Kaplan-Meier data by summing the number of
      # deaths and censored patients at the end of each interval. We limit
      # ourselves to those samples that were censored or had an event prior to
      # the evaluation time. The remaining patients do not affect the survival
      # probability.
      surv_group <- surv_group[outcome_time <= evaluation_time, list("death"=sum(outcome_event == 1),
                                                                     "censored"=sum(outcome_event == 0)),
                               by="outcome_time"][order(outcome_time)]
      
      if(nrow(surv_group) > 0){
        # Add group sizes at the start of each interval.
        surv_group[, "n":=n_surv_group - shift(cumsum(death + censored), n=1, fill=0, type="lag")]
        
        # Compute the probability of survival in the interval
        surv_group[, "survival_in_interval":=(n - death) / n]
        
        # Compute survival probability.
        survival_probability <- prod(surv_group$survival_in_interval)
        
      } else {
        # All samples survived longer than evaluation_time.
        survival_probability <- 1.0
      }
      
      # Compute the number of true and false positives according to Vickers et
      # al. 2008 (10.1186/1147-6947-8-53)
      n_true_positive <- (1.0 - survival_probability) * n_surv_group
      n_false_positive <- survival_probability * n_surv_group
    }
    
    # Update the previous group size.
    previous_group_size <- n_surv_group
    
    # Compute net benefit.
    if(return_intervention){
      net_benefit <- n_true_positive / n_group_size - n_false_positive / n_group_size * (x / (1.0 - x))
    } else {
      net_benefit[ii] <- n_true_positive / n_group_size - n_false_positive / n_group_size * (p_threshold[ii] / (1.0 - p_threshold[ii]))
      
      if(p_threshold[ii] == 1.0) net_benefit[ii] <- 0.0
    }
  }
  
  # Store data.
  model_data <- data.table::data.table("eval_time"=evaluation_time,
                                       "threshold_probability"=x,
                                       "net_benefit"=net_benefit)
  
  return(model_data)
}



..compute_dca_data_net_benefit <- function(data, x){
  # Compute net benefit for models.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n_true_positive <- n_false_positive <- probability <- NULL
  
  # Determine maximum number of true and false positives.
  n_max_true_positive <- max(data$n_true_positive)

    # Determine the number of samples.
  n <- nrow(data)
  
  # Determine net benefit
  data[, ":="("net_benefit"=n_true_positive / n - n_false_positive / n * (probability / (1.0 - probability)))]
  
  # Compute net benefit at the test probabilities.
  net_benefit <- suppressWarnings(stats::approx(x=data$probability,
                                                y=data$net_benefit,
                                                xout=x,
                                                yleft=n_max_true_positive / n,
                                                yright=0.0,
                                                method="constant")$y)
  
  return(net_benefit)
}
