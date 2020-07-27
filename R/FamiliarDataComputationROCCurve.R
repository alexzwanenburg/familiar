#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include FamiliarDataComputation.R
NULL

#'@title Internal function to extract area under the ROC curve information.
#'
#'@description Computes the ROC curve from a `familiarEnsemble`.
#''
#'@inheritParams extract_data
#'
#'@details This function also computes credibility intervals for the ROC curve
#'  for the ensemble model, at the level of `confidence_level`. In the case of
#'  multinomial outcomes, an AUC curve is computed per class in a
#'  one-against-all fashion.
#'
#'  To allow plotting of multiple AUC curves in the same plot and the use of
#'  ensemble models, the AUC curve is evaluated at 0.01 (1-specificity) intervals.
#'
#'@return A list with data.tables for single and ensemble model ROC curve data.
#'@md
#'@keywords internal
setGeneric("extract_auc_data", function(object,
                                        data,
                                        cl=NULL,
                                        ensemble_method=waiver(),
                                        confidence_level=waiver(),
                                        bootstrap_ci_method=waiver(),
                                        aggregate_ci=waiver(),
                                        is_pre_processed=FALSE,
                                        message_indent=0L,
                                        verbose=FALSE,
                                        ...) standardGeneric("extract_auc_data"))

#####extract_auc_data#####
setMethod("extract_auc_data", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   ensemble_method=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   aggregate_ci=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...) {
            # Extract data for plotting AUC curves.
            
            # AUC data can only be prepared for binomial and multinomial outcomes
            if(!object@outcome_type %in% c("binomial", "multinomial")) return(NULL)
            
            # Message start of auc computations
            if(verbose){
              logger.message(paste0("Computing receiver-operating characteristic curves."),
                             indent=message_indent)
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Obtain confidence level from the settings file stored with the
            # familiarEnsemble object.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check alpha
            .check_number_in_valid_range(confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)){
              bootstrap_ci_method <- object@settings$bootstrap_ci_method
            }
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_methpd",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            if(is.waive(aggregate_ci)) aggregate_ci <- object@settings$aggregate_ci
            
            # Extract data for the individual models and the ensemble.
            roc_data <- universal_extractor(object=object,
                                            cl=cl,
                                            FUN=.extract_roc_curve_data,
                                            individual_model_ci=FALSE,
                                            data=data,
                                            is_pre_processed=is_pre_processed,
                                            ensemble_method=ensemble_method,
                                            confidence_level=confidence_level,
                                            aggregate_ci=any(c("all", "auc_data") %in% aggregate_ci),
                                            bootstrap_ci_method=bootstrap_ci_method,
                                            message_indent=message_indent + 1L,
                                            verbose=verbose)
            
            return(roc_data)
          })



.extract_roc_curve_data <- function(object, data, cl=NULL, is_pre_processed, confidence_level, ensemble_method,
                                    aggregate_ci, determine_ci, bootstrap_ci_method, verbose, message_indent=0L){
  
  
  if(object@outcome_type %in% c("binomial", "multinomial")){
    # Iterate over outcome classes.
    
    # Predict class probabilities.
    prediction_data <- .predict(object=object,
                                data=data,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
    
    # Check if any predictions are valid.
    if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
    
    # Iterate over class levels.
    roc_data <- lapply(get_outcome_class_levels(object),
                       .compute_roc_data_categorical,
                       data=prediction_data,
                       object=object,
                       confidence_level=confidence_level,
                       determine_ci=determine_ci,
                       cl=cl,
                       message_indent=message_indent,
                       verbose=verbose)
    
    # Concatenate lists.
    roc_data <- list("model_data"=rbind_list_list(roc_data, "model_data"),
                     "bootstrap_data"=rbind_list_list(roc_data, "bootstrap_data"),
                     "confidence_level"=confidence_level)
    
  } else if(object@outcome_type %in% c("survival")){
    # TODO ROC-like curves for survival endpoints.
    
    # # Iterate over evaluation times.
    # roc_data <- lapply(eval_times,
    #                    .compute_roc_data_survival,
    #                    data=data,
    #                    object=object,
    #                    confidence_level=confidence_level,
    #                    determine_ci=determine_ci,
    #                    cl=cl,
    #                    message_indent=message_indent,
    #                    verbose=verbose)
    # 
    # # Concatenate lists.
    # roc_data <- list("model_data"=rbind_list_list(roc_data, "model_data"),
    #                  "intervention_data"=rbind_list_list(roc_data, "intervention_data"),
    #                  "bootstrap_data"=rbind_list_list(roc_data, "bootstrap_data"),
    #                  "confidence_level"=confidence_level)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }

  if(determine_ci & aggregate_ci){
    
    # Aggregate the data by computing the bootstrap confidence intervals.
    roc_data$model_data <- .compute_bootstrap_ci(x0=roc_data$model_data,
                                                 xb=roc_data$bootstrap_data,
                                                 target_column="tpr",
                                                 bootstrap_ci_method=bootstrap_ci_method,
                                                 additional_splitting_variable="fpr",
                                                 confidence_level=confidence_level,
                                                 cl=cl,
                                                 verbose=verbose,
                                                 message_indent=message_indent)
    
    # Set the bootstrap_data to NULL.
    roc_data$bootstrap_data <- NULL
    
  } else if(determine_ci){
    # Add the bootstrap confidence interval method
    roc_data$bootstrap_ci_method <- bootstrap_ci_method
  }
  
  return(roc_data)
}


.compute_roc_data_categorical <- function(positive_class, data, object, confidence_level,
                                          determine_ci=FALSE, cl=NULL, verbose=FALSE,
                                          message_indent=0L){
  
  # Check if the data has more than 1 row.
  if(nrow(data) <= 1) return(list("confidence_level"=confidence_level))
  
  # Set test probabilities
  if(determine_ci){
    x_values <- seq(from=0.00, to=1.00, by=0.005)
    
  } else {
    x_values <- NULL
  }
  
  # Compute data from the model.
  model_data <- .compute_roc_data_categorical_model(data=data,
                                                    x=x_values,
                                                    positive_class=positive_class)
  
  if(determine_ci){
    if(verbose) logger.message(paste0("Computing bootstrap confidence interval data for the \"", positive_class, "\" class."),
                               indent=message_indent)
    
    # Bootstrap the data.
    bootstrap_data <- bootstrapper(data=data,
                                   alpha= 1.0 - confidence_level,
                                   FUN=.compute_roc_data_categorical_model,
                                   positive_class=positive_class,
                                   x=x_values,
                                   cl=cl,
                                   verbose=verbose)
    
  } else {
    bootstrap_data <- NULL
  }
  
  # Add to list and return
  return(list("model_data"=model_data,
              "bootstrap_data"=bootstrap_data,
              "confidence_level"=confidence_level))
}


.compute_roc_data_categorical_model <- function(data, x, positive_class){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- probability <- is_positive <- n_true_positive <- n_false_positive <- tpr <- NULL
  
  # Make a local copy
  data <- data.table::copy(data)
  data.table::setnames(data, old=get_class_probability_name(positive_class), new="probability")
  
  # Determine positive output.
  data[, "is_positive":=outcome == positive_class]
  
  # Keep only probability and is_positive
  data <- data[, c("probability", "is_positive")]
  
  # Order by inverse probability.
  data <- data[order(-probability)]
  
  # Determine the number of true and false positives.
  data[, ":="("n_true_positive"=cumsum(is_positive),
              "n_false_positive"=cumsum(!is_positive))]
  
  # Determine the total number of positive and negative classes.
  n_max_true_positive <- max(data$n_true_positive)
  n_max_false_positive <- max(data$n_false_positive)
  
  # Compute TPR (sensitivity) and FPR (1-specificity)
  if(n_max_true_positive > 0){
    data[, "tpr":=n_true_positive / n_max_true_positive]
    
  } else {
    data[, "tpr":=0.0]
  }
  
  if(n_max_false_positive > 0){
    data[, "fpr":=n_false_positive / n_max_false_positive]
    
  } else {
    data[, "fpr":=0.0]
  }
  
  # Maintain only unique FPR values, and keep the highest TPR at each value.
  data <- data[, list("tpr"=max(tpr)), by="fpr"]
  
  # If no bootstraps are performed, the x input argument will be NULL. Instead
  # the actual false positive rate values will be used.
  if(is.null(x)){
    x <- data$fpr
    
    # Check if the start point fpr=0.0 is included in x.
    if(!0.0 %in% x) x <- c(0.0, x)
    
    # Check if the end point fpr=1.0 is included in x.
    if(!1.0 %in% x) x <- c(x, 1.0)
  } 
  
  # Interpolate at the values in x.
  y <- suppressWarnings(stats::approx(x=data$fpr,
                                      y=data$tpr,
                                      xout=x,
                                      yleft=0.0,
                                      yright=1.0,
                                      method="constant")$y)
  
  # Store data.
  model_data <- data.table::data.table("pos_class"=positive_class,
                                       "fpr"=x,
                                       "tpr"=y)
  
  return(model_data)
}
