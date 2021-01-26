#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementAUCCurve",
         contains="familiarDataElement",
         prototype = methods::prototype(value_column="y",
                                        grouping_column="x"))

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
                                        detail_level=waiver(),
                                        estimation_type=waiver(),
                                        aggregate_results=waiver(),
                                        confidence_level=waiver(),
                                        bootstrap_ci_method=waiver(),
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
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
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
            
            # Check the level detail
            if(is.waive(detail_level)) detail_level <- object@settings$detail_level
            
            .check_parameter_value_is_valid(x=detail_level, var_name="detail_level",
                                            values=c("ensemble", "hybrid", "model"))
            
            # Check the estimation type
            if(is.waive(estimation_type)) estimation_type <- object@settings$estimation_type
            
            .check_parameter_value_is_valid(x=estimation_type, var_name="estimation_type",
                                            values=c("point", "bias_correction", "bc", "bootstrap_confidence_interval", "bci"))
            
            # Check whether results should be aggregated.
            if(is.waive(aggregate_results)) aggregate_results <- object@settings$aggregate_results
            
            aggregate_results <- tolower(aggregate_results)
            .check_parameter_value_is_valid(x=aggregate_results, var_name="aggregate_results",
                                            values=c(.get_available_data_elements(confidence_interval_only=TRUE),
                                                     "true", "false", "none", "all", "default"))
            # Set as TRUE/FALSE
            aggregate_results <- any(aggregate_results %in% c("true", "all", "auc_data"))
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementAUCCurve",
                                      detail_level = detail_level,
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method)
            
            # Generate elements to send to dispatch.
            roc_data <- extract_dispatcher(FUN=.extract_roc_curve_data,
                                           has_internal_bootstrap=TRUE,
                                           cl=cl,
                                           object=object,
                                           data=data,
                                           proto_data_element=proto_data_element,
                                           is_pre_processed=is_pre_processed,
                                           ensemble_method=ensemble_method,
                                           aggregate_results=aggregate_results,
                                           message_indent=message_indent + 1L,
                                           verbose=verbose)
            
            return(roc_data)
          })



.extract_roc_curve_data <- function(object,
                                    data,
                                    proto_data_element,
                                    cl=NULL,
                                    ensemble_method,
                                    is_pre_processed,
                                    ...){
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  if(object@outcome_type %in% c("binomial", "multinomial")){
    # Iterate over outcome classes.
    
    # Predict class probabilities.
    prediction_data <- .predict(object=object,
                                data=data,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
    
    # Check if any predictions are valid.
    if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
    
    # Determine class levels
    outcome_class_levels <- get_outcome_class_levels(object)
    
    # Select only one outcome type for binomial outcomes.
    if(object@outcome_type == "binomial") outcome_class_levels <- outcome_class_levels[2]
    
    # Add positive class as an identifier.
    data_elements <- add_data_element_identifier(x=proto_data_element,
                                                 positive_class=outcome_class_levels)
    
    # Compute ROC data.
    roc_data <- lapply(data_elements,
                       .compute_auc_data_categorical,
                       data=prediction_data,
                       cl=cl,
                       ...)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  return(roc_data)
}


.compute_auc_data_categorical <- function(data_element,
                                          data,
                                          aggregate_results,
                                          cl=NULL,
                                          progress_bar=FALSE,
                                          verbose=FALSE,
                                          message_indent=0L,
                                          ...){
  
  # Check if the data has more than 1 row.
  if(nrow(data) <= 1) return(NULL)
  
  if(length(data_element@identifiers$positive_class) > 1 & progress_bar){
    logger.message(paste0("Computing ROC and Precision-Recall curves for the \"", data_element@identifiers$positive_class, "\" class."),
                   indent=message_indent)
  }
  
  # Set test probabilities
  threshold_probabilities <- seq(from=0.000, to=1.000, by=0.005)
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x=data_element,
                                               ...)
  
  # Iterate over elements.
  data_elements <- fam_mapply(cl=cl,
                              assign=NULL,
                              FUN=..compute_auc_data_categorical,
                              data_element=bootstrap_data$data_element,
                              bootstrap=bootstrap_data$bootstrap,
                              bootstrap_seed = bootstrap_data$seed,
                              MoreArgs=list("data"=data,
                                            "x"=threshold_probabilities),
                              progress_bar=progress_bar)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}


..compute_auc_data_categorical <- function(data_element,
                                           data,
                                           x,
                                           bootstrap,
                                           bootstrap_seed){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- probability <- ppv <- tpr <- is_positive <- NULL
  n_true_positive <- n_false_positive <- NULL
  
  # Get the positive class.
  positive_class <- data_element@identifiers$positive_class
  
  # Bootstrap the data.
  if(bootstrap) data <- get_bootstrap_sample(data=data,
                                             seed=bootstrap_seed)
  
  # Make a local copy
  data <- data.table::copy(data)
  data.table::setnames(data,
                       old=get_class_probability_name(positive_class),
                       new="probability")

  # Determine the number of positive and negative outcomes.
  n_positive <- sum(data$outcome == positive_class)
  n_negative <- sum(data$outcome != positive_class)
  
  # Determine whether the observed outcome was positive.
  data[, "is_positive":=outcome == positive_class]
  
  # Keep only probability and is_positive
  data <- data[, c("probability", "is_positive")]
    
  # Order by inverse probability.
  data <- data[order(-probability)]
  
  # Determine the number of true and false positives.
  data[, ":="("n_true_positive"=cumsum(is_positive),
              "n_false_positive"=cumsum(!is_positive),
              "n_true_negative"=n_negative - cumsum(!is_positive),
              "n_false_negative"=n_positive - cumsum(is_positive))]
  
  # Compute TPR / recall (sensitivity)
  if(n_positive > 0){
    data[, "tpr":=n_true_positive / n_positive]
    
  } else {
    data[, "tpr":=0.0]
  }
  
  # Compute FPR (1-specificity)
  if(max(data$n_false_positive) > 0){
    data[, "fpr":=n_false_positive / n_negative]
    
  } else {
    data[, "fpr":=0.0]
  }
  
  # Compute precision / positive predictive value
  if(max(data$n_true_positive + data$n_false_positive) > 0){
    data[, "ppv":=n_true_positive / (n_true_positive + n_false_positive)]
    
  } else {
    data[, "ppv":=0.0]
  }
  
  # Prepare AUC-ROC (TPR as function of FPR)
  data_auc_roc <- data[, list("tpr"=max(tpr)), by="fpr"]
  
  # Interpolate at the values in x.
  y_auc_roc <- suppressWarnings(stats::approx(x=data_auc_roc$fpr,
                                              y=data_auc_roc$tpr,
                                              xout=x,
                                              yleft=0.0,
                                              yright=1.0,
                                              method="constant")$y)
  
  # Copy the data element as prototype for ROC curve data.
  data_element_roc <- data_element
  
  # Set ROC curve data.
  data_element_roc@data <- data.table::data.table("x"=x,
                                                  "y"=y_auc_roc)
  
  # Add curve type as an identifier.
  data_element_roc <- add_data_element_identifier(data_element_roc,
                                                  curve_type="roc")
  
  # Prepare AUC-PC (precision as a function of TPR (recall))
  data_auc_pr <- data[, list("ppv"=max(ppv)), by="tpr"]
  
  # Interpolate at the values in x.
  y_auc_pr <- suppressWarnings(stats::approx(x=data_auc_pr$tpr,
                                             y=data_auc_pr$ppv,
                                             xout=x,
                                             yleft=1.0,
                                             yright=0.0,
                                             method="constant")$y)
  
  # Copy the data element as prototype for precisions-recall curve data.
  data_element_pr <- data_element
  
  # Set PR curve data.
  data_element_pr@data <- data.table::data.table("x"=x,
                                                 "y"=y_auc_pr)
  
  # Add curve type as an identifier.
  data_element_pr <- add_data_element_identifier(data_element_pr,
                                                 curve_type="pr")
  
  return(c(data_element_roc,
           data_element_pr))
}




#####export_auc_data#####

#'@title Extract and export ROC and Precision-Recall curves.
#'
#'@description Extract and export ROC and Precision-Recall curves for models in
#'  a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_auc_data
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
#'  ROC curve data are exported for individual and ensemble models. For ensemble
#'  models, a credibility interval for the ROC curve is determined using
#'  bootstrapping for each metric. In case of multinomial outcomes, ROC-curves
#'  are computed for each class, using a one-against-all approach.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_auc_data
#'@md
#'@rdname export_auc_data-methods
setGeneric("export_auc_data", function(object, dir_path=NULL, aggregate_results=TRUE, ...) standardGeneric("export_auc_data"))

#####export_auc_data (collection)#####

#'@rdname export_auc_data-methods
setMethod("export_auc_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            return(.export(x=object,
                           data_slot="auc_data",
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           main_type="performance",
                           subtype="auc_curves"))
          })

#####export_auc_data (generic)#####

#'@rdname export_auc_data-methods
setMethod("export_auc_data", signature(object="ANY"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="auc_data",
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_auc_data,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results),
                                  list(...))))
          })
