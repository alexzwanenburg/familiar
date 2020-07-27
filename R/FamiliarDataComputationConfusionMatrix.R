#'@title Internal function to extract the confusion matrix.
#'
#'@description Computes and extracts the confusion matrix for predicted and
#'  observed categorical outcomes used in a `familiarEnsemble` object.
#'
#'@inheritParams extract_data
#'
#'@return A data.table containing predicted and observed outcome data together
#'  with a co-occurence count.
#'@md
#'@keywords internal
setGeneric("extract_confusion_matrix", function(object,
                                                data,
                                                cl=NULL,
                                                is_pre_processed=FALSE,
                                                ensemble_method=waiver(),
                                                message_indent=0L,
                                                verbose=FALSE,
                                                ...) standardGeneric("extract_confusion_matrix"))

#####extract_confusion_matrix#####
setMethod("extract_confusion_matrix", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   is_pre_processed=FALSE,
                   ensemble_method=waiver(),
                   message_indent=0L,
                   verbose=FALSE){
            
            # Don't compute a confusion matrix if there is nothing to be computed.
            if(!object@outcome_type %in% c("binomial", "multinomial")) return(NULL)
       
            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing confusion matrix."),
                             indent=message_indent)
            }
            
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Extract data for the individual models and the ensemble.
            confusion_matrix_data <- universal_extractor(object=object,
                                                         cl=cl,
                                                         FUN=.extract_confusion_matrix,
                                                         individual_model_ci=FALSE,
                                                         data=data,
                                                         ensemble_method=ensemble_method,
                                                         is_pre_processed=is_pre_processed,
                                                         message_indent=message_indent + 1L,
                                                         verbose=verbose)
            
            return(confusion_matrix_data)
          })



.extract_confusion_matrix <- function(object, data, cl=NULL, is_pre_processed,
                                      determine_ci=FALSE,
                                      ensemble_method, verbose, message_indent=0L){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- count <- NULL
  
  if(object@outcome_type %in% c("binomial", "multinomial")){
    # Iterate over outcome classes.
    
    # Predict class probabilities.
    prediction_data <- .predict(object=object,
                                data=data,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
    
    if(!any_predictions_valid(prediction_table=prediction_data, outcome_type=object@outcome_type)) return(NULL)
    
    # Make a local copy with only the required data
    data <- prediction_data[!is.na(outcome), c("outcome", "predicted_class")]
    
    # Rename outcome columns
    data.table::setnames(data,
                         old=c("outcome", "predicted_class"),
                         new=c("observed_outcome", "expected_outcome"))
    
    # Sum pairs of observed and expected outcome categories.
    data <- data[, list("count"=.N), by=c("observed_outcome", "expected_outcome")]
    
    # Find class levels in the data
    class_levels <- get_outcome_class_levels(object)
    
    # Construct an empty matrix 
    empty_matrix <- data.table::data.table(expand.grid(list("observed_outcome"=class_levels, "expected_outcome"=class_levels), stringsAsFactors=FALSE))
    empty_matrix[, "count":=0L]
    
    # Combine data with the empty matrix to add in combinations that
    # appear 0 times.
    data <- data.table::rbindlist(list(data, empty_matrix), use.names=TRUE)
    
    # Use a max operation to remove any combinations that appear twice in the table.
    data <- data[, list("count"=max(count)), by=c("observed_outcome", "expected_outcome")]
    
    return(list("model_data"=data))
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
}
