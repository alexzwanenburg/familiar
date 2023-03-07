#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarDataElementConfusionMatrix object ------------------------------------

setClass(
  "familiarDataElementConfusionMatrix",
  contains = "familiarDataElement",
  prototype = methods::prototype(
    value_column = "count",
    grouping_column = c("observed_outcome", "expected_outcome")))



# extract_confusion_matrix (generic) -------------------------------------------

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
setGeneric(
  "extract_confusion_matrix",
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    detail_level = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...) {
    standardGeneric("extract_confusion_matrix")
  } 
)



# extract_confusion_matrix (familiarEnsemble) ----------------------------------
setMethod(
  "extract_confusion_matrix",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    detail_level = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE) {
    
    # Don't compute a confusion matrix if there is nothing to be computed.
    if (!object@outcome_type %in% c("binomial", "multinomial")) return(NULL)
    
    # Message extraction start
    logger.message(
      paste0("Computing confusion matrix."),
      indent = message_indent,
      verbose = verbose)
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "ensemble",
      data_element = "confusion_matrix")
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "familiarDataElementConfusionMatrix",
      detail_level = detail_level,
      estimation_type = "point")
    
    # Generate elements to send to dispatch.
    confusion_matrix_data <- extract_dispatcher(
      FUN = .extract_confusion_matrix,
      has_internal_bootstrap = FALSE,
      cl = cl,
      object = object,
      data = data,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      aggregate_results = TRUE,
      message_indent = message_indent + 1L,
      verbose = verbose)
    
    return(confusion_matrix_data)
  }
)



.extract_confusion_matrix <- function(
    object,
    proto_data_element,
    data,
    is_pre_processed,
    ensemble_method,
    verbose,
    message_indent = 0L,
    ...) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  count <- NULL
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  if (object@outcome_type %in% c("binomial", "multinomial")) {
    # Iterate over outcome classes.
    
    # Add model name.
    data_element <- add_model_name(proto_data_element, object = object)
    
    # Predict class probabilities.
    prediction_data <- .predict(
      object = object,
      data = data,
      ensemble_method = ensemble_method,
      is_pre_processed = is_pre_processed)
    
    if (!all_predictions_valid(
      prediction_table = prediction_data,
      outcome_type = object@outcome_type)) {
      return(NULL)
    }
    
    # Remove data with missing outcomes.
    prediction_data <- remove_missing_outcomes(
      data = prediction_data,
      outcome_type = object@outcome_type)
    
    # Check that any prediction data remain.
    if (is_empty(prediction_data)) return(NULL)
    
    # Make a local copy with only the required data
    data <- prediction_data[, c("outcome", "predicted_class")]
    
    # Rename outcome columns
    data.table::setnames(
      x = data,
      old = c("outcome", "predicted_class"),
      new = c("observed_outcome", "expected_outcome"))
    
    # Sum pairs of observed and expected outcome categories.
    data <- data[
      , list("count" = .N),
      by = c("observed_outcome", "expected_outcome")]
    
    # Find class levels in the data
    class_levels <- get_outcome_class_levels(object)
    
    # Construct an empty matrix 
    empty_matrix <- data.table::data.table(expand.grid(
      list(
        "observed_outcome" = class_levels,
        "expected_outcome" = class_levels),
      stringsAsFactors = FALSE))
    empty_matrix[, "count" := 0L]
    
    # Combine data with the empty matrix to add in combinations that appear 0
    # times.
    data <- data.table::rbindlist(
      list(data, empty_matrix),
      use.names = TRUE)
    
    # Use a max operation to remove any combinations that appear twice in the
    # table.
    data <- data[
      , list("count" = max(count)),
      by = c("observed_outcome", "expected_outcome")]
    
    # Set data element.
    data_element@data <- data
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  return(data_element)
}



# export_confusion_matrix_data (generic) ---------------------------------------

#'@title Extract and export confusion matrices.
#'
#'@description Extract and export confusion matrics for models in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams plot_univariate_importance
#'
#'@inheritDotParams extract_confusion_matrix
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
#'  Confusion matrices are exported for individual and ensemble models.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_confusion_matrix_data
#'@md
#'@rdname export_confusion_matrix_data-methods
setGeneric(
  "export_confusion_matrix_data",
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_confusion_matrix_data")
  }
)



# export_confusion_matrix_data (collection) ------------------------------------

#'@rdname export_confusion_matrix_data-methods
setMethod(
  "export_confusion_matrix_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(.export(
      x = object,
      data_slot = "confusion_matrix",
      dir_path = dir_path,
      aggregate_results = TRUE,
      type = "performance",
      subtype = "confusion_matrix",
      export_collection = export_collection))
  }
)



# export_confusion_matrix_data (general) ---------------------------------------

#'@rdname export_confusion_matrix_data-methods
setMethod(
  "export_confusion_matrix_data",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "confusion_matrix"),
        list(...)))
    
    return(do.call(
      export_confusion_matrix_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "export_collection" = export_collection),
        list(...))))
  }
)
