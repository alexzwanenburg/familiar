#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarDataElementFeatureExpression object ----------------------------------
setClass(
  "familiarDataElementFeatureExpression",
  contains = "familiarDataElement",
  slots = list(
    "feature_info" = "ANY",
    "evaluation_time" = "ANY"),
  prototype = methods::prototype(
    detail_level = "ensemble",
    estimation_type = "point",
    feature_info = NULL,
    evaluation_time = NULL))


# extract_feature_expression (generic) -----------------------------------------

#'@title Internal function to extract feature expressions.
#'
#'@description Computes and extracts feature expressions for features
#'  used in a `familiarEnsemble` object.
#'
#'@param feature_similarity Table containing pairwise distance between
#'  sample. This is used to determine cluster information, and indicate which
#'  samples are similar. The table is created by the
#'  `extract_sample_similarity` method.
#'@inheritParams .extract_data
#'
#'@return A list with a data.table containing feature expressions.
#'@md
#'@keywords internal
setGeneric(
  "extract_feature_expression",
  function(
    object,
    data,
    feature_similarity,
    sample_similarity,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_similarity_metric = waiver(),
    sample_cluster_method = waiver(),
    sample_linkage_method = waiver(),
    sample_similarity_metric = waiver(),
    evaluation_times = waiver(),
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    standardGeneric("extract_feature_expression")
  }
)



# extract_feature_expression (familiarEnsemble) --------------------------------
setMethod(
  "extract_feature_expression",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    feature_similarity,
    sample_similarity,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_similarity_metric = waiver(),
    sample_cluster_method = waiver(),
    sample_linkage_method = waiver(),
    sample_similarity_metric = waiver(),
    evaluation_times = waiver(),
    message_indent = 0L,
    verbose = FALSE
  ) {
    
    # Message extraction start
    logger_message(
      paste0("Compute feature expression."),
      indent = message_indent,
      verbose = verbose)
    
    # Obtain evaluation times from the data.
    if (is.waive(evaluation_times) &&
        object@outcome_type %in% c("survival", "competing_risk")) {
      evaluation_times <- object@settings$eval_times
      
    } else if (is.waive(evaluation_times)) {
      evaluation_times <- NULL
    }
    
    # Check if evaluation_times is correct.
    if (object@outcome_type %in% c("survival", "competing_risk")) {
      sapply(
        evaluation_times,
        .check_number_in_valid_range,
        var_name = "evaluation_times",
        range = c(0.0, Inf),
        closed = c(FALSE, TRUE))
    }
    
    # Aggregate data
    data <- aggregate_data(data = data)
    
    # Retrieve input data. Note that we batch_normalisation is taken into
    # account even though the data are converted back to their original scale to
    # derive expressions.
    data <- process_input_data(
      object = object,
      data = data,
      stop_at = "batch_normalisation")
    
    if (is_empty(data)) return(NULL)
    
    # Determine signature features
    model_features <- object@model_features
    
    # Maintain only important features. The current set is based on the
    # important features of the model, i.e. those that end up in the
    # model (potentially as a cluster).
    expression_data <- filter_features(
      data = data,
      available_features = model_features)
    
    # Perform inverse normalisation
    expression_data <- normalise_features(
      data = expression_data,
      feature_info_list = object@feature_info,
      invert = TRUE)
    
    # Perform inverse transformation
    expression_data <- transform_features(
      data = expression_data,
      feature_info_list = object@feature_info,
      invert = TRUE)
    
    # Add sample_name to expression_data
    row_names <- get_unique_row_names(expression_data)
    expression_data@data[, ":="(
      "sample_name" = row_names,
      "batch_id" = NULL,
      "sample_id" = NULL,
      "series_id" = NULL,
      "repetition_id" = NULL)]
    
    # Set expression data.
    expression_data <- methods::new(
      "familiarDataElementFeatureExpression",
      data = expression_data@data,
      feature_info = object@feature_info[model_features],
      evaluation_time = evaluation_times,
      value_column = model_features)
    
    # Add model name.
    expression_data <- add_model_name(expression_data, object)
    
    return(list(expression_data))
  }
)



# extract_feature_expression (prediction table) --------------------------------
setMethod(
  "extract_feature_expression",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, ...) {
    ..warning_no_data_extraction_from_prediction_table("feature expression")
    
    return(NULL)
  }
)



# export_feature_expressions (generic) -----------------------------------------

#'@title Extract and export feature expressions.
#'
#'@description Extract and export feature expressions for the features in a
#'  familiarCollection.
#'
#'@param evaluation_time One or more time points that are used to create the
#'  outcome columns in expression plots. If not provided explicitly, this
#'  parameter is read from settings used at creation of the underlying
#'  `familiarData` objects. Only used for `survival` outcomes.
#'
#'@inheritParams export_all
#'@inheritParams plot_univariate_importance
#'
#'@inheritDotParams extract_feature_expression
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
#'  Feature expressions are computed by standardising each feature, i.e. sample
#'  mean is 0 and standard deviation is 1.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_feature_expressions
#'@md
#'@rdname export_feature_expressions-methods
setGeneric(
  "export_feature_expressions",
  function(
    object,
    dir_path = NULL,
    evaluation_time = waiver(),
    export_collection = FALSE,
    ...) {
    standardGeneric("export_feature_expressions")
  }
)



# export_feature_expressions (collection) --------------------------------------

#'@rdname export_feature_expressions-methods
setMethod(
  "export_feature_expressions",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    evaluation_time = waiver(),
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Extract data.
    x <- object@feature_expressions
    
    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)
    
    if (!is.waive(evaluation_time)) {
      
      # Check values.
      sapply(
        evaluation_time,
        .check_number_in_valid_range,
        var_name = "evaluation_time",
        range = c(0, Inf))
      
      # Set clustering method.
      x <- lapply(
        x,
        function(x, evaluation_time) {
          x@evaluation_time <- evaluation_time
          return(x)
        },
        evaluation_time = evaluation_time)
    }
    
    return(.export(
      x = object,
      data_elements = x,
      dir_path = dir_path,
      aggregate_results = FALSE,
      type = "feature_expression",
      subtype = NULL,
      export_collection = export_collection))
  }
)



# export_feature_expressions (general) -----------------------------------------

#'@rdname export_feature_expressions-methods
setMethod(
  "export_feature_expressions",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    evaluation_time = waiver(),
    export_collection = FALSE,
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "feature_expressions",
          "evaluation_times" = evaluation_time),
        list(...)))
    
    return(do.call(
      export_feature_expressions,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "evaluation_time" = evaluation_time,
          "export_collection" = export_collection),
        list(...))))
  }
)



# .export (familiarDataElementFeatureExpression) -------------------------------
setMethod(
  ".export",
  signature(x = "familiarDataElementFeatureExpression"),
  function(
    x,
    x_list,
    aggregate_results = FALSE,
    ...) {
    
    # Add grouping columns to data. Note that we do not merge the data elements.
    x <- lapply(
      x_list,
      .identifier_as_data_attribute,
      identifier = "all",
      as_grouping_column = TRUE)
    
    return(x)
  }
)
