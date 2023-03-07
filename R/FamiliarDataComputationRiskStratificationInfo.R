#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarDataElementRiskStratificationInfo object -----------------------------
setClass(
  "familiarDataElementRiskStratificationInfo",
  contains = "familiarDataElement",
  prototype = methods::prototype(estimation_type = "point"))

# extract_risk_stratification_info (generic) -----------------------------------

#'@title Internal function to extract risk stratification info from data.
#'
#'@description Collects risk stratification information.
#'
#'@inheritParams extract_data
#'
#'@return A list of familiarDataElements with risk stratification information.
#'@md
#'@keywords internal
setGeneric(
  "extract_risk_stratification_info",
  function(
    object,
    detail_level = waiver(),
    message_indent = 0L,
    verbose = FALSE,
    ...) {
    standardGeneric("extract_risk_stratification_info")
  }
)



# extract_risk_stratification_info (familiarEnsemble) --------------------------
setMethod(
  "extract_risk_stratification_info",
  signature(object = "familiarEnsemble"),
  function(
    object,
    detail_level = waiver(),
    message_indent = 0L,
    verbose = FALSE) {
    
    # Test if the outcome type is survival. Other outcome types do not 
    if (!object@outcome_type %in% c("survival")) return(NULL)
    
    # Message extraction start
    logger.message(
      paste0("Extracting stratification information."),
      indent = message_indent,
      verbose = verbose)
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "hybrid",
      data_element = "risk_stratification_info")
    
    proto_data_element <- methods::new(
      "familiarDataElementRiskStratificationInfo",
      detail_level = detail_level)
    
    # Generate elements to send to dispatch.
    stratification_info <- extract_dispatcher(
      FUN = .extract_risk_stratification_info,
      cl = NULL,
      has_internal_bootstrap = FALSE,
      object = object,
      proto_data_element = proto_data_element,
      aggregate_results = FALSE,
      message_indent = message_indent + 1L,
      verbose = verbose)
    
    return(stratification_info)
  }
)



.extract_risk_stratification_info <- function(
    object,
    proto_data_element,
    ...) {
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  data_element <- add_model_name(proto_data_element, object = object)
  
  # Test if the any of the models in the ensemble were trained.
  if (!model_is_trained(object)) return(NULL)
  
  if (is(object, "familiarModel")) {
    # Iterate over stratification parameters
    data <- lapply(
      object@km_info$parameters,
      function(x) {
        # Extract risk stratification information.
        data <- data.table::data.table(
          "stratification_method" = x$method,
          "cutoff" = x$cutoff,
          "group_id" = seq_len(length(x$cutoff)))
        
        return(data)
      })
    
    # Combine to single list
    data <- data.table::rbindlist(data, use.names = TRUE)
    
    # Check that any data is available..
    if (is_empty(data)) return(NULL)
    
  } else {
    # Compute risk stratification data.
    risk_stratification_data <- extract_risk_stratification_info(
      object = object,
      detail_level = "hybrid",
      verbose = FALSE)
    
    risk_stratification_data <- .compute_data_element_estimates(risk_stratification_data)
    
    if (is_empty(risk_stratification_data)) return(NULL)
    
    # Extract data.
    data <- risk_stratification_data[[1]]@data
  }
  
  # Attach data to the corresponding attribute.
  data_element@data <- data
  
  # Set value columns
  data_element@value_column <- c("cutoff")
  
  # Set grouping columns
  data_element@grouping_column <- c("stratification_method", "group_id")
  
  return(data_element)
}



# export_risk_stratification_info (generic) ------------------------------------ 

#'@title Extract and export cut-off values for risk group stratification.
#'
#'@description Extract and export cut-off values for risk group stratification
#'  by models in a familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
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
#'  Stratification cut-off values are determined when creating a model, using
#'  one of several methods set by the `stratification_method` parameter. These
#'  values are then used to stratify samples in any new dataset. The available
#'  methods are:
#'
#'  * `median` (default): The median predicted value in the development cohort
#'  is used to stratify the samples into two risk groups.
#'
#'  * `fixed`: Samples are stratified based on the sample quantiles of the
#'  predicted values. These quantiles are defined using the
#'  `stratification_threshold` parameter.
#'
#'  * `optimised`: Use maximally selected rank statistics to determine the
#'  optimal threshold (Lausen and Schumacher, 1992; Hothorn et al., 2003) to
#'  stratify samples into two optimally separated risk groups.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@references 1. Lausen, B. & Schumacher, M. Maximally Selected Rank Statistics.
#'  Biometrics 48, 73 (1992).
#'
#'  1. Hothorn, T. & Lausen, B. On the exact distribution of maximally selected
#'  rank statistics. Comput. Stat. Data Anal. 43, 121–137 (2003).
#'@exportMethod export_risk_stratification_info
#'@md
#'@rdname export_risk_stratification_info-methods
setGeneric(
  "export_risk_stratification_info",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_risk_stratification_info")
  }
)



# export_risk_stratification_info (collection) ---------------------------------

#'@rdname export_risk_stratification_info-methods
setMethod(
  "export_risk_stratification_info",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(.export(
      x = object,
      data_slot = "km_info",
      dir_path = dir_path,
      aggregate_results = aggregate_results,
      type = "stratification",
      export_collection = export_collection))
  }
)



# export_risk_stratification_info (general) ------------------------------------

#'@rdname export_risk_stratification_info-methods
setMethod(
  "export_risk_stratification_info",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "risk_stratification_info"),
        list(...)))
    
    return(do.call(
      export_risk_stratification_info,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_resuls" = aggregate_results,
          "export_collection" = export_collection),
        list(...))))
  }
)
