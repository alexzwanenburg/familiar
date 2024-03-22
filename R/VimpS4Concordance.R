#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass(
  "familiarConcordanceVimp",
  contains = "familiarVimpMethod"
)


.get_available_concordance_vimp_method <- function(show_general = TRUE) {
  return("concordance")
}


# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarConcordanceVimp"),
  function(object, ...) {
    
    if (object@outcome_type == "count") {
      ..deprecation_count()
      return(FALSE)
    }
    
    return(TRUE)
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarConcordanceVimp"),
  function(object, data = NULL, ...) {
    return(list())
  }
)



# ..vimp -----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarConcordanceVimp"),
  function(object, data, ...) {
    
    if (is_empty(data)) return(callNextMethod())

    # Use concordance-based measures for variable importance:
    # - Gini index for binomial and multinomial outcomes
    # - Kendall's Tau for continuous and counts outcomes
    # - Concordance index for survival features

    if (object@outcome_type %in% c("binomial", "multinomial")) {
      # Compute gini index for categorical outcomes.

      # Create a new vimp object, and replace the vimp_method slot.
      new_vimp_object <- methods::new("familiarCoreLearnGiniVimp", object)
      new_vimp_object@vimp_method <- "gini"

      return(..vimp(
        object = new_vimp_object,
        data = data
      ))
      
    } else if (object@outcome_type %in% c("continuous")) {
      # For continuous outcomes use kendall's tau.

      # Create a new vimp object, and replace the vimp_method slot.
      new_vimp_object <- methods::new("familiarCorrelationVimp", object)
      new_vimp_object@vimp_method <- "kendall"

      return(..vimp(
        object = new_vimp_object,
        data = data
      ))
      
    } else if (object@outcome_type == "survival") {
      # Compute the concordance index for each feature.

      # Use effect coding to convert categorical data into encoded data - this
      # is required to deal with factors with missing/new levels between
      # training and test data sets.
      encoded_data <- encode_categorical_variables(
        data = data,
        object = object,
        encoding_method = "dummy",
        drop_levels = FALSE
      )

      # Find feature columns in the data.
      feature_columns <- get_feature_columns(x = encoded_data$encoded_data)

      # Compute concordance indices
      c_index <- sapply(
        encoded_data$encoded_data@data[, mget(feature_columns)],
        ..compute_concordance_index,
        time = encoded_data$encoded_data@data$outcome_time,
        event = encoded_data$encoded_data@data$outcome_event
      )

      # Create variable importance object.
      vimp_object <- methods::new("vimpTable",
        vimp_table = data.table::data.table(
          "score" = abs(c_index - 0.5),
          "name" = feature_columns
        ),
        encoding_table = encoded_data$reference_table,
        score_aggregation = "max",
        invert = TRUE
      )

      return(vimp_object)
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }
  }
)
