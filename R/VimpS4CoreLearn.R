#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarCoreLearnVimp",
  contains = "familiarVimpMethod"
)

setClass(
  "familiarCoreLearnGiniVimp",
  contains = "familiarCoreLearnVimp")

setClass(
  "familiarCoreLearnMDLVimp",
  contains = "familiarCoreLearnVimp")

setClass(
  "familiarCoreLearnRelieffExpRankVimp",
  contains = "familiarCoreLearnVimp")

setClass(
  "familiarCoreLearnGainRatioVimp",
  contains = "familiarCoreLearnVimp")


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarCoreLearnVimp"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Update package
    .Object@package <- "CORElearn"

    return(.Object)
  }
)


.get_available_corelearn_gini_vimp_method <- function(show_general = TRUE) {
  return("gini")
}

.get_available_corelearn_mdl_vimp_method <- function(show_general = TRUE) {
  return("mdl")
}

.get_available_corelearn_relieff_exp_rank_vimp_method <- function(show_general = TRUE) {
  return("relieff_exp_rank")
}

.get_available_corelearn_gain_ratio_vimp_method <- function(show_general = TRUE) {
  return("gain_ratio")
}



# is_available (gini) ----------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnGiniVimp"),
  function(object, ...) {
    return(object@outcome_type %in% c("binomial", "multinomial"))
  }
)

# is_available (mdl) -----------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnMDLVimp"),
  function(object, ...) {
    return(object@outcome_type %in% c("binomial", "multinomial"))
  }
)

# is_available (relieff) -------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnRelieffExpRankVimp"),
  function(object, ...) {
    
    if (object@outcome_type == "count") {
      ..deprecation_count()
      return(FALSE)
    }
    
    return(object@outcome_type %in% c("binomial", "multinomial", "continuous"))
  }
)

# is_available (gain ratio) ----------------------------------------------------
setMethod(
  "is_available", 
  signature(object = "familiarCoreLearnGainRatioVimp"),
  function(object, ...) {
    return(object@outcome_type %in% c("binomial", "multinomial"))
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarCoreLearnVimp"),
  function(object, data = NULL, ...) {
    return(list())
  }
)



# ..vimp -----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarCoreLearnVimp"),
  function(object, data, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    score <- NULL

    if (is_empty(data)) return(callNextMethod())

    # Check that required packages are loaded and installed.
    require_package(object, "vimp")

    # Identify feature columns.
    feature_columns <- get_feature_columns(data)

    # Generate a formula.
    formula <- stats::reformulate(feature_columns, response = quote(outcome))

    if (is(object, "familiarCoreLearnGiniVimp")) {
      # Gini measure.
      score <- CORElearn::attrEval(
        formula, 
        data = data@data, 
        estimator = "Gini")
      
    } else if (is(object, "familiarCoreLearnMDLVimp")) {
      # MDL method.
      score <- CORElearn::attrEval(
        formula, 
        data = data@data, 
        estimator = "MDL")
      
    } else if (is(object, "familiarCoreLearnRelieffExpRankVimp")) {
      if (object@outcome_type %in% c("continuous")) {
        # RReliefFexpRank method.
        score <- CORElearn::attrEval(
          formula, 
          data = data@data, 
          estimator = "RReliefFexpRank")
        
      } else if (object@outcome_type %in% c("binomial", "multinomial")) {
        # ReliefFexpRank method.
        score <- CORElearn::attrEval(
          formula, 
          data = data@data, 
          estimator = "ReliefFexpRank")
        
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }
    } else if (is(object, "familiarCoreLearnGainRatioVimp")) {
      # Gain ration method.
      score <- CORElearn::attrEval(
        formula, 
        data = data@data, 
        estimator = "GainRatio")
      
    } else {
      ..error_reached_unreachable_code(
        "..vimp,familiarCoreLearnVimp: unknown class encountered.")
    }

    # Create variable importance object.
    vimp_object <- methods::new(
      "vimpTable",
      vimp_table = data.table::data.table(
        "score" = score, 
        "name" = names(score)),
      score_aggregation = "max",
      invert = TRUE)

    return(vimp_object)
  }
)
