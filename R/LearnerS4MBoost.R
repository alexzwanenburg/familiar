#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarMBoost", contains="familiarModel")

setClass(
  "familiarMBoostLM",
  contains="familiarMBoost",
  slots=list(
    "encoding_reference_table" = "ANY",
    "feature_order"="character"),
  prototype=list(
    "encoding_reference_table" = NULL,
    "feature_order"=character()))

setClass(
  "familiarMBoostTree",
  contains="familiarMBoost",
  slots=list(
    "encoding_reference_table" = "ANY",
    "feature_order"="character"),
  prototype=list(
    "encoding_reference_table" = NULL,
    "feature_order"=character()))



.get_available_mboost_lm_learners <- function(show_general = TRUE) {
  
  # Learners
  learners <- c(
    "boosted_glm", "boosted_glm_logistic",
    "boosted_glm_probit", "boosted_glm_loglog", "boosted_glm_cauchy", "boosted_glm_log",
    "boosted_glm_auc", "boosted_glm_gaussian", "boosted_glm_huber", "boosted_glm_laplace",
    "boosted_glm_poisson", "boosted_glm_cox", "boosted_glm_surv",
    "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex")
  
  if (!show_general) {
    learners <- setdiff(learners, c("boosted_glm", "boosted_glm_surv"))
  }
  
  return(learners)
}


.get_available_mboost_tree_learners <- function(show_general = TRUE) {
  
  # Learners
  learners <- c(
    "boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
    "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log",
    "boosted_tree_auc", "boosted_tree_gaussian", "boosted_tree_huber",
    "boosted_tree_laplace", "boosted_tree_poisson", "boosted_tree_cox", "boosted_tree_surv",
    "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan", "boosted_tree_cindex")
  
  if (!show_general) {
    learners <- setdiff(learners, c("boosted_tree", "boosted_tree_surv"))
  }
  
  return(learners)
}


# is_available,familiarMBoostLM ------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMBoostLM"),
  function(object, ...)
  {
    
    # Extract learner and outcome_type from the familiarModel object.
    learner <- object@learner
    outcome_type <- object@outcome_type
    
    if (outcome_type == "survival" && learner %in% c(
      "boosted_glm", "boosted_glm_cox", "boosted_glm_surv", "boosted_glm_loglog",
      "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "continuous" && learner %in% c(
      "boosted_glm", "boosted_glm_gaussian", "boosted_glm_huber",
      "boosted_glm_laplace", "boosted_glm_poisson")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "binomial" && learner %in% c(
      "boosted_glm", "boosted_glm_logistic", "boosted_glm_probit", "boosted_glm_loglog",
      "boosted_glm_cauchy", "boosted_glm_log", "boosted_glm_auc")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "count" && learner %in% c("boosted_glm", "boosted_glm_poisson")) {
      ..deprecation_mboost()
      ..deprecation_count()
      return(FALSE)
      
    } else {
      return(FALSE)
    }
  }
)



# is_available,familiarMBoostTree ----------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMBoostTree"),
  function(object, ...) {
            
    # Extract learner and outcome_type from the familiarModel object.
    learner <- object@learner
    outcome_type <- object@outcome_type
    
    if (outcome_type == "survival" && learner %in% c(
      "boosted_tree", "boosted_tree_cox", "boosted_tree_surv","boosted_tree_loglog",
      "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan", "boosted_tree_cindex")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "continuous" && learner %in% c(
      "boosted_tree", "boosted_tree_gaussian", "boosted_tree_huber",
      "boosted_tree_laplace", "boosted_tree_poisson")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "binomial" && learner %in% c(
      "boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
      "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log", "boosted_tree_auc")) {
      ..deprecation_mboost()
      return(FALSE)
      
    } else if (outcome_type == "count" && learner %in% c("boosted_tree", "boosted_tree_poisson")) {
      ..deprecation_mboost()
      ..deprecation_count()
      return(FALSE)
      
    } else {
      return(FALSE)
    }
  }
)


# ..predict --------------------------------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarMBoost",
    data = "dataObject"),
  function(object, data, type = "default", ...) {
    ..deprecation_mboost()
    return(callNextMethod())
  }
)
