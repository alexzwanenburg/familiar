#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R

setClass("familiarHyperparameterRandomSearch",
         contains="familiarHyperparameterLearner")



##### initialize ---------------------------------------------------------------
setMethod("initialize", signature(.Object="familiarHyperparameterRandomSearch"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set name
            .Object@name <- "Random search"
            
            return(.Object)
          })



.get_available_random_search_hyperparameter_learners <- function(){
  return(c("random", "random_search"))
}



##### model_is_trained ---------------------------------------------------------
setMethod("model_is_trained", signature(object="familiarHyperparameterRandomSearch"),
          function(object){
            # Override default model_is_trained since random search is
            # technically not a model at all.
            return(TRUE)
          })
