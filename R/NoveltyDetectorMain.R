#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include NoveltyDetectorS4IsolationTree.R
NULL

setMethod("promote_detector", signature(object="familiarNoveltyDetector"),
          function(object){
            
            learner <- object@learner
            
            if(learner %in% .get_available_isolation_forest_detectors()){
              # Isolation forest
              object <- methods::new("familiarIsolationForest", object)
            }
            
            # Returned object can be a standard familiarIsolationForest.
            return(object)
          })



.get_detector_hyperparameters <- function(data,
                                          detector,
                                          names_only=FALSE){
  
  # Create familiarNoveltyDetector
  fam_detector <- methods::new("familiarNoveltyDetector",
                               learner=detector)
  
  # Set up the specific model.
  fam_detector <- promote_detector(object=fam_detector)
  
  # Model hyperparameters
  detector_hyperparameters <- get_default_hyperparameters(object=fam_detector,
                                                          data=data)
  
  # Extract names from parameter list
  if(names_only) detector_hyperparameters <- names(detector_hyperparameters)
  
  return(detector_hyperparameters)
}
