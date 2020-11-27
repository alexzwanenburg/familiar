#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#'@title Update familiar S4 objects to the most recent version.
#'
#'@description Provides backward compatibility for familiar objects exported to
#'  a file. This mitigates compatibility issues when working with files that
#'  become outdated as new versions of familiar are released, e.g. because slots
#'  have been removed.
#'
#'@param object A `familiarModel`, a `familiarEnsemble`, a `familiarData` or
#'  `familiarCollection` object.
#'@param ... Unused arguments.
#'
#'@return An up-to-date version of the respective S4 object.
#'@exportMethod update_object
#'@md
#'@rdname update_object-methods
setGeneric("update_object", function(object, ...) standardGeneric("update_object"))

##### update_object (familiarModel) #####
#'@rdname update_object-methods
setMethod("update_object", signature(object="familiarModel"),
          function(object, ...){
            
            if(object@familiar_version < "0.0.0.54"){
              
              # Rename req_feature_cols to required_features
              attr(object, "required_features") <- attr(object, "req_feature_cols")
              attr(object, "req_feature_cols") <- NULL
              
              # Rename important_features to model_features
              attr(object, "model_features") <- attr(object, "important_features")
              attr(object, "important_features") <- NULL
              
              # Introduce missing novelty_features slot by copying
              # model_features.
              attr(object, "novelty_features") <- attr(object, "model_features")
              
              # Remove signature altogether.
              attr(object, "signature") <- NULL
              
              # Check that the model has been successfully trained. Since
              # version 0.0.0.54, features from models that have not been
              # trained will not be retained for further evaluation.
              if(!model_is_trained(object)){
                object@required_features <- NULL
                object@model_features <- NULL
                object@novelty_features <- NULL
              }
            }
            
            if(!methods::validObject(object)) stop("Could not update the familiarModel object to the most recent definition.")
            
            # Update package version.
            object <- add_package_version(object=object)
            
            return(object)
          })


##### update_object (familiarEnsemble) #####
#'@rdname update_object-methods
setMethod("update_object", signature(object="familiarEnsemble"),
          function(object, ...){
            
            if(object@familiar_version < "0.0.0.54"){
              
              # Rename req_feature_cols to required_features
              attr(object, "required_features") <- attr(object, "req_feature_cols")
              attr(object, "req_feature_cols") <- NULL
              
              # Rename important_features to model_features
              attr(object, "model_features") <- attr(object, "important_features")
              attr(object, "important_features") <- NULL
              
              # Introduce missing novelty_features slot by copying
              # model_features.
              attr(object, "novelty_features") <- attr(object, "model_features")
              
              # Set default model_dir_path and auto_detach attributes.
              attr(object, "model_dir_path") <- NA_character_
              attr(object, "auto_detach") <- FALSE
            }
            
            if(!methods::validObject(object)) stop("Could not update the familiarEnsemble object to the most recent definition.")
            
            # Update package version.
            object <- add_package_version(object=object)
            
            return(object)
          })


##### update_object (familiarData) #####
#'@rdname update_object-methods
setMethod("update_object", signature(object="familiarData"),
          function(object, ...){
            
            if(object@familiar_version < "0.0.0.54"){
              
              # Rename req_feature_cols to required_features
              attr(object, "required_features") <- attr(object, "req_feature_cols")
              attr(object, "req_feature_cols") <- NULL
              
              # Rename important_features to model_features
              attr(object, "model_features") <- attr(object, "important_features")
              attr(object, "important_features") <- NULL
            }
            
            if(!methods::validObject(object)) stop("Could not update the familiarData object to the most recent definition.")
            
            # Update package version.
            object <- add_package_version(object=object)
            
            return(object)
          })


##### update_object (familiarCollection) #####
#'@rdname update_object-methods
setMethod("update_object", signature(object="familiarCollection"),
          function(object, ...){
            
            if(object@familiar_version < "0.0.0.54"){
              
              # Rename req_feature_cols to required_features
              attr(object, "required_features") <- attr(object, "req_feature_cols")
              attr(object, "req_feature_cols") <- NULL
              
              # Rename important_features to model_features
              attr(object, "model_features") <- attr(object, "important_features")
              attr(object, "important_features") <- NULL
            }
            
            if(!methods::validObject(object)) stop("Could not update the familiarCollection object to the most recent definition.")
            
            # Update package version.
            object <- add_package_version(object=object)
            
            return(object)
          })
