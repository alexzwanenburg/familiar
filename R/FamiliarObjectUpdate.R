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
              
              # Add name attribute.
              attr(object, "name") <- NA_character_
            }
            
            if(object@familiar_version < "0.0.0.55"){
              
              # Add missing attributes.
              attr(object, "trimmed_function") <- list()
              attr(object, "learner_package") <- character(0L)
              attr(object, "learner_version") <- as.package_version("0.0.0")
              
              # Rename is_anonymised.
              attr(object, "is_trimmed") <- attr(object, "is_anonymised")
              attr(object, "is_anonymised") <- NULL
            }
            
            if(object@familiar_version < "1.0.0"){
              # Rename learner_package to package
              attr(object, "package") <- attr(object, "learner_package")
              if(is.na(object@package)) methods::slot(object, "package", check=FALSE) <- NULL
              
              # Rename learner_version to package_version
              attr(object, "package_version") <- attr(object, "learner_version")
              if(object@learner_version == as.package_version("0.0.0")) methods::slot(object, "package_version", check=FALSE) <- NULL
              
              # Remove learner_package and learner_version attributes.
              attr(object, "learner_package") <- NULL
              attr(object, "learner_version") <- NULL
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
              
              # Set default model_dir_path, auto_detach and name attributes.
              attr(object, "model_dir_path") <- NA_character_
              attr(object, "auto_detach") <- FALSE
              attr(object, "name") <- NA_character_
            }
            
            if(object@familiar_version < "0.0.0.55"){

              # Remove is_anonymised.
              attr(object, "is_anonymised") <- NULL
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
              
              # Rename mutual_correlation to feature_similarity
              attr(object, "feature_similarity") <- attr(object, "mutual_correlation")
              attr(object, "mutual_correlation") <- NULL
              
              # Add sample_similarity.
              attr(object, "sample_similarity") <- attr(list(), "non_existing_element")
            }
            
            if(object@familiar_version < "0.0.0.55"){
              
              # Remove is_anonymised.
              attr(object, "is_anonymised") <- NULL
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
              
              # Rename collection_name to name
              attr(object, "name") <- attr(object, "collection_name")
              attr(object, "collection_name") <- NULL
              
              # Rename mutual_correlation to feature_similarity
              attr(object, "feature_similarity") <- attr(object, "mutual_correlation")
              attr(object, "mutual_correlation") <- NULL
              
              # Add sample_similarity.
              attr(object, "sample_similarity") <- attr(list(), "non_existing_element")
            }
            
            if(object@familiar_version < "0.0.0.55"){
              
              # Remove is_anonymised.
              attr(object, "is_anonymised") <- NULL
            }
            
            if(!methods::validObject(object)) stop("Could not update the familiarCollection object to the most recent definition.")
            
            # Update package version.
            object <- add_package_version(object=object)
            
            return(object)
          })
