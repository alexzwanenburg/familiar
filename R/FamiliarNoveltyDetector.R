#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

##### .train ###################################################################
setMethod(".train", signature(object="familiarNoveltyDetector", data="dataObject"),
          function(object, data) {
            
          })



##### show #####################################################################
setMethod("show", signature(object="familiarNoveltyDetector"),
          function(object){
            
          })



##### require_package ##########################################################
setMethod("require_package", signature(x="familiarNoveltyDetector"),
          function(x, purpose=NULL, message_type="error", ...){
            
            # Skip if no package is required.
            if(is_empty(x@package)) return(invisible(TRUE))
            
            # Set standard purposes for common uses.
            if(!is.null(purpose)){
              if(purpose %in% c("train", "predict")){
                purpose <- switch(purpose,
                                  "train"="to train a novelty detector",
                                  "predict"="to assess novelty")
              }
            }
            
            return(invisible(.require_package(x=x@package, purpose=purpose, message_type=message_type)))
          })



##### set_package_version ######################################################
setMethod("set_package_version", signature(object="familiarNoveltyDetector"),
          function(object){
            # Do not add package versions if there are no packages.
            if(is_empty(object@package)) return(object)
            
            # Obtain package versions.
            object@package_version <- sapply(object@package, function(x) (as.character(utils::packageVersion(x))))
            
            return(object)
          })



##### check_package_version ####################################################
setMethod("check_package_version", signature(object="familiarNoveltyDetector"),
          function(object){
            # Check whether installed packages are outdated or newer.
            .check_package_version(name=object@package,
                                   version=object@package_version,
                                   when="when creating the novelty detector")
            
          })



#####model_is_trained (familiarNoveltyDetector)#####
setMethod("model_is_trained", signature(object="familiarNoveltyDetector"),
          function(object){
            # Check if a model was trained
            if(is.null(object@model)){
              # Check if a model is present
              return(FALSE)
              
            } else {
              # Assume that the model is present if it is not specifically
              # stated using the model_trained element
              return(TRUE)
            }
          })



#####is_available#####
setMethod("is_available", signature(object="familiarNoveltyDetector"),
          function(object, ...) return(FALSE))


#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarNoveltyDetector"),
          function(object, ...) return(list()))



#####..train (familiarModel, dataObject)#####
setMethod("..train", signature(object="familiarNoveltyDetector", data="dataObject"),
          function(object, data, ...){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })

#####..train (familiarModel, NULL)#####
setMethod("..train", signature(object="familiarNoveltyDetector", data="NULL"),
          function(object, data, ...){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })




#####..predict (familiarModel, dataObject)#####
setMethod("..predict", signature(object="familiarNoveltyDetector", data="dataObject"),
          function(object, data, ...) return(get_placeholder_prediction_table(object=object, data=data, type="novelty")))


#####trim_model (familiarModel)-------------------------------------------------
setMethod("trim_model", signature(object="familiarNoveltyDetector"),
          function(object, ...){
            
            # Do not trim the model if there is nothing to trim.
            if(!model_is_trained(object)) return(object)
            
            # Trim the model.
            trimmed_object <- .trim_model(object=object)
            
            # Skip further processing if the model object was not trimmed.
            if(!trimmed_object@is_trimmed) return(object)
            
            # Go over different functions.
            trimmed_object <- .replace_broken_functions(object=object,
                                                        trimmed_object=trimmed_object)
            
            return(trimmed_object)
          })


#####.trim_model (familiarModel)------------------------------------------------
setMethod(".trim_model", signature(object="familiarNoveltyDetector"),
          function(object, ...){
            # Default method for models that lack a more specific method.
            return(object)
          })
