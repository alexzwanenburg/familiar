##### .train (hyperparameter learner) ------------------------------------------


##### .predict (hyperparameter learner) ----------------------------------------


##### show(hyperparameter learner) ---------------------------------------------




##### require_package (hyperparameter learner) ---------------------------------
setMethod("require_package", signature(x="familiarHyperparameterLearner"),
          function(x, purpose=NULL, message_type="error", ...){
            
            # Skip if no package is required.
            if(is_empty(x@package)) return(invisible(TRUE))
            
            # Set standard purposes for common uses.
            if(!is.null(purpose)){
              if(purpose %in% c("train", "predict", "show")){
                purpose <- switch(purpose,
                                  "train"="to train a model for inferring hyperparameter scores",
                                  "predict"="to infer hyperparameter scores",
                                  "show"="to capture output of the hyperparameter model")
              }
            }
            
            return(invisible(.require_package(x=x@package, purpose=purpose, message_type=message_type)))
          })



##### set_package_version (hyperparameter learner) -----------------------------
setMethod("set_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            # Do not add package versions if there are no packages.
            if(is_empty(object@package)) return(object)
            
            # Obtain package versions.
            object@package_version <- sapply(object@package, function(x) (as.character(utils::packageVersion(x))))
            
            return(object)
          })



##### check_package_version (hyperparameter learner) ---------------------------
setMethod("check_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            .check_package_version(name=object@package,
                                   version=object@package_version,
                                   when="at model creation")
          })



##### model_is_trained (hyperparameter learner) --------------------------------
setMethod("model_is_trained", signature(object="familiarHyperparameterLearner"),
          function(object){
            # Check if a model was trained.
            if(is.null(object@model)){
              # If no model is attached to the object, assume that no model was
              # trained.
              return(FALSE)
              
            } else {
              # If the the model is not NULL, assume that it is present.
              return(TRUE)
            }
          })


##### add_package_version (hyperparameter learner) -----------------------------
setMethod("add_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })


##### ..train (hyperparameter learner, data) -----------------------------------
setMethod("..train", signature(object="familiarHyperparameterLearner", data="ANY"),
          function(object, data, ...){
            # This method is basically a capture-all for when model training
            # does not succeed. Normally ..train calls refer to child classes.
            # This method is then only called when something goes wrong there.
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })


##### ..predict (hyperparameter learner, data) ---------------------------------
setMethod("..predict", signature(object="familiarHyperparameterLearner", data="data.table"),
          function(object, data, ...){
            # Like ..train, this method is a capture-all for when predictions
            # fail.
            return(rep_len(-1.0, length.out=nrow(data)))
          })
