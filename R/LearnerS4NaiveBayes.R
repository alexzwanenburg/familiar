#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarNaiveBayes",
         contains="familiarModel")


#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarNaiveBayes"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "e1071"
            
            return(.Object)
          })


.get_available_naive_bayes_learners <- function(show_general=TRUE) return("naive_bayes")

#####is_available#####
setMethod("is_available", signature(object="familiarNaiveBayes"),
          function(object, ...){
            
            # Naive bayes is only available for categorical outcomes.
            if(object@outcome_type %in% c("binomial", "multinomial")){
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarNaiveBayes"),
          function(object, data=NULL, ...){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            param$laplace <- list()
            
            # If data is explicitly set to NULL, return the list with
            # hyperparameter names only.
            if(is.null(data)) return(param)
            
            ##### Signature size -----------------------------------------------
            param$sign_size <- .get_default_sign_size(data_obj=data, restrict_samples=TRUE)
            
            
            ##### Laplace smoothing parameter ----------------------------------
            param$laplace <- .set_hyperparameter(default=c(0, 1, 2, 5),
                                                 type="numeric",
                                                 range=c(0, 10),
                                                 valid_range=c(0, Inf),
                                                 randomise=TRUE)
            
            return(param)
          })



#####..train####
setMethod("..train", signature(object="familiarNaiveBayes", data="dataObject"),
          function(object, data, ...){
            
            # Check if training data is ok.
            if(reason <- has_bad_training_data(object=object, data=data)){
              return(callNextMethod(object=.why_bad_training_data(object=object, reason=reason)))
            } 
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)){
              return(callNextMethod(object=..update_errors(object=object,
                                                           ..error_message_no_optimised_hyperparameters_available())))
            }
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=data)
            
            # Parse formula
            formula <- stats::reformulate(termlabels=feature_columns,
                                          response=quote(outcome))
            
            # Train the model.
            model <- do.call_with_handlers(e1071::naiveBayes,
                                           args=list(formula,
                                                     "data"=data@data,
                                                     "laplace"=object@hyperparameters$laplace))
            
            # Extract values.
            object <- ..update_warnings(object=object, model$warning)
            object <- ..update_errors(object=object, model$error)
            model <- model$value
            
            # Check if the model trained at all.
            if(!is.null(object@messages$error)) return(callNextMethod(object=object))
            
            # Add model to the familiarModel object.
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })


#### ..train_naive -------------------------------------------------------------
setMethod("..train_naive", signature(object="familiarKNN", data="dataObject"),
          function(object, data, ...){
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              # Turn into a Naive model.
              object <- methods::new("familiarNaiveModel", object)
            }
            
            return(..train(
              object=object,
              data=data,
              ...))
          })


#####..predict#####
setMethod("..predict", signature(object="familiarNaiveBayes", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            if(type == "default"){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=data,
                                                                   type=type)
              
              # Use the model to predict class probabilities.
              model_predictions <- predict(object=object@model,
                                           newdata=data@data,
                                           type="raw")
              
              # Obtain class levels.
              class_levels <- get_outcome_class_levels(x=object)
              
              # Add class probabilities.
              class_probability_columns <- get_class_probability_name(x=object)
              for(ii in seq_along(class_probability_columns)){
                prediction_table[, (class_probability_columns[ii]):=model_predictions[, class_levels[ii]]]
              }
              
              # Update predicted class based on provided probabilities.
              class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
              class_predictions <- factor(class_predictions, levels=class_levels)
              prediction_table[, "predicted_class":=class_predictions]
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Use the model for prediction.
              return(predict(object=object@model,
                             newdata=data@data,
                             type=type,
                             ...))
            }
          })



#####..vimp---------------------------------------------------------------------
# Naive Bayes does not have an associated variable importance method.



#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarNaiveBayes"),
          function(object, ...){
            
            # Update model by removing the call.
            object@model$call <- call("trimmed")
            
            # Add show.
            object <- .capture_show(object)
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            # Default method for models that lack a more specific method.
            return(object)
          })
