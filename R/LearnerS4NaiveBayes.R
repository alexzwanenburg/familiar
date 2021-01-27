#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarNaiveBayes",
         contains="familiarModel")


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
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            
            # If data is explicitly set to NULL, return the list with
            # hyperparameter names only.
            if(is.null(data)) return(param)
            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data, restrict_samples=TRUE)
            
            return(param)
          })



#####..train####
setMethod("..train", signature(object="familiarNaiveBayes", data="dataObject"),
          function(object, data){
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=data)
            
            # Select feature columns which are invariant
            invariant_features <- feature_columns[sapply(feature_columns, function(feature, data) (is_singular_data(data[[feature]])), data=data@data)]
            feature_columns <- setdiff(feature_columns, invariant_features)
            
            if(length(feature_columns) == 0) return(callNextMethod())
            
            # Parse formula
            formula <- stats::reformulate(termlabels=feature_columns, response=quote(outcome))
            
            # Generate model. NOTE: NaiveBayes is directly imported through
            # NAMESPACE as predict is not exported by klaR.
            model <- tryCatch(NaiveBayes(formula,
                                         data=data@data),
                              error=identity)
            
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            # Add model to the familiarModel object.
            object@model <- model
          
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarNaiveBayes", data="dataObject"),
          function(object, data, type="default", ...){
            
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
              
              # Use the model for prediction.
              model_predictions <- predict(object=object@model,
                                           newdata=data@data)
              
              # Obtain class levels.
              class_levels <- get_outcome_class_levels(x=object)
              
              # Add class probabilities.
              class_probability_columns <- get_class_probability_name(x=object)
              for(ii in seq_along(class_probability_columns)){
                prediction_table[, (class_probability_columns[ii]):=model_predictions$posterior[, class_levels[ii]]]
              }
              
              # Add the predicted class.
              prediction_table[, "predicted_class":=model_predictions$class]
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Use the model for prediction. Note that klaR::NaiveBayes does
              # not actually take a type argument currently.
              return(predict(object=object@model,
                             newdata=data@data,
                             type=type,
                             ...))
            }
          })



#####..vimp#####
# Naive Bayes does not have an associated variable importance method.
