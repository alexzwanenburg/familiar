#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarCoxPH",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY"),
         prototype=list("encoding_reference_table" = NULL))


.get_available_cox_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("cox")

  return(learners)
}


#####is_available#####
setMethod("is_available", signature(object="familiarCoxPH"),
          function(object, ...){
            
            # CoxPH is only available if for survival outcomes.
            return(object@outcome_type == "survival")
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarCoxPH"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries
            param    <- list()
            param$sign_size <- list()
            
            # If data is explicitly NULL, return the list with hyperparameter names only
            if(is.null(data)) return(param)
            
            ##### Signature size #####
            param$sign_size <- .get_default_sign_size(data_obj=data, restrict_samples=TRUE)
            
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarCoxPH"),
          function(object, type="default"){
            
            # Cox proportional hazards models predict relative risks
            if(type == "default"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
               
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarCoxPH: unknown type")
            }
          })



#####..train####
setMethod("..train", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data){
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)) return(callNextMethod())
            
            # Use effect coding to convert categorical data into encoded data -
            # this is required to deal with factors with missing/new levels
            # between training and test data sets.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=encoded_data$encoded_data)
            
            # Parse formula
            formula <- stats::reformulate(termlabels=feature_columns,
                                          response=quote(survival::Surv(outcome_time, outcome_event)))
            
            # Generate model -- NOTE: coxph was directly imported to allow access to predict and summary functions that were not exported in survival
            model_control <- survival::coxph.control(iter.max=100)
            
            # Train the model
            model <- tryCatch(coxph(formula,
                                    data=encoded_data$encoded_data@data,
                                    control=model_control,
                                    y=FALSE),
                              error=identity)
            
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            # Check if the model fitter converged in time.
            if(model$iter >= 100) return(callNextMethod())
            
            # Check if all coefficients could not be estimated.
            if(all(!sapply(stats::coef(model), is.finite))) return(callNextMethod())
            
            # Add model
            object@model <- model
            
            # Add the contrast references to model_list
            object@encoding_reference_table <- encoded_data$reference_table
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data, type="default", ...){
            
            if(type == "default"){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=encoded_data$encoded_data,
                                                                   type=type)
              
              # Use the model for prediction.
              model_predictions <- predict(object=object@model,
                                           newdata=encoded_data$encoded_data@data,
                                           type="risk")
              
              # Update the prediction table.
              prediction_table[, "predicted_outcome":=model_predictions]
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              return(predict(object=object@model,
                             newdata=encoded_data$encoded_data@data,
                             type=type,
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarCoxPH"),
          function(object, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Define p-values
            coefficient_z_values <- .compute_z_statistic(object)
            coefficient_z_values <- coefficient_z_values[names(coefficient_z_values) != "(Intercept)"]
            
            if(length(coefficient_z_values) == 0) return(callNextMethod())
            
            # Assign to variable importance table.
            vimp_table <- data.table::data.table("score"=coefficient_z_values,
                                                 "name"=names(coefficient_z_values))
            
            # Decode any categorical variables.
            vimp_table <- decode_categorical_variables_vimp(object=object,
                                                            vimp_table=vimp_table,
                                                            method="max")
            
            # Add ranks and set multi_var
            vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })



#####..set_calibration_info#####
setMethod("..set_calibration_info", signature(object="familiarCoxPH"),
          function(object, data){
            
            # Check if calibration info already.
            if(has_calibration_info(object)) return(object)
            
            if(object@outcome_type == "survival"){
              # Determine baseline survival.
              object@calibration_info <- get_baseline_survival(data=data)
              
            } else {
              return(callNextMethod())
            }
            
            return(object)
          })



#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarCoxPH"),
          function(object, ...){
            
            # Update model by removing the call.
            object@model$call <- call("nullcall")
            
            # Remove .Environment.
            object@model$terms <- .replace_environment(object@model$terms)
            
            # Set anonymised to TRUE.
            object@is_anonymised <- TRUE
            
            # Default method for models that lack a more specific method.
            return(object)
          })
