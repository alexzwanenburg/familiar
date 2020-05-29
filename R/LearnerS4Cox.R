#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarCoxPH",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY"),
         prototype=list("encoding_reference_table" = NULL))



setMethod("is_available", signature(object="familiarCoxPH"),
          function(object, ...){
            
            # CoxPH is only available if for survival outcomes.
            return(object@outcome_type == "survival")
          })



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



setMethod("..train", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data){
            
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
            if(inherits(model, "error")) callNextMethod()
            
            # Check if the model fitter converged in time.
            if(model$iter >= 100) callNextMethod()
            
            # Add model
            object@model <- model
            
            # Add the contrast references to model_list
            object@encoding_reference_table <- encoded_data$reference_table
            
            return(object)
          })



setMethod("..predict", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data, type="risk", ...){
            
            # Check if the model was trained.
            if(!model_is_trained(object)) callNextMethod()
            
            # Encode data so that the features are the same as in the training.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=encoded_data$encoded_data)
            
            # Use the model for prediction.
            model_predictions <- predict(object=object@model,
                                         newdata=encoded_data$encoded_data@data,
                                         type=type)
            
            # Update the prediction table.
            prediction_table[, "predicted_outcome":=model_predictions]
            
            return(prediction_table)
          })


setMethod("..predict_survival_probability", signature(object="familiarCoxPH", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") callNextMethod()
              
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })


setMethod("..vimp", signature(object="familiarCoxPH"),
          function(object){
            
            if(!model_is_trained(object)) callNextMethod()
            
            # Define p-values
            coefficient_p_values <- coefficient_one_sample_z_test(model=object@model)
            coefficient_p_values <- coefficient_p_values[names(coefficient_p_values) != "(Intercept)"]
            
            if(length(coefficient_p_values) == 0) callNextMethod()
            
            # Assign to variable importance table.
            vimp_table <- data.table::data.table("score"=coefficient_p_values,
                                                 "name"=names(coefficient_p_values))
            
            # Decode any categorical variables.
            vimp_table <- decode_categorical_variables_vimp(object=object,
                                                            vimp_table=vimp_table,
                                                            method="min")
            
            # Add ranks and set multi_var
            vimp_table[, "rank":=data.table::frank(score, ties.method="min")]
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })


setMethod("get_prediction_type", signature(object="familiarCoxPH"),
          function(object, type=NULL){
            
            # Cox proportional hazards models predict relative risks
            if(is.null(type)) return("hazard_ratio")
            
            if(type == "risk"){
              return("hazard_ratio")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarCoxPH: unknown type")
            }
          })


setMethod("..set_calibration_info", signature(object="familiarCoxPH"),
          function(object, data){
            
            # Check if calibration info already.
            if(has_calibration_info(object)) return(object)
            
            if(object@outcome_type == "survival"){
              # Determine baseline survival.
              object@calibration_info <- get_baseline_survival(data=data)
              
            } else {
              callNextMethod()
            }
            
            return(object)
          })
