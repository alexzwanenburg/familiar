#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarSurvRegr",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY"),
         prototype=list("encoding_reference_table" = NULL))


.get_available_survival_regression_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("survival_regr", "survival_regr_weibull", "survival_regr_exponential",
                "survival_regr_gaussian", "survival_regr_logistic",
                "survival_regr_lognormal", "survival_regr_loglogistic")
  
  if(!show_general){
    learners <- setdiff(learners, c("survival_regr"))
  }
  
  return(learners)
}


#####is_available#####
setMethod("is_available", signature(object="familiarSurvRegr"),
          function(object, ...){
            
            # Survival regression is only available if for survival outcomes.
            return(object@outcome_type == "survival")
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarSurvRegr"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries
            param <- list()
            param$sign_size <- list()
            param$distribution <- list()
            
            # If data is explicitly set to NULL, return the list with hyperparameter names only
            if(is.null(data)) return(param)
            
            
            ##### Signature size #########################################################
            param$sign_size <- .get_default_sign_size(data_obj=data, restrict_samples=TRUE)
            
            
            ##### Outcome distribution ###################################################
            
            # Randomisation of distribution depends on selected learner.
            if(object@learner == "survival_regr"){
              distribution_default <- c("weibull", "exponential", "gaussian", "logistic", "loglogistic", "lognormal")
              
            } else {
              distribution_default <- stringi::stri_replace_first_fixed(str=object@learner,
                                                                        pattern="survival_regr_",
                                                                        replacement="")
            }
            
            # Set the distribution parameter
            param$distribution <- .set_hyperparameter(default=distribution_default, type="factor", range=distribution_default,
                                                      randomise=ifelse(length(distribution_default) > 1, TRUE, FALSE))
            
            # Return hyper-parameters
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarSurvRegr"),
          function(object, type="default"){
            
            # Survival regression models predict an expected survival time by
            # default.
            if(type == "default"){
              return("expected_survival_time")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarSurvRegr: unknown type")
            }
          })



#####..train####
setMethod("..train", signature(object="familiarSurvRegr", data="dataObject"),
          function(object, data){
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
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
            
            # Set limits to the number of iterations that can be performed by
            # survival regression.
            model_control <- survival::survreg.control(iter.max=100)
            
            # Generate model -- NOTE: survreg was directly imported to allow
            # access to predict and summary functions that were not exported in
            # survival.
            model <- quiet(tryCatch(survreg(formula,
                                            data=encoded_data$encoded_data@data,
                                            control=model_control,
                                            y=FALSE,
                                            dist=object@hyperparameters$distribution),
                                    error=identity))
            
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
setMethod("..predict", signature(object="familiarSurvRegr", data="dataObject"),
          function(object, data, type="default", time=NULL, ...){
            
            if(type %in% c("default", "survival_probability")){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Encode data so that the features are the same as in the
              # training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=encoded_data$encoded_data,
                                                                   type=type)
              
              if(object@outcome_type == "survival"){
                
                if(type == "default"){
                  # Use the model to predict expected survival time.
                  model_predictions <- predict(object=object@model,
                                               newdata=encoded_data$encoded_data@data,
                                               type="response")
                  
                  # Update the prediction table.
                  prediction_table[, "predicted_outcome":=model_predictions]
                  
                } else if(type == "survival_probability"){
                  # To predict survival probability we first compute survival
                  # quantiles, which are survival probabilities. 
                  
                  # Survival quantiles from 1.00 to 0.01
                  survival_quantiles  <- seq(from=1.00, to=0.01, by=-0.01)
                  
                  # Get estimated failure times
                  failure_matrix <- predict(object=object@model,
                                            newdata=encoded_data$encoded_data@data,
                                            type="quantile",
                                            p=1.00 - survival_quantiles)
                  
                  # Set id columns
                  id_columns <- get_id_columns()
                  
                  # Convert event_matrix to a matrix.
                  if(!is.matrix(failure_matrix)){
                    failure_matrix <- matrix(data=failure_matrix, ncol=length(failure_matrix))
                  }
                  
                  # Combine with identifiers and cast to table.
                  failure_table <- cbind(prediction_table[, mget(id_columns)],
                                         data.table::as.data.table(failure_matrix))
                  
                  # Remove duplicate entries
                  failure_table <- unique(failure_table, by=id_columns)
                  
                  # Melt to a long format.
                  failure_table <- data.table::melt(failure_table,
                                                    id.vars=id_columns,
                                                    variable.name="quantile_variable",
                                                    value.name="survival_time")
                  
                  # Create conversion table to convert temporary variables into
                  # the event times.
                  conversion_table <- data.table::data.table("quantile_variable"=paste0("V", seq_along(survival_quantiles)),
                                                             "survival_quantile"=survival_quantiles)
                  
                  # Add in 
                  failure_table <- merge(x=failure_table, y=conversion_table, on="quantile_variable")
                  
                  # Drop the time_variable column
                  failure_table[, "quantile_variable":=NULL]
                  
                  # Now, interpolate at the given time point.
                  failure_table <- lapply(split(failure_table, by=id_columns), function(sample_table, time, id_columns){
                    
                    # Interpolate values at the given time.
                    value <- stats::approx(x=sample_table$survival_time,
                                           y=sample_table$survival_quantile,
                                           xout=time,
                                           rule=2)$y
                    
                    # Create an output table
                    output_table <- data.table::copy(sample_table[1, mget(id_columns)])
                    output_table[, "survival_probability":=value]
                    
                    return(output_table)
                  }, time=time, id_columns=id_columns)
                  
                  # Concatenate to single table.
                  failure_table <- data.table::rbindlist(failure_table)
                  
                  # Remove survival_probability from the prediction table.
                  prediction_table[, "survival_probability":=NULL]
                  
                  # Then merge the event table into the prediction table.
                  prediction_table <- merge(x=prediction_table, y=failure_table, by=id_columns)
                }
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Encode data so that the features are the same as in the
              # training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Use the model to predict expected survival time.
              return(predict(object=object@model,
                             newdata=encoded_data$encoded_data@data,
                             type=type,
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarSurvRegr", data="dataObject"),
          function(object, data, time){
            
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            return(..predict(object=object, data=data, time=time, type="survival_probability"))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarSurvRegr"),
          function(object, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Define p-values
            coefficient_z_values <- coefficient_one_sample_z_test(model=object@model)
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
setMethod("..set_calibration_info", signature(object="familiarSurvRegr"),
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


#####..set_vimp_parameters#####
setMethod("..set_vimp_parameters", signature(object="familiarSurvRegr"),
          function(object, method, ...){
            
            # Randomisation of distribution depends on selected learner.
            if(method == "survival_regr"){
              distribution_default <- "weibull"
              
            } else {
              distribution_default <- stringi::stri_replace_first_fixed(str=method,
                                                                        pattern="survival_regr_",
                                                                        replacement="")
            }
            
            # Set the distribution parameter
            object@hyperparameters$distribution <- distribution_default
            
            return(object)
          })
