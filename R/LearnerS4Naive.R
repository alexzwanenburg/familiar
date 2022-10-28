#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarNaiveModel",
         contains="familiarModel")

setClass("familiarNaiveCoxModel",
         contains="familiarNaiveModel")

setClass("familiarNaiveSurvivalTimeModel",
         contains="familiarNaiveModel")

setClass("familiarNaiveCumulativeHazardsModel",
         contains="familiarNaiveModel")



#### ..train (familiarNaiveModel) ----------------------------------------------
setMethod("..train", signature(object="familiarNaiveModel", data="dataObject"),
          function(object, data, ...){
            
            # Check if training data is ok.
            if(reason <- has_bad_training_data(object=object, data=data, allow_no_features=TRUE)){
              return(callNextMethod(object=.why_bad_training_data(object=object, reason=reason)))
            } 
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)){
              return(callNextMethod(object=..update_errors(object=object, ..error_message_no_optimised_hyperparameters_available())))
            }
            
            if(object@outcome_type %in% c("count", "continuous")){
              model <- object@outcome_info@distribution
              
            } else if(object@outcome_type %in% c("binomial", "multinomial")){
              model <- object@outcome_info@distribution
            
            } else if(object@outcome_type %in% c("survival")){
              model <- object@outcome_info@distribution
              
            } else {
              # Other outcomes should be 
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Add model... well "model" really.
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#### ..predict (familiarNaiveModel) --------------------------------------------
setMethod("..predict", signature(object="familiarNaiveModel", data="dataObject"),
          function(object, data, ...){
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(
              object=object,
              data=data,
              type="default"
            )
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              # Get classes and their probabilities from the stored model data.
              # In the naive model, the probability of each class is its
              # occurrence in the development data.
              class_levels <- object@model$frequency$outcome
              class_probabilities <- object@model$frequency$count / object@model$n
              
              # Fill class probabilities.
              class_probability_columns <- get_class_probability_name(class_levels)
              for(ii in seq_along(class_probability_columns)){
                prediction_table[, (class_probability_columns[ii]):=class_probabilities[ii]]
              }
              
              # Update predicted class based on provided probabilities.
              class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
              class_predictions <- factor(class_predictions, levels=get_outcome_class_levels(object))
              prediction_table[, "predicted_class":=class_predictions]
              
            } else if(object@outcome_type %in% c("count", "continuous")){
              # In the naive model, return the median value for regression
              # problems.
              prediction_table[, "predicted_outcome":=object@model$median]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })



#### ..predict (familiarNaiveCoxModel) -----------------------------------------
setMethod("..predict", signature(object="familiarNaiveCoxModel", data="dataObject"),
          function(object, data, ...){
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(
              object=object,
              data=data,
              type="default"
            )
            
            if(object@outcome_type %in% c("survival")){
              # For survival outcomes based on relative risks, predict the
              # average risk, i.e. 1.0.
              prediction_table[, "predicted_outcome":=1.0]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })



#### ..predict (familiarNaiveSurvivalTimeModel) --------------------------------
setMethod("..predict", signature(object="familiarNaiveSurvivalTimeModel", data="dataObject"),
          function(object, data, ...){
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(
              object=object,
              data=data,
              type="default"
            )
            
            if(object@outcome_type %in% c("survival")){
              # For survival outcomes based on survival times, predict the
              # average survival time.
              
              browser()
              #TODO: CHECK THIS
              prediction_table[, "predicted_outcome":=NA]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })



#### ..predict (familiarNaiveSurvivalTimeModel) --------------------------------
setMethod("..predict", signature(object="familiarNaiveCumulativeHazardsModel", data="dataObject"),
          function(object, data, time=NULL, ...){
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(
              object=object,
              data=data,
              type="default"
            )
            
            if(object@outcome_type %in% c("survival")){
              # For survival outcomes based on survival times, predict the
              # average survival time.
              
              # If time is not provided, set the time at the last observed
              # event.
              
              # TODO: IMPLEMENT THIS
              if(is.null(time)) time <- NA
              
              browser()
              
              # Compute the cumulative hazard at the indicated time point.
              #TODO: CHECK THIS
              prediction_table[, "predicted_outcome":=NA]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })


#### ..predict_survival_probability --------------------------------------------
setMethod("..predict_survival_probability", signature(object="familiarNaiveModel", data="dataObject"),
          function(object, data, time){
            
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            # Prepare an empty table in case things go wrong.
            prediction_table <- get_placeholder_prediction_table(
              object=object,
              data=data,
              type="survival_probability"
            )
            
            # Check for several issues that prevent survival probabilities from
            # being predicted.
            if(!has_calibration_info(object)) return(prediction_table)
            
            # For naive models, survival probability is defined by the
            # Kaplan-Meier estimate of survival at the time point time.
            prediction_table[, "survival_probability":=..survival_probability_relative_risk(
              object=object,
              relative_risk=rep_len(1.0, nrow(prediction_table)),
              time=time
            )]
            
            return(prediction_table)
          })



#### get_prediction_type (familiarNaiveCoxModel) -------------------------------
setMethod("get_prediction_type", signature(object="familiarNaiveCoxModel"),
          function(object, type="default"){
            
            # Cox proportional hazards models predict relative risks.
            if(type == "default"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarNaiveCoxModel: unknown type")
            }
          })



#### get_prediction_type (familiarNaiveSurvivalTimeModel) ----------------------
setMethod("get_prediction_type", signature(object="familiarNaiveSurvivalTimeModel"),
          function(object, type="default"){
            
            # These models predict an expected survival time by
            # default.
            if(type == "default"){
              return("expected_survival_time")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarNaiveSurvivalTimeModel: unknown type")
            }
          })



#### get_prediction_type (familiarNaiveCumulativeHazardsModel) -----------------
setMethod("get_prediction_type", signature(object="familiarNaiveCumulativeHazardsModel"),
          function(object, type="default"){
            
            # These models predict an cumulative hazard by default.
            if(type == "default"){
              return("cumulative_hazard")
              
            } else if(type %in% c("survival_probability")){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarNaiveCumulativeHazardsModel: unknown type")
            } 
          })
