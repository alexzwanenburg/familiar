get_baseline_survival <- function(data){
  
  # Determines baseline survival based on relative risk (i.e. proportional
  # hazards models, such as Cox) This is a general method that only requires
  # that a model produces relative risks, or is calibrated to produce relative
  # risks. Based on Cox and Oakes (1984)
  
  # Suppress NOTES due to non-standard evaluation in data.table
  time <- NULL
  
  if(!is(data, "dataObject")) ..error_reached_unreachable_code("get_baseline_survival: data is not a dataObject object.")
  
  if(!data@outcome_type %in% c("survival")) ..error_reached_unreachable_code("get_baseline_survival: outcome_type is not survival.")
  
  # Extract relevant information regarding survival.
  survival_data <- unique(data@data[, mget(c(get_id_columns(id_depth="series"), "outcome_time", "outcome_event"))])
  
  # Make a local copy.
  survival_data <- data.table::copy(survival_data)
  
  # Get the survival estimate from a Kaplan-Meier fit.
  km_fit <- survival::survfit(Surv(outcome_time, outcome_event)~1, data=survival_data)
  
  # Add censoring rate
  cens_fit <- survival::survfit(Surv(outcome_time, outcome_event==0)~1, data=survival_data)
  
  # Complete a Kaplan-Meier table including censoring rates.
  kaplan_meier_table <- data.table::data.table("time"=km_fit$time,
                                               "km_survival"=km_fit$surv,
                                               "km_survival_var"=km_fit$std.err^2,
                                               "cens_distr"=cens_fit$surv)
  
  # Add time 0.
  if(min(kaplan_meier_table$time)>0){
    kaplan_meier_table <- rbind(kaplan_meier_table,
                                data.table::data.table("time"=0.0, "km_survival"=1.0, "km_survival_var"=0.0, "cens_distr"=1.0))
  }
  
  # Sort by time.
  kaplan_meier_table <- kaplan_meier_table[order(time)]
  
  return(kaplan_meier_table)
}




learner.survival_probability_relative_risk <- function(object, data, time){
  
  if(!is(object, "familiarModel")) ..error_reached_unreachable_code("learner.survival_probability_relative_risk: object is not a familiarModel object.")
  if(!is(data, "dataObject")) ..error_reached_unreachable_code("learner.survival_probability_relative_risk: object is not a dataObject object.")
  
  # Predict relative risks.
  prediction_table <- .predict(object=object, 
                               data=data,
                               allow_recalibration=TRUE,
                               time=time)
  
  # Prepare an empty table in case things go wrong.
  empty_table <- get_placeholder_prediction_table(object=object,
                                                  data=data)
  
  # Check for several issues that prevent survival probabilities from being
  # predicted.
  if(!any_predictions_valid(prediction_table=prediction_table, outcome_type=object@outcome_type)) return(empty_table)
  if(!has_calibration_info(object)) return(empty_table)
  
  # Survival in the group is based on proportional hazards assumption, and
  # uses baseline cumulative hazard and the group's predicted relative risks.
  # This evaluation comes in handy when performing, e.g. the Nam-D'Agostino
  # test. It avoids recalculating the baseline hazard. Following Demler,
  # Paynter and Cook. (Stat. Med. 2015), we compute the survival probability
  # at t=time_max for each sample.
  predicted_risks <- prediction_table$predicted_outcome
  
  # Interpolate the baseline survival function at the fitting times
  baseline_surv <- stats::approx(x=object@calibration_info$time,
                                 y=object@calibration_info$km_survival,
                                 xout=time,
                                 method="linear",
                                 rule=2)$y
  
  # Create a n_sample x n_times matrix
  survival_probability <- sapply(predicted_risks, function(rr, s0) (s0^rr), s0=baseline_surv)
  
  # Updated the prediction table
  prediction_table[, "predicted_outcome":=survival_probability]
  
  return(prediction_table)
}




# learner.survival_probability.random_forest <- function(object, data_obj, time_max){
#   # Get the survival probability from the cumulative hazard function
# 
#   # Suppress NOTES due to non-standard evaluation in data.table
#   survival <- NULL
# 
#   # Predict the data
#   if(is(data_obj, "dataObject")){
#     prediction_list <- predict(object=object, newdata=data_obj, allow_recalibration=TRUE, extra_output=TRUE, time_max=time_max)
#   } else if(is(data_obj, "list")){
#     prediction_list <- data_obj
#   }
#   
#   if(is.null(prediction_list$survival) | is.null(prediction_list$predictions)){
#     return(NULL)
#   }
# 
#   # Compute table with survival probabilities at time=time_max
#   probability_table <- lapply(split(prediction_list$survival, by="subject_id"), function(sample_table, time_max){
#     
#     # Interpolate survival probabilities at time=time_max
#     survival_probability <- stats::approx(x=sample_table$event_time,
#                                           y=sample_table$survival_probability,
#                                           xout=time_max,
#                                           rule=2)$y
#     
#     return(data.table::data.table("subject_id"=sample_table$subject_id[1],
#                                   "survival"=survival_probability))
#   }, time_max=time_max)
#   
#   # Concatenate to single table.
#   probability_table <- data.table::rbindlist(probability_table)
#   
#   # Add in outcome_time and outcome_event from the predictions.
#   probability_table <- merge(x=probability_table,
#                              y=prediction_list$predictions[, c("subject_id", "outcome_time", "outcome_event")],
#                              by="subject_id")
#   
#   # Check if the merged table is not empty (it should not be).
#   if(is_empty(probability_table)){
#     return(NULL)
#   }
#   
#   # Rename outcome_time and outcome_event.
#   data.table::setnames(probability_table, old=c("outcome_time", "outcome_event"), new=c("time", "event"))
#   
#   # Reorder columns
#   data.table::setcolorder(probability_table, c("subject_id", "survival", "time", "event"))
#   
#   # Order by predicted probability.
#   probability_table <- probability_table[order(survival)]
#   
#   return(probability_table)
# }


# 
# learner.survival_probability.accelerated_failure_time <- function(object, data_obj, time_max){
#   # Survival probability based on predicted survival quantiles (1-failure
#   # probability) We are dealing with an inverse problem. We know the survival
#   # quantiles, but need to know them at the event times. A basic approach that
#   # does not depend directly on the choice of distribution is to try and
#   # interpolate the survival quantiles for each sample at time-max.
# 
#   # Suppress NOTES due to non-standard evaluation in data.table
#   survival <- NULL
#   
#   # Get predictions
#   if(is(data_obj, "dataObject")){
#     prediction_list <- predict(object=object, newdata=data_obj, allow_recalibration=TRUE, extra_output=TRUE, time_max=time_max)
#   } else if(is(data_obj, "list")){
#     prediction_list <- data_obj
#   }
# 
#   # Return NULL in case the model prediction could not be performed.
#   if(is.null(prediction_list$failure_times) | is.null(prediction_list$predictions)){
#     return(NULL)
#   }
#   
#   # Compute probability table
#   probability_table <- lapply(split(prediction_list$failure_times, by="subject_id"), function(failure_table, time_max){
#     
#     # Compute survival probability from the failure table.
#     survival_probability <- stats::approx(x=failure_table$failure_time,
#                                           y=failure_table$quantile,
#                                           xout=time_max,
#                                           rule=2)$y
#     
#     return(data.table::data.table("subject_id"=failure_table$subject_id[1],
#                                   "survival"=survival_probability))
#   }, time_max=time_max)
#   
#   # Concatenate to single table.
#   probability_table <- data.table::rbindlist(probability_table)
#   
#   # Add in outcome_time and outcome_event from the predictions.
#   probability_table <- merge(x=probability_table,
#                              y=prediction_list$predictions[, c("subject_id", "outcome_time", "outcome_event")],
#                              by="subject_id")
#   
#   # Check if the merged table is not empty (it should not be).
#   if(is_empty(probability_table)){
#     return(NULL)
#   }
#   
#   # Rename outcome_time and outcome_event.
#   data.table::setnames(probability_table, old=c("outcome_time", "outcome_event"), new=c("time", "event"))
#   
#   # Reorder columns
#   data.table::setcolorder(probability_table, c("subject_id", "survival", "time", "event"))
#   
#   # Order by predicted probability.
#   probability_table <- probability_table[order(survival)]
#   
#   return(probability_table)
# }
