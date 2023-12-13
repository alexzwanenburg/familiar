get_baseline_survival <- function(data) {
  # Determines baseline survival based on relative risk (i.e. proportional
  # hazards models, such as Cox) This is a general method that only requires
  # that a model produces relative risks, or is calibrated to produce relative
  # risks. Based on Cox and Oakes (1984)

  # Suppress NOTES due to non-standard evaluation in data.table
  time <- km_survival_var <- NULL

  if (!is(data, "dataObject")) {
    ..error_reached_unreachable_code(
      "get_baseline_survival: data is not a dataObject object.")
  }

  if (!data@outcome_type %in% c("survival")) {
    ..error_reached_unreachable_code(
      "get_baseline_survival: outcome_type is not survival.")
  }

  # Extract relevant information regarding survival.
  survival_data <- unique(data@data[, mget(c(
    get_id_columns(id_depth = "series"), "outcome_time", "outcome_event"))])

  # Make a local copy.
  survival_data <- data.table::copy(survival_data)

  # Get the survival estimate from a Kaplan-Meier fit.
  km_fit <- survival::survfit(
    Surv(outcome_time, outcome_event) ~ 1,
    data = survival_data)

  # Add censoring rate
  cens_fit <- survival::survfit(
    Surv(outcome_time, outcome_event == 0) ~ 1,
    data = survival_data)

  # Complete a Kaplan-Meier table including censoring rates.
  kaplan_meier_table <- data.table::data.table(
    "time" = km_fit$time,
    "km_survival" = km_fit$surv,
    "km_survival_var" = km_fit$std.err^2,
    "cens_distr" = cens_fit$surv)

  # Add time 0.
  if (min(kaplan_meier_table$time) > 0) {
    kaplan_meier_table <- rbind(
      kaplan_meier_table,
      data.table::data.table(
        "time" = 0.0,
        "km_survival" = 1.0,
        "km_survival_var" = 0.0,
        "cens_distr" = 1.0))
  }

  # Replace inf variance by 1.0
  kaplan_meier_table[
    is.infinite(km_survival_var),
    "km_survival_var" := 1.0]

  # Sort by time.
  kaplan_meier_table <- kaplan_meier_table[order(time)]

  return(kaplan_meier_table)
}



.survival_probability_relative_risk <- function(object, data, time) {
  if (!is(object, "familiarModel")) {
    ..error_reached_unreachable_code(
      ".survival_probability_relative_risk: object is not a familiarModel object.")
  }
  if (!is(data, "dataObject")) {
    ..error_reached_unreachable_code(
      ".survival_probability_relative_risk: object is not a dataObject object.")
  }

  # Predict relative risks.
  prediction_table <- .predict(
    object = object,
    data = data,
    type = "default",
    allow_recalibration = TRUE,
    time = time
  )
  
  # Check for several issues that prevent survival probabilities from being
  # predicted.
  if (is_empty(prediction_table)) return(NULL)
  if (!any_predictions_valid(prediction_table)) return(NULL)
  if (!has_calibration_info(object)) return(NULL)

  # Check that prediction table contains hazard ratios.
  if (!is(prediction_table, "predictionTableSurvivalHazardRatio")) {
    ..error_reached_unreachable_code(paste0(
      "survival probability can only be computed using hazard rations ",
      "(predictionTableSurvivalHazardRatio)"
    ))
  }
  
  # Survival in the group is based on proportional hazards assumption, and
  # uses baseline cumulative hazard and the group's predicted relative risks.
  # This evaluation comes in handy when performing, e.g. the Nam-D'Agostino
  # test. It avoids recalculating the baseline hazard. Following Demler,
  # Paynter and Cook. (Stat. Med. 2015), we compute the survival probability
  # at t=time_max for each sample.
  survival_probabilities <- ..survival_probability_relative_risk(
    object = object,
    relative_risk = prediction_table@prediction_data$predicted_outcome,
    time = time
  )

  # Create prediction table.
  prediction_table <- as_prediction_table(
    x = survival_probabilities,
    type = "survival_probability",
    data = data
  )

  return(prediction_table)
}



..survival_probability_relative_risk <- function(object, relative_risk, time) {
  # Survival in the group is based on proportional hazards assumption, and
  # uses baseline cumulative hazard and the group's predicted relative risks.
  # This evaluation comes in handy when performing, e.g. the Nam-D'Agostino
  # test. It avoids recalculating the baseline hazard. Following Demler,
  # Paynter and Cook. (Stat. Med. 2015), we compute the survival probability
  # at t=time_max for each sample.

  # Interpolate the baseline survival function at the fitting times
  baseline_surv <- stats::approx(
    x = object@calibration_info$time,
    y = object@calibration_info$km_survival,
    xout = time,
    method = "linear",
    rule = 2
  )$y

  # Create a n_sample x n_times matrix
  return(sapply(
    relative_risk, 
    function(rr, s0) (s0^rr), 
    s0 = baseline_surv))
}
