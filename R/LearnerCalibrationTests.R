test.calibration.model <- function(calibration_data, outcome_type, eval_time=NULL){
  # Tests calibration-at-the-large and calibration slope

  # Suppress NOTES due to non-standard evaluation in data.table.
  type <- coef_value <- coef_lower <- coef_upper <- expected <- observed <- NULL

  # Filter out non-finite expected and observed values.
  calibration_data <- calibration_data[is.finite(expected) & is.finite(observed)]
  
  # Check if the input is empty or only contains one entry
  if(nrow(calibration_data) < 2){
    calibration_at_large <- data.table::data.table("type"=character(0),
                                                   "coef_value"=numeric(0),
                                                   "coef_lower"=numeric(0),
                                                   "coef_upper"=numeric(0),
                                                   "p_value"=numeric(0))
    
    if(outcome_type=="survival" & !is.null(eval_time)){
      calibration_at_large[, "evaluation_time":=numeric(0)]
      
    } else if(outcome_type %in% c("binomial", "multinomial")) {
      calibration_at_large[, "pos_class":=character(0)]
    }

    return(calibration_at_large)
  }

  # Perform a linear fit. Note that the slope is offset by 1*expected to
  # directly compare with the expected slope of 1.
  fit <- stats::lm(observed~expected+1+offset(expected), data=calibration_data)
  
  # Get coefficients for intercept and slope
  fit_coef <- stats::coef(fit)

  # Get confidence intervals
  fit_conf_int <- suppressWarnings(stats::confint(fit))

  # Get summary
  fit_summary <- suppressWarnings(stats::summary.lm(fit))$coefficients
  
  # Correct for having multiple overlapping groups in binomial and multinomial
  # tests.
  if(outcome_type %in% c("binomial", "multinomial", "survival")){

    # Determine the number of groups.
    n_groups <- data.table::uniqueN(calibration_data, by="rep_id")
    
    # Recompute the standard deviation
    fit_summary[, 2] <- fit_summary[, 2] * sqrt(n_groups)
    
    # Recompute the t-score
    fit_summary[, 3] <- fit_summary[, 1] / fit_summary[, 2]
    
    # Recompute the p-value
    fit_summary[, 4] <- 2.0 * stats::pt(abs(fit_summary[, 3]), fit$df.residual, lower.tail = FALSE)
    
    # Adapt the confidence intervals
    fit_conf_int[, 1] <- fit_summary[, 1] + fit_summary[, 2] * stats::qnorm(0.025)
    fit_conf_int[, 2] <- fit_summary[, 1] + fit_summary[, 2] * stats::qnorm(0.975)
  }
  
  # Construct data table
  calibration_at_large <- data.table::data.table("type"=c("offset", "slope"),
                                                 "coef_value"=fit_coef,
                                                 "coef_lower"=fit_conf_int[,1],
                                                 "coef_upper"=fit_conf_int[,2],
                                                 "p_value"=fit_summary[, 4])
  
  # Update the slope so that the expected slope (slope+1) is shown instead
  calibration_at_large[type=="slope", ":="("coef_value"=coef_value+1, "coef_lower"=coef_lower+1, "coef_upper"=coef_upper+1)]
  
  # Add in remaining data
  if(outcome_type=="survival" & !is.null(eval_time)){
    calibration_at_large[, "evaluation_time":=eval_time]
  } else if(outcome_type %in% c("binomial", "multinomial")){
    calibration_at_large[, "pos_class":=calibration_data$pos_class[1]]
  }

  return(calibration_at_large)
}



test.calibration.hosmer_lemeshow <- function(calibration_data, outcome_type){

  # Suppress NOTES due to non-standard evaluation in data.table
  observed <- expected <- n_g <- hm_group <- pos_class <- rep_id <- statistic <- n_groups <- p_value <- NULL

  # Make local copy of dt_calibr
  calibration_data <- data.table::copy(calibration_data)

  if(outcome_type %in% c("binomial", "multinomial")){
    # Hosmer-Lemeshow test for categorical outcomes
    
    # Check for empty calibration tables
    if(is_empty(calibration_data)){
      return(data.table::data.table("type"=character(0), "p_value"=numeric(0), "pos_class"=character(0)))
    }

    # Compute test statistic for each group
    calibration_data[, "hm_group":=(observed-expected)^2*n_g / (expected * (1-expected))]

    # Compute test statistic for each time point
    gof_table <- calibration_data[, list(statistic=sum(hm_group, na.rm=TRUE), n_groups=.N),
                                  by=list(pos_class, rep_id)]

    # Remove all entries with n_groups < 3
    gof_table <- gof_table[n_groups >= 3]
    
    if(is_empty(gof_table)){
      return(data.table::data.table("type"=character(0), "p_value"=numeric(0), "pos_class"=character(0)))
    }
    
    # Perform chi-square test
    gof_table <- gof_table[, list(p_value=stats::pchisq(q=statistic, df=n_groups-2, lower.tail=FALSE)),
                           by=list(pos_class, rep_id)]
    
    # Samples overlap, and combination methods that assume independence cannot
    # be used (e.g. Fisher's method) Hence we calculate a mean p-value.
    gof_table <- gof_table[, list(p_value=mean(p_value)), by=list(pos_class)]

    # Add test type
    gof_table[, "type":="hosmer_lemeshow"]

    # Reorder columns
    data.table::setcolorder(gof_table, c("type", "p_value", "pos_class"))

  } else if(outcome_type %in% c("count", "continuous")){
    # Hosmer-Lemeshow test for continuous outcomes. The only difference is that
    # we don't split by the pos_class column as that column is not present.
    
    # Remove entries with out-of-range values.
    calibration_data <- calibration_data[between(expected, 0, 1) & between(observed, 0, 1), ]

    # Check for empty calibration tables
    if(is_empty(calibration_data)){
      return(data.table::data.table("type"=character(0), "p_value"=numeric(0)))
    }
    
    # Compute test statistic for each group.
    calibration_data[, "hm_group":=(observed-expected)^2*n_g / (expected * (1-expected))]

    # Compute test statistic for each time point.
    gof_table <- calibration_data[, list(statistic=sum(hm_group, na.rm=TRUE), n_groups=.N),
                                  by=list(rep_id)]

    # Remove all entries with n_groups < 3
    gof_table <- gof_table[n_groups >= 3]
    
    if(is_empty(gof_table)){
      return(data.table::data.table("type"=character(0), "p_value"=numeric(0)))
    }
    
    # Perform chi-square test.
    gof_table <- gof_table[, list(p_value=stats::pchisq(q=statistic, df=n_groups-2, lower.tail=FALSE)),
                           by=list(rep_id)]

    # Samples overlap, and combination methods that assume independence cannot
    # be used (e.g. Fisher's method) Hence we calculate a mean p-value.
    gof_table <- gof_table[, list(p_value=mean(p_value))]

    # Add test type.
    gof_table[, "type":="hosmer_lemeshow"]

    # Reorder columns.
    data.table::setcolorder(gof_table, c("type", "p_value"))
  }

  return(gof_table)
}



test.calibration.nam_dagostino <- function(calibration_data, eval_time){
  # Nam-D'Agostino and Greenwood-Nam-D'Agostino tests. See Demler et al. 2015.

  # Suppress NOTES due to non-standard evaluation in data.table
  observed <- expected <- n_g <- km_var <- nd_group <- gnd_group <- n_groups <- NULL
  nam_dagostino <- greenwood_nam_dagostino <- p_value <- NULL

  # Make local copy of calibration_data to avoid updating the main copy by
  # reference.
  calibration_data <- data.table::copy(calibration_data)
  
  # Check for empty calibration tables
  if(is_empty(calibration_data)){
    return(data.table::data.table("type"=character(0), "p_value"=numeric(0), "evaluation_time"=numeric(0)))
  }
  
  # Compute test statistic for each group.
  calibration_data[, ":="("nd_group"=(observed-expected)^2*n_g / (expected * (1-expected)),
                          "gnd_group"=(observed-expected)^2 / km_var)]
  
  # Remove gnd_group that are infinite (which occurs when observed==0.000)
  calibration_data[!is.finite(gnd_group), "gnd_group":=0]
  
  # Compute test statistic for each time point
  gof_table <- calibration_data[, list("nam_dagostino"=sum(nd_group, na.rm=TRUE),
                                       "greenwood_nam_dagostino"=sum(gnd_group, na.rm=TRUE),
                                       "n_groups"=.N),
                                by="rep_id"]
  
  # Compute test score for both test tests.
  gof_table <- gof_table[n_groups > 1,
                         list("nam_dagostino"=stats::pchisq(q=nam_dagostino, df=n_groups-1, lower.tail=FALSE),
                              "greenwood_nam_dagostino"=stats::pchisq(q=greenwood_nam_dagostino, df=n_groups-1, lower.tail=FALSE)),
                         by="rep_id"]
  
  # Check for an empty goodness-of-fit table.
  if(is_empty(gof_table)){
    return(data.table("type"=character(0), "p_value"=numeric(0), "evaluation_time"=numeric(0)))
  }
  
  # Melt so that each test is on a separate row
  gof_table <- melt(gof_table, id.vars="rep_id", variable.name="type", value.name="p_value", variable.factor=FALSE)
  
  # Samples overlap, and combination methods that assume independence cannot be
  # used (e.g. Fisher's method). Hence we calculate a mean p-value.
  gof_table <- gof_table[, list("p_value"=mean(p_value)), by="type"]
  
  # Add in evaluation time.
  gof_table[, "evaluation_time":=eval_time]

  # Reorder columns
  data.table::setcolorder(gof_table, c("type", "p_value", "evaluation_time"))
  
  return(gof_table)
}
