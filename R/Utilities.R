#' @include FamiliarS4Generics.R

check_column_name <- function(column_name){
  
  # Remove spaces
  column_name <- gsub(pattern=" ", replacement="_", column_name)
  
  # Remove less/equal/greater than signs
  column_name <- gsub(pattern=">=", replacement="_geq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="<=", replacement="_leq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="!=", replacement="_neq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="<", replacement="_l_", fixed=TRUE, column_name)
  column_name <- gsub(pattern=">", replacement="_g_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="=", replacement="_eq_", fixed=TRUE, column_name)
  
  # Remove punctuation
  column_name <- gsub(pattern="[[:punct:]]", replacement="_", column_name)
  
  # Remove starting number
  column_name <- gsub(pattern="^([0-9])", replacement="n_\\1", column_name)
  
  return(column_name)
}



stop_or_warn <- function(message, as_error=TRUE){
  # Find the name of the calling environment.
  calling_function <- environmentName(parent.env)
  
  if(length(calling_function) > 0){
    if(calling_function != ""){
      message <- paste0(calling_function, ": ", message)
    }
  }

  if(as_error){
    stop(message, call.=FALSE)
    
  } else {
    warning(message, call.=FALSE)
  }
}



compute_univariable_p_values <- function(cl=NULL, data_obj, feature_columns){
  
  outcome_type <- data_obj@outcome_type
  outcome_columns <- get_outcome_columns(data_obj)
  
  if(outcome_type=="survival"){
    univariate_fun <- .univariate_cox_regression_test
    
  } else if(outcome_type=="continuous"){
    univariate_fun <- .univariate_linear_regression_test
    
  } else if(outcome_type=="binomial"){
    univariate_fun <- .univariate_binomial_logistic_regression_test
    
  } else if(outcome_type=="multinomial"){
    univariate_fun <- .univariate_multinomial_logistic_regression_test
    
  } else if(outcome_type=="count"){
    univariate_fun <- .univariate_poisson_regression_test
    
  } else if(outcome_type=="competing_risk"){
    ..error_outcome_type_not_implemented(outcome_type)
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  # Obtain p-values
  coefficient_p_values <- fam_sapply(cl=cl,
                                     assign=NULL,
                                     X=data_obj@data[, mget(feature_columns)],
                                     FUN=univariate_fun,
                                     progress_bar=FALSE,
                                     outcome_data=data_obj@data[, mget(outcome_columns)],
                                     chopchop=TRUE)
  
  return(coefficient_p_values)
}


.univariate_cox_regression_test <- function(x, outcome_data){
  # Cox regression model for univariable analysis
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table::data.table("value"=x,
                                 "outcome_time"=outcome_data$outcome_time,
                                 "outcome_event"=outcome_data$outcome_event)
  
  # Drop entries with missing feature values.
  data <- data[is.finite(value)]
  
  # Check if the data is empty.
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data$value)) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data$value)){
    data$value <- droplevels(data$value)
  }
  
  # Construct model
  model <- tryCatch(coxph(Surv(outcome_time, outcome_event) ~ .,
                               data=data,
                               na.action=stats::na.omit,
                               model=TRUE),
                        error=identity)

  # Check if the model did not converge.
  if(inherits(model, "error")) return(NA_real_)
  
  # Compute z-statistic.
  z <- .compute_z_statistic(model)
  
  # Remove intercept (if present).
  z <- z[names(z) != "(Intercept)"]
  
  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df=length(x)))
  
  # Check if the value is finite.
  if(all(!is.finite(p_value))) return(NA_real_)
  
  # Return p-value.
  return(unname(min(p_value, na.rm=TRUE)))
}



.univariate_linear_regression_test <- function(x, outcome_data){
  # Gaussian regression for univariable analysis
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table::data.table("value"=x, "outcome"=outcome_data$outcome)
  
  # Drop entries with missing feature values.
  data <- data[is.finite(value)]
  
  # Check if the data is empty.
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data$value)) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data$value)){
    data$value <- droplevels(data$value)
  }
  
  # Construct model
  model <- tryCatch(stats::glm(outcome ~ .,
                               data=data,
                               family=stats::gaussian(link="identity")),
                    error=identity)
  
  # Check if the model did not converge.
  if(inherits(model, "error")) return(NA_real_)
  
  # Compute z-statistic.
  z <- .compute_z_statistic(model)
  
  # Remove intercept (if present).
  z <- z[names(z) != "(Intercept)"]
  
  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df=length(x)))
  
  # Check if the value is finite.
  if(all(!is.finite(p_value))) return(NA_real_)
  
  # Return p-value.
  return(unname(min(p_value, na.rm=TRUE)))
}



.univariate_poisson_regression_test <- function(x, outcome_data){
  # Poisson regression for univariable analysis with count-type outcomes
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table::data.table("value"=x, "outcome"=outcome_data$outcome)
  
  # Drop entries with missing feature values.
  data <- data[is.finite(value)]
  
  # Check if the data is empty.
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data$value)) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data$value)){
    data$value <- droplevels(data$value)
  }
  
  # Construct model
  model <- suppressWarnings(tryCatch(stats::glm(outcome~.,
                                                data=data,
                                                family=stats::poisson(link="log")),
                                     error=identity))
  
  # Check if the model did not converge.
  if(inherits(model, "error")) return(NA_real_)
  
  # Compute z-statistic.
  z <- .compute_z_statistic(model)
  
  # Remove intercept (if present).
  z <- z[names(z) != "(Intercept)"]
  
  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df=length(x)))
  
  # Check if the value is finite.
  if(all(!is.finite(p_value))) return(NA_real_)
  
  # Return p-value.
  return(unname(min(p_value, na.rm=TRUE)))
}



.univariate_binomial_logistic_regression_test <- function(x, outcome_data){
  #Binomial model for univariable analysis using logistic regression
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table::data.table("value"=x, "outcome"=outcome_data$outcome)
  
  # Drop entries with missing feature values.
  data <- data[is.finite(value)]
  
  # Check if the data is empty.
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data$value)) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data$value)){
    data$value <- droplevels(data$value)
  }
  
  # Construct model
  model <- suppressWarnings(tryCatch(stats::glm(outcome ~ .,
                                                data=data,
                                                family=stats::binomial(link="logit")),
                                     error=identity))
  
  # Check if the model did not converge
  if(inherits(model, "error")) return(NA_real_)
  
  # Compute z-statistic
  z <- .compute_z_statistic(model)
  
  # Remove intercept (if present).
  z <- z[names(z) != "(Intercept)"]
  
  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df=length(x)))
  
  # Check if the value is finite.
  if(all(!is.finite(p_value))) return(NA_real_)
  
  # Return p-value.
  return(unname(min(p_value, na.rm=TRUE)))
}



.univariate_multinomial_logistic_regression_test <- function(x, outcome_data){
  # Multinomial model for univariable analysis using logistic regression
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table::data.table("value"=x, "outcome"=outcome_data$outcome)
  
  # Drop entries with missing feature values.
  data <- data[is.finite(value)]
  
  # Check if the data is empty.
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data$value)) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data$value)){
    data$value <- droplevels(data$value)
  }

  # Construct model
  model <- suppressWarnings(tryCatch(VGAM::vglm(outcome ~ .,
                                                data=data,
                                                family=VGAM::multinomial()),
                                     error=identity))
  
  # Check if the model did not converge
  if(inherits(model, "error")) return(NA_real_)
  
  # Compute z-statistic
  z <- .compute_z_statistic(model)
  
  # Remove intercept (if present).
  z <- z[names(z) != "(Intercept)"]
  
  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df=length(x)))
  
  # Check if the value is finite.
  if(all(!is.finite(p_value))) return(NA_real_)
  
  # Return p-value.
  return(unname(min(p_value, na.rm=TRUE)))
}



.get_available_icc_types <- function(){
  return(c("1", "2", "3"))
}



compute_icc <- function(x, feature, id_data, type="1"){
  
  # We start from the following equation: xij = mu + a_i + b_j + e_ij, with mu
  # the population mean, a_i the rater-dependent change, b_j the
  # subject-dependent change and eij an error with mean 0.
  #
  # * type = "1": ICC 1: We assume that each rated value is cased by a random
  # effect of the rater: i.e. the actual rotation is not associated with an
  # systematic change in value, and there is no interaction between rater and
  # subject. [Shrout 1979]. This means that a_i has mean 0, so that wij = ai +
  # eij. Put differently, raters are randomly chosen from a larger population
  # for each sample.
  #
  # * type = "2": ICC 2: There is a panel of raters, and the entire panel
  # evaluates each sample. However, the panel is assumed to be part of a larger
  # population of raters. Raters are assumed to be systematically biased.
  
  # * type = "3": ICC 3: There is a panel of raters and the entire panel evaluates each
  # subject. The panel is the entire population and raters are assumed to be
  # systematically biased.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- mu <- bj <- ai <- NULL
  
  # Determine identifier columns.
  sample_id_columns <- get_id_columns(id_depth="series")
  repetition_id_column <- get_id_columns(single_column="repetition_id")
  
  # Create data table from x and combine with id_data
  data <- data.table::data.table("value"=x)
  data <- cbind(id_data, data)
  
  # Calculate each parameter in the equation
  data[,"mu":=mean(value, na.rm=TRUE)][,"bj":=mean(value, na.rm=TRUE)-mu, by=sample_id_columns][,"ai":=mean(value, na.rm=TRUE)-mu, by=repetition_id_column][,"eij":=value-mu-bj-ai]
  
  # Calculate
  n_samples <- data.table::uniqueN(data, by=sample_id_columns)
  n_raters   <- data.table::uniqueN(data, by=repetition_id_column)
  
  # Calculate mean squared errors: msb between subjects (bj), msj between raters
  # (ai), mse of error (eij) and msw of error with rater (ai + eij).
  if(n_samples > 1){
    msb <- sum(data$bj^2, na.rm=TRUE) / (n_samples-1)
  }
  
  if(type=="1"){
    # Calculate mean squared of error with rater
    msw       <- (sum(data$eij^2, na.rm=TRUE) + sum(data$ai^2, na.rm=TRUE)) / (n_samples * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & msw==0) {
      icc              <- 1
      icc_panel        <- 1
      icc_ci_low <- 1; icc_ci_up <- 1; icc_panel_ci_low <- 1; icc_panel_ci_up <- 1
    } else {
      icc              <- (msb-msw) / (msb+ (n_raters-1) * msw)
      icc_panel        <- (msb-msw) / msb
      
      # Fisher score
      s_fisher         <- msb/msw
      s_fisher_low     <- s_fisher / stats::qf(0.975, n_samples-1, n_samples * (n_raters-1))
      s_fisher_up      <- s_fisher / stats::qf(0.025, n_samples-1, n_samples * (n_raters-1))
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low       <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up        <- (s_fisher_up  - 1) / (s_fisher_up  + n_raters - 1)
      icc_panel_ci_low <- 1 - 1/s_fisher_low
      icc_panel_ci_up  <- 1 - 1/s_fisher_up
    }
  }
  
  if(type=="2"){
    # Calculate mean squared error (mse) and mean squared rater error (msj)
    msj <- sum(data$ai^2, na.rm=TRUE) / (n_raters-1)
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_samples-1) * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & mse==0) {
      icc       <- 1
      icc_panel <- 1
      icc_ci_low <- 1; icc_ci_up <- 1; icc_panel_ci_low <- 1; icc_panel_ci_up <- 1
    } else {
      icc       <- (msb-mse) / (msb+ (n_raters-1) * mse + (n_raters/n_samples) * (msj-mse))
      icc_panel <- (msb-mse) / (msb + (msj-mse)/n_samples)
      
      # Determine confidence intervals
      vn <- (n_raters-1)*(n_samples-1) * (n_raters*icc*msj/mse +  n_samples*(1+(n_raters-1)*icc) - n_raters*icc)^2
      vd <- (n_samples-1) * n_raters^2 * icc^2 * (msj/mse)^2   + (n_samples*(1+(n_raters-1)*icc) - n_raters*icc)^2
      v  <- vn/vd
      thresh_low       <- stats::qf(0.975, n_samples-1, v)
      thresh_up        <- stats::qf(0.025, n_samples-1, v)
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low       <- n_samples * (msb - thresh_low*mse) / (thresh_low*(n_raters*msj+(n_raters*n_samples-n_raters-n_samples)*mse) + n_samples*msb)
      icc_ci_up        <- n_samples * (msb - thresh_up*mse)  / (thresh_up*(n_raters*msj+(n_raters*n_samples-n_raters-n_samples)*mse)  + n_samples*msb)
      icc_panel_ci_low <- icc_ci_low * n_raters / (1 + icc_ci_low*(n_raters-1) )
      icc_panel_ci_up  <- icc_ci_up * n_raters /  (1 + icc_ci_up*(n_raters-1) )
    }
  }
  
  if(type=="3"){
    # Calculate mean squared error (mse)
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_samples-1) * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & mse==0) {
      icc       <- 1
      icc_panel <- 1
      icc_ci_low <- 1; icc_ci_up <- 1; icc_panel_ci_low <- 1; icc_panel_ci_up <- 1
    } else {
      icc       <- (msb-mse) / (msb+ (n_raters-1) * mse)
      icc_panel <- (msb-mse) / msb
      
      # Fisher score
      s_fisher     <- msb/mse
      s_fisher_low <- s_fisher / stats::qf(0.975, n_samples-1, (n_samples-1) * (n_raters-1))
      s_fisher_up  <- s_fisher / stats::qf(0.025, n_samples-1, (n_samples-1) * (n_raters-1))
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low       <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up        <- (s_fisher_up  - 1) / (s_fisher_up  + n_raters - 1)
      icc_panel_ci_low <- 1 - 1/s_fisher_low
      icc_panel_ci_up  <- 1 - 1/s_fisher_up
    }
  }
  
  if(icc == 1.0){
    if(!is.finite(icc_ci_low)) icc_ci_low <- 1.0
    if(!is.finite(icc_ci_up)) icc_ci_up <- 1.0
  }
  
  if(icc_panel == 1.0){
    if(!is.finite(icc_ci_low)) icc_panel_ci_low <- 1.0
    if(!is.finite(icc_ci_up)) icc_panel_ci_up <- 1.0
  }
  
  return(data.table::data.table("feature"=feature,
                                "icc"=icc,
                                "icc_low"=icc_ci_low,
                                "icc_up"=icc_ci_up,
                                "icc_panel"=icc_panel,
                                "icc_panel_low"=icc_panel_ci_low,
                                "icc_panel_up"=icc_panel_ci_up))
}



applyContrastReference <- function(dt, dt_ref, method){
  # Updates data table dt based on score
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- orig_name <- score <- NULL
  
  # Check if any categorical variables are present
  if(is.null(dt_ref)) { return(dt) }
  
  # Split data table into non-categorical features and categorical features
  dt_non_cat <- dt[!name %in% dt_ref$ref_name, ]
  dt_cat     <- dt[name %in% dt_ref$ref_name, ]
  
  if(nrow(dt_cat)>0) {
    # Merge dt_cat with dt_ref
    dt_cat     <- merge(dt_cat, dt_ref, by.x="name", by.y="ref_name")
    
    # Summarise score by single value according to "method"
    if(method=="max")        { dt_cat <- dt_cat[, list(score=max(score, na.rm=TRUE)), by=orig_name]}
    if(method=="abs_max")    { dt_cat <- dt_cat[, list(score=max(abs(score), na.rm=TRUE)), by=orig_name]}
    if(method=="min")        { dt_cat <- dt_cat[, list(score=min(score, na.rm=TRUE)), by=orig_name]}
    if(method=="abs_min")    { dt_cat <- dt_cat[, list(score=min(abs(score), na.rm=TRUE)), by=orig_name]}
    if(method=="mean")       { dt_cat <- dt_cat[, list(score=mean(score, na.rm=TRUE)), by=orig_name]}
    if(method=="abs_mean")   { dt_cat <- dt_cat[, list(score=mean(abs(score), na.rm=TRUE)), by=orig_name]}
    if(method=="median")     { dt_cat <- dt_cat[, list(score=stats::median(score, na.rm=TRUE)), by=orig_name]}
    if(method=="abs_median") { dt_cat <- dt_cat[, list(score=stats::median(abs(score), na.rm=TRUE)), by=orig_name]}
    
    # Replace infinite/nan/etc values by NA
    dt_cat[!is.finite(score), "score":=as.double(NA)]
    
    # Change name of orig_name column to name
    data.table::setnames(dt_cat, "orig_name", "name")
    
    # Combine to single data table
    dt <- rbind(dt_non_cat, dt_cat)
  } else {
    # If no categorical variables are found, return only dt_non_cat
    dt <- dt_non_cat
  }

  return(dt)
}


.file_extension <- function(x){
  
  # Find the file extension by extracting the substring to the right of the
  # final period.
  extension <- stringi::stri_sub(x, from=stringi::stri_locate_last_fixed(x, '.')[1,2] + 1L)
  
  # Check for NA values in case an extension is missing.
  if(is.na(extension)) return("")
  
  return(extension)
}

# create_empty_calibration_table <- function(outcome_type){
#   if(outcome_type == "survival"){
#     calibration_table <- data.table::data.table("evaluation_time"=numeric(0),
#                                                 "expected"=numeric(0),
#                                                 "observed"=numeric(0),
#                                                 "km_var"=numeric(0),
#                                                 "n_g"=integer(0),
#                                                 "rep_id"=integer(0))
#     
#   } else if(outcome_type %in% c("binomial", "multinomial")){
#     
#     calibration_table <- data.table::data.table("pos_class"=character(0),
#                                                 "expected"=numeric(0),
#                                                 "observed"=numeric(0),
#                                                 "n_g"=integer(0),
#                                                 "n_pos"=integer(0),
#                                                 "n_neg"=integer(0),
#                                                 "rep_id"=integer(0))
#     
#   } else if(outcome_type %in% c("count", "continuous")){
#     
#     calibration_table <- data.table::data.table("expected"=numeric(0),
#                                                 "observed"=numeric(0),
#                                                 "n_g"=integer(0),
#                                                 "rep_id"=integer(0))
#     
#   } else {
#     ..error_outcome_type_not_implemented(outcome_type=outcome_type)
#   }
# 
#   return(calibration_table)
# }
# 
# 
# 
# strip_calibration_table <- function(calibration_data, outcome_type){
#   if(outcome_type == "survival"){
#     calibration_data <- calibration_data[, c("evaluation_time",
#                                              "expected",
#                                              "observed",
#                                              "km_var",
#                                              "n_g",
#                                              "rep_id"), with=FALSE]
#     
#   } else if(outcome_type %in% c("binomial", "multinomial")){
#     calibration_data <- calibration_data[, c("pos_class",
#                                              "expected",
#                                              "observed",
#                                              "n_g",
#                                              "n_pos",
#                                              "n_neg",
#                                              "rep_id"), with=FALSE]
#     
#   } else if(outcome_type %in% c("count", "continuous")){
#     calibration_data <- calibration_data[, c("expected",
#                                              "observed",
#                                              "n_g",
#                                              "rep_id"), with=FALSE]
#     
#   } else {
#     ..error_outcome_type_not_implemented(outcome_type=outcome_type)
#   }
#   
#   return(calibration_data)
#   
# }


harmonic_p_value <- function(x){
  
  if(data.table::is.data.table(x)){
    x <- x$p_value
    
    # Fix numeric issues due to very small p-values.
    x[x < 1E-15] <- 1E-15
    
    return(list("p_value"=harmonicmeanp::p.hmp(x, L=length(x))))
    
  } else {
    # Fix numeric issues due to very small p-values.
    x[x < 1E-15] <- 1E-15
    
    return(harmonicmeanp::p.hmp(x, L=length(x)))
  }
}


get_mode <- function(x) {
  # Ken Williams:
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(x)
  ux <- ux[which.max(tabulate(match(x, ux)))]

  return(ux)
}


get_estimate <- function(x, na.rm=TRUE){
  
  # Remove NA values.
  if(na.rm) x <- x[!is.na(x)]
  
  if(is.numeric(x)){
    
    # Determine the estimate.
    if(length(x) > 0){
      y <- mean(x)
      
    } else {
      y <- NA_real_
    }
    
  } else {
    # Determine the estimate.
    if(length(x) > 0){
      y <- get_mode(x)
      
    } else {
      y <- NA
    }
  }
  
  return(y)
}


.sanitise_dots <- function(class, ...){
  
  dots <- list(...)
  
  if(length(dots) == 0) return(dots)
  
  slot_names <- names(methods::getSlots(class))
  
  return(dots[intersect(names(dots), slot_names)])
}


strict.do.call <- function(what, args, quote = FALSE, envir = parent.frame()){
  # Only pass fully matching arguments. Side effect is that ... is always empty.
  # Use with care in case arguments need to be passed to another function.
  
  # Get arguments that can be passed.
  passable_argument_names <- intersect(names(formals(what)),
                                       names(args))
  
  return(do.call(what, args=args[passable_argument_names], quote=quote, envir=envir))
}


get_contrasts <- function(data, encoding_method="effect", drop_levels=TRUE, outcome_type=NULL, feature_columns=NULL){
  # Converts categorical data in to contrasts and return data table including contrast
  # TODO convert to S4 method.
  # TODO deprecate getContrasts function.
  
  # Determine columns with factors
  if(is.null(feature_columns) & !is.null(outcome_type)) {
    feature_columns <- get_feature_columns(x=data, outcome_type=outcome_type)
    
  } else if(is.null(feature_columns)){
    feature_columns <- colnames(data)
  }
  
  # Return original if no variables are available.
  if(length(feature_columns) == 0){
    return(list("dt_contrast"=data, "dt_ref"=NULL))
  }
  
  # Find column classes
  column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data)
  
  # Identify categorical columns
  factor_columns <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
  factor_columns <- feature_columns[factor_columns]
  
  # Return original if no categorical variables are available.
  if(length(factor_columns)==0){
    return(list("dt_contrast"=data, "dt_ref"=NULL))
  }
  
  # Initialise contrast list and reference list
  contrast_list  <- list()
  reference_list <- list()
  
  # Iterate over columns and encode the categorical variables.
  for(ii in seq_along(factor_columns)){
    # Extract data from current column.
    current_col <- factor_columns[ii]
    x <- data[[current_col]]
    
    # Drop missing levels
    if(drop_levels){
      x <- droplevels(x)
    }
    
    # Get names of levels and number of levels
    level_names <- levels(x)
    level_count <- nlevels(x)
    
    # Apply coding scheme
    if(encoding_method=="effect"){
      # Effect/one-hot encoding
      curr_contrast_list  <- lapply(level_names, function(ii, x) (as.numeric(x==ii)), x=x)
      
      # Check level_names for inconsistencies (i.e. spaces etc)
      contrast_names <- check_column_name(column_name=paste0(current_col, "_", level_names))
      names(curr_contrast_list) <- contrast_names
      
    } else if(encoding_method=="dummy"){
      # Dummy encoding. The first level is used as a reference (0).
      curr_contrast_list  <- lapply(level_names[2:level_count], function(ii, x) (as.numeric(x==ii)), x=x)
      
      # Check level_names for inconsistencies (i.e. spaces etc)
      contrast_names <- check_column_name(column_name=paste0(current_col, "_", level_names[2:level_count]))
      names(curr_contrast_list) <- contrast_names
      
    } else if(encoding_method=="numeric"){
      # Numeric encoding
      browser()
      curr_contrast_list <- list(as.numeric(x))
      
      # Set names
      contrast_names <- current_col
      names(curr_contrast_list) <- contrast_names
      
    } else {
      ..error_reached_unreachable_code(".get_contrasts: unknown encoding method encountered.")
    }
    
    # Add list of generated contrast to contrast list
    contrast_list <- append(contrast_list, curr_contrast_list)
    
    # Add data table with reference and contrast names to the reference list
    reference_list[[ii]] <- data.table::data.table("orig_name"=current_col, "ref_name"=contrast_names)
  }
  
  # Check if the original data set contained any features other than categorical
  # features. Combine original data and contrasts into data table with added
  # contrast data, and get a list with reference names.
  if(length(factor_columns) == ncol(data)){
    contrast_table = data.table::as.data.table(contrast_list)
    
  } else {
    contrast_table = cbind(data[, !factor_columns, with=FALSE],
                           data.table::as.data.table(contrast_list))
  }
  
  # Return as list
  return(list("dt_contrast"=contrast_table, "dt_ref"=rbindlist(reference_list)))
}



getContrasts <- function(dt, method="effect", drop_levels=TRUE, outcome_type=NULL){
  return(get_contrasts(data=dt, encoding_method=method, drop_levels=drop_levels, outcome_type=outcome_type))
}


get_placeholder_vimp_table <- function(){
  return(data.table::data.table("name"=character(0), "rank"=numeric(0), "score"=numeric(0), "multi_var"=logical(0)))
}



get_id_columns <- function(id_depth="repetition", single_column=NULL){
  
  # Check that until_depth is correctly specified.
  if(!id_depth %in% c("batch", "sample", "series", "repetition")){
    ..error_value_not_allowed(x=id_depth, var_name="id_depth", values=c("batch", "sample", "series", "repetition"))
  }
  
  if(is.null(single_column)){
    # Generate the names of the non-feature columns
    id_columns <- switch(id_depth,
                         "batch" = "batch_id",
                         "sample" = c("batch_id", "sample_id"),
                         "series" = c("batch_id", "sample_id", "series_id"),
                         "repetition" = c("batch_id", "sample_id", "series_id", "repetition_id"))
    
  } else {
    id_columns <- switch(single_column,
                         "batch" = "batch_id",
                         "sample" = "sample_id",
                         "series" = "series_id",
                         "repetition" = "repetition_id")
  }
  
  
  return(id_columns)
}


get_object_file_name <- function(learner,
                                 fs_method,
                                 project_id,
                                 data_id,
                                 run_id,
                                 pool_data_id=NULL,
                                 pool_run_id=NULL,
                                 object_type,
                                 is_ensemble=NULL,
                                 is_validation=NULL,
                                 with_extension=TRUE,
                                 dir_path=NULL){
  # Generate file name for an object
  
  if(!object_type %in% c("familiarModel", "familiarEnsemble", "familiarData")){
    stop("The object type was not recognised.")
  }
  
  # Generate the basic string
  base_str <- paste0(project_id, "_", learner, "_", fs_method, "_", data_id, "_", run_id)
  
  if(object_type=="familiarModel"){
    # For familiarModel objects
    
    output_str <- paste0(base_str, "_model")
  } else if(object_type=="familiarEnsemble") {
    # For familiarEnsemble objects
    
    if(is.null(is_ensemble)){
      stop("The \"is_ensemble\" parameter is not set to TRUE or FALSE.")
    }
    
    output_str <- paste0(base_str, "_", ifelse(is_ensemble, "ensemble", "pool"))
    
  } else if(object_type=="familiarData"){
    # For familiarData objects
    
    if(is.null(is_ensemble)){
      stop("The \"is_ensemble\" parameter is not set to TRUE or FALSE.")
    }
    
    if(is.null(is_validation)){
      stop("The \"is_validation\" parameter is not set to TRUE or FALSE.")
    }
    
    if(is.null(pool_data_id) | is.null(pool_run_id)){
      stop()
    }
    
    output_str <- paste0(base_str, "_", ifelse(is_ensemble, "ensemble", "pool"),
                         "_", pool_data_id, "_", pool_run_id,
                         "_", ifelse(is_validation, "validation", "development"), "_data")
    
  }
  
  if(with_extension){
    # Add extension
    output_str <- paste0(output_str, ".RDS")
  }
  
  # Join with dir_path
  if(!is.null(dir_path)){
    output_str <- normalizePath(file.path(dir_path, output_str), mustWork=FALSE)
  }
  
  return(output_str)
}



get_object_dir_path <- function(dir_path, object_type, learner=NULL, fs_method=NULL){
  # Generate the directory path to an object
  
  if(!object_type %in% c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection")){
    stop("The object type was not recognised.")
  }
  
  if(object_type %in% c("familiarModel", "familiarEnsemble")){
    
    if(is.null(learner) & is.null(fs_method)){
      return(normalizePath(file.path(dir_path), mustWork=FALSE))
    } else {
      return(normalizePath(file.path(dir_path, learner, fs_method), mustWork=FALSE))
    }
    
  } else if(object_type %in% c("familiarData", "familiarCollection")){
    
    return(normalizePath(file.path(dir_path)))
    
  }
}


extract_from_slot <- function(object_list, slot_name, slot_element=NULL, na.rm=FALSE){
  # Extracts values from a slot in an object
  # Providing slot_element allows extraction from a list in a slot
  
  if(is.null(slot_element)){
    slot_values <- sapply(object_list, slot, name=slot_name)
    
  } else {
    slot_values <- sapply(object_list, function(object, slot_name, slot_element){
      list_element <- slot(object=object, name=slot_name)
      if(!is.list(list_element)){
        return(NULL)
      } else {
        return(list_element[[slot_element]])
      }
    }, slot_name=slot_name, slot_element=slot_element)
  }
  
  if(na.rm){
    # First remove NULL
    slot_values <- slot_values[!sapply(slot_values, is.null)]
    
    # Then remove NA
    if(is.numeric(slot_values) | is.logical(slot_values) | is.character(slot_values)){
      slot_values <- slot_values[!sapply(slot_values, is.na)]
    }
    
    # Check if the slot values are numeric, and remove infinite values if so
    if(is.numeric(slot_values)){
      slot_values <- slot_values[!sapply(slot_values, is.infinite)]
    }
  }
  
  return(slot_values)
}


all_empty_slot <- function(object_list, slot_name, slot_element=NULL){
  # Determine if all slots of objects in the object list are empty
  
  # Extract slot values
  slot_values <- extract_from_slot(object_list=object_list, slot_name=slot_name,
                                   slot_element=slot_element, na.rm=TRUE)
  
  # Determine if there are any slot values that have a value
  if(length(slot_values) == 0){
    return(TRUE)
  } else {
    return(FALSE)
  }

}


process_random_forest_survival_predictions <- function(event_matrix, event_times, prediction_table, time, type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  event_time <- NULL
  
  # Set id columns
  id_columns <- get_id_columns()
  
  # Make a local copy of the prediction_table
  prediction_table <- data.table::copy(prediction_table)
  
  # Convert event_matrix to a matrix.
  if(!is.matrix(event_matrix)){
    event_matrix <- matrix(data=event_matrix, ncol=length(event_matrix))
  }
  
  # Combine with identifiers and cast to table.
  event_table <- cbind(prediction_table[, mget(id_columns)],
                       data.table::as.data.table(event_matrix))
  
  # Remove duplicate entries
  event_table <- unique(event_table, by=id_columns)
  
  # Melt to a long format.
  event_table <- data.table::melt(event_table,
                                  id.vars=id_columns,
                                  variable.name="time_variable",
                                  value.name="value")
  
  # Create conversion table to convert temporary variables into
  # the event times.
  conversion_table <- data.table::data.table("time_variable"=paste0("V", seq_along(event_times)),
                                             "event_time"=event_times)
  
  # Add in event times
  event_table <- merge(x=event_table, y=conversion_table, on="time_variable")
  
  # Drop the time_variable column
  event_table[, "time_variable":=NULL]
  
  if(time %in% event_times){
    # Get the event time directly.
    event_table <- event_table[event_time == time, ]
    
    # Remove event_time column and rename the value column to predicted_outcome.
    event_table[, "event_time":=NULL]
    data.table::setnames(x=event_table, old="value", new="predicted_outcome")
    
  } else {
    
    # Add starting values.
    if(! 0 %in% event_times){
      # Create initial table
      initial_event_table <- data.table::copy(event_table[event_time == event_times[1]])
      
      # Update values
      if(type == "cumulative_hazard"){
        initial_event_table[, ":="("value"=0.0, "event_time"=0)]
        
      } else if(type == "survival"){
        initial_event_table[, ":="("value"=1.0, "event_time"=0)]
        
      } else {
        ..error_reached_unreachable_code(paste0("process_random_forest_survival_predictions: type was not recognised: ", type))
      }
      
      # Combine with the event table.
      event_table <- rbind(initial_event_table, event_table)
    }
    
    # Now, interpolate at the given time point.
    event_table <- lapply(split(event_table, by=id_columns), function(sample_table, time, id_columns){
      
      # Interpolate values at the given time.
      value <- stats::approx(x=sample_table$event_time,
                             y=sample_table$value,
                             xout=time,
                             rule=2)$y
      
      # Create an output table
      output_table <- data.table::copy(sample_table[1, mget(id_columns)])
      output_table[, "predicted_outcome":=value]
      
      return(output_table)
    }, time=time, id_columns=id_columns)
    
    # Concatenate to single table.
    event_table <- data.table::rbindlist(event_table)
  }
  
  if(type == "cumulative_hazard"){
    # Remove predicted_outcome from the prediction table.
    prediction_table[, "predicted_outcome":=NULL]
    
  } else {
    # Remove survival_probability from the prediction table.
    prediction_table[, "survival_probability":=NULL]
    
    # Update response column.
    data.table::setnames(event_table,
                         old="predicted_outcome",
                         new="survival_probability")
  }
  
  # Merge the event table into the prediction table.
  prediction_table <- merge(x=prediction_table, y=event_table, by=id_columns)
  
  return(prediction_table)
}


is_singular_data <- function(x){
  # Checks if the input data is singular (i.e. only has one value)
  class_x <- class(x)

  # Drop any NA data.
  x <- x[!is.na(x)]
  if(length(x) <= 1) return(TRUE)
  
  if(any(class_x %in% "factor")){
    if(length(levels(droplevels(x)))==1){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else if(any(class_x %in% c("numeric", "integer", "logical"))) {
    
    # Drop any infinite data
    x <- x[is.finite(x)]
    if(length(x) <= 1) return(TRUE)
    
    if(stats::var(x, na.rm=TRUE)==0){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else if(any(class_x %in% c("character"))) {
    if(length(unique(x))==1){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else {
    return(TRUE)
  }
}


is_valid_data <- function(x){
  # Checks validity of input data x
  
  class_x <- class(x)

  if(any(class_x %in% c("numeric", "integer", "logical"))){
    return(is.finite(x))
    
  } else if(any(class_x %in% c("factor"))){
    return(!is.na(x))
    
  } else if(any(class_x %in% "character")){
    return(!(is.na(x) | tolower(x) %in% c("na","nan","inf","-inf")))
    
  } else {
    return(FALSE)
    
  }
}


.compute_z_statistic <- function(model){
  
  mu <- cov_matrix <- NULL
  
  if(is(model, "familiarModel")){
    # Attempt to extract the estimates and the covariance matrix from the
    # familiarModel object.
    if(!is.null(model@trimmed_function$coef)){
      mu <- model@trimmed_function$coef
    }
    
    if(!is.null(model@trimmed_function$vcov)){
      cov_matrix <- model@trimmed_function$vcov
    }
    
    # Extract the model itself.
    model <- model@model
  }
  
  if(is(model, "vglm")){
    if(is.null(mu)) mu <- VGAM::coefvlm(model)
    if(is.null(cov_matrix)) cov_matrix <- VGAM::vcovvlm(model)
    
  } else {
    if(is.null(mu)) mu <- stats::coef(model)
    if(is.null(cov_matrix)) cov_matrix <- stats::vcov(model)
  }
  
  if(is.matrix(mu)){
    stdevs <- matrix(sqrt(diag(stats::vcov(model))), ncol=ncol(mu), byrow=TRUE)
    
  } else {
    stdevs <- sqrt(diag(cov_matrix))
    
    # Order standard deviations according to the estimates.
    if(is.null(names(stdevs))){
      stdevs <- stdevs[seq_along(mu)]
      
    } else {
      stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
    }
  }
  
  # Compute z-score
  z  <- mu / stdevs
  
  # Return z-score, as p-values can become very small.
  return(abs(z))
}



is_any <- function(object, class2){
  # Tests whether the object is any of the classes in class2.
  return(any(sapply(class2, function(class, object) is(object, class2=class), object=object)))
}


fivenum_summary <- function(x, na.rm=FALSE){
  
  # Compute fivenumber summary
  y <- stats::fivenum(x=x, na.rm=na.rm)
  
  # Return as data.table
  return(data.table::data.table("min"=y[1],
                                "Q1"=y[2],
                                "median"=y[3],
                                "Q3"=y[4],
                                "max"=y[5]))
} 



trim <- function(x, fraction=0.1){
  if(fraction < 0.0 | fraction > 0.5){
    stop("Trimming fraction should be between 0.0 and 0.5.")
  }

  # Determine thresholds based on fraction.
  threshold <- stats::quantile(x=x, probs=c(fraction, 1-fraction), na.rm=TRUE)

  # Set mask
  x_mask <- x >= threshold[1] & x <= threshold[2]

  # Return trimmed array
  return(x[x_mask])
}



winsor <- function(x, fraction=0.1){
  if(fraction < 0.0 | fraction > 0.5){
    stop("Winsoring fraction should be between 0.0 and 0.5.")
  }

  # Determine thresholds based on fraction.
  threshold <- stats::quantile(x=x, probs=c(fraction, 1-fraction), na.rm=TRUE)

  # Values outside valid range are assigned edge values
  x[x < threshold[1]] <- threshold[1]
  x[x > threshold[2]] <- threshold[2]

  return(x)
}



unique_na <- function(...){
  values <- do.call(unique, args=list(...))
  
  return(values[!is.na(values)])
}

#' Create a waiver object
#'
#' This function is functionally identical to `ggplot2::waiver()` function and
#' creates a waiver object. A waiver object is an otherwise empty object that
#' serves the same purpose as `NULL`, i.e. as placeholder for a default value.
#' Because `NULL` can sometimes be a valid input argument, it can therefore not
#' be used to switch to an internal default value.
#'
#' @return waiver object
#' @export
#' @md
waiver <- function(){ structure(list(), class = "waiver") }

#' Internal test to see if an object is a waiver
#'
#' This function tests if the object was created by the `waiver` function. This
#' function is functionally identical to `ggplot2:::is.waive()`.
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` for objects that are waivers, `FALSE` otherwise.
#' @md
#' @keywords internal
is.waive <- function(x){ return(inherits(x, "waiver")) }


#' Encapsulate path
#'
#' This function is used to encapsulate paths to allow for behaviour switches.
#' One use is for example when plotting. The plot_all method will encapsulate a
#' path so that plots may be saved to a directory structure. Other plot methods,
#' e.g. plot_model_performance do not encapsulate a path, and if the user calls
#' these functions directly, the plot may be written to the provided path
#' instead of a directory structure.
#'
#' @return encapsulated_path object
#' @md
#' @keywords internal
encapsulate_path <- function(path){
  structure(path, class="encapsulated_path")
}


#' Internal test for encapsulated_path
#'
#' This function tests if the object is an `encapsulated_path` object.
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` for objects that are `encapsulated_path`, `FALSE` otherwise.
#' @md
#' @keywords internal
is.encapsulated_path <- function(x){ return(inherits(x, "encapsulated_path")) }


quiet <- function(x) { 
  # Removes all output to console.
  
  sink(nullfile()) 
  on.exit(sink()) 
  
  invisible(utils::capture.output(x, file=nullfile(), type="message"))
} 

.append_new <- function(l, new){

  # Find the names of list elements in l and new
  existing_names <- names(l)
  new_names <- names(new)
  
  # Find which elements should be migrated.
  migrate_elements <- setdiff(new_names, existing_names)
  if(length(migrate_elements) == 0){ return(l) }
  
  # Drop any duplicate elements.
  duplicate_elements <- intersect(new_names, existing_names)
  for(duplicate_element in duplicate_elements){
    new[[duplicate_element]] <- NULL
  }
  
  return(append(l, new))
}


is_subclass <- function(class_1, class_2){
  # Find out if class_1 is a subclass of class_2
  extending_classes <- methods::extends(class_1)
  
  # If class 2 is not in the classes from which class 1 inherits, class 1 cannot
  # be a subclass of class 2.
  if(!class_2 %in% extending_classes) return(FALSE)
  
  # If the classes are the same, class 1 cannot be a subclass of class 2.
  if(class_1 == class_2) return(FALSE)
  
  # The classes are ordered by distance. Therefore class 2 cannot be a subclass
  # of class 1 if it is less distant.
  if(extending_classes[1] == class_2) return(FALSE)
  
  return(TRUE)
}


paste_s <- function(...){
  dots <- c(...)
  
  if(length(dots) > 2){
    # 
    initial_string <- paste0(head(dots, n=length(dots)-2), collapse=", ")
    
    final_string <- paste0(tail(dots, n=2), collapse=" and ")
    
    return(paste0(c(initial_string, final_string), collapse=", "))
    
  } else if(length(dots) == 2){
    
    return(paste0(dots, collapse=" and "))
  } else {
    return(paste0(dots))
  }
}


.as_preprocessing_level <- function(x){
  
  if(is(x, "dataObject")) x <- x@preprocessing_level
  
  # Available pre-processing levels
  preprocessing_levels <- c("none",
                            "signature",
                            "transformation",
                            "normalisation",
                            "batch_normalisation",
                            "imputation",
                            "clustering")
  
  if(!all(x %in% preprocessing_levels)){
    ..error_reached_unreachable_code(".as_preprocessing_level: one or more of x could not be matched to preprocessing levels.")
  }
  
  return(factor(x=x,
                levels=preprocessing_levels,
                ordered=TRUE))
}


.flatten_nested_list <- function(x, flatten=FALSE){
  
  # Identify names of elements.
  element_names <- unique(unlist(lapply(x, names)))
  
  # Create a flattened list.
  flattened_list <- lapply(element_names, function(element_name, x, flatten){
    
    # Obtain content stored in each element.
    element_content <- NULL
    
    # Iterate over nested lists, and contents of the element.
    for(ii in seq_along(x)){
      if(flatten){
        element_content <- c(element_content, x[[ii]][[element_name]])
        
      } else {
        # Treat data.table differently, because c() casts data.tables to a list.
        element_content <- c(element_content, list(x[[ii]][[element_name]]))
      }
    }
    
    # Set names of elements
    if(!is.null(names(x))){
      if(length(names(x)) == length(element_content)){
        names(element_content) <- names(x)
      }
    } 
    
    return(element_content)
  },
  x = x,
  flatten = flatten)
  
  # Set names of the list elements.
  names(flattened_list) <- element_names
  
  return(flattened_list)
}


dmapply <- function(FUN, ..., MoreArgs=NULL){
  # mapply for use within data.table. This parses the result of FUN to a flat
  # list. data.table then adds the contents of the list as columns.
  
  # Apply function.
  x <- mapply(FUN, ..., MoreArgs=MoreArgs, SIMPLIFY=FALSE)
  
  # Combine to data.table.
  x <- data.table::rbindlist(x, use.names=TRUE)
  
  # Return as list.
  return(as.list(x))
}


dlapply <- function(X, FUN, ...){
  # lapply for use within data.table. This parses the result of FUN to a flat
  # list. data.table then adds the contents of the list as columns.
  
  # Apply function.
  x <- lapply(X, FUN, ...)
  
  # Combine to data.table.
  x <- data.table::rbindlist(x, use.names=TRUE)
  
  # Return as list.
  return(as.list(x))
}
