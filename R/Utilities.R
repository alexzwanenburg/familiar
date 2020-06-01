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
                                     outcome_data=data_obj@data[, mget(outcome_columns)])
  
  return(coefficient_p_values)
}


.univariate_cox_regression_test <- function(x, outcome_data){
  # Cox regression model for univariable analysis
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table("value"=x,
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
  model_obj <- tryCatch({coxph(Surv(outcome_time, outcome_event) ~ .,
                               data=data,
                               na.action=stats::na.omit,
                               model=TRUE) },
                        error=function(err) return(NULL))

  # Check if the model did not converge
  if(is.null(model_obj)) return(NA_real_)
  
  # Return wald test p-value
  return(unname(summary(model_obj)$waldtest[3]))
}



.univariate_linear_regression_test <- function(x, outcome_data){
  # Gaussian regression for univariable analysis
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table("value"=x, "outcome"=outcome_data$outcome)
  
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
  model_obj <- tryCatch({stats::glm(outcome ~ .,
                                    data=data,
                                    family=stats::gaussian(link="identity"))},
                        error=function(err) return(NULL))
  
  # Check if the model did not converge
  if(is.null(model_obj)) return(NA_real_)
  
  # Get coefficient p-value by removing intercept from the model
  coef_p_val <- regrLocTest(model_obj)
  coef_p_val <- coef_p_val[names(coef_p_val)!="(Intercept)"]
  
  if(all(!is.finite(coef_p_val))) return(NA_real_)
  
  return(unname(min(coef_p_val, na.rm=TRUE)))
}



.univariate_poisson_regression_test <- function(x, outcome_data){
  # Poisson regression for univariable analysis with count-type outcomes
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table("value"=x, "outcome"=outcome_data$outcome)
  
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
  model_obj <- tryCatch({stats::glm(outcome~.,
                                    data=data,
                                    family=stats::poisson(link="log"))},
                        error=function(err) return(NULL))
  
  # Check if the model did not converge
  if(is.null(model_obj)) return(NA_real_)
  
  # Get coefficient p-value by removing intercept from the model
  coef_p_val <- regrLocTest(model_obj)
  coef_p_val <- coef_p_val[names(coef_p_val)!="(Intercept)"]
  
  if(all(!is.finite(coef_p_val))) return(NA_real_)
  
  return(unname(min(coef_p_val, na.rm=TRUE)))
}



.univariate_binomial_logistic_regression_test <- function(x, outcome_data){
  #Binomial model for univariable analysis using logistic regression
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  browser()
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table("value"=x, "outcome"=outcome_data$outcome)
  
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
  
  # We train two models, with and without intercept, to avoid the Hauck-Donner
  # effect.
  p_values <- numeric(2)
  
  # Construct model
  model_obj <- tryCatch({stats::glm(outcome ~ .,
                                    data=data,
                                    family=stats::binomial(link="logit"))},
                        error=function(err) return(NULL))
  
  # Check if the model did not converge
  if(is.null(model_obj)){
    p_values[1] <- NA_real_
    
  } else {
    
    # Get coefficient p-value by removing intercept from the model
    coef_p_val <- regrLocTest(model_obj)
    coef_p_val <- coef_p_val[names(coef_p_val)!="(Intercept)"]
    
    p_values[1] <- ifelse(any(is.finite(coef_p_val)),
                          unname(min(coef_p_val, na.rm=TRUE)),
                          NA_real_)
  }
  
  # Construct model without an intercept
  model_obj <- tryCatch({stats::glm(outcome ~ . -1,
                                    data=data,
                                    family=stats::binomial(link="logit"))},
                        error=function(err) return(NULL))
  
  if(is.null(model_obj)){
    p_values[2] <- NA_real_
    
  } else {
    # Get coefficient p-value
    coef_p_val <- regrLocTest(model_obj)
    
    p_values[2] <- ifelse(any(is.finite(coef_p_val)),
                          unname(min(coef_p_val, na.rm=TRUE)),
                          NA_real_)
  }
  
  # Find the p_value
  p_value <- min(p_values, na.rm=TRUE)
  
  if(!is.finite(p_value)) return(NA_real_)
  
  return(p_value)
}



.univariate_multinomial_logistic_regression_test <- function(x, outcome_data){
  # Multinomial model for univariable analysis using logistic regression
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Check if any data was provided.
  if(length(x) == 0) return(NA_real_)
  
  # Combine the feature value column with the outcome
  data <- data.table("value"=x, "outcome"=outcome_data$outcome)
  
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
  
  # We train two models, with and without intercept, to avoid the Hauck-Donner
  # effect.
  p_values <- numeric(2)
  
  # Construct model
  model_obj <- tryCatch({VGAM::vglm(outcome ~ .,
                                    data=data,
                                    family=VGAM::multinomial())},
                        error=function(err) return(NULL))
  
  # Check if the model did not converge
  if(is.null(model_obj)){
    p_values[1] <- NA_real_
    
  } else {
    
    # Get coefficient p-value by removing intercept from the model
    coef_p_val <- regrLocTest(model_obj)
    coef_p_val <- coef_p_val[!grepl(pattern="(Intercept)", x=names(coef_p_val), fixed=TRUE)]
    
    p_values[1] <- ifelse(any(is.finite(coef_p_val)),
                          unname(min(coef_p_val, na.rm=TRUE)),
                          NA_real_)
  }
  
  # Construct model without an intercept
  model_obj <- tryCatch({VGAM::vglm(outcome ~ . -1,
                                    data=data,
                                    family=VGAM::multinomial())},
                        error=function(err) return(NULL))
  
  if(is.null(model_obj)){
    p_values[2] <- NA_real_
    
  } else {
    # Get coefficient p-value
    coef_p_val <- regrLocTest(model_obj)
    
    p_values[2] <- ifelse(any(is.finite(coef_p_val)),
                          unname(min(coef_p_val, na.rm=TRUE)),
                          NA_real_)
  }
  
  # Find the p_value
  p_value <- min(p_values, na.rm=TRUE)
  
  if(!is.finite(p_value)) return(NA_real_)
  
  return(p_value)
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
  value <- mu <- subject_id <- cohort_id <- repetition_id <- bj <- ai <- NULL
  
  # Create data table from x and combine with id_data
  data <- data.table::data.table("value"=x)
  data <- cbind(id_data, data)
  
  # Calculate each parameter in the equation
  data[,"mu":=mean(value, na.rm=TRUE)][,"bj":=mean(value, na.rm=TRUE)-mu, by=list(subject_id,cohort_id)][,"ai":=mean(value, na.rm=TRUE)-mu, by=list(repetition_id)][,"eij":=value-mu-bj-ai]
  
  # Calculate
  n_subjects <- data.table::uniqueN(data, by="subject_id")
  n_raters   <- data.table::uniqueN(data, by="repetition_id")
  
  # Calculate mean squared errors: msb between subjects (bj), msj between raters (ai), mse of error (eij) and msw of error with rater (ai + eij)
  if(n_subjects > 1){
    msb <- sum(data$bj^2, na.rm=TRUE) / (n_subjects-1)
  }
  
  if(type=="1"){
    # Calculate mean squared of error with rater
    msw       <- (sum(data$eij^2, na.rm=TRUE) + sum(data$ai^2, na.rm=TRUE)) / (n_subjects * (n_raters-1))
    
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
      s_fisher_low     <- s_fisher / stats::qf(0.975, n_subjects-1, n_subjects * (n_raters-1))
      s_fisher_up      <- s_fisher / stats::qf(0.025, n_subjects-1, n_subjects * (n_raters-1))
      
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
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_subjects-1) * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & mse==0) {
      icc       <- 1
      icc_panel <- 1
      icc_ci_low <- 1; icc_ci_up <- 1; icc_panel_ci_low <- 1; icc_panel_ci_up <- 1
    } else {
      icc       <- (msb-mse) / (msb+ (n_raters-1) * mse + (n_raters/n_subjects) * (msj-mse))
      icc_panel <- (msb-mse) / (msb + (msj-mse)/n_subjects)
      
      # Determine confidence intervals
      vn <- (n_raters-1)*(n_subjects-1) * (n_raters*icc*msj/mse +  n_subjects*(1+(n_raters-1)*icc) - n_raters*icc)^2
      vd <- (n_subjects-1) * n_raters^2 * icc^2 * (msj/mse)^2   + (n_subjects*(1+(n_raters-1)*icc) - n_raters*icc)^2
      v  <- vn/vd
      thresh_low       <- stats::qf(0.975, n_subjects-1, v)
      thresh_up        <- stats::qf(0.025, n_subjects-1, v)
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low       <- n_subjects * (msb - thresh_low*mse) / (thresh_low*(n_raters*msj+(n_raters*n_subjects-n_raters-n_subjects)*mse) + n_subjects*msb)
      icc_ci_up        <- n_subjects * (msb - thresh_up*mse)  / (thresh_up*(n_raters*msj+(n_raters*n_subjects-n_raters-n_subjects)*mse)  + n_subjects*msb)
      icc_panel_ci_low <- icc_ci_low * n_raters / (1 + icc_ci_low*(n_raters-1) )
      icc_panel_ci_up  <- icc_ci_up * n_raters /  (1 + icc_ci_up*(n_raters-1) )
    }
  }
  
  if(type=="3"){
    # Calculate mean squared error (mse)
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_subjects-1) * (n_raters-1))
    
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
      s_fisher_low <- s_fisher / stats::qf(0.975, n_subjects-1, (n_subjects-1) * (n_raters-1))
      s_fisher_up  <- s_fisher / stats::qf(0.025, n_subjects-1, (n_subjects-1) * (n_raters-1))
      
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
  
  return(data.table::data.table("name"=feature,
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


# createNonValidPredictionTable <- function(dt, outcome_type){
#   # Create skeleton
#   dt_pred <- dt[, get_non_feature_columns(x=outcome_type), with=FALSE]
# 
#   # Add prediction columns
#   if(outcome_type %in% c("survival", "continuous", "count")){
#     # For survival and continuous outcomes, a single column is required
#     dt_pred$outcome_pred         <- as.double(NA)
#   } else if (outcome_type %in% c("binomial", "multinomial")){
#     # For binomial and multinomial outcomes, we add both predicted class and predicted class probabilities
#     dt_pred$outcome_pred_class   <- as.character(NA)
# 
#     # Add class probabilities
#     outcome_pred_class_prob_cols <- check_column_name(column_name=paste0("outcome_pred_prob_", levels(dt$outcome)))
#     for(ii in 1:length(outcome_pred_class_prob_cols)){
#       dt_pred[, (outcome_pred_class_prob_cols[ii]):=as.double(NA)]
#     }
#   } else if(outcome_type == "competing_risk"){
#     ..error_outcome_type_not_implemented(outcome_type)
#   }
# 
#   # Return without valid data prediction table
#   return(dt_pred)
# }

any_predictions_valid <- function(prediction_table, outcome_type){
  
  if(is_empty(prediction_table)){
    return(FALSE)
  }
  
  if(outcome_type %in% c("survival", "continuous", "count")){
    return(any(is.finite(prediction_table$predicted_outcome)))
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    return(!all(is.na(prediction_table$predicted_class)))
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
}


create_empty_calibration_table <- function(outcome_type){
  if(outcome_type == "survival"){
    calibration_table <- data.table::data.table("evaluation_time"=numeric(0),
                                                "expected"=numeric(0),
                                                "observed"=numeric(0),
                                                "km_var"=numeric(0),
                                                "n_g"=integer(0),
                                                "rep_id"=integer(0))
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    
    calibration_table <- data.table::data.table("pos_class"=character(0),
                                                "expected"=numeric(0),
                                                "observed"=numeric(0),
                                                "n_g"=integer(0),
                                                "n_pos"=integer(0),
                                                "n_neg"=integer(0),
                                                "rep_id"=integer(0))
    
  } else if(outcome_type %in% c("count", "continuous")){
    
    calibration_table <- data.table::data.table("expected"=numeric(0),
                                                "observed"=numeric(0),
                                                "n_g"=integer(0),
                                                "rep_id"=integer(0))
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type=outcome_type)
  }

  return(calibration_table)
}



strip_calibration_table <- function(calibration_data, outcome_type){
  if(outcome_type == "survival"){
    calibration_data <- calibration_data[, c("evaluation_time",
                                             "expected",
                                             "observed",
                                             "km_var",
                                             "n_g",
                                             "rep_id"), with=FALSE]
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    calibration_data <- calibration_data[, c("pos_class",
                                             "expected",
                                             "observed",
                                             "n_g",
                                             "n_pos",
                                             "n_neg",
                                             "rep_id"), with=FALSE]
    
  } else if(outcome_type %in% c("count", "continuous")){
    calibration_data <- calibration_data[, c("expected",
                                             "observed",
                                             "n_g",
                                             "rep_id"), with=FALSE]
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type=outcome_type)
  }
  
  return(calibration_data)
  
}



get_mode <- function(x) {
  # Ken Williams:
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(x)
  ux <- ux[which.max(tabulate(match(x, ux)))]

  return(ux)
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
  return(data.table::data.table("score"=numeric(0), "name"=character(0), "rank"=numeric(0), "multi_var"=logical(0)))
}



getEmptyVimp <- function(){
  # Returns an empty variable importance data table

  dt_vimp <- data.table("score"=numeric(0), "name"=character(0), "rank"=numeric(0), "multi_var"=logical(0))

  return(dt_vimp)
}



get_id_columns <- function(include_repetition_id=TRUE){
  # Generate the names of the non-feature columns
  if(include_repetition_id){
    return(c("subject_id", "cohort_id", "repetition_id"))
    
  } else {
    return(c("subject_id", "cohort_id"))
  }
}

# getPredictedOutcomeColumn <- function(outcome_type){
#   # Returns column name with predicted outcomes, given the outcome type
# 
#   if(outcome_type %in% c("survival", "continuous", "count")){
#     return("outcome_pred")
#   } else if(outcome_type %in% c("binomial", "multinomial")){
#     return("outcome_pred_class")
#   } else if(outcome_type == "competing_risk"){
#     stop()
#   }
# }



get_object_file_name <- function(learner, fs_method, project_id, data_id, run_id, pool_data_id=NULL, pool_run_id=NULL,
                                 object_type, is_ensemble=NULL, is_validation=NULL, with_extension=TRUE, dir_path=NULL){
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
    slot_values <- slot_values[!sapply(slot_values, is.null)]
    
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
  
  # Remove predicted_outcome from the prediction table.
  prediction_table[, "predicted_outcome":=NULL]
  
  # Then merge the event table into the prediction table.
  prediction_table <- merge(x=prediction_table, y=event_table, on=id_columns)
  
  return(prediction_table)
}


# getStandardEvalColumns <- function(){
#   return(c("data_id", "run_id", "model_id", "repetition_id", "fs_method", "learner", "perturbation",
#            "perturb_level", "assignment", "is_ensemble"))
# }

# getEnsembleEvalColumns <- function(){
#   # As getStandardEvalColumns(), but without model_id and repetition_id
#   return(c("data_id", "run_id", "fs_method", "learner", "perturbation",
#            "perturb_level", "assignment"))
# }

# insertPlaceholderValues <- function(x){
#   # Fills NA data with placeholder values
# 
#   class_x <- class(x)
#   if(class_x == "numeric"){
#     x[!is.finite(x)] = as.double(1)
#     return(x) }
#   if(class_x == "integer"){
#     x[!is.finite(x)] = as.integer(1)
#     return(x) }
#   if(class_x == "logical"){
#     x[!is.finite(x)] = as.logical(1)
#     return(x) }
#   if(class_x == "factor"){
#     x[is.na(x)] <- levels(x)[1]
#     return(x) }
#   if(class_x == "character"){
#     x[(is.na(x) | tolower(x) %in% c("na","nan","inf","-inf"))] <- "placeholder"
#     return(x) }
# }


is_singular_data <- function(x){
  # Checks if the input data is singular (i.e. only has one value)
  class_x <- class(x)

  if(any(class_x %in% "factor")){
    if(length(levels(droplevels(x)))==1){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else if(any(class_x %in% c("numeric", "integer", "logical"))) {
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


coefficient_one_sample_z_test <- function(model, mean=0){
  
  # Provides location test for unrestriced regression models
  if(inherits(model, "vglm")){
    # VGAM::vglm-based methods
    mu    <- VGAM::coefvlm(model)
    
    if(is.matrix(mu)){
      stdevs <- matrix(sqrt(diag(VGAM::vcovvlm(model))), ncol=ncol(mu), byrow=TRUE)
    } else {
      stdevs <- sqrt(diag(VGAM::vcovvlm(model)))
      stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
    }
  } else {
    # glm-based methods
    mu    <- stats::coef(model)
    
    if(is.matrix(mu)){
      stdevs <- matrix(sqrt(diag(stats::vcov(model))), ncol=ncol(mu), byrow=TRUE)
    } else {
      stdevs <- sqrt(diag(stats::vcov(model)))
      
      if(is.null(names(stdevs))){
        # Case where no names are provided
        stdevs <- stdevs[seq_len(length(mu))]
      } else {
        stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
      }
    }
  }
  
  # Compute z-score
  z  <- (mu-mean)/stdevs
  
  # Return p-value based on z-score
  return(2*(1-stats::pnorm(abs(z))))
}

# 
# regrLocTest <- function(regr_fit_obj, mean=0){
# 
#   # Provides location test for unrestriced regression models
#   if("vglm" %in% class(regr_fit_obj)){
#     # VGAM::vglm-based methods
#     mu    <- VGAM::coefvlm(regr_fit_obj)
# 
#     if(is.matrix(mu)){
#       stdevs <- matrix(sqrt(diag(VGAM::vcovvlm(regr_fit_obj))), ncol=ncol(mu), byrow=TRUE)
#     } else {
#       stdevs <- sqrt(diag(VGAM::vcovvlm(regr_fit_obj)))
#       stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
#     }
#   } else {
#     # glm-based methods
#     mu    <- stats::coef(regr_fit_obj)
# 
#     if(is.matrix(mu)){
#       stdevs <- matrix(sqrt(diag(stats::vcov(regr_fit_obj))), ncol=ncol(mu), byrow=TRUE)
#     } else {
#       stdevs <- sqrt(diag(stats::vcov(regr_fit_obj)))
#       
#       if(is.null(names(stdevs))){
#         # Case where no names are provided
#         stdevs <- stdevs[seq_len(length(mu))]
#       } else {
#         stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
#       }
#     }
#   }
# 
#   # Compute z-score
#   z  <- (mu-mean)/stdevs
#   
#   # Return p-value based on z-score
#   return(2*(1-stats::pnorm(abs(z))))
# }
# 
# 
# updateWithReplacement <- function(dt, repl_list){
#   # Updates columns of a data table with replacement data from repl_list
#   dt_repl <- copy(dt)
# 
#   # Find feature names corresponding to columns to be replaced
#   repl_feat <- names(repl_list)
# 
#   # Iterate over replacement list entries
#   for(curr_feat in repl_feat){
#     dt_repl[, (curr_feat):=repl_list[[curr_feat]] ]
#   }
# 
#   return(dt_repl)
# }



vectorAsString <- function(ref_names){
  # Parses a character vector to a string
  vect_str = ""

  for(ii in 1:length(ref_names)){
    if(ii==1){
      vect_str <- c(vect_str, ref_names[ii])
    } else if(ii==length(ref_names)){
      vect_str <- c(vect_str, paste0(" and ", ref_names[ii]))
    } else {
      vect_str <- c(vect_str, paste0(", ", ref_names[ii]))
    }
  }

  vect_str <- paste0(vect_str, collapse="")
}

is_package_installed <- function(name, version=NULL, verbose=FALSE){

  # Get all installed packages
  installed_packages <- utils::installed.packages()[,1]

  if(name %in% installed_packages){
    if(!is.null(version)){
      installed_version <- utils::packageVersion(name)

      if(installed_version < version & verbose){
        logger.warning(paste0("Package check: Please update the ", name, " package to version ", version, " or later."))
      }
    }

    is_installed <- TRUE
  } else {
    is_installed <- FALSE
  }

  if(!is_installed & verbose){
    logger.warning(paste0("Package check: Please install the ", name, " package."))
  }

  return(is_installed)
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


rbind_list_list <- function(l, list_elem, ...){

  # Get a list of the required list elements
  elem_list <- lapply(l, function(sub_list) (sub_list[[list_elem]]))
  
  # Remove empty list elements
  elem_list[sapply(elem_list, is.null)] <- NULL
  
  return(data.table::rbindlist(elem_list, ...))
}


describe <- function(x, conf_alpha=0.05){

  # Test conf_alpha
  if(conf_alpha > 1.0){
    stop("No confidence interval exists for widths of 0% or smaller.")
  } else if(conf_alpha <= 0.0){
    stop("The confidence interval cannot exceed 100% width.")
  }
  
  # Define quantile probabilities based on the alpha confidence interval width.
  quantile_prob <- c(conf_alpha/2.0, 1.0 - conf_alpha/2.0)
  
  # Compute quantiles within the data set
  quantiles <- stats::quantile(x, probs=quantile_prob, names=FALSE, na.rm=TRUE)
  
  # Generate a summary list
  summary_list <- list("median"=stats::median(x, na.rm=TRUE), "ci_low"=quantiles[1], "ci_up"=quantiles[2])
  
  return(summary_list)
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
  # Hadley Wickham (http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html)
  
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
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


fam_sample <- function(x, size, replace=FALSE, prob=NULL){
  # This function prevents the documented behaviour of the sample function,
  # where if x is positive, numeric and only has one element, it interprets x as
  # a series of x, i.e. x=seq_len(x). That's bad news if x is a sample
  # identifier.
  
  if(length(x) == 1){
    
    # Check that size is not greater than 1, if items are to be drawn without
    # replacement.
    if(!replace & size > 1){
      stop("cannot take a sample larger than the population when 'replace = FALSE'")
    }
    
    return(rep_len(x=x, length.out=size))
    
  } else {
    # If x is a vector, array or list with multiple elements, then all of the
    # above is not an issue, and we can make use of sample.
    
    return(sample(x=x, size=size, replace=replace, prob=prob))
  }
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
