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
  
  if(is.null(cl)){
    # Serial processing
    if(outcome_type=="survival"){
      regr_p_val <- sapply(feature_columns, function(curr_feat, dt) (eval.univar_cox_regr(data=dt[, c(curr_feat, "outcome_time", "outcome_event"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="continuous"){
      regr_p_val <- sapply(feature_columns, function(curr_feat, dt) (eval.univar_gauss_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="binomial"){
      regr_p_val <- sapply(feature_columns, function(curr_feat, dt) (eval.univar_binomial_logistic_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="multinomial"){
      regr_p_val <- sapply(feature_columns, function(curr_feat, dt) (eval.univar_multinomial_logistic_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="count"){
      regr_p_val <- sapply(feature_columns, function(curr_feat, dt) (eval.univar_poisson_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="competing_risk"){
      ..error_outcome_type_not_implemented(outcome_type)
    }
  } else {
    # Parallel processing
    if(outcome_type=="survival"){
      regr_p_val <- parallel::parSapply(cl, feature_columns, function(curr_feat, dt) (eval.univar_cox_regr(data=dt[, c(curr_feat, "outcome_time", "outcome_event"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="continuous"){
      regr_p_val <- parallel::parSapply(cl, feature_columns, function(curr_feat, dt) (eval.univar_gauss_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="binomial"){
      regr_p_val <- parallel::parSapply(cl, feature_columns, function(curr_feat, dt) (eval.univar_binomial_logistic_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="multinomial"){
      regr_p_val <- parallel::parSapply(cl, feature_columns, function(curr_feat, dt) (eval.univar_multinomial_logistic_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="count"){
      regr_p_val <- parallel::parSapply(cl, feature_columns, function(curr_feat, dt) (eval.univar_poisson_regr(data=dt[, c(curr_feat, "outcome"), with=FALSE])), dt=data_obj@data)
    } else if(outcome_type=="competing_risk"){
      ..error_outcome_type_not_implemented(outcome_type)
    }
  }
  
  return(regr_p_val)
}


eval.univar_cox_regr <- function(data){
  # Cox regression model for univariable analysis
  
  # Get name of the current feature.
  feature <- setdiff(colnames(data), c("outcome_time", "outcome_event"))
  
  # Drop entries with missing feature values.
  data <- data[is.finite(eval(parse(text=feature)))]
  
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data[[feature]])) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data[[feature]])){
    data[[feature]] <- droplevels(data[[feature]])
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



eval.univar_gauss_regr <- function(data){
  # Gaussian regression for univariable analysis
  
  # Get name of the current feature.
  feature <- setdiff(colnames(data), "outcome")
  
  # Drop entries with missing feature values.
  data <- data[is.finite(eval(parse(text=feature)))]
  
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data[[feature]])) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data[[feature]])){
    data[[feature]] <- droplevels(data[[feature]])
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



eval.univar_poisson_regr <- function(data){
  # Poisson regression for univariable analysis with count-type outcomes
  
  # Get name of the current feature.
  feature <- setdiff(colnames(data), "outcome")
  
  # Drop entries with missing feature values.
  data <- data[is.finite(eval(parse(text=feature)))]
  
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data[[feature]])) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data[[feature]])){
    data[[feature]] <- droplevels(data[[feature]])
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



eval.univar_binomial_logistic_regr <- function(data){
  #Binomial model for univariable analysis using logistic regression
  
  # Get name of the current feature.
  feature <- setdiff(colnames(data), "outcome")
  
  # Drop entries with missing feature values.
  data <- data[is.finite(eval(parse(text=feature)))]
  
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data[[feature]])) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data[[feature]])){
    data[[feature]] <- droplevels(data[[feature]])
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



eval.univar_multinomial_logistic_regr <- function(data){
  # Multinomial model for univariable analysis using logistic regression
  
  # Get name of the current feature.
  feature <- setdiff(colnames(data), "outcome")
  
  # Drop entries with missing feature values.
  data <- data[is.finite(eval(parse(text=feature)))]
  
  if(is_empty(data)) return(NA_real_)
  
  # Check if the feature column is singular
  if(is_singular_data(data[[feature]])) return(NA_real_)
  
  # Drop levels from factor
  if(is.factor(data[[feature]])){
    data[[feature]] <- droplevels(data[[feature]])
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



compute_icc <- function(dt, type="1", feat_name){
  
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
  
  # Make a local copy
  dt <- data.table::copy(dt)
  
  # Rename feature colume to value
  data.table::setnames(dt, old=feat_name, new="value")
  
  # Calculate each parameter in the equation
  dt[,"mu":=mean(value, na.rm=TRUE)][,"bj":=mean(value, na.rm=TRUE)-mu, by=list(subject_id,cohort_id)][,"ai":=mean(value, na.rm=TRUE)-mu, by=list(repetition_id)][,"eij":=value-mu-bj-ai]
  
  # Calculate
  n_subjects <- data.table::uniqueN(dt, by="subject_id")
  n_raters   <- data.table::uniqueN(dt, by="repetition_id")
  
  # Calculate mean squared errors: msb between subjects (bj), msj between raters (ai), mse of error (eij) and msw of error with rater (ai + eij)
  if(n_subjects > 1){
    msb <- sum(dt$bj^2, na.rm=TRUE) / (n_subjects-1)
  }
  
  if(type=="1"){
    # Calculate mean squared of error with rater
    msw       <- (sum(dt$eij^2, na.rm=TRUE) + sum(dt$ai^2, na.rm=TRUE)) / (n_subjects * (n_raters-1))
    
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
    msj <- sum(dt$ai^2, na.rm=TRUE) / (n_raters-1)
    mse <- sum(dt$eij^2, na.rm=TRUE) / ((n_subjects-1) * (n_raters-1))
    
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
    mse <- sum(dt$eij^2, na.rm=TRUE) / ((n_subjects-1) * (n_raters-1))
    
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
  
  return(data.table::data.table("name"=feat_name, "icc"=icc, "icc_low"=icc_ci_low, "icc_up"=icc_ci_up,
                                "icc_panel"=icc_panel, "icc_panel_low"=icc_panel_ci_low, "icc_panel_up"=icc_panel_ci_up))
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


createNonValidPredictionTable <- function(dt, outcome_type){
  # Create skeleton
  dt_pred <- dt[, get_non_feature_columns(x=outcome_type), with=FALSE]

  # Add prediction columns
  if(outcome_type %in% c("survival", "continuous", "count")){
    # For survival and continuous outcomes, a single column is required
    dt_pred$outcome_pred         <- as.double(NA)
  } else if (outcome_type %in% c("binomial", "multinomial")){
    # For binomial and multinomial outcomes, we add both predicted class and predicted class probabilities
    dt_pred$outcome_pred_class   <- as.character(NA)

    # Add class probabilities
    outcome_pred_class_prob_cols <- check_column_name(column_name=paste0("outcome_pred_prob_", levels(dt$outcome)))
    for(ii in 1:length(outcome_pred_class_prob_cols)){
      dt_pred[, (outcome_pred_class_prob_cols[ii]):=as.double(NA)]
    }
  } else if(outcome_type == "competing_risk"){
    browser()
  }

  # Return without valid data prediction table
  return(dt_pred)
}

any_predictions_valid <- function(prediction_table, outcome_type){
  
  if(is_empty(prediction_table)){
    return(FALSE)
  }
  
  if(outcome_type %in% c("survival", "continuous", "count")){
    return(any(is.finite(prediction_table$outcome_pred)))
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    return(!all(is.na(prediction_table$outcome_pred_class)))
    
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



getEmptyVimp <- function(){
  # Returns an empty variable importance data table

  dt_vimp <- data.table("score"=numeric(0), "name"=character(0), "rank"=numeric(0), "multi_var"=logical(0))

  return(dt_vimp)
}



getPredictedOutcomeColumn <- function(outcome_type){
  # Returns column name with predicted outcomes, given the outcome type

  if(outcome_type %in% c("survival", "continuous", "count")){
    return("outcome_pred")
  } else if(outcome_type %in% c("binomial", "multinomial")){
    return("outcome_pred_class")
  } else if(outcome_type == "competing_risk"){
    stop()
  }
}



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


regrLocTest <- function(regr_fit_obj, mean=0){

  # Provides location test for unrestriced regression models
  if("vglm" %in% class(regr_fit_obj)){
    # VGAM::vglm-based methods
    mu    <- VGAM::coefvlm(regr_fit_obj)

    if(is.matrix(mu)){
      stdevs <- matrix(sqrt(diag(VGAM::vcovvlm(regr_fit_obj))), ncol=ncol(mu), byrow=TRUE)
    } else {
      stdevs <- sqrt(diag(VGAM::vcovvlm(regr_fit_obj)))
      stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
    }
  } else {
    # glm-based methods
    mu    <- stats::coef(regr_fit_obj)

    if(is.matrix(mu)){
      stdevs <- matrix(sqrt(diag(stats::vcov(regr_fit_obj))), ncol=ncol(mu), byrow=TRUE)
    } else {
      stdevs <- sqrt(diag(stats::vcov(regr_fit_obj)))
      
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


updateWithReplacement <- function(dt, repl_list){
  # Updates columns of a data table with replacement data from repl_list
  dt_repl <- copy(dt)

  # Find feature names corresponding to columns to be replaced
  repl_feat <- names(repl_list)

  # Iterate over replacement list entries
  for(curr_feat in repl_feat){
    dt_repl[, (curr_feat):=repl_list[[curr_feat]] ]
  }

  return(dt_repl)
}



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
