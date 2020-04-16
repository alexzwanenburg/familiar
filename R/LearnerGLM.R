learner.glm.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type=="binomial" & learner %in% c("glm", "glm_logistic", "glm_probit", "glm_cauchy", "glm_log", "glm_loglog")){
    return(TRUE)
  } else if(outcome_type=="multinomial" & learner %in% c("glm", "glm_multinomial")) {
    return(TRUE)
  } else if(outcome_type=="continuous" & learner %in% c("glm", "glm_log", "glm_gaussian", "glm_log_gaussian", "glm_inv_gaussian", "glm_poisson", "glm_log_poisson")){
    return(TRUE)
  } else if(outcome_type=="survival" & learner %in% c("glm")){
    return(TRUE)
  } else if(outcome_type=="count" & learner %in% c("glm", "glm_poisson", "glm_log_poisson")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.glm.param <- function(data_obj, learner){

  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$sign_size <- list()
  param$family    <- list()

  # If no data object is not provided, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Get the outcome type
  outcome_type <- data_obj@outcome_type

  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj, restrict_samples=TRUE)


  ##### Model family ###########################################################

  # Read family string by parsing the learner.
  fam <- stringi::stri_replace_first_regex(str=learner, pattern="glm", replace="")
  if(fam != ""){
    fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="")
  }

  # Determine the family or families.
  if(fam == ""){
    # If no family is specified, the default behaviour is to identify the family
    # through optimisation.
    if(outcome_type=="binomial") {
      family_default <- c("logistic", "probit", "loglog", "cauchy", "log_binomial")
  
    } else if(outcome_type=="continuous"){
      family_default <- c("gaussian", "log_gaussian", "inv_gaussian", "poisson", "log_poisson")
    
    } else if(outcome_type=="count"){
      family_default <- c("poisson", "log_poisson")
    
    } else if(outcome_type=="multinomial") {
      family_default <- "multinomial"
    }
    
  } else if(fam == "log") {
    # "log" is a collection of different families, that should be specified
    # according to the outcome type.
    if(outcome_type=="binomial") {
      family_default <- "log_binomial"
      
    } else if(outcome_type=="continuous") {
      family_default <- c("log_gaussian", "log_poisson")
      
    } else if(outcome_type=="count") {
      family_default <- "log_poisson"
    }
    
  } else {
    # A family was unambiguously specified.
    family_default <- fam
  }

  # Set family parameter.
  param$family <- .set_hyperparameter(default=family_default, type="factor", range=family_default,
                                      randomise=ifelse(length(family_default) > 1, TRUE, FALSE))

  return(param)
}



learner.glm.train <- function(object, data_obj){

  # Extract hyperparameters and outcome type
  param        <- object@hyperparameters
  outcome_type <- object@outcome_type

  # Initialise a model list
  model_list   <- list()

  # Use effect coding to convert categorical data into encoded data - this is required to deal with factors with missing/new levels
  # between training and test data sets.
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Parse formula
  formula      <- stats::reformulate(termlabels=feature_cols, response=quote(outcome))

  # Get family for glm, which determines how the response and predictors are linked
  family_obj   <- learner.glm.get_family(fam=param$family)

  if(outcome_type %in% c("binomial", "continuous", "count")){
    # Generate model
    model_obj  <- tryCatch({ stats::glm(formula, data=dt, family=family_obj, model=FALSE)},
                           error=function(err) { return(NULL) } )

    # Determine whether model has converged
    if(!is.null(model_obj)){
      model_list$model_trained <- model_obj$converged
    } else {
      model_list$model_trained <- FALSE
    }

  } else if(outcome_type=="multinomial"){
    # Generate model
    model_obj  <- tryCatch(VGAM::vglm(formula, data=dt, family=family_obj),
                           error=function(err) return(NULL))

    # Determine whether model has converged
    if(!is.null(model_obj)){
      model_list$model_trained <- TRUE
    } else {
      model_list$model_trained <- FALSE
    }
  }

  # Append model to list
  model_list$model <- model_obj

  # Add the contrast references
  model_list$contrast_ref <- contr_list$dt_ref

  return(model_list)
}



learner.glm.vimp <- function(object){

  # Suppress NOTES due to non-standard evaluation in data.table
  name <- score <- NULL

  # Generate variable importance data table
  if(model_is_trained(object=object)){

    # Determine coefficient p-values
    coef_p_val   <- regrLocTest(regr_fit_obj=object@model$model)

    if("vglm" %in% class(object@model$model)){
      # Parse coefficient names
      coef_names <- stringi::stri_split_fixed(names(coef_p_val), pattern=":")
      coef_names <- sapply(coef_names, function(curr_coef_name) (paste0(curr_coef_name[-length(curr_coef_name)], collapse=":")))

      # Construct variable importance table
      dt_vimp           <- data.table::data.table("score"=coef_p_val, "name"=coef_names)
      dt_vimp           <- dt_vimp[name!="(Intercept)", list(score=min(score)), by=name]
      dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="min")
      dt_vimp$rank      <- data.table::frank(dt_vimp$score, ties.method="min")
      dt_vimp$multi_var <- TRUE

    } else {
      # Remove intercept
      coef_p_val        <- coef_p_val[names(coef_p_val)!="(Intercept)"]
      coef_names        <- names(coef_p_val)[names(coef_p_val)!="(Intercept)"]

      # Construct variable importance table
      dt_vimp           <- data.table::data.table("score"=coef_p_val, "name"=coef_names)
      dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="min")
      dt_vimp$rank      <- data.table::frank(dt_vimp$score, ties.method="min")
      dt_vimp$multi_var <- TRUE
    }
  } else {
    dt_vimp             <- getEmptyVimp()
  }

  return(dt_vimp)
}



learner.glm.test <- function(object, data_obj){

  # Extract data and class levels
  outcome_type <- object@outcome_type
  class_levels <- get_outcome_class_levels(x=object)
  
  # For consistency, use contrast for factors if this was done during model training
  
  # Use effect coding to convert categorical data into encoded data
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)
  
  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast
  
  # Check if the model was trained.
  if(model_is_trained(object=object)){

    # Extract data
    model_obj    <- object@model$model

    # Generate skeleton of prediction data table from dt
    dt_pred <- dt[, get_non_feature_columns(x=object), with=FALSE]

    if(outcome_type=="binomial"){
      # Predict class probabilities for new data
      pred_outc_prob <- stats::predict.glm(object=model_obj, newdata=dt, type="response")

      # Set initial predicted class
      dt_pred[, "outcome_pred_class":=factor(class_levels[1], levels=class_levels)]

      # Add class probabilities (glm always gives probability for the second class)
      outcome_pred_class_prob_cols <- get_class_probability_name(x=class_levels)
      dt_pred[, (outcome_pred_class_prob_cols[1]):=1-pred_outc_prob]
      dt_pred[, (outcome_pred_class_prob_cols[2]):=pred_outc_prob]

      # Update predicted class based on provided probabilities
      dt_pred[get(outcome_pred_class_prob_cols[2]) >= 0.50, "outcome_pred_class":=class_levels[2]]

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred[[outcome_pred_class_prob_cols[1]]]))){
        dt_pred    <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }
      
    } else if(outcome_type == "multinomial") {
      # Predict class probabilities for new data
      pred_outc_prob <- VGAM::predictvglm(object=model_obj, newdata=dt, type="response")

      # Update predicted class
      pred_outc_class <- colnames(pred_outc_prob)[apply(pred_outc_prob, 1, which.max)]
      dt_pred[, "outcome_pred_class":=factor(x=pred_outc_class, levels=class_levels)]

      # Add class probabilities
      outcome_pred_class_prob_cols <- get_class_probability_name(x=class_levels)
      dt_pred <- cbind(dt_pred, data.table::as.data.table(pred_outc_prob))
      data.table::setnames(dt_pred, old=class_levels, new=outcome_pred_class_prob_cols)

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred[[outcome_pred_class_prob_cols[1]]]))){
        dt_pred    <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }

    } else if(outcome_type %in% c("continuous", "count")){
      # Predict regression values
      pred_outc <- stats::predict.glm(object=model_obj, newdata=dt, type="response")

      # Add to table
      dt_pred[, "outcome_pred":=pred_outc]

    }
  } else {
    dt_pred <- createNonValidPredictionTable(dt=dt, outcome_type=object@outcome_type)
  }

  # Return prediction data table
  return(dt_pred)
}



learner.glm.get_family <- function(fam){

  # Load families for linear regression
  if(fam=="logistic")          { family_obj <- stats::binomial(link="logit") }
  else if(fam=="probit")       { family_obj <- stats::binomial(link="probit") }
  else if(fam=="cauchy")       { family_obj <- stats::binomial(link="cauchit") }
  else if(fam=="log_binomial") { family_obj <- stats::binomial(link="log") }
  else if(fam=="loglog")       { family_obj <- stats::binomial(link="cloglog") }
  else if(fam=="gaussian")     { family_obj <- stats::gaussian(link="identity") }
  else if(fam=="log_gaussian") { family_obj <- stats::gaussian(link="log") }
  else if(fam=="inv_gaussian") { family_obj <- stats::gaussian(link="inverse") }
  else if(fam=="poisson")      { family_obj <- stats::poisson(link="identity") }
  else if(fam=="log_poisson")  { family_obj <- stats::poisson(link="log") }
  else if(fam=="multinomial")  { family_obj <- VGAM::multinomial()}

  return(family_obj)
}



learner.glm.calibration_info <- function(object, data_obj){
  
  if(object@outcome_type %in% c("continuous", "count")){
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }
  
  return(calibration_info)
}



learner.glm.get_calibration <- function(object, data_obj){

  # Extract outcome type from the familiar model.
  outcome_type <- object@outcome_type

  if(outcome_type %in% c("binomial", "multinomial") & learner.glm.outcome(object=object)) {

    # Parse calibration
    dt_calibr <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else if(outcome_type %in% c("continuous", "count") & learner.glm.outcome(object=object)) {

    # Parse calibration
    dt_calibr <- learner.calibration.regression(object=object, data_obj=data_obj)

  } else {
    stop()
  }

  return(dt_calibr)
}
