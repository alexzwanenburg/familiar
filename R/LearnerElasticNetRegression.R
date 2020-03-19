learner.net.learner <- function(method, outcome_type){

  # Variable importance methods and learners have the same name and correspond directly.
  return(method)
}



learner.net.outcome <- function(learner=NULL, method=NULL, outcome_type=NULL, object=NULL){

  # Variable importance methods and learners have the same name
  if(!is.null(method)){ learner <- method }

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type=="survival" & learner %in% c("elastic_net", "elastic_net_cox", "lasso", "lasso_cox", "ridge", "ridge_cox")){
    return(TRUE)
  } else if(outcome_type=="continuous" & learner %in% c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson",
                                                        "lasso", "lasso_gaussian", "lasso_poisson",
                                                        "ridge", "ridge_gaussian", "ridge_poisson")){
    return(TRUE)
  } else if(outcome_type=="multinomial" & learner %in% c("elastic_net", "elastic_net_multinomial", "lasso",
                                                         "lasso_multinomial", "ridge", "ridge_multinomial")){
    return(TRUE)
  } else if(outcome_type=="binomial" & learner %in% c("elastic_net", "elastic_net_binomial", "lasso", "lasso_binomial",
                                                      "ridge", "ridge_binomial")){
    return(TRUE)
  } else if(outcome_type=="count" & learner %in% c("elastic_net", "elastic_net_poisson", "lasso", "lasso_poisson", "ridge", "ridge_poisson")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.net.param <- function(data_obj, learner){

  # Initialise list and declare hyperparameter entries
  param <- list()
  param$sign_size <- list()
  param$family <- list()
  param$alpha <- list()
  param$lambda_min <- list()
  param$n_folds <- list()
  param$normalise <- list()

  # If dt is not provided, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Internal
  outcome_type <- data_obj@outcome_type

  # Determine the base learner
  if(stringi::stri_startswith_fixed(str=learner, pattern="elastic_net")) {
    base_learner <- "elastic_net"
  } else if(stringi::stri_startswith_fixed(str=learner, pattern="lasso")) {
    base_learner <- "lasso"
  } else if(stringi::stri_startswith_fixed(str=learner, pattern="ridge")) {
    base_learner <- "ridge"
  } else {
    ..error_reached_unreachable_code("learner_net_param_unknown_base_learner")
  }
  
  # Determine the family
  fam <- stringi::stri_replace_first_regex(str=learner, pattern="elastic_net|lasso|ridge", replace="")
  if(fam != ""){
    fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="")
  }
  
  # Determine number of subjects
  n_samples <- length(unique(data_obj@data$subject_id))

  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)


  ##### Family #################################################################
  if(fam == ""){
    if(outcome_type == "continuous"){
      family_default <- c("gaussian", "poisson")
      
    } else if(outcome_type=="count"){
      family_default <- "poisson"
      
    } else if(outcome_type=="binomial"){
      family_default <- "binomial"
      
    } else if(outcome_type=="multinomial"){
      family_default <- "multinomial"
      
    } else if(outcome_type=="survival"){
      family_default <- "cox"
    }
  } else {
    family_default <- fam
  }
  
  # Set family parameter
  param$family <- .set_hyperparameter(default=family_default, type="factor", range=family_default,
                                      randomise=ifelse(length(family_default) > 1, TRUE, FALSE))
  

  ##### Elastic net mixing parameter ###########################################
  if(base_learner == "elastic_net"){
    alpha_default <- c(0, 1/3, 2/3, 1)
  } else if(base_learner == "lasso"){
    alpha_default <- 1
  } else if(base_learner == "ridge"){
    alpha_default <- 0
  }
  
  # Set alpha parameter. Alpha = 1 is lasso, alpha = 0 is ridge. glmnet requires
  # alpha to be in the closed interval [0, 1].
  param$alpha <- .set_hyperparameter(default=alpha_default, type="numeric", range=c(0, 1), valid_range=c(0, 1),
                                     randomise=ifelse(length(alpha_default) > 1, TRUE, FALSE))
  

  ##### Lambda indicating the optimal model complexity #########################
  param$lambda_min <- .set_hyperparameter(default="lambda.min", type="factor",
                                          range=c("lambda.1se", "lambda.min"), randomise=FALSE)

  
  ##### Number of cross-validation folds #######################################
  
  # glmnet requires at least 3 folds. The default number of cross-validation
  # folds may grow up to 20, for data sets > 200 samples.
  n_folds_default <- min(c(20, max(c(3, floor(n_samples/10)))))
  
  # Set the number of cross-validation folds.
  param$n_folds <- .set_hyperparameter(default=n_folds_default, type="integer", range=c(3, n_samples),
                                       valid_range=c(3, Inf), randomise=FALSE)
  
  
  ##### Feature normalisation #####
  
  # By default, normalisation is part of the pre-processing of familiar, but the
  # user may have disabled it. In that the case, the user can set normalisation
  # to TRUE to avoid complaints by glmnet.
  param$normalise <- .set_hyperparameter(default=FALSE, type="logical", range=c(FALSE, TRUE), randomise=FALSE)
  
  # Return hyper-parameters
  return(param)
}



learner.net.train <- function(object, data_obj){

  # Extract hyper-parameters and outcome type
  param        <- object@hyperparameters
  outcome_type <- object@outcome_type

  # Initiate model_list list
  model_list   <- list()

  # Use effect coding to convert categorical data into encoded data
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Parse outcome data
  if(outcome_type=="survival"){
    outc_data  <- survival::Surv(dt$outcome_time, dt$outcome_event)
  } else {
    outc_data  <- dt$outcome
  }

  # Generate folds using our own fold generating algorithm to handle repeated measurements
  dt_fold <- .create_cv(sample_identifiers=unique(dt$subject_id), n_folds=param$n_folds, settings=NULL,
                        outcome_type=outcome_type, data=dt, stratify=FALSE, return_fold_id=TRUE)
  
  # Order according to subject id in dt so that fold_id corresponds to the correct rows.
  dt_fold <- merge(x=dt_fold, y=dt[, "subject_id", with=FALSE], by="subject_id")

  # Generate model
  model_obj    <- tryCatch({ cv.glmnet(x = as.matrix(dt[, feature_cols, with=FALSE]),
                                       y = outc_data,
                                       family = param$family,
                                       alpha = param$alpha,
                                       standardize = param$normalise,
                                       nfolds = NULL,
                                       foldid = dt_fold$fold_id,
                                       parallel = FALSE)},
                           error=function(err) { return(NULL) } )

  # Append model and lambda_min settings to list
  model_list$model <- model_obj
  model_list$lambda_min <- param$lambda_min
  model_list$family <- param$family
  
  # Store features as glmnet depends on the ordering of the features (which is
  # poor behaviour but not something we can solve from this side of the table).
  model_list$features <- feature_cols

  # Update model status
  if(is.null(model_obj)) { model_list$model_trained <- FALSE }
  else {                   model_list$model_trained <- TRUE }

  # Add the contrast references
  model_list$contrast_ref <- contr_list$dt_ref

  return(model_list)
}



learner.net.test <- function(object, data_obj){

  # Extract internal data
  model_obj    <- object@model$model
  outcome_type <- object@outcome_type
  optim_lambda <- object@model$lambda_min
  
  # Obtain features as glmnet depends on the ordering of the features (which is
  # poor behaviour but not something we can solve from this side of the table).
  # This guarantees that features are ordered in the same manner as during
  # training.
  feature_cols <- object@model$features

  # Use effect coding to convert categorical data into encoded data
  contr_list <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt <- contr_list$dt_contrast
  
  # Generate skeleton of prediction data table from dt
  dt_pred <- data.table::copy(dt[, get_non_feature_columns(x=object), with=FALSE])
  
  # Evaluate trained models
  if(model_is_trained(object=object)){

    # Additionally, predict classes for binomial and multinomial outcomes
    if(outcome_type %in% c("binomial")){

      # Get predicted classes and probabilities
      pred_outc_class <- predict(object=model_obj, newx=as.matrix(dt[, feature_cols, with=FALSE]),
                                 s=optim_lambda, type="class")
      pred_outc_prob  <- predict(object=model_obj, newx=as.matrix(dt[, feature_cols, with=FALSE]),
                                 s=optim_lambda, type="response")

      # Convert to factor and set to prediction data table
      dt_pred[, "outcome_pred_class":=factor(pred_outc_class[,1], levels=object@class_levels)]

      # Add class probabilities (glmnet always gives probability for the second class)
      outcome_pred_class_prob_cols <- get_class_probability_columns(data_obj=data_obj)
      dt_pred[, (outcome_pred_class_prob_cols[1]):=1-pred_outc_prob[,1]]
      dt_pred[, (outcome_pred_class_prob_cols[2]):=pred_outc_prob[,1]]

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred[[outcome_pred_class_prob_cols[1]]]))){
        dt_pred                    <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }

      # Set to dt_pred
    } else if(outcome_type %in% c("multinomial")){

      # Get predicted classes and probabilities
      pred_outc_class <- predict(object=model_obj, newx=as.matrix(dt[, feature_cols, with=FALSE]),
                                 s=optim_lambda, type="class")[,1]
      pred_outc_prob  <- predict(object=model_obj, newx=as.matrix(dt[, feature_cols, with=FALSE]),
                                 s=optim_lambda, type="response")[,,1]

      # Convert to factor and set to prediction data table
      dt_pred[, "outcome_pred_class":=factor(pred_outc_class, levels=object@class_levels)]

      # Add class probabilities
      outcome_pred_class_prob_cols <- get_class_probability_columns(data_obj=data_obj)
      dt_pred <- cbind(dt_pred, data.table::as.data.table(pred_outc_prob))
      data.table::setnames(dt_pred, old=object@class_levels, new=outcome_pred_class_prob_cols)

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred[[outcome_pred_class_prob_cols[1]]]))){
        dt_pred    <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }

    } else if(outcome_type %in% c("survival", "continuous", "count")){

      # Get predicted hazard risk (survival outcome) or linear prediction (continuous outcome)
      dt_pred$outcome_pred <- predict(object=model_obj, newx=as.matrix(dt[, feature_cols, with=FALSE]),
                                      s=optim_lambda, type="response")

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred$outcome_pred))){
        dt_pred <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }
    }
  } else {
    # Create a standard prediction table with NA values
    dt_pred <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
  }

  # Return prediction data table
  return(dt_pred)
}



learner.net.vimp <- function(data_obj=NULL, param=NULL, method=NULL, object=NULL){

  # Suppress NOTES due to non-standard evaluation in data.table
  name <- NULL

  # Check if the model is provided, and train one otherwise. During feature selection, the model is trained here. However, when
  # variable importance of the final model is determined, a model is expected.
  if(is.null(object)){
    # Extract data
    outcome_type <- data_obj@outcome_type

    if(is.null(param)) { stop("Parameters should be provided for elastic net variable importance.") }

    # Train the model. This should be done when the function is called during feature selection
    object <- methods::new("familiarModel",
                           outcome_type = outcome_type,
                           learner = learner.net.learner(method=method, outcome_type=outcome_type),
                           hyperparameters = param,
                           class_levels = get_outcome_levels(x=data_obj))
    
    object <- train(object=object, data=data_obj, get_recalibration=FALSE, get_additional_info=FALSE)
  }

  # Generate variable importance data table
  if(model_is_trained(object=object)){

    if(object@model$family == "multinomial"){
      # Read coefficient lists
      coef_list <- coef(object@model$model, s=object@model$lambda_min)

      # Parse into matrix and retrieve row names
      coef_mat <- sapply(coef_list, as.matrix)
      rownames(coef_mat) <- dimnames(coef_list[[1]])[[1]]

      # Calculate score
      score <- apply(abs(coef_mat), 1, max)
    } else {
      # Read coefficient matrix
      coef_mat <- as.matrix(coef(object@model$model, s=object@model$lambda_min))

      # Calculate score
      score <- abs(coef_mat)[,1]
    }

    # Parse score to data.table
    dt_vimp <- data.table::data.table("score"=score, "name"=names(score))

    # Remove intercept
    dt_vimp <- dt_vimp[name!="(Intercept)"]
    dt_vimp <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="max")

    # Throw out elements with 0.0 coefficients
    dt_vimp <- dt_vimp[score!=0.0]

    # Rank coefficients
    dt_vimp$rank <- data.table::frank(-dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- TRUE
  } else {
    dt_vimp <- getEmptyVimp()
  }

  return(dt_vimp)
}



learner.net.prediction_type <- function(learner, outcome_type){

  # Cox regression models predict risks, not expected survival times
  if(outcome_type=="survival" & learner %in% c("elastic_net", "elastic_net_cox", "lasso", "lasso_cox", "ridge", "ridge_cox")){
    return("hazard_ratio")
  } else {
    return(NULL)
  }
}



learner.net.calibration_info <- function(object, data_obj, time_max=NULL){

  if(object@outcome_type == "survival"){

    # Determine baseline survival.
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }

  return(calibration_info)
}



learner.net.get_calibration <- function(object, data_obj, time_max){

  # Extract interal variables
  outcome_type <- object@outcome_type

  if(outcome_type=="survival" & learner.net.outcome(object=object)) {
    # Get survival probabilities
    probability_table <- learner.survival_probability.relative_risk(object=object,
                                                                    data_obj=data_obj,
                                                                    time_max=time_max)

    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object,
                                                      probability_table=probability_table,
                                                      time_max=time_max)

  } else if(outcome_type %in% c("binomial", "multinomial") & learner.net.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else if(outcome_type %in% c("continuous", "count") & learner.net.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.regression(object=object, data_obj=data_obj)

  } else {
    ..error_reached_unreachable_code("learner.net.get_calibration: unknown combination of outcome and learner.")
  }

  return(calibration_table)
}
