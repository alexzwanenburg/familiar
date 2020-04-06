learner.mbboost.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type=="survival" & learner %in% c("boosted_glm", "boosted_glm_cox", "boosted_glm_surv", "boosted_glm_loglog", "boosted_glm_weibull",
                                               "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex", "boosted_tree", "boosted_tree_cox",
                                               "boosted_tree_surv","boosted_tree_loglog", "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan",
                                               "boosted_tree_cindex")){
    return(TRUE)
  } else if(outcome_type=="continuous" & learner %in% c("boosted_glm", "boosted_glm_gaussian", "boosted_glm_huber", "boosted_glm_laplace", "boosted_glm_poisson",
                                                        "boosted_tree", "boosted_tree_gaussian", "boosted_tree_huber", "boosted_tree_laplace", "boosted_tree_poisson")){
    return(TRUE)
  # } else if(outcome_type=="multinomial" & learner %in% c("boosted_glm", "boosted_glm_multinomial")){
  #   return(TRUE)
  } else if(outcome_type=="binomial" & learner %in% c("boosted_glm", "boosted_glm_logistic", "boosted_glm_probit", "boosted_glm_loglog", "boosted_glm_cauchy",
                                                      "boosted_glm_log", "boosted_glm_auc", "boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
                                                      "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log", "boosted_tree_auc")){
    return(TRUE)
  } else if(outcome_type=="count" & learner %in% c("boosted_glm", "boosted_glm_poisson", "boosted_tree", "boosted_tree_poisson")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.mbboost.param <- function(data_obj, learner){

  # The mboost package offers multiple base learners. We have implemented two:
  # generalised linear models and decision trees.
  if(stringi::stri_startswith_fixed(str=learner, pattern="boosted_glm")) {
    base_model <- "glm"
  } else if(stringi::stri_startswith_fixed(str=learner, pattern="boosted_tree")) {
    base_model <- "tree"
  }
  
  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$base_model    <- list()
  param$sign_size     <- list()
  param$family        <- list()
  param$n_boost       <- list()
  param$learning_rate <- list()
  
  if(base_model == "tree"){
    param$tree_depth    <- list()
    param$min_child_weight <- list()
    param$alpha <- list()
  }

  # If data_obj is explicitly NULL, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Extract outcome type
  outcome_type <- data_obj@outcome_type

  
  ##### Base model #############################################################
  
  # Set the base_model parameter
  param$base_model <- .set_hyperparameter(default=base_model, type="factor", range=base_model,
                                          randomise=FALSE)
  
  
  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)
  
  
  ##### Model family #####
  param$family$type  <- "factor"
  param$family$range <- c("logistic", "probit", "bin_loglog", "cauchy", "log", "auc", "gaussian", "huber", "laplace", "poisson", "cox",
                          "weibull", "lognormal", "surv_loglog", "gehan", "cindex", "multinomial")

  # Read family string by parsing learner
  fam <- stringi::stri_replace_first_regex(str=learner, pattern="boosted_glm|boosted_tree", replace="")
  if(fam!=""){ fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="") }

  # Define the family based on the name of the learner.
  if(fam==""){
    # No specific family is provided.
    if(outcome_type == "continuous"){
      family_default <- c("gaussian", "huber", "poisson")
      
    } else if(outcome_type == "count"){
      family_default <- "poisson"
      
    } else if(outcome_type=="binomial") {
      family_default <- c("logistic", "probit", "bin_loglog", "cauchy", "log")
      
    } else if(outcome_type=="multinomial"){
      family_default <- "multinomial"
      
    } else if(outcome_type=="survival"){
      family_default <- "cox"
    }
    
  } else if(fam=="surv"){
    # A survival family is provided, but not specified further.
    family_default <- c("weibull", "lognormal", "surv_loglog")
    
  } else if(fam=="loglog") {
    # "loglog" is a collection of families that should be further split
    # according to outcome type.
    if(outcome_type=="binomial") {
      family_default <- "bin_loglog"
      
    } else if(outcome_type=="survival") {
      family_default <- "surv_loglog"
    }
    
  } else {
    # Other families are uniquely defined.
    family_default <- fam
  }
  
  # Set the family parameter.
  param$family <- .set_hyperparameter(default=family_default, type="factor", range=family_default,
                                      randomise=ifelse(length(family_default) > 1, TRUE, FALSE))

  ##### Number of boosting iterations ##########################################
  
  # This parameter could be set using the cv or cvrisk functions in mboost.
  # However, the SMAC hyperoptimisation method implemented in the framework is
  # superior to that of the grid-search method of cv and cvrisk This
  # hyper-parameter is expressed on the log 10 scale
  param$n_boost <- .set_hyperparameter(default=c(0, 1, 2, 3), type="numeric", range=c(0, 3),
                                       valid_range=c(0, Inf), randomise=TRUE)


  ##### Learning rate ##########################################################
  
  # Learning rate is on a log10 scale and determines
  # how fast the algorithm tries to learn. Lower values typically lead to better
  # models, but converge slower.
  param$learning_rate <- .set_hyperparameter(default=c(-3, -2, -1), type="numeric", range=c(-5, 0),
                                             valid_range=c(-Inf, 0), randomise=TRUE)


  # Parameters for tree-based learners
  if(base_model == "tree"){
    ##### Tree maximum depth ###################################################

    # This hyperparameter is only used by tree models. Larger depths increase
    # the risk of overfitting.
    param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7), type="integer", range=c(1, 10),
                                            valid_range=c(1, Inf), randomise=TRUE)
    
    
    ##### Minimum sum of instance weight #######################################
    
    # We implement this on a power(10) scale, with -1 offset.
    param$min_child_weight <- .set_hyperparameter(default=c(0, 1, 2), type="numeric", range=c(0, 2),
                                                  valid_range=c(0, Inf), randomise=TRUE)
    
    
    ##### Significance threshold for splitting #################################
    
    # Sets the significance level required to allow a split on a variable.
    param$alpha <- .set_hyperparameter(default=c(0.05, 0.1, 0.5, 1.0), type="numeric", range=c(10^-6, 1.0),
                                       valid_range=c(0.0, 1.0), randomise=TRUE, distribution="log")
  }

  # Return hyper-parameters
  return(param)
}



learner.mbboost.train <- function(object, data_obj){

  # Extract information
  outcome_type <- object@outcome_type
  param <- object@hyperparameters

  # Initiate model_list list
  model_list <- list()

  # Aggregate repeated measurement data - mboost does not facilitate repeated measurements
  data_obj <- aggregate_data(data=data_obj)

  # Use effect coding to convert categorical data into encoded data
  contr_list <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt <- contr_list$dt_contrast
 
  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Parse formula
  if (outcome_type=="multinomial"){
    stop("mboost does not currently support multinomial outcomes.")
    
  } else if(outcome_type == "survival") {
    formula <- stats::reformulate(termlabels=feature_cols, response=quote(survival::Surv(outcome_time, outcome_event)))
    
  } else {
    formula <- stats::reformulate(termlabels=feature_cols, response=quote(outcome))
  }

  # Get family object
  family_obj <- learner.mbboost.get_boosting_family(fam=param$family)

  # Set control object - note that learning rate is defined on the log 10 scale
  control_obj <- mboost::boost_control(mstop = round(10^param$n_boost),
                                       nu = 10^param$learning_rate)

  # Generate gradient boosted model
  if(param$base_model=="glm") {

    # Attempt to create model
    model_obj <- tryCatch({ mboost::glmboost(formula, data=dt, family=family_obj, center=FALSE, control=control_obj) },
                          error=function(err) { return(NULL) } )
    
  } else if(param$base_model=="tree") {

    # Set tree controls - note that every parameter except max depth is kept at default for mboost
    tree_control_obj <- partykit::ctree_control(testtype = "Univariate",
                                                maxdepth = param$tree_depth,
                                                minsplit = 10^param$min_child_weight - 1,
                                                mincriterion = 1 - param$alpha,
                                                saveinfo = FALSE)

    # Attempt to create model
    model_obj <- tryCatch({ mboost::blackboost(formula, data=dt, family=family_obj, control=control_obj, tree_controls=tree_control_obj) },
                          error=function(err) { return(NULL) } )
  } else {
    ..error_reached_unreachable_code("learner_mbboost_train_unknown_base_model")
  }

  # Append model to list
  model_list$model      <- model_obj

  # Update model status
  if(is.null(model_obj)) { model_list$model_trained <- FALSE }
  else {                   model_list$model_trained <- TRUE }

  # Add the contrast references
  model_list$contrast_ref <- contr_list$dt_ref

  # Add feature column names
  model_list$feature_cols <- feature_cols

  return(model_list)
}



learner.mbboost.test <- function(object, data_obj){

  # Get the type of outcome
  outcome_type <- object@outcome_type

  # Use effect coding to convert categorical data into encoded data
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Generate skeleton of prediction data table from dt
  dt_pred <- dt[, get_non_feature_columns(x=object), with=FALSE]

  # Evaluate trained models
  if(model_is_trained(object=object)){

    # Predict classes and probabilities for binomial and multinomial outcomes
    if(outcome_type=="binomial"){

      # Get predicted classes and probabilities
      if(object@hyperparameters$base_model=="glm"){
        pred_outc_class <- mboost::predict.glmboost(object=object@model$model, newdata=dt, type="class")
        pred_outc_prob  <- mboost::predict.glmboost(object=object@model$model, newdata=dt, type="response")
      } else {
        pred_outc_class <- mboost::predict.mboost(object=object@model$model, newdata=dt, type="class")
        pred_outc_prob  <- mboost::predict.mboost(object=object@model$model, newdata=dt, type="response")
      }

      # Set predicted class
      dt_pred[, "outcome_pred_class":=pred_outc_class]

      # Add class probabilities (mboost gives probability for the second class)
      outcome_pred_class_prob_cols <- get_class_probability_name(x=object)
      dt_pred[, (outcome_pred_class_prob_cols[1]):=1-pred_outc_prob]
      dt_pred[, (outcome_pred_class_prob_cols[2]):=pred_outc_prob]

    } else if(outcome_type=="multinomial") {
      # Multinomial is not supported.
      stop()
#
#       # Get predicted classes and probabilities
#       pred_outc_class   <- mboost::predict.glmboost(object=model_list$model, newdata=dt, type="class")
#       pred_outc_prob    <- mboost::predict.glmboost(object=model_list$model, newdata=dt, type="response")
#
#       # Add to dt_pred data.table



    } else if(outcome_type=="survival") {
      # Get family name
      family_name <- object@model$model$family@name

      # Determine output type
      if(family_name %in% c("Cox Partial Likelihood", "Concordance Probability by Uno", "Gehan loss")){
        pred_type <- "link"
      } else if(family_name %in% c("Negative Weibull Likelihood", "Negative Log Logistic Likelihood", "Negative Lognormal Likelihood")){
        pred_type <- "response"
      }

      # Get predicted values
      if(object@hyperparameters$base_model=="glm"){
          pred_outc     <- mboost::predict.glmboost(object=object@model$model, newdata=dt, type=pred_type)
      } else {
          pred_outc     <- mboost::predict.mboost(object=object@model$model, newdata=dt, type=pred_type)
      }

      # Check model family and convert linear predictors to hazard ratio
      if(family_name == "Cox Partial Likelihood"){
        # Cox partial likelihood produces the linear predictor, not relative risks
        pred_outc <- exp(pred_outc)
      } else if (family_name %in% c("Concordance Probability by Uno", "Gehan loss")){
        # Concordance probability and gehan loss produce "time-like" predictions before calibration using cox models, whereas "risk-like" is expected
        pred_outc <- -pred_outc
      }

      # Add to dt_pred data.table
      dt_pred[, "outcome_pred":=pred_outc[,1]]

      # If no valid predictions are made, replace dt_pred by a standard prediction table with NA values
      if(!any(is.finite(dt_pred$outcome_pred))){
        dt_pred <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)
      }
    } else if(outcome_type %in% c("continuous", "count")){

      # Predict response for cont
      if(object@hyperparameters$base_model=="glm"){
        pred_outc       <- mboost::predict.glmboost(object=object@model$model, newdata=dt, type="response")
      } else {
        pred_outc       <- mboost::predict.mboost(object=object@model$model, newdata=dt, type="response")
      }

      # Add to dt_pred data.table
      dt_pred[, "outcome_pred":=pred_outc[,1]]

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



learner.mbboost.vimp <- function(object){
  # Suppress NOTES due to non-standard evaluation in data.table
  variable <- NULL

  # Define exceptions
  if(!model_is_trained(object=object)){
    return(getEmptyVimp())
    
  } else if(length(object@hyperparameters$base_model) == 0){
    return(getEmptyVimp())
    
  } else if(!object@hyperparameters$base_model == "glm"){
    return(getEmptyVimp())
  }
  
  # varimp for tree-based versions seems to malfunction
  
  # Use varimp function from mboost to extract a data table
  dt_vimp_score     <- data.table::as.data.table(mboost::varimp(object@model$model))
  
  # Select only existing features
  dt_vimp_score     <- dt_vimp_score[variable %in% object@model$feature_cols, ]
  
  # Convert factor to character
  dt_vimp_score$variable <- as.character(dt_vimp_score$variable)
  
  # Parse score to data.table
  dt_vimp           <- data.table::data.table("score"=dt_vimp_score$reduction, "name"=dt_vimp_score$variable)
  dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="max")
  dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
  dt_vimp$multi_var <- TRUE

  return(dt_vimp)
}



learner.mbboost.recalibrate <- function(object, data_obj, time_max){
  # Recalibration is performed using stanrd methods
  if(object@outcome_type %in% c("survival") & object@hyperparameters$family %in% c("gehan", "cindex")){
    calibration_model <- learner.recalibrate_model(object=object, data_obj=data_obj, time_max=time_max)
  } else {
    calibration_model <- NULL
  }

  return(calibration_model)
}


learner.mbboost.get_boosting_family <- function(fam){

  # Load families for boosting
  if(fam=="logistic")          { family_obj <- mboost::Binomial(link="logit") }
  else if(fam=="probit")       { family_obj <- mboost::Binomial(link="probit") }
  else if(fam=="bin_loglog")   { family_obj <- mboost::Binomial(link="cloglog") }
  else if(fam=="cauchy")       { family_obj <- mboost::Binomial(link="cauchit") }
  else if(fam=="log")          { family_obj <- mboost::Binomial(link="log") }
  else if(fam=="auc")          { family_obj <- mboost::AUC() }
  else if(fam=="gaussian")     { family_obj <- mboost::Gaussian() }
  else if(fam=="huber")        { family_obj <- mboost::Huber() }
  else if(fam=="laplace")      { family_obj <- mboost::Laplace() }
  else if(fam=="poisson")      { family_obj <- mboost::Poisson() }
  else if(fam=="multinomial")  { family_obj <- mboost::Multinomial() }
  else if(fam=="cox")          { family_obj <- mboost::CoxPH() }
  else if(fam=="weibull")      { family_obj <- mboost::Weibull() }
  else if(fam=="lognormal")    { family_obj <- mboost::Lognormal() }
  else if(fam=="surv_loglog")  { family_obj <- mboost::Loglog() }
  else if(fam=="gehan")        { family_obj <- mboost::Gehan() }
  else if(fam=="cindex")       { family_obj <- mboost::Cindex() }

  return(family_obj)
}



learner.mbboost.prediction_type <- function(learner, outcome_type) {

  # Cox models predict hazard ratios. The outputs of Cindex and Gehan families are calibrated to produce hazard ratios.
  if(outcome_type=="survival" & learner %in% c("boosted_glm", "boosted_glm_cox",  "boosted_tree", "boosted_tree_cox",
                                               "boosted_glm_cindex", "boosted_tree_cindex", "boosted_glm_gehan", "boosted_tree_gehan")){
    return("hazard_ratio")
  } else if (outcome_type=="survival" & learner %in% c("boosted_glm_surv", "boosted_glm_loglog", "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_tree_surv",
                                                       "boosted_tree_loglog", "boosted_tree_weibull", "boosted_tree_lognormal") ) {
    return("expected_survival_time")
  } else {
    return(NULL)
  }
}



learner.mbboost.calibration_info <- function(object, data_obj, time_max=NULL){

  if(object@outcome_type=="survival"){
    
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



learner.mbboost.get_calibration <- function(object, data_obj, time_max){

  # Extract internal variables
  outcome_type <- object@outcome_type
  learner      <- object@learner

  if(outcome_type=="survival" & learner %in% c("boosted_glm", "boosted_glm_cox",  "boosted_tree", "boosted_tree_cox",
                                               "boosted_glm_cindex", "boosted_tree_cindex", "boosted_glm_gehan", "boosted_tree_gehan")){
    
    # Get survival probabilities
    probability_table <- learner.survival_probability.relative_risk(object=object,
                                                                    data_obj=data_obj,
                                                                    time_max=time_max)
    
    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object,
                                                      probability_table=probability_table,
                                                      time_max=time_max)

  } else if (outcome_type=="survival" & learner %in% c("boosted_glm_surv", "boosted_glm_loglog", "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_tree_surv",
                                                       "boosted_tree_loglog", "boosted_tree_weibull", "boosted_tree_lognormal") ) {
    
    # These methods (sadly) do not return any information that can be used for calibration.
    calibration_table <- create_empty_calibration_table(outcome_type=outcome_type)

  } else if(outcome_type %in% c("binomial", "multinomial") & learner.mbboost.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.categorical(object=object,
                                                         data_obj=data_obj)

  } else if(outcome_type %in% c("count", "continuous") & learner.mbboost.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.regression(object=object,
                                                        data_obj=data_obj)

  } else {
    ..error_reached_unreachable_code("learner.mbboost.get_calibration: unknown combination of outcome and learner.")
  }

  return(calibration_table)
}

