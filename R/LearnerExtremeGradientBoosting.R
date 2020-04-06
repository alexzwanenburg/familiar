learner.xgboost.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type=="continuous" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_logistic", "xgboost_tree_logistic", "xgboost_lm_linear", "xgboost_tree_linear",
                                                 "xgboost_lm_poisson", "xgboost_tree_poisson", "xgboost_lm_gamma", "xgboost_tree_gamma")){
    return(TRUE)
  } else if(outcome_type=="multinomial" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_logistic", "xgboost_tree_logistic")){
    return(TRUE)
  } else if(outcome_type=="binomial" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_logistic", "xgboost_tree_logistic")){
    return(TRUE)
  } else if(outcome_type=="survival" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_cox", "xgboost_tree_cox")){
    return(TRUE)
  } else if(outcome_type=="count" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_poisson", "xgboost_tree_poisson")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.xgboost.param <- function(data_obj, learner){

  # Determine base model
  if(stringi::stri_startswith_fixed(str=learner, pattern="xgboost_lm")) {
    base_model <- "gblinear"
  } else if(stringi::stri_startswith_fixed(str=learner, pattern="xgboost_tree")) {
    base_model <- "gbtree"
  } else if(stringi::stri_startswith_fixed(str=learner, pattern="xgboost_dart")){
    base_model <- "dart"
  }

  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$base_model         <- list()
  param$sign_size          <- list()
  param$learn_objective    <- list()

  # General parameters
  param$n_boost            <- list()
  param$learning_rate      <- list()
  param$lambda             <- list()
  param$alpha              <- list()

  # Tree specific parameters
  if(base_model %in% c("gbtree", "dart")){
    param$tree_depth       <- list()
    param$sample_size      <- list()
    param$min_child_weight <- list()
    param$gamma            <- list()
  }

  # DART-specific parameters
  if(base_model=="dart"){
    param$sample_type      <- list()
    param$rate_drop        <- list()
  }

  # Linear model-specific parameters
  if(base_model=="gblinear"){

  }

  # If data_obj is explicitly NULL, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Extract outcome type.
  outcome_type <- data_obj@outcome_type

  # Determine number of samples.
  n_samples <- nrow(unique(data_obj@data, by=c("subject_id", "cohort_id")))

  ##### Base model #############################################################
  
  # The xgboost package offers multiple base learners, namely tree-based, linear
  # and DART.
  param$base_model <- .set_hyperparameter(default=base_model, type="factor", range=base_model, randomise=FALSE)
  
  
  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)
  

  ##### Model objective #####
  # xgboost has several families, or rather objectives, that can be used
  param$learn_objective$type  <- "factor"
  param$learn_objective$range <- c("linear", "continuous_logistic", "multinomial_logistic", "binomial_logistic" ,"poisson", "gamma", "cox")

  # Read family string by parsing learner
  fam <- stringi::stri_replace_first_regex(str=learner, pattern="xgboost_lm|xgboost_tree|xgboost_dart", replace="")
  if(fam!=""){ fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="") }

  # Define the objective based on the name of the learner.
  if(fam==""){
    # No specific objective is provided.
    if(outcome_type == "continuous"){
      learn_objective_default <- c("linear", "continuous_logistic", "poisson", "gamma")

    } else if(outcome_type=="count"){
      learn_objective_default <- "poisson"
      
    } else if(outcome_type=="binomial"){
      learn_objective_default <- "binomial_logistic"
      
    } else if(outcome_type=="multinomial"){
      learn_objective_default <- "multinomial_logistic"
      
    } else if(outcome_type=="survival"){
      learn_objective_default <- "cox"
    }
    
  } else if(fam=="logistic") {
    # Logistic learner objectives form a collection, and are now uniquely
    # defined according to the type of outcome.
    if(outcome_type=="binomial"){
      learn_objective_default <- "binomial_logistic"
      
    } else if(outcome_type=="multinomial"){
      learn_objective_default <- "multinomial_logistic"
      
    } else if(outcome_type=="continuous"){
      learn_objective_default <- "continuous_logistic"
    }
    
  } else {
    # Other objectives are uniquely defined.
    learn_objective_default <- fam
  }


  # Set the learn_objective parameter
  param$learn_objective <- .set_hyperparameter(default=learn_objective_default, type="factor",
                                               range=learn_objective_default,
                                               randomise=ifelse(length(learn_objective_default) > 1, TRUE, FALSE))

  ##### Number of boosting iterations ##########################################
  
  # This hyper-parameter is expressed on the log 10 scale. It is called nrounds
  # in xgboost.
  param$n_boost <- .set_hyperparameter(default=c(0, 1, 2, 3), type="numeric", range=c(0, 3),
                                       valid_range=c(0, Inf), randomise=TRUE)
  
  
  ##### Learning rate ##########################################################
  
  # Learning rate is on a log10 scale from -5 to 0 (0.00001 - 1), and determines
  # how fast the algorithm tries to learn. Lower values typically lead to better
  # models, but take longer to learn. This parameter is called eta by xgboost.
  param$learning_rate <- .set_hyperparameter(default=c(-3, -2, -1), type="numeric", range=c(-5, 0),
                                             valid_range=c(-Inf, 0), randomise=TRUE)

  
  ##### L2 regularisation term #################################################
  
  # The L2 regularisation term lambda lies in the half-open range [0, inf). This
  # term is implemented as a power(10) with a 10^-6 offset.
  param$lambda <- .set_hyperparameter(default=c(-6,-3,-1,1,3), type="numeric", range=c(-6, 3),
                                      valid_range=c(-6, Inf), randomise=TRUE)
  
  
  ##### L1 regularisation term #################################################
  
  # The L1 regularisation term alpha is implemented as the L2 regularisation
  # term.
  param$alpha <- .set_hyperparameter(default=c(-6,-3,-1,1,3), type="numeric", range=c(-6, 3),
                                     valid_range=c(-6, Inf), randomise=TRUE)
  
  # Parameters for tree-based gradient boosting
  if(base_model %in% c("gbtree", "dart")){
    
    ##### Maximum tree depth ###################################################
    
    # This hyperparameter is only used by tree models. The parameter is called
    # max_depth by xgboost. Larger depths increase the risk of overfitting.
    param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7), type="integer", range=c(1, 10),
                                            valid_range=c(1, Inf), randomise=TRUE)
    
    
    ##### Data subsampling fraction ############################################

    # Trees may be grown using only a subset of the data to limit overfitting.
    # The parameter is called subsample in xgboost.
    param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 1.00), type="numeric", range=c(2/n_samples, 1.0),
                                             valid_range=c(0, 1), randomise=TRUE)
    

    ##### Minimum sum of instance weight #######################################
    
    # Minimum sum of instance weight (hessian) needed in a child. If the tree
    # partition step results in a leaf node with the sum of instance weight less
    # than min_child_weight, then the building process will give up further
    # partitioning. In linear regression mode, this simply corresponds to
    # minimum number of instances needed to be in each node. The larger, the
    # more conservative the algorithm will be. (source:xgboost documentation)
    # 
    # We implement this on a power(10) scale, with -1 offset.
    param$min_child_weight <- .set_hyperparameter(default=c(0, 1, 2), type="numeric", range=c(0, 2),
                                                  valid_range=c(0, Inf), randomise=TRUE)
    

    ##### Minimum splitting error reduction ####################################
    
    # Minimum error reduction required for splitting. This hyper-parameters is
    # called gamma or min_split_loss in xgboost. We implement it on the
    # power(10) scale, with 10^-6 offset.
    #
    # For continuous and count-type outcomes, this parameter can be a bit tricky
    # due to a wide range in possible scales, and thus in error values. This is
    # resolved by normalising the outcome to the [0, 1] range.
    param$gamma <- .set_hyperparameter(default=c(-6,-3,-1,1,3), type="numeric", range=c(-6, 3),
                                       valid_range=c(-6, Inf), randomise=TRUE)
  }

  # Parameters for dart tree-based gradient boosting
  if(base_model=="dart"){

    ##### Dart booster sample type #############################################
    
    # Select the sample algorithm used by Dart booster.
    param$samply_type <- .set_hyperparameter(default=c("uniform", "weighted"), type="factor", range=c("uniform", "weighted"),
                                             randomise=TRUE)
    
    ##### Dart booster tree drop rate ##########################################
    
    # Fraction of previous trees to drop during dropout.
    param$rate_drop <- .set_hyperparameter(default=c(0, 0.1, 0.3), type="numeric", range=c(0,1), randomise=TRUE)
 
  }

  # Parameters for linear model-based boosting.
  if(base_model=="gblinear"){
    # Currently none.
  }

  return(param)
}



learner.xgboost.train <- function(object, data_obj){

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL

  # Extract information
  outcome_type <- object@outcome_type
  param        <- object@hyperparameters

  # Initiate model_list list
  model_list          <- list()

  # Aggregate repeated measurement data - xgboost does not facilitate repeated measurements
  data_obj            <- aggregate_data(data=data_obj)

  # Use effect coding to convert categorical data into numerical encoded data, as xgboost only works with numerical, not categorical, data
  contr_list          <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt                  <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols        <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Find outcome columns in data table
  outcome_cols        <- get_outcome_columns(x=outcome_type)

  # Build a xgb data matrix
  if(outcome_type %in% c("binomial", "multinomial")){
    # Convert categorical outcomes to numerical labels
    class_levels      <- get_outcome_levels(x=data_obj)
    class_num_labels  <- as.numeric(seq_len(length(class_levels)))-1
    dt_class_conv     <- data.table::data.table("class_level"=factor(class_levels, levels=class_levels), "num_label"=class_num_labels)

    # Convert categorical outcomes by adding numerical labels to the outcome table
    dt_outcome        <- data.table::copy(dt[, outcome_cols, with=FALSE])
    for(ii in seq_len(length(class_levels))){
      dt_outcome[outcome==dt_class_conv$class_level[ii], "outcome_num":=dt_class_conv$num_label[ii]]
    }

    # Save conversion table to model_list
    model_list$conversion_table <- dt_class_conv

    # Set outcome_labels
    outcome_labels    <- dt_outcome$outcome_num

  } else if(outcome_type %in% c("continuous", "count")){

    # Set outcome_labels
    outcome_labels    <- dt[[outcome_cols]]
    
    # Determine normalisation parameters so that outcome can be normalised to [0, 1] range.
    model_list$outcome_offset <- min(outcome_labels)
    model_list$outcome_scale <- max(outcome_labels) - min(outcome_labels)
    
    # Normalise outcome labels.
    outcome_labels <- (outcome_labels - model_list$outcome_offset) / model_list$outcome_scale

  } else if(outcome_type == "survival"){

    # According to the xgboost documentation, right censored survival time should be represented by negative values
    outcome_labels    <- dt[[outcome_cols[1]]]

    # Identify right-censored entries
    right_censored    <- which(dt[[outcome_cols[2]]] == 0)

    # Parse right-censored entries in outcome_labels to the correct representation
    if(length(right_censored)>0){
      outcome_labels[right_censored] <- outcome_labels[right_censored] * -1.0
    }
  }

  # Create a data_matrix object
  data_matrix         <- xgboost::xgb.DMatrix(data=as.matrix(dt[, feature_cols, with=FALSE]), label=outcome_labels)

  # Set the number of classes for the multi:softmax objective
  if(outcome_type == "multinomial"){
    param_n_class <- length(class_levels)
  } else {
    param_n_class <- 1
  }

  # Train model -- NOTE: xgb.train is imported through NAMESPACE as the predict function is not exported by xgboost
  model_obj <- xgb.train(params=list("booster" = param$base_model,
                                     "nthread" = 1,
                                     "eta" = 10^param$learning_rate,
                                     "max_depth" = param$tree_depth,
                                     "subsample" = param$sample_size,
                                     "min_child_weight" = 10^param$min_child_weight-1,
                                     "gamma" = 10^param$gamma - 10^-6,
                                     "lambda" = 10^param$lambda - 10^-6,
                                     "alpha" = 10^param$alpha - 10^-6,
                                     "sample_type" = param$sample_type,
                                     "rate_drop" = param$rate_drop,
                                     "objective" = learner.xgboost.get_boosting_objective(objective=param$learn_objective),
                                     "num_class" = param_n_class),
                         data=data_matrix, nrounds=round(10^param$n_boost), verbose=0)
  
  # Save model object to model_list
  model_list$model    <- model_obj

  # Add the contrast references
  model_list$contrast_ref <- contr_list$dt_ref

  # Add feature column names
  model_list$feature_cols <- feature_cols

  return(model_list)
}



learner.xgboost.test <- function(object, data_obj){

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_pred_class_num <- NULL

  # Get the type of outcome
  outcome_type      <- object@outcome_type
  
  # Get class levels
  class_levels <- get_outcome_class_levels(x=object)

  if(!model_is_trained(object)){
    return(createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type))
  }
  
  # Use effect coding to convert categorical data into encoded data
  contr_list        <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt                <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Generate skeleton of prediction data table from dt
  dt_pred           <- dt[, get_non_feature_columns(x=object), with=FALSE]

  # Predict classes and probabilities for binomial and multinomial outcomes
  if(outcome_type=="binomial"){

    # Get predicted probabilities for the second class
    pred_outc_prob  <- predict(object=object@model$model, newdata=as.matrix(dt[, feature_cols, with=FALSE]))

    # Set predicted numerical class label
    dt_pred[, "outcome_pred_class_num":=round(pred_outc_prob)]

    # Iterate over classes
    for(ii in seq_len(nrow(object@model$conversion_table))){
      dt_pred[outcome_pred_class_num==object@model$conversion_table$num_label[ii], "outcome_pred_class":=object@model$conversion_table$class_level[ii]]
    }

    # Remove numerical class label
    dt_pred[, "outcome_pred_class_num":=NULL]

    # Convert predicted class to factor
    dt_pred$outcome_pred_class <- factor(dt_pred$outcome_pred_class, levels=class_levels)

    # Add class probabilities (xgboost gives probability for the second class)
    outcome_pred_class_prob_cols <- getClassProbabilityColumns(outcome_type=outcome_type, class_levels=class_levels)
    dt_pred[, (outcome_pred_class_prob_cols[1]):=1-pred_outc_prob]
    dt_pred[, (outcome_pred_class_prob_cols[2]):=pred_outc_prob]

  } else if(outcome_type=="multinomial") {
    # Get predicted probabilities for the different classes
    pred_outc_prob  <- predict(object=object@model$model, newdata=as.matrix(dt[, feature_cols, with=FALSE]), reshape=TRUE)

    # Update predicted class
    pred_outc_class <- class_levels[apply(pred_outc_prob, 1, which.max)]
    dt_pred[, "outcome_pred_class":=factor(x=pred_outc_class, levels=class_levels)]

    # Add levels as column names to the probability matrix
    colnames(pred_outc_prob) <- class_levels

    # Add class probabilities
    outcome_pred_class_prob_cols <- getClassProbabilityColumns(outcome_type=outcome_type, class_levels=class_levels)
    dt_pred <- cbind(dt_pred, data.table::as.data.table(pred_outc_prob))
    data.table::setnames(dt_pred, old=class_levels, new=outcome_pred_class_prob_cols)

  } else if(outcome_type %in% c("continuous", "count")) {

    # Predict regression results
    pred_outc <- predict(object=object@model$model, newdata=as.matrix(dt[, feature_cols, with=FALSE]))

    # Rescale predicted outcome
    pred_outc <- pred_outc * object@model$outcome_scale + object@model$outcome_offset
    
    # Add to prediction data table
    dt_pred[, "outcome_pred":=pred_outc]

  } else if(outcome_type=="survival") {
    # xgboost documentation: predictions are returned on the hazard ratio scale (i.e., as HR = exp(marginal_prediction))

    # Predict regression results
    pred_outc <- predict(object=object@model$model, newdata=as.matrix(dt[, feature_cols, with=FALSE]), outputmargin=FALSE)

    # Add to prediction data table
    dt_pred[, "outcome_pred":=pred_outc]
  }

  # Return prediction data table
  return(dt_pred)
}



learner.xgboost.vimp <- function(object){
  # Suppress NOTES due to non-standard evaluation in data.table
  Weight <- score <- NULL

  if(!model_is_trained(object)){
    return(getEmptyVimp())
  }
  
  if(object@hyperparameters$base_model %in% c("gbtree", "dart")){
    # Use xgb.importance function to determine feature importance
    dt_vimp_score   <- xgboost::xgb.importance(feature_names=object@model$feature_cols, model=object@model$model)

    # Parse score to data.table
    dt_vimp         <- data.table::data.table("score"=dt_vimp_score$Gain, "name"=dt_vimp_score$Feature)

  } else if(object@hyperparameters$base_model == "gblinear"){
    # Use xgb.importance function to determine feature importance
    dt_vimp_score   <- xgboost::xgb.importance(feature_names=object@model$feature_cols, model=object@model$model)

    # Output are linear coefficients, which may be negative
    dt_vimp_score[, "Weight":=abs(Weight)]

    # For multinomial data, we keep the maximum weight by class
    if(object@outcome_type=="multinomial"){
      dt_vimp_score <- dt_vimp_score[, list(Weight=max(Weight)), by="Feature"]
    }

    # Parse score to data.table
    dt_vimp         <- data.table::data.table("score"=dt_vimp_score$Weight, "name"=dt_vimp_score$Feature)
  }

  # Complete data table
  dt_vimp           <- unique(dt_vimp[order(-score)], by="name")
  dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="max")
  dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
  dt_vimp$multi_var <- TRUE

  return(dt_vimp)
}



learner.xgboost.recalibrate <- function(object, data_obj, time_max){
  # Recalibration is performed using standard methods
  if(object@outcome_type %in% c("binomial", "multinomial", "survival")){
    calibration_model <- learner.recalibrate_model(object=object, data_obj=data_obj, time_max=time_max)
  } else {
    calibration_model <- NULL
  }

  return(calibration_model)
}



learner.xgboost.get_boosting_objective <- function(objective){

  # Load families for boosting
  if(objective=="linear")                    { boost_obj <- "reg:linear" }
  else if(objective=="continuous_logistic")  { boost_obj <- "reg:logistic" }
  else if(objective=="multinomial_logistic") { boost_obj <- "multi:softprob" }
  else if(objective=="binomial_logistic")    { boost_obj <- "binary:logistic" }
  else if(objective=="poisson")              { boost_obj <- "count:poisson" }
  else if(objective=="gamma")                { boost_obj <- "reg:gamma" }
  else if(objective=="cox")                  { boost_obj <- "survival:cox" }

  return(boost_obj)
}



learner.xgboost.prediction_type <- function(learner, outcome_type) {

  # Cox regression models predict risks. Note that a hazard-ratio is only produced after calibration.
  if(outcome_type=="survival" & learner %in% c("xgboost_lm", "xgboost_tree", "xgboost_lm_cox", "xgboost_tree_cox")){
    return("hazard_ratio")
  } else {
    return(NULL)
  }
}



learner.xgboost.calibration_info <- function(object, data_obj, time_max=NULL){

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



learner.xgboost.get_calibration <- function(object, data_obj, time_max){

  # Extract internal variables
  outcome_type <- object@outcome_type

  if(outcome_type=="survival" & learner.xgboost.outcome(object=object)){
    
    # Get survival probabilities using the relative risks method.
    probability_table <- learner.survival_probability.relative_risk(object=object,
                                                                    data_obj=data_obj,
                                                                    time_max=time_max)
    
    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object,
                                                      probability_table=probability_table,
                                                      time_max=time_max)
    
  } else if(outcome_type %in% c("binomial", "multinomial") & learner.xgboost.outcome(object=object)){

    # Obtain calibration data.
    calibration_table <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else if(outcome_type %in% c("count", "continuous") & learner.xgboost.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.regression(object=object, data_obj=data_obj)

  } else {
    ..error_reached_unreachable_code("learner.xgboost.get_calibration: unknown combination of outcome and learner.")
  }

  return(calibration_table)
}

