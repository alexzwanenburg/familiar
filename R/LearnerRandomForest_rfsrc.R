learner.rf_rfsrc.learner <- function(method, outcome_type){

  # Return base learner
  return("random_forest_rfsrc")
}



learner.rf_rfsrc.outcome <- function(learner=NULL, method=NULL, outcome_type=NULL, object=NULL){

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  # Learners and variable importance methods differ in name
  if(!is.null(learner)){
    if(outcome_type %in% c("binomial", "multinomial", "continuous", "survival", "count", "competing_risk")){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # All methods are available for every outcome
  if(!is.null(method)){
    if(outcome_type %in% c("binomial", "multinomial", "continuous", "survival", "count", "competing_risk")){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}



learner.rf_rfsrc.param <- function(data_obj){
  
  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$sign_size    <- list()
  param$n_tree       <- list()
  param$sample_size  <- list()
  param$m_try        <- list()
  param$node_size    <- list()
  param$n_split      <- list()
  param$split_rule   <- list()

  # Variable importance only parameters (are not optimised by hyperparameter optimisation)
  param$fs_vh_fold   <- list()
  param$fs_vh_step_size <- list()
  param$fs_vh_n_rep <- list()


  # If dt is not provided, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Get the number of samples
  n_samples <- nrow(unique(data_obj@data, by=c("subject_id", "cohort_id")))

  ###### Signature size ########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)


  ###### Number of trees #######################################################
  
  # Note that the number of trees is defined in powers of 2, based on Oshiro, T.
  # M., Perez, P. S., & Baranauskas, J. A. (2012, July). How many trees in a
  # random forest?. In MLDM (pp. 154-168).
  param$n_tree <- .set_hyperparameter(default=c(4, 8, 10), type="integer", range=c(4, 10),
                                      valid_range=c(0, Inf), randomise=TRUE)


  ###### Sample size ###########################################################
  
  # Note that the sample size is here noted as a fraction, which corresponds to
  # the usage in ranger.
  param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 1.00), type="numeric", range=c(2/n_samples, 1.0),
                                           valid_range=c(0, 1), randomise=TRUE)


  ##### Number of candidate features selected at node ##########################
  
  # Note that the number of features is here noted as a fraction, but is used in
  # randomforestSRC as an integer. Familiar ensures that always at least 1
  # feature is available as a candidate.
  param$m_try <- .set_hyperparameter(default=c(0.1, 0.3, 0.5, 1.0), type="numeric", range=c(0.0, 1.0),
                                     randomise=TRUE)


  ##### Terminal node size #####################################################
  
  # Number of instances in the terminal node. Larger terminal node sizes limit
  # tree depth and overfitting.
  
  # Define the default range.
  node_size_range <- c(5, floor(n_samples / 3))
  
  # Define the default values.
  node_size_default <- c(5, 10, 20, 50)
  node_size_default <- node_size_default[node_size_default >= node_size_range[1] &
                                           node_size_default <= node_size_range[2]]
  
  # Set the node_size parameter.
  param$node_size <- .set_hyperparameter(default=node_size_default, type="integer", range=node_size_range,
                                         valid_range=c(1, Inf), randomise=TRUE)

  
  ##### Maximum tree depth #####################################################
  
  # Determines the depth trees are allowed to grow to. Larger depths increase
  # the risk of overfitting.
  param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7), type="integer", range=c(1, 10),
                                          valid_range=c(1, Inf), randomise=TRUE)
  

  ##### Number of split points #################################################
  
  # The number of split points for each candidate variable is not randomised by
  # default, and deterministic splitting is used.
  param$n_split <- .set_hyperparameter(default=0, type="integer", range=c(0, 10),
                                       valid_range=c(0, Inf), randomise=FALSE)


  ##### Splitting rule #########################################################

  # Splitting rule is dependent on the outcome
  if(data_obj@outcome_type %in% c("binomial", "multinomial")){
    split_rule_range <- c("gini", "auc", "entropy")
    split_rule_default <- "gini"
    
  } else if(data_obj@outcome_type %in% c("continuous", "count")){
    split_rule_range <- c("mse", "quantile.regr", "la.quantile.regr")
    split_rule_default <- "mse"
    
  } else if(data_obj@outcome_type == "survival" ){
    split_rule_range <- c("logrank", "logrankscore", "bs.gradient")
    split_rule_default <- "logrank"
    
  } else if(data_obj@outcome_type == "competing_risk"){
    split_rule_range       <- c("logrankCR", "logrank")
    split_rule_default <- "logrankCR"
  }

  # Set the split_rule parameter.
  param$split_rule <- .set_hyperparameter(default=split_rule_default, type="factor", range=split_rule_range,
                                          randomise=FALSE)
  
  
  ##### Variable hunting cross-validation folds (variable importance only) #####
  param$fs_vh_fold <- .set_hyperparameter(default=5, type="integer", range=c(2, n_samples),
                                          valid_range=c(2, Inf), randomise=FALSE)
  

  ##### Variable huntingstep size (variable importance only) #####
  param$fs_vh_step_size <- .set_hyperparameter(default=1, type="integer", range=c(1, 50),
                                               valid_range=c(1, Inf), randomise=FALSE)
  
  
  ##### Variable hunting repetitions (variable importance only) #####
  param$fs_vh_n_rep <- .set_hyperparameter(default=50, type="integer", range=c(1,50),
                                           valid_range=c(1, Inf), randomise=FALSE)
  
  return(param)
}



learner.rf_rfsrc.train <- function(object, data_obj){

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL

  # Extract information
  outcome_type <- object@outcome_type
  param        <- object@hyperparameters

  # Initiate model_list list
  model_list       <- list()

  # Aggregate repeated measurement data - randomForestSRC does not facilitate repeated measurements
  data_obj         <- aggregate_data(data=data_obj)

  # Find feature columns in data table
  feature_cols     <- get_feature_columns(x=data_obj)

  # Parse formula
  if(outcome_type == "survival") {
    # The formula parser of randomForestSRC::rfsrc doesn't like survival::Surv, but wants Surv instead. Otherwise it throws an error.
    formula        <- stats::reformulate(termlabels=feature_cols, response=quote(Surv(outcome_time, outcome_event)))
  } else {
    formula        <- stats::reformulate(termlabels=feature_cols, response=quote(outcome))
  }

  # For count-type outcomes, convert the outcome through a logarithmic transformation
  if(outcome_type == "count") {
    data_obj@data  <- data.table::copy(data_obj@data)[, "outcome":=log1p(outcome)]
  }

  # Generate random forest
  model_obj        <- randomForestSRC::rfsrc(formula, data=data_obj@data,
                                             ntree     = 2^param$n_tree,
                                             sampsize  = ceiling(param$sample_size * nrow(data_obj@data)),
                                             mtry      = max(c(1, ceiling(param$m_try * length(feature_cols)))),
                                             nodesize  = param$node_size,
                                             nodedepth = param$tree_depth,
                                             nsplit    = param$n_split,
                                             splitrule = param$split_rule)

  # Append model to list
  model_list$model <- model_obj

  return(model_list)
}



learner.rf_rfsrc.test <- function(object, data_obj, time_max, extra_output=FALSE){

  # Limit process to one core
  # options(rf.cores=as.integer(1))

  # Get the type of outcome
  outcome_type  <- object@outcome_type

  # Exceptions in case the model was not trained.
  if(!model_is_trained(object)){
    if(extra_output){
      return(list("predictions" = createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type)))
      
    } else {
      return(createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type))
    }
  }
  
  # Generate skeleton of prediction data table from dt
  dt_pred       <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]

  # Predict outcome for new data
  pred_outc_obj <- randomForestSRC::predict.rfsrc(object=object@model$model, newdata=data_obj@data)

  if(outcome_type %in% c("binomial", "multinomial")){

    # Set predicted class
    dt_pred[, "outcome_pred_class":=pred_outc_obj$class]

    # Add class probabilities
    outcome_pred_class_prob_cols <- get_class_probability_columns(data_obj=data_obj)
    for(ii in seq_len(length(outcome_pred_class_prob_cols))){
      dt_pred[, (outcome_pred_class_prob_cols[ii]):=pred_outc_obj$predicted[, ii]]
    }

  } else if(outcome_type %in% c("continuous")) {
    # Add predicted value
    dt_pred[, "outcome_pred":=pred_outc_obj$predicted]
  } else if(outcome_type %in% c("count")){
    # Add predicted count, after conversion to original scale
    dt_pred[, "outcome_pred":=expm1(pred_outc_obj$predicted)]
  } else if(outcome_type %in% c("survival")) {

    # Identify time closest to time_max
    time_id <- which.max(pred_outc_obj$time.interest[pred_outc_obj$time.interest<=time_max])
    if(length(time_max)==0){
      time_id <- 1
    }

    # Extract cumulative hazard function
    if(is.matrix(pred_outc_obj$chf)){
      # Keep all entries up to and including time_id. Rows are patients, whereas columns are unique event times.
      chf <- pred_outc_obj$chf[, seq_len(time_id)]
      if(time_id > 1){
        # If time_id == 1, only one column is kept
        dt_pred[, "outcome_pred":=rowSums(chf)]
      } else {
        dt_pred[, "outcome_pred":=chf]
      }
    } else {
      chf <- pred_outc_obj$chf[seq_len(time_id)]
      dt_pred[, "outcome_pred":=sum(chf)]
    }

    # Produce additional output, i.e. survival probabilities and event times
    if(extra_output){
      
      # Get the survival probability table
      survival_prob <- pred_outc_obj$survival
      
      # Get the unique event times
      event_times <- pred_outc_obj$time.interest
      
      # Parse failure times to a matrix in a single-sample result
      if(!is.matrix(survival_prob)){
        survival_prob <- matrix(data=survival_prob, ncol=length(survival_prob))
      }
      
      # Cast to data.table
      survival_table <- cbind(data.table::data.table("subject_id"=dt_pred$subject_id),
                              data.table::as.data.table(survival_prob))
      
      # Melt to long format
      survival_table <- data.table::melt(survival_table, id.vars="subject_id", variable.name="time_variable",
                                         value.name="survival_probability")
      
      # Create conversion table to convert temporary variables into the event times
      conversion_table <- data.table::data.table("time_variable"=paste0("V", seq_len(length(event_times))),
                                                 "event_time"=event_times)
      # Add in event times
      survival_table <- merge(x=survival_table, y=conversion_table, on="time_variable")
      
      # Drop the time_variable column
      survival_table[, "time_variable":=NULL]
      
      # Generate a prediction list
      prediction_list <- list("predictions" = dt_pred,
                              "survival" = survival_table)

      return(prediction_list)
    }

  }

  # Return prediction data table
  return(dt_pred)
}



learner.rf_rfsrc.vimp <- function(data_obj=NULL, param=NULL, method="random_forest_rfsrc_permutation", object=NULL){

  # Suppress NOTES due to non-standard evaluation in data.table
  # score <- NULL

  # Check if the model is provided, and train one otherwise. During feature selection, the model is trained here. However, when
  # variable importance of the final model is determined, a model is expected.
  if(is.null(object)){

    # Extract data
    outcome_type <- data_obj@outcome_type

    # Check if model hyperparameters have been set
    if(is.null(param)) { stop("Parameters should be provided for random forest variable importance calculations.") }

    # Train the model. This should be done when the function is called during feature selection
    object <- methods::new("familiarModel",
                           outcome_type = outcome_type,
                           learner = learner.rf_rfsrc.learner(method=method, outcome_type=outcome_type),
                           hyperparameters = param,
                           class_levels = get_outcome_levels(x=data_obj))

    # Generate model for the given variable importance method and forest type
    object <- train(object=object, data=data_obj, get_recalibration=FALSE, get_additional_info=FALSE)
  }

  # Determine if the model was successfully trained.
  if(!model_is_trained(object)){
    return(getEmptyVimp())
  }
  
  # Extract the variable importance score
  if(method %in% c("random_forest_permutation", "random_forest_rfsrc_permutation")){

    # Determine permutation variable importance
    vimp_score        <- randomForestSRC::vimp(object=object@model$model, importance="permute")$importance

    # The variable importance score for binomial and multinomial outcomes is per class
    if(is.matrix(vimp_score)){
        vimp_score_names <- rownames(vimp_score)
        vimp_score       <- vimp_score[,1]
    } else {
      vimp_score_names   <- names(vimp_score)
    }

    # Create the variable importance data table
    dt_vimp           <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
    dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- TRUE

  } else if(method %in% c("random_forest_minimum_depth", "random_forest_rfsrc_minimum_depth")){
    # Determine minimum depth variable importance
    vimp_score        <- randomForestSRC::var.select(object=object@model$model, method="md", verbose=FALSE)$md.obj$order

    # Select the "min depth" column, which is the first column
    if(is.matrix(vimp_score)){
      vimp_score_names  <- rownames(vimp_score)
      vimp_score        <- vimp_score[,1]
    } else {
      # vimp_score <- vimp_score[1]
      vimp_score_names  <- names(vimp_score)
    }

    # Create the variable importance data table
    dt_vimp           <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
    dt_vimp$rank      <- data.table::frank(dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- TRUE

  } else if(method %in% c("random_forest_variable_hunting", "random_forest_rfsrc_variable_hunting")){
    # Perform variable hunting
    vimp_score        <- randomForestSRC::var.select(object=object@model$model, method="vh",
                                                     K=param$fs_vh_fold, nstep=param$fs_vh_step_size,
                                                     nrep=param$fs_vh_n_rep,
                                                     verbose=FALSE, refit=FALSE)$varselect

    # Select the "rel.freq" column, which is the second column
    if(is.matrix(vimp_score)){
      vimp_score_names  <- rownames(vimp_score)
      vimp_score        <- vimp_score[,2]
    } else {
      # vimp_score <- vimp_score[1]
      vimp_score_names  <- names(vimp_score)
    }

    # Create the variable importance data table
    dt_vimp           <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
    dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- TRUE
  }

  return(dt_vimp)
}



learner.rf_rfsrc.prediction_type <- function(learner, outcome_type){

  # Cox regression models predict risks, not expected survival times
  if(outcome_type=="survival"){
    return("sum_cumulative_hazard")
  } else {
    return(NULL)
  }
}



learner.rf_rfsrc.calibration_info <- function(object, data_obj, time_max=NULL){

  if(object@outcome_type == "survival") {
    # Determine baseline survival
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)
    
  } else if(object@outcome_type %in% c("count", "continuous")) {
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }

  return(calibration_info)
}



learner.rf_rfsrc.get_calibration <- function(object, data_obj, time_max){

  # Extract internal variables
  outcome_type <- object@outcome_type

  if(outcome_type=="survival" & learner.rf_rfsrc.outcome(object=object)){
    # Get survival probabilities.
    probability_table <- learner.survival_probability.random_forest(object=object,
                                                                    data_obj=data_obj,
                                                                    time_max=time_max)
    
    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object,
                                              probability_table=probability_table,
                                              time_max=time_max)
    
  } else if(outcome_type %in% c("binomial", "multinomial") & learner.rf_rfsrc.outcome(object=object)){

    # Obtain calibration data.
    calibration_table <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else if(outcome_type %in% c("count", "continuous") & learner.rf_rfsrc.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.regression(object=object, data_obj=data_obj)

  } else {
    ..error_reached_unreachable_code("learner.rf_rfsrc.get_calibration: unknown combination of outcome and learner.")
  }

  return(calibration_table)
}
