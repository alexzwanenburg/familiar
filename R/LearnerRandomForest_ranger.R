learner.rf_ranger.learner <- function(method, outcome_type){

  # Return base learner
  return("random_forest_ranger")
}



learner.rf_ranger.outcome <- function(learner=NULL, method=NULL, outcome_type=NULL, object=NULL){

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  # Learners and variable importance methods differ in name
  if(!is.null(learner)){
    if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  if(!is.null(method)){
    if(method %in% c("random_forest_ranger_holdout_permutation", "random_forest_ranger_permutation")){
      if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (method %in% c("random_forest_ranger_impurity")){
      # Impurity methods are only available for non-survival outcomes
      if(outcome_type %in% c("binomial", "multinomial", "continuous", "count")){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
}



learner.rf_ranger.param <- function(data_obj){

  # Initialise list and declare hyperparameter entries
  param <- list()
  param$sign_size   <- list()
  param$n_tree      <- list()
  param$sample_size <- list()
  param$m_try       <- list()
  param$node_size   <- list()
  param$tree_depth  <- list()
  param$split_rule  <- list()
  param$alpha       <- list()

  # The following hyperparameters are only used for feature selection
  param$fs_forest_type <- list()
  param$fs_vimp_method <- list()
  
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
  # ranger as an integer. Familiar ensures that always at least 1 feature is
  # available as a candidate.
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
  
  
  ##### Splitting rule #########################################################
  
  # Availability of splitting rules is dependent on the type of outcome.
  if(data_obj@outcome_type %in% c("binomial", "multinomial")){
    split_rule_range    <- c("gini", "extratrees")
    split_rule_default <- "gini"
  } else if(data_obj@outcome_type %in% c("continuous", "count")){
    split_rule_range    <- c("variance", "extratrees", "maxstat")
    split_rule_default <- "maxstat"
  } else if(data_obj@outcome_type == "survival" ){
    split_rule_range    <- c("logrank", "extratrees", "C", "maxstat")
    split_rule_default <- "maxstat"
  }
  
  # Set the split_rule parameter.
  param$split_rule <- .set_hyperparameter(default=split_rule_default, type="factor", range=split_rule_range,
                                          randomise=FALSE)
  

  ##### Significance threshold for splitting ###################################
  
  # Sets the significance level required to allow a split on a variable. It is
  # only used with maxstat.
  alpha_randomise <- split_rule_default == "maxstat"
  
  # Set the alpha parameter
  param$alpha <- .set_hyperparameter(default=c(0.5, 0.05, 0.1, 1.0), type="numeric", range=c(10^-6, 1.0),
                                     valid_range=c(0.0, 1.0), randomise=alpha_randomise,
                                     distribution="log")
  
  ##### Feature selection forest type ##########################################
  
  # Enables the construction of holdout forests. A conventional forest is grown
  # by default.
  param$fs_forest_type <- .set_hyperparameter(default="standard", type="factor",
                                              range=c("standard", "holdout"),
                                              randomise=FALSE)
  
  ##### Feature selection variable importance method ###########################
  
  # Enables the use of different variable importance methods. The permutation
  # method is used by default.
  param$fs_vimp_method <- .set_hyperparameter(default="permutation", type="factor",
                                              range=c("permutation", "impurity", "impurity_corrected"),
                                              randomise=FALSE)
  
  return(param)
}



learner.rf_ranger.train <- function(object, data_obj){

  # Extract information
  outcome_type   <- object@outcome_type
  param          <- object@hyperparameters

  # Aggregate repeated measurement data - ranger does not facilitate repeated measurements
  data_obj       <- aggregate_data(data=data_obj)

  # Initiate model_list list
  model_list     <- list()

  # Find feature columns in data table
  feature_cols   <- get_feature_columns(x=data_obj)

  # Parse formula
  if(outcome_type == "survival") {
    formula      <- stats::reformulate(termlabels=feature_cols, response=quote(survival::Surv(outcome_time, outcome_event)))
  } else {
    formula      <- stats::reformulate(termlabels=feature_cols, response=quote(outcome))
  }

  # Train probability trees for binomial and multinomial trees
  fit_probability <- ifelse(outcome_type %in% c("binomial", "multinomial"), TRUE, FALSE)

  # Generate random forest
  if(param$fs_forest_type=="standard"){
    # Conventional random forest (used for model building and variable importance estimations)
    # NOTE: ranger is imported through NAMESPACE to allow access to the predict generic
    model_obj      <- ranger(formula,
                             data = data_obj@data,
                             num.trees = 2^param$n_tree,
                             sample.fraction = param$sample_size,
                             mtry = max(c(1, ceiling(param$m_try * length(feature_cols)))),
                             min.node.size = param$node_size,
                             max.depth = param$tree_depth,
                             alpha = param$alpha,
                             splitrule = param$split_rule,
                             importance = param$fs_vimp_method,
                             probability = fit_probability,
                             num.threads = 1,
                             verbose = FALSE)
    
  } else if(param$fs_forest_type=="holdout") {
    # Hold-out random forest (used only for variable importance estimations)
    model_obj      <- ranger::holdoutRF(formula,
                                        data = data_obj@data,
                                        num.trees = 2^param$n_tree,
                                        sample.fraction = param$sample_size,
                                        mtry = max(c(1, ceiling(param$m_try * length(feature_cols)))),
                                        min.node.size = param$node_size,
                                        max.depth = param$tree_depth,
                                        alpha = param$alpha,
                                        splitrule = param$split_rule,
                                        probability = fit_probability,
                                        num.threads = 1,
                                        verbose = FALSE)
  }
  
  # Append model to list
  model_list$model <- model_obj

  return(model_list)
}



learner.rf_ranger.test <- function(object, data_obj, time_max, extra_output=FALSE){

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
  dt_pred           <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]

  # Predict outcome for new data
  pred_outc_obj     <- predict(object=object@model$model, data=data_obj@data, num.threads=1, verbose=FALSE)

  if(outcome_type %in% c("binomial", "multinomial")){

    # Extract class levels from the predictions
    class_levels    <- colnames(pred_outc_obj$predictions)

    # We have to determine the predicted class based on the class probabilities. We do so by first determining the column with the maximum probability.
    # Subsequently we read the corresponding class level, i.e. column name.
    pred_outc_class <- class_levels[apply(pred_outc_obj$predictions, 1, which.max)]
    pred_outc_class <- factor(pred_outc_class, levels=get_outcome_class_levels(x=object))

    # Set predicted class
    dt_pred[, "outcome_pred_class":=pred_outc_class]

    # Add class probabilities
    outcome_pred_class_prob_cols <- get_class_probability_columns(outcome_type=outcome_type, class_levels= class_levels)
    for(ii in seq_len(length(outcome_pred_class_prob_cols))){
      dt_pred[, (outcome_pred_class_prob_cols[ii]):=pred_outc_obj$predictions[, ii]]
    }

  } else if(outcome_type %in% c("survival")) {

    # Identify time closest to time_max
    time_id <- which.max(pred_outc_obj$unique.death.times[pred_outc_obj$unique.death.times<=time_max])
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
      event_times <- pred_outc_obj$unique.death.times
      
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

  } else if(outcome_type %in% c("continuous", "count")){
    # Extract predicted regression values
    dt_pred[, "outcome_pred":=pred_outc_obj$predictions]
  }

  # Return prediction data table
  return(dt_pred)
}



learner.rf_ranger.vimp <- function(data_obj=NULL, param=NULL, method="random_forest_ranger_permutation", object=NULL){

  # Suppress NOTES due to non-standard evaluation in data.table
  # score <- NULL

  # Determine variable importance method
  if(method %in% c("random_forest_ranger_holdout_permutation", "random_forest_ranger_permutation")){
    vimp_method <- "permutation"
  } else if(method %in% c("random_forest_ranger_impurity")){
    vimp_method <- "impurity_corrected"
  }

  # Determine forest type
  if(method %in% c("random_forest_ranger_holdout_permutation")){
    forest_type <- "holdout"
  } else if(method %in% c("random_forest_ranger_permutation", "random_forest_ranger_impurity")){
    forest_type <- "standard"
  }

  # Check if the model is provided, and train one otherwise. During feature selection, the model is trained here. However, when
  # variable importance of the final model is determined, a model is expected.
  if(is.null(object)){

    # Extract data
    outcome_type <- data_obj@outcome_type

    # Check if parameters have been set
    if(is.null(param)) { stop("Parameters should be provided for ranger random forest variable importance.") }

    # Update the fs_vimp_method and fs_forest_type hyperparameters
    param$fs_vimp_method <- vimp_method
    param$fs_forest_type <- forest_type
    
    # Train the model. This should be done when the function is called during feature selection
    object <- methods::new("familiarModel",
                           outcome_type = outcome_type,
                           learner = learner.rf_ranger.learner(method=method, outcome_type=outcome_type),
                           hyperparameters = param,
                           outcome_info = .get_outcome_info(x=data_obj))

    # Generate model for the given variable importance method and forest type
    object <- train(object=object, data=data_obj, get_recalibration=FALSE, get_additional_info=FALSE)

  }

  if(!model_is_trained(object)){
    return(getEmptyVimp())
  }
  
  # Extract the variable importance score
  if(forest_type=="standard"){
    vimp_score      <- ranger::importance(object@model$model)
  } else {
    vimp_score      <- object@model$model$variable.importance
  }

  # Create the variable importance data table
  dt_vimp           <- data.table::data.table("score"=vimp_score, "name"=names(vimp_score))
  dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
  dt_vimp$multi_var <- TRUE

  return(dt_vimp)
}



learner.rf_ranger.prediction_type <- function(learner, outcome_type){

  # Cox regression models predict risks, not expected survival times
  if(outcome_type=="survival"){
    return("sum_cumulative_hazard")
  } else {
    return(NULL)
  }

  return(FALSE)
}



learner.rf_ranger.calibration_info <- function(object, data_obj, time_max=NULL){

  if(object@outcome_type=="survival") {

    # Determine baseline survival.
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)

  } else if(object@outcome_type %in% c("count", "continuous")) {
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }

  return(calibration_info)
}



learner.rf_ranger.get_calibration <- function(object, data_obj, time_max){

  # Extract internal variables
  outcome_type <- object@outcome_type

  if(outcome_type=="survival" & learner.rf_ranger.outcome(object=object)){
    # Get survival probabilities.
    probability_table <- learner.survival_probability.random_forest(object=object, data_obj=data_obj, time_max=time_max)

    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object, probability_table=probability_table, time_max=time_max)

  } else if(outcome_type %in% c("binomial", "multinomial") & learner.rf_ranger.outcome(object=object)){

    # Obtain calibration data.
    calibration_table <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else if(outcome_type %in% c("count", "continuous") & learner.rf_ranger.outcome(object=object)) {

    # Obtain calibration data.
    calibration_table <- learner.calibration.regression(object=object, data_obj=data_obj)

  } else {
    ..error_reached_unreachable_code("learner.rf_ranger.get_calibration: unknown combination of outcome and learner.")
  }

  return(calibration_table)
}
