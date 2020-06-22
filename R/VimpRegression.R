vimp.regression.learner <- function(method, outcome_type){
  # Even though univariate regression makes use of learners, there is only one tunable hyperparameter,
  # which can only be set after determining variable importance.

  return(NULL)
}

vimp.regression.outcome <- function(method, outcome_type){

  if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
    return(TRUE)
  } else {
    return(FALSE)
  }

}



vimp.regression.param <- function(data_obj, method){
  # Parameters for univariate and multivariate regression learners

  # Internal
  outcome_type <- data_obj@outcome_type

  param <- list()
  param$n_bootstrap <- list()
  param$alpha       <- list()
  param$learner     <- list()
  param$metric      <- list()
  
  if(is.null(data_obj)) { return(param) }
  
  ##### Number of bootstrap iterations #####
  param$n_bootstrap <- .set_hyperparameter(default=10L, type="integer", range=c(1, Inf), randomise=FALSE)
  
  ##### Alpha-level for excluding features #####
  param$alpha <- .set_hyperparameter(default=0.05, type="numeric", range=c(0, 1), randomise=FALSE)

  ##### Distribution family to use for linear regresssion #####
  if(outcome_type == "binomial"){
    learner_default <- "glm_logistic"
    learner_range <- "glm_logistic"
    
  } else if(outcome_type == "multinomial") {
    learner_default <- "glm_multinomial"
    learner_range <- "glm_multinomial"
    
  } else if(outcome_type == "continuous") {
    learner_default <- "glm_gaussian"
    learner_range <- c("glm_gaussian", "glm_log", "glm_gaussian", "glm_inv_gaussian", "glm_poisson", "glm_log_poisson")
    
  } else if(outcome_type == "count"){
    learner_default <- "glm_poisson"
    learner_range <- c("glm_poisson", "glm_log_poisson")
    
  } else if(outcome_type == "survival"){
    learner_default <- "cox"
    learner_range <- c("cox", "survival_regr_weibull", "survival_regr_exponential", "survival_regr_gaussian", "survival_regr_logistic",
                       "survival_regr_lognormal", "survival_regr_loglogistic")
  } else if(outcome_type == "competing_risk"){
    ..error_outcome_type_not_implemented(outcome_type)
  } else {
    ..error_reached_unreachable_code("vimp_regression_param_unknown_outcome_type")
  }
  
  # Create the learner hyperparameter
  param$learner <- .set_hyperparameter(default=learner_default, type="factor", range=learner_range, randomise=FALSE)
  
  ##### Metric for evaluation #####
  if(outcome_type %in% c("binomial", "multinomial")){
    metric_default <- "auc_roc"
    metric_range <- c("auc_roc", "auc", "brier", "accuracy", "balanced_accuracy", "bac", "balanced_error_rate",
                      "ber", "sensitivity", "recall", "true_positive_rate", "tpr", "specificity", "true_negative_rate",
                      "tnr", "precision", "ppv", "npv", "false_discovery_rate", "fdr", "f1_score",  "kappa",
                      "mcc", "matthews_correlation_coefficient", "informedness", "youden_j", "youden_index", "markedness")
    
  } else if(outcome_type == "continuous") {
    metric_default <- "mse"
    metric_range <- .get_available_regression_metrics()
    
  } else if(outcome_type == "count"){
    metric_default <- "msle"
    metric_range <- .get_available_regression_metrics()
    
  } else if(outcome_type == "survival"){
    metric_default <- "concordance_index"
    metric_range <- c("concordance_index", "global_concordance_index")
    
  } else if(outcome_type == "competing_risk"){
    ..error_outcome_type_not_implemented(outcome_type)
  } else {
    ..error_reached_unreachable_code("vimp_regression_param_unknown_outcome_type")
  }
  
  # Create the metric hyperparameter
  param$metric <- .set_hyperparameter(default=metric_default, type="factor", range=metric_range, randomise=FALSE)
  
  return(param)
}



vimp.regression.vimp <- function(data_obj, method, param){

  # Suppress NOTES due to non-standard evaluation in data.table
  available <- score <- name <- obj_score <- obj_score_sd <- selected <- NULL

  # Internal
  outcome_type <- data_obj@outcome_type

  # Select only unique entries from dt (in case of bootstraps)
  dt            <- unique(data_obj@data)

  # Find feature columns in data table
  feature_cols  <- get_feature_columns(x=data_obj)
  non_feat_cols <- get_non_feature_columns(x=data_obj)

  # Generate iteration list
  iter_list     <- .create_bootstraps(sample_identifiers=dt$subject_id,
                                      n_iter=param$n_bootstrap,
                                      outcome_type=outcome_type, data=dt)

  # Generate score table
  dt_score      <- data.table::data.table("name"=feature_cols, "available"=TRUE, "selected"=FALSE,
                                          "select_step"=0, "score"=as.double(NA))

  # Calculate objective scores and convert to data.table
  dt_obj <- data.table::rbindlist(lapply(dt_score[available==TRUE, ]$name,
                                         function(ii, sel_feat, dt, iter_list, metric, learner, outcome_type) {
                                           vimp.regression.evaluate_model_(add_feat=ii, sel_feat=sel_feat, dt_data=dt,
                                                                           iter_list=iter_list, metric=metric,
                                                                           learner=learner, outcome_type=outcome_type)
                                           
                                         }, sel_feat=non_feat_cols, dt=dt, iter_list=iter_list,
                                         metric=param$metric, learner=param$learner, outcome_type=outcome_type)
                                  )

  # Further processing depends on the selected method
  if(method=="univariate_regression"){
    # Generate variable importance table
    dt_vimp           <- data.table::data.table("name"=dt_obj$name, "score"=dt_obj$obj_score)
    dt_vimp$rank      <- data.table::frank(-dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- FALSE

    return(dt_vimp)

  } else if(method=="multivariate_regression"){

    # Update objective score in dt_score
    dt_score[available==TRUE, "score":=dt_obj$obj_score]

    # Select the best performing feature and update table
    max_score          <- max(dt_score[available==TRUE, ]$score)
    max_feature        <- dt_score[available==TRUE & score==max_score, ]$name[1]
    dt_score[name==max_feature, ":="("available"=FALSE, "selected"=TRUE, "select_step"=1)]

    # Make too poorly performing features unavailable
    bad_features       <- dt_obj[max_score>(obj_score+stats::qnorm(1-param$alpha)*obj_score_sd) | !is.finite(obj_score),]$name
    dt_score[name %in% bad_features, "available":=FALSE]

    # Go to multivariate selection if the number of variables available is 1 or more
    available_features <- dt_score[available==TRUE, ]$name
    selected_features  <- dt_score[selected==TRUE, ]$name

    # Clean up
    rm(dt_obj, max_feature, bad_features)
    if(length(available_features)>0){

      # Iterate to maximum model size (breaks are set to stop earlier)
      for(jj in 2:nrow(dt_score)){
        # Generate a new iteration list to avoid become selective for the particular bootstraps
        iter_list <- .create_bootstraps(sample_identifiers=dt$subject_id,
                                        n_iter=param$n_bootstrap,
                                        outcome_type=outcome_type, data=dt)

        # Calculate objective scores for the models combining selected features with an additional feature and convert to data.table
        dt_obj             <- data.table::rbindlist(lapply(available_features, function(ii, sel_feat, dt, iter_list, metric, learner, outcome_type) (
          vimp.regression.evaluate_model_(add_feat=ii, sel_feat=sel_feat, dt_data=dt, iter_list=iter_list, metric=metric, learner=learner, outcome_type=outcome_type)),
          sel_feat=c(selected_features, non_feat_cols), dt=dt, iter_list=iter_list, metric=param$metric, learner=param$learner, outcome_type=outcome_type))

        # Determine if any scores improve the previous max_score; otherwise stop the analysis
        if(!any(dt_obj$obj_score > max_score)) { break() }

        # Update objective score in dt_score
        dt_score[available==TRUE, "score":=dt_obj$obj_score]

        # Select the best performing feature and update table
        max_score          <- max(dt_score[available==TRUE, ]$score)
        max_feature        <- dt_score[available==TRUE & score==max_score, ]$name[1]
        dt_score[name==max_feature, ":="("available"=FALSE, "selected"=TRUE, "select_step"=jj)]

        # Make too poorly performing features unavailable
        bad_features       <- dt_obj[max_score>(obj_score+stats::qnorm(1-param$alpha)*obj_score_sd) | !is.finite(obj_score),]$name
        dt_score[name %in% bad_features, "available":=FALSE]

        # Go to multivariate selection if the number of variables available is 1 or more
        available_features <- dt_score[available==TRUE, ]$name
        selected_features  <- dt_score[selected==TRUE, ]$name

        # Clean up
        rm(dt_obj, max_feature, bad_features)

        # Break if there are no more features available
        if(length(available_features)==0) { break() }

      }
    }

    # Generate variable importance data table
    dt_vimp <- dt_score[selected==TRUE, c("name", "score", "select_step")]

    # Update names
    data.table::setnames(dt_vimp, "select_step", "rank")

    # Add multivariate flag
    dt_vimp$multi_var <- TRUE

    return(dt_vimp)
  }

}



vimp.regression.evaluate_model_ <- function(add_feat, sel_feat, dt_data, iter_list, metric, learner, outcome_type){

  # Suppress NOTES due to non-standard evaluation in data.table
  bt_id <- obj_score_train <- obj_score_valid <- NULL

  # Create a dataObject using the subselection of data for the selected features "sel_feat" and the additional feature "add_feat"
  data_obj <- methods::new("dataObject", data=dt_data[, c(add_feat, sel_feat), with=FALSE], is_pre_processed=TRUE, outcome_type=outcome_type)

  # Build data table with bootstraps and performance scores
  dt_performance  <- data.table::data.table("bt_id"=seq_len(length(iter_list$train_list)), "obj_score_train"=as.double(NA), "obj_score_valid"=as.double(NA))

  # Get hyperparameters
  param <- .get_preset_hyperparameters(data=data_obj, learner=learner, names_only=FALSE)
  param <- lapply(param, function(list_entry) (list_entry$init_config[1]))

  # Iterate over bootstraps
  for(ii in dt_performance$bt_id){
    # Select train and test data
    data_train <- select_data_from_samples(data=data_obj, samples=iter_list$train_list[[ii]])
    data_test  <- select_data_from_samples(data=data_obj, samples=iter_list$valid_list[[ii]])

    # Create familiar model
    fam_model         <- methods::new("familiarModel",
                                      outcome_type = outcome_type,
                                      learner = learner,
                                      hyperparameters = param,
                                      signature = c(add_feat, sel_feat),
                                      req_feature_cols = c(add_feat, sel_feat),
                                      outcome_info = .get_outcome_info(x=data_obj))
    
    # Train model
    fam_model     <- .train(object=fam_model, data=data_train, get_additional_info=FALSE)

    # Collect objective scores
    score_train   <- assess_performance(object=fam_model, newdata=data_train, metric=metric, allow_recalibration=FALSE, as_objective=TRUE, na.rm=FALSE)
    score_test    <- assess_performance(object=fam_model, newdata=data_test, metric=metric, allow_recalibration=FALSE, as_objective=TRUE, na.rm=FALSE)

    # Add data to
    dt_performance[bt_id==ii, ":="("obj_score_train"=score_train, "obj_score_valid"=score_test)]

    rm(data_train, data_test, fam_model, score_train, score_test)
  }

  # Get score range
  score_range <- metric.get_metric_objective_range(metric=metric, outcome_type=outcome_type)
  min_score   <- min(score_range)
  max_score   <- max(score_range)

  # Deal with NA
  dt_performance[is.na(obj_score_train)|is.na(obj_score_valid), ":="("obj_score_train"=max_score, "obj_score_valid"=min_score)]

  # Calculate objective score
  dt_performance[, "obj_score":=obj_score_valid - abs(obj_score_valid - obj_score_train)]

  # Return mean objective score
  return(list("name"=add_feat, "obj_score"=mean(dt_performance$obj_score), "obj_score_sd"=stats::sd(dt_performance$obj_score)))
}
