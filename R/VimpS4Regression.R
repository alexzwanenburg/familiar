#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarRegressionVimp",
         contains="familiarVimpMethod")

.get_available_regression_vimp_methods <- function(show_general=TRUE){
  return(c("univariate_regression", "multivariate_regression"))
}



#####is_available#####
setMethod("is_available", signature(object="familiarRegressionVimp"),
          function(object, ...){
            return(TRUE)
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarRegressionVimp"),
          function(object, data=NULL){

            param <- list()
            param$n_bootstrap <- list()
            # param$alpha       <- list()
            param$learner     <- list()
            param$metric      <- list()
            param$drop_rate   <- list()
            
            if(is.null(data)) return(param)
            
            ##### Number of bootstrap iterations #####
            param$n_bootstrap <- .set_hyperparameter(default=10L, type="integer", range=c(1, Inf), randomise=FALSE)
            
            ##### Alpha-level for excluding features #####
            # param$alpha <- .set_hyperparameter(default=0.01, type="numeric", range=c(0, 1), randomise=FALSE)
            
            ##### Rate at which least performing features are dropped #####
            param$drop_rate <- .set_hyperparameter(default=0.333, type="numeric", range=c(0, 1), randomise=FALSE)
            
            ##### Distribution family to use for linear regression #####
            if(object@outcome_type == "binomial"){
              learner_default <- "glm_logistic"
              learner_range <- "glm_logistic"
              
            } else if(object@outcome_type == "multinomial") {
              learner_default <- "glm_multinomial"
              learner_range <- "glm_multinomial"
              
            } else if(object@outcome_type == "continuous") {
              learner_default <- "glm_gaussian"
              learner_range <- c("glm_gaussian", "glm_log", "glm_gaussian", "glm_inv_gaussian", "glm_poisson", "glm_log_poisson")
              
            } else if(object@outcome_type == "count"){
              learner_default <- "glm_poisson"
              learner_range <- c("glm_poisson", "glm_log_poisson")
              
            } else if(object@outcome_type == "survival"){
              learner_default <- "cox"
              learner_range <- c("cox", "survival_regr_weibull", "survival_regr_exponential", "survival_regr_gaussian", "survival_regr_logistic",
                                 "survival_regr_lognormal", "survival_regr_loglogistic")
              
            } else {
              ..error_outcome_type_not_implemented(outcome_type)
            }
            
            # Create the learner hyperparameter
            param$learner <- .set_hyperparameter(default=learner_default, type="factor", range=learner_range, randomise=FALSE)
            
            ##### Metric for evaluation #####
            if(object@outcome_type %in% c("binomial", "multinomial")){
              metric_default <- "auc_roc"
              metric_range <- c("auc_roc", "auc", "brier", "accuracy", "balanced_accuracy", "bac", "balanced_error_rate",
                                "ber", "sensitivity", "recall", "true_positive_rate", "tpr", "specificity", "true_negative_rate",
                                "tnr", "precision", "ppv", "npv", "false_discovery_rate", "fdr", "f1_score",  "kappa",
                                "mcc", "matthews_correlation_coefficient", "informedness", "youden_j", "youden_index", "markedness")
              
            } else if(object@outcome_type == "continuous") {
              metric_default <- "mse"
              metric_range <- .get_available_regression_metrics()
              
            } else if(object@outcome_type == "count"){
              metric_default <- "msle"
              metric_range <- .get_available_regression_metrics()
              
            } else if(object@outcome_type == "survival"){
              metric_default <- "concordance_index"
              metric_range <- c("concordance_index", "global_concordance_index")

            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Create the metric hyperparameter
            param$metric <- .set_hyperparameter(default=metric_default, type="factor", range=metric_range, randomise=FALSE)
            
            return(param)
          })



#####..vimp######
setMethod("..vimp", signature(object="familiarRegressionVimp"),
          function(object, data, ...){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- objective_score_mean <- name <- available <- NULL
          
            if(is_empty(data)) return(callNextMethod())
            
            # Aggregate data.
            data <- aggregate_data(data)
            
            # Find feature columns in data table.
            feature_columns  <- get_feature_columns(x=data)

            # Generate iteration list.
            iteration_list <- .create_bootstraps(sample_identifiers=data@data$subject_id,
                                                n_iter=object@hyperparameters$n_bootstrap,
                                                outcome_type=object@outcome_type,
                                                data=data@data)
            
            # Generate score table.
            score_table <- data.table::data.table("name"=feature_columns,
                                                  "available"=TRUE,
                                                  "selected"=FALSE,
                                                  "select_step"=0,
                                                  "score"=as.double(NA))
            
            # Compute objective score for every feature.
            objective_score <- lapply(feature_columns,
                                      ..regression_vimp_assess_feature,
                                      object=object,
                                      data=data,
                                      iteration_list=iteration_list,
                                      fixed_set=NULL)
            
            # Combine into data.table.
            objective_score <- data.table::rbindlist(objective_score)
            
            # In case of univariate regression, return at this point.
            if(object@vimp_method == "univariate_regression"){
              
              # Create variable importance table.
              vimp_table <- data.table::data.table("name"=objective_score$name,
                                                   "score"=objective_score$objective_score_mean)
              
              # Add ranks.
              vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
              vimp_table[, "multi_var":=FALSE]
              
              return(vimp_table)
            }
            
            # Proceed with forward selection and multivariate regression.
            
            # Find the best performing feature.
            max_objective_score <- max(objective_score$objective_score_mean)
            best_feature <- objective_score[objective_score_mean == max_objective_score]$name
            
            # Update the score_table.
            score_table[name %in% best_feature, ":="("score"=max_objective_score, 
                                                     "available"=FALSE,
                                                     "selected"=TRUE,
                                                     "select_step"=1)]
            
            # Identify features to be dropped.
            n_available <- nrow(objective_score) - length(best_feature)
            n_dropped <- floor(object@hyperparameters$drop_rate * n_available)
            
            # Make dropped features unavailable.
            if(n_dropped > 0){
              # Identify the worst performing features.
              bad_features <- tail(objective_score[(order(-objective_score_mean))], n=n_dropped)$name
              score_table[name %in% bad_features, "available":=FALSE]
            }

            # Determine the features still available.
            available_features <- score_table[available == TRUE, ]$name
            selected_features  <- score_table[selected == TRUE, ]$name
            
            # Iteration counter.
            ii <- 2
            
            # Set current objective score.
            previous_objective_score <- max_objective_score
            
            while(length(available_features) > 0 ){
              # Generate new iteration list.
              iteration_list <- .create_bootstraps(sample_identifiers=data@data$subject_id,
                                                   n_iter=object@hyperparameters$n_bootstrap,
                                                   outcome_type=object@outcome_type,
                                                   data=data@data)
              
              # Compute objective score for every feature.
              objective_score <- lapply(available_features,
                                        ..regression_vimp_assess_feature,
                                        object=object,
                                        data=data,
                                        iteration_list=iteration_list,
                                        fixed_set=selected_features)
              
              # Combine into data.table.
              objective_score <- data.table::rbindlist(objective_score)
              
              # Find the best performing feature.
              max_objective_score <- max(objective_score$objective_score_mean)
              best_feature <- objective_score[objective_score_mean == max_objective_score]$name
              
              # Only continue adding, if the expected value changed.
              if(max_objective_score <= previous_objective_score) break()
              
              # Update the score_table.
              score_table[name %in% best_feature, ":="("score"=max_objective_score,
                                                       "available"=FALSE,
                                                       "selected"=TRUE,
                                                       "select_step"=ii)]
              
              # Identify features to be dropped.
              n_available <- nrow(objective_score) - length(best_feature)
              n_dropped <- floor(object@hyperparameters$drop_rate * n_available)
              
              # Make dropped features unavailable.
              if(n_dropped > 0){
                # Identify the worst performing features.
                bad_features <- tail(objective_score[(order(-objective_score_mean))], n=n_dropped)$name
                score_table[name %in% bad_features, "available":=FALSE]
              }
              
              # In addition, drop any features that do not improve upon the
              # previous best score.
              bad_features <- objective_score[objective_score_mean < previous_objective_score]$name
              score_table[name %in% bad_features, "available":=FALSE]
              
              # Determine the features still available.
              available_features <- score_table[available == TRUE, ]$name
              selected_features  <- score_table[selected == TRUE, ]$name
              
              # Update iteration counter.
              ii <- ii + 1
              
              # Set current objective score.
              previous_objective_score <- max_objective_score
            }
            
            # Create variable importance table from the score table.
            vimp_table <- score_table[selected == TRUE, c("name", "score", "select_step")]
            
            # Update names in the variable importance table.
            data.table::setnames(vimp_table, "select_step", "rank")
            
            # Add multivariate flag
            vimp_table[, "multi_var"] <- TRUE
            
            return(vimp_table)
          })



..regression_vimp_assess_feature <- function(feature, object, data, iteration_list, fixed_set){

  # Make local copy of the data prior to filtering features.
  data <- data.table::copy(data)
  
  # Remove features that are neither in feature nor fixed_set.
  data <- filter_features(data, available_features=c(feature, fixed_set))
  
  # Iterate over bootstraps.
  performance_data <- lapply(seq_along(iteration_list$train_list), function(bootstrap_id, data, object, iteration_list){
    
    # Select train and test data.
    train_data <- select_data_from_samples(data=data,
                                           samples=iteration_list$train_list[[bootstrap_id]])
    test_data <- select_data_from_samples(data=data,
                                          samples=iteration_list$valid_list[[bootstrap_id]])
    
    # Create familiar model.
    fam_model <- methods::new("familiarModel",
                              outcome_type = object@outcome_type,
                              learner = object@hyperparameters$learner,
                              hyperparameters = list("sign_size"=length(c(feature, fixed_set))),
                              outcome_info = object@outcome_info)
    
    # Promote the correct type.
    fam_model <- promote_learner(object=fam_model)
    
    # Set additional parameters, e.g. the learner family.
    fam_model <- ..set_vimp_parameters(object=fam_model, method=object@hyperparameters$learner)
    
    # Train the model using the train data.
    fam_model <- .train(object=fam_model, data=train_data, get_additional_info=FALSE)
    
    # Collect objective score for training data.
    train_score <- assess_performance(object=fam_model,
                                      newdata=train_data,
                                      metric=object@hyperparameters$metric,
                                      allow_recalibration=FALSE,
                                      as_objective=TRUE,
                                      na.rm=FALSE)
    
    # Collect objective score for test data.
    test_score <- assess_performance(object=fam_model,
                                     newdata=test_data,
                                     metric=object@hyperparameters$metric,
                                     allow_recalibration=FALSE,
                                     as_objective=TRUE,
                                     na.rm=FALSE)
    
    # Generate data.table to return as output.
    return(data.table::data.table("bootstrap_id"=bootstrap_id,
                                  "obj_score_train"=train_score,
                                  "obj_score_valid"=test_score))
    
  }, data=data, object=object, iteration_list=iteration_list)
  
  # Combine performance data to a single table.
  performance_data <- data.table::rbindlist(performance_data)
  
  # Compute objective score from performance data.
  performance_data <- metric.get_objective_score(performance_data,
                                                 metric=object@hyperparameters$metric,
                                                 objective="max_validation",
                                                 outcome_type=object@outcome_type)
  
  # Return sample mean and standard deviation of the objective score.
  return(data.table::data.table("name"=feature,
                                "objective_score_mean"=mean(performance_data$obj_score),
                                "objective_score_sd"=stats::sd(performance_data$obj_score)))
}
