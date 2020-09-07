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
            learner_default <- switch(object@outcome_type,
                                      "binomial"="glm_logistic",
                                      "multinomial"="glm_multinomial",
                                      "continuous"="glm_gaussian",
                                      "count"="glm_poisson",
                                      "survival"="cox")
            
            # Set the learner range, i.e. all generalised linear models.
            learner_range <- unique(c(.get_available_glm_learners(),
                                      .get_available_cox_learners(),
                                      .get_available_survival_regression_learners()))
            
            # Determine which learners are available for the outcome_type
            learner_is_available <- sapply(learner_range,
                                           learner.check_outcome_type,
                                           outcome_type=object@outcome_type,
                                           as_flag=TRUE)
            
            # Create the learner hyperparameter
            param$learner <- .set_hyperparameter(default=learner_default,
                                                 type="factor",
                                                 range=learner_range[learner_is_available],
                                                 randomise=FALSE)
            
            ##### Metric for evaluation #####
            metric_default <- switch(object@outcome_type,
                                     "binomial"="auc_roc",
                                     "multinomial"="auc_roc",
                                     "continuous"="mse",
                                     "count"="msle",
                                     "survival"="concordance_index")
            
            # Get all available metrics.
            metric_range <- .get_all_metrics()
            
            # Determine which of the metrics is available for the outcome type.
            metric_is_available <- sapply(metric_range,
                                          metric.check_outcome_type,
                                          outcome_type=object@outcome_type,
                                          as_flag=TRUE)
            
            # Create the metric hyperparameter
            param$metric <- .set_hyperparameter(default=metric_default,
                                                type="factor",
                                                range=metric_range[metric_is_available],
                                                randomise=FALSE)
            
            return(param)
          })



#####..vimp######
setMethod("..vimp", signature(object="familiarRegressionVimp"),
          function(object, data, ...){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- name <- available <- NULL
          
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
            
            # Create a generic model.
            fam_model <- promote_learner(object=methods::new("familiarModel",
                                                             outcome_type = object@outcome_type,
                                                             learner = object@hyperparameters$learner,
                                                             fs_method = "none",
                                                             feature_info = object@feature_info,
                                                             outcome_info = .compute_outcome_distribution_data(object=object@outcome_info, data=data)))
            
            # Create metric objects.
            metric_object_list <- lapply(object@hyperparameters$metric,
                                         as_metric,
                                         object=fam_model)
            
            # Add baseline values for each metric.
            metric_object_list <- lapply(metric_object_list,
                                         set_metric_baseline_value,
                                         object=fam_model,
                                         data=data)
           
            # Generate score table.
            score_table <- data.table::data.table("name"=feature_columns,
                                                  "available"=TRUE,
                                                  "selected"=FALSE,
                                                  "select_step"=0,
                                                  "score"=as.double(NA))
            
            # Compute objective score for every feature.
            objective_score <- lapply(feature_columns,
                                      ..regression_vimp_assess_feature,
                                      fam_model=fam_model,
                                      metric_objects=metric_object_list,
                                      data=data,
                                      iteration_list=iteration_list,
                                      fixed_set=NULL)
            
            # Combine into data.table.
            objective_score <- data.table::rbindlist(objective_score)
            
            # In case of univariate regression, return at this point.
            if(object@vimp_method == "univariate_regression"){
              
              # Create variable importance table.
              vimp_table <- data.table::data.table("name"=objective_score$name,
                                                   "score"=objective_score$score)
              
              # Add ranks.
              vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
              vimp_table[, "multi_var":=FALSE]
              
              return(vimp_table)
            }
            
            # Proceed with forward selection and multivariate regression.
            
            # Find the best performing feature.
            max_objective_score <- max(objective_score$score)
            best_feature <- objective_score[score == max_objective_score]$name
            
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
              bad_features <- tail(objective_score[(order(-score))], n=n_dropped)$name
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
                                        fam_model=fam_model,
                                        metric_objects=metric_object_list,
                                        data=data,
                                        iteration_list=iteration_list,
                                        fixed_set=selected_features)
              
              # Combine into data.table.
              objective_score <- data.table::rbindlist(objective_score)
              
              # Find the best performing feature.
              max_objective_score <- max(objective_score$score)
              best_feature <- objective_score[score == max_objective_score]$name
              
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
                bad_features <- tail(objective_score[(order(-score))], n=n_dropped)$name
                score_table[name %in% bad_features, "available":=FALSE]
              }
              
              # In addition, drop any features that do not improve upon the
              # previous best score.
              bad_features <- objective_score[score < previous_objective_score]$name
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
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })



..regression_vimp_assess_feature <- function(feature, fam_model, metric_objects, data, iteration_list, fixed_set){

  # Make local copy of the data prior to filtering features.
  data <- data.table::copy(data)
  
  # Remove features that are neither in feature nor fixed_set.
  data <- filter_features(data,
                          available_features=c(feature, fixed_set))
  
  # Iterate over bootstraps.
  performance_data <- lapply(seq_along(iteration_list$train_list), function(bootstrap_id, data, fam_model, metric_objects, iteration_list){
    
    # Select train and test data.
    train_data <- select_data_from_samples(data=data,
                                           samples=iteration_list$train_list[[bootstrap_id]])
    test_data <- select_data_from_samples(data=data,
                                          samples=iteration_list$valid_list[[bootstrap_id]])
    
    # Update the familiar model
    fam_model@hyperparameters$sign_size=length(c(feature, fixed_set))
    
    # Set additional parameters, e.g. the learner family.
    fam_model <- ..set_vimp_parameters(object=fam_model,
                                       method=fam_model@learner)
    
    # Train the model using the train data.
    fam_model <- .train(object=fam_model,
                        data=train_data,
                        get_additional_info=FALSE)
    
    # Compute score using the metrics.
    score_table <- mapply(function(data, data_set, object, metric_objects, settings){
      
      # Get metric names.
      metric_names <- sapply(metric_objects, function(metric_object) metric_object@metric)
      
      # Predict for the in-bag and out-of-bag datasets.
      prediction_table <- .predict(object=object,
                                   data=data)
      
      # Compute objective scores.
      metrics_objective_score <- sapply(metric_objects,
                                        compute_objective_score,
                                        data=prediction_table)
      
      # Return as data.table.
      return(data.table::data.table("metric"=metric_names,
                                    "data_set"=data_set,
                                    "objective_score"=metrics_objective_score))
    },
    data=list(train_data, test_data),
    data_set=c("training", "validation"),
    MoreArgs=list("object"=fam_model,
                  "metric_objects"=metric_objects),
    SIMPLIFY=FALSE)
    
    # Aggregate to a single table.
    score_table <- data.table::rbindlist(score_table, use.names=TRUE)
    
    # Add run id.
    score_table[, ":="("run_id"=bootstrap_id)]
    
    # Set the column order.
    data.table::setcolorder(score_table, neworder=c("run_id"))
    
    return(score_table)
  },
  data=data,
  fam_model=fam_model,
  metric_objects=metric_objects,
  iteration_list=iteration_list)
  
  # Combine performance data to a single table and compute optimisation scores.
  performance_data <- metric.compute_optimisation_score(score_table=data.table::rbindlist(performance_data),
                                                        optimisation_function="max_validation")
  
  # Compute the summary score.
  performance_data <- metric.summarise_optimisation_score(score_table=performance_data,
                                                          method="median")
  
  # Return sample mean and standard deviation of the objective score.
  return(data.table::data.table("name"=feature,
                                "score"=performance_data$optimisation_score))
}
