#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarRFSRC",
         contains="familiarModel")

.get_available_rfsrc_learners <- function(show_general=TRUE) return(c("random_forest", "random_forest_rfsrc"))

.get_available_rfsrc_vimp_methods <- function(show_general=TRUE){
  return(c("random_forest_permutation", "random_forest_minimum_depth", "random_forest_variable_hunting",
           "random_forest_rfsrc_permutation", "random_forest_rfsrc_minimum_depth", "random_forest_rfsrc_variable_hunting",
           "random_forest_holdout", "random_forest_rfsrc_holdout"))
}

#####is_available#####
setMethod("is_available", signature(object="familiarRFSRC"),
          function(object, ...){
            # Random forests exists for all outcome types and variable
            # importance methods.
            return(TRUE)
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarRFSRC"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries
            param    <- list()
            param$sign_size    <- list()
            param$n_tree       <- list()
            param$sample_size  <- list()
            param$m_try        <- list()
            param$node_size    <- list()
            param$tree_depth   <- list()
            param$n_split      <- list()
            param$split_rule   <- list()
            
            # Variable importance only parameters (are not optimised by hyperparameter optimisation)
            param$fs_vimp_method  <- list()
            param$fs_vh_fold   <- list()
            param$fs_vh_step_size <- list()
            param$fs_vh_n_rep  <- list()
            
            
            # If dt is not provided, return the list with hyperparameter names only
            if(is.null(data)) return(param)
            
            # Get the number of samples
            n_samples <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="series"))
            
            ###### Signature size ########################################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ###### Number of trees #######################################################
            
            # Note that the number of trees is defined in powers of 2, based on Oshiro, T.
            # M., Perez, P. S., & Baranauskas, J. A. (2012, July). How many trees in a
            # random forest?. In MLDM (pp. 154-168).
            param$n_tree <- .set_hyperparameter(default=c(4, 8, 10), type="integer", range=c(4, 10),
                                                valid_range=c(0, Inf), randomise=TRUE)
            
            
            ###### Sample size ###########################################################
            
            # Note that the sample size is here noted as a fraction, which corresponds to
            # the usage in random forest SRC
            param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 0.90), type="numeric", range=c(2.0/n_samples, 1.00),
                                                     valid_range=c(0, 1.0), randomise=TRUE)
            
            
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
            if(data@outcome_type %in% c("binomial", "multinomial")){
              split_rule_range <- c("gini", "auc", "entropy")
              split_rule_default <- "gini"
              
            } else if(data@outcome_type %in% c("continuous", "count")){
              split_rule_range <- c("mse", "quantile.regr", "la.quantile.regr")
              split_rule_default <- "mse"
              
            } else if(data@outcome_type == "survival" ){
              split_rule_range <- c("logrank", "logrankscore", "bs.gradient")
              split_rule_default <- "logrank"
              
            } else if(data@outcome_type == "competing_risk"){
              split_rule_range <- c("logrankCR", "logrank")
              split_rule_default <- "logrankCR"
              
            } else {
              ..error_no_known_outcome_type(data@outcome_type)
            }
            
            # Set the split_rule parameter.
            param$split_rule <- .set_hyperparameter(default=split_rule_default, type="factor", range=split_rule_range,
                                                    randomise=FALSE)
            
            ##### variable importance method #####
            param$fs_vimp_method <- .set_hyperparameter(default="permutation", type="factor",
                                                        range=c("permutation", "minimum_depth", "variable_hunting", "holdout"),
                                                        randomise=FALSE)
            
            
            ##### Variable hunting cross-validation folds (variable importance only) #####
            param$fs_vh_fold <- .set_hyperparameter(default=5, type="integer", range=c(2, n_samples),
                                                    valid_range=c(2, Inf), randomise=FALSE)
            
            
            ##### Variable hunting step size (variable importance only) #####
            param$fs_vh_step_size <- .set_hyperparameter(default=1, type="integer", range=c(1, 50),
                                                         valid_range=c(1, Inf), randomise=FALSE)
            
            
            ##### Variable hunting repetitions (variable importance only) #####
            param$fs_vh_n_rep <- .set_hyperparameter(default=50, type="integer", range=c(1,50),
                                                     valid_range=c(1, Inf), randomise=FALSE)
            
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarRFSRC"),
          function(object, type="default"){
            
            if(object@outcome_type == "survival"){
              if(type == "default"){
                # Standard predictions.
                return("cumulative_hazard")
                
              } else if(type %in% c("survival", "survival_probability")){
                
                return("survival_probability")
                
              } else if(type %in% c("response", "cumulative_hazard")) {
                
                return("cumulative_hazard")
                
              } else {
                stop(paste0("Prediction type is not implemented: ", type))
              } 
              
            } else {
              return(callNextMethod())
            }
          })


#####..train####
setMethod("..train", signature(object="familiarRFSRC", data="dataObject"),
          function(object, data){
            
            # Aggregate repeated measurement data - randomForestSRC does not
            # facilitate repeated measurements.
            data <- aggregate_data(data=data)
            
            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Find feature columns in data table
            feature_columns <- get_feature_columns(x=data)
            
            # Parse formula.
            if(object@outcome_type == "survival") {
              Surv <- survival::Surv
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(Surv(outcome_time, outcome_event)))
              
            } else if(object@outcome_type %in% c("binomial", "multinomial", "count", "continuous")){
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(outcome))
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Extract hyperparameters from the model object.
            param <- object@hyperparameters
            
            # Determine the sample size
            sample_size <- ceiling(param$sample_size * nrow(data@data))
            sample_type <- ifelse(sample_size == nrow(data@data), "swr", "swor")
            
            # Generate random forest.
            model <- randomForestSRC::rfsrc(formula,
                                            data = data@data,
                                            ntree = 2^param$n_tree,
                                            samptype = sample_type,
                                            sampsize = sample_size,
                                            mtry = max(c(1, ceiling(param$m_try * length(feature_columns)))),
                                            nodesize = param$node_size,
                                            nodedepth = param$tree_depth,
                                            nsplit = param$n_split,
                                            splitrule = param$split_rule)
            
            # Add model to the object.
            object@model <- model
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarRFSRC", data="dataObject"),
          function(object, data, type="default", time=NULL, ...){
            
            if(type %in% c("default", "survival_probability")){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=data,
                                                                   type=type)
              
              # Make predictions using the model.
              model_predictions <- predict(object=object@model,
                                           newdata=data@data)
              
              
              if(object@outcome_type %in% c("binomial", "multinomial")){
                #####Categorical outcomes######
                
                # Set predicted class.
                prediction_table[, "predicted_class":=model_predictions$class]
                
                # Add class probabilities.
                class_probability_columns <- get_class_probability_name(x=object)
                for(ii in seq_along(class_probability_columns)){
                  prediction_table[, (class_probability_columns[ii]):=model_predictions$predicted[, ii]]
                }
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                #####Numerical outcomes######
                
                # Extract predicted regression values.
                prediction_table[, "predicted_outcome":=model_predictions$predicted]
                
              } else if(object@outcome_type %in% c("survival")){
                #####Survival outcomes######
                
                # Get the unique event times
                event_times <- model_predictions$time.interest
                
                # Set default time, if not provided.
                time <- ifelse(is.null(time), max(event_times), time)
                
                if(type == "default"){
                  # Cumulative hazard.
                  
                  # Get the cumulative hazards at the given time point.
                  prediction_table <- process_random_forest_survival_predictions(event_matrix=model_predictions$chf,
                                                                                 event_times=event_times,
                                                                                 prediction_table=prediction_table,
                                                                                 time=time,
                                                                                 type="cumulative_hazard")
                  
                } else if(type == "survival_probability"){
                  # Survival probability.
                  
                  # Get the survival probability at the given time point.
                  prediction_table <- process_random_forest_survival_predictions(event_matrix=model_predictions$survival,
                                                                                 event_times=event_times,
                                                                                 prediction_table=prediction_table,
                                                                                 time=time,
                                                                                 type="survival")
                  
                } else {
                  ..error_outcome_type_not_implemented(object@outcome_type)
                }
              }  
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Make predictions using the model.
              return(predict(object=object@model,
                             newdata=data@data,
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarRFSRC", data="dataObject"),
          function(object, data, time){
            
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            return(..predict(object=object, data=data, time=time, type="survival_probability"))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarRFSRC"),
          function(object, data=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Attempt to train the model if it has not been trained yet.
            if(!model_is_trained(object)) object <- ..train(object, data)
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Find the vimp_method specified.
            vimp_method <- object@hyperparameters$fs_vimp_method
            if(is.null(vimp_method)) ..error_reached_unreachable_code("..vimp,familiarRFSRC: vimp method was not specified as a hyperparameter (fs_vimp_method)")
            
            # Extract the variable importance score
            if(vimp_method == "permutation"){
              
              # Determine permutation variable importance
              vimp_score <- randomForestSRC::vimp(object=object@model, importance="permute")$importance
              
              # The variable importance score for binomial and multinomial outcomes is per class
              if(is.matrix(vimp_score)){
                vimp_score_names <- rownames(vimp_score)
                vimp_score <- vimp_score[, 1]
                
              } else {
                vimp_score_names <- names(vimp_score)
              }
              
              # Create the variable importance data table
              vimp_table <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
              vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
              vimp_table[, "multi_var":=TRUE]

            } else if(vimp_method == "minimum_depth"){
              # Determine minimum depth variable importance
              vimp_score <- randomForestSRC::var.select(object=object@model, method="md", verbose=FALSE)$md.obj$order
              
              # Select the "min depth" column, which is the first column
              if(is.matrix(vimp_score)){
                vimp_score_names <- rownames(vimp_score)
                vimp_score <- vimp_score[, 1]
                
              } else {
                vimp_score_names  <- names(vimp_score)
              }
              
              # Create the variable importance data table
              vimp_table <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
              vimp_table[, "rank":=data.table::frank(score, ties.method="min")]
              vimp_table[, "multi_var":=TRUE]
              
            } else if(vimp_method == "variable_hunting"){
              # Perform variable hunting
              vimp_score <- randomForestSRC::var.select(object=object@model,
                                                        method="vh",
                                                        K=object@hyperparameters$fs_vh_fold,
                                                        nstep=object@hyperparameters$fs_vh_step_size,
                                                        nrep=object@hyperparameters$fs_vh_n_rep,
                                                        verbose=FALSE,
                                                        refit=FALSE)$varselect
              
              # Select the "rel.freq" column, which is the second column
              if(is.matrix(vimp_score)){
                vimp_score_names <- rownames(vimp_score)
                vimp_score <- vimp_score[, 2]
                
              } else {
                vimp_score_names <- names(vimp_score)
              }
              
              # Create the variable importance data table
              vimp_table <- data.table::data.table("score"=vimp_score, "name"=vimp_score_names)
              vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
              vimp_table[, "multi_var":=TRUE]
              
            } else if(vimp_method == "holdout"){
              
              # Check that data is present.
              if(is_empty(data)){
                warning("Data is required to assess variable importance using randomForestRFSRC::holdout.vimp")
                return(callNextMethod())
              }
              
              if(get_n_features(data) <= 1){
                warning("Variable importance using randomForestRFSRC::holdout.vimp requires more than 1 feature.")
                
                return(data.table::data.table("score"=0.0,
                                              "name"=get_feature_columns(x=data),
                                              "rank"=1L,
                                              "multi_var"=TRUE))
              }
              
              # Find feature columns in the data.
              feature_columns <- get_feature_columns(x=data)
              
              # Parse formula.
              if(object@outcome_type == "survival") {
                # Creating a local instance of survival::Surv() prevents an
                # error.
                Surv <- survival::Surv
                formula <- stats::reformulate(termlabels=feature_columns,
                                              response=quote(Surv(outcome_time, outcome_event)))

              } else if(object@outcome_type %in% c("binomial", "multinomial", "count", "continuous")){
                formula <- stats::reformulate(termlabels=feature_columns,
                                              response=quote(outcome))
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }

              # Determine the sample size
              sample_size <- ceiling(object@hyperparameters$sample_size * nrow(data@data))
              sample_type <- ifelse(sample_size == nrow(data@data), "swr", "swor")
              
              # Perform holdout variable importance.
              vimp_score <-randomForestSRC::holdout.vimp(formula,
                                                         data = data@data,
                                                         samptype = sample_type,
                                                         sampsize = sample_size,
                                                         mtry = max(c(1, ceiling(object@hyperparameters$m_try * get_n_features(data)))),
                                                         nodesize = object@hyperparameters$node_size,
                                                         nodedepth = object@hyperparameters$tree_depth,
                                                         nsplit = object@hyperparameters$n_split,
                                                         splitrule = object@hyperparameters$split_rule,
                                                         verbose=FALSE)$importance
              
              # Select the "all" column, which is the first column
              if(is.matrix(vimp_score)){
                vimp_score_names <- rownames(vimp_score)
                vimp_score <- vimp_score[, 1]
                
              } else {
                vimp_score_names  <- names(vimp_score)
              }
              
              # Create the variable importance data table
              vimp_table <- data.table::data.table("score"=vimp_score, "name"=names(vimp_score))
              vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
              vimp_table[, "multi_var":=TRUE]
              
            } else {
              ..error_reached_unreachable_code(paste0("..vimp,familiarRFSRC: unknown vimp method was specified: ", vimp_method))
            }
            
            return(vimp_table)
          })



#####..set_calibration_info#####
setMethod("..set_calibration_info", signature(object="familiarRFSRC"),
          function(object, data){
            
            # Check if calibration info already.
            if(has_calibration_info(object)) return(object)
            
            if(object@outcome_type=="survival") {
              # Determine baseline survival.
              object@calibration_info <- get_baseline_survival(data=data)
              
            } else {
              return(callNextMethod())
            }
            
            return(object)
          })



#####..set_vimp_parameters#####
setMethod("..set_vimp_parameters", signature(object="familiarRFSRC"),
          function(object, method, ...){
            
            # Determine variable importance method
            if(method %in% c("random_forest_permutation", "random_forest_rfsrc_permutation")){
              vimp_method <- "permutation"
              
            } else if(method %in% c("random_forest_minimum_depth", "random_forest_rfsrc_minimum_depth")){
              vimp_method <- "minimum_depth"
              
            } else if(method %in% c("random_forest_variable_hunting", "random_forest_rfsrc_variable_hunting")) {
              vimp_method <- "variable_hunting"
              
            } else if(method %in% c( "random_forest_holdout", "random_forest_rfsrc_holdout")){
              vimp_method <- "holdout"
              
            } else {
              ..error_reached_unreachable_code("..set_vimp_parameters,familiarRFSRC: unknown feature selection method was specified.")
            }
            
            # Check if a list of hyperparameters is already present.
            if(is.null(object@hyperparameters)) {
              hyperparameters <- list()
              
            } else {
              hyperparameters <- object@hyperparameters
            }
            
            # Update the fs_vimp_method hyperparameters.
            hyperparameters$fs_vimp_method <- vimp_method

            # Store in the object
            object@hyperparameters <- hyperparameters
            
            return(object)
          })
