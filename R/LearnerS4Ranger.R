#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarRanger",
         contains="familiarModel")

setClass("familiarRangerDefault",
         contains="familiarRanger")

#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarRanger"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "ranger"
            
            return(.Object)
          })


.get_available_ranger_learners <- function(show_general=TRUE){
  return("random_forest_ranger")
} 

.get_available_ranger_default_learners <- function(show_general=TRUE){
  return("random_forest_ranger_default")
} 

.get_available_ranger_vimp_methods <- function(show_general=TRUE){
  return(c("random_forest_ranger_holdout_permutation", "random_forest_ranger_permutation",
           "random_forest_ranger_impurity"))
}

.get_available_ranger_default_vimp_methods <- function(show_general=TRUE){
  return(paste0(.get_available_ranger_vimp_methods(show_general=show_general), "_default"))
}

#####is_available#####
setMethod("is_available", signature(object="familiarRanger"),
          function(object, ...){
            # Ranger exists for all outcome types and variable importance
            # methods, including impurity for survival.
            return(TRUE)
          })



##### get_default_hyperparameters (familiarRanger) -----------------------------
setMethod("get_default_hyperparameters", signature(object="familiarRanger"),
          function(object, data=NULL, user_list=NULL, ...){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size   <- list()
            param$n_tree      <- list()
            param$sample_size <- list()
            param$m_try       <- list()
            param$node_size   <- list()
            param$tree_depth  <- list()
            param$split_rule  <- list()
            param$alpha       <- list()
            param$sample_weighting <- list()
            param$sample_weighting_beta <- list()
            
            # The following hyperparameters are only used for feature selection.
            param$fs_forest_type <- list()
            param$fs_vimp_method <- list()
            
            # If dt is not provided, return the list with hyperparameter /names only.
            if(is.null(data)) return(param)
            
            # Get the number of samples
            n_samples <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="series"))
            
            ###### Signature size ##############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ###### Number of trees #############################################
            
            # Note that the number of trees is defined in powers of 2, based on
            # Oshiro, T. M., Perez, P. S., & Baranauskas, J. A. (2012, July).
            # How many trees in a random forest?. In MLDM (pp. 154-168).
            param$n_tree <- .set_hyperparameter(default=c(4, 8, 10),
                                                type="integer",
                                                range=c(4, 10),
                                                valid_range=c(0, Inf),
                                                randomise=TRUE)
            
            
            ###### Sample size #################################################
            
            # Note that the sample size is here noted as a fraction, which
            # corresponds to the usage in ranger.
            param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 1.00),
                                                     type="numeric",
                                                     range=c(2/n_samples, 1.0),
                                                     valid_range=c(0, 1),
                                                     randomise=TRUE)
            
            
            ##### Number of candidate features selected at node ################
            
            # Note that the number of features is here noted as a fraction, but
            # is used in ranger as an integer. Familiar ensures that always at
            # least 1 feature is available as a candidate.
            param$m_try <- .set_hyperparameter(default=c(0.1, 0.3, 0.5, 1.0),
                                               type="numeric",
                                               range=c(0.0, 1.0),
                                               randomise=TRUE)
            
            
            ##### Terminal node size ###########################################
            
            # Number of instances in the terminal node. Larger terminal node
            # sizes limit tree depth and overfitting.
            
            # Define the default range.
            node_size_range <- c(5, floor(n_samples / 3))
            
            # Define the default values.
            node_size_default <- c(5, 10, 20, 50)
            node_size_default <- node_size_default[node_size_default >= node_size_range[1] &
                                                     node_size_default <= node_size_range[2]]
            
            # Set the node_size parameter.
            param$node_size <- .set_hyperparameter(default=node_size_default,
                                                   type="integer",
                                                   range=node_size_range,
                                                   valid_range=c(1, Inf),
                                                   randomise=TRUE)
            
            
            ##### Maximum tree depth ###########################################
            
            # Determines the depth trees are allowed to grow to. Larger depths
            # increase the risk of overfitting.
            param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7),
                                                    type="integer",
                                                    range=c(1, 10),
                                                    valid_range=c(1, Inf),
                                                    randomise=TRUE)
            
            
            ##### Splitting rule ###############################################
            
            # Availability of splitting rules is dependent on the type of
            # outcome.
            if(object@outcome_type %in% c("binomial", "multinomial")){
              split_rule_range <- c("gini", "extratrees", "hellinger")
              split_rule_default <- "gini"
              
            } else if(object@outcome_type %in% c("continuous", "count")){
              split_rule_range <- c("variance", "extratrees", "maxstat", "beta")
              split_rule_default <- "maxstat"
              
            } else if(object@outcome_type == "survival" ){
              split_rule_range <- c("logrank", "extratrees", "C", "maxstat")
              split_rule_default <- "maxstat"
              
            } else {
              ..error_no_known_outcome_type(object@outcome_type)
            }
            
            # Set the split_rule parameter.
            param$split_rule <- .set_hyperparameter(default=split_rule_default,
                                                    type="factor",
                                                    range=split_rule_range,
                                                    randomise=FALSE)
            
            
            ##### Significance threshold for splitting #########################
            
            # Sets the significance level required to allow a split on a
            # variable. It is only used with maxstat.
            alpha_randomise <- split_rule_default == "maxstat"
            
            if(alpha_randomise){
              default_values <- c(0.05, 0.1, 0.5, 1.0)
              
            } else {
              default_values <- c(0.05)
            }
            
            # Set the alpha parameter
            param$alpha <- .set_hyperparameter(default=default_values,
                                               type="numeric",
                                               range=c(10^-6, 1.0),
                                               valid_range=c(0.0, 1.0),
                                               randomise=alpha_randomise,
                                               distribution="log")
            
            ##### Sample weighting method ######################################
            #Class imbalances may lead to learning majority classes. This can be
            #partially mitigated by increasing weight of minority classes.
            param$sample_weighting <- .get_default_sample_weighting_method(outcome_type=object@outcome_type)
            
            ##### Effective number of samples beta #############################
            #Specifies the beta parameter for effective number sample weighting
            #method. See Cui et al. (2019).
            param$sample_weighting_beta <- .get_default_sample_weighting_beta(method=c(param$sample_weighting$init_config,
                                                                                       user_list$sample_weighting),
                                                                              outcome_type=object@outcome_type)
            
            
            ##### Feature selection forest type ################################
            
            # Enables the construction of holdout forests. A conventional forest
            # is grown by default.
            param$fs_forest_type <- .set_hyperparameter(default="standard",
                                                        type="factor",
                                                        range=c("standard", "holdout"),
                                                        randomise=FALSE)
            
            ##### Feature selection variable importance method #################
            
            # Enables the use of different variable importance methods. The
            # permutation method is used by default.
            param$fs_vimp_method <- .set_hyperparameter(default="permutation",
                                                        type="factor",
                                                        range=c("permutation", "impurity", "impurity_corrected"),
                                                        randomise=FALSE)
            
            return(param)
          })



##### get_default_hyperparameters (familiarRangerDefault) ----------------------
setMethod("get_default_hyperparameters", signature(object="familiarRangerDefault"),
          function(object, data=NULL, user_list=NULL, ...){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size   <- list()
            param$sample_weighting <- list()
            param$sample_weighting_beta <- list()
            
            # The following hyperparameters are only used for feature selection.
            param$fs_forest_type <- list()
            param$fs_vimp_method <- list()
            
            # If dt is not provided, return the list with hyperparameter /names only.
            if(is.null(data)) return(param)
            
            ###### Signature size ##############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### Sample weighting method ######################################
            # Class imbalances may lead to learning majority classes. This can
            # be partially mitigated by increasing weight of minority classes.
            param$sample_weighting <- .get_default_sample_weighting_method(outcome_type=object@outcome_type)
            
            
            ##### Effective number of samples beta #############################
            # Specifies the beta parameter for effective number sample weighting
            # method. See Cui et al. (2019).
            param$sample_weighting_beta <- .get_default_sample_weighting_beta(method=c(param$sample_weighting$init_config,
                                                                                       user_list$sample_weighting),
                                                                              outcome_type=object@outcome_type)
            
            
            ##### Feature selection forest type ################################
            
            # Enables the construction of holdout forests. A conventional forest
            # is grown by default.
            param$fs_forest_type <- .set_hyperparameter(default="standard",
                                                        type="factor",
                                                        range=c("standard", "holdout"),
                                                        randomise=FALSE)
            
            
            ##### Feature selection variable importance method #################
            
            # Enables the use of different variable importance methods. The
            # permutation method is used by default.
            param$fs_vimp_method <- .set_hyperparameter(default="permutation",
                                                        type="factor",
                                                        range=c("permutation", "impurity", "impurity_corrected"),
                                                        randomise=FALSE)
            
            return(param)
          })




#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarRanger"),
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
setMethod("..train", signature(object="familiarRanger", data="dataObject"),
          function(object, data, ...){

            # Aggregate repeated measurement data - ranger does not facilitate
            # repeated measurements.
            data <- aggregate_data(data=data)
            
            # Check if training data is ok.
            if(reason <- has_bad_training_data(object=object, data=data)){
              return(callNextMethod(object=.why_bad_training_data(object=object, reason=reason)))
            } 
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)){
              return(callNextMethod(object=..update_errors(object=object,
                                                           ..error_message_no_optimised_hyperparameters_available())))
            }
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
            # Find feature columns in data table
            feature_columns <- get_feature_columns(x=data)
            
            # Parse formula.
            if(object@outcome_type == "survival") {
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(survival::Surv(outcome_time, outcome_event)))
              
            } else if(object@outcome_type %in% c("binomial", "multinomial", "count", "continuous")){
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(outcome))
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Train probability trees for binomial and multinomial trees
            fit_probability <- ifelse(object@outcome_type %in% c("binomial", "multinomial"), TRUE, FALSE)
            
            # Extract hyperparameters from the model
            param <- object@hyperparameters
            
            # Set weights.
            weights <- create_instance_weights(data=data,
                                               method=object@hyperparameters$sample_weighting,
                                               beta=..compute_effective_number_of_samples_beta(object@hyperparameters$sample_weighting_beta),
                                               normalisation="average_one")
            
            # Get the arguments which are shared between holdout and standard
            # forests. 
            learner_arguments <- list(formula,
                                      "data" = data@data,
                                      "probability" = fit_probability,
                                      "num.threads" = 1L,
                                      "verbose" = FALSE)
            
            if(!is(object, "familiarRangerDefault")){
              # Non-default random forests have more arguments.
              learner_arguments <- c(learner_arguments,
                                     list("num.trees" = 2^param$n_tree,
                                          "mtry" = max(c(1, ceiling(param$m_try * length(feature_columns)))),
                                          "min.node.size" = param$node_size,
                                          "max.depth" = param$tree_depth,
                                          "alpha" = param$alpha,
                                          "splitrule" = as.character(param$split_rule)))
            }
            
            # Create random forest using ranger.
            if(param$fs_forest_type == "standard"){
              # Conventional random forest (used for model building and variable
              # importance estimations)
              
              if(!is(object, "familiarRangerDefault")){
                # Non-default random forests have more arguments.
                learner_arguments <- c(learner_arguments,
                                       list("sample.fraction" = param$sample_size))
              }
              
              model <- do.call_with_handlers(ranger::ranger,
                                             args=c(learner_arguments,
                                                    list("case.weights" = weights,
                                                         "importance" = as.character(param$fs_vimp_method))))
              
            } else if(param$fs_forest_type == "holdout") {
              # Hold-out random forest (used only for variable importance
              # estimations).
              model <- do.call_with_handlers(ranger::holdoutRF,
                                             args=learner_arguments)
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarRanger: encountered unknown forest type: ", param$fs_forest_type))
            }
            
            # Extract values.
            object <- ..update_warnings(object=object, model$warning)
            object <- ..update_errors(object=object, model$error)
            model <- model$value
            
            # Check if the model trained at all.
            if(!is.null(object@messages$error)) return(callNextMethod(object=object))
            
            # Add model
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarRanger", data="dataObject"),
          function(object, data, type="default", time=NULL, ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
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
              if(inherits(object@model, "ranger")){
                model_predictions <- suppressWarnings(predict(object=object@model,
                                                              data=data@data,
                                                              type="response",
                                                              num.threads=1,
                                                              verbose=FALSE))
                
              } else if(inherits(object@model, "holdoutRF")){
                model_predictions <- suppressWarnings(predict(object=object@model$rf1,
                                                              data=data@data,
                                                              type="response",
                                                              num.threads=1,
                                                              verbose=FALSE))
                
              } else {
                ..error_reached_unreachable_code(paste0("..predict,familiarRanger,dataObject: unknown model class detected ", class(object@model), ". Expected: ranger, holdoutRF"))
              }
              
              
              if(object@outcome_type %in% c("binomial", "multinomial")){
                #####Categorical outcomes######
                
                # Extract class levels from the predictions.
                class_levels <- colnames(model_predictions$predictions)
                
                # We have to determine the predicted class based on the class
                # probabilities. We do so by first determining the column with the
                # maximum probability. Subsequently we read the corresponding
                # class level, i.e. column name.
                class_predicted <- class_levels[apply(model_predictions$predictions, 1, which.max)]
                class_predicted <- factor(class_predicted, levels=get_outcome_class_levels(x=object))
                
                # Set predicted class.
                prediction_table[, "predicted_class":=class_predicted]
                
                # Add class probabilities.
                class_probability_columns <- get_class_probability_name(x=class_levels)
                for(ii in seq_along(class_probability_columns)){
                  prediction_table[, (class_probability_columns[ii]):=model_predictions$predictions[, ii]]
                }
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                #####Numerical outcomes######
                
                # Extract predicted regression values.
                prediction_table[, "predicted_outcome":=model_predictions$predictions]
                
              } else if(object@outcome_type %in% c("survival")){
                #####Survival outcomes######
                
                # Get the unique event times
                if(inherits(object@model, "ranger")){
                  event_times <- ranger::timepoints(object@model)
                  
                } else {
                  event_times <- ranger::timepoints(object@model$rf1)
                }
                
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
                  
                }
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
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
                             data=data@data,
                             type=type,
                             num.threads=1,
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarRanger", data="dataObject"),
          function(object, data, time){
            
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            return(..predict(object=object, data=data, time=time, type="survival_probability"))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarRanger"),
          function(object, data=NULL, ...){
            
            # Attempt to train the model if it has not been trained yet.
            if(!model_is_trained(object)) object <- .train(object=object,
                                                           data=data,
                                                           get_additional_info=FALSE,
                                                           trim_model=FALSE)
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "vimp")
            
            # Extract the variable importance score
            if(object@hyperparameters$fs_forest_type == "standard"){
              vimp_score <- ranger::importance(object@model)
              
            } else {
              vimp_score <- object@model$variable.importance
            }
            
            # Create variable importance object.
            vimp_object <- methods::new("vimpTable",
                                        vimp_table=data.table::data.table("score"=vimp_score, "name"=names(vimp_score)),
                                        score_aggregation="max",
                                        invert=TRUE)
            
            return(vimp_object)
          })



#####..set_calibration_info#####
setMethod("..set_calibration_info", signature(object="familiarRanger"),
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
setMethod("..set_vimp_parameters", signature(object="familiarRanger"),
          function(object, method, ...){
            
            # Determine variable importance method
            if(startswith_any(method, prefix=c("random_forest_ranger_holdout_permutation", "random_forest_ranger_permutation"))){
              vimp_method <- "permutation"
            } else if(startswith_any(method, prefix=c("random_forest_ranger_impurity"))){
              vimp_method <- "impurity_corrected"
            }
            
            # Determine forest type
            if(startswith_any(method, prefix=c("random_forest_ranger_holdout_permutation"))){
              forest_type <- "holdout"
            } else if(startswith_any(method, prefix=c("random_forest_ranger_permutation", "random_forest_ranger_impurity"))){
              forest_type <- "standard"
            }
            
            if(is.null(object@hyperparameters)) {
              hyperparameters <- list()
              
            } else {
              hyperparameters <- object@hyperparameters
            }
            
            # Update the fs_vimp_method and fs_forest_type hyperparameters
            hyperparameters$fs_vimp_method <- vimp_method
            hyperparameters$fs_forest_type <- forest_type
            
            # Store in the object
            object@hyperparameters <- hyperparameters
            
            return(object)
          })


##### .trim_model---------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarRanger"),
          function(object, ...){
            
            # Update model by removing the call.
            object@model$call <- call("trimmed")
            
            # Add show.
            object <- .capture_show(object)
            
            # Remove the predictions.
            object@model$predictions <- NULL
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            return(object)
          })
