#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


##### promote_learner (hyperparameter learner) ---------------------------------
setMethod("promote_learner", signature(object="familiarHyperparameterLearner"),
          function(object){
            
            learner <- object@learner
            
            # if(learner %in% .get_available_naive_bayes_learners()){
            #   # Naive bayes model
            #   object <- methods::new("familiarNaiveBayes", object)
            #   
            # } else if(learner %in% .get_available_knn_learners()){
            #   # k-nearest neighbours model with radial kernel
            #   object <- methods::new("familiarKNN", object)
            #   
            # } else if(learner %in% .get_available_svm_c_learners()){
            #   # C-classification support vector machines.
            #   object <- methods::new("familiarSVMC", object)
            #   
            # } else if(learner %in% .get_available_svm_nu_learners()){
            #   # Nu-classification and regression support vector machines.
            #   object <- methods::new("familiarSVMNu", object)
            #   
            # } else if(learner %in% .get_available_svm_eps_learners()){
            #   # Epsilon regression support vector machines.
            #   object <- methods::new("familiarSVMEps", object)
            #   
            # } else if(learner %in% .get_available_glm_learners()){
            #   # Generalised linear models
            #   object <- methods::new("familiarGLM", object)
            #   
            # } else if(learner %in% .get_available_glmnet_ridge_learners()){
            #   # Ridge penalised regression models
            #   object <- methods::new("familiarGLMnetRidge", object)
            #   
            # } else if(learner %in% .get_available_glmnet_lasso_learners()){
            #   # Lasso penalised regression models
            #   object <- methods::new("familiarGLMnetLasso", object)
            #   
            # } else if(learner %in% .get_available_glmnet_elastic_net_learners()){
            #   # Elastic net penalised regression models.
            #   object <- methods::new("familiarGLMnetElasticNet", object)
            #   
            # } else if(learner %in% .get_available_mboost_lm_learners()){
            #   # Boosted generalised linear models
            #   object <- methods::new("familiarMBoostLM", object)
            #   
            # } else if(learner %in% .get_available_xgboost_lm_learners()){
            #   # Extreme gradient boosted linear models
            #   object <- methods::new("familiarXGBoostLM", object)
            #   
            # } else if(learner %in% .get_available_cox_learners()){
            #   # Cox proportional hazards regression model
            #   object <- methods::new("familiarCoxPH", object)
            #   
            # } else if(learner %in% .get_available_survival_regression_learners()){
            #   # Fully parametric survival regression model
            #   object <- methods::new("familiarSurvRegr", object)
            #   
            # } else if(learner %in% .get_available_rfsrc_learners()){
            #   # Random forests for survival, regression, and classification
            #   object <- methods::new("familiarRFSRC", object)
            #   
            # } else if(learner %in% .get_available_ranger_learners()){
            #   # Ranger random forests
            #   object <- methods::new("familiarRanger", object)
            #   
            # } else if(learner %in% .get_available_mboost_tree_learners()){
            #   # Boosted regression trees
            #   object <- methods::new("familiarMBoostTree", object)
            #   
            # } else if(learner %in% .get_available_xgboost_tree_learners()){
            #   # Extreme gradient boosted trees
            #   object <- methods::new("familiarXGBoostTree", object)
            #   
            # } else if(learner %in% .get_available_xgboost_dart_learners()){
            #   # Extreme gradient boosted trees
            #   object <- methods::new("familiarXGBoostDart", object)
            #   
            # } else if(learner %in% .get_available_glmnet_lasso_learners_test()){
            #   # Lasso penalised regression models for testing purposes.
            #   object <- methods::new("familiarGLMnetLassoTest", object)
            # }
            
            # Returned object can be a standard familiarModel
            return(object)
          })


##### .train (hyperparameter learner) ------------------------------------------
setMethod(".train", signature(object="familiarHyperparameterLearner", data="data.table"),
          function(object,
                   data,
                   parameter_data=NULL,
                   ...) {
            # Train method for training hyperparameter models
            
            # Check if the class of object is a subclass of
            # familiarHyperparameterLearner.
            if(!is_subclass(class(object)[1], "familiarHyperparameterLearner")) object <- promote_learner(object)
            
            # Check whether data contains the optimisation_score column.
            if(!"optimisation_score" %in% colnames(data)){
              data <- .compute_hyperparameter_optimisation_score(score_table=data,
                                                                 optimisation_function=object@optimisation_function)
            } 
            
            # Merge data and parameter_data on param_id.
            if(!is.null(parameter_data)){
              data <- merge(x=data,
                            y=parameter_data,
                            all.x=TRUE,
                            all.y=FALSE,
                            by="param_id")
            }
            
            # Get hyperparameter names from the target learner.
            default_hyperparameters <- .get_learner_hyperparameters(learner=object@target_learner,
                                                                    outcome_type=object@target_outcome_type,
                                                                    names_only=TRUE)
            
            # Set hyperparameters in data.
            target_hyperparameters <- intersect(colnames(data),default_hyperparameters)
            if(length(target_hyperparameters) == 0) stop("The provided hyperparameter data does not contain any of the hyperparameters expected by the learner.")
            object@target_hyperparameters <- target_hyperparameters
            
            if(has_bad_training_data(object=object, data=data)){
              # Create a random search object in case training data are bad in
              # some way or form.
              object <- methods::new("familiarHyperparameterRandomSearch", object)
            }
            
            # Train a new model based on data.
            object <- ..train(object=object,
                              data=data,
                              ...)
            
            return(object)
          })


##### .predict (hyperparameter learner) ----------------------------------------
setMethod(".predict", signature(object="familiarHyperparameterLearner", data="data.table"),
          function(object,
                   data,
                   type="default",
                   percentile=NULL,
                   ...) {
            # Predict method for hyperparameter models. The type argument can be
            # default (reports the model estimate), sd (reports the model
            # estimate + standard deviation), percentile, in which case the
            # model estimate and the requested percentile are returned, and raw,
            # which returns the data in a wide format. Not all learners support
            # the same types. Use the get_prediction_type method to figure out
            # which type of output can be generated.
            
            # Dispatch to the ..predict methods of the child classes. 
            prediction_table <- ..predict(object=object,
                                          data=data,
                                          type=type,
                                          percentile=percentile)
            
            return(prediction_table)
          })

##### show(hyperparameter learner) ---------------------------------------------
setMethod("show", signature(object="familiarHyperparameterLearner"),
          function(object){
            if(!model_is_trained(object)){
              cat(paste0("A ", object@learner, " model (class: ", class(object)[1],
                         ") for inferring hyperparameters of the ", object@target_learner, ". ",
                         "This hyperparameter model could not successfully be trained (v", object@familiar_version, ").\n"))
              
            } else {
              # Describe the learner and the version of familiar.
              message_str <- paste0("A ", object@learner, " model (class: ", class(object)[1],
                                    "; v", object@familiar_version, ") ",
                                    "for inferring hyperparameters of the ", object@target_learner, " learner. ")
              
              # Describe the package(s), if any
              if(!is.null(object@package)){
                message_str <- c(message_str,
                                 paste0("This model was trained using "),
                                 paste_s(mapply(..message_package_version, x=object@package, version=object@package_version)),
                                 ifelse(length(object@package) > 1, " packages", " package"))
              }
              
              # Complete message and write.
              message_str <- paste0(c(message_str, ".\n"), collapse="")
              cat(message_str)
              
              cat(paste0("\n--------------- Model details ---------------\n"))
              
              # Model details
              show(object@model)
              
              cat(paste0("---------------------------------------------\n"))
              
              # Details concerning hyperparameters.
              cat(paste0("\nThe model was trained to infer the optimisation score of the following hyperparameter set:\n",
                         paste_s(object@target_hyperparameters), "\n"))
              
              cat(paste0("\nOptimisation scores were determined using the ", object@optimisation_function, " method, ",
                         "based on assessment of model performance using the ", paste_s(object@optimisation_metric), " metrics.\n"))
              
              # Check package version.
              check_package_version(object)
            }
          })



##### require_package (hyperparameter learner) ---------------------------------
setMethod("require_package", signature(x="familiarHyperparameterLearner"),
          function(x, purpose=NULL, message_type="error", ...){
            
            # Skip if no package is required.
            if(is_empty(x@package)) return(invisible(TRUE))
            
            # Set standard purposes for common uses.
            if(!is.null(purpose)){
              if(purpose %in% c("train", "predict", "show")){
                purpose <- switch(purpose,
                                  "train"="to train a model for inferring hyperparameter scores",
                                  "predict"="to infer hyperparameter scores",
                                  "show"="to capture output of the hyperparameter model")
              }
            }
            
            return(invisible(.require_package(x=x@package, purpose=purpose, message_type=message_type)))
          })



##### set_package_version (hyperparameter learner) -----------------------------
setMethod("set_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            # Do not add package versions if there are no packages.
            if(is_empty(object@package)) return(object)
            
            # Obtain package versions.
            object@package_version <- sapply(object@package, function(x) (as.character(utils::packageVersion(x))))
            
            return(object)
          })



##### check_package_version (hyperparameter learner) ---------------------------
setMethod("check_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            .check_package_version(name=object@package,
                                   version=object@package_version,
                                   when="at model creation")
          })



##### model_is_trained (hyperparameter learner) --------------------------------
setMethod("model_is_trained", signature(object="familiarHyperparameterLearner"),
          function(object){
            # Check if a model was trained.
            if(is.null(object@model)){
              # If no model is attached to the object, assume that no model was
              # trained.
              return(FALSE)
              
            } else {
              # If the the model is not NULL, assume that it is present.
              return(TRUE)
            }
          })


##### add_package_version (hyperparameter learner) -----------------------------
setMethod("add_package_version", signature(object="familiarHyperparameterLearner"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })


##### ..train (hyperparameter learner, data) -----------------------------------
setMethod("..train", signature(object="familiarHyperparameterLearner", data="ANY"),
          function(object, data, ...){
            # This method is basically a capture-all for when model training
            # does not succeed. Normally ..train calls refer to child classes.
            # This method is then only called when something goes wrong there.
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })


##### ..predict (hyperparameter learner, data) ---------------------------------
setMethod("..predict", signature(object="familiarHyperparameterLearner", data="data.table"),
          function(object, data, type="default", ...){
            # Like ..train, this method is a capture-all for when predictions
            # fail.
            return(get_placeholder_prediction_table(object=object,
                                                    data=data,
                                                    type=type))
          })


#####has_bad_training_data######################################################
setMethod("has_bad_training_data", signature(object="familiarHyperparameterLearner", data="data.table"),
          function(object, data, ...){
            
            # One cannot train without data or on a single sample.
            if(is_empty(data)) return(TRUE)
            if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample")) < 2) return(TRUE)
            
            # Check if all data are non-singular.
            if(all(sapply(data[, mget(object@target_hyperparameters)], is_singular_data))) return(TRUE)
            
            return(FALSE)
          })

