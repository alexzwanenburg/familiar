#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarGLMnet",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY",
                    "feature_order"="character"),
         prototype=list("encoding_reference_table" = NULL,
                        "feature_order"=character()))

#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarGLMnet"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set the required package
            .Object@package <- "glmnet"
            
            return(.Object)
          })

setClass("familiarGLMnetRidge",
         contains="familiarGLMnet")

setClass("familiarGLMnetLasso",
         contains="familiarGLMnet")

setClass("familiarGLMnetElasticNet",
         contains="familiarGLMnet")

setClass("familiarGLMnetLassoTest",
         contains="familiarGLMnetLasso")

setClass("familiarGLMnetLassoTestAllFail",
         contains="familiarGLMnetLassoTest")

setClass("familiarGLMnetLassoTestSomeFail",
         contains="familiarGLMnetLassoTest")


.get_available_glmnet_ridge_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("ridge", "ridge_gaussian", "ridge_gaussian",
                "ridge_poisson", "ridge_binomial", "ridge_multinomial", "ridge_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("ridge"))
  }
  
  return(learners)
}


.get_available_glmnet_lasso_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("lasso", "lasso_gaussian", "lasso_poisson",
                "lasso_binomial", "lasso_multinomial", "lasso_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("lasso"))
  }
  
  return(learners)
}


.get_available_glmnet_elastic_net_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson",
                "elastic_net_binomial","elastic_net_multinomial", "elastic_net_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("elastic_net"))
  }
  
  return(learners)
}

.get_available_glmnet_ridge_vimp_methods <- function(show_general=TRUE){
  return(.get_available_glmnet_ridge_learners(show_general=show_general))
}

.get_available_glmnet_lasso_vimp_methods <- function(show_general=TRUE){
  return(.get_available_glmnet_lasso_learners(show_general=show_general))
}

.get_available_glmnet_elastic_net_vimp_methods <- function(show_general=TRUE){
  return(.get_available_glmnet_elastic_net_learners(show_general=show_general))
}

#####is_available#####
setMethod("is_available", signature(object="familiarGLMnet"),
          function(object, ...){
            
            # Extract learner and outcome_type.
            learner      <- object@learner
            outcome_type <- object@outcome_type
            
            if(outcome_type=="survival" & learner %in% c("elastic_net", "elastic_net_cox",
                                                         "lasso", "lasso_cox",
                                                         "ridge", "ridge_cox")){
              return(TRUE)
              
            } else if(outcome_type=="continuous" & learner %in% c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson",
                                                                  "lasso", "lasso_gaussian", "lasso_poisson",
                                                                  "ridge", "ridge_gaussian", "ridge_poisson")){
              return(TRUE)
              
            } else if(outcome_type=="multinomial" & learner %in% c("elastic_net", "elastic_net_multinomial",
                                                                   "lasso", "lasso_multinomial",
                                                                   "ridge", "ridge_multinomial")){
              return(TRUE)
              
            } else if(outcome_type=="binomial" & learner %in% c("elastic_net", "elastic_net_binomial",
                                                                "lasso", "lasso_binomial",
                                                                "ridge", "ridge_binomial")){
              return(TRUE)
              
            } else if(outcome_type=="count" & learner %in% c("elastic_net", "elastic_net_poisson",
                                                             "lasso", "lasso_poisson",
                                                             "ridge", "ridge_poisson")) {
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters,familiarGLMnet#####
setMethod("get_default_hyperparameters", signature(object="familiarGLMnet"),
          function(object, data=NULL, user_list=NULL, ...){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            param$family <- list()
            param$lambda_min <- list()
            param$n_folds <- list()
            param$normalise <- list()
            param$sample_weighting <- list()
            param$sample_weighting_beta <- list()
            
            if(is(object, "familiarGLMnetElasticNet")){
              param$alpha <- list()
            }
            
            # If dt is not provided, return the list with hyperparameter names
            # only.
            if(is.null(data)) return(param)
            
            # Internal
            outcome_type <- data@outcome_type
            
            # Determine the family.
            fam <- sub_all_patterns(x=object@learner, pattern=c("elastic_net", "lasso", "ridge"), replacement="", fixed=TRUE)
            if(fam != "") fam <- sub(x=fam, pattern="_", replacement="", fixed=TRUE)
            
            # Check for lasso_test
            if(object@learner %in% c("lasso_test_all_fail", "lasso_test_some_fail")) fam <- ""
            
            # Determine number of subjects
            n_samples <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="series"))
            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### Family #######################################################
            if(fam == ""){
              if(outcome_type == "continuous"){
                family_default <- c("gaussian", "poisson")
                
              } else if(outcome_type == "count"){
                family_default <- "poisson"
                
              } else if(outcome_type == "binomial"){
                family_default <- "binomial"
                
              } else if(outcome_type == "multinomial"){
                family_default <- "multinomial"
                
              } else if(outcome_type == "survival"){
                family_default <- "cox"
              }
            } else {
              family_default <- fam
            }
            
            # Set family parameter
            param$family <- .set_hyperparameter(default=family_default,
                                                type="factor",
                                                range=family_default,
                                                randomise=ifelse(length(family_default) > 1, TRUE, FALSE))
            
            
            ##### Lambda indicating the optimal model complexity ###############
            param$lambda_min <- .set_hyperparameter(default="lambda.min",
                                                    type="factor",
                                                    range=c("lambda.1se", "lambda.min"),
                                                    randomise=FALSE)
            
            
            ##### Number of cross-validation folds #############################
            
            # glmnet requires at least 3 folds. The default number of
            # cross-validation folds may grow up to 20, for data sets > 200
            # samples.
            n_folds_default <- min(c(20, max(c(3, floor(n_samples/10)))))
            
            # Set the number of cross-validation folds.
            param$n_folds <- .set_hyperparameter(default=n_folds_default,
                                                 type="integer",
                                                 range=c(3, n_samples),
                                                 valid_range=c(3, Inf),
                                                 randomise=FALSE)
            
            
            ##### Feature normalisation ########################################
            
            # By default, normalisation is part of the pre-processing of
            # familiar, but the user may have disabled it. In that the case, the
            # user can set normalisation to TRUE to avoid complaints by glmnet.
            param$normalise <- .set_hyperparameter(default=FALSE,
                                                   type="logical",
                                                   range=c(FALSE, TRUE),
                                                   randomise=FALSE)
            
            
            if(is(object, "familiarGLMnetElasticNet")){
              ##### Elastic net mixing parameter ###############################
              
              # Set alpha parameter. Alpha = 1 is lasso, alpha = 0 is ridge.
              # glmnet requires alpha to be in the closed interval [0, 1].
              param$alpha <- .set_hyperparameter(default= c(0, 1/3, 2/3, 1),
                                                 type="numeric",
                                                 range=c(0, 1),
                                                 valid_range=c(0, 1),
                                                 randomise=TRUE)
            }
            ##### Sample weighting method ######################################
            #Class imbalances may lead to learning majority classes. This can be
            #partially mitigated by increasing weight of minority classes.
            param$sample_weighting <- .get_default_sample_weighting_method(outcome_type=outcome_type)
            
            ##### Effective number of samples beta #############################
            #Specifies the beta parameter for effective number sample weighting
            #method. See Cui et al. (2019).
            param$sample_weighting_beta <- .get_default_sample_weighting_beta(method=c(param$sample_weighting$init_config,
                                                                                       user_list$sample_weighting),
                                                                              outcome_type=outcome_type)
            
            # Return hyperparameters
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarGLMnet"),
          function(object, type="default"){
            
            if(object@outcome_type != "survival" & object@learner %in% c("elastic_net", "elastic_net_cox", "lasso", "lasso_cox", "ridge", "ridge_cox")) return(callNextMethod())
            
            # Default are hazard ratios.
            if(type == "default"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarGLMnet: unknown type")
            }
          })



#####..train####
setMethod("..train", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data, force_signature=FALSE, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            original_name <- NULL
            
            # Check if training data is ok.
            if(reason <- has_bad_training_data(object=object, data=data)){
              return(callNextMethod(object=.why_bad_training_data(object=object, reason=reason)))
            } 
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)){
              return(callNextMethod(object=..update_errors(object=object,
                                                           ..error_message_no_optimised_hyperparameters_available())))
            }
            
            # For data with one feature, switch to familiarGLM.
            if(get_n_features(data) == 1){
              # Create a familiarGLM object.
              object <- methods::new("familiarGLM", object)
              
              return(..train(object=object,
                             data=data,
                             ...))
            }
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
            # Use effect coding to convert categorical data into encoded data -
            # this is required to deal with factors with missing/new levels
            # between training and test data sets.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=encoded_data$encoded_data)
            
            # Parse outcome data.
            if(object@outcome_type=="survival"){
              outcome_data <- survival::Surv(data@data$outcome_time,
                                             data@data$outcome_event)
              
            } else {
              outcome_data <- data@data$outcome
            }
            
            # Determine id columns
            id_columns <- get_id_columns("series")
            
            # Generate folds using our own fold generating algorithm to handle repeated measurements
            fold_table <- .create_cv(n_folds = object@hyperparameters$n_folds,
                                     outcome_type = object@outcome_type,
                                     data = encoded_data$encoded_data@data,
                                     stratify = FALSE,
                                     return_fold_id = TRUE)
            
            # Order according to samples in encoded_data$encoded_data@data so
            # that fold_id corresponds to the correct rows.
            fold_table <- merge(x=fold_table,
                                y=encoded_data$encoded_data@data[, mget(id_columns)],
                                by=id_columns)
            
            if(force_signature){
              
              # Find signature features.
              signature_feature <- names(object@feature_info)[sapply(object@feature_info, is_in_signature)]
              
              if(length(signature_feature) > 0 ){
                # Initially mark all features for shrinkage.
                penalty_factor <- rep(1, length(feature_columns))
                
                # Update all signature features that were not encoded.
                penalty_factor[feature_columns %in% signature_feature] <- 0
                
                # Update all signatures features that were encoded.
                encoded_signature <- encoded_data$reference_table[original_name %in% signature_feature]$reference_name
                penalty_factor[feature_columns %in% encoded_signature] <- 0
                
              } else {
                # Allow shrinking of each feature.
                penalty_factor <- rep(1, length(feature_columns))
              }
              
            } else {
              # Allow shrinking of each feature.
              penalty_factor <- rep(1, length(feature_columns))
            }
            
            # Set weights
            weights <- create_instance_weights(data=encoded_data$encoded_data,
                                               method=object@hyperparameters$sample_weighting,
                                               beta=..compute_effective_number_of_samples_beta(object@hyperparameters$sample_weighting_beta),
                                               normalisation="average_one")
            
            # Get the arguments which are shared between all different objects.
            learner_arguments <- list("x" = as.matrix(encoded_data$encoded_data@data[, mget(feature_columns)]),
                                      "y" = outcome_data,
                                      "family" = as.character(object@hyperparameters$family),
                                      "weights" = weights,
                                      "standardize" = object@hyperparameters$normalise,
                                      "nfolds" = NULL,
                                      "foldid" = fold_table$fold_id,
                                      "parallel" = FALSE,
                                      "penalty.factor" = penalty_factor)
            
            # Set learner-specific arguments.
            if(is(object, "familiarGLMnetRidge")){
              learner_arguments <- c(learner_arguments, list("alpha"=0.0))
            
            } else if(is(object, "familiarGLMnetLasso")){
              learner_arguments <- c(learner_arguments, list("alpha"=1.0))
              
            } else if(is(object, "familiarGLMnetElasticNet")){
              learner_arguments <- c(learner_arguments, list("alpha"=object@hyperparameters$alpha))
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarGLMnet: encountered unknown learner of unknown class: ", paste_s(class(object))))
            }
            
            # Train the model.
            model <- do.call_with_handlers(glmnet::cv.glmnet,
                                           args=learner_arguments)
            
            # Extract values.
            object <- ..update_warnings(object=object, model$warning)
            object <- ..update_errors(object=object, model$error)
            model <- model$value
            
            # Check if the model trained at all.
            if(!is.null(object@messages$error)) return(callNextMethod(object=object))
            
            # Add model
            object@model <- model
            
            # Add the contrast references to the object.
            object@encoding_reference_table <- encoded_data$reference_table
            
            # Add feature order
            object@feature_order <- feature_columns
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            if(type == "default"){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=encoded_data$encoded_data,
                                                                   type=type)
              
              if(object@outcome_type == "binomial"){
                #####Binomial outcomes##########################################
                
                # Use the model to predict class probabilities.
                model_predictions <- predict(object=object@model,
                                             newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                             s=as.character(object@hyperparameters$lambda_min),
                                             type="response")
                
                # Obtain class levels.
                class_levels <- get_outcome_class_levels(x=object)
                
                # Add class probabilities (glmnet always gives probability for the
                # second class).
                class_probability_columns <- get_class_probability_name(x=object)
                prediction_table[, (class_probability_columns[1]):= 1.0 - model_predictions]
                prediction_table[, (class_probability_columns[2]):= model_predictions]
                
                # Update predicted class based on provided probabilities.
                class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
                class_predictions <- factor(class_predictions, levels=class_levels)
                prediction_table[, "predicted_class":=class_predictions]
                
              } else if(object@outcome_type == "multinomial") {
                #####Multinomial outcomes#######################################
                
                # Use the model to predict class probabilities.
                model_predictions <- predict(object=object@model,
                                             newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                             s=as.character(object@hyperparameters$lambda_min),
                                             type="response")[, , 1]
                
                # Obtain class levels.
                class_levels <- get_outcome_class_levels(x=object)
                
                # Add class probabilities.
                class_probability_columns <- get_class_probability_name(x=object)
                for(ii in seq_along(class_probability_columns)){
                  
                  if(is.matrix(model_predictions)){
                    # Check if model_predictions is a matrix.
                    prediction_table[, (class_probability_columns[ii]):=model_predictions[, class_levels[ii]]]
                    
                  } else {
                    # Or not.
                    prediction_table[, (class_probability_columns[ii]):=model_predictions[class_levels[ii]]]
                  }
                }
                
                # Update predicted class based on provided probabilities.
                class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
                class_predictions <- factor(class_predictions, levels=class_levels)
                prediction_table[, "predicted_class":=class_predictions]
                
              } else if(object@outcome_type %in% c("survival", "continuous", "count")){
                #####Survival, count and continuous outcomes####################
                
                # Use the model for prediction.
                model_predictions <- predict(object=object@model,
                                             newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                             s=as.character(object@hyperparameters$lambda_min),
                                             type="response")
                
                # Add regression.
                prediction_table[, "predicted_outcome":=model_predictions]
                
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
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Use the model to predict class probabilities.
              return(predict(object=object@model,
                             newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                             s=as.character(object@hyperparameters$lambda_min),
                             type=type,
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarGLMnet"),
          function(object, data=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Attempt to train the model if it has not been trained yet.
            if(!model_is_trained(object)) object <- .train(object=object,
                                                           data=data,
                                                           get_additional_info=FALSE,
                                                           trim_model=FALSE,
                                                           force_signature=TRUE)
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the model is a familiarGLMnet object, and not
            # familiarGLM (which happens for one-feature datasets).
            if(!is(object, "familiarGLMnet")) return(..vimp(object=object, data=data))
            
            # Check that required packages are loaded and installed.
            require_package(object, "vimp")
            
            if(object@hyperparameters$family == "multinomial"){
              # Read coefficient lists
              coefficient_list <- coef(object@model,
                                       s=as.character(object@hyperparameters$lambda_min))
              
              # Parse into matrix and retrieve row names
              coefficient_matrix <- sapply(coefficient_list, as.matrix)
              rownames(coefficient_matrix) <- dimnames(coefficient_list[[1]])[[1]]
              
              # Compute variable importance score
              vimp_score <- apply(abs(coefficient_matrix), 1, max)
              
            } else {
              # Read coefficient matrix
              coefficient_matrix <- as.matrix(coef(object@model,
                                                   s=as.character(object@hyperparameters$lambda_min)))
              
              # Compute variable importance score
              vimp_score <- abs(coefficient_matrix)[, 1]
            }
            
            # Remove intercept from the variable importances.
            vimp_score <- vimp_score[names(vimp_score) != "(Intercept)"]
            if(length(vimp_score) == 0) return(callNextMethod())
            
            # Assign to variable importance table.
            vimp_table <- data.table::data.table("score"=vimp_score,
                                                 "name"=names(vimp_score))
            
            # Throw out elements with 0.0 coefficients
            vimp_table <- vimp_table[score != 0.0]
            
            # Check if any features remain.
            if(is_empty(vimp_table)) return(callNextMethod())
            
            # Create variable importance object.
            vimp_object <- methods::new("vimpTable",
                                        vimp_table=vimp_table,
                                        encoding_table=object@encoding_reference_table,
                                        score_aggregation="max",
                                        invert=TRUE)
            
            return(vimp_object)
          })



#####..set_calibration_info-----------------------------------------------------
setMethod("..set_calibration_info", signature(object="familiarGLMnet"),
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


#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarGLMnet"),
          function(object, ...){

            # Update model.
            object@model <- ..trim_glmnet(object@model)
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            # Default method for models that lack a more specific method.
            return(object)
          })



..trim_glmnet <- function(object){
  # Function to trim glmnet objects.
  
  # Check if the object is a glmnet object.
  if(!(inherits(object, "glmnet") | inherits(object, "cv.glmnet"))) return(object)
  
  # Replace calls
  object$call <- call("nullcall")
  
  # Specific to cv.glmnet.
  if(!is.null(object$glmnet.fit)){
    object$glmnet.fit$call <- call("nullcall")  
  }
  
  return(object)
}




.get_available_glmnet_lasso_learners_test_all_fail <- function(show_general=TRUE){
  return("lasso_test_all_fail")
}

.get_available_glmnet_lasso_learners_test_some_fail <- function(show_general=TRUE){
  return("lasso_test_some_fail")
}


#####is_available (test) #######################################################
setMethod("is_available", signature(object="familiarGLMnetLassoTest"),
          function(object, ...){
            return(TRUE)
          })



#####..predict (all fail) ------------------------------------------------------
setMethod("..predict", signature(object="familiarGLMnetLassoTestAllFail", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # Encode data so that the features are the same as in the training.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=encoded_data$encoded_data,
                                                                 type=type)
            
            return(prediction_table)
          })


#####..predict (some fail) -----------------------------------------------------
setMethod("..predict", signature(object="familiarGLMnetLassoTestSomeFail", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Get an empty prediction table.
            prediction_table <- callNextMethod()
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              
              # Set all class probabilities for the first row to infinite.
              class_probability_columns <- get_class_probability_name(x=object)
              for(ii in seq_along(class_probability_columns)){
                prediction_table[1L, (class_probability_columns[ii]):= Inf]
              }
              
              # Set the class to NA.
              prediction_table[1L, "predicted_class":=NA]
              
            } else if(object@outcome_type %in% c("survival", "continuous", "count")){
              
              # Add regression.
              prediction_table[1L, "predicted_outcome":=Inf]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })


#####get_prediction_type (test)#####
setMethod("get_prediction_type", signature(object="familiarGLMnetLassoTest"),
          function(object, type="default"){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # Default are hazard ratios.
            if(type == "default"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarGLMnetLassoTest: unknown type")
            }
          })



#####..predict_survival_probability (all fail) #####
setMethod("..predict_survival_probability", signature(object="familiarGLMnetLassoTestAllFail", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # Predict, just to obtain a correctly formatted table.
            survival_table <- learner.survival_probability_relative_risk(object=object, data=data, time=time)
            
            # Set predicted values to NA.
            survival_table[, "survival_probability":=NA_real_]
            
            return(survival_table)
          })



#####..predict_survival_probability (all fail) #####
setMethod("..predict_survival_probability", signature(object="familiarGLMnetLassoTestSomeFail", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # Predict, just to obtain a correctly formatted table.
            survival_table <- learner.survival_probability_relative_risk(object=object, data=data, time=time)
            
            # Set first predicted probability to infinite.
            survival_table[1L, "survival_probability":=Inf]
            
            return(survival_table)
          })
