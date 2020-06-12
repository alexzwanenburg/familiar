#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarGLMnet",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY",
                    "feature_order"="character"),
         prototype=list("encoding_reference_table" = NULL,
                        "feature_order"=character()))


#####is_available#####
setMethod("is_available", signature(object="familiarGLMnet"),
          function(object, ...){
            
            # Extract learner and 
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



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarGLMnet"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            param$family <- list()
            param$alpha <- list()
            param$lambda_min <- list()
            param$n_folds <- list()
            param$normalise <- list()
            
            # If dt is not provided, return the list with hyperparameter names
            # only.
            if(is.null(data)) return(param)
            
            # Internal
            outcome_type <- data@outcome_type
            
            # Determine the base learner
            if(stringi::stri_startswith_fixed(str=object@learner, pattern="elastic_net")) {
              base_learner <- "elastic_net"
              
            } else if(stringi::stri_startswith_fixed(str=object@learner, pattern="lasso")) {
              base_learner <- "lasso"
              
            } else if(stringi::stri_startswith_fixed(str=object@learner, pattern="ridge")) {
              base_learner <- "ridge"
              
            } else {
              ..error_reached_unreachable_code(paste0("get_default_hyperparameters,familiarGLMnet: could not determine base_learner from learner: ", object@learner))
            }
            
            # Determine the family.
            fam <- stringi::stri_replace_first_regex(str=object@learner, pattern="elastic_net|lasso|ridge", replace="")
            if(fam != "") fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="")
            
            # Determine number of subjects
            n_samples <- data.table::uniqueN(data@data, by=c("subject_id", "cohort_id"))
            
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
            param$family <- .set_hyperparameter(default=family_default, type="factor", range=family_default,
                                                randomise=ifelse(length(family_default) > 1, TRUE, FALSE))
            
            
            ##### Elastic net mixing parameter #################################
            if(base_learner == "elastic_net"){
              alpha_default <- c(0, 1/3, 2/3, 1)
              
            } else if(base_learner == "lasso"){
              alpha_default <- 1
              
            } else if(base_learner == "ridge"){
              alpha_default <- 0
              
            } else {
              ..error_reached_unreachable_code(paste0("get_default_hyperparameters,familiarGLMnet: encountered unknown base_learner: ", base_learner))
            }
            
            # Set alpha parameter. Alpha = 1 is lasso, alpha = 0 is ridge.
            # glmnet requires alpha to be in the closed interval [0, 1].
            param$alpha <- .set_hyperparameter(default=alpha_default, type="numeric", range=c(0, 1), valid_range=c(0, 1),
                                               randomise=ifelse(length(alpha_default) > 1, TRUE, FALSE))
            
            
            ##### Lambda indicating the optimal model complexity ###############
            param$lambda_min <- .set_hyperparameter(default="lambda.min", type="factor",
                                                    range=c("lambda.1se", "lambda.min"), randomise=FALSE)
            
            
            ##### Number of cross-validation folds #############################
            
            # glmnet requires at least 3 folds. The default number of
            # cross-validation folds may grow up to 20, for data sets > 200
            # samples.
            n_folds_default <- min(c(20, max(c(3, floor(n_samples/10)))))
            
            # Set the number of cross-validation folds.
            param$n_folds <- .set_hyperparameter(default=n_folds_default, type="integer", range=c(3, n_samples),
                                                 valid_range=c(3, Inf), randomise=FALSE)
            
            
            ##### Feature normalisation ########################################
            
            # By default, normalisation is part of the pre-processing of
            # familiar, but the user may have disabled it. In that the case, the
            # user can set normalisation to TRUE to avoid complaints by glmnet.
            param$normalise <- .set_hyperparameter(default=FALSE, type="logical", range=c(FALSE, TRUE), randomise=FALSE)
            
            # Return hyperparameters
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarGLMnet"),
          function(object, type=NULL){
            
            if(object@outcome_type != "survival" & object@learner %in% c("elastic_net", "elastic_net_cox", "lasso", "lasso_cox", "ridge", "ridge_cox")) return(callNextMethod())
            
            # This is a backup in case glm is used to refer to CoxPH methods.
            if(is.null(type)) return("hazard_ratio")
            
            if(type == "risk"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarGLMnet: unknown type")
            }
          })



#####..train####
setMethod("..train", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data){
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # For data with one feature, switch to familiarGLM.
            if(get_n_features(data) == 1){
              # Create a familiarGLM object.
              object <- methods::new("familiarGLM", object)
              
              return(..train(object=object, data=data))
            }
            
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
            
            # Generate folds using our own fold generating algorithm to handle repeated measurements
            fold_table <- .create_cv(sample_identifiers = unique(encoded_data$encoded_data@data$subject_id),
                                     n_folds = object@hyperparameters$n_folds,
                                     outcome_type = object@outcome_type,
                                     data = encoded_data$encoded_data@data,
                                     stratify = FALSE,
                                     return_fold_id = TRUE)
            
            # Order according to subject_id in encoded_data$encoded_data@data so
            # that fold_id corresponds to the correct rows.
            fold_table <- merge(x=fold_table,
                                y=fold_table[, "subject_id"],
                                by="subject_id")
            
            # Attempt to train the model
            model <- tryCatch(cv.glmnet(x = as.matrix(encoded_data$encoded_data@data[, mget(feature_columns)]),
                                        y = outcome_data,
                                        family = object@hyperparameters$family,
                                        alpha = object@hyperparameters$alpha,
                                        standardize = object@hyperparameters$normalise,
                                        nfolds = NULL,
                                        foldid = fold_table$fold_id,
                                        parallel = FALSE),
                              error=identity)
            
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            # Add model
            object@model <- model
            
            # Add the contrast references to model_list
            object@encoding_reference_table <- encoded_data$reference_table
            
            # Add feature order
            object@feature_order <- feature_columns
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data, type="response", ...){
            
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
                                                                 data=encoded_data$encoded_data)
            
            if(object@outcome_type == "binomial"){
              #####Binomial outcomes######
              
              # Use the model to predict class probabilities.
              model_predictions <- predict(object=object@model,
                                           newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                           s=object@hyperparameters$lambda_min,
                                           type=type)
              
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
              #####Multinomial outcomes######
              
              # Use the model to predict class probabilities.
              model_predictions <- predict(object=object@model,
                                           newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                           s=object@hyperparameters$lambda_min,
                                           type=type)[, , 1]
              
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
              #####Count and continuous outcomes#####
              
              # Use the model for prediction.
              model_predictions <- predict(object=object@model,
                                           newx=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                           s=object@hyperparameters$lambda_min,
                                           type=type)
              
              # Add regression.
              prediction_table[, "predicted_outcome":=model_predictions]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarGLMnet", data="dataObject"),
          function(object, data, time){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarGLMnet"),
          function(object, data=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Attempt to train the model if it has not been trained yet.
            if(!model_is_trained(object)) object <- ..train(object, data)
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            if(object@hyperparameters$family == "multinomial"){
              # Read coefficient lists
              coefficient_list <- coef(object@model,
                                       s=object@hyperparameters$lambda_min)
              
              # Parse into matrix and retrieve row names
              coefficient_matrix <- sapply(coefficient_list, as.matrix)
              rownames(coefficient_matrix) <- dimnames(coefficient_list[[1]])[[1]]
              
              # Compute variable importance score
              vimp_score <- apply(abs(coefficient_matrix), 1, max)
              
            } else {
              # Read coefficient matrix
              coefficient_matrix <- as.matrix(coef(object@model,
                                                   s=object@hyperparameters$lambda_min))
              
              # Compute variable importance score
              vimp_score <- abs(coefficient_matrix)[, 1]
            }
            
            # Remove intercept from the variable importances.
            vimp_score <- vimp_score[names(vimp_score) != "(Intercept)"]
            if(length(vimp_score) == 0) return(callNextMethod())
            
            # Assign to variable importance table.
            vimp_table <- data.table::data.table("score"=vimp_score,
                                                 "name"=names(vimp_score))
            
            # Decode any categorical variables.
            vimp_table <- decode_categorical_variables_vimp(object=object,
                                                            vimp_table=vimp_table,
                                                            method="max")
            
            # Throw out elements with 0.0 coefficients
            vimp_table <- vimp_table[score != 0.0]
            
            # Check if any features remain.
            if(is_empty(vimp_table)) return(callNextMethod())
            
            # Add ranks and set multi_var
            vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })



#####..set_calibration_info#####
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
