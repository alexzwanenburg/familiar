#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarMBoost",
         contains="familiarModel")

setClass("familiarMBoostLM",
         contains="familiarMBoost",
         slots=list("encoding_reference_table" = "ANY",
                    "feature_order"="character"),
         prototype=list("encoding_reference_table" = NULL,
                        "feature_order"=character()))

setClass("familiarMBoostTree",
         contains="familiarMBoost",
         slots=list("encoding_reference_table" = "ANY",
                    "feature_order"="character"),
         prototype=list("encoding_reference_table" = NULL,
                        "feature_order"=character()))


.get_available_mboost_lm_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("boosted_glm", "boosted_glm_logistic",
                "boosted_glm_probit", "boosted_glm_loglog", "boosted_glm_cauchy", "boosted_glm_log",
                "boosted_glm_auc", "boosted_glm_gaussian", "boosted_glm_huber", "boosted_glm_laplace",
                "boosted_glm_poisson", "boosted_glm_cox", "boosted_glm_surv",
                "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex")
  
  if(!show_general){
    learners <- setdiff(learners, c("boosted_glm", "boosted_glm_surv"))
  }
  
  return(learners)
}


.get_available_mboost_tree_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
                "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log",
                "boosted_tree_auc", "boosted_tree_gaussian", "boosted_tree_huber",
                "boosted_tree_laplace", "boosted_tree_poisson", "boosted_tree_cox", "boosted_tree_surv",
                "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan", "boosted_tree_cindex")
  
  if(!show_general){
    learners <- setdiff(learners, c("boosted_tree", "boosted_tree_surv"))
  }
  
  return(learners)
}


#####is_available,familiarMBoostLM#####
setMethod("is_available", signature(object="familiarMBoostLM"),
          function(object, ...){
            
            # Extract learner and outcome_type from the familiarModel object.
            learner      <- object@learner
            outcome_type <- object@outcome_type
            
            if(outcome_type == "survival" & learner %in% c("boosted_glm", "boosted_glm_cox", "boosted_glm_surv",
                                                           "boosted_glm_loglog", "boosted_glm_weibull",
                                                           "boosted_glm_lognormal", "boosted_glm_gehan",
                                                           "boosted_glm_cindex")){
              return(TRUE)
              
            } else if(outcome_type == "continuous" & learner %in% c("boosted_glm", "boosted_glm_gaussian",
                                                                    "boosted_glm_huber", "boosted_glm_laplace",
                                                                    "boosted_glm_poisson")){
              return(TRUE)
              
            # } else if(outcome_type == "multinomial" & learner %in% c("boosted_glm", "boosted_glm_multinomial")){
              # return(TRUE)
              
            } else if(outcome_type == "binomial" & learner %in% c("boosted_glm", "boosted_glm_logistic",
                                                                  "boosted_glm_probit", "boosted_glm_loglog",
                                                                  "boosted_glm_cauchy", "boosted_glm_log",
                                                                  "boosted_glm_auc")){
              return(TRUE)
              
            } else if(outcome_type == "count" & learner %in% c("boosted_glm", "boosted_glm_poisson")) {
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####is_available,familiarMBoostTree#####
setMethod("is_available", signature(object="familiarMBoostTree"),
          function(object, ...){
            
            # Extract learner and outcome_type from the familiarModel object.
            learner      <- object@learner
            outcome_type <- object@outcome_type
            
            if(outcome_type == "survival" & learner %in% c("boosted_tree", "boosted_tree_cox",
                                                           "boosted_tree_surv","boosted_tree_loglog",
                                                           "boosted_tree_weibull", "boosted_tree_lognormal",
                                                           "boosted_tree_gehan", "boosted_tree_cindex")){
              return(TRUE)
              
            } else if(outcome_type == "continuous" & learner %in% c("boosted_tree", "boosted_tree_gaussian", "boosted_tree_huber",
                                                                    "boosted_tree_laplace", "boosted_tree_poisson")){
              return(TRUE)
              
            } else if(outcome_type == "binomial" & learner %in% c("boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
                                                                  "boosted_tree_loglog", "boosted_tree_cauchy",
                                                                  "boosted_tree_log", "boosted_tree_auc")){
              return(TRUE)
              
            } else if(outcome_type == "count" & learner %in% c("boosted_tree", "boosted_tree_poisson")) {
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarMBoost"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            param$family <- list()
            param$n_boost <- list()
            param$learning_rate <- list()
            
            if(is(object, "familiarMBoostTree")){
              param$tree_depth <- list()
              param$min_child_weight <- list()
              param$alpha <- list()
            }
            
            # If data is explicitly NULL, return the list with hyperparameter
            # names only.
            if(is.null(data)) return(param)
            
            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### Model family #####
            param$family$type  <- "factor"
            param$family$range <- c("logistic", "probit", "bin_loglog", "cauchy",
                                    "log", "auc", "gaussian", "huber", "laplace",
                                    "poisson", "cox", "weibull", "lognormal",
                                    "surv_loglog", "gehan", "cindex", "multinomial")
            
            # Read family string by parsing learner
            fam <- stringi::stri_replace_first_regex(str=object@learner, pattern="boosted_glm|boosted_tree", replace="")
            if(fam != "") fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="")
            
            # Define the family based on the name of the learner.
            if(fam == ""){
              # No specific family is provided.
              if(object@outcome_type == "continuous"){
                family_default <- c("gaussian", "huber", "poisson")
                
              } else if(object@outcome_type == "count"){
                family_default <- "poisson"
                
              } else if(object@outcome_type == "binomial") {
                family_default <- c("logistic", "probit", "bin_loglog", "cauchy", "log")
                
              # } else if(object@outcome_type == "multinomial"){
              #   family_default <- "multinomial"
              #   
              } else if(object@outcome_type == "survival"){
                family_default <- "cox"
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
            } else if(fam == "surv"){
              # A survival family is provided, but not specified further.
              family_default <- c("weibull", "lognormal", "surv_loglog")
              
            } else if(fam == "loglog") {
              # "loglog" is a collection of families that should be further
              # split according to outcome type.
              if(object@outcome_type == "binomial") {
                family_default <- "bin_loglog"
                
              } else if(object@outcome_type == "survival") {
                family_default <- "surv_loglog"
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
            } else {
              # Other families are uniquely defined.
              family_default <- fam
            }
            
            # Set the family parameter.
            param$family <- .set_hyperparameter(default=family_default, type="factor", range=family_default,
                                                randomise=ifelse(length(family_default) > 1, TRUE, FALSE))
            
            ##### Number of boosting iterations ################################
            
            # This parameter could be set using the cv or cvrisk functions in
            # mboost. However, the SMAC hyperoptimisation method implemented in
            # the framework is superior to that of the grid-search method of cv
            # and cvrisk This hyper-parameter is expressed on the log 10 scale
            param$n_boost <- .set_hyperparameter(default=c(0, 1, 2, 3), type="numeric", range=c(0, 3),
                                                 valid_range=c(0, Inf), randomise=TRUE)
            
            
            ##### Learning rate ################################################
            
            # Learning rate is on a log10 scale and determines how fast the
            # algorithm tries to learn. Lower values typically lead to better
            # models, but converge slower.
            param$learning_rate <- .set_hyperparameter(default=c(-5, -3, -2, -1), type="numeric", range=c(-7, 0),
                                                       valid_range=c(-Inf, 0), randomise=TRUE)
            
            if(is(object, "familiarMBoostTree")){
              ##### Tree maximum depth #########################################
              
              # This hyperparameter is only used by tree models. Larger depths
              # increase the risk of overfitting.
              param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7), type="integer", range=c(1, 10),
                                                      valid_range=c(1, Inf), randomise=TRUE)
              
              
              ##### Minimum sum of instance weight #############################
              
              # We implement this on a power(10) scale, with -1 offset.
              param$min_child_weight <- .set_hyperparameter(default=c(0, 1, 2), type="numeric", range=c(0, 2),
                                                            valid_range=c(0, Inf), randomise=TRUE)
              
              
              ##### Significance threshold for splitting #######################
              
              # Sets the significance level required to allow a split on a variable.
              param$alpha <- .set_hyperparameter(default=c(0.05, 0.1, 0.5, 1.0), type="numeric", range=c(10^-6, 1.0),
                                                 valid_range=c(0.0, 1.0), randomise=TRUE, distribution="log")
            }
            
            # Return hyper-parameters
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarMBoost"),
          function(object, type=NULL){
            

            if(object@outcome_type != "survival") return(callNextMethod())
            
            # The prediction type is a bit more complicated for mboost methods.
            if(is.null(type)){
              if(object@hyperparameters$family %in% c("cox", "cindex", "gehan")){
                return("hazard_ratio")
                
              } else if(object@hyperparameters$family %in% c("weibull", "lognormal", "surv_loglog")){
                return("expected_survival_time")
              }
            }
            
            if(type == "link" & object@hyperparameters$family %in% c("cox", "cindex", "gehan")){
              return("hazard_ratio")
              
            } else if(type == "response" & object@hyperparameters$family %in% c("weibull", "lognormal", "surv_loglog")) {
              return("expected_survival_time")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code(paste0("get_prediction_type,familiarGLM: unknown type (", type,
                                                      ") for the current family (", object@hyperparameters$family, ")."))
            }
          })



#####..train####
setMethod("..train", signature(object="familiarMBoost", data="dataObject"),
          function(object, data){
            
            # Aggregate repeated measurement data - ranger does not facilitate
            # repeated measurements.
            data <- aggregate_data(data=data)
            
            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Use effect coding to convert categorical data into encoded data -
            # this is required to deal with factors with missing/new levels
            # between training and test data sets.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=encoded_data$encoded_data)
            
            # Parse formula.
            if(object@outcome_type == "survival") {
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(survival::Surv(outcome_time, outcome_event)))
              
            } else if(object@outcome_type %in% c("binomial", "count", "continuous")){
              formula <- stats::reformulate(termlabels=feature_columns,
                                            response=quote(outcome))
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Potentially update the outcome data
            encoded_data$encoded_data <- ..update_outcome(object=object,
                                                          data=encoded_data$encoded_data)
            
            # Get family for mboost, which determines how the response and
            # predictors are linked.
            family <- ..get_distribution_family(object)
            
            # Set control object. Note that learning rate is defined on the log
            # 10 scale.
            control_object <- mboost::boost_control(mstop = round(10^object@hyperparameters$n_boost),
                                                    nu = 10^object@hyperparameters$learning_rate)
            
            if(is(object, "familiarMBoostLM")){
              # Attempt to create model
              model <- tryCatch(mboost::glmboost(formula,
                                                 data=encoded_data$encoded_data@data,
                                                 family=family,
                                                 center=FALSE,
                                                 control=control_object),
                                error=identity)
              
            } else if(is(object, "familiarMBoostTree")){
              # Set tree controls. Note that every parameter except max depth is
              # kept at default for mboost.
              tree_control_object <- partykit::ctree_control(testtype = "Univariate",
                                                             maxdepth = object@hyperparameters$tree_depth,
                                                             minsplit = 10^object@hyperparameters$min_child_weight - 1,
                                                             mincriterion = 1 - object@hyperparameters$alpha,
                                                             saveinfo = FALSE)
              
              # Attempt to create model
              model <- tryCatch(mboost::blackboost(formula,
                                                   data=encoded_data$encoded_data@data,
                                                   family=family,
                                                   control=control_object,
                                                   tree_controls=tree_control_object),
                                error=identity)
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarMBoost: encountered unknown learner of unknown class: ", paste0(class(object), collapse=", ")))
            }
            
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
setMethod("..predict", signature(object="familiarMBoost", data="dataObject"),
          function(object, data, type=NULL, ...){
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Set type
            if(is.null(type)){
              prediction_type <- "response"
              
              if(object@hyperparameters$family %in% c("auc", "cox", "cindex", "gehan")){
                prediction_type <- "link"
              }
              
            } else {
              prediction_type <- type
            }
            
            # Encode data so that the features are the same as in the training.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=encoded_data$encoded_data)
            
            # Make predictions.
            if(is(object, "familiarMBoostLM")){
              model_predictions <- mboost::predict.glmboost(object=object@model,
                                                            newdata=encoded_data$encoded_data@data,
                                                            type=prediction_type)
              
            } else if(is(object, "familiarMBoostTree")){
              model_predictions <- mboost::predict.mboost(object=object@model,
                                                          newdata=encoded_data$encoded_data@data,
                                                          type=prediction_type)
              
            } else {
              return(callNextMethod())
            }
            
            
            if(object@outcome_type == "binomial"){
              #####Binomial outcomes######
              
              if(object@hyperparameters$family %in% "auc"){
                # AUC produces the linear predictor, not class probabilities.
                # These are set here, prior to re-calibration.
                model_predictions <- 0.5 + model_predictions
              }
              
              # Obtain class levels.
              class_levels <- get_outcome_class_levels(x=object)
              
              # Add class probabilities (glm always gives probability for the
              # second class).
              class_probability_columns <- get_class_probability_name(x=object)
              prediction_table[, (class_probability_columns[1]):= 1.0 - model_predictions]
              prediction_table[, (class_probability_columns[2]):= model_predictions]
              
              # Update predicted class based on provided probabilities.
              class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
              class_predictions <- factor(class_predictions, levels=class_levels)
              prediction_table[, "predicted_class":=class_predictions]
              
            } else if(object@outcome_type %in% c("continuous", "count")){
              #####Numerical outcomes######
              
              # Extract predicted regression values.
              prediction_table[, "predicted_outcome":=model_predictions[, 1]]
              
            } else if(object@outcome_type %in% c("survival")){
              #####Survival outcomes######
              
              # Check model family and convert linear predictors to hazard
              # ratio.
              if(object@hyperparameters$family %in% "cox"){
                # Cox partial likelihood produces the linear predictor, not
                # relative risks.
                model_predictions <- exp(model_predictions)
                
              } else if(object@hyperparameters$family %in% c("cindex", "gehan")){
                # Concordance probability and gehan loss produce "time-like"
                # predictions before calibration using cox models, whereas
                # "risk-like" is expected.
                model_predictions <- - model_predictions
              }
              
              # Add predictions to the prediction table.
              prediction_table[, "predicted_outcome":=model_predictions[, 1]]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarMBoost", data="dataObject"),
          function(object, data, time){
            
            # Only predict survival probability for survival outcomes.
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            # Weibull, log-normal and log-log don't have an associated survival
            # probability function.
            if(object@hyperparameters$family %in% c("weibull", "lognormal", "surv_loglog")) return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarMBoostLM"),
          function(object, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- variable <- NULL
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Use varimp function from mboost to extract a data table.
            vimp_score <- data.table::as.data.table(mboost::varimp(object@model))
            
            # Select only existing features.
            vimp_score <- vimp_score[variable %in% object@feature_order, ]
            
            # Convert factor to character
            vimp_score$variable <- as.character(vimp_score$variable)
            
            # Parse score to data.table
            vimp_table <- data.table::data.table("score"=vimp_score$reduction,
                                                 "name"=vimp_score$variable)
            
            # Decode any categorical variables.
            vimp_table <- decode_categorical_variables_vimp(object=object,
                                                            vimp_table=vimp_table,
                                                            method="max")
            
            # Add rank and the multi_var variable.
            vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })



#####..set_calibration_info#####
setMethod("..set_calibration_info", signature(object="familiarMBoost"),
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



#####..get_distribution_family#####
setMethod("..get_distribution_family", signature(object="familiarMBoost"),
          function(object){
            # Obtain family from the hyperparameters.
            family <- object@hyperparameters$family
            
            # Check that the family hyperparameter exists.
            if(!is.character(family)){
              ..error_reached_unreachable_code("..get_distribution_family,familiarMBoost: family hyperparameter was not set.")
            }
            
            # Load families for boosted gradients
            if(family == "logistic"){
              family_fun <- mboost::Binomial(link="logit")
              
            } else if(family == "probit"){
              family_fun <- mboost::Binomial(link="probit")
              
            } else if(family == "bin_loglog"){
              family_fun <- mboost::Binomial(link="cloglog")
              
            } else if(family == "cauchy"){
              family_fun <- mboost::Binomial(link="cauchit")
              
            } else if(family == "log"){
              family_fun <- mboost::Binomial(link="log")
              
            } else if(family == "auc"){
              family_fun <- mboost::AUC()
            
            } else if(family == "gaussian"){
              family_fun <- mboost::Gaussian()
              
            } else if(family == "huber"){
              family_fun <- mboost::Huber()
              
            } else if(family == "laplace"){
              family_fun <- mboost::Laplace()
              
            } else if(family == "poisson"){
              family_fun <- mboost::Poisson()
              
            # } else if(family == "multinomial"){
            #   family_fun <- mboost::Multinomial()
            #   
            } else if(family == "cox"){
              family_fun <- mboost::CoxPH()
              
            } else if(family == "weibull"){
              family_fun <- mboost::Weibull()
              
            } else if(family == "lognormal"){
              family_fun <- mboost::Lognormal()
              
            } else if(family == "surv_loglog"){
              family_fun <- mboost::Loglog()
              
            } else if(family == "gehan"){
              family_fun <- mboost::Gehan()
              
            } else if(family == "cindex"){
              family_fun <- mboost::Cindex()
              
            } else {
              ..error_reached_unreachable_code(paste0("..get_distribution_family,familiarMBoost: unknown family.", family))
            }
            
            return(family_fun)
          })



#####..set_recalibration_model######
setMethod("..set_recalibration_model", signature(object="familiarMBoost", data="dataObject"),
          function(object, data, time=NULL){
            # Recalibration is performed using standard methods
            if(object@outcome_type %in% c("survival") & object@hyperparameters$family %in% c("gehan", "cindex")){
              
              # Calibrate the models.
              object@calibration_model <- learner.recalibrate_model(object=object, data=data, time=time)
              
              # Return object.
              return(object)
              
            } else if(object@outcome_type %in% c("binomial") & object@hyperparameters$family %in% c("auc")){
              
              # Calibrate the models.
              object@calibration_model <- learner.recalibrate_model(object=object, data=data)
              
              # Return object.
              return(object)
              
            } else {
              return(callNextMethod())
            }
          })


#####..update_outcome######
setMethod("..update_outcome", signature(object="familiarMBoost", data="dataObject"),
          function(object, data){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            outcome <- NULL
            
            if(is_empty(data)) return(data)
            
            if(object@outcome_type %in% c("count", "continuous") & object@hyperparameters$family %in% c("poisson")){
              # Make a copy to prevent updating by reference.
              data@data <- data.table::copy(data@data)
              
              data@data[, "outcome":=round(outcome)]
            }
            
            return(data)
          })
