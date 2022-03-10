#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarGLM",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY"),
         prototype=list("encoding_reference_table" = NULL))

#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarGLM"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            if(.Object@outcome_type == "multinomial"){
              .Object@package <- "VGAM"
              
            } else if(.Object@outcome_type == "survival"){
              .Object@package <- "survival"
              
            } else {
              .Object@package <- "stats"
            }
            
            return(.Object)
          })


.get_available_glm_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("glm", "glm_logistic", "glm_probit", "glm_cauchy",
                "glm_log", "glm_loglog", "glm_multinomial", "glm_gaussian",
                "glm_log_gaussian", "glm_inv_gaussian", "glm_poisson",
                "glm_log_poisson")
  
  if(!show_general){
    learners <- setdiff(learners, c("glm", "glm_log"))
  }
  
  return(learners)
}


#####is_available#####
setMethod("is_available", signature(object="familiarGLM"),
          function(object, ...){
            
            # Extract outcome type and learner from the model object.
            outcome_type <- object@outcome_type
            learner <- object@learner
            
            # Check outcome type and learner.
            if(outcome_type == "binomial" & learner %in% c("glm", "glm_logistic", "glm_probit",
                                                           "glm_cauchy", "glm_loglog")){
              return(TRUE)
              
            } else if(outcome_type == "multinomial" & learner %in% c("glm", "glm_multinomial")) {
              return(TRUE)
              
            } else if(outcome_type == "continuous" & learner %in% c("glm", "glm_log", "glm_gaussian",
                                                                    "glm_log_gaussian", "glm_inv_gaussian",
                                                                    "glm_poisson", "glm_log_poisson")){
              return(TRUE)
              
            } else if(outcome_type == "survival" & learner %in% c("glm")){
              return(TRUE)
              
            } else if(outcome_type == "count" & learner %in% c("glm", "glm_poisson", "glm_log_poisson")) {
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarGLM"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries
            param <- list()
            param$sign_size <- list()
            param$family <- list()
            
            # If no data object is not provided, return the list with hyperparameter names only
            if(is.null(data)) return(param)
            
            # Get the outcome type
            outcome_type <- data@outcome_type
            
            ##### Signature size #########################################################
            param$sign_size <- .get_default_sign_size(data_obj=data, restrict_samples=TRUE)
            
            
            ##### Model family ###########################################################
            
            # Read family string by parsing the learner.
            fam <- stringi::stri_replace_first_regex(str=object@learner, pattern="glm", replace="")
            if(fam != "") fam <- stringi::stri_replace_first_regex(str=fam, pattern="_", replace="")
            
            # Determine the family or families.
            if(fam == ""){
              # If no family is specified, the default behaviour is to identify the family
              # through optimisation.
              if(outcome_type=="binomial") {
                family_default <- c("logistic", "probit", "loglog", "cauchy")
                
              } else if(outcome_type=="continuous"){
                family_default <- c("gaussian", "log_gaussian", "inv_gaussian", "poisson", "log_poisson")
                
              } else if(outcome_type=="count"){
                family_default <- c("poisson", "log_poisson")
                
              } else if(outcome_type=="multinomial") {
                family_default <- "multinomial"
                
              } else if(outcome_type == "survival") {
                family_default <- "cox"
                
              } else {
                ..error_outcome_type_not_implemented(outcome_type)
              }
              
            } else if(fam == "log") {
              # "log" is a collection of different families, that should be specified
              # according to the outcome type.
              if(outcome_type=="continuous") {
                family_default <- c("log_gaussian", "log_poisson")
                
              } else if(outcome_type=="count") {
                family_default <- "log_poisson"
              }
              
            } else {
              # A family was unambiguously specified.
              family_default <- fam
            }
            
            # Set family parameter.
            param$family <- .set_hyperparameter(default=family_default,
                                                type="factor",
                                                range=family_default,
                                                randomise=ifelse(length(family_default) > 1, TRUE, FALSE))
            
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarGLM"),
          function(object, type="default"){
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # This is a backup in case glm is used to refer to CoxPH methods.
            if(type == "default"){
              return("hazard_ratio")
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code("get_prediction_type,familiarGLM: unknown type")
            }
          })



#####..train####
setMethod("..train", signature(object="familiarGLM", data="dataObject"),
          function(object, data, ...){
            
            # For survival outcomes, switch to familiarCoxPH.
            if(object@outcome_type == "survival"){
              # Create a familiarCoxPH object.
              object <- methods::new("familiarCoxPH", object)
              
              return(..train(object=object,
                             data=data,
                             ...))
            }
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)) return(callNextMethod())
            
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
            
            # Parse formula
            formula <- stats::reformulate(termlabels=feature_columns, response=quote(outcome))
            
            # Get family for glm, which determines how the response and
            # predictors are linked.
            family <- ..get_distribution_family(object)
            
            if(object@outcome_type %in% c("binomial", "continuous", "count")){
              # Generate model
              model <- tryCatch(suppressWarnings(stats::glm(formula,
                                                            data=encoded_data$encoded_data@data,
                                                            family=family,
                                                            model=FALSE,
                                                            x=FALSE,
                                                            y=FALSE)),
                                error=identity)
              
            } else if(object@outcome_type=="multinomial"){
              # Generate model
              model <- tryCatch(suppressWarnings(VGAM::vglm(formula,
                                                            data=encoded_data$encoded_data@data,
                                                            family=family)),
                                error=identity)
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarGLM: unknown outcome type: ", object@outcome_type))
            }
              
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            # Check if all coefficients could not be estimated.
            if(all(!sapply(stats::coef(model), is.finite))) return(callNextMethod())
            
            # Add model
            object@model <- model
            
            # Add the contrast references to model_list
            object@encoding_reference_table <- encoded_data$reference_table
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarGLM", data="dataObject"),
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
                #####Binomial outcomes######
                
                # Use the model for prediction.
                model_predictions <- suppressWarnings(predict(object=object@model,
                                                              newdata=encoded_data$encoded_data@data,
                                                              type="response"))
                
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
                
              } else if(object@outcome_type == "multinomial") {
                #####Multinomial outcomes######
                
                # Use the model for prediction.
                model_predictions <- suppressWarnings(VGAM::predictvglm(object=object@model,
                                                                        newdata=encoded_data$encoded_data@data,
                                                                        type="response"))
                
                # Obtain class levels.
                class_levels <- get_outcome_class_levels(x=object)
                
                # Add class probabilities.
                class_probability_columns <- get_class_probability_name(x=object)
                for(ii in seq_along(class_probability_columns)){
                  prediction_table[, (class_probability_columns[ii]):=model_predictions[, ii]]
                }
                
                # Update predicted class based on provided probabilities.
                class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
                class_predictions <- factor(class_predictions, levels=class_levels)
                prediction_table[, "predicted_class":=class_predictions]
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                #####Count and continuous outcomes#####
                
                # Use the model for prediction.
                model_predictions <- suppressWarnings(predict(object=object@model,
                                                              newdata=encoded_data$encoded_data@data,
                                                              type="response"))
                
                # Add regression.
                prediction_table[, "predicted_outcome":=model_predictions]
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              if(object@outcome_type  %in% c("continuous", "count", "binomial")){
                #####Binomial, count and continuous outcomes####################
                
                # Use the model for prediction.
                return(predict(object=object@model,
                               newdata=encoded_data$encoded_data@data,
                               type=type,
                               ...))
                
              } else if(object@outcome_type == "multinomial") {
                #####Multinomial outcomes#######################################
                
                # Use the model for prediction.
                return(suppressWarnings(VGAM::predictvglm(object=object@model,
                                                          newdata=encoded_data$encoded_data@data,
                                                          type=type,
                                                          ...)))
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
            }
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarGLM"),
          function(object, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "vimp")
            
            # Compute z-values
            coefficient_z_values <- .compute_z_statistic(object)
            
            if(is(object@model, "vglm")){
              
              # Parse coefficient names. vglm adds :1 and :2 (and so on) to
              # coefficient names.
              coefficient_names <- stringi::stri_split_fixed(names(coefficient_z_values), pattern=":")
              coefficient_names <- sapply(coefficient_names, function(coefficient_name) coefficient_name[1])
              names(coefficient_z_values) <- coefficient_names
            }
            
            # Remove intercept from the coefficients.
            coefficient_z_values <- coefficient_z_values[names(coefficient_z_values) != "(Intercept)"]
            if(length(coefficient_z_values) == 0) return(callNextMethod())
          
            # Assign to variable importance table.
            vimp_table <- data.table::data.table("score"=coefficient_z_values,
                                                 "name"=names(coefficient_z_values))
            
            # Merge by name (vglm coefficients can occur multiple times for the
            # same feature).
            vimp_table <- vimp_table[, list("score"=max(score)), by="name"]
            
            # Decode any categorical variables.
            vimp_table <- decode_categorical_variables_vimp(object=object,
                                                            vimp_table=vimp_table,
                                                            method="max")
            
            # Add ranks and set multi_var
            vimp_table[, "rank":=data.table::frank(-score, ties.method="min")]
            vimp_table[, "multi_var":=TRUE]
            
            return(vimp_table)
          })



#####..get_distribution_family#####
setMethod("..get_distribution_family", signature(object="familiarGLM"),
          function(object){
            # Obtain family from the hyperparameters.
            family <- object@hyperparameters$family
            
            # Check that required packages are loaded and installed.
            require_package(object, "distribution")
            
            # Check that the family hyperparameter exists.
            if(!is.character(family) & !is.factor(family)){
              ..error_reached_unreachable_code("..get_distribution_family,familiarGLM: family hyperparameter was not set.")
            }
            
            # Load families for linear regression
            if(family %in% c("logistic", "binomial")){
              family_fun <- stats::binomial(link="logit")
              
            } else if(family == "probit"){
              family_fun <- stats::binomial(link="probit")
              
            } else if(family == "cauchy"){
              family_fun <- stats::binomial(link="cauchit")
              
            } else if(family == "loglog"){
              family_fun <- stats::binomial(link="cloglog")
              
            } else if(family == "gaussian"){
              family_fun <- stats::gaussian(link="identity")
              
            } else if(family == "log_gaussian"){
              family_fun <- stats::gaussian(link="log")
              
            } else if(family == "inv_gaussian"){
              family_fun <- stats::gaussian(link="inverse")
              
            } else if(family == "poisson"){
              family_fun <- stats::poisson(link="identity")
              
            } else if(family == "log_poisson"){
              family_fun <- stats::poisson(link="log")
              
            } else if(family == "multinomial"){
              family_fun <- VGAM::multinomial()
              
            } else {
              ..error_reached_unreachable_code(paste0("..get_distribution_family,familiarGLM: unknown family.", family))
            }
            
            return(family_fun)
          })


#####..set_vimp_parameters#####
setMethod("..set_vimp_parameters", signature(object="familiarGLM"),
          function(object, method, ...){
            
            # Read family string by parsing the learner.
            family_str <- stringi::stri_replace_first_regex(str=method, pattern="glm", replace="")
            if(family_str != "") family_str <- stringi::stri_replace_first_regex(str=family_str, pattern="_", replace="")
            
            # Determine the family or families.
            if(family_str == ""){
              # If no family is specified, the default behaviour is to identify the family
              # through optimisation.
              if(object@outcome_type=="binomial") {
                family_default <- c("logistic")
                
              } else if(object@outcome_type=="continuous"){
                family_default <- c("gaussian")
                
              } else if(object@outcome_type=="count"){
                family_default <- c("poisson")
                
              } else if(object@outcome_type=="multinomial") {
                family_default <- "multinomial"
                
              } else if(object@outcome_type == "survival") {
                family_default <- "cox"
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
            } else if(family_str == "log") {
              # "log" is a collection of different families, that should be specified
              # according to the outcome type.
              if(object@outcome_type=="continuous") {
                family_default <- c("log_gaussian")
                
              } else if(object@outcome_type=="count") {
                family_default <- "log_poisson"
              }
              
            } else {
              # A family was unambiguously specified.
              family_default <- family_str
            }
            
            # Update family hyperparameter.
            object@hyperparameters$family <- family_default
            
            return(object)
          })


#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarGLM"),
          function(object, ...){
            
            if(object@outcome_type == "multinomial"){
              # Update model by removing the call.
              object@model@call <- call("trimmed")
              
              # Add show.
              object <- .capture_show(object)
              
              # Remove .Environment.
              object@model@terms$terms <- .replace_environment(object@model@terms$terms)
              object@model@misc$formula <- .replace_environment(object@model@misc$formula)
              
              # Remove elements that contain sample-specific values.
              object@model@predictors <- matrix(0)
              object@model@effects <- numeric(0)
              object@model@qr$qr <- NULL
              object@model@fitted.values <- matrix(0)
              object@model@residuals <- matrix(0)
              object@model@weights <- matrix(0)
              object@model@x <- matrix(0)
              object@model@y <- matrix(0)
              
            } else {
              # Update model by removing the call.
              object@model$call <- call("trimmed")
              
              # Add show.
              object <- .capture_show(object)
              
              # Remove .Environment.
              object@model$terms <- .replace_environment(object@model$terms)
              object@model$formula <- .replace_environment(object@model$formula)
              
              # Remove elements that contain sample-specific values.
              object@model$fitted.values <- NULL
              object@model$data <- NULL
              object@model$linear.predictors <- NULL
              object@model$prior.weights <- NULL
              object@model$weights <- NULL
              object@model$qr$qr <- NULL
              object@model$residuals <- NULL
              object@model$effects <- NULL
            }
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            # Default method for models that lack a more specific method.
            return(object)
          })
