#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarXGBoost",
         contains="familiarModel",
         slots=list("encoding_reference_table" = "ANY",
                    "outcome_table" = "ANY",
                    "outcome_shift" = "numeric",
                    "outcome_scale" = "numeric",
                    "feature_order" = "character"),
         prototype=list("encoding_reference_table" = NULL,
                        "outcome_table" = NULL,
                        "outcome_shift" = 0.0,
                        "outcome_scale" = 1.0,
                        "feature_order"=character()))

setClass("familiarXGBoostLM",
         contains="familiarXGBoost")

setClass("familiarXGBoostTree",
         contains="familiarXGBoost")

setClass("familiarXGBoostDart",
         contains="familiarXGBoost")


#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarXGBoost"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "xgboost"
            
            return(.Object)
          })


.get_available_xgboost_lm_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("xgboost_lm", "xgboost_lm_logistic", "xgboost_lm_gaussian",
                "xgboost_lm_poisson", "xgboost_lm_gamma", "xgboost_lm_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("xgboost_lm"))
  }
  
  return(learners)
}


.get_available_xgboost_tree_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("xgboost_tree", "xgboost_tree_logistic", "xgboost_tree_gaussian",
                "xgboost_tree_poisson", "xgboost_tree_gamma", "xgboost_tree_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("xgboost_tree"))
  }
  
  return(learners)
}


.get_available_xgboost_dart_learners <- function(show_general=TRUE){
  
  # Learners
  learners <- c("xgboost_dart", "xgboost_dart_logistic", "xgboost_dart_gaussian",
                "xgboost_dart_poisson", "xgboost_dart_gamma", "xgboost_dart_cox")
  
  if(!show_general){
    learners <- setdiff(learners, c("xgboost_dart"))
  }
  
  return(learners)
}


#####is_available#####
setMethod("is_available", signature(object="familiarXGBoost"),
          function(object, ...){
            
            # Extract learner and outcome_type from the familiarModel object.
            outcome_type <- object@outcome_type
            
            # Strip booster type from the learner.
            learner <- sub_all_patterns(x=object@learner, pattern=c("xgboost_lm", "xgboost_tree", "xgboost_dart"), replacement="", fixed=TRUE)
            if(startsWith(learner, "_")) learner <- sub(x=learner, pattern="_", replacement = "", fixed=TRUE)

            if(outcome_type == "continuous" & learner %in% c("", "logistic", "gaussian", "gamma")){
              return(TRUE)
              
            } else if(outcome_type == "multinomial" & learner %in% c("", "logistic")){
              return(TRUE)
              
            } else if(outcome_type == "binomial" & learner %in% c("", "logistic")){
              return(TRUE)
              
            } else if(outcome_type == "survival" & learner %in% c("", "cox")){
              return(TRUE)
              
            } else if(outcome_type == "count" & learner %in% c("", "poisson", "gaussian")) {
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarXGBoost"),
          function(object, data=NULL, user_list=NULL, ...){
            
            # Initialise list and declare hyperparameter entries
            param <- list()
            param$sign_size <- list()
            param$learn_objective <- list()
            
            # General parameters
            param$n_boost <- list()
            param$learning_rate <- list()
            param$lambda <- list()
            param$alpha <- list()
            param$sample_weighting <- list()
            param$sample_weighting_beta <- list()
            
            # Tree specific parameters
            if(is(object, "familiarXGBoostTree") | is(object, "familiarXGBoostDart")){
              param$tree_depth <- list()
              param$sample_size <- list()
              param$min_child_weight <- list()
              param$gamma <- list()
            }
            
            # DART-specific parameters
            if(is(object, "familiarXGBoostDart")){
              param$sample_type <- list()
              param$rate_drop <- list()
            }
            
            # If data is explicitly NULL, return the list with hyperparameter
            # names only.
            if(is.null(data)) return(param)
            
            # Extract outcome type.
            outcome_type <- object@outcome_type
            
            # Determine number of samples.
            n_samples <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="series"))
            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### Model objective ##############################################
            
            # xgboost has several families, or rather objectives, that can be
            # used.
            
            # Read family string by parsing learner.
            fam <- sub_all_patterns(x=object@learner, pattern=c("xgboost_lm", "xgboost_tree", "xgboost_dart"), replacement="", fixed=TRUE)
            if(fam != "") fam <- sub(x=fam, pattern="_", replacement="", fixed=TRUE)
            
            # Define the objective based on the name of the learner.
            if(fam == ""){
              # No specific objective is provided.
              if(outcome_type == "continuous"){
                learn_objective_default <- c("gaussian", "continuous_logistic", "gamma")
                
              } else if(outcome_type=="count"){
                learn_objective_default <- c("gaussian", "poisson")
                
              } else if(outcome_type=="binomial"){
                learn_objective_default <- "binomial_logistic"
                
              } else if(outcome_type=="multinomial"){
                learn_objective_default <- "multinomial_logistic"
                
              } else if(outcome_type=="survival"){
                learn_objective_default <- "cox"
              }
              
            } else if(fam == "logistic") {
              # Logistic learner objectives form a collection, and are now uniquely
              # defined according to the type of outcome.
              if(outcome_type == "binomial"){
                learn_objective_default <- "binomial_logistic"
                
              } else if(outcome_type=="multinomial"){
                learn_objective_default <- "multinomial_logistic"
                
              } else if(outcome_type=="continuous"){
                learn_objective_default <- "continuous_logistic"
              }
              
            } else {
              # Other objectives are uniquely defined.
              learn_objective_default <- fam
            }
            
            
            # Set the learn_objective parameter
            param$learn_objective <- .set_hyperparameter(default=learn_objective_default,
                                                         type="factor",
                                                         range=learn_objective_default,
                                                         randomise=ifelse(length(learn_objective_default) > 1, TRUE, FALSE))
            
            ##### Number of boosting iterations ################################
            
            # This hyper-parameter is expressed on the log 10 scale. It is
            # called nrounds in xgboost.
            param$n_boost <- .set_hyperparameter(default=c(0, 1, 2, 3),
                                                 type="numeric",
                                                 range=c(0, 3),
                                                 valid_range=c(0, Inf),
                                                 randomise=TRUE)
            
            
            ##### Learning rate ################################################
            
            # Learning rate is on a log10 scale from -5 to 0 (0.00001 - 1), and
            # determines how fast the algorithm tries to learn. Lower values
            # typically lead to better models, but take longer to learn. This
            # parameter is called eta by xgboost.
            param$learning_rate <- .set_hyperparameter(default=c(-3, -2, -1),
                                                       type="numeric",
                                                       range=c(-5, 0),
                                                       valid_range=c(-Inf, 0),
                                                       randomise=TRUE)
            
            
            ##### L2 regularisation term #######################################
            
            # The L2 regularisation term lambda lies in the half-open range [0,
            # inf). This term is implemented as a power(10) with a 10^-6 offset.
            param$lambda <- .set_hyperparameter(default=c(-6,-3,-1,1,3),
                                                type="numeric",
                                                range=c(-6, 3),
                                                valid_range=c(-6, Inf),
                                                randomise=TRUE)
            
            
            ##### L1 regularisation term #######################################
            
            # The L1 regularisation term alpha is implemented as the L2
            # regularisation term.
            param$alpha <- .set_hyperparameter(default=c(-6,-3,-1,1,3),
                                               type="numeric",
                                               range=c(-6, 3),
                                               valid_range=c(-6, Inf),
                                               randomise=TRUE)
            
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
            
            # Parameters for tree-based gradient boosting
            if(is(object, "familiarXGBoostTree") | is(object, "familiarXGBoostDart")){
              
              ##### Maximum tree depth #########################################
              
              # This hyperparameter is only used by tree models. The parameter
              # is called max_depth by xgboost. Larger depths increase the risk
              # of overfitting.
              param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7),
                                                      type="integer",
                                                      range=c(1, 10),
                                                      valid_range=c(1, Inf),
                                                      randomise=TRUE)
              
              
              ##### Data subsampling fraction ##################################
              
              # Trees may be grown using only a subset of the data to limit
              # overfitting. The parameter is called subsample in xgboost.
              param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 1.00),
                                                       type="numeric",
                                                       range=c(2/n_samples, 1.0),
                                                       valid_range=c(0, 1),
                                                       randomise=TRUE)
              
              
              ##### Minimum sum of instance weight #############################
              
              # Minimum sum of instance weight (hessian) needed in a child. If
              # the tree partition step results in a leaf node with the sum of
              # instance weight less than min_child_weight, then the building
              # process will give up further partitioning. In linear regression
              # mode, this simply corresponds to minimum number of instances
              # needed to be in each node. The larger, the more conservative the
              # algorithm will be. (source:xgboost documentation)
              #
              # We implement this on a power(10) scale, with -1 offset.
              param$min_child_weight <- .set_hyperparameter(default=c(0, 1, 2),
                                                            type="numeric",
                                                            range=c(0, 2),
                                                            valid_range=c(0, Inf),
                                                            randomise=TRUE)
              
              
              ##### Minimum splitting error reduction ##########################
              
              # Minimum error reduction required for splitting. This
              # hyper-parameters is called gamma or min_split_loss in xgboost.
              # We implement it on the power(10) scale, with 10^-6 offset.
              #
              # For continuous and count-type outcomes, this parameter can be a
              # bit tricky due to a wide range in possible scales, and thus in
              # error values. This is resolved by normalising the outcome to the
              # [0, 1] range.
              param$gamma <- .set_hyperparameter(default=c(-6,-3,-1,1,3),
                                                 type="numeric",
                                                 range=c(-6, 3),
                                                 valid_range=c(-6, Inf),
                                                 randomise=TRUE)
            }
            
            # Parameters for dart tree-based gradient boosting
            if(is(object, "familiarXGBoostDart")){
              
              ###### Dart booster sample type ##################################
              
              # Select the sample algorithm used by Dart booster.
              param$sample_type <- .set_hyperparameter(default=c("uniform", "weighted"),
                                                       type="factor",
                                                       range=c("uniform", "weighted"),
                                                       randomise=TRUE)
              
              ##### Dart booster tree drop rate ################################
              
              # Fraction of previous trees to drop during dropout.
              param$rate_drop <- .set_hyperparameter(default=c(0, 0.1, 0.3),
                                                     type="numeric", 
                                                     range=c(0,1), 
                                                     randomise=TRUE)
            }
            
            # Return hyper-parameters
            return(param)
          })



#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarXGBoost"),
          function(object, type="default"){
            
            
            if(object@outcome_type != "survival") return(callNextMethod())
            
            # The prediction type is a bit more complicated for xgboost methods.
            if(type == "default"){
              if(as.character(object@hyperparameters$learn_objective %in% c("cox"))){
                return("hazard_ratio")
              }
              
            } else if(type == "survival_probability"){
              return("survival_probability")
              
            } else {
              ..error_reached_unreachable_code(paste0("get_prediction_type,familiarXGBoost: unknown type (", type,
                                                      ") for the current objective (", as.character(object@hyperparameters$learn_objective), ")."))
            }
          })



#####..train####
setMethod("..train", signature(object="familiarXGBoost", data="dataObject"),
          function(object, data, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            outcome <- NULL
            
            # Aggregate repeated measurement data - xgboost does not facilitate
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
            
            # Use effect coding to convert categorical data into encoded data -
            # this is required to deal with factors with missing/new levels
            # between training and test data sets.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=encoded_data$encoded_data)
            
            # Find outcome columns in data table.
            outcome_columns <- get_outcome_columns(x=object)
            
            # Build a xgb data matrix
            if(object@outcome_type %in% c("binomial", "multinomial")){
              # Convert categorical outcomes to numerical labels expected by
              # xgboost.
              class_levels  <- get_outcome_class_levels(x=object)
              class_num_labels <- as.numeric(seq_along(class_levels) - 1)
              class_conversion_table <- data.table::data.table("class_level"=factor(class_levels, levels=class_levels),
                                                               "num_label"=class_num_labels)
              
              # Convert categorical outcomes by adding numerical labels to the
              # outcome data.
              outcome_data  <- data.table::copy(encoded_data$encoded_data@data[, mget(outcome_columns)])
              for(ii in seq_along(class_levels)){
                outcome_data[outcome == class_conversion_table$class_level[ii],
                             "outcome_label":=class_conversion_table$num_label[ii]]
              }
              
              # Save conversion table to model_list
              object@outcome_table <- class_conversion_table
              
              # Set outcome_labels
              outcome_labels <- outcome_data$outcome_label
              
            } else if(object@outcome_type %in% c("continuous", "count")){
              
              # Set outcome_labels
              outcome_labels <- encoded_data$encoded_data@data[[outcome_columns[1]]]
              
              # # Determine normalisation parameters so that outcome can be
              # # normalised to [0, 1] range (for logistic regression).
              if(as.character(object@hyperparameters$learn_objective) == "continuous_logistic"){
                object@outcome_shift <- min(outcome_labels)
                object@outcome_scale <- max(outcome_labels) - min(outcome_labels)
                
                # Normalise outcome labels.
                outcome_labels <- (outcome_labels - object@outcome_shift) / object@outcome_scale
              }
              
            } else if(object@outcome_type == "survival"){
              
              # According to the xgboost documentation, right censored survival
              # time should be represented by negative values.
              outcome_labels <- encoded_data$encoded_data@data[[outcome_columns[1]]]
              
              # Identify right-censored entries
              right_censored <- encoded_data$encoded_data@data[[outcome_columns[2]]] == 0
              
              # Parse right-censored entries in outcome_labels to the correct
              # representation.
              outcome_labels[right_censored] <- outcome_labels[right_censored] * -1.0
            }
            
            # Set weights.
            weights <- create_instance_weights(data=encoded_data$encoded_data,
                                               method=object@hyperparameters$sample_weighting,
                                               beta=..compute_effective_number_of_samples_beta(object@hyperparameters$sample_weighting_beta),
                                               normalisation="average_one")
            
            # Create a data_matrix object
            data_matrix <- xgboost::xgb.DMatrix(data=as.matrix(encoded_data$encoded_data@data[, mget(feature_columns)]),
                                                label=outcome_labels)
            
            # Set the number of classes for the multi:softmax objective
            n_classes <- 1
            if(object@outcome_type == "multinomial"){
              n_classes <- length(class_levels)
            }

            # Identify the booster to use.
            if(is(object, "familiarXGBoostLM")){
              booster <- "gblinear"
              
            } else if(is(object, "familiarXGBoostTree")){
              booster <- "gbtree"
              
            } else if(is(object, "familiarXGBoostDart")){
              booster <- "dart"
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarXGBoost: could not set booster for object of unknown class: ",
                                                      paste0(class(object), collapse=", ")))
            }
            
            # Select shared arguments.
            learner_arguments <- list("params"=list("booster" = booster,
                                                    "nthread" = 1L,
                                                    "eta" = 10^object@hyperparameters$learning_rate,
                                                    "lambda" = 10^object@hyperparameters$lambda - 10^-6,
                                                    "alpha" = 10^object@hyperparameters$alpha - 10^-6,
                                                    "objective" = ..get_distribution_family(object),
                                                    "num_class" = n_classes),
                                      "data"=data_matrix,
                                      "weight"=weights,
                                      "nrounds"=round(10^object@hyperparameters$n_boost),
                                      "verbose"=0)
            
            if(is(object, "familiarXGBoostTree") | is(object, "familiarXGBoostDart")){
              learner_arguments$params <- c(learner_arguments$params,
                                            list("max_depth" = object@hyperparameters$tree_depth,
                                                 "subsample" = object@hyperparameters$sample_size,
                                                 "min_child_weight" = 10^object@hyperparameters$min_child_weight - 1.0,
                                                 "gamma" = 10^object@hyperparameters$gamma - 10^-6))
            }
              
            if(is(object, "familiarXGBoostDart")){
              learner_arguments$params <- c(learner_arguments$params,
                                            list("sample_type" = as.character(object@hyperparameters$sample_type),
                                                 "rate_drop" = object@hyperparameters$rate_drop))
            }
            
            # Train the model.
            model <- do.call_with_handlers(xgboost::xgboost,
                                           args=learner_arguments)
            
            # Extract values.
            object <- ..update_warnings(object=object, model$warning)
            object <- ..update_errors(object=object, model$error)
            model <- model$value
            
            # Check if the model trained at all.
            if(!is.null(object@messages$error)) return(callNextMethod(object=object))
            
            # Add model. Note that we use xgboost to save raw model data to
            # prevent issues with saveRDS. This serialises the model.
            object@model <- xgboost::xgb.save.raw(model)
            
            # Add the contrast references to model_list
            object@encoding_reference_table <- encoded_data$reference_table
            
            # Add feature order
            object@feature_order <- feature_columns
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarXGBoost", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            if(type == "default"){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Load model through unserialisation.
              object@model <- xgboost::xgb.load.raw(object@model)
              
              # Encode data so that the features are the same as in the training.
              encoded_data <- encode_categorical_variables(data=data,
                                                           object=object,
                                                           encoding_method="dummy",
                                                           drop_levels=FALSE)
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=encoded_data$encoded_data,
                                                                   type=type)
              
              # Make predictions. If the booster object is DART type, predict()
              # will perform dropouts, i.e. only some of the trees will be
              # evaluated. This will produce incorrect results if data is not the
              # training data. To obtain correct results on test sets, set
              # ntree_limit to a nonzero value, e.g. preds = bst.predict(dtest,
              # ntree_limit=num_round) [from the documentation].
              #
              # Also note that for cox regression, the predictions are
              # recalibrated based on the linear predictor / marginal prediction.
              if(utils::packageVersion("xgboost")  < "1.4.0"){
                model_predictions <- predict(object=object@model,
                                             newdata=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                             outputmargin=object@outcome_type == "survival",
                                             ntreelimit=round(10^object@hyperparameters$n_boost),
                                             reshape=TRUE)
                
              } else {
                # From version 1.4 onward, ntreelimit was deprecated, and
                # replaced by iterationrange.
                model_predictions <- predict(object=object@model,
                                             newdata=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                                             outputmargin=object@outcome_type == "survival",
                                             iterationrange=c(1, round(10^object@hyperparameters$n_boost)),
                                             reshape=TRUE)
              }
              
              if(object@outcome_type == "binomial"){
                #####Binomial outcomes######
                
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
                
              } else if(object@outcome_type == "multinomial"){
                #####Multinomial outcomes######
                
                # Obtain class levels.
                class_levels <- get_outcome_class_levels(x=object)
                
                # Add class probabilities.
                class_probability_columns <- get_class_probability_name(x=object)
                for(ii in seq_along(class_probability_columns)){
                  
                  if(is.matrix(model_predictions)){
                    # Check if model_predictions is a matrix.
                    prediction_table[, (class_probability_columns[ii]):=model_predictions[, ii]]
                    
                  } else {
                    # Or not.
                    prediction_table[, (class_probability_columns[ii]):=model_predictions[ii]]
                  }
                }
                
                # Update predicted class based on provided probabilities.
                class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
                class_predictions <- factor(class_predictions, levels=class_levels)
                prediction_table[, "predicted_class":=class_predictions]
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                #####Numerical outcomes######
                
                # Map predictions back to original scale.
                model_predictions <- model_predictions * object@outcome_scale + object@outcome_shift
                
                # Extract predicted regression values.
                prediction_table[, "predicted_outcome":=model_predictions]
                
              } else if(object@outcome_type %in% c("survival")){
                #####Survival outcomes######
                
                # Add predictions to the prediction table.
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
              
              # Note that xgboost:::predict.xgb.Booster does not have a type
              # argument.
              return(predict(object=object@model,
                             newdata=as.matrix(encoded_data$encoded_data@data[, mget(object@feature_order)]),
                             ...))
            }
          })



#####..predict_survival_probability#####
setMethod("..predict_survival_probability", signature(object="familiarXGBoost", data="dataObject"),
          function(object, data, time){
            
            # Only predict survival probability for survival outcomes.
            if(!object@outcome_type %in% c("survival")) return(callNextMethod())
            
            # We can only predict probability for Cox.
            if(!as.character(object@hyperparameters$learn_objective) %in% c("cox")) return(callNextMethod())
            
            # If time is unset, read the max time stored by the model.
            if(is.null(time)) time <- object@settings$time_max
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            return(learner.survival_probability_relative_risk(object=object, data=data, time=time))
          })



#####..vimp#####
setMethod("..vimp", signature(object="familiarXGBoost"),
          function(object, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            Weight <- score <- NULL
            
            # Check if the model has been trained upon retry.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "vimp")
            
            # Load model through unserialisation.
            object@model <- xgboost::xgb.load.raw(object@model)
            
            # Use xgboost::xgb.importance function from xgboost to extract a
            # data.table.
            xgboost_score <- tryCatch(xgboost::xgb.importance(model=object@model),
                                      error=identity)
            
            if(inherits(xgboost_score, "error")) return(callNextMethod())
            
            if(is(object, "familiarXGBoostTree") | is(object, "familiarXGBoostDart")){
              # Process to variable importance table.
              vimp_table <- data.table::data.table("score"=xgboost_score$Gain,
                                                   "name"=xgboost_score$Feature)
              
            } else if(is(object, "familiarXGBoostLM")){
              # Output are linear coefficients, which may be negative. We keep
              # the maximum weight of each feature.
              xgboost_score <- xgboost_score[, list(Weight=max(abs(Weight))), by="Feature"]
              
              # Parse score to data.table
              vimp_table <- data.table::data.table("score"=xgboost_score$Weight,
                                                   "name"=xgboost_score$Feature)
              
            } else {
              ..error_reached_unreachable_code(paste0("..vimp,familiarXGBoost: could not process vimp for object of unknown class: ",
                                                      paste0(class(object), collapse=", ")))
            }
            
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
setMethod("..set_calibration_info", signature(object="familiarXGBoost"),
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
setMethod("..get_distribution_family", signature(object="familiarXGBoost"),
          function(object){
            # Obtain family from the hyperparameters.
            objective <- object@hyperparameters$learn_objective
            
            # Check that the learn_objective hyperparameter exists.
            if(!is.character(objective) & !is.factor(objective)){
              ..error_reached_unreachable_code("..get_distribution_family,familiarXGBoost: learn_objective hyperparameter was not set.")
            }
            
            # Load objective for extreme gradient boosting.
            if(objective == "gaussian"){
              boost_objective <- "reg:squarederror"
              
            } else if(objective == "continuous_logistic"){
              boost_objective <- "reg:logistic"
              
            } else if(objective == "multinomial_logistic"){
              boost_objective <- "multi:softprob"
              
            } else if(objective == "binomial_logistic"){
              boost_objective <- "binary:logistic"
              
            } else if(objective == "poisson"){
              boost_objective <- "count:poisson"
              
            } else if(objective == "gamma"){
              boost_objective <- "reg:gamma"
              
            } else if(objective == "cox"){
              boost_objective <- "survival:cox"
              
            } else {
              ..error_reached_unreachable_code(paste0("..get_distribution_family,familiarXGBoost: unknown learn objective: ", objective))
            }
            
            return(boost_objective)
          })



#####..set_recalibration_model######
setMethod("..set_recalibration_model", signature(object="familiarXGBoost", data="dataObject"),
          function(object, data, time=NULL){
            # Recalibration is performed using standard methods
            if(object@outcome_type %in% c("survival")){

              # Calibrate the models.
              object@calibration_model <- learner.recalibrate_model(object=object, data=data, time=time)

              # Return object.
              return(object)

            } else {
              return(callNextMethod())
            }
          })


#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarXGBoost"),
          function(object, ...){
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            # Prevent trimming of raw, serialised model information.
            if(inherits(object@model, "raw")) return(object)
            
            # Update model by removing the call.
            object@model$call <- call("trimmed")
            
            # Add show.
            object <- .capture_show(object)

            # Default method for models that lack a more specific method.
            return(object)
          })
