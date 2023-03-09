#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarSVM",
         contains="familiarModel")

#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarSVM"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "e1071"
            
            return(.Object)
          })


setClass("familiarSVMC", contains="familiarSVM")
setClass("familiarSVMNu", contains="familiarSVM")
setClass("familiarSVMEps", contains="familiarSVM")


.get_available_svm_c_learners <- function(show_general=TRUE){
  return(c("svm_c", paste("svm_c", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_nu_learners <- function(show_general=TRUE){
  return(c("svm_nu", paste("svm_nu", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_eps_learners <- function(show_general=TRUE){
  return(c("svm_eps", paste("svm_eps", ..get_available_svm_kernels(), sep="_")))
}

..get_available_svm_kernels <- function() return(c("linear", "radial", "polynomial", "sigmoid"))


..find_kernel_type <- function(learner){

  # Find all available svm kernels.
  svm_kernels <- ..get_available_svm_kernels()
  
  # Find matches with end of learner string.
  kernel_matches <- sapply(svm_kernels, function(suffix, x) (endsWith(x=x, suffix=suffix)), x=learner)
  
  # If all are missing (e.g. "svm_eps), use default RBF kernel.
  if(all(!kernel_matches)) return("radial")
  
  # Else, return selected kernel.
  return(svm_kernels[kernel_matches])
}


#####is_available#####
setMethod("is_available", signature(object="familiarSVM"),
          function(object, ...){
            
            # Extract outcome type.
            outcome_type <- object@outcome_type
            
            if(outcome_type %in% c("continuous", "count") & (is(object, "familiarSVMNu") |
                                                             is(object, "familiarSVMEps"))){
              return(TRUE)
              
            } else if(outcome_type %in% c("binomial", "multinomial") & (is(object, "familiarSVMNu") |
                                                                        is(object, "familiarSVMC"))){
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarSVM"),
          function(object, data=NULL, ...){
            
            # Find kernel type.
            svm_kernel <- ..find_kernel_type(learner=object@learner)
            
            # Initialise list and declare hyperparameter entries Note that some
            # hyperparameters may not be required, dependent on the kernel and
            # type of svm.
            param <- list()
            param$sign_size <- list()
            param$kernel <- list()
            param$c <- list()
            
            # Type-specific parameters.
            if(is(object, "familiarSVMNu") | is(object, "familiarSVMEps")){
              param$epsilon <- list()
            }

            if(is(object, "familiarSVMNu")){
              param$nu <- list()
            }
            
            # Kernel-specific parameters.
            if(svm_kernel %in% c("radial")){
              param$gamma <- list()
              
            } else if(svm_kernel %in% c("polynomial")){
              param$degree <- param$gamma <- param$offset <- list()
              
            } else if(svm_kernel %in% c("sigmoid")){
              param$gamma <- param$offset <- list()
              
            }
            
            # If the data object is explicitly NULL, return the list with
            # hyperparameter names only.
            if(is.null(data)) return(param)

            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data=data)
            
            
            ##### SVM kernel ###################################################
            
            # A default kernel has been set earlier. Convert the svm_kernel to
            # the internal naming used by e1071::svm.
            
            # Set the svm kernel.
            param$kernel <- .set_hyperparameter(default=svm_kernel,
                                                type="factor",
                                                range=svm_kernel,
                                                randomise=FALSE)
            
            
            ##### constraints violation cost C #################################
            
            # This parameter defines the cost for constraint violations. It is
            # expressed on a log10 scale.
            param$c <- .set_hyperparameter(default=c(-3, -1, -0, 1, 3),
                                           type="numeric",
                                           range=c(-5, 3),
                                           valid_range=c(-Inf, Inf),
                                           randomise=TRUE)
            
            
            ##### Error tolerance epsilon ####################################
            if(is(object, "familiarSVMNu") | is(object, "familiarSVMEps")){
              
              # This parameter defines the error tolerance for regression SVM.
              # It is expressed on a log10 scale.
              param$epsilon <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1),
                                                   type="numeric",
                                                   range=c(-5, 1),
                                                   valid_range=c(-Inf, Inf),
                                                   randomise=TRUE)
            }
            
            
            ##### Error bounds parameter nu ##################################
            if(is(object, "familiarSVMNu")){
              
              # nu is expressed on a log10 scale.
              param$nu <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1),
                                              type="numeric",
                                              range=c(-5, 1),
                                              valid_range=c(-Inf, Inf),
                                              randomise=TRUE)
              
            }
            
            
            ##### Inverse kernel width gamma #################################
            if(svm_kernel %in% c("radial", "polynomial", "sigmoid")){
              
              # sigma is expressed on a log10 scale
              param$gamma <- .set_hyperparameter(default=c(-7, -5, -3, -1, 1),
                                                 type="numeric",
                                                 range=c(-9, 3),
                                                 valid_range=c(-Inf, Inf),
                                                 randomise=TRUE)
            }
            
            
            ##### Polynomial degree ##########################################
            if(svm_kernel %in% c("polynomial")){
              
              # polydot, besseldot and anovadot expect positive integer degrees.
              param$degree <- .set_hyperparameter(default=c(1, 2, 3, 4, 5),
                                                  type="integer",
                                                  range=c(1, 5),
                                                  valid_range=c(1, Inf),
                                                  randomise=TRUE)
            }
            
            
            ##### Kernel offset parameter ####################################
            if(svm_kernel %in% c("polynomial", "sigmoid")){
              
              # As feature data is rescaled internally by svm, we should not
              # expect offsets outside the [0, 1] range. Also, negative values
              # are not allowed for either kernel.
              param$offset <- .set_hyperparameter(default=c(0.0, 0.2, 0.5, 1.0),
                                                  type="numeric",
                                                  range=c(0, 1),
                                                  valid_range=c(0, Inf),
                                                  randomise=TRUE)
            }
            
            return(param)
          })


#####..train####
setMethod("..train", signature(object="familiarSVM", data="dataObject"),
          function(object, data, ...){
            
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
            
            # Parse the formula.
            formula <- stats::reformulate(termlabels=feature_columns,
                                          response=quote(outcome))

            # Derive fitting parameters for fitting class probabilities.
            fit_probability <- ifelse(object@outcome_type %in% c("binomial", "multinomial"), TRUE, FALSE)
            
            # Derive svm type from object
            if(is(object, "familiarSVMC")) svm_type <- "C-classification"
            if(is(object, "familiarSVMNu") & object@outcome_type %in% c("binomial", "multinomial")) svm_type <- "nu-classification"
            if(is(object, "familiarSVMNu") & object@outcome_type %in% c("count", "continuous")) svm_type <- "nu-regression"
            if(is(object, "familiarSVMEps")) svm_type <- "eps-regression"
            
            # Set svm-related parameters.
            svm_parameter_list <- list("kernel"=as.character(object@hyperparameters$kernel),
                                       "cost"=10^(object@hyperparameters$c))
            
            # Set nu-parameter (which not all svm types use).
            if(is(object, "familiarSVMNu")){
              svm_parameter_list$nu <- 10^(object@hyperparameters$nu)
            }
            
            # Set epsilon parameter (which not all svm types use).
            if(is(object, "familiarSVMNu") | is(object, "familiarSVMEps")){
              svm_parameter_list$epsilon <- 10^(object@hyperparameters$epsilon)
            }
            
            
            ##### Set kernel-specific parameters #########################################
            if(!is.null(object@hyperparameters$gamma)){
              svm_parameter_list$gamma <- 10^object@hyperparameters$gamma
            }
            
            if(!is.null(object@hyperparameters$degree)){
              svm_parameter_list$degree <- object@hyperparameters$degree
            }
           
            if(!is.null(object@hyperparameters$offset)){
              svm_parameter_list$coef0 <- object@hyperparameters$offset
            }
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              svm_parameter_list$class.weights <- "inverse"
            }
            
            # Train the model.
            model <- do.call_with_handlers(e1071::svm,
                                           args=c(list(formula,
                                                       "data"=data@data,
                                                       "type"=svm_type,
                                                       "probability"=fit_probability,
                                                       "fitted"=FALSE,
                                                       "cross"=0L),
                                                  svm_parameter_list))
            
            # Extract values.
            object <- ..update_warnings(object=object, model$warning)
            object <- ..update_errors(object=object, model$error)
            model <- model$value
            
            # Check if the model trained at all.
            if(!is.null(object@messages$error)) return(callNextMethod(object=object))
            if(is.null(model)) return(callNextMethod(object=..update_errors(object=object,
                                                                            "SVM model returned as NULL.")))
            
            # Add model
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#### ..train_naive -------------------------------------------------------------
setMethod("..train_naive", signature(object="familiarSVM", data="dataObject"),
          function(object, data, ...){
            
            if(object@outcome_type %in% c("count", "continuous", "binomial", "multinomial")){
              # Turn into a naive model.
              object <- methods::new("familiarNaiveModel", object)
            }
            
            return(..train(
              object=object,
              data=data,
              ...))
          })



#####..predict#####
setMethod("..predict", signature(object="familiarSVM", data="dataObject"),
          function(object, data, type="default", ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            if(type == "default"){
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
              model_predictions <- tryCatch(predict(object=object@model,
                                                    newdata=data@data,
                                                    probability=object@outcome_type %in% c("binomial", "multinomial")),
                                            error=identity)
              
              # Check if the model trained at all.
              if(inherits(model_predictions, "error")) return(callNextMethod())
              
              
              if(object@outcome_type %in% c("binomial", "multinomial")){
                #####Categorical outcomes#######################################
                
                # Isolate probabilities.
                model_predictions <- attr(model_predictions, "probabilities")
                
                # Obtain class levels from the object.
                class_levels <- get_outcome_class_levels(x=object)
                
                # Add class probabilities to the prediction table.
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
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                #####Numerical outcomes#########################################
                
                # Extract predicted regression values.
                prediction_table[, "predicted_outcome":=model_predictions]
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
              return(prediction_table)
              
            }
          })



#####..vimp---------------------------------------------------------------------
# SVM does not have an associated variable importance method.



#####.trim_model----------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarSVM"),
          function(object, ...){
            
            # Update model by removing the call.
            object@model$call <- call("trimmed")
            
            # Add show.
            object <- .capture_show(object)
            
            # Remove .Environment.
            object@model$terms <- .replace_environment(object@model$terms)
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            # Default method for models that lack a more specific method.
            return(object)
          })
