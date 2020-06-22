#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarSVM",
         contains="familiarModel")


setClass("familiarSVMC", contains="familiarSVM")
setClass("familiarSVMCBound", contains="familiarSVM")
setClass("familiarSVMNu", contains="familiarSVM")
setClass("familiarSVMEps", contains="familiarSVM")
setClass("familiarSVMEpsBound", contains="familiarSVM")


.get_available_svm_c_learners <- function(show_general=TRUE){
  return(c("svm_c", paste("svm_c", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_c_bound_learners <- function(show_general=TRUE){
  return(c("svm_c_bound", paste("svm_c_bound", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_nu_learners <- function(show_general=TRUE){
  return(c("svm_nu", paste("svm_nu", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_eps_learners <- function(show_general=TRUE){
  return(c("svm_eps", paste("svm_eps", ..get_available_svm_kernels(), sep="_")))
}

.get_available_svm_eps_bound_learners <- function(show_general=TRUE){
  return(c("svm_eps_bound", paste("svm_eps_bound", ..get_available_svm_kernels(), sep="_")))
}

..get_available_svm_kernels <- function() return(c("vanilla", "linear", "rbf", "radial", "poly",
                                                   "polynomial", "tanh", "sigmoid", "bessel",
                                                   "laplace", "anova"))

..find_kernel_type <- function(learner){
  
  # Find all available svm kernels.
  svm_kernels <- ..get_available_svm_kernels()
  
  # Find matches with end of learner string.
  kernel_matches <- stringi::stri_endswith_fixed(learner, pattern=svm_kernels)
  
  # If all are missing (e.g. "svm_eps), use default RBF kernel.
  if(all(!kernel_matches)) return("rbf")
  
  # Else, return selected kernel.
  return(svm_kernels[kernel_matches])
}


#####is_available#####
setMethod("is_available", signature(object="familiarSVM"),
          function(object, ...){
            
            # Extract outcome type.
            outcome_type <- object@outcome_type
            
            if(outcome_type %in% c("continuous", "count") & (is(object, "familiarSVMNu") |
                                                             is(object, "familiarSVMEps") |
                                                             is(object, "familiarSVMEpsBound"))){
              return(TRUE)
              
            } else if(outcome_type %in% c("binomial", "multinomial") & (is(object, "familiarSVMNu") |
                                                                        is(object, "familiarSVMC") |
                                                                        is(object, "familiarSVMCBound"))){
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarSVM"),
          function(object, data=NULL){
            
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
            if(is(object, "familiarSVMNu") |
               is(object, "familiarSVMEps") |
               is(object, "familiarSVMEpsBound")){
              param$epsilon <- list()
            }

            if(is(object, "familiarSVMNu")){
              param$nu <- list()
            }
            
            # Kernel-specific parameters.
            if(svm_kernel %in% c("rbf", "radial")){
              param$sigma <- list()
              
            } else if(svm_kernel %in% c("poly", "polynomial")){
              param$degree <- param$scale <- param$offset <- list()
              
            } else if(svm_kernel %in% c("tanh", "sigmoid")){
              param$scale <- param$offset <- list()
              
            } else if(svm_kernel %in% c("bessel")){
              param$sigma <- param$order <- param$degree <- list()
              
            } else if(svm_kernel %in% c("laplace")){
              param$sigma <- list()
              
            } else if(svm_kernel %in% c("anova")){
              param$sigma <- param$degree <- list()
            }
            
            # If the data object is explicitly NULL, return the list with
            # hyperparameter names only.
            if(is.null(data)) return(param)

            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### SVM kernel ###################################################
            
            # A default kernel has been set earlier. Convert the svm_kernel to
            # the internal naming used by kernlab.
            if(svm_kernel %in% c("vanilla", "linear")){
              svm_kernel <- "vanilladot"
              
            } else if(svm_kernel %in% c("rbf", "radial")){
              svm_kernel <- "rbfdot"
              
            } else if(svm_kernel %in% c("poly", "polynomial")){
              svm_kernel <- "polydot"
              
            } else if(svm_kernel %in% c("tanh", "sigmoid")){
              svm_kernel <- "tanhdot"
              
            } else if(svm_kernel %in% c("bessel")){
              svm_kernel <- "besseldot"
              
            } else if(svm_kernel %in% c("laplace")){
              svm_kernel <- "laplacedot"
              
            } else if(svm_kernel %in% c("anova")){
              svm_kernel <- "anovadot"
              
            } else {
              ..error_reached_unreachable_code(paste0("get_default_hyperparameters,familiarSVM: unknown svm kernel encountered: ", svm_kernel))
            }
            
            # Set the svm kernel.
            param$kernel <- .set_hyperparameter(default=svm_kernel, type="factor", range=svm_kernel,
                                                randomise=FALSE)
            
            
            ##### constraints violation cost C #################################
            
            # This parameter defines the cost for constraint violations. It is
            # expressed on a log10 scale.
            param$c <- .set_hyperparameter(default=c(-3, -1, -0, 1, 3), type="numeric", range=c(-5, 3),
                                           valid_range=c(-Inf, Inf), randomise=TRUE)
            
            
            if(is(object, "familiarSVMNu") | is(object, "familiarSVMEps") | is(object, "familiarSVMEpsBound")){
              ##### Error tolerance epsilon ####################################
              
              # This parameter defines the error tolerance for regression SVM.
              # It is expressed on a log10 scale.
              param$epsilon <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1), type="numeric", range=c(-5, 1),
                                                   valid_range=c(-Inf, Inf), randomise=TRUE)
            }
            
            
            if(is(object, "familiarSVMNu")){
              ##### Error bounds parameter nu ##################################
              
              # nu is expressed on a log10 scale.
              param$nu <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1), type="numeric", range=c(-5, 1),
                                              valid_range=c(-Inf, Inf), randomise=TRUE)
              
            }
            
            
            if(svm_kernel %in% c("rbfdot", "besseldot", "laplacedot", "anovadot")){
              ##### Inverse kernel width sigma #################################
              
              # sigma is expressed on a log10 scale
              param$sigma <- .set_hyperparameter(default=c(-5, -3, -1, 1, 3, 5), type="numeric", range=c(-5, 5),
                                                 valid_range=c(-Inf, Inf), randomise=TRUE)
            }
            
            if(svm_kernel %in% c("polydot", "besseldot", "anovadot")){
              ##### Polynomial degree ##########################################
              
              # polydot, besseldot and anovadot expect positive integer degrees.
              param$degree <- .set_hyperparameter(default=c(1, 2, 3, 4, 5), type="integer", range=c(1, 5),
                                                  valid_range=c(1, Inf), randomise=TRUE)
            }
            
            
            if(svm_kernel %in% c("polydot", "tanhdot")){
              ##### Distance scale parameter ###################################
              
              # scale is expressed on a log10 scale. Note that as the feature
              # data is rescaled internally, we should not expect a scale much
              # larger then 1.
              param$scale <- .set_hyperparameter(default=c(-3, -1, 0), type="numeric", range=c(-5, log10(2)),
                                                 valid_range=c(-Inf, Inf), randomise=TRUE)
              
              
              ##### Kernel offset parameter ####################################
              
              # As feature data is rescaled internally by kernlab, we should not
              # expect offsets outside the [0, 1] range. Also, negative values
              # are not allowed for either kernel.
              param$offset <- .set_hyperparameter(default=c(0.0, 0.2, 0.5, 1.0), type="numeric", range=c(0, 1),
                                                  valid_range=c(0, Inf), randomise=TRUE)
            }
            
            if(svm_kernel %in% c("besseldot")){
              ##### Bessel function order ######################################
              
              # The order of the Bessel function should be a non-negative
              # integer. Note that kernlab does not complain about non-integer
              # values, but this results in models that are the same as if they
              # were generated using a floor operation on the order parameter.
              param$order <- .set_hyperparameter(default=c(0, 1, 2, 3, 5), type="integer", range=c(0, 10),
                                                 valid_range=c(0, Inf), randomise=TRUE)
            }
            
            return(param)
          })


#####..train####
setMethod("..train", signature(object="familiarSVM", data="dataObject"),
          function(object, data){
            
            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Find feature columns in data table
            feature_columns <- get_feature_columns(x=data)
            
            # Parse the formula.
            formula <- stats::reformulate(termlabels=feature_columns,
                                          response=quote(outcome))

            # Derive fitting parameters for fitting class probabilities.
            fit_probability <- ifelse(object@outcome_type %in% c("binomial", "multinomial"), TRUE, FALSE)
            
            # Derive svm type from object
            if(is(object, "familiarSVMC")) svm_type <- "C-svc"
            if(is(object, "familiarSVMCBound")) svm_type <- "C-bsvc"
            if(is(object, "familiarSVMNu") & object@outcome_type %in% c("binomial", "multinomial")) svm_type <- "nu-svc"
            if(is(object, "familiarSVMNu") & object@outcome_type %in% c("count", "continuous")) svm_type <- "nu-svr"
            if(is(object, "familiarSVMEps")) svm_type <- "eps-svr"
            if(is(object, "familiarSVMEpsBound")) svm_type <- "eps-bsvr"

            # Set svm-related parameters.
            svm_parameter_list <- list("kernel"=object@hyperparameters$kernel,
                                       "C"=10^(object@hyperparameters$c))
            
            # Set nu-parameter (which not all svm types use).
            if(is(object, "familiarSVMNu")){
              svm_parameter_list$nu <- 10^(object@hyperparameters$nu)
            }
            
            # Set epsilon parameter (which not all svm types use).
            if(is(object, "familiarSVMNu") |
               is(object, "familiarSVMEps") |
               is(object, "familiarSVMEpsBound")){
              svm_parameter_list$epsilon <- 10^(object@hyperparameters$epsilon)
            }
            
            
            ##### Set kernel-specific parameters #########################################
            kernel_parameter_list <- list()
            if(!is.null(object@hyperparameters$sigma)){
              kernel_parameter_list$sigma <- 10^object@hyperparameters$sigma
            }
            
            if(!is.null(object@hyperparameters$degree)){
              kernel_parameter_list$degree <- object@hyperparameters$degree
            }
            
            if(!is.null(object@hyperparameters$scale)){
              kernel_parameter_list$scale <- object@hyperparameters$scale
            }
            
            if(!is.null(object@hyperparameters$offset)){
              kernel_parameter_list$offset <- object@hyperparameters$offset
            }
            
            if(!is.null(object@hyperparameters$order)){
              kernel_parameter_list$order <- object@hyperparameters$order
            }
            
            # Fit the SVM model using kernlab::ksvm
            model <- tryCatch(quiet(do.call(kernlab::ksvm,
                                            args=c(list("x"=formula,
                                                        "data"=data@data,
                                                        "type"=svm_type,
                                                        "kpar"=kernel_parameter_list,
                                                        "prob.model"=fit_probability,
                                                        "fit"=FALSE,
                                                        "cross"=0L),
                                                   svm_parameter_list))),
                              error=identity)
            
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            if(is.null(model)) return(callNextMethod())
            
            # Add model
            object@model <- model
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarSVM", data="dataObject"),
          function(object, data, type=NULL, ...){
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Set the prediction type
            if(is.null(type)){
              if(object@outcome_type %in% c("binomial", "multinomial")){
                type <- "probabilities"
                
              } else if(object@outcome_type %in% c("count", "continuous")){
                type <- "response"
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
            }
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=data)
            
            # Make predictions using the model.
            model_predictions <- tryCatch(kernlab::predict(object=object@model,
                                                           newdata=data@data,
                                                           type=type),
                                          error=identity)
            
            # Check if the model trained at all.
            if(inherits(model_predictions, "error")) return(callNextMethod())
            
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              #####Categorical outcomes######
              
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
              #####Numerical outcomes######
              
              # Extract predicted regression values.
              prediction_table[, "predicted_outcome":=model_predictions]
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(prediction_table)
          })
