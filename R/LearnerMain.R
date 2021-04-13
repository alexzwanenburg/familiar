#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include LearnerS4Cox.R
#' @include LearnerS4GLM.R
#' @include LearnerS4GLMnet.R
#' @include LearnerS4KNN.R
#' @include LearnerS4NaiveBayes.R
#' @include LearnerS4MBoost.R
#' @include LearnerS4Ranger.R
#' @include LearnerS4RFSRC.R
#' @include LearnerS4SVM.R
#' @include LearnerS4SurvivalRegression.R
#' @include LearnerS4XGBoost.R
NULL

setMethod("promote_learner", signature(object="familiarModel"),
          function(object){
            
            learner <- object@learner
            
            if(learner %in% .get_available_naive_bayes_learners()){
              # Naive bayes model
              object <- methods::new("familiarNaiveBayes", object)
            
            } else if(learner %in% .get_available_radial_knn_learners()){
              # k-nearest neighbours model with radial kernel
              object <- methods::new("familiarKNNradial", object)
              
            } else if(learner %in% .get_available_linear_knn_learners()){
              # k-nearest neighbours model with radial kernel
              object <- methods::new("familiarKNNlinear", object)
              
            } else if(learner %in% .get_available_svm_c_learners()){
              # C-classification support vector machines.
              object <- methods::new("familiarSVMC", object)
              
            } else if(learner %in% .get_available_svm_nu_learners()){
              # Nu-classification and regression support vector machines.
              object <- methods::new("familiarSVMNu", object)
              
            } else if(learner %in% .get_available_svm_eps_learners()){
              # Epsilon regression support vector machines.
              object <- methods::new("familiarSVMEps", object)
              
            } else if(learner %in% .get_available_glm_learners()){
              # Generalised linear models
              object <- methods::new("familiarGLM", object)

            } else if(learner %in% .get_available_glmnet_ridge_learners()){
              # Ridge penalised regression models
              object <- methods::new("familiarGLMnetRidge", object)
              
            } else if(learner %in% .get_available_glmnet_lasso_learners()){
              # Lasso penalised regression models
              object <- methods::new("familiarGLMnetLasso", object)
              
            } else if(learner %in% .get_available_glmnet_elastic_net_learners()){
              # Elastic net penalised regression models.
              object <- methods::new("familiarGLMnetElasticNet", object)
              
            } else if(learner %in% .get_available_mboost_lm_learners()){
              # Boosted generalised linear models
              object <- methods::new("familiarMBoostLM", object)
              
            } else if(learner %in% .get_available_xgboost_lm_learners()){
              # Extreme gradient boosted linear models
              object <- methods::new("familiarXGBoostLM", object)
              
            } else if(learner %in% .get_available_cox_learners()){
              # Cox proportional hazards regression model
              object <- methods::new("familiarCoxPH", object)
            
            } else if(learner %in% .get_available_survival_regression_learners()){
              # Fully parametric survival regression model
              object <- methods::new("familiarSurvRegr", object)
              
            } else if(learner %in% .get_available_rfsrc_learners()){
              # Random forests for survival, regression, and classification
              object <- methods::new("familiarRFSRC", object)
              
            } else if(learner %in% .get_available_ranger_learners()){
              # Ranger random forests
              object <- methods::new("familiarRanger", object)
              
            } else if(learner %in% .get_available_mboost_tree_learners()){
              # Boosted regression trees
              object <- methods::new("familiarMBoostTree", object)
              
            } else if(learner %in% .get_available_xgboost_tree_learners()){
              # Extreme gradient boosted trees
              object <- methods::new("familiarXGBoostTree", object)
            
            } else if(learner %in% .get_available_xgboost_dart_learners()){
              # Extreme gradient boosted trees
              object <- methods::new("familiarXGBoostDart", object)

            }
            
            # Returned object can be a standard familiarModel
            return(object)
          })



learner.get_model_hyperparameters <- function(data, learner, outcome_type, names_only=FALSE){
  
  # Get the outcome type from the data object, if available
  if(!is.null(data)) outcome_type <- data@outcome_type

  # Create familiarModel
  fam_model <- methods::new("familiarModel",
                            learner=learner,
                            outcome_type=outcome_type)
  
  # Set up the specific model
  fam_model <- promote_learner(fam_model)
  
  # Model hyperparameters
  model_hyperparameters <- get_default_hyperparameters(fam_model, data=data)
  
  # Extract names from parameter list
  if(names_only){
    model_hyperparameters <- names(model_hyperparameters)
  }
  
  # Return hyperparameter list, or hyperparameter names
  return(model_hyperparameters)
}



learner.check_outcome_type <- function(learner, outcome_type, as_flag=FALSE){
  
  # Create familiarModel
  fam_model <- methods::new("familiarModel",
                            learner=learner,
                            outcome_type=outcome_type)
  
  # Set up the specific model
  fam_model <- promote_learner(fam_model)
  
  # Check validity.
  learner_available <- is_available(fam_model)
  
  if(as_flag) return(learner_available)
    
  # Check if the familiar model has been successfully promoted.
  if(!is_subclass(class(fam_model)[1], "familiarModel")){
    stop(paste0(learner, " is not a valid learner. Please check the vignette for available learners."))
  }
  
  # Check if the learner is available.
  if(!learner_available){
    stop(paste0(learner, " is not available for \"", outcome_type, "\" outcomes."))
  }
}



learner.check_model_hyperparameters <- function(learner, user_param, outcome_type){
  
  # Check if current learner is defined in the user parameter list
  if(is.null(user_param[[learner]])) return()
  
  # Get parameters defined by the model
  defined_param <- learner.get_model_hyperparameters(data=NULL,
                                                     learner=learner,
                                                     outcome_type=outcome_type,
                                                     names_only=TRUE)
  
  if(is.null(defined_param)){ return() }
  
  # Parse names of the user-defined parameters
  user_param    <- names(user_param[[learner]])
  
  # Determine if parameters are incorrectly set
  if(!all(user_param %in% defined_param)){
    
    # Determine incorrectly defined user parameters
    undefined_user_param <- user_param[!user_param %in% defined_param]
    
    # Notify the user
    logger.stop(paste0("Configuration: \"", paste(undefined_user_param, collapse="\", \""), "\" are invalid parameters. Valid parameters are: \"",
                       paste(defined_param, collapse="\", \"")), "\".")
  }
}



learner.check_model_prediction_type <- function(learner, outcome_type){
  # Prediction types are specified by the S4 familiarModel objects.
  
  # Create placeholder familiar model and promote it to the right subclass.
  fam_model <- methods::new("familiarModel",
                            outcome_type=outcome_type,
                            learner=learner)
  
  fam_model <- promote_learner(fam_model)
  
  # Return the predicted outcome type.
  return(get_prediction_type(fam_model))
}


#' Internal function for obtaining a default signature size parameter
#'
#' @param data_obj dataObject class object which contains the data on which the
#'   preset parameters are determined.
#' @param outcome_type Type of oucome. Default settings are determined, among
#'   others by the the outcome type. For example, one default signature size
#'   depends on the number of events for `survival` outcomes, based on rules of
#'   thumb.
#' @param restrict_samples Logical indicating whether the signature size should
#'   be limited by the number of samples in addition to the number of available
#'   features. This may help convergence of OLS-based methods.
#'
#' @return List containing the preset values for the signature size parameter.
#'
#' @md
#' @keywords internal
.get_default_sign_size <- function(data_obj, restrict_samples=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_event <- NULL
  
  # Determine the outcome type
  outcome_type <- data_obj@outcome_type
  
  # Determine the number of samples and features
  n_samples <- data.table::uniqueN(data_obj@data, by=get_id_columns(id_depth="series"))
  n_features <- get_n_features(data_obj)
  
  # Determine the actual range of features dynamically.
  if(restrict_samples &  n_samples > 1){
    sign_size_range <- c(1, min(n_samples - 1, n_features))
    
  } else if(restrict_samples & n_samples <= 1){
    sign_size_range <- c(1, 1)
    
  } else {
    sign_size_range <- c(1, n_features)
  }
  
  if(outcome_type %in% c("binomial", "multinomial")){

    # Get the number of outcome classes
    n_classes <- nlevels(data_obj@data$outcome)
    
    # Determine the range
    sign_size_default <- unique(c(1, 2, 5, 10, max(c(1.0, floor(n_samples / (n_classes * 7.5))))))
    
  } else if(outcome_type %in% c("survival")) {
    
    # Get the number of events
    n_events <- nrow(data_obj@data[outcome_event==1, ])
    
    # Determine the range
    sign_size_default <- unique(c(1, 2, 5, 10, max(c(1.0, floor(n_events / 15)))))
           
  } else if(outcome_type %in% c("count", "continuous")){
    
    # Determine the range
    sign_size_default <- unique(c(1, 2, 5, 10, max(c(1.0, floor(n_samples / 15)))))
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  # Limit default to those values that fall within the range.
  sign_size_default <- sign_size_default[sign_size_default >= sign_size_range[1] &
                                           sign_size_default <= sign_size_range[2]]
  
  return(.set_hyperparameter(default=sign_size_default, type="integer", range=sign_size_range,
                             valid_range=c(1, Inf), randomise=TRUE, distribution="log"))
}
