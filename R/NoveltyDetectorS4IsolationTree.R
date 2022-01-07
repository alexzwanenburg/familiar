#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarIsolationForest",
         contains="familiarNoveltyDetector")


#####initialize#################################################################
setMethod("initialize", signature(.Object="familiarIsolationForest"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "isotree"
            
            return(.Object)
          })


.get_available_isolation_forest_detectors <- function(show_general=TRUE){
  return(c("isolation_forest", "simple_isolation_forest", "extended_isolation_forest"))
} 



#####is_available#####
setMethod("is_available", signature(object="familiarIsolationForest"),
          function(object, ...){
            # Isolation forests exists for all outcome types and data.
            return(TRUE)
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarIsolationForest"),
          function(object, data=NULL){
            
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
            param$n_tree <- .set_hyperparameter(default=c(4, 8, 10), type="integer", range=c(4, 10),
                                                valid_range=c(0, Inf), randomise=TRUE)
            
            
            ###### Sample size #################################################
            
            # Note that the sample size is here noted as a fraction, which
            # corresponds to the usage in ranger.
            param$sample_size <- .set_hyperparameter(default=c(0.30, 0.50, 0.70, 1.00), type="numeric", range=c(2/n_samples, 1.0),
                                                     valid_range=c(0, 1), randomise=TRUE)
            
            
            ##### Number of candidate features selected at node ################
            
            # Note that the number of features is here noted as a fraction, but
            # is used in ranger as an integer. Familiar ensures that always at
            # least 1 feature is available as a candidate.
            param$m_try <- .set_hyperparameter(default=c(0.1, 0.3, 0.5, 1.0), type="numeric", range=c(0.0, 1.0),
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
            param$node_size <- .set_hyperparameter(default=node_size_default, type="integer", range=node_size_range,
                                                   valid_range=c(1, Inf), randomise=TRUE)
            
            
            ##### Maximum tree depth ###########################################
            
            # Determines the depth trees are allowed to grow to. Larger depths
            # increase the risk of overfitting.
            param$tree_depth <- .set_hyperparameter(default=c(1, 2, 3, 7), type="integer", range=c(1, 10),
                                                    valid_range=c(1, Inf), randomise=TRUE)
            
            
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
            param$split_rule <- .set_hyperparameter(default=split_rule_default, type="factor", range=split_rule_range,
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
            
            ##### Feature selection forest type ################################
            
            # Enables the construction of holdout forests. A conventional forest
            # is grown by default.
            param$fs_forest_type <- .set_hyperparameter(default="standard", type="factor",
                                                        range=c("standard", "holdout"),
                                                        randomise=FALSE)
            
            ##### Feature selection variable importance method #################
            
            # Enables the use of different variable importance methods. The
            # permutation method is used by default.
            param$fs_vimp_method <- .set_hyperparameter(default="permutation", type="factor",
                                                        range=c("permutation", "impurity", "impurity_corrected"),
                                                        randomise=FALSE)
            
            return(param)
          })



#####..train####
setMethod("..train", signature(object="familiarIsolationForest", data="dataObject"),
          function(object, data){
            browser()
            # Aggregate repeated measurement data - ranger does not facilitate
            # repeated measurements.
            data <- aggregate_data(data=data)
            
            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
  
            
            # Add model
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarIsolationForest", data="dataObject"),
          function(object, data, type="default", time=NULL, ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            browser()
          })



##### .trim_model---------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarIsolationForest"),
          function(object, ...){
            browser()
            
            # # Update model by removing the call.
            # object@model$call <- call("trimmed")
            # 
            # # Add show.
            # object <- .capture_show(object)
            # 
            # # Remove the predictions.
            # object@model$predictions <- NULL
            # 
            # # Set is_trimmed to TRUE.
            # object@is_trimmed <- TRUE
            
            return(object)
          })
