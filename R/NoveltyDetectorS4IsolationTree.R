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
          function(object, data=NULL, ...){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$n_tree      <- list()
            param$sample_size <- list()
            param$m_try       <- list()
            param$tree_depth  <- list()
            param$n_dim       <- list()

            # If data is not provided, return the list with hyperparameter
            # /names only.
            if(is.null(data)) return(param)
            
            # Get the number of instances. Note that we are not interested in
            # samples per se, as we are assessing novelty.
            n_samples <- nrow(data@data)
            
            # Get the number of features.
            n_features <- get_n_features(x=data)
            
            
            ###### Number of trees #############################################
            # We limit the number of trees to limit memory footprint.
            default_n_trees <- log2(max(c(64, ceiling(sqrt(n_samples)))))
            
            # Note that the number of trees is defined in powers of 2, based on
            # Oshiro, T. M., Perez, P. S., & Baranauskas, J. A. (2012, July).
            # How many trees in a random forest?. In MLDM (pp. 154-168).
            param$n_tree <- .set_hyperparameter(default=default_n_trees,
                                                type="integer",
                                                range=c(4, 10),
                                                valid_range=c(0, Inf),
                                                randomise=FALSE)
            
            
            ###### Sample size #################################################
            # We limit the number of samples in each tree to limit memory
            # footprint.
            default_sample_size <- max(c(128, ceiling(sqrt(n_samples))))
            if(n_samples < default_sample_size) default_sample_size <- n_samples
            
            # Express as fraction.
            default_sample_size <- default_sample_size / n_samples
            
            # Note that the sample size is here noted as a fraction, which
            # corresponds to the usage in ranger.
            param$sample_size <- .set_hyperparameter(default=default_sample_size,
                                                     type="numeric",
                                                     range=c(2/n_samples, 1.0),
                                                     valid_range=c(0, 1),
                                                     randomise=FALSE)
            
            
            ##### Number of candidate features selected at node ################
            
            default_m_try <- 3 / n_features
            if(default_m_try > 1.0) default_m_try <- 1.0
            
            # Note that the number of features is here noted as a fraction, but
            # is used in the isolation forest as an integer. Familiar ensures
            # that always at least 1 feature is available as a candidate.
            param$m_try <- .set_hyperparameter(default=default_m_try,
                                               type="numeric",
                                               range=c(0.0, 1.0),
                                               randomise=FALSE)

            
            ##### Maximum tree depth ###########################################

            default_tree_depth <- ceiling(log2(default_sample_size * n_samples))
            if(default_tree_depth < 1) default_tree_depth <- 1
            
            # Determines the depth trees are allowed to grow to. Larger depths
            # increase the risk of overfitting.
            param$tree_depth <- .set_hyperparameter(default=default_tree_depth,
                                                    type="integer",
                                                    range=c(1, 10),
                                                    valid_range=c(1, Inf),
                                                    randomise=FALSE)
            
            
            ##### Number of splitting dimensions ###############################
            
            # Switch between extended and conventional isolation trees.
            if(object@learner %in% c("isolation_forest", "extended_isolation_forest")){
              default_n_dim <- 3
              
            } else if(object@learner %in% c("simple_isolation_forest")){
              default_n_dim <- 1
            }
            
            # The number of splitting dimensions cannot be larger than the
            # number of features.
            if(default_n_dim > n_features && n_features > 0) default_n_dim <- n_features
            
            param$n_dim <- .set_hyperparameter(default=default_n_dim,
                                               type="integer",
                                               range=c(1, 3),
                                               valid_range=c(1, Inf),
                                               randomise=FALSE)
            
            return(param)
          })



#####..train####
setMethod("..train", signature(object="familiarIsolationForest", data="dataObject"),
          function(object, data, ...){

            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Check if hyperparameters are set.
            if(is.null(object@hyperparameters)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
            # Replace any ordered variables by factors. We do this because
            # ordered features can not be handled using isotree.
            ordered_features <- colnames(data@data)[sapply(data@data, is.ordered)]
            for(current_feature in ordered_features){
              data@data[[current_feature]] <- factor(x=data@data[[current_feature]],
                                                     levels=levels(data@data[[current_feature]]),
                                                     ordered=FALSE)
            }
            
            # Extract hyperparameters from the object.
            param <- object@hyperparameters
            
            # Create an isolation forest. Note that in addition to specifying
            # the number of trees and the number of samples assessed for each
            # tree, missing_action is set to "fail" -- this decreases model
            # footprint and is not necessary as familiar has its own imputation
            # routines.
            detector <- isotree::isolation.forest(data=data@data[, mget(get_feature_columns(data))],
                                                  sample_size=ceiling(param$sample_size * nrow(data@data)),
                                                  ntrees=ceiling(2^(param$n_tree)),
                                                  ndim=param$n_dim,
                                                  ntry=max(c(1, ceiling(param$m_try * get_n_features(x=data)))),
                                                  max_depth=param$tree_depth,
                                                  nthreads=1L,
                                                  missing_action="fail")
            
            # Add model
            object@model <- detector
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarIsolationForest", data="dataObject"),
          function(object, data, type="novelty", ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Get a placeholder prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=data,
                                                                 type="novelty")
            
            # Find and replace ordered features.
            ordered_features <- colnames(data@data)[sapply(data@data, is.ordered)]
            for(current_feature in ordered_features){
              data@data[[current_feature]] <- factor(x=data@data[[current_feature]],
                                                     levels=levels(data@data[[current_feature]]),
                                                     ordered=FALSE)
            }
            
            # Find novelty values.
            novelty_values <- predict(object=object@model,
                                      newdata=data@data)
            
            # Store the novelty values in the table.
            prediction_table[, "novelty":=novelty_values]
            
            return(prediction_table)
          })



##### .trim_model---------------------------------------------------------------
setMethod(".trim_model", signature(object="familiarIsolationForest"),
          function(object, ...){
            
            # Add show.
            object <- .capture_show(object)
            
            # Set is_trimmed to TRUE.
            object@is_trimmed <- TRUE
            
            return(object)
          })
