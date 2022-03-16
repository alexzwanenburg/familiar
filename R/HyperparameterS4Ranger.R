#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R

setClass("familiarHyperparameterLearnerRanger",
         contains="familiarHyperparameterLearner")



##### initialize ---------------------------------------------------------------
setMethod("initialize", signature(.Object="familiarHyperparameterLearnerRanger"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # Set required package
            .Object@package <- "ranger"
            
            # Set name
            .Object@name <- "Random Forest (ranger) hyperparameter optimisation model"
            
            return(.Object)
          })



.get_available_ranger_hyperparameter_learners <- function(){
  return("random_forest")
}



##### get_prediction_type ------------------------------------------------------
setMethod("get_prediction_type", signature(object="familiarHyperparameterLearnerRanger"),
          function(object, ...){
            return(c("default", "sd", "percentile", "raw"))
          })



##### ..train ------------------------------------------------------------------
setMethod("..train", signature(object="familiarHyperparameterLearnerRanger", data="data.table"),
          function(object, data, ...){
            
            # Check if the training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Check that required packages are loaded and installed.
            require_package(object, "train")
            
            # Parse formula.
            formula <- stats::reformulate(termlabels=object@target_hyperparameters,
                                          response="optimisation_score")
            
            # Set hyperparameters of the random forest.
            n_tree <- 400
            n_train <- nrow(data)
            sample_fraction <- max(c(0.3, min(c(1, 1/(0.025*n_train)))))
            
            # Parse formula.
            formula <- stats::reformulate(termlabels=parameter_names,
                                          response="optimisation_score")
            
            # Train a conventional random forest
            model <- ranger::ranger(formula,
                                    data = data,
                                    num.trees = n_tree,
                                    sample.fraction = sample_fraction,
                                    num.threads = 1L,
                                    verbose = FALSE)
            
            # Add model
            object@model <- model
            
            # Set learner version
            object <- set_package_version(object)
            
            return(object)
          })



##### ..predict ----------------------------------------------------------------
setMethod("..predict", signature(object="familiarHyperparameterLearnerRanger", data="data.table"),
          function(object, data, type="default", percentile=NULL, ...){
            
            # Check that required packages are loaded and installed.
            require_package(object, "predict")
            
            # Check if the model was trained.
            if(!model_is_trained(object)) return(callNextMethod())
            
            # Check if the data is empty.
            if(is_empty(data)) return(callNextMethod())
            
            # Get an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=data,
                                                                 type=type)
            
            # Make predictions.
            predicted_scores <- predict(object=object,
                                        data=data,
                                        predict.all=TRUE,
                                        num.threads=1L,
                                        verbose=FALSE)$predictions
            
            # Compute mean and standard deviation.
            score_mean <- apply(predicted_scores, MARGIN=1, mean)
            score_sd <- apply(predicted_scores, MARGIN=1, stats::sd)
            
            # Separate by type
            if(type == "default"){
              prediction_table[, "mu":=score_mean]
              
            } else if(type == "sd"){
              prediction_table[, ":="("mu"=score_mean,
                                      "sigma"=score_sd)]
              
            } else if(type == "percentile"){
              # Compute the requested percentile.
              score_percentile <- apply(predicted_scores, MARGIN=1, stats::quantile, probs=percentile, names=FALSE)
              
              prediction_table[, "percentile":=score_percentile]
              
            } else if(type == "raw"){
              # Drop raw_1 from the placeholder prediction table.
              prediction_table[, "raw_1":=NULL]
              
              # Parse raw data to a data.table with the expected output.
              raw_data <- data.table::data.table(predicted_scores)
              
              # Set colnames.
              data.table::setnames(raw_data, new=paste0("raw_", seq_len(ncol(raw_data))))
              
              # Combine with the placeholder prediction table.
              prediction_table <- cbind(prediction_table, raw_data)
              
            } else {
              ..error_reached_unreachable_code(paste0("..predict,familiarHyperparameterLearnerRanger,data.table: ",
                                                      "Encountered an unknown prediction type: ", type))
            }
            
            return(prediction_table)
          })
