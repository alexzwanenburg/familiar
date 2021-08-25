#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarMetricCM",
         contains="familiarMetric")

setClass("familiarMetricCMAveraging",
         contains="familiarMetricCM",
         slots=list("averaging"="character"),
         prototype=list("averaging"="macro"))

setMethod("initialize", signature="familiarMetricCMAveraging",
          function(.Object, metric, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            if(stringi::stri_endswith_fixed(str=metric, pattern="_macro")){
              .Object@averaging <- "macro"
              .Object@metric <- stringi::stri_replace_last_fixed(str=metric,
                                                                 pattern="_macro",
                                                                 replacement="")
              
            } else if(stringi::stri_endswith_fixed(str=metric, pattern="_micro")){
              .Object@averaging <- "micro"
              .Object@metric <- stringi::stri_replace_last_fixed(str=metric,
                                                                 pattern="_micro",
                                                                 replacement="")
              
            } else if(stringi::stri_endswith_fixed(str=metric, pattern="_weighted")){
              .Object@averaging <- "weighted"
              .Object@metric <- stringi::stri_replace_last_fixed(str=metric,
                                                                 pattern="_weighted",
                                                                 replacement="")
              
            } else {
              # Default setting.
              .Object@averaging <- "macro"
              .Object@metric <- metric
            }
            
            return(.Object)
          })



.get_available_confusion_matrix_metrics <- function(){
  return(c(.get_available_accuracy_metrics(),
           .get_available_balanced_accuracy_metrics(),
           .get_available_balanced_error_rate_metrics(),
           .get_available_cohen_kappa_metrics(),
           .get_available_f1_score_metrics(),
           .get_available_fdr_metrics(),
           .get_available_informedness_metrics(),
           .get_available_markedness_metrics(),
           .get_available_mcc_metrics(),
           .get_available_npv_metrics(),
           .get_available_ppv_metrics(),
           .get_available_sensitivity_metrics(),
           .get_available_specificity_metrics(),
           .get_available_youden_metrics()))
}
  

setMethod("is_available", signature(object="familiarMetricCM"),
          function(object, ...){
            return(object@outcome_type %in% c("binomial", "multinomial"))
          })



#####Accuracy###################################################################
setClass("familiarMetricAccuracy",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Accuracy",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_accuracy_metrics <- function() return(c("accuracy"))



#####compute_metric_score (accuracy)#####
setMethod("compute_metric_score", signature(metric="familiarMetricAccuracy"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute accuracy
            return(sum(cm$tp) / cm$n_samples)
          })



#####Balanced accuracy##########################################################
setClass("familiarMetricBalancedAccuracy",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Balanced Accuracy",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_balanced_accuracy_metrics <- function() return(c("balanced_accuracy", "bac"))


#####compute_metric_score (balanced accuracy)#####
setMethod("compute_metric_score", signature(metric="familiarMetricBalancedAccuracy"),
          function(metric, data, ...){
            # Balanced accuracy [Brodersen, K. H., Ong, C. S., Stephan, K. E. & Buhmann,
            # J. M. The Balanced Accuracy and Its Posterior Distribution. in 2010 20th
            # International Conference on Pattern Recognition 3121–3124 (2010).]
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute balanced accuracy
            return(sum(cm$tp / (cm$tp + cm$fn)) / cm$n_classes)
          })


#####Balanced error rate########################################################
setClass("familiarMetricBalancedErrorRate",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Balanced Error Rate",
                                      value_range = c(0.0, 1.0),
                                      higher_better = FALSE))


.get_available_balanced_error_rate_metrics <- function() return(c("balanced_error_rate", "ber"))


#####compute_metric_score (balanced error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricBalancedErrorRate"),
          function(metric, data, ...){
           
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute balanced error rate
            return(sum(cm$fn / (cm$tp + cm$fn)) / cm$n_classes)
          })



#####Cohen's kappa#############################################################
setClass("familiarMetricCohenKappa",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Cohen's Kappa",
                                      value_range = c(-1.0, 1.0),
                                      higher_better = TRUE))


.get_available_cohen_kappa_metrics <- function() return(c("cohen_kappa", "kappa"))


#####compute_metric_score (Cohen's kappa)#####
setMethod("compute_metric_score", signature(metric="familiarMetricCohenKappa"),
          function(metric, data, ...){
            # Cohen's kappa [Cohen, J. A Coefficient of Agreement for Nominal Scales.
            # Educ. Psychol. Meas. 20, 37–46 (1960).]
            #
            # Implementation after
            # https://stats.stackexchange.com/questions/251165/cohens-kappa-with-three-categories-of-variable
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute the observed agreement
            obs_agreement <- sum(cm$tp) / cm$n_samples
            
            # Agreement expected at random
            rand_agreement <- sum((cm$tp + cm$fp) * (cm$tp + cm$fn)) / cm$n_samples^2
            
            if(rand_agreement == 1.0 & obs_agreement == 1.0){
              # Prevent division by zero for perfect predictions.
              return(1.0)
              
            } else {
              # Compute kappa
              return((obs_agreement - rand_agreement) / (1.0 - rand_agreement))
            }
          })


#####F1 Score###################################################################
setClass("familiarMetricF1Score",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "F1 score",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_f1_score_metrics <- function() return(paste0("f1_score", c("", "_micro", "_macro", "_weighted")))


#####compute_metric_score (F1 score)#####
setMethod("compute_metric_score", signature(metric="familiarMetricF1Score"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute score
            score <- 2.0 * cm$tp / (2.0 * cm$tp + cm$fp + cm$fn)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####False discovery rate#######################################################
setClass("familiarMetricFDR",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "False Discovery Rate",
                                      value_range = c(0.0, 1.0),
                                      higher_better = FALSE))


.get_available_fdr_metrics <- function() return(c(paste0("fdr", c("", "_micro", "_macro", "_weighted")),
                                                  paste0("false_discovery_rate", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (False discovery rate)#####
setMethod("compute_metric_score", signature(metric="familiarMetricFDR"),
          function(metric, data, ...){
           
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute score.
            score <- cm$fp / (cm$tp + cm$fp)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####Informedness###############################################################
setClass("familiarMetricInformedness",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Informedness",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_informedness_metrics <- function() return("informedness")


#####compute_metric_score (Informedness)#####
setMethod("compute_metric_score", signature(metric="familiarMetricInformedness"),
          function(metric, data, ...){
            # Informedness [Powers, D. M. Evaluation: from precision, recall and
            # F-measure to ROC, informedness, markedness and correlation.
            # International Journal of Machine Learning Technology 2, 37–63
            # (2011).]
            #
            # See equations 28, 42.
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute the score
            return(sum(cm$prevalence / (1.0 - cm$prevalence) * (cm$tp / (cm$tp + cm$fn) - cm$bias)))
          })



#####Markedness###############################################################
setClass("familiarMetricMarkedness",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Markedness",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_markedness_metrics <- function() return("markedness")


#####compute_metric_score (Markedness)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMarkedness"),
          function(metric, data, ...){
            # Markedness [Powers, D. M. Evaluation: from precision, recall and
            # F-measure to ROC, informedness, markedness and correlation.
            # International Journal of Machine Learning Technology 2, 37–63
            # (2011).]
            #
            # See equations 29, 43.
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute the score
            return(sum(cm$bias / (1.0 - cm$bias) * (cm$tp / (cm$tp + cm$fp) - cm$prevalence)))
          })



#####Matthews` correlation coefficient##########################################
setClass("familiarMetricMCC",
         contains="familiarMetricCM",
         prototype=methods::prototype(name = "Matthews' Correlation Coefficient",
                                      value_range = c(-1.0, 1.0),
                                      higher_better = TRUE))


.get_available_mcc_metrics <- function() return(c("mcc", "matthews_correlation_coefficient"))


#####compute_metric_score (Matthews` correlation coefficient)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMCC"),
          function(metric, data, ...){
            # See [Jurman, G., Riccadonna, S. & Furlanello, C. A comparison of MCC and CEN
            # error measures in multi-class prediction. PLoS One 7, e41882 (2012);
            # Gorodkin, J. Comparing two K-category assignments by a K-category
            # correlation coefficient. Comput. Biol. Chem. 28, 367–374 (2004).]
            # 
            # Mirrors scikit-learn implementation.
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Compute the score
            cov_obs_pred  <- sum(cm$tp) / cm$n_samples - sum(cm$prevalence * cm$bias)
            cov_obs_obs   <- 1.0 - sum(cm$prevalence * cm$prevalence)
            cov_pred_pred <- 1.0 - sum(cm$bias * cm$bias)
            
            if(cov_obs_obs == 0.0 | cov_pred_pred == 0.0){
              return(0.0)
              
            } else {
              return(cov_obs_pred / sqrt(cov_obs_obs * cov_pred_pred))
            }
          })



#####Negative predictive value##################################################
setClass("familiarMetricNPV",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "Negative Predictive Value",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_npv_metrics <- function() return(c(paste0("npv", c("", "_micro", "_macro", "_weighted")),
                                                  paste0("negative_predictive_value", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (Negative predictive value)#####
setMethod("compute_metric_score", signature(metric="familiarMetricNPV"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute NPV
            score <- cm$tn / (cm$tn + cm$fn)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####Positive predictive value##################################################
setClass("familiarMetricPPV",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "Positive Predictive Value",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_ppv_metrics <- function() return(c(paste0("ppv", c("", "_micro", "_macro", "_weighted")),
                                                  paste0("positive_predictive_value", c("", "_micro", "_macro", "_weighted")),
                                                  paste0("precision", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (Positive predictive value)#####
setMethod("compute_metric_score", signature(metric="familiarMetricPPV"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute PPV.
            score <- cm$tp / (cm$tp + cm$fp)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####Sensitivity################################################################
setClass("familiarMetricSensitivity",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "Sensitivity",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_sensitivity_metrics <- function() return(c(paste0("sensitivity", c("", "_micro", "_macro", "_weighted")),
                                                          paste0("recall", c("", "_micro", "_macro", "_weighted")),
                                                          paste0("tpr", c("", "_micro", "_macro", "_weighted")),
                                                          paste0("true_positive_rate", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (Sensitivity)#####
setMethod("compute_metric_score", signature(metric="familiarMetricSensitivity"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute sensitivity
            score <- cm$tp / (cm$tp + cm$fn)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####Specificity################################################################
setClass("familiarMetricSpecificity",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "Specificity",
                                      value_range = c(0.0, 1.0),
                                      higher_better = TRUE))


.get_available_specificity_metrics <- function() return(c(paste0("specificity", c("", "_micro", "_macro", "_weighted")),
                                                          paste0("tnr", c("", "_micro", "_macro", "_weighted")),
                                                          paste0("true_negative_rate", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (Specificity)#####
setMethod("compute_metric_score", signature(metric="familiarMetricSpecificity"),
          function(metric, data, ...){
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute specificity.
            score <- cm$tn / (cm$tn + cm$fp)
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



#####Youden's Index#############################################################
setClass("familiarMetricYouden",
         contains="familiarMetricCMAveraging",
         prototype=methods::prototype(name = "Youden's J Statistic",
                                      value_range = c(-1.0, 1.0),
                                      higher_better = TRUE))

.get_available_youden_metrics <- function() return(c(paste0("youden_j", c("", "_micro", "_macro", "_weighted")),
                                                     paste0("youden_index", c("", "_micro", "_macro", "_weighted"))))


#####compute_metric_score (Youden's Index)#####
setMethod("compute_metric_score", signature(metric="familiarMetricYouden"),
          function(metric, data, ...){
            # Youden's J index [Youden, W. J. Index for rating diagnostic tests. Cancer 3,
            # 32–35 (1950).]
            
            # Compute data from the confusion metrics.
            cm <- ..compute_confusion_matrix_data(data=data, outcome_type=metric@outcome_type)
            
            if(is.null(cm)) return(callNextMethod())
            
            # Aggregate confusion matrix.
            cm <- ..aggregate_confusion_matrix(cm=cm,
                                               averaging=metric@averaging,
                                               outcome_type=metric@outcome_type)
            
            # Compute Youden index.
            score <- cm$tp / (cm$tp + cm$fn) + cm$tn / (cm$tn + cm$fp) - 1.0
            
            # Aggregate score.
            score <- ..aggregate_confusion_matrix_score(score=score,
                                                        cm=cm,
                                                        averaging=metric@averaging,
                                                        outcome_type=metric@outcome_type)
            
            return(score)
          })



..compute_confusion_matrix_data <- function(data, outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  predicted_class <- NULL
  
  # Get the classes and number of classes in data.
  outcome_classes <- get_outcome_class_levels(data, outcome_type=outcome_type)
  n_classes <- length(outcome_classes)
  
  # Remove any entries that lack valid predictions.
  data <- remove_nonvalid_predictions(prediction_table=data,
                                      outcome_type=outcome_type)
  
  # Remove any entries that lack observed values.
  data <- remove_missing_outcomes(prediction_table=data,
                                  outcome_type=outcome_type)
  
  if(is_empty(data)) return(NULL)
  
  # Create empty scalars
  tp <- tn <- fp <- fn <- prevalence <- bias <- numeric(n_classes)
  
  # Total number of samples
  n_samples <- nrow(data)
  
  # Get the outcome column.
  outcome_column <- get_outcome_columns(outcome_type)
  
  # Iterate over positive classes
  for(ii in seq_along(outcome_classes)){
    
    # Determine true positives, true negatives, false positives and false negatives
    tp[ii] <- nrow(data[predicted_class==outcome_classes[ii] & get(outcome_column)==outcome_classes[ii], ])
    tn[ii] <- nrow(data[predicted_class!=outcome_classes[ii] & get(outcome_column)!=outcome_classes[ii], ])
    fp[ii] <- nrow(data[predicted_class==outcome_classes[ii] & get(outcome_column)!=outcome_classes[ii], ])
    fn[ii] <- nrow(data[predicted_class!=outcome_classes[ii] & get(outcome_column)==outcome_classes[ii], ])
    
    # Prevalence (fraction of observed outcome_column class)
    prevalence[ii] <- nrow(data[get(outcome_column)==outcome_classes[ii]]) / n_samples
    
    # Bias (fraction of predicted positive class)
    bias[ii] <- nrow(data[predicted_class==outcome_classes[ii]]) / n_samples
  }
  
  return(list("tp"=tp,
              "tn"=tn,
              "fp"=fp,
              "fn"=fn,
              "prevalence"=prevalence,
              "bias"=bias,
              "n_samples"=n_samples,
              "n_classes"=n_classes))
}



..aggregate_confusion_matrix <- function(cm, averaging, outcome_type){
  
  if(outcome_type == "binomial"){
    # Use second class as positive class
    return(list("tp" = cm$tp[2],
                "tn" = cm$tn[2],
                "fp" = cm$fp[2],
                "fn" = cm$fn[2],
                "prevalence" = 1.0,
                "bias" = 1.0,
                "n_samples" = cm$n_samples,
                "n_classes" = 1.0))
    
  } else if(outcome_type == "multinomial"){
    
    if(averaging == "micro"){
      # Merge confusion matrix for micro-averaging.
      return(list("tp" = sum(cm$tp),
                  "tn" = sum(cm$tn),
                  "fp" = sum(cm$fp),
                  "fn" = sum(cm$fn),
                  "prevalence" = 1.0,
                  "bias" = 1.0,
                  "n_samples" = cm$n_classes * cm$n_samples,
                  "n_classes" = 1.0))
      
    } else {
      # No changes are needed.
      return(cm)
    }
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
}



..aggregate_confusion_matrix_score <- function(score, cm, averaging, outcome_type){
  
  # Replace all non-finite values.
  score[!is.finite(score)] <- NA_real_
  
  if(all(is.na(score))) return(NA_real_)
  
  # Aggregation is only required for multinomial outcomes.
  if(outcome_type == "multinomial"){
    
    if(averaging == "weighted"){
      # Weighted averaging
      score <- sum(cm$prevalence * score, na.rm=TRUE)
      
    } else if(averaging == "macro"){
      
      # Compute the average.
      score <- mean(score, na.rm=TRUE)
      
    } else if(averaging == "micro"){
      # Micro averaging
      score <- score
      
    } else {
      ..error_reached_unreachable_code("..aggregate_confusion_matrix_score: encountered unknown averaging method.")
    }
  } 
  
  return(score)
}
