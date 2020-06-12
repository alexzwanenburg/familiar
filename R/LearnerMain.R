#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include LearnerS4Cox.R
#' @include LearnerS4GLM.R
#' @include LearnerS4GLMnet.R
#' @include LearnerS4Ranger.R
#' @include LearnerS4RFSRC.R
NULL

setMethod("promote_learner", signature(object="familiarModel"),
          function(object){
            
            learner <- object@learner
            
            if(learner == "naive_bayes"){
              # Naive bayes model
              object <- methods::new("familiarNaiveBayes", object)
              
            } else if(learner %in% c("k_nearest_neighbours", "k_nearest_neighbours_radial")) {
              # K-nearest neighbours model
              object <- methods::new("familiarKNN", object)
              
            } else if(learner.svm.is_svm(learner=learner)){
              # Support vector machines
              object <- methods::new("familiarSVM", object)
              
            } else if(learner %in% c("glm", "glm_logistic", "glm_probit", "glm_cauchy",
                                     "glm_log", "glm_loglog", "glm_multinomial", "glm_gaussian",
                                     "glm_log_gaussian", "glm_inv_gaussian", "glm_poisson",
                                     "glm_log_poisson")){
              # Generalised linear models
              object <- methods::new("familiarGLM", object)
              
            } else if(learner %in% c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson",
                                     "elastic_net_binomial","elastic_net_multinomial", "elastic_net_cox",
                                     "lasso", "lasso_gaussian", "lasso_poisson",
                                     "lasso_binomial", "lasso_multinomial", "lasso_cox",
                                     "ridge", "ridge_gaussian", "ridge_gaussian",
                                     "ridge_poisson", "ridge_binomial", "ridge_multinomial", "ridge_cox")){
              # Elastic net penalised regression models
              object <- methods::new("familiarGLMnet", object)
              
            } else if(learner %in% c("boosted_glm", "boosted_glm_multinomial", "boosted_glm_logistic",
                              "boosted_glm_probit", "boosted_glm_loglog", "boosted_glm_cauchy", "boosted_glm_log",
                              "boosted_glm_auc", "boosted_glm_gaussian", "boosted_glm_huber", "boosted_glm_laplace",
                              "boosted_glm_poisson", "boosted_glm_cox", "boosted_glm_surv",
                              "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex")){
              # Boosted generalised linear models
              object <- methods::new("familiarMBBoostLM", object)
              
            } else if(learner %in% c("xgboost_lm", "xgboost_lm_logistic", "xgboost_lm_linear",
                                     "xgboost_lm_poisson", "xgboost_lm_gamma", "xgboost_lm_cox")){
              # Extreme gradient boosted linear models
              object <- methods::new("familiarXGBoostLM", object)
              
            } else if(learner == "cox"){
              # Cox proportional hazards regression model
              object <- methods::new("familiarCoxPH", object)
            
            } else if(learner %in% c("survival_regr", "survival_regr_weibull", "survival_regr_exponential",
                                     "survival_regr_gaussian", "survival_regr_logistic",
                                     "survival_regr_lognormal", "survival_regr_loglogistic")){
              # Fully parametric survival regression model
              object <- methods::new("familiarSurvRegr", object)
              
            } else if(learner %in% c("random_forest", "random_forest_rfsrc")){
              # Random forests for survival, regression, and classification
              object <- methods::new("familiarRFSRC", object)
              
            } else if(learner=="random_forest_ranger"){
              # Ranger random forests
              object <- methods::new("familiarRanger", object)
              
            } else if(learner %in% c("boosted_tree", "boosted_tree_logistic", "boosted_tree_probit",
                                     "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log",
                                     "boosted_tree_auc", "boosted_tree_gaussian", "boosted_tree_huber",
                                     "boosted_tree_laplace", "boosted_tree_poisson", "boosted_tree_cox", "boosted_tree_surv",
                                     "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan", "boosted_tree_cindex")){
              # Boosted regression trees
              object <- methods::new("familiarMBBoostTree")
              
            } else if(learner %in% c("xgboost_tree", "xgboost_tree_logistic", "xgboost_tree_linear",
                                     "xgboost_tree_poisson", "xgboost_tree_gamma", "xgboost_tree_cox")){
              # Extreme gradient boosted trees
              object <- methods::new("familiarXGBoostTree")
              
            } else if(learner %in% c("__test_invalid", "__test_perfect", "__test_intercept")){
              # TEST learners.
              object <- methods::new("familiarTestLearner")
            }
            
            # Returned object can be a standard familiarModel
            return(object)
          })

# 
# learner.main <- function(object=NULL, learner=NULL, purpose, outcome_type=NULL, data_obj=NULL, time_max=NULL, extra_output=FALSE){
#   # This is a convenience function so that all relevant access to learner-specific functions can be implemented and called here.
#   # Do not call this function directly, but use interface functions learner.run_model, learner.get_model_hyperparameters and learner.check_outcome_type
# 
#   # Extract details from the familiar model
#   if(!is.null(object)){
#     learner      <- object@learner
#     outcome_type <- object@outcome_type
#   }
# 
#   # Divert calls to "glm" learner for survival data to "cox"
#   if(learner=="glm" & outcome_type=="survival"){
#     learner <- "cox"
#   }
# 
#   ##### Decision-boundary based learners #####
# 
#   # Naive bayes model
#   if(learner=="naive_bayes"){
#     if(purpose=="train") {
#       model_list    <- learner.naive_bayes.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.naive_bayes.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.naive_bayes.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.naive_bayes.param(data_obj=data_obj)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.naive_bayes.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.naive_bayes.get_calibration(object=object, data_obj=data_obj)
#     }
#   }
# 
#   # K-nearest neighbours
#   if(learner %in% c("k_nearest_neighbours", "k_nearest_neighbours_radial")){
#     if(purpose=="train") {
#       model_list    <- learner.knn.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.knn.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.knn.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.knn.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.knn.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.knn.get_calibration(object=object, data_obj=data_obj)
#     }
#   }
# 
#   # Support vector machines
#   if(learner.svm.is_svm(learner=learner)){
#     if(purpose=="train") {
#       model_list    <- learner.svm.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.svm.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.svm.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.svm.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.svm.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.svm.calibration_info(object=object, data_obj=data_obj)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.svm.get_calibration(object=object, data_obj=data_obj)
#     }
#   }
# 
# 
#   ##### Regression learners #####
# 
#   # Generalised linear models
#   if(learner %in% c("glm", "glm_logistic", "glm_probit", "glm_cauchy", "glm_log", "glm_loglog", "glm_multinomial", "glm_gaussian", "glm_log_gaussian",
#                     "glm_inv_gaussian", "glm_poisson", "glm_log_poisson")){
#     if(purpose=="train") {
#       model_list    <- learner.glm.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.glm.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.glm.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.glm.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.glm.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.glm.calibration_info(object=object, data_obj=data_obj)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.glm.get_calibration(object=object, data_obj=data_obj)
#     }
#   }
# 
#   # Elastic net penalised regression models
#   if(learner %in% c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson", "elastic_net_binomial","elastic_net_multinomial",
#                     "elastic_net_cox", "lasso", "lasso_gaussian", "lasso_poisson", "lasso_binomial", "lasso_multinomial", "lasso_cox",
#                     "ridge", "ridge_gaussian", "ridge_gaussian", "ridge_poisson", "ridge_binomial", "ridge_multinomial", "ridge_cox")){
#     if(purpose=="train") {
#       model_list    <- learner.net.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.net.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.net.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.net.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.net.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.net.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.net.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.net.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
# 
#   # Boosted generalised linear models
#   if(learner %in% c("boosted_glm", "boosted_glm_multinomial", "boosted_glm_logistic", "boosted_glm_probit", "boosted_glm_loglog", "boosted_glm_cauchy", "boosted_glm_log",
#                     "boosted_glm_auc", "boosted_glm_gaussian", "boosted_glm_huber", "boosted_glm_laplace", "boosted_glm_poisson", "boosted_glm_cox", "boosted_glm_surv",
#                     "boosted_glm_weibull", "boosted_glm_lognormal", "boosted_glm_gehan", "boosted_glm_cindex")){
#     if(purpose=="train") {
#       model_list    <- learner.mbboost.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.mbboost.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.mbboost.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.mbboost.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.mbboost.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.mbboost.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="recalibrate") {
#       calibration_model <- learner.mbboost.recalibrate(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.mbboost.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.mbboost.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Extreme gradient boosted linear models
#   if(learner %in% c("xgboost_lm", "xgboost_lm_logistic", "xgboost_lm_linear", "xgboost_lm_poisson", "xgboost_lm_gamma", "xgboost_lm_cox")){
#     if(purpose=="train") {
#       model_list    <- learner.xgboost.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.xgboost.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.xgboost.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.xgboost.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.xgboost.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.xgboost.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="recalibrate") {
#       calibration_model <- learner.xgboost.recalibrate(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.xgboost.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.xgboost.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Cox proportional hazards regression model
#   if(learner == "cox"){
#     if(purpose=="train") {
#       model_list    <- learner.cox.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.cox.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.cox.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.cox.param(data_obj=data_obj)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.cox.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.cox.prediction_type()
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.cox.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.cox.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Fully parametric survival regression model
#   if(learner %in% c("survival_regr", "survival_regr_weibull", "survival_regr_exponential", "survival_regr_gaussian", "survival_regr_logistic",
#                     "survival_regr_lognormal", "survival_regr_loglogistic")){
#     if(purpose=="train") {
#       model_list    <- learner.survreg.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.survreg.test(object=object, data_obj=data_obj, extra_output=extra_output)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.survreg.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.survreg.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.survreg.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.survreg.prediction_type()
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.survreg.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.survreg.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   
#   ##### Random forest learners #####
#   # Random forest
#   if(learner %in% c("random_forest", "random_forest_rfsrc")){
#     if(purpose=="train") {
#       model_list    <- learner.rf_rfsrc.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.rf_rfsrc.test(object=object, data_obj=data_obj, time_max=time_max, extra_output=extra_output)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.rf_rfsrc.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.rf_rfsrc.param(data_obj=data_obj)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.rf_rfsrc.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.rf_rfsrc.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.rf_rfsrc.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.rf_rfsrc.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Maximally selected rank random forest
#   if(learner=="random_forest_ranger"){
#     if(purpose=="train") {
#       model_list    <- learner.rf_ranger.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.rf_ranger.test(object=object, data_obj=data_obj, time_max=time_max, extra_output=extra_output)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.rf_ranger.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.rf_ranger.param(data_obj=data_obj)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.rf_ranger.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.rf_ranger.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.rf_ranger.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.rf_ranger.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Boosted regression trees
#   if(learner %in% c("boosted_tree", "boosted_tree_logistic", "boosted_tree_probit", "boosted_tree_loglog", "boosted_tree_cauchy", "boosted_tree_log",
#                     "boosted_tree_auc", "boosted_tree_gaussian", "boosted_tree_huber", "boosted_tree_laplace", "boosted_tree_poisson", "boosted_tree_cox", "boosted_tree_surv",
#                     "boosted_tree_weibull", "boosted_tree_lognormal", "boosted_tree_gehan", "boosted_tree_cindex")){
#     if(purpose=="train") {
#       model_list    <- learner.mbboost.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.mbboost.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp"){
#       dt_vimp       <- learner.mbboost.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.mbboost.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.mbboost.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.mbboost.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="recalibrate") {
#       calibration_model <- learner.mbboost.recalibrate(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.mbboost.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.mbboost.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Extreme gradient boosted trees
#   if(learner %in% c("xgboost_tree", "xgboost_tree_logistic", "xgboost_tree_linear", "xgboost_tree_poisson", "xgboost_tree_gamma", "xgboost_tree_cox")){
#     if(purpose=="train") {
#       model_list    <- learner.xgboost.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.xgboost.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.xgboost.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.xgboost.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.xgboost.outcome(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.xgboost.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="recalibrate") {
#       calibration_model <- learner.xgboost.recalibrate(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.xgboost.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.xgboost.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   # Test learners (not for serious use)
#   if(learner %in% c("__test_invalid", "__test_perfect", "__test_intercept")){
#     if(purpose=="train") {
#       model_list    <- learner.test_learner.train(object=object, data_obj=data_obj)
#     } else if (purpose=="test") {
#       dt_pred       <- learner.test_learner.test(object=object, data_obj=data_obj)
#     } else if (purpose=="vimp") {
#       dt_vimp       <- learner.test_learner.vimp(object=object)
#     } else if (purpose=="parameters"){
#       param         <- learner.test_learner.param(data_obj=data_obj, learner=learner)
#     } else if (purpose=="outcome") {
#       type_is_valid <- learner.test_learner.outcome()
#     } else if (purpose=="prediction_type") {
#       predicts_risk <- learner.test_learner.prediction_type(learner=learner, outcome_type=outcome_type)
#     } else if (purpose=="calibration_info"){
#       calibr_info   <- learner.test_learner.calibration_info(object=object, data_obj=data_obj, time_max=time_max)
#     } else if (purpose=="assess_calibration"){
#       dt_calibr     <- learner.test_learner.get_calibration(object=object, data_obj=data_obj, time_max=time_max)
#     }
#   }
#   
#   
#   # Return objects
#   if(purpose=="train"){
#     # Return trained model and auxiliary data
#     return(model_list)
#     
#   } else if(purpose=="test"){
#     # Return predictions
#     
#     if(extra_output & is.null(dt_pred$predictions)){
#       dt_pred <- list("predictions"=dt_pred)
#     }
#     
#     return(dt_pred)
#     
#   } else if (purpose=="vimp"){
#     # Return variable importance
#     return(dt_vimp)
#     
#   } else if (purpose=="parameters"){
#     # Return hyperparameters
#     return(param)
#     
#   } else if (purpose=="outcome") {
#     # Return whether the outcome is valid for the selected learner
#     # This may be unset if the learner is not specified correctly
#     if(exists("type_is_valid", where=environment())) {
#       return(type_is_valid)
#     } else {
#       return(NULL)
#     }
#     
#   } else if (purpose=="prediction_type") {
#     if(exists("predicts_risk", where=environment())){
#       return(predicts_risk)
#     } else {
#       return(NULL)
#     }
#   } else if (purpose=="recalibrate"){
#     if(exists("calibration_model", where=environment())){
#       return(calibration_model)
#     } else {
#       return(NULL)
#     }
#   } else if (purpose=="calibration_info"){
#     if(exists("calibr_info", where=environment())){
#       return(calibr_info)
#     } else {
#       return(NULL)
#     }
#   } else if(purpose=="assess_calibration"){
#     return(dt_calibr)
#   }
# }



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



learner.check_outcome_type <- function(learner, outcome_type){
  
  # Create familiarModel
  fam_model <- methods::new("familiarModel",
                            learner=learner,
                            outcome_type=outcome_type)
  
  # Set up the specific model
  fam_model <- promote_learner(fam_model)
  
  # Check validity.
  learner_available <- is_available(fam_model)
  
  # Check if the familiar model has been successfuly promoted.
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
  n_samples <- nrow(unique(data_obj@data, by=c("subject_id", "cohort_id")))
  n_features <- get_n_features(data_obj)
  
  # Determine the actual range of features dynamically.
  if(restrict_samples){
    sign_size_range <- c(1, min(n_samples - 1, n_features))
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
