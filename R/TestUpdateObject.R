# Note: the function that creates the archive data is located under
# test/old_experiments.
# Create generator.
test_generate_experiment_parameters <- coro::generator(function(outcome_type){
  
  for(current_outcome_type in outcome_type){
    
    # Set learner
    learner <- switch(current_outcome_type,
                      "binomial"=c("glm_logistic", "lasso"),
                      "multinomial"=c("glm", "lasso"),
                      "count"=c("glm", "lasso"),
                      "continuous"=c("glm_gaussian", "lasso"),
                      "survival"=c("cox", "survival_regr_weibull"))
    
    coro::yield(list("experimental_design"="bs(fs+mb,3)",
                     "outcome_type"=current_outcome_type,
                     "fs_method"=c("mim", "concordance"),
                     "learner"=learner))
  }
})
