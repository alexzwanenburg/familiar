# First test if all selectable learners are also available
familiar:::test_all_vimp_methods_available(vimp_methods=familiar:::.get_available_glmnet_ridge_vimp_methods(show_general=TRUE))
familiar:::test_all_vimp_methods_available(vimp_methods=familiar:::.get_available_glmnet_lasso_vimp_methods(show_general=TRUE))
familiar:::test_all_vimp_methods_available(vimp_methods=familiar:::.get_available_glmnet_elastic_net_vimp_methods(show_general=TRUE))

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_glmnet_ridge_vimp_methods(show_general=FALSE),
                                            debug=TRUE,
                                            parallel=FALSE,
                                            always_available=TRUE)
familiar:::test_all_vimp_methods(vimp_methods=familiar:::.get_available_glmnet_ridge_vimp_methods(show_general=FALSE))

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_glmnet_lasso_vimp_methods(show_general=FALSE),
                                            debug=TRUE,
                                            parallel=FALSE,
                                            always_available=TRUE)
familiar:::test_all_vimp_methods(vimp_methods=familiar:::.get_available_glmnet_lasso_vimp_methods(show_general=FALSE))

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_glmnet_elastic_net_vimp_methods(show_general=TRUE),
                                            debug=TRUE,
                                            parallel=FALSE)
familiar:::test_all_vimp_methods(vimp_methods=familiar:::.get_available_glmnet_elastic_net_vimp_methods(show_general=FALSE),
                                                hyperparameter_list=list("count"=list("alpha"=0.50),
                                                                         "continuous"=list("alpha"=0.50),
                                                                         "binomial"=list("alpha"=0.50),
                                                                         "multinomial"=list("alpha"=0.50),
                                                                         "survival"=list("alpha"=0.50)))
