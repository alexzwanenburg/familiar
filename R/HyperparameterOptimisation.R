#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



run_hyperparameter_optimisation <- function(cl=NULL,
                                            project_list=NULL,
                                            data_id=NULL,
                                            settings=NULL,
                                            file_paths=NULL,
                                            vimp_method,
                                            learner=NULL,
                                            message_indent=0L,
                                            verbose=TRUE){
  
  # Hyper-parameters are parameters of a learner, such as the signature size or
  # the number of trees in a random forest. The selection of hyper-parameters
  # influences predictive performance of a model, often to a large extent.
  # Therefore hyper-parameters require optimisation. There are various way to
  # deal with this issue, for example grid search or random search, both of
  # which can be computationally expensive for complex learners. An efficient,
  # successful alternative is using a model-based strategy. In general, building
  # a model using the learner of interest is the most time-consuming step in the
  # process. Therefore, model building should be done only when necessary. The
  # main idea behind model-based hyper-parameter optimisation is that a
  # hyper-parameter model is trained and used to assess random configurations.
  # Then the most interesting configurations, i.e. those which are expected tp
  # provide good objective scores or are located in an unexplored region of the
  # hyperparameter space, are actually build and assessed.
  #
  # The algorithm implemented here is SMAC ("Hutter, Frank, Holger H. Hoos, and
  # Kevin Leyton-Brown. "Sequential Model-Based Optimization for General
  # Algorithm Configuration." LION 5 (2011): 507-523"). The version implemented
  # here varies from the original in the following ways:
  #
  # 1. The intensify step, where promising configurations are compared with the
  # best known configuration, is done in parallel, not sequentially.
  #
  # 2. During the intensify step we assess the probability that a promising
  # challenger configuration is at least as good as the best known
  # configuration. If it is highly unlikely to be better (default: p>=0.05), we
  # skip further intensification with that challenger. The non-parametric
  # Wilcoxon signed-rank test is used to compare objective scores on the same
  # bootstraps of the data.
  #
  # 3. Early stopping criteria are included for converging objective scores and
  # unaltered best configurations over multiple iterations.
  #
  # 4. The initial grid may be randomised by setting
  # \code{settings$hpo$hpo_grid_initialisation_method}. User-provided hyper-parameter
  # settings are never randomised.
  #
  # If not provided by the user, hyper-parameters and corresponding meta-data
  # are sourced from the learner.

  # In absence of a learner, we assume that feature selection is performed.
  is_vimp <- is.null(learner)
  
  if(is.null(project_list)) project_list <- get_project_list()
  if(is.null(settings)) settings <- get_settings()
  if(is.null(file_paths)) file_paths <- get_file_paths()
  
  # Get the iteration list for the feature selection or model development step
  # for which hyperparameter optimisation is performed.
  hpo_id_list <- .get_preprocessing_iteration_identifiers(run=.get_run_list(iteration_list=project_list$iter_list,
                                                                            data_id=data_id,
                                                                            run_id=1L))
  
  iteration_list <- .get_run_list(iteration_list=project_list$iter_list,
                                  data_id=hpo_id_list$data)
  
  # Generate objects for hyperparameter optimisation.
  object_list <- lapply(iteration_list,
                        .create_hyperparameter_optimisation_initial_objects,
                        vimp_method=vimp_method,
                        learner=learner,
                        settings=settings,
                        project_id=project_list$project_id)
  
  # Replace objects that already exist as a file.
  object_list <- lapply(object_list,
                        .collect_hyperparameter_optimisation_completed_objects,
                        vimp_method=vimp_method,
                        learner=learner,
                        file_paths=file_paths)
  
  # Check which objects have already been created.
  object_exist <- sapply(object_list,
                         .exists_hyperparameter_optimisation_object,
                         vimp_method=vimp_method,
                         learner=learner,
                         file_paths=file_paths)
  
  # Return objects as is in case they already exist.
  if(all(object_exist)) return(object_list)
  
  # Determine how parallel processing takes place.
  if(settings$hpo$do_parallel %in% c("TRUE", "inner")){
    cl_inner <- cl
    cl_outer <- NULL
    
  } else if(settings$hpo$do_parallel %in% c("outer")){
    cl_inner <- NULL
    cl_outer <- cl
    
    if(!is.null(cl_outer)) logger.message(paste0("Hyperparameter optimisation: Load-balanced parallel processing is done in the outer loop. ",
                                                 "No progress can be displayed."),
                                          indent=message_indent,
                                          verbose=verbose)
  } else {
    cl_inner <- cl_outer <- NULL
  }
  
  # Message start of hyperparameter optimisation.
  if(is_vimp){
    logger.message(paste("Hyperparameter optimisation: Starting parameter optimisation for the", vimp_method,
                         "variable importance method."),
                   indent=message_indent,
                   verbose=verbose)
  } else {
    logger.message(paste("Hyperparameter optimisation: Starting parameter optimisation for the", learner,
                         "learner, based on variable importances from the", vimp_method, "variable importance method."),
                   indent=message_indent,
                   verbose=verbose)
  }
  
  # Generate experiment info
  experiment_info <- lapply(which(!object_exist), function(ii, n_total) (list("experiment_id"=ii,
                                                                              "n_experiment_total"=n_total)),
                            n_total=length(object_exist))
  
  # Pass to fam_mapply_lb to allow for passing cluster objects to the
  # optimise_hyperparameters method.
  new_object_list <- fam_mapply_lb(cl=cl_outer,
                                   assign="all",
                                   FUN=optimise_hyperparameters,
                                   object=object_list[!object_exist],
                                   experiment_info=experiment_info,
                                   progress_bar=!is.null(cl_outer),
                                   MoreArgs=list("data"=NULL,
                                                 "cl"=cl_inner,
                                                 "user_list"=NULL,
                                                 "metric"=settings$hpo$hpo_metric,
                                                 "optimisation_function"=settings$hpo$hpo_optimisation_function,
                                                 "acquisition_function"=settings$hpo$hpo_acquisition_function,
                                                 "grid_initialisation_method"=settings$hpo$hpo_grid_initialisation_method,
                                                 "n_random_sets"=settings$hpo$hpo_n_grid_initialisation_samples,
                                                 "exploration_method"=settings$hpo$hpo_exploration_method,
                                                 "determine_vimp"=settings$hpo$hpo_determine_vimp,
                                                 "measure_time"=TRUE,
                                                 "hyperparameter_learner"=settings$hpo$hpo_hyperparameter_learner,
                                                 "n_max_bootstraps"=settings$hpo$hpo_max_bootstraps,
                                                 "n_initial_bootstraps"=settings$hpo$hpo_initial_bootstraps,
                                                 "n_intensify_step_bootstraps"=settings$hpo$hpo_bootstraps,
                                                 "n_max_optimisation_steps"=settings$hpo$hpo_smbo_iter_max,
                                                 "n_max_intensify_steps"=settings$hpo$hpo_intensify_max_iter,
                                                 "intensify_stop_p_value"=settings$hpo$hpo_alpha,
                                                 "convergence_tolerance"=settings$hpo$hpo_convergence_tolerance,
                                                 "convergence_stopping"=settings$hpo$hpo_conv_stop,
                                                 "time_limit"=settings$hpo$hpo_time_limit,
                                                 "verbose"=verbose,
                                                 "message_indent"=message_indent+1L,
                                                 "save_in_place"=TRUE,
                                                 "is_vimp"=is.null(learner)))
  
  # Fill in object list.
  object_list[!object_exist] <- new_object_list
  
  # Message completion of hyperparameter optimisation.
  if(is_vimp){
    logger.message("", verbose=verbose)
    logger.message(paste("Hyperparameter optimisation: Completed parameter optimisation for the", vimp_method,
                         "variable importance method."),
                   indent=message_indent,
                   verbose=verbose)
    
  } else {
    logger.message("", verbose=verbose)
    logger.message(paste("Hyperparameter optimisation: Completed parameter optimisation for the", learner,
                         "learner, based on variable importances from the", vimp_method, "variable importance method."),
                   indent=message_indent,
                   verbose=verbose)
  }
  
  # Return the object which contains the (optimised) hyperparameters.
  return(object_list)
}



#####optimise_hyperparameters (ANY, NULL)---------------------------------------
setMethod("optimise_hyperparameters", signature(object="ANY", data="NULL"),
          function(object,
                   data,
                   ...,
                   save_in_place=FALSE,
                   is_vimp=NULL){
            
            # Create dataset on the fly. This is the usual route for
            # summon_familiar.
            data <- methods::new("dataObject",
                                 data = NULL,
                                 preprocessing_level="none",
                                 outcome_type = object@outcome_type,
                                 delay_loading = TRUE,
                                 perturb_level = tail(object@run_table, n=1)$perturb_level,
                                 load_validation = FALSE,
                                 aggregate_on_load = FALSE,
                                 outcome_info = object@outcome_info)
            
            # Make sure the input data is processed.
            data <- process_input_data(object=object,
                                       data=data)
            
            # Check that any data is present.
            if(is_empty(data)) return(object)
            
            # Call proper routine.
            new_object <- do.call(optimise_hyperparameters, args=c(list("object"=object,
                                                                        "data"=data),
                                                                   list(...)))
            
            # Check if the code is called downstream from summon_familiar.
            is_main_process <- !inherits(tryCatch(get_file_paths(), error=identity), "error")
            
            # Save object.
            if(save_in_place & is_main_process){
              .create_hyperparameter_optimisation_directory(object=new_object,
                                                            is_vimp=is_vimp)
              
              file_path <- .get_hyperparameter_optimisation_object_path(object=new_object,
                                                                        is_vimp=is_vimp)
              
              saveRDS(new_object, file=file_path)
            }
            
            return(new_object)
          })



#####optimise_hyperparameters (familiarNoveltyDetector, dataObject)-------------
setMethod("optimise_hyperparameters", signature(object="familiarNoveltyDetector", data="dataObject"),
          function(object,
                   data,
                   user_list=NULL,
                   ...){
            
            # Obtain standard parameters.
            parameter_list <- get_default_hyperparameters(object=object,
                                                          data=data)
            
            # Check that any parameters are present.
            if(is_empty(parameter_list)) return(object)
            
            # Set the user_list if it is not present, or set through
            # hyperparameter attribute.
            if(is.null(user_list) & is.null(object@hyperparameters)){
              user_list <- list()
              
            } else if(is.null(user_list) & !is.null(object@hyperparameters)){
              user_list <- object@hyperparameters
            }
            
            # Recreate the default parameter list with information from the
            # user-provided list, if any. This allows for changing some
            # hyperparameter settings that depend on other hyperparameters.
            parameter_list <- get_default_hyperparameters(object=object,
                                                          data=data,
                                                          user_list=user_list)
            
            # Update the parameter list With user-defined variables.
            parameter_list <- .update_hyperparameters(parameter_list=parameter_list,
                                                      user_list=user_list)
            
            if(.any_randomised_hyperparameters(parameter_list=parameter_list)){
              ..error_reached_unreachable_code("optimise_hyperparameters,familiarNoveltyDetector,dataObject: unset hyperparameters are present, but not expected.")
            }
            
            # Update hyperparameters to set any fixed parameters.
            object@hyperparameters <- lapply(parameter_list, function(list_entry) list_entry$init_config)
            
            return(object)
          })



#####optimise_hyperparameters (familiarVimpMethod, dataObject)------------------
setMethod("optimise_hyperparameters", signature(object="familiarVimpMethod", data="dataObject"),
          function(object,
                   data,
                   user_list=NULL,
                   ...){
            
            # Obtain standard parameters.
            parameter_list <- get_default_hyperparameters(object=object,
                                                          data=data)
            
            # Check that any parameters are present.
            if(is_empty(parameter_list)) return(object)
            
            # Set the user_list if it is not present, or set through
            # hyperparameter attribute.
            if(is.null(user_list) & is.null(object@hyperparameters)){
              user_list <- list()
              
            } else if(is.null(user_list) & !is.null(object@hyperparameters)){
              user_list <- object@hyperparameters
            }
            
            # Set the signature size. This parameter may not be used by all
            # feature selection methods, and will be ignored in that case.
            user_list$sign_size <- get_n_features(x=data)
            
            # Recreate the default parameter list with information from the
            # user-provided list, if any. This allows for changing some
            # hyperparameter settings that depend on other hyperparameters.
            parameter_list <- get_default_hyperparameters(object=object,
                                                          data=data,
                                                          user_list=user_list)
            
            # Update the parameter list With user-defined variables.
            parameter_list <- .update_hyperparameters(parameter_list=parameter_list,
                                                      user_list=user_list)
            
            if(.any_randomised_hyperparameters(parameter_list=parameter_list)){
              ..error_reached_unreachable_code("optimise_hyperparameters,familiarVimpMethod,dataObject: unset hyperparameters are present, but not expected.")
            }
            
            # Update hyperparameters to set any fixed parameters.
            object@hyperparameters <- lapply(parameter_list, function(list_entry) list_entry$init_config)
            
            return(object)
          })


#####optimise_hyperparameters (familiarModel, dataObject)-----------------------
setMethod("optimise_hyperparameters", signature(object="familiarModel", data="dataObject"),
          function(object,
                   data,
                   cl=NULL,
                   experiment_info=NULL,
                   user_list=NULL,
                   metric=waiver(),
                   optimisation_function="validation",
                   acquisition_function="expected_improvement",
                   grid_initialisation_method="fixed_subsample",
                   exploration_method="successive_halving",
                   n_random_sets=100L,
                   determine_vimp=TRUE,
                   measure_time=TRUE,
                   hyperparameter_learner="gaussian_process",
                   n_max_bootstraps=20L,
                   n_initial_bootstraps=1L,
                   n_intensify_step_bootstraps=3L,
                   n_max_optimisation_steps=20L,
                   n_max_intensify_steps=5L,
                   n_challengers=20L,
                   intensify_stop_p_value=0.05,
                   convergence_tolerance=NULL,
                   convergence_stopping=3,
                   no_decent_model_stopping=4,
                   time_limit=NULL,
                   verbose=TRUE,
                   message_indent=0L,
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            param_id <- .NATURAL <- NULL
            
            if(!is.null(experiment_info)){
              logger.message("", verbose=verbose)
              logger.message(paste0("Starting hyperparameter optimisation for data subsample ",
                                    experiment_info$experiment_id, " of ",
                                    experiment_info$n_experiment_total, "."),
                             indent=message_indent,
                             verbose=verbose)
            }
            
            # Set default metric
            if(is.waive(metric)) metric <- .get_default_metric(outcome_type=object@outcome_type)
            
            # Set convergence tolerance, if it has not been set.
            if(is.null(convergence_tolerance)){
              # Get the number of samples.
              n_samples <- get_n_samples(data, "series")
              
              # Nothing to do if there are no samples.
              if(n_samples == 0) return(object)
              
              # Set convergence tolerance.
              convergence_tolerance <- -2 + (-log10(n_samples) + 2) / 2
              
              # Limit between -2 and -3.
              if(convergence_tolerance > -2) convergence_tolerance <- -2
              if(convergence_tolerance < -3) convergence_tolerance <- -3
              
              # Convert to value.
              convergence_tolerance <- 10^convergence_tolerance
            }
            
            # Check if the metric is ok. Packed into a for loop to enable
            # multi-metric optimisation.
            sapply(metric, metric.check_outcome_type, outcome_type=object@outcome_type)
            
            # Check if acquisition_function is correctly specified.
            .check_parameter_value_is_valid(x=acquisition_function,
                                            var_name="acquisition_function",
                                            values=.get_available_acquisition_functions())
            
            # Check if hyperparameter learner is correctly specified.
            .check_parameter_value_is_valid(x=hyperparameter_learner,
                                            var_name="hyperparameter_learner",
                                            values=.get_available_hyperparameter_learners())
            
            # Check if optimisation_function is correctly specified.
            .check_parameter_value_is_valid(x=optimisation_function,
                                            var_name="optimisation_function",
                                            values=.get_available_optimisation_functions(hyperparameter_learner=hyperparameter_learner))
            
            # Check if exploration_method is correctly specified.
            .check_parameter_value_is_valid(x=exploration_method,
                                            var_name="exploration_method",
                                            values=.get_available_hyperparameter_exploration_methods())
            
            # Report on methodology used.
            optimisation_description <- switch(
              optimisation_function,
              "max_validation"="maximising out-of-bag performance",
              "validation"="maximising out-of-bag performance",
              "balanced"="maximising out-of-bag performance while constraining performance differences between in-bag and out-of-bag data",
              "stronger_balance"="maximising out-of-bag performance while strongly constraining performance differences between in-bag and out-of-bag data",
              "validation_minus_sd"="maximising out-of-bag performance while penalising variance using the lower 1 standard deviation bound",
              "validation_25th_percentile"="maximising out-of-bag performance while penalising variance using the 25th percentile bound",
              "model_estimate"="maximising performance estimates from the hyperparameter model",
              "model_estimate_minus_sd"="maximising performances estimates while penalising variance estimated by the model",
              "model_balanced_estimate"="maximising performance under estimates of the performance differences between in-bag and out-of-bag data",
              "model_balanced_estimate_minus_sd"="maximising performance under estimates of the performance differences between in-bag and out-of-bag data while penalising estimated variance in performance differences"
            )
            
            logger.message(paste0("Hyperparameter optimisation is conducted using the ", paste_s(metric), ifelse(length(metric) > 1, " metrics", " metric"),
                                  " by ", optimisation_description, "."),
                           indent=message_indent+1L,
                           verbose=verbose)
            
            inference_description <- switch(
              hyperparameter_learner,
              "random_forest"="selected after inferring utility using a Random Forest",
              "gaussian_process"="selected after inferring utility using a localised approximate Gaussian Process",
              "bayesian_additive_regression_trees"="selected after inferring utility using Bayesian Additive Regression Trees",
              "bart"="selected after inferring utility using Bayesian Additive Regression Trees",
              "random"="drawn randomly",
              "random_search"="drawn randomly"
            )
            
            logger.message(paste0("Candidate hyperparameter sets after the initial run are ", inference_description, "."),
                           indent=message_indent+1L,
                           verbose=verbose)
            
            if(!hyperparameter_learner %in% c("random", "random_search")){
              utility_description <- switch(
                acquisition_function,
                "improvement_probability"="probability of improvement over the best known hyperparameter set",
                "improvement_empirical_probability"="empirical probability of improvement over the best known hyperparameter set",
                "expected_improvement"="expected improvement",
                "upper_confidence_bound"="the upper regret bound",
                "bayes_upper_confidence_bound"="the Bayesian upper confidence bound"
              )
              
              logger.message(paste0("Utility is measured as ", utility_description, "."),
                             indent=message_indent+1L,
                             verbose=verbose)
            }
            
            ##### Create and update hyperparameter sets ------------------------
            
            # Set the user_list if it is not present, or set through
            # hyperparameter attribute.
            if(is.null(user_list) & is.null(object@hyperparameters)){
              user_list <- list()
              
            } else if(is.null(user_list) & !is.null(object@hyperparameters)){
              user_list <- object@hyperparameters
            }
            
            # Create the default parameter list with information from the
            # user-provided list, if any. This allows for changing some
            # hyperparameter settings that depend on other hyperparameters.
            parameter_list <- get_default_hyperparameters(object=object,
                                                          data=data,
                                                          user_list=user_list)
            
            # Check that any parameters are present.
            if(is_empty(parameter_list)) return(object)
            
            # Update the parameter list With user-defined variables.
            parameter_list <- .update_hyperparameters(
              parameter_list=parameter_list,
              user_list=user_list,
              n_features=ifelse(object@fs_method %in% .get_available_no_features_vimp_methods(),
                                0L,
                                get_n_features(data))
            )
            
            # Check that any parameters can be randomised.
            if(!.any_randomised_hyperparameters(parameter_list=parameter_list)){
              
              # Update hyperparameters to set any fixed parameters.
              object@hyperparameters <- lapply(parameter_list, function(list_entry) list_entry$init_config)
              
              logger.message("Hyperparameter optimisation: All hyperparameters are fixed. No optimisation is required.",
                             indent=message_indent,
                             verbose=verbose)
              
              return(object)
            }
            
            ##### Create bootstrap samples -------------------------------------
            
            # Generate bootstrap samples
            bootstraps <- tryCatch(.create_bootstraps(n_iter=n_max_bootstraps,
                                                      outcome_type=object@outcome_type,
                                                      data=data@data),
                                   error=identity)
            
            # Check that bootstraps could be created. This may fail if the data
            # set is too small.
            if(inherits(bootstraps, "error")){
              logger.message("Hyperparameter optimisation: Failed to create bootstraps. The dataset may be too small.",
                             indent=message_indent,
                             verbose=verbose)
              
              # Remove any fixed hyperparameters.
              object@hyperparameters <- NULL
              
              return(object)
            } 
            
            ##### Create or obtain variable importance -------------------------
            rank_table_list <- .compute_hyperparameter_variable_importance(
              cl=cl,
              determine_vimp=determine_vimp,
              object=object,
              data=data,
              bootstraps=bootstraps$train_list,
              metric=metric,
              measure_time=measure_time,
              optimisation_function=optimisation_function,
              acquisition_function=acquisition_function,
              grid_initialisation_method=grid_initialisation_method,
              n_random_sets=min(c(n_random_sets, 50L)),
              n_max_bootstraps=min(c(n_max_bootstraps, 20L)),
              n_max_optimisation_steps=min(c(n_max_optimisation_steps, 5L)),
              n_max_intensify_steps=min(c(n_max_intensify_steps, 3L)),
              n_intensify_step_bootstraps=min(c(n_intensify_step_bootstraps, 5L)),
              intensify_stop_p_value=intensify_stop_p_value,
              convergence_tolerance=min(c(convergence_tolerance, 1E-2)),
              convergence_stopping=min(c(convergence_stopping, 3L)),
              time_limit=time_limit,
              verbose=verbose,
              message_indent=message_indent
            )
            
            ##### Set signature size -------------------------------------------
            # Signature size depends on the variable importance method that is
            # used.
            
            if(!is.null(parameter_list$sign_size)){
              
              # Set signature size.
              user_list$sign_size <- .set_signature_size(object=object,
                                                         rank_table_list=rank_table_list,
                                                         suggested_range=user_list$sign_size)
              
              # Update the parameter list With user-defined variables.
              parameter_list <- .update_hyperparameters(parameter_list=parameter_list,
                                                        user_list=user_list,
                                                        n_features=get_n_features(data))
              
              # Check that any parameters can be randomised.
              if(!.any_randomised_hyperparameters(parameter_list=parameter_list)){
                
                # Update hyperparameters to set any fixed parameters.
                object@hyperparameters <- lapply(parameter_list, function(list_entry) list_entry$init_config)
                
                logger.message("Hyperparameter optimisation: All hyperparameters are fixed. No optimisation is required.",
                               indent=message_indent,
                               verbose=verbose)
                
                return(object)
              }
            }
            
            
            ##### Create metric objects ----------------------------------------
            
            # Update the outcome_info attribute of the familiar model. This is
            # required to set the metric baseline value.
            object@outcome_info <- .compute_outcome_distribution_data(object=object@outcome_info,
                                                                      data=data)
            
            # Create metric objects.
            metric_object_list <- lapply(metric,
                                         as_metric,
                                         object=object)
            
            # Add baseline values for each metric.
            metric_object_list <- lapply(metric_object_list,
                                         set_metric_baseline_value,
                                         object=object,
                                         data=data)
            
            
            ##### Create initial set of randomised hyperparameters and bootstraps -------------
            
            # Update the parameter list to make sure that categorical variables
            # are encoded as factors.
            parameter_list <- .encode_categorical_hyperparameters(parameter_list)
            
            # Create initial set of configurations.
            parameter_table <- ..create_initial_hyperparameter_set(parameter_list=parameter_list,
                                                                   grid_initialisation_method=grid_initialisation_method,
                                                                   n_random_sets=n_random_sets)
            
            # Check that the parameter table is not empty.
            if(is_empty(parameter_table)){
              logger.message("Hyperparameter optimisation: No hyperparameters were found to initialise the optimisation process.",
                             indent=message_indent,
                             verbose=verbose)
              
              # Remove any fixed hyperparameters.
              object@hyperparameters <- NULL
              
              return(object)
            } 
            
            
            ##### Setup the hyperparameter model prototype ---------------------
            
            # Set up the model prototype.
            optimisation_model_prototype <- methods::new("familiarHyperparameterLearner",
                                                         learner = hyperparameter_learner,
                                                         target_learner = object@learner,
                                                         target_outcome_type = object@outcome_type,
                                                         optimisation_metric = metric,
                                                         optimisation_function = optimisation_function)
            
            ##### Perform initial set of computations --------------------------
            
            # Initialise the process clock.
            process_clock <- NULL
            if(!is.null(time_limit)){
              if(require_package(c("callr", "microbenchmark"),
                                 purpose="to measure hyperparameter optimisation time",
                                 message_type="warning")){
                
                process_clock <- ProcessClock$new()
                on.exit(process_clock$close(), add=TRUE)
                
              } else {
                # If the required packages are not present, avoid early
                # abortion of the process.
                time_limit <- NULL
              }
            }
                
            
            if(measure_time & n_initial_bootstraps > 1L) {
              
              # Start with an initial run to measure performance and time.
              run_table <- ..create_hyperparameter_run_table(run_ids=1L,
                                                             parameter_ids=parameter_table$param_id)

              # Message begin.
              logger.message(paste("Compute initial model performance based on",
                                   nrow(run_table), "hyperparameter sets."),
                             indent=message_indent+1L,
                             verbose=verbose)
              
              # Build and evaluate models. This creates a table with metric
              # values, objective scores for in-bag and out-of-bag data.
              score_results <- .compute_hyperparameter_model_performance(
                cl=cl,
                object=object,
                run_table=run_table,
                bootstraps=bootstraps,
                data=data,
                rank_table_list=rank_table_list,
                parameter_table=parameter_table,
                metric_objects=metric_object_list,
                iteration_id=0L,
                verbose=verbose
              )
              
              # Get score table.
              score_table <- score_results$results
              
              # Get errors encountered during model training.
              train_errors <- score_results$error
              
              if(.optimisation_process_time_available(process_clock=process_clock,
                                                      time_limit=time_limit,
                                                      verbose=FALSE)){
                
                # Set up the runs for the remaining initial computations.
                run_table <- ..create_hyperparameter_run_table(run_ids=setdiff(seq_len(n_initial_bootstraps), 1L),
                                                               measure_time=measure_time,
                                                               score_table=score_table,
                                                               parameter_table=parameter_table,
                                                               optimisation_model=optimisation_model_prototype,
                                                               n_max_bootstraps=n_max_bootstraps)
                
                logger.message(paste0("Compute initial model performance based on the second batch of ",
                                      nrow(run_table), " hyperparameter sets."),
                               indent=message_indent+1L,
                               verbose=verbose)
                
                # Create a model to predict the time a process takes to complete
                # training.
                time_optimisation_model <- .create_hyperparameter_time_optimisation_model(score_table=score_table,
                                                                                          parameter_table=parameter_table,
                                                                                          optimisation_function=optimisation_function)
                
                # Compute second batch of results.
                score_results <- .compute_hyperparameter_model_performance(cl=cl,
                                                                           object=object,
                                                                           run_table=run_table,
                                                                           bootstraps=bootstraps,
                                                                           data=data,
                                                                           rank_table_list=rank_table_list,
                                                                           parameter_table=parameter_table,
                                                                           metric_objects=metric_object_list,
                                                                           iteration_id=0L,
                                                                           time_optimisation_model=time_optimisation_model,
                                                                           overhead_time=score_results$overhead_time,
                                                                           verbose=verbose)
                
                # Add new data to score and optimisation tables.
                score_table <- rbind(score_table,
                                     score_results$results,
                                     use.names=TRUE)
                
                # Add errors encountered during model training.
                train_errors <- c(train_errors, score_results$error)
              }
              
            } else {
              # Set up hyperparameter experiment runs
              run_table <- ..create_hyperparameter_run_table(run_ids=seq_len(n_initial_bootstraps),
                                                             parameter_ids=parameter_table$param_id)
              
              logger.message(paste0("Compute initial model performance based on ",
                                    nrow(run_table), " hyperparameter sets."),
                             indent=message_indent+1L,
                             verbose=verbose)
              
              # Build and evaluate models. This creates a table with metric
              # values, objective scores for in-bag and out-of-bag data.
              score_results <- .compute_hyperparameter_model_performance(cl=cl,
                                                                         object=object,
                                                                         run_table=run_table,
                                                                         bootstraps=bootstraps,
                                                                         data=data,
                                                                         rank_table_list=rank_table_list,
                                                                         parameter_table=parameter_table,
                                                                         iteration_id=0L,
                                                                         metric_objects=metric_object_list,
                                                                         verbose=verbose)
              
              # Get score table.
              score_table <- score_results$results
              
              # Get errors encountered during model training.
              train_errors <- score_results$error
            }
            
            # Find information regarding the dataset that has the highest
            # optimisation score.
            incumbent_set <- get_best_hyperparameter_set(score_table=score_table,
                                                         parameter_table=parameter_table,
                                                         optimisation_model=optimisation_model_prototype,
                                                         n_max_bootstraps=n_max_bootstraps,
                                                         n=1L)
            
            # Message the user concerning the initial optimisation score.
            logger.message(paste0("Hyperparameter optimisation: Initialisation complete: ",
                                  ..parse_optimisation_summary_to_string(
                                    parameter_set=incumbent_set,
                                    parameter_table=parameter_table,
                                    parameter_list=parameter_list)),
                           indent=message_indent+1L,
                           verbose=verbose)
            
            # Check whether any combination yielded anything valid.
            skip_optimisation <- incumbent_set$summary_score <= ..get_replacement_optimisation_score()
            
            # Update n_intensify_step_bootstraps and n_max_intensify_steps when
            # exploration_method equals none. There is no reason to perform
            # steps sequentially as there is no pruning.
            if(exploration_method == "none"){
              n_intensify_step_bootstraps <- n_intensify_step_bootstraps * n_max_intensify_steps
              n_max_intensify_steps <- 1L
              
            } else if(exploration_method == "single_shot"){
              n_max_intensify_steps <- 1L
              n_intensify_step_bootstraps <- 1L
            }
            
            # Create list with stopping criteria based on initial runs.
            stop_list <- ..update_hyperparameter_optimisation_stopping_criteria(set_data=incumbent_set,
                                                                                stop_data=NULL,
                                                                                tolerance=convergence_tolerance)
            
            optimisation_step <- 0L
            while(optimisation_step < n_max_optimisation_steps & !skip_optimisation){
              
              # Stop optimisation if there is no time left. This is an initial
              # check because the nested loop may break for different reasons.
              # This check is not verbose, pending a later, final check.
              if(!.optimisation_process_time_available(process_clock=process_clock,
                                                       time_limit=time_limit,
                                                       verbose=FALSE)){
                break()
              }
              
              ##### SMBO - Intensify -------------------------------------------
              
              # Create a model to predict the time a process takes to complete
              # training.
              time_optimisation_model <- .create_hyperparameter_time_optimisation_model(score_table=score_table,
                                                                                        parameter_table=parameter_table,
                                                                                        optimisation_function=optimisation_function)
              
              # Local neighbourhood + random hyperparameter randomisation for
              # challenger configurations. This selects challengers
              # combinations.
              challenger_data <- .create_hyperparameter_challenger_sets(score_table=score_table,
                                                                        parameter_list=parameter_list,
                                                                        parameter_table=parameter_table,
                                                                        time_optimisation_model=time_optimisation_model,
                                                                        score_optimisation_model=optimisation_model_prototype,
                                                                        acquisition_function=acquisition_function,
                                                                        n_max_bootstraps=n_max_bootstraps,
                                                                        smbo_iter=optimisation_step,
                                                                        measure_time=measure_time,
                                                                        n_challengers=n_challengers)
              
              # Check that any challenger datasets were found. 
              if(is_empty(challenger_data)) break()
              
              # Add challenger parameters to the parameter table
              parameter_table <- rbind(parameter_table,
                                       challenger_data,
                                       use.names=TRUE)
              
              # Select only unique parameters
              parameter_table <- unique(parameter_table,
                                        by="param_id")
              
              # Determine parameter ids from incumbent and challenger
              parameter_id_incumbent <- incumbent_set$param_id
              parameter_id_challenger <- challenger_data$param_id
              
              # Drop incumbent parameter id from the list of challengers
              parameter_id_challenger <- setdiff(parameter_id_challenger, parameter_id_incumbent)
              
              # Check if there are any challengers left
              if(length(parameter_id_challenger) == 0) break()
              
              # Start intensification rounds
              n_intensify_steps  <- 0L
              while(n_intensify_steps < n_max_intensify_steps){
                
                # Check if there is time left for intensification.
                if(!.optimisation_process_time_available(process_clock=process_clock,
                                                         time_limit=time_limit,
                                                         verbose=FALSE)){
                  break()
                }
                
                # Create run table.
                run_table <- ..create_hyperparameter_intensify_run_table(parameter_id_incumbent=parameter_id_incumbent,
                                                                         parameter_id_challenger=parameter_id_challenger,
                                                                         score_table=score_table,
                                                                         n_max_bootstraps=n_max_bootstraps,
                                                                         n_intensify_step_bootstraps=n_intensify_step_bootstraps,
                                                                         exploration_method=exploration_method)
                
                # Check if there are any runs to perform.
                if(is_empty(run_table)) break()
                
                # Message the user.
                logger.message(paste("Intensify step", n_intensify_steps + 1L, "using", length(parameter_id_challenger),
                                     "challenger hyperparameter sets."),
                               indent=message_indent+1L,
                               verbose=verbose)
                
                # Compute metric values for the bootstraps of the incumbent and
                # challenger parameter sets.
                score_results <- .compute_hyperparameter_model_performance(cl=cl,
                                                                           object=object,
                                                                           run_table=run_table,
                                                                           bootstraps=bootstraps,
                                                                           data=data,
                                                                           rank_table_list=rank_table_list,
                                                                           parameter_table=parameter_table,
                                                                           metric_objects=metric_object_list,
                                                                           iteration_id=optimisation_step + 1L,
                                                                           time_optimisation_model=time_optimisation_model,
                                                                           overhead_time=score_results$overhead_time,
                                                                           verbose=verbose)
                
                # Get score table.
                # Add new data to score and optimisation tables.
                score_table <- rbind(score_table,
                                     score_results$results,
                                     use.names=TRUE)
                
                # Add errors encountered during model training.
                train_errors <- c(train_errors, score_results$error)
                
                # Find scores and return parameter ids for challenger and
                # incumbent hyperparameter sets. Compared to the original SMAC
                # algorithm, we actively eliminate unsuccessful challengers. To
                # do so we determine the probability that the optimisation score
                # of a challenger does not exceed the incumbent score.
                runoff_parameter_ids <- .compare_hyperparameter_sets(score_table=score_table,
                                                                     parameter_table=parameter_table,
                                                                     optimisation_model=optimisation_model_prototype,
                                                                     parameter_id_incumbent=parameter_id_incumbent,
                                                                     parameter_id_challenger=parameter_id_challenger,
                                                                     exploration_method=exploration_method,
                                                                     intensify_stop_p_value=intensify_stop_p_value)
                
                # Extract hyperparameter set identifiers.
                parameter_id_incumbent <- runoff_parameter_ids$parameter_id_incumbent
                parameter_id_challenger <- runoff_parameter_ids$parameter_id_challenger
                
                # Check if there are challengers remaining
                if(length(parameter_id_challenger) == 0) break()
                
                # Update intensify iterator
                n_intensify_steps <- n_intensify_steps + 1L
                
                # Update the model that predicts the time a process takes to
                # complete training.
                time_optimisation_model <- .create_hyperparameter_time_optimisation_model(
                  score_table=score_table,
                  parameter_table=parameter_table,
                  optimisation_function=optimisation_function
                )
              }
              
              ##### SMBO - Evaluate --------------------------------------------
              # We assess improvement to provide early stopping on non-improving
              # incumbents.
              
              # Get all runs and compute details for the incumbent set.
              incumbent_set <- get_best_hyperparameter_set(
                score_table=score_table,
                parameter_table=parameter_table,
                optimisation_model=optimisation_model_prototype,
                n_max_bootstraps=n_max_bootstraps
              )
              
              # Update list with stopping criteria
              stop_list <- ..update_hyperparameter_optimisation_stopping_criteria(
                set_data=incumbent_set,
                stop_data=stop_list,
                tolerance=convergence_tolerance
              )
              
              # Message progress.
              logger.message(paste0("Hyperparameter optimisation: SMBO iteration ", optimisation_step + 1L, ": ",
                                    ..parse_optimisation_summary_to_string(
                                      parameter_set=incumbent_set,
                                      parameter_table=parameter_table,
                                      parameter_list=parameter_list)),
                             indent=message_indent+1L,
                             verbose=verbose)
              
              # Check time and finish the optimisation process if required. This
              # will also automatically generate a message if verbose is TRUE.
              if(!.optimisation_process_time_available(process_clock=process_clock,
                                                       time_limit=time_limit,
                                                       message_indent=message_indent,
                                                       verbose=verbose)){
                break()
              }
              
              # Break if the convergence counter reaches a certain number
              if(stop_list$convergence_counter_score >= convergence_stopping | 
                 stop_list$convergence_counter_parameter_id >= convergence_stopping){
                
                # Message convergence
                logger.message(paste0("Hyperparameter optimisation: Optimisation stopped early as convergence was achieved."),
                               indent=message_indent,
                               verbose=verbose)
                
                # Stop SMBO
                break()
              }
              
              # Break if no model better than the naive model was found.
              if(stop_list$no_naive_improvement_counter >= no_decent_model_stopping){
                
                # Message early stopping.
                logger.message(paste0("Hyperparameter optimisation: Optimisation stopped early because no models were found to perform better than naive models."),
                               indent=message_indent,
                               verbose=verbose)
                
                # Stop SMBO.
                break()
              }
              
              # Update main iterator
              optimisation_step <- optimisation_step + 1L
            }
            ##### SMBO - Wrap-up and report-------------------------------------
            
            # Get all runs and determine incumbent parameter id.
            incumbent_set <- get_best_hyperparameter_set(score_table=score_table,
                                                         parameter_table=parameter_table,
                                                         optimisation_model=optimisation_model_prototype,
                                                         n_max_bootstraps=n_max_bootstraps)
            
            # Add corresponding hyper parameters and remove redundant columns.
            optimal_set_table <- parameter_table[param_id==incumbent_set$param_id, ]
            optimal_set_table[ ,"param_id":=NULL]
            
            # Check that a suitable set of hyperparameters was found.
            if(incumbent_set$summary_score <= ..get_replacement_optimisation_score()){
              # In this case, no suitable hyperparameters were found, for
              # whatever reason.
              
              logger.message(paste0("Hyperparameter optimisation: No suitable set of hyperparameters was found."),
                             indent=message_indent,
                             verbose=verbose)
              
              # Report on errors.
              if(!is.null(train_errors)){
                # Generate a summary of the errors.
                train_errors <- condition_summary(train_errors)
                
                # Notify that one or more errors were encountered.
                logger.message(paste0("Hyperparameter optimisation: The following ",
                                      ifelse(length(train_errors) == 1, "error was", "errors were"),
                                      " encountered while training models:"),
                               indent=message_indent,
                               verbose=verbose)
                
                # Show errors.
                sapply(train_errors, logger.message, indent=message_indent+1, verbose=verbose)
              }
              
              # Set NULL.
              object@hyperparameters <- NULL
              
            } else if(incumbent_set$validation_score < 0.0 & !object@fs_method %in% c("none", "signature_only")){
              # In this case, no set of hyperparameters found that led to a
              # model that was better than the naive model. We then train a
              # naive model instead, by forcing sign_size to 0.
              
              object@hyperparameters <- as.list(optimal_set_table)
              object@hyperparameters$sign_size <- 0
              
              logger.message(paste0("Hyperparameter optimisation: No set of hyperparameters was identified that ",
                                    "leads to a model that performs better than a naive model."),
                             indent=message_indent,
                             verbose=verbose)
              
            } else {
              # In this case the model trained properly.
              object@hyperparameters <- as.list(optimal_set_table)
              
              logger.message(paste0("Hyperparameter optimisation: A suitable set of hyperparameters was identified: ",
                                    ..parse_optimisation_summary_to_string(
                                      parameter_set=incumbent_set,
                                      parameter_table=parameter_table,
                                      parameter_list=parameter_list)),
                             indent=message_indent,
                             verbose=verbose)
            }
            
            time_taken <- NULL
            if(is(process_clock, "processClock")) time_taken <- process_clock$time(units="mins")
            
            # Update attributes of object.
            object@hyperparameter_data <- list(
              "score_table"=score_table,
              "parameter_table"=parameter_table,
              "time_taken"=time_taken,
              "metric"=metric,
              "metric_object"=metric_object_list,
              "hyperparameter_learner"=hyperparameter_learner,
              "optimisation_function"=optimisation_function,
              "n_samples"=get_n_samples(data),
              "n_features"=get_n_features(data)
            )
            
            return(object)
          })



.create_hyperparameter_optimisation_initial_objects <- function(run,
                                                                vimp_method,
                                                                learner=NULL,
                                                                settings,
                                                                project_id){
  # Obtain feature information list.
  feature_info_list <- .get_feature_info_list(run=run)
  
  if(is.null(learner)){
     
    # Create the variable importance met hod object or familiar model object to
    # compute variable importance with.
    object <- promote_vimp_method(object=methods::new("familiarVimpMethod",
                                                      outcome_type=settings$data$outcome_type,
                                                      hyperparameters=settings$fs$param[[vimp_method]],
                                                      vimp_method=vimp_method,
                                                      outcome_info=.get_outcome_info(),
                                                      run_table=run$run_table,
                                                      project_id=project_id))
    
    # Set multivariate methods.
    if(is(object, "familiarModel")) is_multivariate <- TRUE
    if(is(object, "familiarVimpMethod")) is_multivariate <- object@multivariate
    
    # Find required features.
    required_features <- get_required_features(x=feature_info_list,
                                               exclude_signature=!is_multivariate)
    
    # Limit to required features. In principle, this removes signature features
    # which are not assessed through variable importance.
    feature_info_list <- feature_info_list[required_features]
    
    # Update the object.
    object@required_features <- required_features
    object@feature_info <- feature_info_list
    
  } else {
    
    # Create familiar model object. The following need to be updated:
    object <- promote_learner(object=methods::new("familiarModel",
                                                  outcome_type = settings$data$outcome_type,
                                                  hyperparameters = settings$mb$hyper_param[[learner]],
                                                  learner = learner,
                                                  fs_method = vimp_method,
                                                  run_table = run$run_table,
                                                  outcome_info = .get_outcome_info(),
                                                  settings = settings$eval,
                                                  project_id = project_id))
    
    # Find required features.
    required_features <- get_required_features(x=feature_info_list,
                                               exclude_signature=FALSE)
    
    # Limit to required features. In principle, this removes signature features
    # which are not assessed through variable importance.
    feature_info_list <- feature_info_list[required_features]
    
    # Update the object.
    object@required_features <- required_features
    object@feature_info <- feature_info_list
  }
  
  return(object)
}



.create_hyperparameter_optimisation_directory <- function(object,
                                                          is_vimp,
                                                          file_paths=NULL){
  # Set file paths.
  if(is.null(file_paths)) file_paths <- get_file_paths()
  
  if(is(object, "familiarVimpMethod")){
    vimp_method <- object@vimp_method
    
  } else if(is_vimp){
    vimp_method <- object@learner
    
  } else {
    vimp_method <- object@fs_method
  }
  
  # Set created hyperparameters.
  if(is_vimp){
    dir_path <- file.path(file_paths$fs_dir, vimp_method)
    
  } else {
    dir_path <- file.path(file_paths$mb_dir, object@learner, vimp_method)
  }
  
  # Create directory if it does not exist.
  if(!dir.exists(dir_path)) dir.create(dir_path, recursive=TRUE)
}



.collect_hyperparameter_optimisation_completed_objects <- function(object,
                                                                   vimp_method,
                                                                   learner,
                                                                   file_paths){
  
  # Identify file path to object.
  file_path <- .get_hyperparameter_optimisation_object_path(object=object,
                                                            is_vimp=is.null(learner),
                                                            file_paths=file_paths)
  
  if(file.exists(file_path)) object <- readRDS(file_path)
  
  return(object)
}



.exists_hyperparameter_optimisation_object <- function(object,
                                                       vimp_method,
                                                       learner,
                                                       file_paths){
  # Identify file path to object.
  file_path <- .get_hyperparameter_optimisation_object_path(object=object,
                                                            is_vimp=is.null(learner),
                                                            file_paths=file_paths)
  
  return(file.exists(file_path))
}



.get_hyperparameter_optimisation_object_path <- function(object,
                                                         is_vimp,
                                                         file_paths=NULL){
  
  if(is.null(file_paths)) file_paths <- get_file_paths()
  
  # Extract data and run id
  object_data_id <- tail(object@run_table, n=1)$data_id
  object_run_id  <- tail(object@run_table, n=1)$run_id
  
  # Get the variable importance method.
  if(is_vimp & is(object, "familiarVimpMethod")){
    vimp_method <- object@vimp_method
    
  } else if(is_vimp){
    vimp_method <- object@learner
    
  } else {
    vimp_method <- object@fs_method
  }
  
  if(is_vimp){
    dir_path  <- file.path(file_paths$fs_dir, vimp_method)
    file_name <- paste0(object@project_id, "_hyperparameters_", vimp_method, "_", object_data_id, "_", object_run_id, ".RDS")
    
  } else {
    dir_path  <- file.path(file_paths$mb_dir, object@learner, vimp_method)
    file_name <- paste0(object@project_id, "_hyperparameters_", object@learner, "_", vimp_method, "_", object_data_id, "_", object_run_id, ".RDS")
  }
  
  file_path <- normalizePath(file.path(dir_path, file_name), mustWork=FALSE)
  
  return(file_path)
}



.find_hyperparameters_for_run <- function(run,
                                          hpo_list,
                                          allow_random_selection=FALSE,
                                          as_list=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL
  
  # Identify the right entry on hpo_list.
  for(ii in rev(run$run_table$perturb_level)){
    run_id_list <- .get_iteration_identifiers(run=run, perturb_level=ii)
    
    # Check whether there are any matching data and run ids by determining the
    # number of rows in the table after matching
    match_hpo <- sapply(hpo_list, function(iter_hpo, run_id_list){
      
      # Determine if there are any rows in the run_table of the parameter list
      # that match the data and run identifiers of the current level.
      match_size <- nrow(iter_hpo@run_table[data_id==run_id_list$data & run_id==run_id_list$run])
      
      # Return TRUE if any matching rows are found.
      return(match_size > 0)
      
    }, run_id_list=run_id_list)
    
    # If there is a match, we step out of the loop
    if(any(match_hpo)) break()
  }
  
  # Extract the table of parameters
  if(allow_random_selection & sum(match_hpo) > 1){
    random_set <- sample(which(match_hpo), size=1)
    
    object <- hpo_list[[random_set]]
    
  } else {
    object <- hpo_list[match_hpo][[1]]
  }
  
  if(as_list){
    return(object@hyperparameters)
    
  } else {
    return(object)
  }
}
