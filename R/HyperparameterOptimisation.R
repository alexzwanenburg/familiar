run_hyperparameter_optimisation <- function(cl,
                                            project_list,
                                            data_id,
                                            settings,
                                            file_paths,
                                            fs_method,
                                            learner=NULL,
                                            message_indent=0L){

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
  # \code{settings$hpo$hpo_randomise_init_grid}. User-provided hyper-parameter
  # settings are never randomised.
  #
  # If not provided by the user, hyper-parameters and corresponding meta-data
  # are sourced from the learner.

  ################### General initialisation #######################################################
  
  # In absence of a learner, we assume that feature selection is performed.
  is_vimp <- is.null(learner)
  
  # Get directory and file name for the optimised hyper-parameter data
  if(is_vimp){
    dir_path  <- file.path(file_paths$fs_dir)
    file_name_1 <- paste0(project_list$project_id, "_", fs_method, "_hyperparameter_config.RData")
    file_name_2 <- paste0(project_list$project_id, "_", fs_method, "_hyperparameter_config.RDS")
    
  } else {
    dir_path  <- file.path(file_paths$mb_dir, learner, fs_method)
    file_name_1 <- paste0(project_list$project_id, "_hyperparameter_config.RData")
    file_name_2 <- paste0(project_list$project_id, "_hyperparameter_config.RDS")
  }

  # Generate full path to the hyper-parameter optimisation file
  hpo_file_rdata <- normalizePath(file.path(dir_path, file_name_1), mustWork=FALSE)
  hpo_file_rds <- normalizePath(file.path(dir_path, file_name_2), mustWork=FALSE)

  # Check if file already exists and use the parameters stored there
  if(file.exists(hpo_file_rds)){
    return(readRDS(hpo_file_rds))
    
  } else if(file.exists(hpo_file_rdata)){
    load(hpo_file_rdata)
    return(hpo_list)
  }
  
  # Check if the directory exists, and create it otherwise
  if(!dir.exists(dir_path)) dir.create(dir_path, recursive=TRUE)

  # Get the iteration list for the feature selection or model development step
  # for which hyperparameter optimisation is performed.
  hpo_id_list <- getPreprocessingID(run=getRunList(iter_list=project_list$iter_list,
                                                   data_id=data_id,
                                                   run_id=1))
  
  iter_list <- getRunList(iter_list=project_list$iter_list,
                          data_id=hpo_id_list$data)

  # Load functions to cluster
  if(settings$hpo$do_parallel & (length(iter_list) >= length(cl) | length(cl) > 10)){
    # Perform an outer parallellisation.
    outer_parallel <- TRUE
    cl_outer <- cl
    cl_inner <- NULL
    show_progress_bar <- TRUE
    
    logger.message(paste0("Hyperparameter optimisation: Load-balanced parallel processing is done in the outer loop. ",
                          "No progress can be displayed."),
                   indent=message_indent)
    
  } else if(settings$hpo$do_parallel){
    # Perform an inner parallellisation.
    outer_parallel <- FALSE
    cl_outer <- NULL
    cl_inner <- cl
    show_progress_bar <- FALSE
    
  } else {
    outer_parallel <- FALSE
    cl_outer <- NULL
    cl_inner <- NULL
    show_progress_bar <- FALSE
  }

  # Message start of hyperparameter optimisation
  if(is_vimp){
    logger.message(paste("Hyperparameter optimisation: Starting parameter optimisation for the", fs_method,
                         "feature selection method."),
                   indent=message_indent)
  } else {
    logger.message(paste("Hyperparameter optimisation: Starting parameter optimisation for the", learner,
                         "learner, based on variable importances from the", fs_method, "feature selection method."),
                   indent=message_indent)
  }

  # Find optimised hyperparameters and scores by iterating over different
  # primary datasets.
  hpo_list <- fam_mapply_lb(cl=cl_outer,
                            assign="all",
                            FUN=hpo.perform_smbo,
                            run=iter_list,
                            run_id=seq_along(iter_list),
                            progress_bar=show_progress_bar,
                            MoreArgs=list("n_run_total"=length(iter_list),
                                          "cl"=cl_inner,
                                          "fs_method"=fs_method,
                                          "learner"=learner,
                                          "message_indent"=message_indent))
  
  # Adapt list.
  hpo_list <- mapply(FUN=function(run, tuning_list){
    return(list("run_table"=run$run_table,
                "param"=tuning_list$selected_parameters,
                "score"=tuning_list$complete_score))
    
  }, run=iter_list, tuning_list=hpo_list, SIMPLIFY=FALSE)
  
  # Store to disk as RDS file
  saveRDS(object=hpo_list, file=hpo_file_rds)
  
  # Message finish of hyperparameter optimisation
  if(is_vimp){
    logger.message(paste("Hyperparameter optimisation: Completed parameter optimisation for the", fs_method,
                         "feature selection method."),
                   indent=message_indent)
    
  } else {
    logger.message(paste("Hyperparameter optimisation: Completed parameter optimisation for the", learner,
                         "learner, based on variable importances from the", fs_method, "feature selection method."),
                   indent=message_indent)

  }
  
  # Return the hpo_list variable which contains the (optimised) hyperparameters.
  return(hpo_list)
}



hpo.perform_smbo <- function(run, run_id, n_run_total, cl, fs_method, learner=NULL, message_indent=0L){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL
  browser()
  # In absence of a learner, we assume that feature selection is performed.
  is_vimp <- is.null(learner)

  # Message
  logger.message("\n")
  logger.message(paste0("Starting hyperparameter optimisation for run ", run_id, " of ", n_run_total, "."),
                 indent=message_indent)
  
  # Create an empty-placeholder return list.
  empty_return_data <- list("selected_parameters"=NULL,
                            "complete_score"=NULL)
  
  # Get project list, file_paths and settings.
  settings <- get_settings()
  project_list <- get_project_list()
  file_paths <- get_file_paths()
  
  # Get list of feature information objects.
  feature_info_list <- get_feature_info_list(run=run)
  
  
  ##### Create familiarModel or familiarVimpMethod object ----------------------
  
  if(is_vimp){
    
    # Find required features.
    required_features <- find_required_features(features=get_available_features(feature_info_list=feature_info_list,
                                                                                exclude_signature=TRUE),
                                                feature_info_list=feature_info_list)
    
    # Limit to required features. In principle, this removes signature features
    # which are not assessed through variable importance.
    feature_info_list <- feature_info_list[required_features]
    
    # Create the variable importance method object or familiar model object to
    # compute variable importance with. Note that the hyperparameters will be
    # set later.
    fam_model <- promote_vimp_method(object=methods::new("familiarVimpMethod",
                                                         outcome_type=settings$data$outcome_type,
                                                         vimp_method=fs_method,
                                                         outcome_info=.get_outcome_info(),
                                                         feature_info=feature_info_list,
                                                         req_feature_cols=required_features,
                                                         run_table=run$run_table))
    
  } else {
    
    # Find required features. This will be updated once a signature size has been set.
    required_features <- find_required_features(features=get_available_features(feature_info_list=feature_info_list,
                                                                                exclude_signature=FALSE),
                                                feature_info_list=feature_info_list)
    
    # Limit to required features. In principle, this removes signature features
    # which are not assessed through variable importance.
    feature_info_list <- feature_info_list[required_features]
    
    # Create familiar model object. The following need to be updated:
    #
    # * hyperparameters
    # * req_feature_cols
    # * signature
    fam_model <- promote_learner(object=methods::new("familiarModel",
                                                     outcome_type = settings$data$outcome_type,
                                                     learner = learner,
                                                     fs_method = fs_method,
                                                     run_table = run$run_table,
                                                     signature = selected_features,
                                                     req_feature_cols =  required_features,
                                                     feature_info = feature_info_list,
                                                     outcome_info = .get_outcome_info()))
  }
  
  
  ##### Create and process dataObject ------------------------------------------
  
  # Create dataset.
  primary_data <- methods::new("dataObject",
                               data = NULL,
                               preprocessing_level="none",
                               outcome_type = settings$data$outcome_type,
                               delay_loading = TRUE,
                               perturb_level = tail(run$run_table, n=1)$perturb_level,
                               load_validation = FALSE,
                               aggregate_on_load = FALSE,
                               outcome_info = create_outcome_info(settings=settings))
  
  # Pre-process input data.
  primary_data <- process_input_data(object=fam_model,
                                     data=primary_data)
  
  # Check that any data is present.
  if(is_empty(primary_data)) return(empty_return_data)
  
  
  ##### Create and update hyperparameter sets ----------------------------------
  
  # Obtain standard parameters.
  parameter_list <- get_default_hyperparameters(object=fam_model,
                                                data=primary_data)
  
  # Check that any parameters are present.
  if(is_empty(parameter_list)) return(empty_return_data)
  
  # Load initial parameter configurations
  #
  # Two configuration generating algorithms are available. The first
  # (randomise_grid=FALSE) creates a initial grid based on the init_config
  # settings of the respective hyperparameter, and whatever the user provides.
  # The second (randomise_grid=TRUE) creates a grid where the initial values are
  # randomised, however respecting user provided settings and hyperparameters
  # which may not be randomised. The parameter_list list of hyperparameters is
  # first loaded from definitions provided in the source file of the learner.
  # Subsequently, any user-specified settings are loaded and applied. Then
  # configurations are created from the hyper-parameter list.
  if(is_vimp){
    
    # Get the list provided by the user, or generate it so we can fix the
    # sign_size parameter to a fixed value, if required.
    user_list <- settings$fs$param[[fs_method]]
    if(is.null(user_list)) user_list <- list()
    
    # Set the signature size. This parameter may not be used by all feature
    # selection methods, and will be ignored in that case.
    user_list$sign_size <- get_n_features(x=primary_data)
    
  } else {
    # Retrieve any variables set by the user.
    user_list <- settings$mb$hyper_param[[learner]]
  }
  
  # Update the parameter list With user variables.
  parameter_list <- .update_hyperparameters(parameter_list=parameter_list,
                                            user_list=user_list)
  
  # Create initial set of configurations.
  parameter_table <- hpo.create_initial_hyperparameter_set(parameter_list=parameter_list,
                                                          randomise_grid=settings$hpo$hpo_randomise_init_grid)
  
  if(is_empty(parameter_table)) return(empty_return_data)
  
  if(!.any_randomised_hyperparameters(parameter_list=parameter_list)){
    # Remove the param_id column variable.
    parameter_table[ ,":="("param_id"=NULL)]
    
    # Return relevant data
    return(list("selected_parameters"=parameter_table,
                "complete_score"=NULL))
  }
  
  ##### Create metric objects --------------------------------------------------
  
  # Create metric objects.
  metric_object_list <- lapply(settings$hpo$hpo_metric,
                               as_metric,
                               object=fam_model)
  
  # Add baseline values for each metric.
  metric_object_list <- lapply(metric_object_list,
                               set_metric_baseline_value,
                               object=fam_model)
  
  ##### Obtain rank table ------------------------------------------------------
  
  if(!is_vimp){
    # Load rank table (if any)
    rank_table <- rank.get_feature_ranks(run=run,
                                         fs_method=fs_method,
                                         settings=settings,
                                         proj_list=project_list,
                                         file_paths=file_paths)
  } else {
    rank_table <- NULL
  }
  
  # If we arrive at this point, it means that there are optimisable parameters
  # present in the parameter list. Initialise list for hyperparameter
  # configuration evaluations.
  hpo_eval_list <- list()

  ################### SMBO - Initialisation ############################################
  # Generate data bootstrap samples
  hpo_bootstrap_list <- .create_bootstraps(sample_identifiers = primary_data@data$subject_id,
                                           n_iter = settings$hpo$hpo_max_bootstraps,
                                           settings = settings,
                                           data = primary_data@data)

  # Generate a run list from bootstraps
  hpo_run_list <- .add_iteration_to_run(run=run,
                                        train_samples = hpo_bootstrap_list$train_list,
                                        valid_samples = hpo_bootstrap_list$valid_list,
                                        can_pre_process = FALSE,
                                        perturbation = "bootstrap")
  
  # Select hyperparameter bootstraps
  sel_run_id <- fam_sample(x=seq_len(settings$hpo$hpo_max_bootstraps),
                           size=settings$hpo$hpo_bootstraps,
                           replace=FALSE)

  # Set up hyperparameter runs
  hpo_run_table <- data.table::as.data.table(expand.grid(param_id=parameter_table$param_id,
                                                         run_id=sel_run_id,
                                                         KEEP.OUT.ATTRS=FALSE,
                                                         stringsAsFactors=FALSE))

  # Message
  logger.message(paste("Compute initial model performance based on",
                       nrow(hpo_run_table) / settings$hpo$hpo_bootstraps, "hyperparameter sets."),
                 indent=message_indent)
  
  # Build and evaluate models
  hpo_score_table <- hpo.get_model_performance(cl=cl,
                                               object=fam_model,
                                               run_table=hpo_run_table,
                                               run_list=hpo_run_list,
                                               data=primary_data,
                                               rank_table=rank_table,
                                               parameter_table=parameter_table,
                                               metric_objects=metric_object_list,
                                               settings=settings)
  
  # Aggregate to single list
  hpo_score_table <- data.table::rbindlist(hpo_score_table, use.names=TRUE)
  
  pred_list          <- hpo.get_model_performance(cl = cl,
                                                  hpo_run_table = hpo_run_table,
                                                  run_list = hpo_run_list,
                                                  data_obj = data_prmry,
                                                  dt_ranks = dt_ranks,
                                                  dt_param = dt_param,
                                                  feature_info_list = feature_info_list,
                                                  settings = settings,
                                                  learner = learner,
                                                  fs_method = fs_method)

  # Bind list of predictions to data table
  dt_hpo_score       <- metric.get_objective_score(dt=data.table::rbindlist(pred_list), metric=settings$hpo$hpo_metric,
                                                   objective=settings$hpo$hpo_objective, outcome_type=settings$data$outcome_type)

  # Add to configuration evaluation list
  hpo_eval_list      <- append(hpo_eval_list, list(dt_hpo_score))

  # Initialise vector to track old config scores and parameter ids
  stop_list <- list("score"=numeric(0), "param_id"=integer(0), "conv_counter"=0)
  
  # Message progress
  dt_inc    <- hpo.get_best_parameter_set(score_table=dt_hpo_score, objective=settings$hpo$hpo_objective, n=1)
  param_str <- hpo.parse_parameters_to_string(id=dt_inc$param_id, parameter_table=dt_param, parameter_list=param_list)
  logger.message(paste0("\tHyperparameter optimisation: Initialisation complete: ", dt_inc$summ_obj_score, "; ", param_str))
  
  # Clean up
  rm(hpo_run_table, pred_list, sel_run_id, dt_inc, param_str)
  
  smbo_iter     <- 0
  while(smbo_iter < settings$hpo$hpo_smbo_iter_max){

    ################### SMBO - Intensify ############################################

    # Concatenate all evaluations
    dt_hpo_score    <- data.table::rbindlist(hpo_eval_list)

    # Local neighbourhood + random hyperparameter randomisation for challenger configurations
    dt_param_chal   <- hpo.find_challenger_sets(parameter_table=dt_param, score_table=dt_hpo_score, parameter_list=param_list,
                                                smbo_iter=smbo_iter, settings=settings)

    # Perform check - there are no random configurations selected
    if(nrow(dt_param_chal)==0) { break() }

    # Add challenger parameters to the parameter table
    dt_param        <- rbind(dt_param, dt_param_chal, use.names=TRUE)

    # Select only unique parameters
    dt_param        <- unique(dt_param, by="param_id")

    # Determine parameter ids from incumbent and challenger
    param_id_inc    <- hpo.get_best_parameter_set(score_table=dt_hpo_score, objective=settings$hpo$hpo_objective, n=1)$param_id
    param_id_chal   <- dt_param_chal$param_id

    # Drop incumbent parameter id from the list of challengers
    param_id_chal   <- param_id_chal[param_id_chal!=param_id_inc]

    # Check if there are any challengers left
    if(length(param_id_chal)==0) { break() }

    rm(dt_param_chal)

    # Start intensification rounds
    intensify_iter  <- 0
    while(intensify_iter < settings$hpo$hpo_intensify_max_iter){

      # Concatenate all evaluations
      dt_hpo_score  <- data.table::rbindlist(hpo_eval_list)

      # Create run table
      hpo_run_table    <- hpo.create_runoff_run_table(param_id_inc=param_id_inc, param_id_chal=param_id_chal,
                                                   dt_score=dt_hpo_score, n_max=settings$hpo$hpo_max_bootstraps,
                                                   n_new=settings$hpo$hpo_bootstraps)

      # Check if there are any runs to perform
      if(nrow(hpo_run_table) == 0){ break() }

      # Message
      logger.message(paste("\tIntensify step", intensify_iter + 1, "using", length(param_id_chal),
                           "challenger hyperparameter sets."))
      
      # Build and evaluate models
      pred_list     <- hpo.get_model_performance(cl = cl,
                                                 hpo_run_table = hpo_run_table,
                                                 run_list = hpo_run_list,
                                                 data_obj = data_prmry,
                                                 dt_ranks = dt_ranks,
                                                 dt_param = dt_param,
                                                 feature_info_list = feature_info_list,
                                                 settings = settings,
                                                 learner = learner,
                                                 fs_method = fs_method)

      # Bind list of predictions to data table and convert to log-score
      dt_hpo_score  <- metric.get_objective_score(dt=data.table::rbindlist(pred_list), metric=settings$hpo$hpo_metric,
                                                  objective=settings$hpo$hpo_objective, outcome_type=settings$data$outcome_type)

      # Add to configuration evaluation list
      hpo_eval_list <- append(hpo_eval_list, list(dt_hpo_score))

      # Clean up
      rm(hpo_run_table, pred_list, dt_hpo_score)

      # Add scores from all runs
      dt_hpo_score  <- data.table::rbindlist(hpo_eval_list)

      # Find scores and return parameter ids for challenger and incumbent
      # Compared to the original SMAC algorithm, we actively eliminate unsuccesful challengers.
      # To do so we determine the probability that the mean log score of a challenger will not exceed the incumbent score.
      param_id_list <- hpo.compare_runoff_scores(dt_score=dt_hpo_score, param_id_inc=param_id_inc, param_id_chal=param_id_chal,
                                                 objective=settings$hpo$hpo_objective, settings=settings)

      # Extract parameter ids
      param_id_inc  <- param_id_list$param_id_inc
      param_id_chal <- param_id_list$param_id_chal

      # Check if there are challengers remaining
      if(length(param_id_chal)==0) { break() }

      # Update intensify iterator
      intensify_iter     <- intensify_iter + 1

      # Clean up
      rm(dt_hpo_score, param_id_list)
    }

    ################### SMBO - Evaluate ############################################
    # We assess improvement to provide early stopping on non-improving incumbents

    # Get all runs and determine incumbent parameter id
    dt_hpo_score <- data.table::rbindlist(hpo_eval_list)
    dt_inc       <- hpo.get_best_parameter_set(score_table=dt_hpo_score, objective=settings$hpo$hpo_objective, n=1)

    # Update list with stopping criteria
    stop_list    <- hpo.update_stopping_criteria(dt_score=dt_hpo_score, param_id_inc=dt_inc$param_id, stop_list=stop_list, settings=settings)

    # Message progress
    param_str    <- hpo.parse_parameters_to_string(id=param_id_inc, parameter_table=dt_param, parameter_list=param_list)
    logger.message(paste0("\tHyperparameter optimisation: SMBO iteration ", smbo_iter+1, ": score ",
                          dt_inc$summ_obj_score, "; ", param_str))

    # Clean up
    rm(dt_hpo_score, param_id_inc, param_str, dt_inc)

    # Break if the convergence counter reaches a certain number
    if(stop_list$conv_counter >= settings$hpo$hpo_conv_stop){
      # Message convergence
      logger.message(paste0("\tHyperparameter optimisation: Optimisation stopped early as convergence was achieved."))

      # Stop SMBO
      break()
    }

    # Update main iterator
    smbo_iter <- smbo_iter + 1
    ################### SMBO - Wrap-up and report ############################################
  }

  # Get all runs and determine incumbent parameter id
  dt_hpo_score  <- data.table::rbindlist(hpo_eval_list)

  # Select best parameter set
  sel_param_id  <- hpo.get_best_parameter_set(score_table=dt_hpo_score, objective=settings$hpo$hpo_objective, n=1)$param_id

  # Add corresponding hyper parameters and remove redundant columns
  dt_sel_param <- dt_param[param_id==sel_param_id, ]
  dt_sel_param[ ,"param_id":=NULL]

  # Return relevant data
  return(list("param"=dt_sel_param,
              "full_score"=dt_hpo_score))
}



hpo.build_random_forest <- function(score_table, parameter_table){

  # Merge score and parameter data tables on param id.
  joint_table <- merge(x=score_table, y=parameter_table, by="param_id", all=FALSE)

  # Get parameter names
  parameter_names <- colnames(parameter_table)[colnames(parameter_table) != "param_id"]

  # Hyperparameters for the random forest.
  n_tree <- 200
  n_train <- nrow(joint_table)
  sample_fraction <- max(c(0.3, min(c(1, 1/(0.025*n_train)))))

  # Parse formula.
  formula <- stats::reformulate(termlabels=parameter_names, response="obj_score")

  # Train random forest. Note that ranger is imported throught the NAMESPACE.
  rf_model <- ranger(formula, data=joint_table, num.trees=n_tree, num.threads=1,
                     sample.fraction=sample_fraction, verbose=FALSE)

  return(rf_model)
}



hpo.compare_runoff_scores <- function(dt_score, param_id_inc, param_id_chal, objective, settings){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- score_chal <- p_val <- NULL

  # Determine scores and comparison p-values
  dt_comp    <- dt_score[param_id %in% param_id_chal,
                         hpo.compare_challenger_and_incumbent(dt_chal=.SD, dt_inc=dt_score[param_id==param_id_inc, ], objective=objective),
                         by=list(param_id)]

  # Determine if there is a new incumbent score
  if(any(dt_comp$score_chal > dt_comp$score_inc)){
    # If a challenger beats the incumbent, replace the incumbent
    param_id_inc_new  <- head(dt_comp[order(-score_chal)],1)$param_id

    # Remove new incumbent from the challengers and determine remaining challengers by against significance threshold alpha
    param_id_chal_new <- dt_comp[param_id!=param_id_inc_new & p_val > settings$hpo$hpo_alpha, ]$param_id

    # Add incumbent to the challengers
    param_id_chal_new <- append(param_id_chal_new, param_id_inc)
    
  } else {
    # Challengers and incumbent remain and challengers are compared against the significance threshold
    param_id_inc_new  <- param_id_inc
    param_id_chal_new <- dt_comp[p_val > settings$hpo$hpo_alpha, ]$param_id
  }

  return(list("param_id_inc"=param_id_inc_new, "param_id_chal"=param_id_chal_new))
}



hpo.compare_challenger_and_incumbent <- function(dt_chal, dt_inc, objective){

  # Suppress NOTES due to non-standard evaluation in data.table
  run_id <- NULL

  # Find matching bootstrap ids
  run_id_match    <- intersect(dt_chal$run_id, dt_inc$run_id)

  # Calculate mean for challenger and incumbent for the matching bootstraps
  score_chal <- metric.summarise_objective_scores(dt=dt_chal[run_id %in% run_id_match, ], objective=objective, as_vector=TRUE)
  score_inc  <- metric.summarise_objective_scores(dt=dt_inc[run_id %in% run_id_match, ], objective=objective, as_vector=TRUE)


  # Find p-value for matching means using paired wilcoxon rank test
  p_val <- suppressWarnings(stats::wilcox.test(x=dt_chal[run_id %in% run_id_match, ][order(run_id_match)]$obj_score,
                                               y=dt_inc[run_id %in% run_id_match, ][order(run_id_match)]$obj_score,
                                               paired=TRUE, alternative="less")$p.value)

  # Return list with data
  return(list("score_chal"=score_chal, "score_inc"=score_inc, "p_val"=p_val))

}



hpo.create_initial_hyperparameter_set <- function(parameter_list, randomise_grid=TRUE){

  if(length(parameter_list) == 0) { return(NULL) }
  
  if(!.any_randomised_hyperparameters(parameter_list=parameter_list)){
    # All variables have been fixed. No optimisation is required.

    # Get values for each parameter.
    value_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config))
    
    # Convert to a data table.
    parameter_table <- data.table::as.data.table(value_list)
    
  } else if(randomise_grid){
    # Randomised grid

    # Create random hyperparameter sets.
    random_list <- list()
    for(ii in seq_len(200)){
      random_list <- append(random_list, list(hpo.randomise_hyperparameter_set(parameter_list=parameter_list, local=FALSE)))
    }
    
    # Combine list into a single data table.
    parameter_table <- data.table::rbindlist(random_list)
    
  } else {
    # Fixed grid

    # Get initial values for each parameter.
    value_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config))
    
    # Generate a table with all permutations of the grid points.
    parameter_table <- expand.grid(value_list, stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
    parameter_table <- data.table::as.data.table(parameter_table)
    
    # Select up to 100 initial parameter sets.
    selected_row_id <- sample(x = seq_len(nrow(parameter_table)),
                              size = min(c(nrow(parameter_table), 100)),
                              replace = FALSE)
      
    parameter_table <- parameter_table[selected_row_id, ]
  }

  # Allow only unique parameter sets.
  parameter_table <- unique(parameter_table, by=names(parameter_list))
  
  # Add parameter id.
  parameter_table[, "param_id":=.I]

  return(parameter_table)
}


hpo.create_runoff_run_table <- function(param_id_inc, param_id_chal, dt_score, n_max, n_new){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- sampled <- run_id <- to_sample <- sample_id <- NULL

  # Make a copy of dt_score and keep only relevant parameter ids and run ids
  dt_score <- data.table::copy(dt_score)[, c("param_id","run_id")][param_id %in% c(param_id_inc, param_id_chal),][, "sampled":=TRUE]

  # Generate run data table
  dt_run   <- data.table::as.data.table(expand.grid(param_id=c(param_id_inc, param_id_chal), run_id=seq_len(n_max), KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE))

  # Mark those runs that have already been sampled
  dt_run   <- merge(x=dt_run, y=dt_score, by=c("param_id", "run_id"), all.x=TRUE)
  dt_run[is.na(sampled), "sampled":=FALSE]

  # Add a to_sample column to mark new samples to be made
  dt_run[, "to_sample":=FALSE]

  # Find all run ids that have sampled by the incumbent
  run_id_inc  <- unique(dt_run[param_id==param_id_inc & sampled==TRUE, ]$run_id)

  # Find all run ids that have been sampled by challengers
  run_id_chal <- unique(dt_run[param_id %in% param_id_chal & sampled==TRUE, ]$run_id)

  # Determine run ids that have been sampled by challengers, but not by the incumbent
  run_id_inc_new <- run_id_chal[!run_id_chal %in% run_id_inc]

  # Identify if additional new bootstraps should be made
  if(length(run_id_inc_new) < n_new){
    # Sample n_sample new runs
    n_sample <- n_new - length(run_id_inc_new)

    # Determine which run ids have not been sampled by the incumbent
    run_id_inc_unsample <- unique(dt_run[param_id==param_id_inc & sampled==FALSE, ]$run_id)

    # Update n_sample to be the smallest of itself or the number of unsampled runs. This prevents an overflow beyond n_max.
    n_sample <- min(c(n_sample, length(run_id_inc_unsample)))

    # Sample and add to run_id_inc_new
    if(n_sample > 0){
      run_id_inc_new <- append(run_id_inc_new, fam_sample(x=run_id_inc_unsample, size=n_sample, replace=FALSE))
    }
  } else {
    # Sample up to n_new from run_id_inc_new
    run_id_inc_new <- fam_sample(x=run_id_inc_new, size=n_new, replace=FALSE)
  }

  # Add new runs to the incumbent and applicable challengers
  if(length(run_id_inc_new)>0){
    dt_run[run_id %in% run_id_inc_new & sampled==FALSE, "to_sample":=TRUE]
  }

  # Add incumbent runs that have not been performed by the challengers to the sample
  dt_run[param_id %in% param_id_chal & run_id %in% run_id_inc & sampled==FALSE, "to_sample":=TRUE]

  # Select only those runs that are to be sampled
  dt_run <- dt_run[to_sample==TRUE, ]

  # Do not sample more than n_new for challengers
  dt_run[param_id %in% param_id_chal, "sample_id":=sample(x=seq_len(.N), size=.N), by=param_id]
  dt_run <- dt_run[(param_id==param_id_inc) | (param_id %in% param_id_chal & sample_id <= n_new), c("param_id", "run_id")]

  return(dt_run)
}


hpo.expected_improvement <- function(parameter_set, rf_model, incumbent_score){
  # Extract and return predicted mean log score and its standard deviation over the tree ensemble

  # Predict objective scores for every tree using the configuration in the parameter table
  prediction_list <- predict(rf_model, data=parameter_set, predict.all=TRUE, num.threads=1, verbose=FALSE)

  # Calculate predicted mean objective score and its standard deviation over the trees
  m <- mean(prediction_list$predictions)
  s <- stats::sd(prediction_list$predictions)

  # Compute a inverse z-score, using the incumbent score as observed data X.
  if(s > 0){
    z <- (m - incumbent_score) / s
  } else {
    z <- m - incumbent_score
  }

  # The equation for expected improvement in "Hutter, Frank, Holger H. Hoos, and
  # Kevin Leyton-Brown. "Sequential Model-Based Optimization for General
  # Algorithm Configuration." LION 5 (2011): 507-523" is a log improvement. The
  # current equation is based on recent code by the same authors:
  # https://github.com/automl/SMAC3/blob/master/smac/optimizer/acquisition.py
  # The arrangement of m and incumbent_score has been switched, as we expect to
  # maximise incumbent_score, instead of minimising it.
  expected_improvement <- (m - incumbent_score) * stats::pnorm(z) + s * stats::dnorm(z)

  return(expected_improvement)
}


hpo.evaluate_hyperparameters <- function(run,
                                         parameter_table,
                                         object,
                                         data,
                                         rank_table, 
                                         data,
                                         metric_objects,
                                         settings){

  browser()
  # Find parameter id and run id for the current run
  parameter_list <- as.list(parameter_table[, -c("param_id")])
  param_id <- parameter_table$param_id[1]
  run_id <- tail(run$run_table, 1)$run_id[1]

  # Select training data for current run
  data_training <- select_data_from_samples(data=data,
                                            samples=run$train_samples)
  
  if(!is(object, "familiarModel")){
    ..error_reached_unreachable_code("hpo_evaluate_hyperparameters: object is not a familiarModel.")
  }
  
  if(is.null(rank_table)){
    # Update the familiar model (for variable importance)
    object@hyperparameters <- parameter_list
    object@run_table <- run$run_table
    
  } else {
    # Get a signature based on hyperparameters.
    selected_features <- get_signature(feature_info_list=fam_model@feature_info,
                                       dt_ranks=rank_table,
                                       fs_method=fam_model@fs_method,
                                       param=parameter_list,
                                       settings=settings)
    
    # Apply signature to data.
    data_training <- apply_signature(data_obj=data_training,
                                     selected_feat=selected_features)
    
    # Update various slots.
    object@hyperparameters <- parameter_list
    object@signature <- selected_features
    object@run_table <- run$run_table
  }

  # Train model with the set of hyperparameters.
  object <- .train(object=object,
                   data=data_training,
                   get_additional_info=FALSE)
  
  # TODO: GOT HERE
  ############## Assess model performance ##############################

  # Assess performance on the development data
  score_train <- assess_performance(object=fam_model, newdata=data_obj_tr, metric=settings$hpo$hpo_metric,
                                    allow_recalibration=FALSE,
                                    time_max=settings$eval_time_max, as_objective=TRUE, na.rm=FALSE)

  # Clean up
  rm(data_obj_tr, param_list)

  # Get validation data
  data_obj_val <- select_data_from_samples(data=data_obj, samples=run$valid_samples)

  if(!is.null(dt_ranks)){
    # Apply signature to data
    data_obj_val <- apply_signature(data_obj=data_obj_val, selected_feat=sel_feat)
  }
  
  # Assess performance on out-of-bag set
  score_valid <- assess_performance(object=fam_model, newdata=data_obj_val, metric=settings$hpo$hpo_metric,
                                    allow_recalibration=FALSE,
                                    time_max=settings$eval_time_max, as_objective=TRUE, na.rm=FALSE)

  # Create a score data table
  dt_score <- data.table::data.table("param_id"=param_id, "run_id"=run_id, "obj_score_train"=score_train,
                                     "obj_score_valid"=score_valid)

  # Update progress bar
  if(!is.null(pb_conn)){
    utils::setTxtProgressBar(pb=pb_conn, value=run_iter)
  }
  
  # Return score table
  return(dt_score)
}


hpo.find_challenger_sets <- function(parameter_table, score_table, parameter_list, smbo_iter, settings){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- ei <- NULL

  # Set max number of local steps and max number of failure to improve expected improvement
  n_local_steps <- 10
  n_max_no_improv <- 5
  
  # Set number of challengers
  n_challengers <- 20
  
  # Determine the incumbent parameter set.
  incumbent_set <- hpo.get_best_parameter_set(score_table=score_table, objective=settings$hpo$hpo_objective, n=1)
  incumbent_score <- incumbent_set$summ_obj_score
  incumbent_id <- incumbent_set$param_id
  
  # Create a random forest model for all parameters
  rf_model <- hpo.build_random_forest(score_table=score_table, parameter_table=parameter_table)
  
  # Set a flag for local search
  local_search <- ifelse(smbo_iter %% 2 == 0, TRUE, FALSE)
  
  
  ##### Generate random sets ###################################################
  n_random_sets <- 200
  
  # Generate random sets
  random_sets <- lapply(seq_len(n_random_sets), function(ii, parameter_list, rf_model, incumbent_score){
    # Create random configuration
    random_set <- hpo.randomise_hyperparameter_set(parameter_list=parameter_list, local=FALSE)
    
    # Add the expected improvement
    random_set[, "ei":=hpo.expected_improvement(parameter_set=random_set, rf_model=rf_model, incumbent_score=incumbent_score)]
    
    return(random_set)
    
  }, parameter_list=parameter_list, rf_model=rf_model, incumbent_score=incumbent_score)
  
  
  
  ###### Identify local sets ###################################################
  if(local_search){
  
    # Make local copy of parameter table.
    temp_parameter_table <- data.table::copy(parameter_table)
    
    # Compute expected improvement for the sets in the parameter table
    temp_parameter_table[, "ei":=hpo.expected_improvement(parameter_set=.SD, rf_model=rf_model, incumbent_score=incumbent_score), by=param_id]
    
    # Select the 10 best parameter sets
    best_sets <- head(temp_parameter_table[order(-ei)], 10)
    
    # Find local sets
    local_sets <- list()
    
    # Perform a local search
    for(best_param_id in best_sets$param_id){
      
      # Initialise iterator variables
      iter_local_step <- iter_no_improv  <- 0
      
      # Select initial configuration for local search
      selected_set  <- temp_parameter_table[param_id==best_param_id, ]
      
      # Determine the expected improvement for the currently selected set
      selected_set_ei <- selected_set$ei[1]
      
      while(iter_local_step < n_local_steps & iter_no_improv < n_max_no_improv){
        
        # Randomise configuration
        local_random_set <- hpo.randomise_hyperparameter_set(parameter_table=selected_set,
                                                             parameter_list=parameter_list, local=TRUE)
        
        # Compute the expected improvement for the local set
        local_random_set[, "ei":=hpo.expected_improvement(parameter_set=local_random_set, rf_model=rf_model, incumbent_score=incumbent_score)]
        local_random_set_ei <- local_random_set$ei
        
        # Accept new configuration if there is an expected improvement
        if(local_random_set_ei > selected_set_ei){
          
          # Add configuration to list
          local_sets <- append(local_sets, list(local_random_set))
          
          # Continue search from current set
          selected_set <- local_random_set
          selected_set_ei <- local_random_set_ei
          
          # Update iterator
          iter_local_step <- iter_local_step + 1
          
          # Reset iterator for no improvements in ei
          iter_no_improv  <- 0
          
        } else {
          # Increment the failure to improve iterator
          iter_no_improv  <- iter_no_improv + 1
          
        }
      }
    }
  }

  ##### Select challengers #####################################################
  if(local_search){
  
    # Combine original, local and random sets.
    new_sets <- data.table::rbindlist(append(list(temp_parameter_table[, -c("param_id"), with=FALSE]),
                                             append(local_sets,
                                                    random_sets)))

    # Select only unique parameter sets
    new_sets <- unique(new_sets, by=names(parameter_list))
    
    # Merge in original parameter-ids
    new_sets <- merge(x=new_sets, y=parameter_table, by=names(parameter_list), all.x=TRUE, all.y=FALSE)
    
    # Keep all sets except the incumbent, as it cannot have a run-off against itself.
    new_sets <- new_sets[param_id != incumbent_id | is.na(param_id)]
    
    # Sort by expected improvement and select up to 20 best
    challenger_sets <- head(new_sets[(order(-ei))], n_challengers)
    
  } else {

    # Combine random sets into a data.table, but only select unique sets
    new_sets <- unique(data.table::rbindlist(random_sets), by=names(parameter_list))
    
    # Merge in original parameter set ids
    new_sets <- merge(x=new_sets, y=parameter_table, by=names(parameter_list), all.x=TRUE, all.y=FALSE)
    
    # Keep all sets except the incumbent, as it cannot have a run-off against itself.
    new_sets <- new_sets[param_id != incumbent_id | is.na(param_id)]
    
    # Prefer challengers to be parameters that have not been seen.
    challenger_sets <- head(new_sets[is.na(param_id)], n_challengers)
    
    # Pad with challengers that contain parameter sets that were previously visited
    if(nrow(challenger_sets) < n_challengers){
      challenger_sets <- rbind(challenger_sets,
                               head(new_sets[!is.na(param_id)], n=n_challengers-nrow(challenger_sets)))
    }
  }
  
  # Remove expected improvement
  challenger_sets[, "ei":=NULL]
  
  # Provide a new parameter id for new configurations
  if(any(is.na(challenger_sets$param_id))){
    
    # Find the largest existing parameter id
    max_param_id <- max(parameter_table$param_id)
    
    challenger_sets[is.na(param_id), "param_id":= max_param_id + .I]
  }
  
  return(challenger_sets)
}


hpo.get_best_parameter_set <- function(score_table, objective, n=1){
  # Find the best configurations based on onjective score

  # Suppress NOTES due to non-standard evaluation in data.table
  summ_obj_score <- NULL

  # Average objective score over known available in the score table.
  best_sets <- head(metric.summarise_objective_scores(dt=score_table, objective=objective)[order(-summ_obj_score)], n)

  return(best_sets)
}



hpo.get_model_performance <- function(cl,
                                      object,
                                      run_table,
                                      run_list,
                                      data,
                                      rank_table,
                                      parameter_table,
                                      metric_objects,
                                      settings){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL

  # Prepare new hpo_run_list and parameter lists for the mapping operation.
  hpo_run_list <- lapply(run_table$run_id,
                         function(ii, run_list) (run_list[[as.character(ii)]]),
                         run_list=run_list)
  
  parameter_list <- lapply(run_table$param_id,
                           function(ii, parameter_table) (parameter_table[param_id==ii, ]),
                           parameter_table=parameter_table)
  
  prediction_list <- fam_mapply_lb(cl=cl,
                                   assign=NULL,
                                   FUN=hpo.evaluate_hyperparameters,
                                   run=hpo_run_list,
                                   parameter_table=parameter_list,
                                   progress_bar=!settings$hpo$do_parallel,
                                   MoreArgs=list("object"=object,
                                                 "rank_table"=rank_table,
                                                 "metric_objects"=metric_objects,
                                                 "data"=data,
                                                 "settings"=settings))
  
  # Return predictions
  return(prediction_list)
}



hpo.update_stopping_criteria <- function(dt_score, param_id_inc, stop_list, settings){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- obj_score <- NULL

  # Calculate mean, sd and n for current run
  dt_inc  <- dt_score[param_id==param_id_inc,
                      list(mean=metric.summarise_objective_scores(dt=.SD, objective=settings$hpo$hpo_objective, as_vector=TRUE), sd=stats::sd(obj_score), n=.N),
                      by=list(param_id)]

  # Compare incumbent objective score with the minimum
  score_range      <- metric.get_objective_score_range(metric=settings$hpo$hpo_metric, objective=settings$hpo$hpo_objective,
                                                       outcome_type=settings$data$outcome_type)
  above_min        <- dt_inc$mean[1] > min(score_range)

  conv_counter     <- stop_list$conv_counter

  # Assess covergence and stability
  if(length(stop_list$score) >= 4 & above_min){
    # Calculate mean over last 4 incumbent scores and compare with incumbent
    # Note that if the series is converging, the difference between the moving average over the last 4 incumbents
    # and the current incumbent should be positive or 0.
    mov_mean_score <- mean(tail(stop_list$score, 4))
    diff_score     <- dt_inc$mean[1] - mov_mean_score

    # Start counting convergence if:
    #   1. convergence is positive and the rate of convergence has decreased below a fraction of the standard deviation
    #   2. the param_id of the incumbent has not changed over the last iterations
    if(diff_score >= 0  & diff_score < 0.1 * dt_inc$sd[1]){
      conv_counter <- conv_counter + 1
    } else if(all(tail(stop_list$param_id, 4) == dt_inc$param_id[1])) {
      conv_counter <- conv_counter + 1
    } else {
      conv_counter <- 0
    }
  } else {
    conv_counter <- 0
  }

  # Append incumbent score to list of incumbent scores
  stop_list$score        <- append(stop_list$score, dt_inc$mean[1])
  stop_list$param_id     <- append(stop_list$param_id, dt_inc$param_id[1])
  stop_list$conv_counter <- conv_counter

  return(stop_list)
}


hpo.randomise_hyperparameter_set <- function(parameter_table=NULL, parameter_list, local=TRUE){

  if(local & is_empty(parameter_table)){
    ..error_reached_unreachable_code("hpo.randomise_hyperparameter_set_no_values_for_local_search")
  }
  
  if(local){
    # Parameters are locally randomised.

    # Create the updated list from the parameter table.
    updated_list <- as.list(parameter_table)
    
    # Remove param_id and ei from updated list
    updated_list$param_id <- NULL
    updated_list$ei <- NULL
    
    # Select one hyperparameter from the list of randomisable hyperparameters to
    # randomly update.
    random_parameters <- sapply(parameter_list, function(list_entry) (list_entry$randomise))
    selected_parameter <- fam_sample(names(parameter_list)[random_parameters], size=1)

    # Get current value of hyperparameter from dt
    current_parameter_value <- parameter_table[[selected_parameter]]

    # Get type and range of current hyperparameter
    parameter_type <- parameter_list[[selected_parameter]]$type
    parameter_range <- parameter_list[[selected_parameter]]$range

    if(parameter_type %in% c("integer", "numeric")){
      
      if(length(parameter_range) == 2){
        
        # Check that the range is not 0.
        if(max(parameter_range) != min(parameter_range)){
          # Convert to float in [0,1] range
          hp_val_float <- (current_parameter_value - min(parameter_range)) / (max(parameter_range) - min(parameter_range))
          
          # Draw a new random position in [0,1]
          rand_valid <- FALSE
          while(!rand_valid){
            # Draw 20 random numbers from a normal distribution with mean=hp_val_float and sd=0.2
            hp_rand    <- stats::rnorm(20, mean=hp_val_float, sd=0.1)
            rand_valid <- any(hp_rand>=0.0 & hp_rand<=1.0)
          }
          
          # Select new value in [0,1] and convert to original range
          new_hp_val_float <- hp_rand[hp_rand>=0 & hp_rand<=1.0][1]
          new_hp_val_float <- new_hp_val_float * (max(parameter_range) - min(parameter_range)) + min(parameter_range)
          
        } else {
          # Range is 0. Just copy the current parameter value.
          new_hp_val_float <- current_parameter_value
        }
        
      } else {
        # Treat a range such as c(0,1,3) as if only these values can be selected.
        new_hp_val_float <- fam_sample(parameter_range, size=1)
      }

      # Set new value as integer or numeric float
      if(parameter_type=="integer"){
        updated_list[[selected_parameter]] <- as.integer(round(new_hp_val_float))
      } else {
        updated_list[[selected_parameter]] <- new_hp_val_float
      }
      
    } else if(parameter_type %in% c("factor", "logical")){
      # Find range of available options
      available_parameter_values <- parameter_range[parameter_range != current_parameter_value]

      # Randomly select one option
      updated_list[[selected_parameter]] <- fam_sample(available_parameter_values, size=1)
      
    } else {
      ..error_reached_unreachable_code("randomise_hyperparameter_set_unknown_type")
    }
    
  } else {
    # Parameters are globally randomised.
    
    # Generate an updated list from parameter_list for randomisation
    updated_list <- lapply(parameter_list, function(list_entry) (list_entry$init_config[1]))

    # Select hyperparameters to be randomised
    random_parameters <- sapply(parameter_list, function(list_entry) (list_entry$randomise))
    random_parameters <- names(parameter_list)[random_parameters]

    # Iterate over hyperparameters
    for(selected_parameter in random_parameters){
      # Get type and range of current hyperparameter
      parameter_type <- parameter_list[[selected_parameter]]$type
      parameter_range <- parameter_list[[selected_parameter]]$range
      parameter_distribution <- parameter_list[[selected_parameter]]$rand_distr

      if(parameter_type %in% c("integer", "numeric")){
        if(length(parameter_range) == 2){
          if(is.null(parameter_distribution)){
            # Select new value in [0,1] and convert to original range
            hp_rand          <- stats::runif(1)
            new_hp_val_float <- hp_rand * (max(parameter_range) - min(parameter_range)) + min(parameter_range)
            
          } else if(parameter_distribution=="log"){
            # Select new value in [0,1] and convert to log transform of original range
            hp_log_rand      <- stats::runif(1) * (log(max(parameter_range)) - log(min(parameter_range))) + log(min(parameter_range))
            
            # Transform back from log transformation: note this emphasises smaller values
            new_hp_val_float <- exp(hp_log_rand)
          }
          
        } else {
          # Treat a range such as c(0,1,3) as if only these values can be selected.
          new_hp_val_float <- fam_sample(parameter_range, size=1)
        }
        
        # Set new value as integer or numeric float
        if(parameter_type=="integer"){
          updated_list[[selected_parameter]] <- as.integer(round(new_hp_val_float))
        } else {
          updated_list[[selected_parameter]] <- new_hp_val_float
        }
        
      } else if(parameter_type %in% c("factor", "logical")){
        # Randomly select one option
        updated_list[[selected_parameter]] <- fam_sample(parameter_range, size=1)
      }
    }
  }

  # Return a randomised configuration as a data.table
  return(data.table::as.data.table(updated_list))
}



hpo.parse_parameters_to_string <- function(id, parameter_table, parameter_list){

  # Suppress NOTES due to non-standard evaluation in data.table
  param_id <- NULL

  # Initialise an empty string
  parameter_string <- character(0)

  # Iterate through parameter list and identify parameters that are being optimised
  for(current_parameter in names(parameter_list)){
    
    # Check if the current parameter is randomised.
    if(parameter_list[[current_parameter]]$randomise==TRUE){
      # Determine the value of the parameter for the set identified by the id
      # variable.
      optimal_value <- parameter_table[param_id==id, ][[current_parameter]][1]

      # Append to string.
      parameter_string <- append(parameter_string,
                                 paste0(current_parameter, ": ", optimal_value))
    }
  }

  # Concatenate all separate strings into one string.
  parameter_string <- paste(parameter_string, collapse="; ")

  return(parameter_string)
}
