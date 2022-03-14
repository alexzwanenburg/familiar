#' familiar: Fully Automated Machine Learning with Interpretable Analysis of Results
#'
#' End-to-end, automated machine learning package for creating 
#' trustworthy and interpretable machine learning models. Familiar supports
#' modelling of regression, categorical and time-to-event (survival) outcomes.
#' Models created using familiar are self-containing, and their use does not
#' require additional information such as baseline survival, feature
#' clustering, or feature transformation and normalisation parameters. In
#' addition, an novelty or out-of-distribution detector is trained
#' simultaneously and contained with every model. Model performance,
#' calibration, risk group stratification, (permutation) variable importance,
#' individual conditional expectation, partial dependence, and more, are
#' assessed automatically as part of the evaluation process and exported in
#' tabular format and plotted, and may also be computed manually using export
#' and plot functions. Where possible, metrics and values obtained during the
#' evaluation process come with confidence intervals.
#'
#' @docType package
#' @name familiar
#' @import data.table
#' @import methods
#' @importClassesFrom rstream rstream.runif
#' @importFrom stats predict coef vcov
#' @importFrom survival Surv coxph survreg
#' @importFrom utils head tail
#' @importFrom rlang quo quos enquo enquos sym syms ensym ensyms parse_expr parse_exprs
"_PACKAGE"



#' Perform end-to-end machine learning and data analysis
#'
#' @param formula An R formula. The formula can only contain feature names and
#'   dot (`.`). The `*` and `+1` operators are not supported as these refer to
#'   columns that are not present in the data set.
#'   
#'   Use of the formula interface is optional.
#'
#' @param data A `data.table` object, a `data.frame` object, list containing
#'   multiple `data.table` or `data.frame` objects, or paths to data files.
#'
#'   `data` should be provided if no file paths are provided to the `data_files`
#'   argument. If both are provided, only `data` will be used.
#'
#'   All data is expected to be in wide format, and ideally has a sample
#'   identifier (see `sample_id_column`), batch identifier (see `cohort_column`)
#'   and outcome columns (see `outcome_column`).
#'
#'   In case paths are provided, the data should be stored as `csv`, `rds` or
#'   `RData` files. See documentation for the `data_files` argument for more
#'   information.
#'
#' @param cl Cluster created using the `parallel` package. This cluster is then
#'   used to speed up computation through parallelisation. When a cluster is not
#'   provided, parallelisation is performed by setting up a cluster on the local
#'   machine.
#'
#'   This parameter has no effect if the `parallel` argument is set to `FALSE`.
#' @param config List containing configuration parameters, or path to an `xml`
#'   file containing these parameters. An empty configuration file can obtained
#'   using the `get_xml_config` function.
#'
#'   All parameters can also be set programmatically. These supersede any
#'   arguments derived from the configuration list.
#'
#' @param config_id Identifier for the configuration in case the list or `xml`
#'   table indicated by `config` contains more than one set of configurations.
#' @param verbose Indicates verbosity of the results. Default is TRUE, and all
#'   messages and warnings are returned.
#'
#' @inheritDotParams .parse_file_paths -config -verbose
#' @inheritDotParams .parse_experiment_settings -config
#' @inheritDotParams .parse_setup_settings -config
#' @inheritDotParams .parse_preprocessing_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_feature_selection_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_model_development_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_hyperparameter_optimisation_settings -config -parallel -outcome_type
#' @inheritDotParams .parse_evaluation_settings -config -data -parallel -outcome_type -hpo_metric -development_batch_id -vimp_aggregation_rank_threshold -vimp_aggregation_method -prep_cluster_method -prep_cluster_linkage_method -prep_cluster_cut_method -prep_cluster_similarity_threshold -prep_cluster_similarity_metric
#'
#' @return Nothing. All output is written to the experiment directory. If the 
#'   experiment directory is in a temporary location, a list with all
#'   familiarModel, familiarEnsemble, familiarData and familiarCollection
#'   objects will be returned.
#'
#' @inherit .parse_general_settings references
#'
#' @export
#' @md
summon_familiar <- function(formula=NULL,
                            data=NULL,
                            cl=NULL,
                            config=NULL,
                            config_id=1,
                            verbose=TRUE,
                            ...){
  
  # Set options.
  # Disable randomForestSRC OpenMP core use.
  options(rf.cores=as.integer(1))
  on.exit(options(rf.cores=-1L), add=TRUE)
  
  # Disable multithreading on data.table to prevent reduced performance due to
  # resource collisions with familiar parallelisation.
  data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(0L), add=TRUE)
  

  ##### Load configuration file ------------------------------------------------
  config <- .load_configuration_file(config=config,
                                     config_id=config_id)
  
  
  ##### Test arguments provided by ... and config ------------------------------
  .check_dots_is_parameter(dots=names(list(...)))
  .check_configuration_tag_is_parameter(config=config)
  
  
  ##### File paths -------------------------------------------------------------
  # Parse file paths
  file_paths  <- do.call(.parse_file_paths,
                         args=c(list("config"=config,
                                     "verbose"=verbose),
                                list(...)))
  
  # Set paths to data
  if(!is.null(file_paths$data) & is.null(data)){
    data <- file_paths$data
  }
  
  # Make sure to remove the directory if it is on the temporary path.
  if(file_paths$is_temporary) on.exit(unlink(file_paths$experiment_dir, recursive=TRUE), add=TRUE)
  
  ##### Load data --------------------------------------------------------------
  # Parse experiment and data settings
  settings <- .parse_initial_settings(config=config, ...)
  
  if(is(data, "dataObject")){
    # Reconstitute settings from the data.
    new_settings <- extract_settings_from_data(data)
    
    # Replace settings with new settings.
    settings$data[names(new_settings$data)] <- new_settings$data
    
    # Extract data as a data.table.
    data <- data@data
    
  } else {
    # Load data.
    data <- do.call(.load_data,
                    args=c(list("data"=data),
                           settings$data))
    
    # Update settings
    settings <- .update_initial_settings(formula=formula,
                                         data=data,
                                         settings=settings)
  }
  
  # Parse data
  data <- .finish_data_preparation(data = data,
                                   sample_id_column = settings$data$sample_col,
                                   batch_id_column = settings$data$batch_col,
                                   series_id_column = settings$data$series_col,
                                   outcome_column = settings$data$outcome_col,
                                   outcome_type = settings$data$outcome_type,
                                   include_features = settings$data$include_features,
                                   class_levels = settings$data$class_levels,
                                   censoring_indicator=settings$data$censoring_indicator,
                                   event_indicator=settings$data$event_indicator,
                                   competing_risk_indicator=settings$data$competing_risk_indicator)

  # Derive experimental design
  experiment_setup <- extract_experimental_setup(experimental_design=settings$data$exp_design,
                                                 file_dir=file_paths$iterations_dir,
                                                 verbose=verbose)
  
  # Check experiment settings
  settings <- .update_experimental_design_settings(section_table=experiment_setup,
                                                   data=data,
                                                   settings=settings)
  
  # Import remaining settings
  settings <- .parse_general_settings(config=config,
                                      data=data,
                                      settings=settings,
                                      ...)
  
  # Create a generic outcome object
  outcome_info <- create_outcome_info(settings=settings)
  
  # Define iterations etc.
  project_info <- .get_iteration_data(file_dir=file_paths$iterations_dir,
                                      data=data,
                                      experiment_setup=experiment_setup,
                                      settings=settings,
                                      verbose=verbose)
  
  # In case the iterations are loaded from a iterations file provided by the
  # user, perform some checks on the experimental design given the current data
  # set.
  if(is.waive(experiment_setup)){
    # Replace experiment_setup with the setup that was loaded from the
    # iterations file.
    experiment_setup <- project_info$experiment_setup
    
    # Check experiment settings for the current dataset.
    settings <- .update_experimental_design_settings(section_table=experiment_setup,
                                                     data=data,
                                                     settings=settings)
  }
  
  # Generate feature info
  feature_info_list <- .get_feature_info_data(data=data,
                                              file_paths=file_paths,
                                              project_id=project_info$project_id,
                                              outcome_type=settings$data$outcome_type)

  # Identify if an external cluster is provided, and required.
  if(settings$run$parallel){
    is_external_cluster <- inherits(cl, "cluster")
    
  } else {
    is_external_cluster <- FALSE
  }
  
  # Assign objects that should be accessible everywhere to the familiar global
  # environment. Note that .assign_data_to_backend will also start backend
  # server processes.
  .assign_settings_to_global(settings=settings)
  .assign_file_paths_to_global(file_paths=file_paths)
  .assign_project_info_to_global(project_info=project_info)
  .assign_outcome_info_to_global(outcome_info=outcome_info)
  .assign_backend_options_to_global(backend_type=settings$run$backend_type,
                                    server_port=settings$run$server_port)
  .assign_data_to_backend(data=data,
                          backend_type=settings$run$backend_type,
                          server_port=settings$run$server_port)
  .assign_feature_info_to_backend(feature_info_list=feature_info_list,
                                  backend_type=settings$run$backend_type,
                                  server_port=settings$run$server_port)
  .assign_parallel_options_to_global(is_external_cluster=is_external_cluster,
                                     restart_cluster=settings$run$restart_cluster,
                                     n_cores=settings$run$parallel_nr_cores,
                                     cluster_type=settings$run$cluster_type)
  
  # Make sure that backend server will close after the process finishes.
  on.exit(shutdown_backend_server(backend_type=settings$run$backend_type,
                                  server_port=settings$run$server_port),
          add=TRUE)
  
  if(settings$run$parallel & !settings$run$restart_cluster & !is_external_cluster){
    # Start local cluster in the overall process.
    cl <- .restart_cluster(cl=NULL, assign="all")
    on.exit(.terminate_cluster(cl), add=TRUE)
    
  } else if(settings$run$parallel & settings$run$restart_cluster & !is_external_cluster){
    # Start processes locally.
    cl <- waiver()
    
  } else if(!settings$run$parallel){
    # No cluster is created when 
    cl <- NULL
  }
  
  # Clean familiar environment on exit. This is run last to avoid cleaning up
  # the familiar environment prior to shutting down the socket server process.
  on.exit(.clean_familiar_environment(), add=TRUE)
  
  # Start feature selection
  run_feature_selection(cl=cl,
                        project_list=project_info,
                        settings=settings,
                        file_paths=file_paths,
                        verbose=verbose)
  
  # Start model building
  run_model_development(cl=cl,
                        project_list=project_info,
                        settings=settings,
                        file_paths=file_paths,
                        verbose=verbose)
  
  # Start evaluation
  run_evaluation(cl=cl,
                 proj_list=project_info,
                 settings=settings,
                 file_paths=file_paths,
                 verbose=verbose)
  
  if(file_paths$is_temporary){
    # Collect all familiarModels, familiarEnsemble, familiarData and
    # familiarCollection objects.
    familiar_list <- .import_all_familiar_objects(file_paths=file_paths)
    
    # Return list with objects
    return(familiar_list)
    
  } else {
    return(invisible(TRUE))
  }
}



#' Create models using end-to-end machine learning
#'
#' @description Train models using familiar. Evaluation is not performed.
#'
#' @param experimental_design (**required**) Defines what the experiment looks
#'   like, e.g. `cv(bt(fs,20)+mb,3,2)` for 2 times repeated 3-fold
#'   cross-validation with nested feature selection on 20 bootstraps and
#'   model-building. The basic workflow components are:
#'
#'   * `fs`: (required) feature selection step.
#'
#'   * `mb`: (required) model building step.
#'
#'   * `ev`: (optional) external validation. Setting this is not required for
#'   `train_familiar`, but if validation batches or cohorts are present in the
#'   dataset (`data`), these should be indicated in the `validation_batch_id`
#'   argument.
#'
#'   The different components are linked using `+`.
#'
#'   Different subsampling methods can be used in conjunction with the basic
#'   workflow components:
#'
#'   * `bs(x,n)`: (stratified) .632 bootstrap, with `n` the number of
#'   bootstraps. In contrast to `bt`, feature pre-processing parameters and
#'   hyperparameter optimisation are conducted on individual bootstraps.
#'
#'   * `bt(x,n)`: (stratified) .632 bootstrap, with `n` the number of
#'   bootstraps. Unlike `bs` and other subsampling methods, no separate
#'   pre-processing parameters or optimised hyperparameters will be determined
#'   for each bootstrap.
#'
#'   * `cv(x,n,p)`: (stratified) `n`-fold cross-validation, repeated `p` times.
#'   Pre-processing parameters are determined for each iteration.
#'
#'   * `lv(x)`: leave-one-out-cross-validation. Pre-processing parameters are
#'   determined for each iteration.
#'
#'   * `ip(x)`: imbalance partitioning for addressing class imbalances on the
#'   data set. Pre-processing parameters are determined for each partition. The
#'   number of partitions generated depends on the imbalance correction method
#'   (see the `imbalance_correction_method` parameter).
#'
#'   As shown in the example above, sampling algorithms can be nested.
#'
#'   The simplest valid experimental design is `fs+mb`. This is the default in
#'   `train_familiar`, and will create one model for each feature selection
#'   method in `fs_method`. To create more models, a subsampling method should
#'   be introduced, e.g. `bs(fs+mb,20)` to create 20 models based on bootstraps
#'   of the data.
#'
#' @param learner (**required**) Name of the learner used to develop a model. A
#'   sizeable number learners is supported in `familiar`. Please see the
#'   vignette on learners for more information concerning the available
#'   learners. Unlike the `summon_familiar` function, `train_familiar` only
#'   allows for a single learner.
#' @param hyperparameter (*optional*) List, or nested list containing
#'   hyperparameters for learners. If a nested list is provided, each sublist
#'   should have the name of the learner method it corresponds to, with list
#'   elements being named after the intended hyperparameter, e.g.
#'   \code{"glm_logistic"=list("sign_size"=3)}
#'
#'   All learners have hyperparameters. Please refer to the vignette on learners
#'   for more details. If no parameters are provided, sequential model-based
#'   optimisation is used to determine optimal hyperparameters.
#'
#'   Hyperparameters provided by the user are never optimised. However, if more
#'   than one value is provided for a single hyperparameter, optimisation will
#'   be conducted using these values.
#'
#' @inheritParams summon_familiar
#' @inheritDotParams .parse_experiment_settings -config
#' @inheritDotParams .parse_setup_settings -config
#' @inheritDotParams .parse_preprocessing_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_feature_selection_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_model_development_settings -config -data -parallel -outcome_type
#' @inheritDotParams .parse_hyperparameter_optimisation_settings -config -parallel -outcome_type
#'
#' @details This is a thin wrapper around `summon_familiar`, and functions like
#'   it, but automatically skip all evaluation steps. Only a single learner is
#'   allowed.
#'
#' @return One or more familiarModel objects.
#'   
#' @export
#' @md
train_familiar <- function(formula=NULL,
                           data=NULL,
                           cl=NULL,
                           experimental_design="fs+mb",
                           learner=NULL,
                           hyperparameter=NULL,
                           verbose=TRUE,
                           ...){
  
  # Check that a single learner is present.
  learner <- .parse_arg(x_config=NULL,
                        x_var=learner,
                        var_name="learner",
                        type="character",
                        optional=FALSE)
  
  # Hyperparameters may be interpreted as belonging to the specified learner.
  hyperparameter <- .parse_arg(x_config=NULL,
                               x_var=hyperparameter,
                               var_name="hyperparameter",
                               type="list",
                               optional=TRUE,
                               default=list())
  
  # Encode hyperparameter as expected by parsing it to a nested list.
  if(length(hyperparameter) > 0 & is.null(hyperparameter[[learner]])){
    hyperparameter_list <- list()
    hyperparameter_list[[learner]] <- hyperparameter
    hyperparameter <- hyperparameter_list
  }
  
  # Isolate dots.
  dots <- list(...)
  
  # Drop skip_na, project_dir, experiment_dir, and config if present.
  dots$skip_evaluation_elements <- NULL
  dots$project_dir <- NULL
  dots$experiment_dir <- NULL
  
  # Summon a familiar.
  familiar_models <- do.call(summon_familiar, args=(c(list("formula"=formula,
                                                           "data"=data,
                                                           "cl"=cl,
                                                           "experimental_design"=experimental_design,
                                                           "learner"=learner,
                                                           "hyperparameter"=hyperparameter,
                                                           "experiment_dir"=NULL,
                                                           "project_dir"=NULL,
                                                           "skip_evaluation_elements"="all",
                                                           "verbose"=verbose),
                                                      dots)))
  # Extract familiar models.
  return(familiar_models$familiarModel)
}



.is_absolute_path <- function(x){
  return(dir.exists(paste0(unlist(strsplit(x, split=.Platform$file.sep))[1], .Platform$file.sep)))
}



.load_configuration_file <- function(config, config_id=1){
  if(!is.null(config)){
    
    # Load as file path
    if(is.character(config)){
      if(length(config) > 1){
        stop(paste("Configuration: the path to the configuration file is expected",
                   "to be a single character string. Multiple strings were found."))
      }
      
      # Normalise file paths
      config <- normalizePath(config)
      
      # Check that the xml2 package is installed
      require_package("xml2", "to configure familiar using a configuration file")
      
      # Read xml file, parse to list and remove comments
      config <- xml2::as_list(xml2::read_xml(config))[[1]][[config_id]]
      config <- .clean_configuration_comments(config=config)
      
    } else {
      if(!is.list(config) || !is.recursive(config)){
        stop("Configuration: the input configuration data is not a list of lists.")
      }
    }
  } else {
    config <- NULL
  }
  
  return(config)
}



.clean_configuration_comments <- function(config){
  
  cleaning_cycle <- function(conf_list){
    # Nested function for iterating through configuration list
    
    # If the current position is no longer a list, skip the following steps
    if(is.list(conf_list)){
      
      # Retain only those list entries that are not comments "[ comment ]".
      conf_list <- Filter(Negate(function(x) ((is.character(x[1]) & x[1]=="[ comment ]"))), conf_list)
      
      # Go one level deeper by applying this function to the list entries of the current list
      conf_list <- lapply(conf_list, cleaning_cycle)
    }
    
    return(conf_list)
  }
  
  # Remove comments by running the iterative cleaningCycle script
  config_new <- cleaning_cycle(conf_list=config)
  
  return(config_new)
}


#' Create an empty xml configuration file
#'
#' This function creates an empty configuration xml file in the directory
#' specified by `dir_path`. This provides an alternative to the use of input
#' arguments for familiar.
#'
#' @param dir_path Path to the directory where the configuration file should be
#'  created. The directory should exist, and no file named `config.xml` should
#'  be present.
#'
#' @return Nothing. A file named `config.xml` is created in the directory
#' indicated by `dir_path`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Creates a config.xml file in the working directory
#' get_xml_config(dir_path=getwd())
#' }
#' @md
#' @keywords IO
get_xml_config <- function(dir_path){
  
  file.copy(from=system.file("config.xml", package="familiar"), to=dir_path,
            overwrite=FALSE)
  invisible()
}



.assign_settings_to_global <- function(settings){
  # Assign settings into the familiar global, environment
  assign("settings", settings, envir=familiar_global_env)
}



get_settings <- function(){
  
  # Retrieve settings from the backend
  if(exists("familiar_global_env")){
    if(exists("settings", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("settings", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("Settings not found in backend.")
    }
  } else if (exists("settings", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("Settings not found in backend.")
  }
  
  return(get("settings", envir=data_env))
}



.assign_file_paths_to_global <- function(file_paths){
  # Put file_paths into the familiar environment
  assign("file_paths", file_paths, envir=familiar_global_env)
}



get_file_paths <- function(){
  
  # Retrieve the paths to files and directories
  if(exists("familiar_global_env")){
    if(exists("file_paths", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("file_paths", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("File paths were not found in backend.")
    }
  } else if (exists("file_paths", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("File paths were not found in backend.")
  }
  
  return(get("file_paths", envir=data_env))
}



.assign_project_info_to_global <- function(project_info){
  # Put project_info_list into the familiar environment
  assign("project_info_list", project_info, envir=familiar_global_env)
}



get_project_list <- function(){
  
  # Retrieve the project list
  if(exists("familiar_global_env")){
    if(exists("project_info_list", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("project_info_list", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("Project list not found in backend.")
    }
  } else if (exists("project_info_list", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("Project list not found in backend.")
  }
  
  return(get("project_info_list", envir=data_env))
}


.import_all_familiar_objects <- function(file_paths){
  familiar_list <- list()
  
  # Find familiarModel files
  model_files <- list.files(path=file_paths$mb_dir, pattern="model.RDS", recursive=TRUE)
  model_files <- sapply(model_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$mb_dir)
  
  # Load familiarModel files and add to list
  familiar_list$familiarModel <- load_familiar_object(model_files)
  
  
  # Find familiarEnsemble files
  ensemble_files <- list.files(path=file_paths$mb_dir, pattern="ensemble.RDS", recursive=TRUE)
  ensemble_files <- sapply(ensemble_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$mb_dir)
  
  # Load familiarEnsemble files and add to list
  familiar_list$familiarEnsemble <- load_familiar_object(ensemble_files)
  
  
  # Find familiarData files
  data_files <- list.files(path=file_paths$fam_data_dir, pattern="data.RDS")
  data_files <- sapply(data_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$fam_data_dir)
  
  # Load familiarData files and add to list
  familiar_list$familiarData <- load_familiar_object(data_files)
  
  
  # Find familiarCollection files
  coll_files <- list.files(path=file_paths$fam_coll_dir, pattern="ensemble.RDS|pooled_data.RDS")
  coll_files <- sapply(coll_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$fam_coll_dir)
  
  # Load familiarCollection files and add to list
  familiar_list$familiarCollection <- load_familiar_object(coll_files)
  
  return(familiar_list)
}


.clean_familiar_environment <- function(){
  # Cleans all objects assigned to the familiar global environment.
  if(exists("familiar_global_env")){
    rm(list=ls(envir=familiar_global_env), envir=familiar_global_env)
  }
}
