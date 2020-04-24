#' familiar: Fully Automated MachIne Learning In Accessible R
#'
#' Familiar processes data for diagnostic and prognostic purposes.
#'
#' @docType package
#' @name familiar
#' @import data.table
#' @import methods
#' @importFrom stats predict coef
#' @importFrom survival Surv coxph survreg
#' @importFrom utils head tail getFromNamespace
#' @importFrom ranger ranger
#' @importFrom klaR NaiveBayes sknn
#' @importFrom xgboost xgb.train
#' @importFrom rlang quo quos enquo enquos sym syms ensym ensyms parse_expr parse_exprs
#' @importFrom glmnet glmnet cv.glmnet
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
#' @inheritDotParams .parse_file_paths -config
#' @inheritDotParams .parse_experiment_settings -config
#' @inheritDotParams .parse_setup_settings -config
#' @inheritDotParams .parse_preprocessing_settings -config -parallel
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
summon_familiar <- function(formula=NULL, data=NULL, cl=NULL, config=NULL, config_id=1,  ...){
  
  # Set options
  # Disable randomForestSRC OpenMP core use
  options(rf.cores=as.integer(1))
  on.exit(options(rf.cores=-1L), add=TRUE)
  
  # Disable multithreading on data.table to prevent reduced performance due to resource collisions with familiar parallelisation.
  data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(0L))
  
  ##### Load configuration file -----------------------------------
  if(!is.null(config)){
    
    # Load as file path
    if(is.character(config)){
      if(length(config) > 1){
        stop(paste("Configuration: the path to the configuration file is expected",
                   "to be a single character string. Multiple strings were found."))
      }
      
      # Normalise file paths
      config <- normalizePath(config)
      
      # Read xml file, parse to list and remove comments
      config <- xml2::as_list(xml2::read_xml(config))[[1]][[config_id]]
      config <- .clean_configuration_comments(config=config)
      
    } else {
      if(!is.list(config) || !is.recursive(config)){
        stop("Configuration: the input configuration data is not a list of lists.")
      }
    }
  } else {
    config    <- NULL
  }
  
  
  ##### File paths ------------------------------------------------------------------
  # Parse file paths
  file_paths  <- do.call(.parse_file_paths, append(list("config"=config), list(...)))
  
  # Set paths to data
  if(!is.null(file_paths$data) & is.null(data)){
    data <- file_paths$data
  }
  
  ##### Load data -------------------------------------------------------------------
  # Parse experiment and data settings
  settings <- .parse_initial_settings(config=config, ...)
  
  # Load data
  data <- do.call(.load_data, args=append(list("data"=data), settings$data))
  
  # Update settings
  settings <- .update_initial_settings(formula=formula, data=data, settings=settings)
  
  # Create a generic outcome object
  outcome_info <- create_outcome_info(settings=settings)
  
  # Parse data
  data <- .finish_data_preparation(data = data,
                                   sample_id_column = settings$data$sample_col,
                                   batch_id_column = settings$data$batch_col,
                                   outcome_column = settings$data$outcome_col,
                                   outcome_type = settings$data$outcome_type,
                                   include_features = settings$data$include_features,
                                   class_levels = settings$data$class_levels,
                                   censoring_indicator=settings$data$censoring_indicator,
                                   event_indicator=settings$data$event_indicator,
                                   competing_risk_indicator=settings$data$competing_risk_indicator)
  

  
  # Derive experimental design
  experiment_setup <- extract_experimental_setup(experimental_design=settings$data$exp_design,
                                                 file_dir=file_paths$iterations_dir)
  
  # Check experiment settings
  settings <- .update_experimental_design_settings(section_table=experiment_setup, data=data, settings=settings)
  
  # Import remaining settings
  settings <- .parse_general_settings(config=config, data=data, settings=settings, ...)
  
  # Define iterations etc.
  project_info <- .get_iteration_data(file_dir=file_paths$iterations_dir, data=data,
                                      experiment_setup=experiment_setup, settings=settings)
  
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
  feature_info_list <- .get_feature_info_data(data=data, file_paths=file_paths, project_id=project_info$project_id,
                                              outcome_type=settings$data$outcome_type)

  # Identify if an external cluster is provided, and required.
  if(settings$run$parallel){
    is_external_cluster <- inherits(cl, "cluster")
    
  } else {
    is_external_cluster <- FALSE
  }

  # Assign objects that should be accessible everywhere to the familiar global
  # environment.
  .assign_settings_to_global(settings=settings)
  .assign_file_paths_to_global(file_paths=file_paths)
  .assign_feature_info_to_global(feature_info_list=feature_info_list)
  .assign_project_info_to_global(project_info=project_info)
  .assign_outcome_info_to_global(outcome_info=outcome_info)
  .assign_data_to_global(backend_data=data, backend=settings$run$backend, server_port=settings$run$server_port)
  .assign_parallel_options_to_global(is_external_cluster=is_external_cluster,
                                     restart_cluster=settings$run$restart_cluster,
                                     n_cores=settings$run$parallel_nr_cores,
                                     parallel_backend=settings$run$backend)
  
  if(settings$run$parallel & !settings$run$restart_cluster & !is_external_cluster){
    # Start local cluster in the overall process.
    cl <- .restart_cluster(cl=NULL, assign="all")
    
  } else if(settings$run$parallel & settings$run$restart_cluster & !is_external_cluster){
    # Start processes locally.
    cl <- waiver()
    
  } else if(!settings$run$parallel){
    # No cluster is created when 
    cl <- NULL
  }
  
  
  # Start feature selection
  run_feature_selection(cl=cl, proj_list=project_info, settings=settings, file_paths=file_paths)
  
  # Start model building
  run_model_development(cl=cl, proj_list=project_info, settings=settings, file_paths=file_paths)
  
  # Start evaluation
  run_evaluation(cl=cl, proj_list=project_info, settings=settings, file_paths=file_paths)
  
  # Stop locally initiated clusters
  .terminate_cluster(cl)
  
  if(file_paths$is_temporary){
    # Collect all familiarModels, familiarEnsemble, familiarData and
    # familiarCollection objects.
    familiar_list <- .import_all_familiar_objects(file_paths=file_paths)
    
    # Remove the temporary folder
    unlink(x=file_paths$experiment_dir, recursive=TRUE)
    
    # Return list with objects
    return(familiar_list)
    
  } else {
    
    return(invisible())
  }
}



.is_absolute_path <- function(x){
  return(dir.exists(paste0(unlist(strsplit(x, split=.Platform$file.sep))[1], .Platform$file.sep)))
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
  familiar_list$familiarModel <- lapply(model_files, readRDS)
  
  
  # Find familiarEnsemble files
  ensemble_files <- list.files(path=file_paths$mb_dir, pattern="ensemble.RDS|pool.RDS", recursive=TRUE)
  ensemble_files <- sapply(ensemble_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$mb_dir)
  
  # Load familiarEnsemble files and add to list
  familiar_list$familiarEnsemble <- lapply(ensemble_files, readRDS)
  
  
  # Find familiarData files
  data_files <- list.files(path=file_paths$fam_data_dir, pattern="data.RDS")
  data_files <- sapply(data_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$fam_data_dir)
  
  # Load familiarData files and add to list
  familiar_list$familiarData <- lapply(data_files, readRDS)
  
  
  # Find familiarCollection files
  coll_files <- list.files(path=file_paths$fam_coll_dir, pattern="collection.RDS")
  coll_files <- sapply(coll_files, function(x, dir_path) (file.path(dir_path, x)), dir_path=file_paths$fam_coll_dir)
  
  # Load familiarCollection files and add to list
  familiar_list$familiarCollection <- lapply(coll_files, readRDS)
  
  return(familiar_list)
}
