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
#' @importFrom isotree isolation.forest
#' @importFrom stats predict coef
#' @importFrom survival Surv coxph survreg
#' @importFrom utils head tail getFromNamespace
#' @importFrom ranger ranger
#' @importFrom rlang quo quos enquo enquos sym syms ensym ensyms parse_expr parse_exprs
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom e1071 svm naiveBayes gknn
#' @importFrom BART wbart
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
#' @inheritDotParams .parse_file_paths -config
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
  

  ##### Load configuration file -----------------------------------
  config <- .load_configuration_file(config=config,
                                     config_id=config_id)
  
  
  ##### Test arguments provided by ... and config ------------------------------
  .check_dots_is_parameter(dots=names(list(...)))
  .check_configuration_tag_is_parameter(config=config)
  
  
  ##### File paths ------------------------------------------------------------------
  # Parse file paths
  file_paths  <- do.call(.parse_file_paths, append(list("config"=config), list(...)))
  
  # Set paths to data
  if(!is.null(file_paths$data) & is.null(data)){
    data <- file_paths$data
  }
  
  # Make sure to remove the directory if it is on the temporary path.
  if(file_paths$is_temporary) on.exit(unlink(file_paths$experiment_dir, recursive=TRUE), add=TRUE)
  
  ##### Load data -------------------------------------------------------------------
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
    data <- do.call(.load_data, args=append(list("data"=data), settings$data))
    
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
