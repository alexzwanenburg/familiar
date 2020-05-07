#' Internal function for parsing file paths
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param project_dir (*optional*) Path to the project directory. `familiar`
#'   checks if the directory indicated by `experiment_dir` and data files in
#'   `data_file` are relative to the `project_dir`.
#'
#' @param experiment_dir (**recommended**) Path to the directory where all
#'   intermediate and final results produced by `familiar` are written to.
#'
#'   The `experiment_dir` can be a path relative to `project_dir` or an absolute
#'   path.
#'
#'   In case no project directory is provided and the experiment directory is
#'   not on an absolute path, a directory will be created in the temporary R
#'   directory indicated by `tempdir()`. This directory is deleted after closing
#'   the R session or once data analysis has finished. All information will be
#'   lost afterwards. Hence, it is recommended to provide either
#'   `experiment_dir` as an absolute path, or provide both `project_dir` and
#'   `experiment_dir`.
#' @param data_file (*optional*) Path to files containing data that should be
#'   analysed. The paths can be relative to `project_dir` or absolute paths. An
#'   error will be raised if the file cannot be found.
#'
#'   The following types of data are supported.
#'
#'   * `csv` files containing column headers on the first row, and samples per
#'   row. `csv` files are read using `data.table::fread`.
#'
#'   * `rds` files that contain a `data.table` or `data.frame` object. `rds`
#'   files are imported using `base::readRDS`.
#'
#'   * `RData` files that contain a single `data.table` or `data.frame` object.
#'   `RData` files are imported using `base::load`.
#'
#'   All data are expected in wide format, with sample information organised
#'   row-wise.
#'
#'   More than one data file can be provided. `familiar` will try to combine
#'   data files based on column names and identifier columns.
#'
#'   Alternatively, data can be provided using the `data` argument. These data
#'   are expected to be `data.frame` or `data.table` objects or paths to data
#'   files. The latter are handled in the same way as file paths provided to
#'   `data_file`.
#' @param ... Unused arguments
#'
#' @return List of paths to important directories and files.
#'
#' @md
#' @keywords internal
.parse_file_paths <- function(config=NULL,
                              project_dir=waiver(),
                              experiment_dir=waiver(),
                              data_file=waiver(),
                              ...){

  # Initialise list of file paths
  file_paths      <- list()
  
  # Read project directory; if none is provided through the configuration file
  # or the function input, defer to the R temp directory
  project_dir <- .parse_arg(x_config=config$paths$project_dir, x_var=project_dir,
                            var_name="project_dir", type="character",
                            optional=TRUE, default=NULL)
  
  if(!is.null(project_dir)) {
    project_dir <- normalizePath(project_dir, mustWork=TRUE)
    file_paths$is_temporary <- FALSE
  } else {
    project_dir <- normalizePath(file.path(tempdir(), "familiar"), mustWork=FALSE)
    file_paths$is_temporary <- TRUE
  }
  
  # Read experiment directory path and create the directory if required
  experiment_dir <- .parse_arg(x_config=config$paths$experiment_dir, x_var=experiment_dir, var_name="experiment_dir",
                               type="character", optional=TRUE, default="")
  
  # Determine if the experiment directory is relative to the project directory,
  # or if it is its own path.
  if(.is_absolute_path(experiment_dir)){
    experiment_dir <- normalizePath(experiment_dir, mustWork=FALSE)
    
    # If the experiment directory is an absolute path, there is no need to
    # create a temporary directory.
    file_paths$is_temporary <- FALSE
    
  } else {
    experiment_dir <- normalizePath(file.path(project_dir, experiment_dir), mustWork=FALSE)
  }
  
  file_paths$experiment_dir    <- experiment_dir
  if(!dir.exists(file_paths$experiment_dir)){ dir.create(file_paths$experiment_dir, recursive=TRUE) }
  
  # Log file - set as global variable as well
  file_paths$log_file          <- normalizePath(file.path(experiment_dir, "log.txt"), mustWork=FALSE)
  assign("log_file", file_paths$log_file, envir=familiar_global_env)
  
  # Directory for iterations
  file_paths$iterations_dir    <- normalizePath(experiment_dir, mustWork=FALSE)
  if(!dir.exists(file_paths$iterations_dir)){ dir.create(file_paths$iterations_dir) }
  
  # Directory for pre-processing
  file_paths$process_data_dir  <- normalizePath(experiment_dir, mustWork=FALSE)
  if(!dir.exists(file_paths$process_data_dir)){ dir.create(file_paths$process_data_dir) }
  
  # Directory for feature selection
  file_paths$fs_dir            <- normalizePath(file.path(experiment_dir, "variable_importance"),mustWork=FALSE)
  if(!dir.exists(file_paths$fs_dir)){ dir.create(file_paths$fs_dir) }
  
  # Directory and files for model building
  file_paths$mb_dir            <- normalizePath(file.path(experiment_dir, "trained_models"), mustWork=FALSE)
  if(!dir.exists(file_paths$mb_dir)){ dir.create(file_paths$mb_dir) }
  
  # Directory for familiarData and familiarDataCollection objects
  file_paths$fam_data_dir    <- normalizePath(file.path(experiment_dir, "familiar_data"), mustWork=FALSE)
  if(!dir.exists(file_paths$fam_data_dir)) { dir.create(file_paths$fam_data_dir) }
  file_paths$fam_coll_dir    <- normalizePath(file.path(experiment_dir, "familiar_collections"), mustWork=FALSE)
  if(!dir.exists(file_paths$fam_coll_dir)) { dir.create(file_paths$fam_coll_dir) }
  
  # Create results directory
  file_paths$results_dir     <- normalizePath(file.path(experiment_dir, "results"), mustWork=FALSE)
  if(!dir.exists(file_paths$results_dir)) { dir.create(file_paths$results_dir) }
  
  # Data file location
  data_file <- .parse_arg(x_config=config$paths$data_file, x_var=data_file, var_name="data_file",
                          type="character_list", optional=TRUE, default=NULL)
  
  if(!is.null(data_file)) {
    # Iterate over data file(s) to determine the full path
    file_paths$data <- sapply(data_file, function(curr_data_file, project_dir){
      
      # Check if the current file is represented using an absolute path
      if(file.exists(curr_data_file)){
        data_file_path <- curr_data_file
        
      } else if(file.exists(file.path(project_dir, curr_data_file))) {
        data_file_path <- file.path(project_dir, curr_data_file)
        
      } else {
        stop(paste("Data could not be found. A file was expected at:", curr_data_file, "or at", file.path(project_dir, curr_data_file)))
      }
      
    }, project_dir=project_dir)
  }
  
  if(file_paths$is_temporary){
    logger.warning(paste0("Configuration: A temporary R directory is created for the analysis: ", tempdir()))
  }
  
  return(file_paths)
}



#' Internal function for parsing settings required to parse the input data
#' and define the experiment
#' 
#' This function parses settings required to parse the data set, e.g. determine
#' which columns are identfier columns, what column contains outcome data, which
#' type of outcome is it?
#'
#' @param config A list of settings, e.g. from an xml file.
#' @inheritDotParams .parse_experiment_settings -config
#' 
#' @details Three variants of parameters exist:
#' * required: this parameter is required and must be set by the user.
#' * recommended: not setting this parameter might cause an error to be thrown,
#' dependent on other input.
#' * optional: these parameters have default values that may be altered if
#' required.
#' 
#' @return A list of settings to be used for configuring the experiments.
#' @md
#' @keywords internal
.parse_initial_settings <- function(config=NULL, ...){
  
  # Generate settings list
  settings <- list()
  
  # Experiment and data settings
  settings$data <- do.call(.parse_experiment_settings, args=append(list("config"=config$data), list(...)))

  return(settings)
}


#' Internal function for parsing settings that configure various aspects of the
#' worklow
#'
#' @param settings List of settings that was previously generated by
#'   `.parse_initial_settings`.
#' @param config A list of settings, e.g. from an xml file.
#' @param data Data set as loaded using the `.load_data` function.
#' @inheritDotParams .parse_setup_settings -config
#' @inheritDotParams .parse_preprocessing_settings -data -config -parallel -outcome_type
#' @inheritDotParams .parse_feature_selection_settings -data -config -parallel -outcome_type
#' @inheritDotParams .parse_model_development_settings -data -config -parallel -outcome_type
#' @inheritDotParams .parse_hyperparameter_optimisation_settings -config -parallel -outcome_type
#' @inheritDotParams .parse_evaluation_settings -config -data -parallel -outcome_type -hpo_metric -development_batch_id -vimp_aggregation_rank_threshold -vimp_aggregation_method -prep_cluster_method -prep_cluster_linkage_method -prep_cluster_cut_method -prep_cluster_similarity_threshold -prep_cluster_similarity_metric
#'
#' @return A list of settings to be used within the workflow
#'
#' @references 1. Storey, J. D. A direct approach to false discovery rates. J.
#'   R. Stat. Soc. Series B Stat. Methodol. 64, 479–498 (2002).
#'
#'   1. Shrout, P. E. & Fleiss, J. L. Intraclass correlations: uses in assessing
#'   rater reliability. Psychol. Bull. 86, 420–428 (1979).
#'
#'   1. Koo, T. K. & Li, M. Y. A guideline of selecting and reporting intraclass
#'   correlation coefficients for reliability research. J. Chiropr. Med. 15,
#'   155–163 (2016).
#'
#'   1. Yeo, I. & Johnson, R. A. A new family of power transformations to
#'   improve normality or symmetry. Biometrika 87, 954–959 (2000).
#'
#'   1. Box, G. E. P. & Cox, D. R. An analysis of transformations. J. R. Stat.
#'   Soc. Series B Stat. Methodol. 26, 211–252 (1964).
#'
#'   1. Park, M. Y., Hastie, T. & Tibshirani, R. Averaged gene expressions for
#'   regression. Biostatistics 8, 212–227 (2007).
#'
#'   1. Tolosi, L. & Lengauer, T. Classification with correlated features:
#'   unreliability of feature ranking and solutions. Bioinformatics 27,
#'   1986–1994 (2011).
#'
#'   1. Johnson, W. E., Li, C. & Rabinovic, A. Adjusting batch effects in
#'   microarray expression data using empirical Bayes methods. Biostatistics 8,
#'   118–127 (2007)
#'
#'   1. Kaufman, L. & Rousseeuw, P. J. Finding groups in data: an introduction
#'   to cluster analysis. (John Wiley & Sons, 2009).
#'
#'   1. Muellner, D. fastcluster: fast hierarchical, agglomerative clustering
#'   routines for R and Python. J. Stat. Softw. 53, 1–18 (2013).
#'
#'   1. Rousseeuw, P. J. Silhouettes: A graphical aid to the interpretation and
#'   validation of cluster analysis. J. Comput. Appl. Math. 20, 53–65 (1987).
#'
#'   1. Langfelder, P., Zhang, B. & Horvath, S. Defining clusters from a
#'   hierarchical cluster tree: the Dynamic Tree Cut package for R.
#'   Bioinformatics 24, 719–720 (2008).
#'
#'   1. McFadden, D. Conditional logit analysis of qualitative choice behavior.
#'   in Frontiers in Econometrics (ed. Zarembka, P.) 105–142 (Academic Press,
#'   1974).
#'
#'   1. Cox, D. R. & Snell, E. J. Analysis of binary data. (Chapman and Hall,
#'   1989).
#'
#'   1. Nagelkerke, N. J. D. A note on a general definition of the coefficient
#'   of determination. Biometrika 78, 691–692 (1991).
#'
#'   1. Meinshausen, N. & Buehlmann, P. Stability selection. J. R. Stat. Soc.
#'   Series B Stat. Methodol. 72, 417–473 (2010).
#'
#'   1. Haury, A.-C., Gestraud, P. & Vert, J.-P. The influence of feature
#'   selection methods on accuracy, stability and interpretability of molecular
#'   signatures. PLoS One 6, e28210 (2011).
#'
#'   1. Wald, R., Khoshgoftaar, T. M., Dittman, D., Awada, W. & Napolitano,A. An
#'   extensive comparison of feature ranking aggregation techniques in
#'   bioinformatics. in 2012 IEEE 13th International Conference on Information
#'   Reuse Integration (IRI) 377–384 (2012).
#'
#'   1. Hutter, F., Hoos, H. H. & Leyton-Brown, K. Sequential model-based
#'   optimization for general algorithm configuration. in Learning and
#'   Intelligent Optimization (ed. Coello, C. A. C.) 6683, 507–523 (Springer
#'   Berlin Heidelberg, 2011).
#'
#'   1. Davison, A. C. & Hinkley, D. V. Bootstrap methods and their application.
#'   (Cambridge University Press, 1997).
#'
#'   1. Lausen, B. & Schumacher, M. Maximally Selected Rank Statistics.
#'   Biometrics 48, 73 (1992).
#'
#'   1. Hothorn, T. & Lausen, B. On the exact distribution of maximally selected
#'   rank statistics. Comput. Stat. Data Anal. 43, 121–137 (2003).
#' @md
#' @keywords internal
.parse_general_settings <- function(settings, config=NULL, data, ...){
  
  # Computational setup settings
  settings$run <- do.call(.parse_setup_settings, args=append(list("config"=config$run), list(...)))
  
  # Remove outcome_type, development_batch_id and parallel from ... This prevents an error caused by
  # multiple matching arguments.
  dots <- list(...)
  dots$parallel <- NULL
  dots$outcome_type <- NULL
  dots$development_batch_id <- NULL
  dots$hpo_metric <- NULL
  dots$vimp_aggregation_method <- NULL
  dots$vimp_aggregation_rank_threshold <- NULL
  
  # Pre-processing settings
  settings$prep <- do.call(.parse_preprocessing_settings,
                           args=append(list("config"=config$preprocessing,
                                            "data"=data,
                                            "parallel"=settings$run$parallel,
                                            "outcome_type"=settings$data$outcome_type),
                                       dots))
  
  # Feature selection settings
  settings$fs <- do.call(.parse_feature_selection_settings,
                         args=append(list("config"=config$feature_selection,
                                          "data"=data,
                                          "parallel"=settings$run$parallel,
                                          "outcome_type"=settings$data$outcome_type),
                                     dots))
  
  # Model development settings
  settings$mb <- do.call(.parse_model_development_settings,
                         args=append(list("config"=config$model_development,
                                          "data"=data,
                                          "parallel"=settings$run$parallel,
                                          "outcome_type"=settings$data$outcome_type),
                                     dots))
  
  # Hyperparameter optimisation settings
  settings$hpo <- do.call(.parse_hyperparameter_optimisation_settings,
                          args=append(list("config"=config$hyperparameter_optimisation,
                                           "parallel"=settings$run$parallel,
                                           "outcome_type"=settings$data$outcome_type),
                                      dots))

  # Evaluation settings
  settings$eval <- do.call(.parse_evaluation_settings,
                           args=append(list("config"=config$evaluation,
                                            "data"=data, "parallel"=settings$run$parallel,
                                            "outcome_type"=settings$data$outcome_type,
                                            "hpo_metric"=settings$hpo$hpo_metric,
                                            "development_batch_id"=settings$data$train_cohorts,
                                            "vimp_aggregation_method"=settings$fs$aggregation,
                                            "vimp_aggregation_rank_threshold"=settings$fs$aggr_rank_threshold,
                                            "prep_cluster_method"=settings$prep$cluster_method,
                                            "prep_cluster_linkage_method"=settings$prep$cluster_linkage,
                                            "prep_cluster_cut_method"=settings$prep$cluster_cut_method,
                                            "prep_cluster_similarity_threshold"=settings$prep$cluster_sim_thresh,
                                            "prep_cluster_similarity_metric"=settings$prep$cluster_similarity_metric),
                                       dots))

  # Set the general parallel switch to FALSE if all workflow steps disabled parallel processing.
  settings$run$parallel <- settings$prep$do_parallel | settings$fs$do_parallel | settings$mb$do_parallel | settings$hpo$do_parallel | settings$eval$do_parallel
    
  return(settings)
}


#' Internal function for parsing settings related to the computational setup
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param sample_id_column (**recommended**) Name of the column containing
#'   sample or subject identifiers. This parameter is required if more than one
#'   dataset is provided.
#' @param batch_id_column (**recommended**) Name of the column containing batch
#'   or cohort identifiers. This parameter is required if more than one dataset
#'   is provided, or if external validation is performed.
#' @param development_batch_id (*optional*) One or more batch or cohort
#'   identifiers to constitute data sets for development. Defaults to all, or
#'   all minus the identifiers in `validation_batch_id` for external validation.
#'   Required if external validation is performed and `validation_batch_id` is
#'   not provided.
#' @param validation_batch_id (*optional*) One or more batch or cohort
#'   identifiers to constitute data sets for external validation. Defaults to
#'   all data sets except those in `development_batch_id` for external
#'   validation, or none if not. Required if `development_batch_id` is not
#'   provided.
#'
#' @param outcome_name (*optional*) Name of the modelled outcome. This name will
#'   be used in figures created by `familiar`.
#'
#'   If not set, the column name in `outcome_column` will be used for
#'   `binomial`, `multinomial`, `count` and `continuous` outcomes. For other
#'   outcomes (`survival` and `competing_risk`) no default is used.
#'
#' @param outcome_column (**recommended**) Name of the column containing the
#'   outcome of interest. May be identified from a formula, if a formula is
#'   provided as an argument. Otherwise an error is raised. Note that `survival`
#'   and `competing_risk` outcome type outcomes require two columns that
#'   indicate the time-to-event or the time of last follow-up and the event
#'   status.
#'
#' @param outcome_type (**recommended**) Type of outcome found in the outcome
#'   column. The outcome type determines many aspects of the overall process,
#'   e.g. the available feature selection methods and learners, but also the
#'   type of assessments that can be conducted to evaluate the resulting models.
#'   Implemented outcome types are:
#'
#'   * `binomial`: categorical outcome with 2 levels.
#'
#'   * `multinomial`: categorical outcome with 2 or more levels.
#'
#'   * `count`: Poisson-distributed numeric outcomes.
#'
#'   * `continuous`: general continuous numeric outcomes.
#'
#'   * `survival`: survival outcome for time-to-event data.
#'
#'   If not provided, the algorithm will attempt to obtain outcome_type from
#'   contents of the outcome column. This may lead to unexpected results, and we
#'   therefore advise to provide this information manually.
#'
#'   Note that `competing_risk` survival analysis are not fully supported, and
#'   is currently not a valid choice for `outcome_type`.
#'
#' @param class_levels (*optional*) Class levels for `binomial` or `multinomial`
#'   outcomes. This argument can be used to specify the ordering of levels for
#'   categorical outcomes. These class levels must exactly match the levels
#'   present in the outcome column.
#'
#' @param event_indicator (**recommended**) Indicator for events in `survival`
#'   and `competing_risk` analyses. `familiar` will automatically recognise `1`,
#'   `true`, `t`, `y` and `yes` as event indicators, including different
#'   capitalisations. If this parameter is set, it replaces the default values.
#'
#' @param censoring_indicator (**recommended**) Indicator for right-censoring in
#'   `survival` and `competing_risk` analyses. `familiar` will automatically
#'   recognise `0`, `false`, `f`, `n`, `no` ascensoring indicators, including
#'   different capitalisations. If this parameter is set, it replaces the
#'   default values.
#'
#' @param competing_risk_indicator (**recommended**) Indicator for competing
#'   risks in `competing_risk` analyses. There are no default values, and if
#'   unset, all values other than those specified by the `event_indicator` and
#'   `censoring_indicator` parameters are considered to indicate competing
#'   risks.
#'
#' @param signature (*optional*) One or more names of feature columns that are
#'   considered part of a specific signature. Features specified here will
#'   always be used for modeling. Ranking from feature selection has no effect
#'   for these features.
#' @param exclude_features (*optional*) Feature columns that will be removed
#'   from the data set. Cannot overlap with features in `signature` or
#'   `include_features`.
#' @param include_features (*optional*) Feature columns that are specifically
#'   included in the data set. By default all features are included. Cannot
#'   overlap with `exclude_features`, but may overlap `signature`. Features in
#'   `signature` are always included. If both `exclude_features` and
#'   `include_features` are provided, `include_features` takes precedence,
#'   provided that there is no overlap between the two.
#' @param experimental_design (**required**) Defines what the experiment looks
#'   like, e.g. `cv(bt(fs,20)+mb,3,2)+ev` for 2 times repeated 3-fold
#'   cross-validation with nested feature selection on 20 bootstraps and
#'   model-building, and external validation. The basic workflow components are:
#'
#'   * `fs`: (required) feature selection step.
#'
#'   * `mb`: (required) model building step.
#'
#'   * `ev`: (optional) external validation. Note that internal validation due
#'   to subsampling will always be conducted if the subsamplers create any
#'   validation data sets.
#'
#'   The different components are linked using `+`.
#'
#'   Different subsampling algorithms can be used in conjuction with the basic
#'   workflow components:
#'
#'   * `bt(x,n)`: (stratified) .632 bootstrap, with `n` the number of
#'   bootstraps. Unlike other subsamplers, no separate pre-processing parameters
#'   are determined for each bootstrap.
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
#'   (see the `imbalance_correction_method` parameter). Imbalance partitioning
#'   does not generate validation sets.
#'
#'   As shown in the example above, sampling algorithms can be nested.
#'
#'   The simplest valid experimental design is `fs+mb`, which corresponds to a
#'   TRIPOD type 1a analysis. Type 1b analyses are only possible using
#'   bootstraps, e.g. `bt(fs+mb,100)`. Type 2a analyses can be conducted using
#'   cross-validation, e.g. `cv(bt(fs,100)+mb,10,1)`. Depending on the origin of
#'   the external validation data, designs such as `fs+mb+ev` or
#'   `cv(bt(fs,100)+mb,10,1)+ev` constitute type 2b or type 3 analyses. Type 4
#'   analyses can be done by obtaining one or more `familiarModel` objects from
#'   others and applying them to your own data set.
#'
#'   Alternatively, the `experiment_design` parameter may be used to provide a
#'   path to a file containing iterations, which is named `####_iterations.RDS`
#'   by convention. This path can be relative to the directory of the current
#'   experiment (`experiment_dir`), or an absolute path. The absolute path may
#'   thus also point to a file from a different experiment.
#'
#' @param imbalance_correction_method (*optional*) Type of method used to
#'   address class imbalances. Available options are:
#'
#'   * `full_undersampling` (default): All data will be used in an ensemble
#'   fashion. The full minority class will appear in each partition, but
#'   majority classes are undersampled until all data have been used.
#'
#'   * `random_undersampling`: Randomly undersamples majority classes. This is
#'   useful in cases where full undersampling would lead to the formation of
#'   many models due major overrepresentation of the largest class.
#'
#'   This parameter is only used in combination with imbalance partitioning in
#'   the experimental design, and `ip` should therefore appear in the string
#'   that defines the design.
#' @param imbalance_n_partitions (*optional*) Number of times random
#'   undersampling should be repeated. 10 undersampled subsets with balanced
#'   classes are formed by default.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to data parsing and the experiment.
#' @md
#' @keywords internal
.parse_experiment_settings <- function(config=NULL,
                                       sample_id_column=waiver(),
                                       batch_id_column=waiver(),
                                       development_batch_id=waiver(),
                                       validation_batch_id=waiver(),
                                       outcome_name=waiver(),
                                       outcome_column=waiver(),
                                       outcome_type=waiver(),
                                       event_indicator=waiver(),
                                       censoring_indicator=waiver(),
                                       competing_risk_indicator=waiver(),
                                       class_levels=waiver(),
                                       signature=waiver(),
                                       exclude_features=waiver(),
                                       include_features=waiver(),
                                       experimental_design=waiver(),
                                       imbalance_correction_method=waiver(),
                                       imbalance_n_partitions=waiver(),
                                       ...){
  
  settings <- list()

  # Experimental design
  settings$exp_design <- .parse_arg(x_config=config$experimental_design, x_var=experimental_design,
                                    var_name="experimental_design", optional=FALSE, type="character")
  
  # Class imbalance correction method
  settings$imbalance_method <- .parse_arg(x_config=config$imbalance_correction_method,
                                          x_var=imbalance_correction_method, var_name="imbalance_correction_method",
                                          type="character", optional=TRUE, default="full_undersampling")
  
  .check_parameter_value_is_valid(x=settings$imbalance_method, var_name="imbalance_correction_method",
                                  values=c("full_undersampling", "random_undersampling"))
  
  # Number of imbalance partitions for random undersampling
  settings$imbalance_n_partitions <- .parse_arg(x_config=config$imbalance_n_partitions,
                                                x_var=imbalance_n_partitions, var_name="imbalance_n_partitions",
                                                type="integer", optional=TRUE, default=10)
  
  .check_number_in_valid_range(x=settings$imbalance_n_partitions, var_name="imbalance_n_partitions", range=c(1, Inf))
  
  # Sample identifier column
  settings$sample_col <- .parse_arg(x_config=config$sample_id_column, x_var=sample_id_column,
                                    var_name="sample_id_colum", type="character", optional=TRUE, default=NULL)
  
  # Update column name
  if(!is.null(settings$sample_col)){
    settings$sample_col <- check_column_name(settings$sample_col)
  }
  
  # Cohort identifier column
  settings$batch_col  <- .parse_arg(x_config=config$batch_id_column, x_var=batch_id_column,
                                    var_name="batch_id_column", type="character", optional=TRUE, default=NULL)
  
  # Update column name
  if(!is.null(settings$batch_col)){
    settings$batch_col <- check_column_name(settings$batch_col)
  }
  
  # Development cohort identifier
  settings$train_cohorts <- .parse_arg(x_config=config$development_batch_id, x_var=development_batch_id,
                                       var_name="development_batch_id", type="character_list", optional=TRUE, default=NULL)
  
  # Validation cohort identifier
  settings$valid_cohorts <- .parse_arg(x_config=config$validation_batch_id, x_var=validation_batch_id,
                                       var_name="validation_batch_id", type="character_list", optional=TRUE, default=NULL)
  
  # Outcome column(s)
  settings$outcome_col <- .parse_arg(x_config=config$outcome_column, x_var=outcome_column,
                                     var_name="outcome_column", type="character_list", optional=TRUE, default=NULL)
  
  if(!is.null(settings$outcome_col)){
    settings$outcome_col <- check_column_name(settings$outcome_col)
  }
  
  # Outcome type - a check will be done later
  settings$outcome_type <- .parse_arg(x_config=config$outcome_type, x_var=outcome_type,
                                      var_name="outcome_type", type="character", optional=TRUE, default=NULL)
  
  # Outcome name
  settings$outcome_name <- .parse_arg(x_config=config$outcome_name, x_var=outcome_name,
                                      var_name="outcome_name", type="character", optional=TRUE, default=NULL)
  
  # Class levels
  settings$class_levels <- .parse_arg(x_config=config$class_levels, x_var=class_levels,
                                      var_name="class_levels", type="character_list", optional=TRUE, default=NULL)
  
  # Event indicator
  settings$event_indicator <- .parse_arg(x_config=config$event_indicator, x_var=event_indicator,
                                         var_name="event_indicator", type="character_list", optional=TRUE, default=NULL)
  
  # Censoring indicator
  settings$censoring_indicator <- .parse_arg(x_config=config$censoring_indicator, x_var=censoring_indicator,
                                             var_name="censoring_indicator", type="character_list", optional=TRUE, default=NULL)
  
  # Competing risk indicator
  settings$competing_risk_indicator <- .parse_arg(x_config=config$competing_risk_indicator, x_var=competing_risk_indicator,
                                                  var_name="competing_risk_indicator", type="character_list", optional=TRUE, default=NULL) 
  
  # Signature features
  settings$signature <- .parse_arg(x_config=config$signature, x_var=signature,
                                   var_name="signature", type="character_list", optional=TRUE, default=NULL)
  
  if(!is.null(settings$signature)){
    settings$signature <- check_column_name(settings$signature)
  }
  
  # Included features
  settings$include_features <- .parse_arg(x_config=config$include_features, x_var=include_features,
                                          var_name="include_features", type="character_list", optional=TRUE, default=NULL)
  
  if(!is.null(settings$include_features)){
    settings$include_features <- check_column_name(settings$include_features)
  }
  
  # Excluded features
  settings$exclude_features <- .parse_arg(x_config=config$exclude_features, x_var=exclude_features,
                                          var_name="exclude_features", type="character_list", optional=TRUE, default=NULL)
  
  if(!is.null(settings$exclude_features)){
    settings$exclude_features <- check_column_name(settings$exclude_features)
  }
  
  return(settings)
}



#' Internal function for parsing settings related to the computational setup
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param parallel (*optional*) Enable parallel processing. Defaults to `TRUE`.
#'   When set to `FALSE`, this disables all parallel processing, regardless of
#'   specific parameters such as `parallel_preprocessing`. However, when
#'   `parallel` is `TRUE`, parallel processing of different parts of the
#'   workflow can be disabled by setting respective flags to `FALSE`.
#' @param parallel_nr_cores (*optional*) Number of cores available for
#'   parallelisation. Defaults to all available cores-1. This setting does
#'   nothing if parallelisation is disabled.
#' @param restart_cluster (*optional*) Restart nodes used for parallel computing
#'   to free up memory prior to starting a parallel process. Note that it does
#'   take time to set up the clusters. Therefore setting this argument to `TRUE`
#'   may impact processing speed. This argument is ignored if `parallel` is
#'   `FALSE` or the cluster was initialised outside of familiar. Default is
#'   `FALSE`, which causes the clusters to be initialised only once.
#' @param backend (*optional*) Selection of the back-end for distributing copies
#'   of the data. Several backend options are available, notably `rserve`,
#'   `rserve_coop` (a version of rserve that is forced to operate in cooperative
#'   mode), `fork` and `non_fork`. Availability of the backend depends on the OS
#'   and package installation. `rserve_coop` (all OS), `rserve` (under windows)
#'   and `fork` (under Linux and derived OS) maintain a single copy of the
#'   complete dataset that is accessible to all parallel processes. This is
#'   usually considerably more memory-efficient.
#' @param server_port (*optional*) Integer indicating the port on which the
#'   RServe process should communicate. Defaults to port 6311. Note that ports 0
#'   to 1024 and 49152 to 65535 cannot be used.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to the computational setup.
#' @md
#' @keywords internal
.parse_setup_settings <- function(config=NULL,
                                  parallel=waiver(),
                                  parallel_nr_cores=waiver(),
                                  restart_cluster=waiver(),
                                  backend=waiver(),
                                  server_port=waiver(),
                                  ...){
  
  settings <- list()

  # Parallelisation master switch
  settings$parallel <- .parse_arg(x_config=config$parallel, x_var=parallel, var_name="parallel",
                                  type="logical", optional=TRUE, default=TRUE)
  
  # Maximum number of cores that a R may use
  settings$parallel_nr_cores <- .parse_arg(x_config=config$parallel_nr_cores, x_var=parallel_nr_cores,
                                           var_name="parallel_nr_cores", type="integer", optional=TRUE, default=NULL)
  
  if(!is.null(settings$parallel_nr_cores)){
    .check_number_in_valid_range(x=settings$parallel_nr_cores, var_name="parallel_nr_cores", range=c(1, parallel::detectCores()))
  }
  
  # Set cores to 1 in case parallel processing is disabled.
  if(!settings$parallel){
    settings$parallel_nr_cores <- 1L
  }
  
  # Restart clusters
  settings$restart_cluster <- .parse_arg(x_config=config$restart_cluster, x_var=restart_cluster,
                                         var_name="restart_cluster", type="logical", optional=TRUE, default=FALSE)
  
  if(!settings$parallel){
    settings$restart_cluster <- FALSE
  }
  
  # Data server backend - this is os- and package-dependent
  settings$backend <- .parse_arg(x_config=config$backend, x_var=backend, var_name="backed",
                                 type="character", optional=TRUE, default=.get_default_backend())
  
  # Set to non-fork processing if parallel is disabled. This avoids starting any
  # RServe or RServeCoop processes.
  if(!settings$parallel){
    settings$backend <- "non_fork"
  }
  
  .check_backend_availability(backend_option=settings$backend)
  
  # RServe communications port
  settings$server_port <- .parse_arg(x_config=config$server_port, x_var=server_port, var_name="server_port",
                                     type="integer", optional=TRUE, default=6311L)
  
  .check_number_in_valid_range(x=settings$server_port, var_name="server_port", range=c(1025, 49151))
  return(settings)
}



#' Internal function for parsing settings related to preprocessing
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param data Data set as loaded using the `.load_data` function.
#' @param parallel Logical value that whether familiar uses parallelisation. If
#'   `FALSE` it will override `parallel_preprocessing`.
#' @param outcome_type Type of outcome found in the data set.
#' @param feature_max_fraction_missing (*optional*) Numeric value between `0.0`
#'   and `0.95` that determines the meximum fraction of missing values that
#'   still allows a feature to be included in the data set. All features with a
#'   missing value fraction over this threshold are not processed further. The
#'   default value is `0.30`.
#' @param sample_max_fraction_missing (*optional*) Numeric value between `0.0`
#'   and `0.95` that determines the maximum fraction of missing values that
#'   still allows a sample to be included in the data set. All samples with a
#'   missing value fraction over this threshold are excluded and not processed
#'   further. The default value is `0.30`.
#' @param filter_method (*optional*) One or methods used to reduce
#'   dimensionality of the data set by removing irrelevant or poorly
#'   reproducible features.
#'
#'   Several method are available:
#'
#'   * `none` (default): None of the features will be filtered.
#'
#'   * `low_variance`: Features with a variance below the
#'   `low_var_minimum_variance_threshold` are filtered. This can be useful to
#'   filter, for example, genes that are not differentially expressed.
#'
#'   * `univariate_test`: Features undergo a univariate regression using an
#'   outcome-appropriate regression model. The p-value of the model coefficient
#'   is collected. Features with coefficient p or q-value above the
#'   `univariate_test_threshold` are subsequently filtered.
#'
#'   * `robustness`: Features that are not sufficiently robust according to the
#'   intraclass correlation coefficient are filtered. Use of this method
#'   requires that repeated measurements are present in the data set, i.e. there
#'   should be entries for which the sample and cohort identifiers are the same.
#'
#'   More than one method can be used simultaneously. Features with singular
#'   values are always filtered, as these do not contain information.
#' @param univariate_test_threshold (*optional*) Numeric value between `1.0` and
#'   `0.0` that determines which features are irrelevant and will be filtered by
#'   the `univariate_test`. The p or q-values are compared to this threshold.
#'   All features with values above the threshold are filtered. The default
#'   value is `0.20`.
#' @param univariate_test_threshold_metric (*optional*) Metric used with the to
#'   compare the `univariate_test_threshold` against. The following metrics can
#'   be chosen:
#'
#'   * `p_value` (default): The unadjusted p-value of each feature is used for
#'   to filter features.
#'
#'   * `q_value`: The q-value (Story, 2002), is used to filter features. Some
#'   data sets may have insufficient samples to compute the q-value. The
#'   `qvalue` package must be installed from Bioconductor to use this method.
#'
#' @param univariate_test_max_feature_set_size (*optional*) Maximum size of the
#'   feature set after the univariate test. P or q values of features are
#'   compared against the threshold, but if the resulting data set would be
#'   larger than this setting, only the most relevant features up to the desired
#'   feature set size are selected.
#'
#'   The default value is `NULL`, which causes features to be filtered based on
#'   their relevance only.
#' @param low_var_minimum_variance_threshold (required, if used) Numeric value
#'   that determines which features will be filtered by the `low_variance`
#'   method. The variance of each feature is computed and compared to the
#'   threshold. If it is below the threshold, the feature is removed.
#'
#'   This parameter has no default value and should be set if `low_variance` is
#'   used.
#' @param low_var_max_feature_set_size (*optional*) Maximum size of the feature
#'   set after filtering features with a low variance. All features are first
#'   compared against `low_var_minimum_variance_threshold`. If the resulting
#'   feature set would be larger than specified, only the most strongly varying
#'   features will be selected, up to the desired size of the feature set.
#'
#'   The default value is `NULL`, which causes features to be filtered based on
#'   their variance only.
#' @param robustness_icc_type (*optional*) String indicating the type of
#'   intraclass correlation coefficient (`1`, `2` or `3`) that should be used to
#'   compute robustness for features in repeated measurements. These types
#'   correspond to the types in Shrout and Fleiss (1979). The default value is
#'   `1`.
#' @param robustness_threshold_metric (*optional*) String indicating which
#'   specific intraclass correlation coefficient (ICC) metric should be used to
#'   filter features. This should be one of:
#'
#'   * `icc`: The estimated ICC value itself.
#'
#'   * `icc_low` (default): The estimated lower limit of the 95% confidence
#'   interval of the ICC, as suggested by Koo and Li (2016).
#'
#'   * `icc_panel`: The estimated ICC value over the panel average, i.e. the ICC
#'   that would be obtained if all repeated measurements were averaged.
#'
#'   * `icc_panel_low`: The estimated lower limit of the 95% confidence interval
#'   of the panel ICC.
#'
#' @param robustness_threshold_value (*optional*) The intraclass correlation
#'   coefficient value that is as threshold. The default value is `0.70`.
#' @param transformation_method (*optional*) The transformation method used to
#'   change the distribution of the data to be more normal-like. The following
#'   methods are available:
#'
#'   * `none`: This disables transformation of features.
#'
#'   * `yeo_johnson` (default): Transformation using the Yeo-Johnson
#'   transformation (Yeo and Johnson, 2000). The algorithm tests various lambda
#'   values (-2.0, -1.0, -0.5, 0.0, 0.33333, 0.5, 1.0, 1.5, 2.0) and selects the
#'   lambda that maximises the log-likelihood.
#'
#'   * `yeo_johnson_trim`: As `yeo_johnson`, but based on the set of feature
#'   values where the 5% lowest and 5% highest values are discarded. This
#'   reduces the effect of outliers.
#'
#'   * `yeo_johnson_winsor`: As `yeo_johnson`, but based on the set of feature
#'   values where the 5% lowest and 5% highest values are winsorised. This
#'   reduces the effect of outliers.
#'
#'   * `box_cox`: Transformation using the Box-Cox transformation (Box and Cox,
#'   1964). Unlike the Yeo-Johnson transformation, the Box-Cox transformation
#'   requires that all data are positive. Features that contain zero or negative
#'   values cannot be transformed using this transformation. The algorithm tests
#'   various lambda values (-2.0, -1.0, -0.5, 0.0, 0.3333, 0.5, 1.0, 1.5, 2.0)
#'   and selects the lambda that maximises the log-likelihood.
#'
#'   * `box_cox_trim`: As `box_cox`, but based on the set of feature values
#'   where the 5% lowest and 5% highest values are discarded. This reduces the
#'   effect of outliers.
#'
#'   * `box_cox_winsor`: As `box_cox`, but based on the set of feature values
#'   where the 5% lowest and 5% highest values are winsorised. This reduces the
#'   effect of outliers.
#'
#'   Only features that contain numerical data are transformed. Transformation
#'   parameters obtained in development data are stored within `featureInfo`
#'   objects for later use with validation data sets.
#' @param normalisation_method (*optional*) The normalisation method used to
#'   improve the comparability between numerical features that may have very
#'   different scales. The following normalisation methods can be chosen:
#'
#'   * `none`: This disables feature normalisation.
#'
#'   * `standardisation` (default): Features are normalised by subtraction of
#'   their mean values and division by their standard deviations. This causes
#'   every feature to be have a center value of 0.0 and standard deviation of
#'   1.0.
#'
#'   * `standardisation_trim`: As `standardisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are discarded.
#'   This reduces the effect of outliers.
#'
#'   * `standardisation_winsor`: As `standardisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are winsorised.
#'   This reduces the effect of outliers.
#'
#'   * `normalisation`: Features are normalised by subtraction of their minimum
#'   values and division by their ranges. This maps all feature values to a
#'   \eqn{[0, 1]} interval.
#'
#'   * `normalisation_trim`: As `normalisation`, but based on the set of feature
#'   values where the 5% lowest and 5% highest values are discarded. This
#'   reduces the effect of outliers.
#'
#'   * `normalisation_winsor`: As `normalisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are winsorised.
#'   This reduces the effect of outliers.
#'
#'   * `quantile`: Features are normalised by subtraction of their median values
#'   and division by their interquartile range.
#'
#'   * `mean_centering`: Features are centered by substracting the mean, but do
#'   not undergo rescaling.
#'
#'   Only features that contain numerical data are normalised. Normalisation
#'   parameters obtained in development data are stored within `featureInfo`
#'   objects for later use with validation data sets.
#' @param batch_normalisation_method (*optional*) The method used for batch
#'   normalisation. Available methods are:
#'
#'   * `none` (default): This disables batch normalisation of features.
#'
#'   * `standardisation`: Features within each batch are normalised by
#'   subtraction of the mean value and division by the standard deviation in
#'   each batch.
#'
#'   * `standardisation_trim`: As `standardisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are discarded.
#'   This reduces the effect of outliers.
#'
#'   * `standardisation_winsor`: As `standardisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are winsorised.
#'   This reduces the effect of outliers.
#'
#'   * `normalisation`: Features within each batch are normalised by subtraction
#'   of their minimum values and division by their range in each batch. This
#'   maps all feature values in each batch to a \eqn{[0, 1]} interval.
#'
#'   * `normalisation_trim`: As `normalisation`, but based on the set of feature
#'   values where the 5% lowest and 5% highest values are discarded. This
#'   reduces the effect of outliers.
#'
#'   * `normalisation_winsor`: As `normalisation`, but based on the set of
#'   feature values where the 5% lowest and 5% highest values are winsorised.
#'   This reduces the effect of outliers.
#'
#'   * `quantile`: Features in each batch are normalised by subtraction of the
#'   median value and division by the interquartile range of each batch.
#'
#'   * `mean_centering`: Features in each batch are centered on 0.0 by
#'   substracting the mean value in each batch, but are not rescaled.
#'
#'   * `combat_parametric`: Batch adjustments using parametric empirical Bayes
#'   (Johnson et al, 2007). `combat_p` leads to the same method.
#'
#'   * `combat_non_parametric`: Batch adjustments using non-parametric empirical
#'   Bayes (Johnson et al, 2007). `combat_np` and `combat` lead to the same
#'   method. Note that we reduced complexity from O(\eqn{n^2}) to O(\eqn{n}) by
#'   only computing batch adjustment parameters for each feature on a subset of
#'   50 randomly selected features, instead of all features.
#'
#'   Only features that contain numerical data are normalised using batch
#'   normalisation. Batch normalisation parameters obtained in development data
#'   are stored within `featureInfo` objects for later use with validation data
#'   sets, in case the validation data is from the same batch.
#'
#'   If validation data contains data from unknown batches, normalisation
#'   parameters are separately determined for these batches.
#'
#'   Note that for both empirical Bayes methods, the batch effect is assumed to
#'   produce results across the features. This is often true for things such as
#'   gene expressions, but the assumption may not hold generally.
#'
#'   When performing batch normalisation, it is moreover important to check that
#'   differences between batches or cohorts are not related to the studied
#'   endpoint.
#'
#' @param imputation_method (*optional*) Method used for imputing missing
#'   feature values. Two methods are implemented:
#'
#'   * `simple`: Simple replacement of a missing value by the median value (for
#'   numeric features) or the modal value (for categorical features).
#'
#'   * `lasso`: Imputation of missing value by lasso regression (using `glmnet`)
#'   based on information contained in other features.
#'
#'   `simple` imputation precedes `lasso` imputation to ensure that any missing
#'   values in predictors required for `lasso` regression are resolved. The
#'   `lasso` estimate is then used to replace the missing value.
#'
#'   The default value depends on the number of features in the dataset. If the
#'   number is lower than 100, `lasso` is used by default, and `simple`
#'   otherwise.
#'
#'   Only single imputation is performed. Imputation models and parameters are
#'   stored within `featureInfo` objects for later use with validation data
#'   sets.
#' @param cluster_method (*optional*) Clustering is performed to identify and
#'   replace redundant features, for example those that are highly correlated.
#'   Such features do not carry much additional information and may be removed
#'   or replaced instead (Park et al., 2007; Tolosi and Lengauer, 2011).
#'
#'   The cluster method determines the algorithm used to form the clusters. The
#'   following cluster methods are implemented:
#'
#'   * `none`: No clustering is performed.
#'
#'   * `hclust` (default): Hierarchical agglomerative clustering. If the
#'   `fastcluster` package is installed, `fastcluster::hclust` is used (Muellner
#'   2013), otherwise `stats::hclust` is used.
#'
#'   * `agnes`: Hierarchical clustering using agglomerative nesting (Kaufman and
#'   Rousseeuw, 1990). This algorithm is similar to `hclust`, but uses the
#'   `cluster::agnes` implementation.
#'
#'   * `diana`: Divisive analysis hierarchical clustering. This method uses
#'   divisive instead of agglomerative clustering (Kaufman and Rousseeuw, 1990).
#'   `cluster::diana` is used.
#'
#'   * `pam`: Partioning around medioids. This partitions the data into $k$
#'   clusters around medioids (Kaufman and Rousseeuw, 1990). $k$ is selected
#'   using the `silhouette` metric. `pam` is implemented using the
#'   `cluster::pam` function.
#'
#'   Clusters and cluster information is stored within `featureInfo` objects for
#'   later use with validation data sets. This enables reproduction of the same
#'   clusters as formed in the development data set.
#' @param cluster_linkage_method (*optional*) Linkage method used for
#'   agglomerative clustering in `hclust` and `agnes`. The following linkage
#'   methods can be used:
#'
#'   * `average` (default): Average linkage.
#'
#'   * `single`: Single linkage.
#'
#'   * `complete`: Complete linkage.
#'
#'   * `weighted`: Weighted linkage, also known as McQuitty linkage.
#'
#'   * `ward`: Linkage using Ward's minimum variance method.
#'
#'   `diana` and `pam` do not require a linkage method.
#' @param cluster_cut_method (*optional*) The method used to define the actual
#'   clusters. The following methods can be used:
#'
#'   * `silhouette`: Clusters are formed based on the silhouette score
#'   (Rousseeuw, 1987). The average silhouette score is computed from 2 to
#'   \eqn{n} clusters, with \eqn{n} the number of features. Clusters are only
#'   formed if the average silhouette exceeds 0.50, which indicates reasonable
#'   evidence for structure. This procedure may be slow if the number of
#'   features is large (>100s).
#'
#'   * `fixed_cut`: Clusters are formed by cutting the hierarchical tree at the
#'   point indicated by the `cluster_similarity_threshold`, e.g. where features
#'   in a cluster have an average Spearman correlation of 0.90. `fixed_cut` is
#'   only available for `agnes`, `diana` and `hclust`.
#'
#'   * `dynamic_cut`: Dynamic cluster formation using the cutting algorithm in
#'   the `dynamicTreeCut` package. This package should be installed to select
#'   this option. `dynamic_cut` can only be used with `agnes` and `hclust`.
#'
#'   The default options are `silhouette` for partioning around medioids (`pam`)
#'   and `fixed_cut` otherwise.
#'
#' @param cluster_similarity_metric (*optional*) Clusters are formed based on
#'   feature similarity. All features are compared in a pair-wise fashion to
#'   compute similarity, for example correlation. The resulting similarity grid
#'   is converted into a distance matrix that is subsequently used for
#'   clustering. The following metrics are supported to compute pairwise
#'   similarities:
#'
#'   * `mcfadden_r2` (default): McFadden's pseudo R-squared (McFadden, 1974).
#'
#'   * `cox_snell_r2`: Cox and Snell's pseudo R-squared (Cox and Snell, 1989).
#'
#'   * `nagelkerke_r2`: Nagelkerke's pseudo R-squared (Nagelkerke, 1991).
#'
#'   * `spearman`: Spearman's rank order correlation.
#'
#'   * `kendall`: Kendall rank correlation.
#'
#'   * `pearson`: Pearson product-moment correlation.
#'
#'   The pseudo R-squared metrics can be used to assess similarity between mixed
#'   pairs of numeric and categorical features, as these are based on the
#'   log-likelihood of regression models. In `familiar`, the more informative
#'   feature is used as the predictor and the other feature as the reponse
#'   variable. In numeric-categorical pairs, the numeric feature is considered
#'   to be more informative and is thus used as the predictor. In
#'   categorical-categorical pairs, the feature with most levels is used as the
#'   predictor.
#'
#'   In case any of the classical correlation coefficients (`pearson`,
#'   `spearman` and `kendall`) are used with (mixed) categorical features, the
#'   categorical features are one-hot encoded and the mean correlation over all
#'   resulting pairs is used as similarity.
#'
#' @param cluster_similarity_threshold (*optional*) The threshold level for
#'   pair-wise similarity that is required to form clusters using `fixed_cut`.
#'   This should be a numerical value between 0.0 and 1.0. Note however, that a
#'   reasonable threshold value depends strongly on the similarity metric. The
#'   following are the default values used:
#'
#'   * `mcfadden_r2`: `0.30`
#'
#'   * `cox_snell_r2` and `nagelkerke_r2`: `0.75`
#'
#'   * `spearman`, `kendall` and `pearson`: `0.90`
#'
#'   Alternatively, if the `fixed cut` method is not used, this value determines
#'   whether any clustering should be performed, because the data may not
#'   contain highly similar features. The default values in this situation are:
#'
#'   * `mcfadden_r2`: `0.05`
#'
#'   * `cox_snell_r2` and `nagelkerke_r2`: `0.40`
#'
#'   * `spearman`, `kendall` and `pearson`: `0.50`
#'
#'   The threshold value is converted to a distance (1-similarity) prior to
#'   cutting hierarchical trees.
#' @param cluster_representation_method (*optional*) Method used to determine
#'   how the information of co-clustered features is summarised and used to
#'   represent the cluster. The following methods can be selected:
#'
#'   * `best_predictor` (default): The feature with the highest importance
#'   according to univariate regression with the outcome is used to represent
#'   the cluster.
#'
#'   * `medioid`: The feature closest to the cluster center, i.e. the feature
#'   that is most similar to the remaining features in the cluster, is used to
#'   represent the feature.
#'
#'   * `mean`: A meta-feature is generated by averaging the feature values for
#'   all features in a cluster. This method aligns all features so that all
#'   features will be positively correlated prior to averaging. Should a cluster
#'   contain one or more categorical features, the `medioid` method will be used
#'   instead, as averaging is not possible. Note that if this method is chosen,
#'   the `normalisation_method` parameter should be one of `standardisation`,
#'   `standardisation_trim`, `standardisation_winsor` or `quantile`.`
#'
#'   If the `pam` cluster method is selected, only the `medioid` method can be
#'   used. In that case 1 medioid is used by default.
#' @param parallel_preprocessing (*optional*) Enable parallel processing for the
#'   preprocessing workflow. Defaults to `TRUE`. When set to `FALSE`, this will
#'   disable the use of parallel processing while preprocessing, regardless of
#'   the settings of the `parallel` parameter. `parallel_preprocessing` is
#'   ignored if `parallel=FALSE`.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to preprocessing.
#'
#' @references 1. Storey, J. D. A direct approach to false discovery rates. J.
#'   R. Stat. Soc. Series B Stat. Methodol. 64, 479–498 (2002).
#'
#'   1. Shrout, P. E. & Fleiss, J. L. Intraclass correlations: uses in assessing
#'   rater reliability. Psychol. Bull. 86, 420–428 (1979).
#'
#'   1. Koo, T. K. & Li, M. Y. A guideline of selecting and reporting intraclass
#'   correlation coefficients for reliability research. J. Chiropr. Med. 15,
#'   155–163 (2016).
#'
#'   1. Yeo, I. & Johnson, R. A. A new family of power transformations to
#'   improve normality or symmetry. Biometrika 87, 954–959 (2000).
#'
#'   1. Box, G. E. P. & Cox, D. R. An analysis of transformations. J. R. Stat.
#'   Soc. Series B Stat. Methodol. 26, 211–252 (1964).
#'
#'   1. Park, M. Y., Hastie, T. & Tibshirani, R. Averaged gene expressions for
#'   regression. Biostatistics 8, 212–227 (2007).
#'
#'   1. Tolosi, L. & Lengauer, T. Classification with correlated features:
#'   unreliability of feature ranking and solutions. Bioinformatics 27,
#'   1986–1994 (2011).
#'
#'   1. Johnson, W. E., Li, C. & Rabinovic, A. Adjusting batch effects in
#'   microarray expression data using empirical Bayes methods. Biostatistics 8,
#'   118–127 (2007)
#'
#'   1. Kaufman, L. & Rousseeuw, P. J. Finding groups in data: an introduction
#'   to cluster analysis. (John Wiley & Sons, 2009).
#'
#'   1. Muellner, D. fastcluster: fast hierarchical, agglomerative clustering
#'   routines for R and Python. J. Stat. Softw. 53, 1–18 (2013).
#'
#'   1. Rousseeuw, P. J. Silhouettes: A graphical aid to the interpretation and
#'   validation of cluster analysis. J. Comput. Appl. Math. 20, 53–65 (1987).
#'
#'   1. Langfelder, P., Zhang, B. & Horvath, S. Defining clusters from a
#'   hierarchical cluster tree: the Dynamic Tree Cut package for R.
#'   Bioinformatics 24, 719–720 (2008).
#'
#'   1. McFadden, D. Conditional logit analysis of qualitative choice behavior.
#'   in Frontiers in Econometrics (ed. Zarembka, P.) 105–142 (Academic Press,
#'   1974).
#'
#'   1. Cox, D. R. & Snell, E. J. Analysis of binary data. (Chapman and Hall,
#'   1989).
#'
#'   1. Nagelkerke, N. J. D. A note on a general definition of the coefficient
#'   of determination. Biometrika 78, 691–692 (1991).
#'
#' @md
#' @keywords internal
.parse_preprocessing_settings <- function(config=NULL, data, parallel, outcome_type,
                                          feature_max_fraction_missing=waiver(),
                                          sample_max_fraction_missing=waiver(),
                                          filter_method=waiver(),
                                          univariate_test_threshold=waiver(),
                                          univariate_test_threshold_metric=waiver(),
                                          univariate_test_max_feature_set_size=waiver(),
                                          low_var_minimum_variance_threshold=waiver(),
                                          low_var_max_feature_set_size=waiver(),
                                          robustness_icc_type=waiver(),
                                          robustness_threshold_metric=waiver(),
                                          robustness_threshold_value=waiver(),
                                          transformation_method=waiver(),
                                          normalisation_method=waiver(),
                                          batch_normalisation_method=waiver(),
                                          imputation_method=waiver(),
                                          cluster_method=waiver(),
                                          cluster_linkage_method=waiver(),
                                          cluster_cut_method=waiver(),
                                          cluster_similarity_metric=waiver(),
                                          cluster_similarity_threshold=waiver(),
                                          cluster_representation_method=waiver(),
                                          parallel_preprocessing=waiver(),
                                          ...){
  
  settings <- list()
  
  # Maximum fraction of data points missing for inclusion of a feature
  settings$feat_max_fract_missing <- .parse_arg(x_config=config$feature_max_fraction_missing,
                                                x_var=feature_max_fraction_missing, var_name="feature_max_fraction_missing",
                                                type="numeric", optional=TRUE, default=0.30)
  
  .check_number_in_valid_range(x=settings$feat_max_fract_missing, var_name="feature_max_fraction_missing", range=c(0.0, 0.95))
  

    # Maximum fraction of features missing for inclusion of a subject
  settings$subj_max_fract_missing <- .parse_arg(x_config=config$sample_max_fraction_missing,
                                                x_var=sample_max_fraction_missing, var_name="sample_max_fraction_missing",
                                                type="numeric", optional=TRUE, default=0.30)
  
  .check_number_in_valid_range(x=settings$subj_max_fract_missing, var_name="sample_max_fraction_missing", range=c(0.0, 0.95))
  
  
  # Univariate filter methods
  settings$filter_method <- .parse_arg(x_config=config$filter_method, x_var=filter_method, var_name="filter_method",
                                       type="character_list", optional=TRUE, default="none")
  
  .check_parameter_value_is_valid(x=settings$filter_method, var_name="filter_method",
                                  values=c("none", "low_variance", "univariate_test", "robustness"))
  
  
  # Univariate model filter threshold value
  settings$univar_threshold <- .parse_arg(x_config=config$univariate_test_threshold, x_var=univariate_test_threshold,
                                          var_name="univariate_test_threshold", type="numeric", optional=TRUE, default=0.20)
  
  .check_number_in_valid_range(x=settings$univar_threshold, var_name="univariate_test_threshold", range=c(0.0, 1.0), closed=c(FALSE, TRUE))
  
  
  # Univariate model threshold metric
  settings$univar_metric <- .parse_arg(x_config=config$univariate_test_threshold_metric, x_var=univariate_test_threshold_metric,
                                       var_name="univariate_test_threshold_metric", type="character", optional=TRUE,
                                       default="p_value")
  
  .check_parameter_value_is_valid(x=settings$univar_metric, var_name="univariate_test_threshold_metric", values=c("p_value", "q_value"))
  
  # If the qvalue package is not available, switch to p_value if necessary
  if(!is_package_installed(name="qvalue", verbose=FALSE) & settings$univar_metric == "q_value" &
     "univar_test" %in% settings$filter_method){
    stop(paste("The qvalue package is not installed. Please install qvalue from Bioconductor",
               "to be able to use q_value as the univariate test threshold metric during",
               "preprocessing."))
  }
  
  
  # Maximum feature set size after univariate regression models
  settings$univar_feat_set_size <- .parse_arg(x_config=config$univariate_test_max_feature_set_size, x_var=univariate_test_max_feature_set_size,
                                              var_name="univariate_test_max_feature_set_size", type="integer", optional=TRUE, default=NULL)
  
  if(!is.null(settings$univar_feat_set_size)){
    .check_number_in_valid_range(x=settings$univar_feat_set_size, var_name="univariate_test_max_feature_set_size", range=c(1, Inf))
  }
  
  
  # Minimum amount of variance for inclusion of feature
  if("low_variance" %in% settings$filter_method){
    settings$low_var_threshold <- .parse_arg(x_config=config$low_var_minimum_variance_threshold, x_var=low_var_minimum_variance_threshold,
                                             var_name="low_var_minimum_variance_threshold", type="numeric", optional=FALSE)
    
    .check_number_in_valid_range(x=settings$low_var_threshold, var_name="low_var_minimum_variance_threshold", range=c(0.0, Inf))
  }
  
  
  # Maximum feature set size after variance thresholding
  settings$low_var_max_feature_set_size <- .parse_arg(x_config=config$low_var_max_feature_set_size, x_var=low_var_max_feature_set_size,
                                                      var_name="low_var_max_feature_set_size", type="integer", optional=TRUE, default=NULL)
  
  if(!is.null(settings$low_var_max_feature_set_size)){
    .check_number_in_valid_range(x=settings$low_var_max_feature_set_size, var_name="low_var_max_feature_set_size", range=c(1, Inf))
  }
  
  # Intraclass correlation coefficient (ICC) type for robustness analysis
  settings$robustness_icc_type <- .parse_arg(x_config=config$robustness_icc_type, x_var=robustness_icc_type, var_name="robustness_icc_type",
                                             type="character", optional=TRUE, default="1")
  
  .check_parameter_value_is_valid(x=settings$robustness_icc_type, var_name="robustness_icc_type",
                                  values=.get_available_icc_types())
  
  
  # ICC parameter to use for thresholding. Can be icc (estimated icc), icc_low (lower edge of the icc confidence interval), icc_panel (estimated panel icc), icc_panel_low (lower edge of the panel icc confidence interval)
  settings$robustness_threshold_param <- .parse_arg(x_config=config$robustness_threshold_metric, x_var=robustness_threshold_metric,
                                                    var_name="robustness_threshold_metric", type="character", optional=TRUE,
                                                    default="icc_low")
  
  .check_parameter_value_is_valid(x=settings$robustness_threshold_param, var_name="robustness_threshold_metric",
                                  values=c("icc", "icc_low", "icc_panel", "icc_panel_low"))
  
  
  # ICC value for thresholding.
  settings$robustness_threshold_value <- .parse_arg(x_config=config$robustness_threshold_value, x_var=robustness_threshold_value,
                                                    var_name="robustness_threshold_value", type="numeric", optional=TRUE, default=0.70)
  
  .check_number_in_valid_range(x=settings$robustness_threshold_value, var_name="robustness_threshold_value", range=c(-Inf, 1.0))
  
  
  # Data imputation method. For datasets smaller than 100 features we use lasso,
  # and simple imputation is used otherwise.
  default_imputation_method <- ifelse(get_n_features(data, outcome_type=outcome_type) < 100, "lasso", "simple")
  
  settings$imputation_method <- .parse_arg(x_config=config$imputation_method, x_var=imputation_method,
                                           var_name="imputation_method", type="character", optional=TRUE, default=default_imputation_method)
  
  .check_parameter_value_is_valid(x=settings$imputation_method, var_name="imputation_method", values=c("simple", "lasso"))

  
  # Transformation method
  settings$transform_method <- .parse_arg(x_config=config$transformation_method, x_var=transformation_method,
                                          var_name="transformation_method", type="character", optional=TRUE, default="yeo_johnson")
  
  .check_parameter_value_is_valid(x=settings$transform_method, var_name="transformation_method",
                                  values=c("none", "yeo_johnson", "yeo_johnson_trim", "yeo_johnson_winsor", "box_cox", "box_cox_trim", "box_cox_winsor"))
  
  # Normalisation method
  settings$normalisation_method <- .parse_arg(x_config=config$normalisation_method, x_var=normalisation_method,
                                              var_name="normalisation_method", type="character", optional=TRUE, default="standardisation")
  
  .check_parameter_value_is_valid(x=settings$normalisation_method, var_name="normalisation_method",
                                  values=.get_available_normalisation_methods())

  # Batch normalisation method
  settings$batch_normalisation_method <- .parse_arg(x_config=config$batch_normalisation_method, x_var=batch_normalisation_method,
                                                    var_name="batch_normalisation_method", type="character", optional=TRUE,
                                                    default="none")
  
  .check_parameter_value_is_valid(x=settings$batch_normalisation_method, var_name="batch_normalisation_method",
                                  values=.get_available_batch_normalisation_methods())
  
  # If the batch normalisation method is combat, pre-normalisation of the
  # entire data is required.
  if(settings$batch_normalisation_method %in% .get_available_batch_normalisation_methods(type="combat") &
     settings$normalisation_method %in% c("none", "mean_centering")){
    settings$normalisation_method <- "standardisation"
  }
  
  # Feature clustering
  settings$cluster_method <- .parse_arg(x_config=config$cluster_method, x_var=cluster_method,
                                        var_name="cluster_method", type="character", optional=TRUE, default="hclust")
  
  # Feature cluster linkage method
  settings$cluster_linkage <- .parse_arg(x_config=config$cluster_linkage_method, x_var=cluster_linkage_method,
                                         var_name="cluster_linkage_method", type="character", optional=TRUE, default="average")

  # Feature cluster cut method
  default_cluster_cut_method <- ifelse(settings$cluster_method == "pam", "silhouette", "fixed_cut")
  settings$cluster_cut_method <- .parse_arg(x_config=config$cluster_cut_method, x_var=cluster_cut_method,
                                            var_name="cluster_cut_method", type="character", optional=TRUE, default=default_cluster_cut_method)
  
  # Feature similarity metric which expresses some sort of correlation between a pair of features
  settings$cluster_similarity_metric <- .parse_arg(x_config=config$cluster_similarity_metric, x_var=cluster_similarity_metric,
                                                   var_name="cluster_similarity_metric", type="character", optional=TRUE, default="mcfadden_r2")
  
  # Feature cluster similarity that determines the similarity threshold for features to be considered part of one cluster.
  # Should be expressed in terms of the similarity metric, e.g. 0.8 for spearman would consider all features that have a pairwise correlation of 0.8 and over to belong to a cluster.
  settings$cluster_sim_thresh <- .parse_arg(x_config=config$cluster_similarity_threshold, x_var=cluster_similarity_threshold,
                                            var_name="cluster_similarity_threshold", type="numeric", optional=TRUE, default=NULL)
  
  if(is.null(settings$cluster_sim_thresh)){
    if(settings$cluster_cut_method %in% c("fixed_cut")){
      # Fixed cut requires stringent defaults, otherwise non-sense clusters will be produced
      if(settings$cluster_similarity_metric %in% c("mcfadden_r2")){
        settings$cluster_sim_thresh <- 0.30
      } else if(settings$cluster_similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2")){
        settings$cluster_sim_thresh <- 0.75
      } else {
        settings$cluster_sim_thresh <- 0.90
      }
    } else {
      # The similarity threshold is also used to determine if any clusters could potentially
      # be found. The threshold is set low so that other cut methods can be explored.
      if(settings$cluster_similarity_metric %in% c("mcfadden_r2")){
        settings$cluster_sim_thresh <- 0.05
      } else if(settings$cluster_similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2")) {
        settings$cluster_sim_thresh <- 0.40
      } else {
        settings$cluster_sim_thresh <- 0.50
      }
    }
  }
  
  
  # Method to select the feature that represents the cluster
  settings$cluster_repr_method <- .parse_arg(x_config=config$cluster_representation_method, x_var=cluster_representation_method,
                                             var_name="cluster_representation_method", type="character", optional=TRUE, default="best_predictor")
  
  # Partioning around medioids only allows the use of medioids for representation 
  if(settings$cluster_method == "pam"){
    settings$cluster_repr_method <- "medioid"
  }
  
  # If mean is used, this requires the data to have some kind of standardisation with centering at 0.
  if(settings$cluster_repr_method %in% c("mean")){
    if(!settings$normalisation_method %in% c("standardisation", "standardisation_trim", "standardisation_winsor", "quantile")){
      warning(paste0("When computing the meta-feature for a cluster using the mean value of co-clustered features, ",
                     "each feature is expected to be centered at 0.0 and to have a standard scale. ",
                     "The provided normalisation method (", settings$normalisation_method, ") does not allow this, ",
                     "and has been replaced by the standardisation method."))
      
      # Replace by standardisation_winsor
      settings$normalisation_method <- "standardisation"
    }
    
  }
  
  # Perform a check on all methods.
  .check_cluster_parameters(cluster_method=settings$cluster_method,
                            cluster_cut_method=settings$cluster_cut_method,
                            cluster_linkage=settings$cluster_linkage,
                            cluster_similarity_threshold=settings$cluster_sim_thresh,
                            cluster_similarity_metric=settings$cluster_similarity_metric,
                            cluster_representation_method=settings$cluster_repr_method,
                            var_type="cluster")
  
  
  # Parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_preprocessing, x_var=parallel_preprocessing,
                                     var_name="parallel_preprocessing", type="logical", optional=TRUE, default=TRUE)

  # Disable if parallel is FALSE.
  if(!parallel){ settings$do_parallel <- FALSE }

  return(settings)
}


#' Internal function for parsing settings related to feature selection
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param data Data set as loaded using the `.load_data` function.
#' @param parallel Logical value that whether familiar uses parallelisation. If
#'   `FALSE` it will override `parallel_feature_selection`.
#' @param outcome_type Type of outcome found in the data set.
#' @param fs_method (**required**) Feature selection method to be used for
#'   determining variable importance. `familiar` implements various feature
#'   selection methods. Please refer to the vignette on feature selection
#'   methods for more details.
#'
#'   More than one feature selection method can be chosen. The experiment will
#'   then repeated for each feature selection method.
#'
#'   Feature selection methods determines the ranking of features. Actual
#'   selection of features is done by optimising the signature size model
#'   hyperparameter during the hyperparameter optimisation step.
#' @param fs_method_parameter (*optional*) List of lists containing parameters
#'   for feature selection methods. Each sublist should have the name of the
#'   feature selection method it corresponds to.
#'
#'   Most feature selection methods do not have parameters that can be set.
#'   Please refer to the vignette on feature selection methods for more details.
#'   Note that if the feature selection method is based on a learner (e.g. lasso
#'   regression), hyperparameter optimisation may be performed prior to
#'   assessing variable importance.
#' @param vimp_aggregation_method (*optional*) The method used to aggregate
#'   variable importances over different data subsets, e.g. bootstraps. The
#'   following methods can be selected:
#'
#'   * `mean` (default): Use the mean rank of a feature over the subsets to
#'   determine the aggregated feature rank.
#'
#'   * `median`: Use the median rank of a feature over the subsets to determine
#'   the aggregated feature rank.
#'
#'   * `best`: Use the best rank the feature obtained in any subset to determine
#'   the aggregated feature rank.
#'
#'   * `worst`: Use the worst rank the feature obtained in any subset to
#'   determine the aggregated feature rank.
#'
#'   * `stability`: Use the frequency of the feature being in the subset of
#'   highly ranked features as measure for the aggregated feature rank
#'   (Meinshausen and Buehlmann, 2010).
#'
#'   * `exponential`: Use a rank-weighted frequence of occurrence in the subset
#'   of highly ranked features as measure for the aggregated feature rank (Haury
#'   et al., 2011).
#'
#'   * `borda`: Use the borda count as measure for the aggregated feature rank
#'   (Wald et al., 2012).
#'
#'   * `enhanced_borda`: Use an occurrence frequency-weighted borda count as
#'   measure for the aggregated feature rank (Wald et al., 2012).
#'
#'   * `truncated_borda`: Use borda count computed only on features within the
#'   subset of highly ranked features.
#'
#'   * `enhanced_truncated_borda`: Apply both the enhanced borda method and the
#'   truncated borda method and use the resulting borda count as the aggregated
#'   feature rank.
#'
#' @param vimp_aggregation_rank_threshold (*optional*) The threshold used to
#'   define the subset of highly important features. If not set, this threshold
#'   is determined by maximising the variance in the occurrence value over all
#'   features over the subset size.
#'
#'   This parameter is only relevant for `stability`, `exponential`,
#'   `enhanced_borda`, `truncated_borda` and `enhanced_truncated_borda` methods.
#' @param parallel_feature_selection (*optional*) Enable parallel processing for
#'   the feature selection workflow. Defaults to `TRUE`. When set to `FALSE`,
#'   this will disable the use of parallel processing while performing feature
#'   selection, regardless of the settings of the `parallel` parameter.
#'   `parallel_feature_selection` is ignored if `parallel=FALSE`.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to feature selection.
#'
#' @references 1. Wald, R., Khoshgoftaar, T. M., Dittman, D., Awada, W. &
#'   Napolitano, A. An extensive comparison of feature ranking aggregation
#'   techniques in bioinformatics. in 2012 IEEE 13th International Conference on
#'   Information Reuse Integration (IRI) 377–384 (2012).
#'
#'   1. Meinshausen, N. & Buehlmann, P. Stability selection. J. R. Stat. Soc.
#'   Series B Stat. Methodol. 72, 417–473 (2010).
#'
#'   1. Haury, A.-C., Gestraud, P. & Vert, J.-P. The influence of feature
#'   selection methods on accuracy, stability and interpretability of molecular
#'   signatures. PLoS One 6, e28210 (2011).
#' @md
#' @keywords internal
.parse_feature_selection_settings <- function(config=NULL, data, parallel, outcome_type,
                                              fs_method=waiver(),
                                              fs_method_parameter=waiver(),
                                              vimp_aggregation_method=waiver(),
                                              vimp_aggregation_rank_threshold=waiver(),
                                              parallel_feature_selection=waiver(),
                                              ...){
  settings <- list()
  
  # Feature selection methods
  settings$fs_methods <- .parse_arg(x_config=config$fs_method, x_var=fs_method,
                                    var_name="fs_method", type="character_list", optional=FALSE)
  
  sapply(settings$fs_methods, vimp.check_outcome_type, outcome_type=outcome_type)
  
  
  # Feature selection parameters
  settings$param <- .parse_arg(x_config=config$fs_method_parameter, x_var=fs_method_parameter,
                               var_name="fs_method_parameter", type="list", optional=TRUE, default=list())
  
  settings$param <- .parse_hyperparameters(data=data, parameter_list=settings$param,
                                           outcome_type=outcome_type, fs_method=settings$fs_methods)
  # sapply(settings$fs_methods, vimp.check_fs_parameters, user_param=settings$param, outcome_type=outcome_type)
  
  
  # Variable importance aggregation methods
  settings$aggregation <- .parse_arg(x_config=config$vimp_aggregation_method, x_var=vimp_aggregation_method,
                                     var_name="vimp_aggregation_method", type="character", optional=TRUE, default="mean")
  
  rank.check_aggregation_method(method=settings$aggregation)
  
  # Variable importance rank threshold (used by some aggregation methods)
  settings$aggr_rank_threshold <- .parse_arg(x_config=config$vimp_aggregation_rank_threshold, x_var=vimp_aggregation_rank_threshold,
                                             var_name="vimp_aggregation_rank_threshold", type="integer", optional=TRUE, default=NULL)
  
  if(!is.null(settings$aggr_rank_threshold)){
    .check_number_in_valid_range(x=settings$aggr_rank_threshold, var_name="vimp_aggregation_rank_threshold", range=c(1, Inf))
  }
  
  # Parallelisation switch for feature selection
  settings$do_parallel <- .parse_arg(x_config=config$parallel_feature_selection, x_var=parallel_feature_selection,
                                     var_name="parallel_feature_selection", type="logical", optional=TRUE, default=TRUE)
  
  # Disable if parallel is FALSE.
  if(!parallel){ settings$do_parallel <- FALSE }
  
  return(settings)
}



#' Internal function for parsing settings related to model development
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param data Data set as loaded using the `.load_data` function.
#' @param parallel Logical value that whether familiar uses parallelisation. If
#'   `FALSE` it will override `parallel_model_development`.
#' @param outcome_type Type of outcome found in the data set.
#' @param learner (**required**) One or more algorithms used for model
#'   development. A sizeable number learners is supported in `familiar`. Please
#'   see the vignette on learners for more information concerning the available
#'   learners.
#' @param hyperparameter (*optional*) List of lists containing hyperparameters
#'   for learners. Each sublist should have the name of the learner method it
#'   corresponds to, with list elements being named after the intended
#'   hyperparameter, e.g. \code{"glm_logistic"=list("sign_size"=3)}
#'
#'   All learners have hyperparameters. Please refer to the vignette on learners
#'   for more details. If no parameters are provided, sequential model-based
#'   optimisation is used to determine optimal hyperparameters.
#'
#'   Hyperparameters provided by the user are never optimised. However, if more
#'   than one value is provided for a single hyperparameter, optimisation will
#'   be conducted using these values.
#' @param parallel_model_development (*optional*) Enable parallel processing for
#'   the model development workflow. Defaults to `TRUE`. When set to `FALSE`,
#'   this will disable the use of parallel processing while developing models,
#'   regardless of the settings of the `parallel` parameter.
#'   `parallel_model_development` is ignored if `parallel=FALSE`.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to model development.
#' @md
#' @keywords internal
.parse_model_development_settings <- function(config=NULL, data, parallel, outcome_type,
                                              learner=waiver(),
                                              hyperparameter=waiver(),
                                              parallel_model_development=waiver(),
                                              ...){
  settings <- list()
  
  # Learners for model development
  settings$learners <- .parse_arg(x_config=config$learner, x_var=learner,
                                  var_name="learner", type="character_list", optional=FALSE)
  
  sapply(settings$learners, learner.check_outcome_type, outcome_type=outcome_type)
  
  
  # Model hyperparameters
  settings$hyper_param <- .parse_arg(x_config=config$hyperparameter, x_var=hyperparameter,
                                     var_name="hyperparameter", type="list", optional=TRUE, default=list())
  
  
  # sapply(settings$learners, learner.check_model_hyperparameters, user_param=settings$hyper_param, outcome_type=outcome_type)
  settings$hyper_param <- .parse_hyperparameters(data=data, parameter_list=settings$hyper_param,
                                                 outcome_type=outcome_type, learner=settings$learners)
  
  # Parallelisation switch for model building
  settings$do_parallel <- .parse_arg(x_config=config$parallel_model_development, x_var=parallel_model_development,
                                     var_name="parallel_model_development", type="logical", optional=TRUE, default=TRUE)
  
  # Disable if parallel is FALSE
  if(!parallel) { settings$do_parallel <- FALSE }
  
  return(settings)
}




#' Internal function for parsing settings related to model hyperparameter
#' optimisation
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param parallel Logical value that whether familiar uses parallelisation. If
#'   `FALSE` it will override `parallel_hyperparameter_optimisation`.
#' @param outcome_type Type of outcome found in the data set.
#' @param optimisation_bootstraps (*optional*) Number of bootstraps that should
#'   be generated from the development data set. During the optimisation
#'   procedure one or more of these bootstraps (indicated by
#'   `smbo_step_bootstraps`) are used for model development using different
#'   combinations of hyperparameters. The effect of the hyperparameters is then
#'   assessed by comparing in-bag and out-of-bag model performance.
#'
#'   The default number of bootstraps is `200`. Hyperparameter optimisation may
#'   finish before exhausting the set of boostraps.
#' @param smbo_random_initialisation (*optional*) Logical indicating random
#'   (`TRUE`) or grid-based (`FALSE`) initialisation of combinations of
#'   hyperparameters. If random initialisation is selected, up to 200 unique
#'   combinations will be randomly generated and assessed during the initial
#'   step of the sequential model-based boosting (SMBO) algorithm (Hutter et
#'   al., 2011). If a grid-based initialisation is selected, up to 100
#'   permutations will be randomly selected from the complete parameter grid and
#'   assessed during the initial SMBO step.
#'
#'   The default is `TRUE` (random initialisation). Grid-based initialisation
#'   uses pre-defined parameter ranges.
#' @param max_smbo_iterations (*optional*) Maximum number of intensify
#'   iterations of the SMBO algorithm. During an intensify iteration a run-off
#'   occurs between the current *best* hyperparameter combination and either 10
#'   challenger combination with the highest expected improvement or a set of 20
#'   random combinations.
#'
#'   Run-off with random combinations is used to force exploration of the
#'   hyperparameter space, and is performed every second intensify iteration, or
#'   if there is no expected improvement for any challenger combination.
#'
#'   If a combination of hyperparameters leads to better performance on the same
#'   data than the incumbent *best* set of hyperparameters, it replaces the
#'   incumbent set at the end of the intensify iteration.
#'
#'   The default number of intensify iteration is `30`. Iterations may be
#'   stopped early if the incumbent set of hyperparameters remains the same for
#'   `smbo_stop_convergent_iterations` iterations, or performance improvement is
#'   minimal. This behaviour is suppressed during the first 4 iterations to
#'   enable the algorithm to explore the hyperparameter space.
#' @param smbo_stop_convergent_iterations (*optional*) The number of subsequent
#'   convergent SMBO iterations required to stop hyperparameter optimisation
#'   early. The default value is `3`.
#' @param smbo_step_bootstraps (*optional*) The number of bootstraps taken from
#'   the set of `optimisation_bootstraps` bootstraps as data for the initial
#'   SMBO step and the steps in each intensify iteration.
#'
#'   The default value is `5`. The value cannot be larger than
#'   `optimisation_bootstraps`.
#' @param smbo_intensify_steps (*optional*) The number of steps in each SMBO
#'   intensify iteration. Each step a new set of `smbo_step_bootstraps`
#'   bootstraps is drawn and used in the run-off between the incumbent *best*
#'   hyperparameter combination and its challengers.
#'
#'   The default value is `3`. Higher numbers allow for a more detailed
#'   comparison, but this comes with added computational cost.
#' @param smbo_intensify_stop_p_value (*optional*) The p-value threshold which
#'   is used to test the hypothesis that a challenger hyperparameter set and the
#'   incumbent set have the same performance. A paired Wilcoxon test is
#'   performed, with the alternative hypothesis that the challenger set is less
#'   performant.
#'
#'   The p-value from the test is compared to `smbo_intensify_stop_p_value` and
#'   if it is found lower, the challenger set is eliminated and not used in any
#'   further intensify steps during the current iteration. Elimination of sets
#'   of hyperparameters that are unlikely to lead to better models improves
#'   computational efficiency.
#'
#'   The default value is `0.05`.
#' @param objective (*optional*) Type of objective used to determine the
#'   performance of a hyperparameter set. Model performance is assessed using
#'   the metric specified by `optimisation_metric` on the in-bag and out-of-bag
#'   samples of a bootstrap. These will be referred to as \eqn{s_{ib}} and
#'   \eqn{s_{oob}}, respectivily. The method indicated by `objective` computes a
#'   objective score from each pair of values. The following objective methods
#'   are available:
#'
#'   * `max_validation`: Uses the out-of-bag validation score \eqn{s_{oob}} as
#'   objective. This is a standard machine learning objective.
#'
#'   * `balanced` (default): Computes \eqn{s_{oob} - |s_{oob} - s_{ib}|}. This
#'   objective forces the algorithm to consider hyperparameter sets that perform
#'   well on both development and validation data.
#'
#'   * `stronger_balance`: Computes \eqn{s_{oob} - 2.0 |s_{oob} - s_{ib}|}.
#'   Stronger penalty than in the `balance` objective.
#'
#' @param optimisation_metric (*optional*) Type of metric used to compute
#'   performance scores. See the vignette on performance metrics for the
#'   available metrics.
#'
#'   If unset, the following metrics are used by default:
#'
#'   * `auc_roc`: For `binomial` and `multinomial` models.
#'
#'   * `mse`: Mean squared error for `continuous` models.
#'
#'   * `msle`: Mean squared logarithmic error for `count` models.
#'
#'   * `concordance_index`: For `survival` models.
#'
#'   Currently, only one optimisation metric can be specified. Note that instead
#'   of computing the actual metric, an objective version is computed for
#'   hyperparameter optimisation, so that better performance always yields
#'   higher values.
#' @param parallel_hyperparameter_optimisation (*optional*) Enable parallel
#'   processing for hyperparameter optimisation. Defaults to `TRUE`. When set to
#'   `FALSE`, this will disable the use of parallel processing while performing
#'   optimisation, regardless of the settings of the `parallel` parameter.
#'   `parallel_hyperparameter_optimisation` is ignored if `parallel=FALSE`.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to model hyperparameter optimisation.
#'
#' @references 1. Hutter, F., Hoos, H. H. & Leyton-Brown, K. Sequential
#'   model-based optimization for general algorithm configuration. in Learning
#'   and Intelligent Optimization (ed. Coello, C. A. C.) 6683, 507–523 (Springer
#'   Berlin Heidelberg, 2011).
#' @md
#' @keywords internal
.parse_hyperparameter_optimisation_settings <- function(config=NULL, parallel, outcome_type,
                                                        optimisation_bootstraps=waiver(),
                                                        smbo_random_initialisation=waiver(),
                                                        max_smbo_iterations=waiver(),
                                                        smbo_stop_convergent_iterations=waiver(),
                                                        smbo_step_bootstraps=waiver(),
                                                        smbo_intensify_steps=waiver(),
                                                        smbo_intensify_stop_p_value=waiver(),
                                                        objective=waiver(),
                                                        optimisation_metric=waiver(),
                                                        parallel_hyperparameter_optimisation=waiver(),
                                                        ...){
  settings <- list()
  
  # Randomisation of initial parameter grid
  settings$hpo_randomise_init_grid <- .parse_arg(x_config=config$smbo_random_initialisation, x_var=smbo_random_initialisation,
                                                 var_name="smbo_random_initialisation", type="logical", optional=TRUE, default=TRUE)
  
  
  # Maximum number of bootstraps for hyperparameter evaluation
  settings$hpo_max_bootstraps <- .parse_arg(x_config=config$optimisation_bootstraps, x_var=optimisation_bootstraps,
                                            var_name="optimisation_bootstraps", type="integer", optional=TRUE, default=200)
  
  .check_number_in_valid_range(x=settings$hpo_max_bootstraps, var_name="optimisation_bootstraps", range=c(20, Inf))
  
  
  # Maximum number of SMBO iterations before stopping
  settings$hpo_smbo_iter_max <- .parse_arg(x_config=config$max_smbo_iterations, x_var=max_smbo_iterations,
                                           var_name="max_smbo_iterations", type="integer", optional=TRUE, default=20)
  
  .check_number_in_valid_range(x=settings$hpo_smbo_iter_max, var_name="max_smbo_iterations", range=c(1, Inf))
  
  
  # Maximum number of bootstrap evaluated initially and during each intensify step of each SMBO iteration
  settings$hpo_bootstraps <- .parse_arg(x_config=config$smbo_step_bootstraps, x_var=smbo_step_bootstraps,
                                        var_name="smbo_step_bootstraps", type="integer", optional=TRUE, default=5)
  
  .check_number_in_valid_range(x=settings$hpo_bootstraps, var_name="smbo_step_bootstraps", range=c(1, settings$hpo_max_bootstraps))
  
  
  # Maximum number of intensify iterations during each SMBO iteration
  settings$hpo_intensify_max_iter <- .parse_arg(x_config=config$smbo_intensify_steps, x_var=smbo_intensify_steps,
                                                var_name="smbo_intensify_steps", type="integer", optional=TRUE, default=3)
  
  .check_number_in_valid_range(x=settings$hpo_intensify_max_iter, var_name="smbo_intensify_steps", range=c(1, Inf))
  

  # Significance level for early stopping of intensity iterations
  settings$hpo_alpha <- .parse_arg(x_config=config$smbo_intensify_stop_p_value, x_var=smbo_intensify_stop_p_value,
                                   var_name="smbo_intensify_stop_p_value", type="numeric", optional=TRUE, default=0.05)
  
  .check_number_in_valid_range(x=settings$hpo_alpha, var_name="smbo_intensify_stop_p_value", range=c(0.0, 1.0), closed=c(FALSE, TRUE))
  
  
  # Number of convergant iterations before stopping SMBO
  settings$hpo_conv_stop <- .parse_arg(x_config=config$smbo_stop_convergent_iterations, x_var=smbo_stop_convergent_iterations,
                                       var_name="smbo_stop_convergent_iterations", type="integer", optional=TRUE, default=3)

  .check_number_in_valid_range(x=settings$hpo_conv_stop, var_name="smbo_stop_convergent_iterations", range=c(1, Inf))
  
  
  # Objective function
  settings$hpo_objective <- .parse_arg(x_config=config$objective, x_var=objective,
                                       var_name="objective", type="character", optional=TRUE, default="balanced")
  
  .check_parameter_value_is_valid(x=settings$hpo_objective, var_name="objective",
                                  values=c("max_validation", "balanced", "stronger_balance"))
  
  
  # Performance metric for hyperparameter optimisation
  settings$hpo_metric <- .parse_arg(x_config=config$optimisation_metric, x_var=optimisation_metric,
                                    var_name="optimisation_metric", type="character", optional=TRUE, default=NULL)
  
  # Set default metric
  if(is.null(settings$hpo_metric)){
    if(outcome_type %in% c("binomial", "multinomial")){
      settings$hpo_metric <- "auc_roc"
    } else if(outcome_type == "continuous"){
      settings$hpo_metric <- "mse"
    } else if(outcome_type == "count"){
      settings$hpo_metric <- "msle"
    } else if(outcome_type == "survival"){
      settings$hpo_metric <- "concordance_index"
    } else if(outcome_type == "competing_risk"){
      ..error_outcome_type_not_implemented(outcome_type)
    } else {
      ..error_no_known_outcome_type(outcome_type)
    }
  }
  
  # Check if the metric is ok. Packed into a for loop to enable multi-metric optimisation in the future
  sapply(settings$hpo_metric, metric.check_outcome_type, outcome_type=outcome_type)
  
  # Parallelisation switch for parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_hyperparameter_optimisation, x_var=parallel_hyperparameter_optimisation,
                                     var_name="parallel_hyperparameter_optimisation", type="logical", optional=TRUE, default=TRUE)
  
  # Disable if parallel is FALSE
  if(!parallel) { settings$do_parallel <- FALSE }
  
  return(settings) 
}





#'Internal function for parsing settings related to model evaluation
#'
#'@param config A list of settings, e.g. from an xml file.
#'@param data Data set as loaded using the `.load_data` function.
#'@param parallel Logical value that whether familiar uses parallelisation. If
#'  `FALSE` it will override `parallel_evaluation`.
#'@param outcome_type Type of outcome found in the data set.
#'@param hpo_metric Metric defined for hyperparameter optimisation.
#'@param development_batch_id Identifiers of batches used for model development.
#'  These identifiers are used to determine the cohorts used to determine a
#'  setting for `time_max`, if the `outcome_type` is `survival`, and both
#'  `time_max` and `evaluation_times` are not provided.
#'@param vimp_aggregation_method Method for variable importance aggregation that
#'  was used for feature selection.
#'@param vimp_aggregation_rank_threshold Rank threshold for variable importance
#'  aggregation used during feature selection.
#'@param prep_cluster_method Cluster method used during pre-processing.
#'@param prep_cluster_linkage_method Cluster linkage method used during
#'  pre-processing.
#'@param prep_cluster_cut_method Cluster cut method used during pre-processing.
#'@param prep_cluster_similarity_threshold Cluster similarity threshold used
#'  during pre-processing.
#'@param prep_cluster_similarity_metric Cluster similarity metric used during
#'@param ensemble_method (*optional*) Method for ensembling predictions from
#'  models for the same sample. Available methods are:
#'
#'  * `mean` (default): Use the mean of the predicted values as the ensemble
#'  value for a sample.
#'
#'@param evaluation_metric (*optional*) One or more metrics for assessing model
#'  performance. See the vignette on performance metrics for the available
#'  metrics.
#'
#'  Confidence intervals (or rather credibility intervals) are computed for each
#'  metric during evaluation. This is done using bootstraps, the number of which
#'  depends on the value of `confidence_alpha` (Davison and Hinkley, 1997).
#'
#'  If unset, the metric in the `optimisation_metric` variable is used.
#'
#'@param confidence_alpha (*optional*) Numeric value for the alpha level at
#'  which confidence (or credibility intervals) intervals are determined using
#'  bootstrap estimation. The number of bootstraps depend on `confidence_alpha`.
#'  `familiar` uses the rule of thumb \eqn{n = 20 / \alpha} to determine the
#'  number of required bootstraps.
#'
#'  The default value is `0.05`.
#'
#'@param feature_cluster_method (*optional*) Method used to perform clustering
#'  of features. The same methods as for the `cluster_method` configuration
#'  parameter are available: `none`, `hclust`, `agnes`, `diana` and `pam`.
#'
#'  The value for the `cluster_method` configuration parameter is used by
#'  default. When generating clusters for the purpose of determining mutual
#'  correlation and ordering feature expressions, `none` is ignored and `hclust`
#'  is used instead.
#'
#'@param feature_linkage_method (*optional*) Method used for agglomerative
#'  clustering with `hclust` and `agnes`. Linkage determines how features are
#'  sequentially combined into clusters based on distance. The methods are
#'  shared with the `cluster_linkage_method` configuration parameter: `average`,
#'  `single`, `complete`, `weighted`, and `ward`.
#'
#'  The value for the `cluster_linkage_method` configuration parameters is used
#'  by default.
#'
#'@param feature_cluster_cut_method (*optional*) Method used to divide features
#'  into separate clusters. The available methods are the same as for the
#'  `cluster_cut_method` configuration parameter: `silhouette`, `fixed_cut` and
#'  `dynamic_cut`.
#'
#'  `silhouette` is available for all cluster methods, but `fixed_cut` only
#'  applies to methods that create hierarchical trees (`hclust`, `agnes` and
#'  `diana`). `dynamic_cut` requires the `dynamicTreeCut` package and can only
#'  be used with `agnes` and `hclust`.
#'
#'  The value for the `cluster_cut_method` configuration parameter is used by
#'  default.
#'
#'@param feature_similarity_metric (*optional*) Metric to determine pairwise
#'  similarity between features. Similarity is computed in the same manner as
#'  for clustering, and `feature_similarity_metric` therefore has the same
#'  options as `cluster_similarity_metric`: `mcfadden_r2`, `cox_snell_r2`,
#'  `nagelkerke_r2`, `spearman`, `kendall` and `pearson`.
#'
#'  The value used for the `cluster_similarity_metric` configuration parameter
#'  is used by default.
#'
#'@param feature_similarity_threshold (*optional*) The threshold level for
#'  pair-wise similarity that is required to form feature clusters with the
#'  `fixed_cut` method. This threshold functions in the same manner as the one
#'  defined using the `cluster_similarity_threshold` parameter.
#'
#'  By default, the value for the `cluster_similarity_threshold` configuration
#'  parameter is used.
#'
#'@param sample_cluster_method (*optional*) The method used to perform
#'  clustering based on distance between samples. These are the same methods as
#'  for the `cluster_method` configuration parameter: `hclust`, `agnes`, `diana`
#'  and `pam`.
#'
#'  The value for the `cluster_method` configuration parameter is used by
#'  default. When generating clusters for the purpose of ordering samples in
#'  feature expressions, `none` is ignored and `hclust` is used instead.
#'
#'@param sample_linkage_method (*optional*) The method used for agglomerative
#'  clustering in `hclust` and `agnes`. These are the same methods as for the
#'  `cluster_linkage_method` configuration parameter: `average`, `single`,
#'  `complete`, `weighted`, and `ward`.
#'
#'  The value for the `cluster_linkage_method` configuration parameters is used
#'  by default.
#'
#'@param sample_similarity_metric (*optional*) Metric to determine pairwise
#'  similarity between samples. Similarity is computed in the same manner as for
#'  clustering, but `sample_similarity_metric` has different options that are
#'  better suited to computing distance between samples instead of between
#'  features. The following metrics are available.
#'
#'  * `gower` (default): compute Gower's distance between samples. By default,
#'  Gower's distance is computed based on winsorised data to reduce the effect
#'  of outliers (see below).
#'
#'  * `euclidean`: compute the Euclidean distance between samples.
#'
#'  The underlying feature data for numerical features is scaled to the
#'  \eqn{[0,1]} range using the feature values across the samples. The
#'  normalisation parameters required can optionally be computed from feature
#'  data with the outer 5% (on both sides) of feature values trimmed or
#'  winsorised. To do so append `_trim` (trimming) or `_winsor` (winsorising) to
#'  the metric name. This reduces the effect of outliers somewhat.
#'
#'  Regardless of metric, all categorical features are handled as for the
#'  Gower's distance: distance is 0 if the values in a pair of samples match,
#'  and 1 if they do not.
#'
#'@param eval_aggregation_method (*optional*) Method for aggregating variable
#'  importances for the purpose of evaluation. Variable importances are
#'  determined during feature selection steps and after training the model. Both
#'  types are evaluated, but feature selection variable importance is only
#'  evaluated at run-time.
#'
#'  See the documentation for the `vimp_aggregation_method` argument for
#'  information concerning the different methods available.
#'@param eval_aggregation_rank_threshold (*optional*) The threshold used to
#'  define the subset of highly important features during evaluation.
#'
#'  See the documentation for the `vimp_aggregation_rank_threshold` argument for
#'  more information.
#'@param eval_icc_type (*optional*) String indicating the type of intraclass
#'  correlation coefficient (`1`, `2` or `3`) that should be used to compute
#'  robustness for features in repeated measurements during the evaluation of
#'  univariate importance. These types correspond to the types in Shrout and
#'  Fleiss (1979). The default value is `1`.
#'@param stratification_method (*optional*) Method for determining the
#'  stratification threshold for creating survival groups. The actual,
#'  model-dependent, threshold value is obtained from the development data, and
#'  can afterwards be used to perform stratification on validation data.
#'
#'  The following stratification methods are available:
#'
#'  * `median` (default): The median predicted value in the development cohort
#'  is used to stratify the samples into two risk groups.
#'
#'  * `fixed`: Samples are stratified based on the sample quantiles of the
#'  predicted values. These quantiles are defined using the
#'  `stratification_threshold` parameter.
#'
#'  * `optimised`: Use maximally selected rank statistics to determine the
#'  optimal threshold (Lausen and Schumacher, 1992; Hothorn et al., 2003) to
#'  stratify samples into two optimally separated risk groups.
#'
#'  One or more stratification methods can be selected simultaneously.
#'
#'  This parameter is only relevant for `survival` outcomes.
#'@param stratification_threshold (*optional*) Numeric value(s) signifying the
#'  sample quantiles for stratification using the `fixed` method. The number of
#'  risk groups will be the number of values +1.
#'
#'  The default value is `c(1/3, 2/3)`, which will yield two thresholds that
#'  divide samples into three equally sized groups. If `fixed` is not among the
#'  selected stratification methods, this parameter is ignored.
#'
#'  This parameter is only relevant for `survival` outcomes.
#'
#'@param stratification_ensemble_method (*optional*) Method for ensembling the
#'  risk group assignments from different models for the same sample.
#'
#'  The following methods are available:
#'
#'  * `ensemble_mean`: Risk groups are determined for each sample using the
#'  threshold values of the different models in the ensemble. The risk groups
#'  are treated as ordinal and encoded using integer values. The mean of the
#'  encoded values is computed. The risk group bin containing the mean value is
#'  then used as the ensemble-based risk group.
#'
#'  * `ensemble_mode` (default): Risk groups are determined for each sample
#'  using the threshold values of the different models in the ensemble. The most
#'  commonly assigned risk group is then used as the ensemble-based risk group.
#'
#'  * `median_threshold`: The median threshold value for each risk group is
#'  determined from the threshold values of the different models in the
#'  ensemble. The resulting threshold(s) are then applied to the ensemble
#'  prediction of a sample to identify the ensemble-based risk group.
#'
#'  * `mean_threshold`: Similar to `median_threshold`, but uses the mean
#'  threshold value for each risk group.
#'
#'  This parameter is only relevant for `survival` outcomes.
#'@param time_max (*optional*) Time point which is used as the benchmark for
#'  e.g. cumulative risks generated by random forest, or the cutoff for Uno's
#'  concordance index.
#'
#'  If `time_max` is not provided, but `evaluation_times` is, the largest value
#'  of `evaluation_times` is used. If both are not provided, `time_max` is set
#'  to the 98th percentile of the distribution of survival times for samples
#'  with an event in the development data set.
#'
#'  This parameter is only relevant for `survival` outcomes.
#'@param evaluation_times (*optional*) One or more time points that are used for
#'  assessing calibration in survival problems. This is done as expected and
#'  observed survival probabilities depend on time.
#'
#'  If unset, `evaluation_times` will be equal to `time_max`.
#'
#'  This parameter is only relevant for `survival` outcomes.
#'@param parallel_evaluation (*optional*) Enable parallel processing for
#'  hyperparameter optimisation. Defaults to `TRUE`. When set to `FALSE`, this
#'  will disable the use of parallel processing while performing optimisation,
#'  regardless of the settings of the `parallel` parameter.
#'  `parallel_evaluation` is ignored if `parallel=FALSE`.
#'@param ... Unused arguments.
#'
#'@return List of parameters related to model evaluation.
#'
#'@references 1. Davison, A. C. & Hinkley, D. V. Bootstrap methods and their
#'  application. (Cambridge University Press, 1997).
#'
#'  1. Lausen, B. & Schumacher, M. Maximally Selected Rank Statistics.
#'  Biometrics 48, 73 (1992).
#'
#'  1. Hothorn, T. & Lausen, B. On the exact distribution of maximally selected
#'  rank statistics. Comput. Stat. Data Anal. 43, 121–137 (2003).
#'@md
#'@keywords internal
.parse_evaluation_settings <- function(config=NULL, data, parallel, outcome_type, hpo_metric, development_batch_id,
                                       vimp_aggregation_method, vimp_aggregation_rank_threshold,
                                       prep_cluster_method,
                                       prep_cluster_linkage_method,
                                       prep_cluster_cut_method,
                                       prep_cluster_similarity_threshold,
                                       prep_cluster_similarity_metric,
                                       ensemble_method=waiver(),
                                       evaluation_metric=waiver(),
                                       confidence_alpha=waiver(),
                                       feature_cluster_method=waiver(),
                                       feature_cluster_cut_method=waiver(),
                                       feature_linkage_method=waiver(),
                                       feature_similarity_metric=waiver(),
                                       feature_similarity_threshold=waiver(),
                                       sample_cluster_method=waiver(),
                                       sample_linkage_method=waiver(),
                                       sample_similarity_metric=waiver(),
                                       eval_aggregation_method=waiver(),
                                       eval_aggregation_rank_threshold=waiver(),
                                       eval_icc_type=waiver(),
                                       stratification_method=waiver(),
                                       stratification_threshold=waiver(),
                                       stratification_ensemble_method=waiver(),
                                       time_max=waiver(),
                                       evaluation_times=waiver(),
                                       parallel_evaluation=waiver(),
                                       ...){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_event <- cohort_id <- NULL
  
  settings <- list()
  
  # Method for ensemble predictions
  settings$ensemble_method <- .parse_arg(x_config=config$ensemble_method, x_var=ensemble_method,
                                         var_name="ensemble_method", type="character", optional=TRUE, default="mean")
  
  .check_parameter_value_is_valid(x=settings$ensemble_method, var_name="ensemble_method",
                                  values=.get_available_ensemble_prediction_methods())
  
  
  # List of performance metrics for evaluation
  settings$metric <- .parse_arg(x_config=config$evaluation_metric, x_var=evaluation_metric,
                                var_name="evaluation_metric", type="character_list", optional=TRUE, default=hpo_metric)
  
  sapply(settings$metric, metric.check_outcome_type, outcome_type=outcome_type)
  
  
  # Width of the confidence intervals
  settings$metric_alpha <- .parse_arg(x_config=config$confidence_alpha, x_var=confidence_alpha,
                                      var_name="confidence_alpha", type="numeric", optional=TRUE, default=0.05)
  
  .check_number_in_valid_range(x=settings$metric_alpha, var_name="confidence_alpha", range=c(0.0, 1.0), closed=c(FALSE, FALSE))
  
  # Feature cluster method
  settings$feature_cluster_method <- .parse_arg(x_config=config$feature_cluster_method, x_var=feature_cluster_method,
                                                var_name="feature_cluster_method", type="character", optional=TRUE, default=prep_cluster_method)
  
  # Feature linkage method
  settings$feature_linkage_method <- .parse_arg(x_config=config$feature_linkage_method, x_var=feature_linkage_method,
                                                var_name="feature_linkage_method", type="character", optional=TRUE, default=prep_cluster_linkage_method)
  
  # Feature cluster cluster cut method
  settings$feature_cluster_cut_method <- .parse_arg(x_config=config$feature_cut_method, x_var=feature_cluster_cut_method,
                                                    var_name="feature_cluster_cut_method", type="character", optional=TRUE, default=prep_cluster_cut_method)
  
  # Feature similarity metric
  settings$feature_similarity_metric <- .parse_arg(x_config=config$feature_similarity_metric, x_var=feature_similarity_metric,
                                                   var_name="feature_similarity_metric", type="character", optional=TRUE, default=prep_cluster_similarity_metric)
  
  # Feature similarity threshold
  settings$feature_similarity_threshold <- .parse_arg(x_config=config$feature_similarity_threshold, x_var=feature_similarity_threshold,
                                                      var_name="feature_similarity_threshold", type="numeric", optional=TRUE, default=prep_cluster_similarity_threshold)
  
  .check_cluster_parameters(cluster_method=settings$feature_cluster_method,
                            cluster_linkage=settings$feature_linkage_method,
                            cluster_cut_method=settings$feature_cluster_cut_method,
                            cluster_similarity_threshold=settings$feature_similarity_threshold,
                            cluster_similarity_metric=settings$feature_similarity_metric,
                            var_type="feature")
  
  
  # Sample cluster method
  settings$sample_cluster_method <- .parse_arg(x_config=config$sample_cluster_method, x_var=sample_cluster_method,
                                               var_name="sample_cluster_method", type="character", optional=TRUE, default=prep_cluster_method)
  
  # Sample cluster linkage method
  settings$sample_linkage_method <- .parse_arg(x_config=config$sample_linkage_method, x_var=sample_linkage_method,
                                               var_name="sample_linkage_method", type="character", optional=TRUE, default=prep_cluster_linkage_method)
  
  # Sample similarity metric
  settings$sample_similarity_metric <- .parse_arg(x_config=config$sample_similarity_metric, x_var=sample_similarity_metric,
                                                  var_name="sample_similarity_metric", type="character", optional=TRUE, default="gower_winsor")
  
  .check_cluster_parameters(cluster_method=settings$sample_cluster_method,
                            cluster_linkage=settings$sample_linkage_method,
                            cluster_similarity_metric=settings$sample_similarity_metric,
                            var_type="sample")
  
  # Variable importance aggregation methods
  settings$aggregation <- .parse_arg(x_config=config$eval_aggregation_method, x_var=eval_aggregation_method,
                                     var_name="eval_aggregation_method", type="character", optional=TRUE, default=vimp_aggregation_method)
  
  rank.check_aggregation_method(method=settings$aggregation)
  
  # Variable importance rank threshold (used by some aggregation methods)
  settings$aggr_rank_threshold <- .parse_arg(x_config=config$eval_aggregation_rank_threshold, x_var=eval_aggregation_rank_threshold,
                                             var_name="eval_aggregation_rank_threshold", type="integer", optional=TRUE, default=vimp_aggregation_rank_threshold)
  
  if(!is.null(settings$aggr_rank_threshold)){
    .check_number_in_valid_range(x=settings$aggr_rank_threshold, var_name="eval_aggregation_rank_threshold", range=c(1, Inf))
  }
  
  
  # Type of ICC computed for univariate analysis.
  settings$icc_type <- .parse_arg(x_config=config$eval_icc_type, x_var=eval_icc_type,
                                  var_name="eval_icc_type", type="character", optional=TRUE, default="1")
  
  .check_parameter_value_is_valid(x=settings$icc_type, var_name="eval_icc_type", values=.get_available_icc_types())
  
  
  # Method used to set stratification thresholds for Kaplan-Meier analysis
  settings$strat_method <- .parse_arg(x_config=config$stratification_method, x_var=stratification_method,
                                      var_name="stratification_method", type="character_list", optional=TRUE, default="median")
  
  .check_parameter_value_is_valid(x=settings$strat_method, var_name="stratification_method",
                                  values=c("median", "fixed", "optimised"))
  
  
  # Quantile stratification thresholds for the "fixed" stratification method. Note that c(0.333, 0.667) means
  # that 3 groups are created: a low risk group (up to 0.333), a moderate risk group (between 0.333 and 0.667),
  # and a high risk group (0.667)
  settings$strat_quant_threshold <- .parse_arg(x_config=config$stratification_threshold, x_var=stratification_threshold,
                                               var_name="stratification_threshold", type="numeric_list", optional=TRUE, default=c(1/3, 2/3))
  
  sapply(settings$strat_quant_threshold, .check_number_in_valid_range, var_name="stratification_threshold", range=c(0.0, 1.0), closed=c(FALSE, FALSE))
  
  
  # Method used to ensemble predicted stratifications by multiple models.
  settings$strat_ensemble_method <- .parse_arg(x_config=config$stratification_ensemble_method, x_var=stratification_ensemble_method,
                                               var_name="stratification_ensemble_method", type="character", optional=TRUE, default="ensemble_mode")
  
  .check_parameter_value_is_valid(x=settings$strat_ensemble_method, var_name="stratification_ensemble_method",
                                  values=.get_available_risk_ensemble_methods())

    
  # Study end time (this is used for plotting, and Uno's concordance index)
  settings$time_max <- .parse_arg(x_config=config$time_max, x_var=time_max,
                                  var_name="time_max", type="numeric", optional=TRUE, default=NULL)
  
  # Times at which calibration is evaluated for time-to-event (survival) data.
  settings$eval_times <- .parse_arg(x_config=config$evaluation_times, evaluation_times,
                                    var_name="evaluation_time", type="numeric_list", optional=TRUE, default=NULL)

  # Update time_max and eval_times only if we are dealing with survival endpoints.
  if(outcome_type %in% c("survival")){
  
    # Identify values for time_max if it has not been provided.
    if(is.null(settings$time_max)){
      if(!is.null(settings$eval_times)){
        # Use maximum evaluation time
        settings$time_max <- max(settings$eval_times)
      } else {
        # 98th percentile of all outcome times.
        settings$time_max <- stats::quantile(data[outcome_event==1 & cohort_id %in% development_batch_id]$outcome_time, probs=0.98, names=FALSE)
      }
    }
    
    .check_number_in_valid_range(settings$time_max, var_name="time_max", range=c(0.0, Inf), closed=c(FALSE, TRUE))
    
    # Identify evaluation times if they were not provided.
    if(is.null(settings$eval_times)){
      settings$eval_times <- settings$time_max
    }
    
    sapply(settings$eval_times, .check_number_in_valid_range, var_name="evaluation_time", range=c(0.0, Inf), closed=c(FALSE, TRUE))
  }
  
  # Parallelisation switch for parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_evaluation, x_var=parallel_evaluation,
                                     var_name="parallel_evaluation", type="logical", optional=TRUE, default=TRUE)
  
  # Disable if parallel is FALSE
  if(!parallel) { settings$do_parallel <- FALSE }
  
  # Return list of settings
  return(settings)
}
