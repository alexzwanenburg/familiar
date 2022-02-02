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
  file_paths <- list()
  
  # Read project directory; if none is provided through the configuration file
  # or the function input, defer to the R temp directory
  project_dir <- .parse_arg(x_config=config$paths$project_dir,
                            x_var=project_dir,
                            var_name="project_dir",
                            type="character",
                            optional=TRUE,
                            default=NULL)
  
  if(!is.null(project_dir)) {
    project_dir <- normalizePath(project_dir, mustWork=TRUE)
    file_paths$is_temporary <- FALSE
    
  } else {
    temporary_directory <- file.path(tempdir(), "familiar", stringi::stri_rand_strings(1, 8))
    project_dir <- normalizePath(temporary_directory, mustWork=FALSE)
    file_paths$is_temporary <- TRUE
  }
  
  # Read experiment directory path and create the directory if required
  experiment_dir <- .parse_arg(x_config=config$paths$experiment_dir,
                               x_var=experiment_dir,
                               var_name="experiment_dir",
                               type="character",
                               optional=TRUE,
                               default="")
  
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
  
  file_paths$experiment_dir <- experiment_dir
  if(!dir.exists(file_paths$experiment_dir)) dir.create(file_paths$experiment_dir, recursive=TRUE)
  
  # Log file - set as global variable as well
  if(!file_paths$is_temporary) file_paths$log_file <- normalizePath(file.path(experiment_dir, "log.txt"), mustWork=FALSE)
  
  # Assign to global environment.
  assign("log_file", file_paths$log_file, envir=familiar_global_env)
  
  # Directory for iterations
  file_paths$iterations_dir <- normalizePath(experiment_dir, mustWork=FALSE)
  if(!dir.exists(file_paths$iterations_dir)) dir.create(file_paths$iterations_dir)
  
  # Directory for pre-processing
  file_paths$process_data_dir <- normalizePath(experiment_dir, mustWork=FALSE)
  if(!dir.exists(file_paths$process_data_dir)) dir.create(file_paths$process_data_dir)
  
  # Directory for feature selection
  file_paths$fs_dir <- normalizePath(file.path(experiment_dir, "variable_importance"),mustWork=FALSE)
  if(!dir.exists(file_paths$fs_dir)) dir.create(file_paths$fs_dir)
  
  # Directory and files for model building
  file_paths$mb_dir <- normalizePath(file.path(experiment_dir, "trained_models"), mustWork=FALSE)
  if(!dir.exists(file_paths$mb_dir)) dir.create(file_paths$mb_dir)
  
  # Directory for familiarData objects
  file_paths$fam_data_dir <- normalizePath(file.path(experiment_dir, "familiar_data"), mustWork=FALSE)
  if(!dir.exists(file_paths$fam_data_dir)) dir.create(file_paths$fam_data_dir)
  
  # Directory for familiarDataCollection objects
  file_paths$fam_coll_dir <- normalizePath(file.path(experiment_dir, "familiar_collections"), mustWork=FALSE)
  if(!dir.exists(file_paths$fam_coll_dir)) dir.create(file_paths$fam_coll_dir)
  
  # Create results directory
  file_paths$results_dir <- normalizePath(file.path(experiment_dir, "results"), mustWork=FALSE)
  if(!dir.exists(file_paths$results_dir)) dir.create(file_paths$results_dir)
  
  # Data file location
  data_file <- .parse_arg(x_config=config$paths$data_file,
                          x_var=data_file,
                          var_name="data_file",
                          type="character_list",
                          optional=TRUE,
                          default=NULL)
  
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
    logger.message(paste0("Configuration: A temporary R directory is created for the analysis: ", temporary_directory))
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
  settings$data <- do.call(.parse_experiment_settings,
                           args=c(list("config"=config$data),
                                  list(...)))

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
#'   1. Shahriari, B., Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N.
#'   Taking the Human Out of the Loop: A Review of Bayesian Optimization. Proc.
#'   IEEE 104, 148–175 (2016)
#'
#'   1. Srinivas, N., Krause, A., Kakade, S. M. & Seeger, M. W.
#'   Information-Theoretic Regret Bounds for Gaussian Process Optimization in
#'   the Bandit Setting. IEEE Trans. Inf. Theory 58, 3250–3265 (2012)
#'
#'   1. Kaufmann, E., Cappé, O. & Garivier, A. On Bayesian upper confidence
#'   bounds for bandit problems. in Artificial intelligence and statistics
#'   592–600 (2012).
#'
#'   1. Jamieson, K. & Talwalkar, A. Non-stochastic Best Arm Identification and
#'   Hyperparameter Optimization. in Proceedings of the 19th International
#'   Conference on Artificial Intelligence and Statistics (eds. Gretton, A. &
#'   Robert, C. C.) vol. 51 240–248 (PMLR, 2016).
#'
#'   1. Gramacy, R. B. laGP: Large-Scale Spatial Modeling via Local Approximate
#'   Gaussian Processes in R. Journal of Statistical Software 72, 1–46 (2016)
#'
#'   1. Sparapani, R., Spanbauer, C. & McCulloch, R. Nonparametric Machine
#'   Learning and Efficient Computation with Bayesian Additive Regression Trees:
#'   The BART R Package. Journal of Statistical Software 97, 1–66 (2021)
#'
#'   1. Davison, A. C. & Hinkley, D. V. Bootstrap methods and their application.
#'   (Cambridge University Press, 1997).
#'
#'   1. Efron, B. & Hastie, T. Computer Age Statistical Inference. (Cambridge
#'   University Press, 2016).
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
  settings$run <- strict.do.call(.parse_setup_settings,
                                 args=c(list("config"=config$run),
                                        list(...)))
  
  # Remove outcome_type, development_batch_id and parallel from ... This
  # prevents an error caused by multiple matching arguments.
  dots <- list(...)
  dots$parallel <- NULL
  dots$outcome_type <- NULL
  
  # Pre-processing settings
  settings$prep <- strict.do.call(.parse_preprocessing_settings,
                                  args=c(list("config"=config$preprocessing,
                                              "data"=data,
                                              "parallel"=settings$run$parallel,
                                              "outcome_type"=settings$data$outcome_type),
                                         dots))
  
  # Feature selection settings
  settings$fs <- strict.do.call(.parse_feature_selection_settings,
                                args=c(list("config"=config$feature_selection,
                                            "data"=data,
                                            "parallel"=settings$run$parallel,
                                            "outcome_type"=settings$data$outcome_type),
                                       dots))
  
  # Model development settings
  settings$mb <- strict.do.call(.parse_model_development_settings,
                                args=c(list("config"=config$model_development,
                                            "data"=data,
                                            "parallel"=settings$run$parallel,
                                            "outcome_type"=settings$data$outcome_type),
                                       dots))
  
  # Hyperparameter optimisation settings
  settings$hpo <- strict.do.call(.parse_hyperparameter_optimisation_settings,
                                 args=c(list("config"=config$hyperparameter_optimisation,
                                             "parallel"=settings$run$parallel,
                                             "outcome_type"=settings$data$outcome_type),
                                        dots))
  
  # Remove development_batch_id, hpo_metric, vimp_aggregation_method and
  # vimp_aggregation_rank_threshold from ... This prevents an error caused by
  # multiple matching arguments.
  dots$development_batch_id <- NULL
  dots$hpo_metric <- NULL
  dots$vimp_aggregation_method <- NULL
  dots$vimp_aggregation_rank_threshold <- NULL
  
  # Evaluation settings
  settings$eval <- strict.do.call(.parse_evaluation_settings,
                                  args=c(list("config"=config$evaluation,
                                              "data"=data,
                                              "parallel"=settings$run$parallel,
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
  settings$run$parallel <- (settings$prep$do_parallel |
                              settings$fs$do_parallel |
                              settings$mb$do_parallel |
                              settings$hpo$do_parallel %in% c("TRUE", "inner", "outer") |
                              settings$eval$do_parallel %in% c("TRUE", "inner", "outer"))
  
  # Report any issues with missing packages that have been written to the
  # backend.
  .report_missing_package_messages()
  
  return(settings)
}


#' Internal function for parsing settings related to the computational setup
#'
#' @param config A list of settings, e.g. from an xml file.
#' @param batch_id_column (**recommended**) Name of the column containing batch
#'   or cohort identifiers. This parameter is required if more than one dataset
#'   is provided, or if external validation is performed.
#'
#'   In familiar any row of data is organised by four identifiers:
#'
#'   * The batch identifier `batch_id_column`: This denotes the group to which a
#'   set of samples belongs, e.g. patients from a single study, samples measured
#'   in a batch, etc. The batch identifier is used for batch normalisation, as
#'   well as selection of development and validation datasets.
#'
#'   * The sample identifier `sample_id_column`: This denotes the sample level,
#'   e.g. data from a single individual. Subsets of data, e.g. bootstraps or
#'   cross-validation folds, are created at this level.
#'
#'   * The series identifier `series_id_column`: Indicates measurements on a
#'   single sample that may not share the same outcome value, e.g. a time
#'   series, or the number of cells in a view.
#'
#'   * The repetition identifier: Indicates repeated measurements in a single
#'   series where any feature values may differ, but the outcome does not.
#'   Repetition identifiers are always implicitly set when multiple entries for
#'   the same series of the same sample in the same batch that share the same
#'   outcome are encountered.
#'
#' @param sample_id_column (**recommended**) Name of the column containing
#'   sample or subject identifiers. See `batch_id_column` above for more
#'   details.
#'
#'   If unset, every row will be identified as a single sample.
#' @param series_id_column (**optional**) Name of the column containing series
#'   identifiers, which distinguish between measurements that are part of a
#'   series for a single sample. See `batch_id_column` above for more details.
#'
#'   If unset, rows which share the same batch and sample identifiers but have a
#'   different outcome are assigned unique series identifiers.
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
#'   recognise `0`, `false`, `f`, `n`, `no` as censoring indicators, including
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
#'   always be used for modelling. Ranking from feature selection has no effect
#'   for these features.
#'
#' @param novelty_features (*optional*) One or more names of feature columns
#'   that should be included for the purpose of novelty detection.
#'
#' @param exclude_features (*optional*) Feature columns that will be removed
#'   from the data set. Cannot overlap with features in `signature`,
#'   `novelty_features` or `include_features`.
#'
#' @param include_features (*optional*) Feature columns that are specifically
#'   included in the data set. By default all features are included. Cannot
#'   overlap with `exclude_features`, but may overlap `signature`. Features in
#'   `signature` and `novelty_features` are always included. If both
#'   `exclude_features` and `include_features` are provided, `include_features`
#'   takes precedence, provided that there is no overlap between the two.
#'
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
#'   to subsampling will always be conducted if the subsampling methods create
#'   any validation data sets.
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
                                       batch_id_column=waiver(),
                                       sample_id_column=waiver(),
                                       series_id_column=waiver(),
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
                                       novelty_features=waiver(),
                                       exclude_features=waiver(),
                                       include_features=waiver(),
                                       experimental_design=waiver(),
                                       imbalance_correction_method=waiver(),
                                       imbalance_n_partitions=waiver(),
                                       ...){
  
  settings <- list()
  
  ##### experimental_design ####################################################
  # Experimental design
  settings$exp_design <- .parse_arg(x_config=config$experimental_design,
                                    x_var=experimental_design,
                                    var_name="experimental_design",
                                    optional=FALSE,
                                    type="character")
  
  ##### imbalance_method #######################################################
  # Class imbalance correction method
  settings$imbalance_method <- .parse_arg(x_config=config$imbalance_correction_method,
                                          x_var=imbalance_correction_method,
                                          var_name="imbalance_correction_method",
                                          type="character",
                                          optional=TRUE,
                                          default="full_undersampling")
  
  .check_parameter_value_is_valid(x=settings$imbalance_method,
                                  var_name="imbalance_correction_method",
                                  values=c("full_undersampling", "random_undersampling"))
  
  ##### imbalance_n_partitions #################################################
  # Number of imbalance partitions for random undersampling
  settings$imbalance_n_partitions <- .parse_arg(x_config=config$imbalance_n_partitions,
                                                x_var=imbalance_n_partitions,
                                                var_name="imbalance_n_partitions",
                                                type="integer",
                                                optional=TRUE,
                                                default=10)
  
  .check_number_in_valid_range(x=settings$imbalance_n_partitions,
                               var_name="imbalance_n_partitions",
                               range=c(1, Inf))
  
  ##### sample_id_column #######################################################
  # Sample identifier column
  settings$sample_col <- .parse_arg(x_config=config$sample_id_column,
                                    x_var=sample_id_column,
                                    var_name="sample_id_colum",
                                    type="character",
                                    optional=TRUE,
                                    default=NULL)
  
  # Update column name
  if(!is.null(settings$sample_col)) settings$sample_col <- check_column_name(settings$sample_col)
  
  ##### batch_id_column ########################################################
  # Batch identifier column
  settings$batch_col <- .parse_arg(x_config=config$batch_id_column,
                                   x_var=batch_id_column,
                                   var_name="batch_id_column",
                                   type="character",
                                   optional=TRUE,
                                   default=NULL)
  
  # Update column name
  if(!is.null(settings$batch_col)) settings$batch_col <- check_column_name(settings$batch_col)
  
  ##### series_id_column #######################################################
  # Series identifier column
  settings$series_col <- .parse_arg(x_config=config$series_id_column,
                                    x_var=series_id_column,
                                    var_name="series_id_column",
                                    type="character",
                                    optional=TRUE,
                                    default=NULL)
  
  if(!is.null(settings$series_col)) settings$series_col <- check_column_name(settings$series_col)
  
  ##### development_batch_id ###################################################
  # Development cohort identifier
  settings$train_cohorts <- .parse_arg(x_config=config$development_batch_id,
                                       x_var=development_batch_id,
                                       var_name="development_batch_id",
                                       type="character_list",
                                       optional=TRUE,
                                       default=NULL)
  
  ##### validation_batch_id ####################################################
  # Validation cohort identifier
  settings$valid_cohorts <- .parse_arg(x_config=config$validation_batch_id,
                                       x_var=validation_batch_id,
                                       var_name="validation_batch_id",
                                       type="character_list",
                                       optional=TRUE,
                                       default=NULL)
  
  ##### outcome_column #########################################################
  # Outcome column(s)
  settings$outcome_col <- .parse_arg(x_config=config$outcome_column,
                                     x_var=outcome_column,
                                     var_name="outcome_column",
                                     type="character_list",
                                     optional=TRUE,
                                     default=NULL)
  
  if(!is.null(settings$outcome_col)) settings$outcome_col <- check_column_name(settings$outcome_col)
  
  ##### outcome_type ###########################################################
  # Outcome type - a check will be done later
  settings$outcome_type <- .parse_arg(x_config=config$outcome_type,
                                      x_var=outcome_type,
                                      var_name="outcome_type",
                                      type="character",
                                      optional=TRUE,
                                      default=NULL)
  
  ##### outcome_name ###########################################################
  # Outcome name
  settings$outcome_name <- .parse_arg(x_config=config$outcome_name,
                                      x_var=outcome_name,
                                      var_name="outcome_name",
                                      type="character",
                                      optional=TRUE,
                                      default=NULL)
  
  ##### class_levels ###########################################################
  # Class levels
  settings$class_levels <- .parse_arg(x_config=config$class_levels,
                                      x_var=class_levels,
                                      var_name="class_levels",
                                      type="character_list",
                                      optional=TRUE,
                                      default=NULL)
  
  ##### event_indicator ########################################################
  # Event indicator
  settings$event_indicator <- .parse_arg(x_config=config$event_indicator,
                                         x_var=event_indicator,
                                         var_name="event_indicator",
                                         type="character_list",
                                         optional=TRUE,
                                         default=NULL)
  
  ##### censoring_indicator ####################################################
  # Censoring indicator
  settings$censoring_indicator <- .parse_arg(x_config=config$censoring_indicator,
                                             x_var=censoring_indicator,
                                             var_name="censoring_indicator",
                                             type="character_list",
                                             optional=TRUE,
                                             default=NULL)
  
  ##### competing_risk_indicator ###############################################
  # Competing risk indicator
  settings$competing_risk_indicator <- .parse_arg(x_config=config$competing_risk_indicator,
                                                  x_var=competing_risk_indicator,
                                                  var_name="competing_risk_indicator",
                                                  type="character_list",
                                                  optional=TRUE,
                                                  default=NULL) 
  
  ##### signature ##############################################################
  # Signature features
  settings$signature <- .parse_arg(x_config=config$signature,
                                   x_var=signature,
                                   var_name="signature",
                                   type="character_list",
                                   optional=TRUE,
                                   default=NULL)
  
  if(!is.null(settings$signature)) settings$signature <- check_column_name(settings$signature)
  
  ##### novelty_features #######################################################
  # Novelty features
  settings$novelty_features <- .parse_arg(x_config=config$novelty_features,
                                          x_var=novelty_features,
                                          var_name="novelty_features",
                                          type="character_list",
                                          optional=TRUE,
                                          default=NULL)
  
  if(!is.null(settings$novelty_features)) settings$novelty_features <- check_column_name(settings$novelty_features)
  
  ##### include_features #######################################################
  # Included features
  settings$include_features <- .parse_arg(x_config=config$include_features,
                                          x_var=include_features,
                                          var_name="include_features",
                                          type="character_list",
                                          optional=TRUE,
                                          default=NULL)
  
  if(!is.null(settings$include_features)) settings$include_features <- check_column_name(settings$include_features)
  
  ##### exclude_features #######################################################
  # Excluded features
  settings$exclude_features <- .parse_arg(x_config=config$exclude_features,
                                          x_var=exclude_features,
                                          var_name="exclude_features",
                                          type="character_list",
                                          optional=TRUE,
                                          default=NULL)
  
  if(!is.null(settings$exclude_features)) settings$exclude_features <- check_column_name(settings$exclude_features)
  
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
#'   parallelisation. Defaults to 2. This setting does nothing if
#'   parallelisation is disabled.
#' @param restart_cluster (*optional*) Restart nodes used for parallel computing
#'   to free up memory prior to starting a parallel process. Note that it does
#'   take time to set up the clusters. Therefore setting this argument to `TRUE`
#'   may impact processing speed. This argument is ignored if `parallel` is
#'   `FALSE` or the cluster was initialised outside of familiar. Default is
#'   `FALSE`, which causes the clusters to be initialised only once.
#' @param cluster_type (*optional*) Selection of the cluster type for parallel
#'   processing. Available types are the ones supported by the parallel package
#'   that is part of the base R distribution: `psock` (default), `fork`, `mpi`,
#'   `nws`, `sock`. In addition, `none` is available, which also disables
#'   parallel processing.
#' @param backend_type (*optional*) Selection of the backend for distributing
#'   copies of the data. This backend ensures that only a single master copy is
#'   kept in memory. This limits memory usage during parallel processing.
#'
#'   Several backend options are available, notably `socket_server`, and `none`
#'   (default). `socket_server` is based on the callr package and R sockets,
#'   comes with `familiar` and is available for any OS. `none` uses the package
#'   environment of familiar to store data, and is available for any OS.
#'   However, `none` requires copying of data to any parallel process, and has a
#'   larger memory footprint.
#' @param server_port (*optional*) Integer indicating the port on which the
#'   socket server or RServe process should communicate. Defaults to port 6311.
#'   Note that ports 0 to 1024 and 49152 to 65535 cannot be used.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to the computational setup.
#' @md
#' @keywords internal
.parse_setup_settings <- function(config=NULL,
                                  parallel=waiver(),
                                  parallel_nr_cores=waiver(),
                                  restart_cluster=waiver(),
                                  cluster_type=waiver(),
                                  backend_type=waiver(),
                                  server_port=waiver(),
                                  ...){
  
  settings <- list()

  ##### parallel ###############################################################
  # Parallelisation master switch
  settings$parallel <- .parse_arg(x_config=config$parallel,
                                  x_var=parallel,
                                  var_name="parallel",
                                  type="logical",
                                  optional=TRUE,
                                  default=TRUE)
  
  ##### parallel_nr_cores ######################################################
  # Maximum number of cores that a R may use
  settings$parallel_nr_cores <- .parse_arg(x_config=config$parallel_nr_cores,
                                           x_var=parallel_nr_cores,
                                           var_name="parallel_nr_cores",
                                           type="integer",
                                           optional=TRUE,
                                           default=2L)
  
  if(!is.null(settings$parallel_nr_cores)){
    .check_number_in_valid_range(x=settings$parallel_nr_cores,
                                 var_name="parallel_nr_cores",
                                 range=c(1, parallel::detectCores()))
  }
  
  # Set cores to 1 in case parallel processing is disabled.
  if(!settings$parallel) settings$parallel_nr_cores <- 1L
  
  ##### restart_cluster ########################################################
  # Restart clusters
  settings$restart_cluster <- .parse_arg(x_config=config$restart_cluster,
                                         x_var=restart_cluster,
                                         var_name="restart_cluster",
                                         type="logical",
                                         optional=TRUE,
                                         default=FALSE)
  
  if(!settings$parallel) settings$restart_cluster <- FALSE
  
  ##### cluster_type ###########################################################
  # Define the cluster type
  settings$cluster_type <- .parse_arg(x_config=config$cluster_type,
                                      x_var=cluster_type,
                                      var_name="cluster_type",
                                      type="character",
                                      optional=TRUE,
                                      default="psock")
  
  .check_parameter_value_is_valid(settings$cluster_type,
                                  var_name="cluster_type",
                                  values=c("psock", "fork", "mpi", "nws", "sock", "none"))
  
  if(!settings$parallel) settings$cluster_type <- "none"
  
  .check_cluster_type_availability(cluster_type=settings$cluster_type)
    
  if(settings$cluster_type != "none"){
    require_package(x="microbenchmark",
                    purpose="to make use of optimised parallel processing",
                    message_type="backend_warning")
  }
  
  ##### backend_type ###########################################################
  # Data server backend - this is os- and package-dependent
  settings$backend_type <- .parse_arg(x_config=config$backend_type,
                                      x_var=backend_type,
                                      var_name="backend_type",
                                      type="character",
                                      optional=TRUE,
                                      default="none")
  
  .check_parameter_value_is_valid(settings$backend_type, var_name="backend_type",
                                  values=.get_available_backend_types())
  
  require_package(x=.required_packages_backend(settings$backend_type),
                  purpose="to use the requested backend (", settings$backend_type, ")",
                  message_type="backend_error")
  
  ##### server_port ############################################################
  # RServe communications port
  settings$server_port <- .parse_arg(x_config=config$server_port,
                                     x_var=server_port,
                                     var_name="server_port",
                                     type="integer",
                                     optional=TRUE,
                                     default=6311L)
  
  .check_number_in_valid_range(x=settings$server_port,
                               var_name="server_port",
                               range=c(1025, 49151))
  
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
.parse_preprocessing_settings <- function(config=NULL,
                                          data,
                                          parallel,
                                          outcome_type,
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
  
  ##### feature_max_fraction_missing ###########################################
  # Maximum fraction of data points missing for inclusion of a feature
  settings$feature_max_fraction_missing <- .parse_arg(x_config=config$feature_max_fraction_missing,
                                                      x_var=feature_max_fraction_missing,
                                                      var_name="feature_max_fraction_missing",
                                                      type="numeric",
                                                      optional=TRUE,
                                                      default=0.30)
  
  .check_number_in_valid_range(x=settings$feature_max_fraction_missing,
                               var_name="feature_max_fraction_missing",
                               range=c(0.0, 0.95))
  
  
  ##### sample_max_fraction_missing ############################################
  # Maximum fraction of features missing for inclusion of a subject
  settings$sample_max_fraction_missing <- .parse_arg(x_config=config$sample_max_fraction_missing,
                                                     x_var=sample_max_fraction_missing,
                                                     var_name="sample_max_fraction_missing",
                                                     type="numeric",
                                                     optional=TRUE,
                                                     default=0.30)
  
  .check_number_in_valid_range(x=settings$sample_max_fraction_missing,
                               var_name="sample_max_fraction_missing",
                               range=c(0.0, 0.95))
  
  ##### filter_method ##########################################################
  # Univariate filter methods
  settings$filter_method <- .parse_arg(x_config=config$filter_method,
                                       x_var=filter_method,
                                       var_name="filter_method",
                                       type="character_list",
                                       optional=TRUE,
                                       default="none")
  
  .check_parameter_value_is_valid(x=settings$filter_method,
                                  var_name="filter_method",
                                  values=c("none", "low_variance", "univariate_test", "robustness"))
  
  if(outcome_type == "multinomial" & "univariate_test" %in% settings$filter_method){
    # The VGAM package is required for univariate tests with multinomial
    # endpoints.
    if(!require_package(x="VGAM",
                        purpose="to filter features using univariate tests",
                        message_type="backend_warning")){
      
      # If the VGAM package is not present, avoid the univariate test.
      settings$filter_method <- setdiff(settings$filter_method, "univariate_test")
      if(length(settings$filter_method) == 0) settings$filter_method <- "none"
    }
  }
  
  ##### univariate_test_threshold ##############################################
  # Univariate model filter threshold value
  settings$univar_threshold <- .parse_arg(x_config=config$univariate_test_threshold,
                                          x_var=univariate_test_threshold,
                                          var_name="univariate_test_threshold",
                                          type="numeric",
                                          optional=TRUE,
                                          default=0.20)
  
  .check_number_in_valid_range(x=settings$univar_threshold,
                               var_name="univariate_test_threshold",
                               range=c(0.0, 1.0),
                               closed=c(FALSE, TRUE))
  
  ##### univariate_test_threshold_metric #######################################
  # Univariate model threshold metric
  settings$univar_metric <- .parse_arg(x_config=config$univariate_test_threshold_metric,
                                       x_var=univariate_test_threshold_metric,
                                       var_name="univariate_test_threshold_metric",
                                       type="character",
                                       optional=TRUE,
                                       default="p_value")
  
  .check_parameter_value_is_valid(x=settings$univar_metric,
                                  var_name="univariate_test_threshold_metric",
                                  values=c("p_value", "q_value"))
  
  # If the qvalue package is not installed, use p-values instead.
  if(settings$univar_metric == "q_value"){
    if(!require_package(x="qvalue",
                        purpose="to use q-values as a metric for univariate feature tests",
                        message_type="backend_warning")){
      settings$univar_metric <- "p_value"
    }
  }
  
  ##### univariate_test_max_feature_set_size ###################################
  # Maximum feature set size after univariate regression models.
  settings$univar_feat_set_size <- .parse_arg(x_config=config$univariate_test_max_feature_set_size,
                                              x_var=univariate_test_max_feature_set_size,
                                              var_name="univariate_test_max_feature_set_size",
                                              type="integer",
                                              optional=TRUE,
                                              default=NULL)
  
  if(!is.null(settings$univar_feat_set_size)){
    .check_number_in_valid_range(x=settings$univar_feat_set_size,
                                 var_name="univariate_test_max_feature_set_size",
                                 range=c(1, Inf))
  }
  
  ##### low_var_minimum_variance_threshold #####################################
  # Minimum amount of variance for inclusion of feature.
  if("low_variance" %in% settings$filter_method){
    settings$low_var_threshold <- .parse_arg(x_config=config$low_var_minimum_variance_threshold,
                                             x_var=low_var_minimum_variance_threshold,
                                             var_name="low_var_minimum_variance_threshold",
                                             type="numeric",
                                             optional=FALSE)
    
    .check_number_in_valid_range(x=settings$low_var_threshold,
                                 var_name="low_var_minimum_variance_threshold",
                                 range=c(0.0, Inf))
  }
  
  ##### low_var_max_feature_set_size ###########################################
  # Maximum feature set size after variance thresholding
  settings$low_var_max_feature_set_size <- .parse_arg(x_config=config$low_var_max_feature_set_size,
                                                      x_var=low_var_max_feature_set_size,
                                                      var_name="low_var_max_feature_set_size",
                                                      type="integer",
                                                      optional=TRUE,
                                                      default=NULL)
  
  if(!is.null(settings$low_var_max_feature_set_size)){
    .check_number_in_valid_range(x=settings$low_var_max_feature_set_size,
                                 var_name="low_var_max_feature_set_size",
                                 range=c(1, Inf))
  }
  
  ##### robustness_icc_type ####################################################
  # Intraclass correlation coefficient (ICC) type for robustness analysis
  settings$robustness_icc_type <- .parse_arg(x_config=config$robustness_icc_type,
                                             x_var=robustness_icc_type,
                                             var_name="robustness_icc_type",
                                             type="character",
                                             optional=TRUE,
                                             default="1")
  
  .check_parameter_value_is_valid(x=settings$robustness_icc_type,
                                  var_name="robustness_icc_type",
                                  values=.get_available_icc_types())
  
  ##### robustness_threshold_metric ############################################
  # ICC parameter to use for thresholding. Can be icc (estimated icc), icc_low
  # (lower edge of the icc confidence interval), icc_panel (estimated panel
  # icc), icc_panel_low (lower edge of the panel icc confidence interval)
  settings$robustness_threshold_param <- .parse_arg(x_config=config$robustness_threshold_metric,
                                                    x_var=robustness_threshold_metric,
                                                    var_name="robustness_threshold_metric",
                                                    type="character",
                                                    optional=TRUE,
                                                    default="icc_low")
  
  .check_parameter_value_is_valid(x=settings$robustness_threshold_param,
                                  var_name="robustness_threshold_metric",
                                  values=c("icc", "icc_low", "icc_panel", "icc_panel_low"))
  
  
  ##### robustness_threshold_value #############################################
  # ICC value for thresholding.
  settings$robustness_threshold_value <- .parse_arg(x_config=config$robustness_threshold_value,
                                                    x_var=robustness_threshold_value,
                                                    var_name="robustness_threshold_value",
                                                    type="numeric",
                                                    optional=TRUE,
                                                    default=0.70)
  
  .check_number_in_valid_range(x=settings$robustness_threshold_value,
                               var_name="robustness_threshold_value",
                               range=c(-Inf, 1.0))
  
  
  ##### imputation_method ######################################################
  # Data imputation method. For datasets smaller than 100 features we use lasso,
  # and simple imputation is used otherwise.
  default_imputation_method <- ifelse(get_n_features(data, outcome_type=outcome_type) < 100, "lasso", "simple")
  
  settings$imputation_method <- .parse_arg(x_config=config$imputation_method,
                                           x_var=imputation_method,
                                           var_name="imputation_method",
                                           type="character", optional=TRUE,
                                           default=default_imputation_method)
  
  .check_parameter_value_is_valid(x=settings$imputation_method,
                                  var_name="imputation_method",
                                  values=c("simple", "lasso"))
  
  if(settings$imputation_method == "lasso"){
    # If glmnet is not installed, use simple imputation.
    if(!require_package(x="glmnet",
                        purpose="to impute data using lasso regression",
                        message_type="backend_warning")){
      settings$imputation_method <- "simple"
    }
  }
  
  ##### transformation_method ##################################################
  # Transformation method
  settings$transform_method <- .parse_arg(x_config=config$transformation_method,
                                          x_var=transformation_method,
                                          var_name="transformation_method",
                                          type="character",
                                          optional=TRUE,
                                          default="yeo_johnson")
  
  .check_parameter_value_is_valid(x=settings$transform_method,
                                  var_name="transformation_method",
                                  values=.get_available_transformation_methods())
  
  ##### normalisation_method ###################################################
  # Normalisation method
  settings$normalisation_method <- .parse_arg(x_config=config$normalisation_method,
                                              x_var=normalisation_method,
                                              var_name="normalisation_method",
                                              type="character",
                                              optional=TRUE,
                                              default="standardisation")
  
  .check_parameter_value_is_valid(x=settings$normalisation_method,
                                  var_name="normalisation_method",
                                  values=.get_available_normalisation_methods())
  
  ##### batch_normalisation_method #############################################
  # Batch normalisation method
  settings$batch_normalisation_method <- .parse_arg(x_config=config$batch_normalisation_method,
                                                    x_var=batch_normalisation_method,
                                                    var_name="batch_normalisation_method",
                                                    type="character",
                                                    optional=TRUE,
                                                    default="none")
  
  .check_parameter_value_is_valid(x=settings$batch_normalisation_method,
                                  var_name="batch_normalisation_method",
                                  values=.get_available_batch_normalisation_methods())
  
  # If the batch normalisation method is combat, pre-normalisation of the
  # entire data is required.
  if(settings$batch_normalisation_method %in% .get_available_batch_normalisation_methods(type="combat") &
     settings$normalisation_method %in% c("none", "mean_centering")){
    settings$normalisation_method <- "standardisation"
  }
  
  ##### cluster_method #########################################################
  # Feature clustering
  settings$cluster_method <- .parse_arg(x_config=config$cluster_method,
                                        x_var=cluster_method,
                                        var_name="cluster_method",
                                        type="character",
                                        optional=TRUE,
                                        default="hclust")
  
  # Check that the cluster package is installed, and revert to if none.
  if(!settings$cluster_method %in% c("none", "hclust")){
    if(!require_package(x="cluster",
                        purpose="to cluster similar features together",
                        message_type="backend_warning")){
      
      settings$cluster_method <- "hclust"
    }
  }
  
  # Advise to install the fastcluster package.
  if(settings$cluster_method == "hclust"){
    require_package(x="fastcluster",
                    purpose="to create clusters faster",
                    message_type="backend_warning")
  }
  
  ##### cluster_linkage ########################################################
  # Feature cluster linkage method
  settings$cluster_linkage <- .parse_arg(x_config=config$cluster_linkage_method,
                                         x_var=cluster_linkage_method,
                                         var_name="cluster_linkage_method",
                                         type="character",
                                         optional=TRUE,
                                         default="average")
  
  ##### cluster_cut_method #####################################################
  # Feature cluster cut method
  default_cluster_cut_method <- ifelse(settings$cluster_method == "pam", "silhouette", "fixed_cut")
  settings$cluster_cut_method <- .parse_arg(x_config=config$cluster_cut_method,
                                            x_var=cluster_cut_method,
                                            var_name="cluster_cut_method",
                                            type="character",
                                            optional=TRUE,
                                            default=default_cluster_cut_method)
  
  if(settings$cluster_cut_method == "dynamic_cut"){
    if(!require_package(x="dynamicTreeCut",
                        purpose="to cut dendrograms dynamically",
                        message_type="backend_warning")){
      
      settings$cluster_cut_method <- "fixed_cut"
    }
  }
  
  ##### cluster_similarity_metric ##############################################
  #Feature similarity metric which expresses some sort of correlation between a
  #pair of features.
  settings$cluster_similarity_metric <- .parse_arg(x_config=config$cluster_similarity_metric,
                                                   x_var=cluster_similarity_metric,
                                                   var_name="cluster_similarity_metric",
                                                   type="character",
                                                   optional=TRUE,
                                                   default="mcfadden_r2")
  
  if(settings$cluster_similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    if(!require_package(x="VGAM",
                        purpose=paste0("to compute log-likelihood pseudo R2 similarity using the ", settings$cluster_similarity_metric, " metric"),
                        message_type="backend_warning")){
      
      settings$cluster_similarity_metric <- "spearman"
    }
  }
  
  ##### cluster_similarity_threshold ###########################################
  #Feature cluster similarity that determines the similarity threshold for
  #features to be considered part of one cluster. Should be expressed in terms
  #of the similarity metric, e.g. 0.8 for spearman would consider all features
  #that have a pairwise correlation of 0.8 and over to belong to a cluster.
  settings$cluster_sim_thresh <- .parse_arg(x_config=config$cluster_similarity_threshold,
                                            x_var=cluster_similarity_threshold,
                                            var_name="cluster_similarity_threshold",
                                            type="numeric",
                                            optional=TRUE,
                                            default=NULL)
  
  if(is.null(settings$cluster_sim_thresh)){
    if(settings$cluster_cut_method %in% c("fixed_cut")){
      # Fixed cut requires stringent defaults, otherwise non-sense clusters will
      # be produced.
      if(settings$cluster_similarity_metric %in% c("mcfadden_r2")){
        settings$cluster_sim_thresh <- 0.30
        
      } else if(settings$cluster_similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2")){
        settings$cluster_sim_thresh <- 0.75
        
      } else {
        settings$cluster_sim_thresh <- 0.90
      }
      
    } else {
      # The similarity threshold is also used to determine if any clusters could
      # potentially be found. The threshold is set low so that other cut methods
      # can be explored.
      if(settings$cluster_similarity_metric %in% c("mcfadden_r2")){
        settings$cluster_sim_thresh <- 0.05
        
      } else if(settings$cluster_similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2")) {
        settings$cluster_sim_thresh <- 0.40
        
      } else {
        settings$cluster_sim_thresh <- 0.50
      }
    }
  }
  
  ##### cluster_representation_method ##########################################
  # Method to select the feature that represents the cluster
  settings$cluster_repr_method <- .parse_arg(x_config=config$cluster_representation_method,
                                             x_var=cluster_representation_method,
                                             var_name="cluster_representation_method",
                                             type="character",
                                             optional=TRUE,
                                             default="best_predictor")
  
  # Partioning around medioids only allows the use of medioids for
  # representation.
  if(settings$cluster_method == "pam") settings$cluster_repr_method <- "medioid"
  
  # If mean is used, this requires the data to have some kind of standardisation
  # with centering at 0.
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
                            var_type="cluster",
                            test_required_packages=FALSE)
  
  ##### parallel_preprocessing #################################################
  # Parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_preprocessing,
                                     x_var=parallel_preprocessing,
                                     var_name="parallel_preprocessing",
                                     type="logical",
                                     optional=TRUE,
                                     default=TRUE)

  # Disable if parallel is FALSE.
  if(!parallel) settings$do_parallel <- FALSE

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
#'   * `none`: Don't aggregate ranks, but rather aggregate the variable
#'   importance scores themselves.
#'
#'   * `mean`: Use the mean rank of a feature over the subsets to
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
#'   * `borda` (default): Use the borda count as measure for the aggregated
#'   feature rank (Wald et al., 2012).
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
#'   The *feature selection methods* vignette provides additional information.
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
.parse_feature_selection_settings <- function(config=NULL,
                                              data,
                                              parallel,
                                              outcome_type,
                                              fs_method=waiver(),
                                              fs_method_parameter=waiver(),
                                              vimp_aggregation_method=waiver(),
                                              vimp_aggregation_rank_threshold=waiver(),
                                              parallel_feature_selection=waiver(),
                                              ...){
  settings <- list()
  
  ##### fs_method ##############################################################
  # Feature selection methods
  settings$fs_methods <- .parse_arg(x_config=config$fs_method,
                                    x_var=fs_method,
                                    var_name="fs_method",
                                    type="character_list",
                                    optional=FALSE)
  
  sapply(settings$fs_methods, .check_vimp_outcome_type, outcome_type=outcome_type)
  
  ##### fs_method_parameter ####################################################
  # Feature selection parameters
  settings$param <- .parse_arg(x_config=config$fs_method_parameter,
                               x_var=fs_method_parameter,
                               var_name="fs_method_parameter",
                               type="list",
                               optional=TRUE,
                               default=list())
  
  settings$param <- .parse_hyperparameters(data=data,
                                           parameter_list=settings$param,
                                           outcome_type=outcome_type,
                                           fs_method=settings$fs_methods)
  
  ##### vimp_aggregation_method ################################################
  # Variable importance aggregation methods
  settings$aggregation <- .parse_arg(x_config=config$vimp_aggregation_method,
                                     x_var=vimp_aggregation_method,
                                     var_name="vimp_aggregation_method",
                                     type="character",
                                     optional=TRUE,
                                     default="borda")
  
  rank.check_aggregation_method(method=settings$aggregation)
  
  ##### vimp_aggregation_rank_threshold ########################################
  # Variable importance rank threshold (used by some aggregation methods)
  settings$aggr_rank_threshold <- .parse_arg(x_config=config$vimp_aggregation_rank_threshold,
                                             x_var=vimp_aggregation_rank_threshold,
                                             var_name="vimp_aggregation_rank_threshold",
                                             type="integer",
                                             optional=TRUE,
                                             default=5L)
  
  if(!is.null(settings$aggr_rank_threshold)){
    .check_number_in_valid_range(x=settings$aggr_rank_threshold,
                                 var_name="vimp_aggregation_rank_threshold",
                                 range=c(1, Inf))
  }
  
  ##### parallel_feature_selection #############################################
  # Parallelisation switch for feature selection
  settings$do_parallel <- .parse_arg(x_config=config$parallel_feature_selection,
                                     x_var=parallel_feature_selection,
                                     var_name="parallel_feature_selection",
                                     type="logical",
                                     optional=TRUE,
                                     default=TRUE)
  
  # Disable if parallel is FALSE.
  if(!parallel) settings$do_parallel <- FALSE
  
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
#' @param novelty_detector (*optional*) Specify the algorithm used for training
#'   a novelty detector. This detector can be used to identify
#'   out-of-distribution data prospectively.
#' @param detector_parameters (*optional*) List lists containing hyperparameters
#'   for novelty detectors. Currently not used.
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
.parse_model_development_settings <- function(config=NULL,
                                              data,
                                              parallel,
                                              outcome_type,
                                              learner=waiver(),
                                              hyperparameter=waiver(),
                                              novelty_detector=waiver(),
                                              detector_parameters=waiver(),
                                              parallel_model_development=waiver(),
                                              ...){
  settings <- list()
  
  ##### learner ################################################################
  # Learners for model development
  settings$learners <- .parse_arg(x_config=config$learner, x_var=learner,
                                  var_name="learner",
                                  type="character_list",
                                  optional=FALSE)
  
  sapply(settings$learners, .check_learner_outcome_type, outcome_type=outcome_type)
  
  ##### hyperparameters ########################################################
  # Model hyperparameters
  settings$hyper_param <- .parse_arg(x_config=config$hyperparameter,
                                     x_var=hyperparameter,
                                     var_name="hyperparameter",
                                     type="list",
                                     optional=TRUE,
                                     default=list())
  
  settings$hyper_param <- .parse_hyperparameters(data=data,
                                                 parameter_list=settings$hyper_param,
                                                 outcome_type=outcome_type,
                                                 learner=settings$learners)
  
  ##### novelty_detector #######################################################
  settings$novelty_detector <- .parse_arg(x_config=config$novelty_detector,
                                          x_var=novelty_detector,
                                          var_name="novelty_detector",
                                          type="character",
                                          optional=TRUE,
                                          default="isolation_forest")
  
  .check_novelty_detector_available(detector=settings$novelty_detector)
  
  ##### detector_parameters ####################################################
  settings$detector_parameters <- .parse_arg(x_config=config$detector_parameters,
                                             x_var=detector_parameters,
                                             var_name="detector_parameters",
                                             type="list",
                                             optional=TRUE,
                                             default=list())
  
  settings$detector_parameters <- .parse_hyperparameters(data=data,
                                                         parameter_list=settings$detector_parameters,
                                                         detector=settings$novelty_detector,
                                                         outcome_type="unsupervised")
  
  ##### parallel_model_development #############################################
  # Parallelisation switch for model building
  settings$do_parallel <- .parse_arg(x_config=config$parallel_model_development,
                                     x_var=parallel_model_development,
                                     var_name="parallel_model_development",
                                     type="logical",
                                     optional=TRUE,
                                     default=TRUE)
  
  # Disable if parallel is FALSE
  if(!parallel) settings$do_parallel <- FALSE
  
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
#'   The default number of bootstraps is `50`. Hyperparameter optimisation may
#'   finish before exhausting the set of bootstraps.
#' @param optimisation_determine_vimp (*optional*) Logical value that indicates
#'   whether variable importance is determined separately for each of the
#'   bootstraps created during the optimisation process (`TRUE`) or the
#'   applicable results from the feature selection step are used (`FALSE`).
#'
#'   Determining variable importance increases the initial computational
#'   overhead. However, it prevents positive biases for the out-of-bag data due
#'   to overlap of these data with the development data set used for the feature
#'   selection step. In this case, any hyperparameters of the variable
#'   importance method are not determined separately for each bootstrap, but
#'   those obtained during the feature selection step are used instead. In case
#'   multiple of such hyperparameter sets could be applicable, the set that will
#'   be used is randomly selected for each bootstrap.
#'
#'   This parameter only affects hyperparameter optimisation of learners. The
#'   default is `TRUE`.
#' @param smbo_random_initialisation (*optional*) String indicating the
#'   initialisation method for the hyperparameter space. Can be one of
#'   `fixed_subsample` (default), `fixed`, or `random`. `fixed` and
#'   `fixed_subsample` first create hyperparameter sets from a range of default
#'   values set by familiar. `fixed_subsample` then randomly draws up to
#'   `smbo_n_random_sets` from the grid. `random` does not rely upon a fixed
#'   grid, and randomly draws up to `smbo_n_random_sets` hyperparameter sets
#'   from the hyperparameter space.
#' @param smbo_n_random_sets (*optional*) Number of random or subsampled
#'   hyperparameters drawn during the initialisation process. Default: `100`.
#'   Cannot be smaller than `10`. The parameter is not used when
#'   `smbo_random_initialisation` is `fixed`, as the entire pre-defined grid
#'   will be explored.
#'
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
#'   The default number of intensify iteration is `20`. Iterations may be
#'   stopped early if the incumbent set of hyperparameters remains the same for
#'   `smbo_stop_convergent_iterations` iterations, or performance improvement is
#'   minimal. This behaviour is suppressed during the first 4 iterations to
#'   enable the algorithm to explore the hyperparameter space.
#' @param smbo_stop_convergent_iterations (*optional*) The number of subsequent
#'   convergent SMBO iterations required to stop hyperparameter optimisation
#'   early. An iteration is convergent if the *best* parameter set has not
#'   changed or the optimisation score over the 4 most recent iterations has not
#'   changed beyond the tolerance level in `smbo_stop_tolerance`.
#'
#'   The default value is `3`.
#' @param smbo_stop_tolerance (*optional*) Tolerance for early stopping due to
#'   convergent optimisation score.
#'
#'   The default value is `0.01`.
#' @param smbo_step_bootstraps (*optional*) The number of bootstraps taken from
#'   the set of `optimisation_bootstraps` bootstraps as data for the initial
#'   SMBO step and the steps in each intensify iteration.
#'
#'   The default value is `3`. The value cannot be larger than
#'   `optimisation_bootstraps`.
#' @param smbo_intensify_steps (*optional*) The number of steps in each SMBO
#'   intensify iteration. Each step a new set of `smbo_step_bootstraps`
#'   bootstraps is drawn and used in the run-off between the incumbent *best*
#'   hyperparameter combination and its challengers.
#'
#'   The default value is `5`. Higher numbers allow for a more detailed
#'   comparison, but this comes with added computational cost.
#'
#' @param optimisation_metric (*optional*) One or more metrics used to compute
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
#'   Multiple optimisation metrics can be specified. Actual metric values are
#'   converted to an objective value by comparison with a baseline metric value
#'   that derives from a trivial model, i.e. majority class for binomial and
#'   multinomial outcomes, the median outcome for count and continuous outcomes
#'   and a fixed risk or time for survival outcomes.
#'
#' @param optimisation_function (*optional*) Type of optimisation function used
#'   to quantify the performance of a hyperparameter set. Model performance is
#'   assessed using the metric(s) specified by `optimisation_metric` on the
#'   in-bag and out-of-bag samples of a bootstrap. These values are converted to
#'   objective scores with a standardised interval of \eqn{[-1.0, 1.0]}, and are
#'   referred to as \eqn{s'_{ib}} and \eqn{s'_{oob}}, respectively. The function
#'   indicated by `optimisation_function` computes a objective score from each
#'   pair of values:
#'
#'   * `max_validation`: Uses the out-of-bag validation objective score
#'   \eqn{s'_{oob}} as optimisation score. This function is widely used in
#'   machine learning.
#'
#'   * `balanced` (default): Computes \eqn{s'_{oob} - |s'_{oob} - s'_{ib}|}.
#'   This function forces the algorithm to consider hyperparameter sets that
#'   perform well on both development and validation data.
#'
#'   * `stronger_balance`: Computes \eqn{s'_{oob} - 2.0 |s'_{oob} - s'_{ib}|}.
#'   Stronger penalty than in the `balanced` objective.
#'
#' @param hyperparameter_learner (*optional*) Any point in the hyperparameter
#'   space has a single, scalar, optimisation score value that is *a priori*
#'   unknown. During the optimisation process, the algorithm samples from the
#'   hyperparameter space by selecting hyperparameter sets and computing the
#'   optimisation score value for one or more bootstraps. For each
#'   hyperparameter set the resulting values are distributed around the actual
#'   value. The learner indicated by `hyperparameter_learner` is then used to
#'   infer optimisation score estimates for unsampled parts of the
#'   hyperparameter space.
#'
#'   The following models are available:
#'
#'   * `bayesian_additive_regression_trees` or `bart`: Uses Bayesian Additive
#'   Regression Trees (Sparapani et al., 2021) for inference. Unlike standard
#'   random forests, BART allows for estimating posterior distributions directly
#'   and can extrapolate.
#'
#'   * `gaussian_process` (default): Creates a localised approximate Gaussian
#'   process for inference (Gramacy, 2016). This allows for better scaling than
#'   deterministic Gaussian Processes.
#'
#'   * `random_forest`: Creates a random forest for inference. Originally
#'   suggested by Hutter et al. (2011). A weakness of random forests is their
#'   lack of extrapolation beyond observed values, which limits their usefulness
#'   in exploiting promising areas of hyperparameter space.
#'
#'   * `random` or `random_search`: Forgoes the use of models to steer
#'   optimisation. Instead, a random search is performed.
#'
#' @param acquisition_function (*optional*) The acquisition function influences
#'   how new hyperparameter sets are selected. The algorithm uses the model
#'   learned by the learner indicated by `hyperparameter_learner` to search the
#'   hyperparameter space for hyperparameter sets that are either likely better
#'   than the best known set (*exploitation*) or where there is considerable
#'   uncertainty (*exploration*). The acquisition function quantifies this
#'   (Shahriari et al., 2016).
#'
#'   The following acquisition functions are available, and are described in
#'   more detail in the *learner algorithms* vignette:
#'
#'   * `improvement_probability`: The probability of improvement quantifies the
#'   probability that the expected optimisation score for a set is better than
#'   the best observed optimisation score
#'
#'   * `improvement_empirical_probability`: Similar to
#'   `improvement_probability`, but based directly on optimisation scores
#'   predicted by the individual decision trees.
#'
#'   * `expected_improvement` (default): Computes expected improvement.
#'
#'   * `upper_confidence_bound`: This acquisition function is based on the upper
#'   confidence bound of the distribution (Srinivas et al., 2012).
#'
#'   * `bayes_upper_confidence_bound`: This acquisition function is based on the
#'   upper confidence bound of the distribution (Kaufmann et al., 2012).
#' @param exploration_method (*optional*) Method used to steer exploration in
#'   post-initialisation intensive searching steps. As stated earlier, each SMBO
#'   iteration step compares suggested alternative parameter sets with an
#'   incumbent **best** set in a series of steps. The exploration method
#'   controls how the set of alternative parameter sets is pruned after each
#'   step in an iteration. Can be one of the following:
#'
#'   * `successive_halving` (default): The set of alternative parameter sets is
#'   pruned by removing the worst performing half of the sets after each step
#'   (Jamieson and Talwalkar, 2016).
#'
#'   * `stochastic_reject`: The set of alternative parameter sets is pruned by
#'   comparing the performance of each parameter set with that of the incumbent
#'   **best** parameter set using a paired Wilcoxon test based on shared
#'   bootstraps. Parameter sets that perform significantly worse, at an alpha
#'   level indicated by `smbo_stochastic_reject_p_value`, are pruned.
#'
#'   * `none`: The set of alternative parameter sets is not pruned.
#'
#' @param smbo_stochastic_reject_p_value (*optional*) The p-value threshold used
#'   for the `stochastic_reject` exploration method.
#'
#'   The default value is `0.05`.
#' @param parallel_hyperparameter_optimisation (*optional*) Enable parallel
#'   processing for hyperparameter optimisation. Defaults to `TRUE`. When set to
#'   `FALSE`, this will disable the use of parallel processing while performing
#'   optimisation, regardless of the settings of the `parallel` parameter. The
#'   parameter moreover specifies whether parallelisation takes place within the
#'   optimisation algorithm (`inner`, default), or in an outer loop ( `outer`)
#'   over learners, data subsamples, etc.
#'
#'   `parallel_hyperparameter_optimisation` is ignored if `parallel=FALSE`.
#' @param ... Unused arguments.
#'
#' @return List of parameters related to model hyperparameter optimisation.
#'
#' @references 1. Hutter, F., Hoos, H. H. & Leyton-Brown, K. Sequential
#'   model-based optimization for general algorithm configuration. in Learning
#'   and Intelligent Optimization (ed. Coello, C. A. C.) 6683, 507–523 (Springer
#'   Berlin Heidelberg, 2011).
#'
#'   1. Shahriari, B., Swersky, K., Wang, Z., Adams, R. P. & de Freitas, N.
#'   Taking the Human Out of the Loop: A Review of Bayesian Optimization. Proc.
#'   IEEE 104, 148–175 (2016)
#'
#'   1. Srinivas, N., Krause, A., Kakade, S. M. & Seeger, M. W.
#'   Information-Theoretic Regret Bounds for Gaussian Process Optimization in
#'   the Bandit Setting. IEEE Trans. Inf. Theory 58, 3250–3265 (2012)
#'
#'   1. Kaufmann, E., Cappé, O. & Garivier, A. On Bayesian upper confidence
#'   bounds for bandit problems. in Artificial intelligence and statistics
#'   592–600 (2012).
#'
#'   1. Jamieson, K. & Talwalkar, A. Non-stochastic Best Arm Identification and
#'   Hyperparameter Optimization. in Proceedings of the 19th International
#'   Conference on Artificial Intelligence and Statistics (eds. Gretton, A. &
#'   Robert, C. C.) vol. 51 240–248 (PMLR, 2016).
#'
#'   1. Gramacy, R. B. laGP: Large-Scale Spatial Modeling via Local Approximate
#'   Gaussian Processes in R. Journal of Statistical Software 72, 1–46 (2016)
#'
#'   1. Sparapani, R., Spanbauer, C. & McCulloch, R. Nonparametric Machine
#'   Learning and Efficient Computation with Bayesian Additive Regression Trees:
#'   The BART R Package. Journal of Statistical Software 97, 1–66 (2021)
#' @md
#' @keywords internal
.parse_hyperparameter_optimisation_settings <- function(config=NULL,
                                                        parallel,
                                                        outcome_type,
                                                        optimisation_bootstraps=waiver(),
                                                        optimisation_determine_vimp=waiver(),
                                                        smbo_random_initialisation=waiver(),
                                                        smbo_n_random_sets = waiver(),
                                                        max_smbo_iterations=waiver(),
                                                        smbo_stop_convergent_iterations=waiver(),
                                                        smbo_stop_tolerance=waiver(),
                                                        smbo_step_bootstraps=waiver(),
                                                        smbo_intensify_steps=waiver(),
                                                        smbo_stochastic_reject_p_value=waiver(),
                                                        optimisation_function=waiver(),
                                                        optimisation_metric=waiver(),
                                                        acquisition_function=waiver(),
                                                        exploration_method=waiver(),
                                                        hyperparameter_learner=waiver(),
                                                        parallel_hyperparameter_optimisation=waiver(),
                                                        ...){
  settings <- list()
  
  ##### smbo_random_initialisation #############################################
  # Randomisation of initial parameter grid
  settings$hpo_grid_initialisation_method <- .parse_arg(x_config=config$smbo_random_initialisation,
                                                        x_var=smbo_random_initialisation,
                                                        var_name="smbo_random_initialisation",
                                                        type="character",
                                                        optional=TRUE,
                                                        default="fixed_subsample")
  
  .check_parameter_value_is_valid(x=settings$hpo_grid_initialisation_method,
                                  var_name="smbo_random_initialisation",
                                  values=c("fixed_subsample", "fixed", "random"))
  
  ##### smbo_n_random_sets #####################################################
  # Number of samples in the initial parameter grid.
  settings$hpo_n_grid_initialisation_samples <- .parse_arg(x_config=config$smbo_n_random_sets,
                                                           x_var=smbo_n_random_sets,
                                                           var_name="smbo_n_random_sets",
                                                           type="integer",
                                                           optional=TRUE,
                                                           default=100)
  
  .check_number_in_valid_range(x=settings$hpo_n_grid_initialisation_samples,
                               var_name="smbo_n_random_sets",
                               range=c(10, Inf))
  
  ##### optimisation_determine_vimp ############################################
  # Variable importance for the bootstraps
  settings$hpo_determine_vimp <- .parse_arg(x_config=config$optimisation_determine_vimp,
                                            x_var=optimisation_determine_vimp,
                                            var_name="optimisation_determine_vimp",
                                            type="logical",
                                            optional=TRUE,
                                            default=TRUE)
  
  ##### optimisation_bootstraps ################################################
  # Maximum number of bootstraps for hyperparameter evaluation
  settings$hpo_max_bootstraps <- .parse_arg(x_config=config$optimisation_bootstraps,
                                            x_var=optimisation_bootstraps,
                                            var_name="optimisation_bootstraps",
                                            type="integer",
                                            optional=TRUE,
                                            default=50)
  
  .check_number_in_valid_range(x=settings$hpo_max_bootstraps,
                               var_name="optimisation_bootstraps",
                               range=c(20, Inf))
  
  ##### max_smbo_iterations ####################################################
  # Maximum number of SMBO iterations before stopping
  settings$hpo_smbo_iter_max <- .parse_arg(x_config=config$max_smbo_iterations,
                                           x_var=max_smbo_iterations,
                                           var_name="max_smbo_iterations",
                                           type="integer",
                                           optional=TRUE,
                                           default=20)
  
  .check_number_in_valid_range(x=settings$hpo_smbo_iter_max,
                               var_name="max_smbo_iterations",
                               range=c(1, Inf))
  
  ##### smbo_step_bootstraps ###################################################
  # Maximum number of bootstrap evaluated initially and during each intensify
  # step of each SMBO iteration.
  settings$hpo_bootstraps <- .parse_arg(x_config=config$smbo_step_bootstraps,
                                        x_var=smbo_step_bootstraps,
                                        var_name="smbo_step_bootstraps",
                                        type="integer",
                                        optional=TRUE,
                                        default=3)
  
  .check_number_in_valid_range(x=settings$hpo_bootstraps,
                               var_name="smbo_step_bootstraps",
                               range=c(1, settings$hpo_max_bootstraps))
  
  ##### smbo_intensify_steps ###################################################
  # Maximum number of intensify iterations during each SMBO iteration.
  settings$hpo_intensify_max_iter <- .parse_arg(x_config=config$smbo_intensify_steps,
                                                x_var=smbo_intensify_steps,
                                                var_name="smbo_intensify_steps",
                                                type="integer",
                                                optional=TRUE,
                                                default=5)
  
  .check_number_in_valid_range(x=settings$hpo_intensify_max_iter,
                               var_name="smbo_intensify_steps",
                               range=c(1, Inf))
  
  ##### smbo_stochastic_reject_p_value #########################################
  # Significance level for early stopping of intensity iterations. Only used for
  # stochastic rejection as an exploration method.
  settings$hpo_alpha <- .parse_arg(x_config=config$smbo_stochastic_reject_p_value,
                                   x_var=smbo_stochastic_reject_p_value,
                                   var_name="smbo_stochastic_reject_p_value",
                                   type="numeric",
                                   optional=TRUE,
                                   default=0.05)
  
  .check_number_in_valid_range(x=settings$hpo_alpha,
                               var_name="smbo_stochastic_reject_p_value",
                               range=c(0.0, 1.0),
                               closed=c(FALSE, TRUE))
  
  ##### smbo_stop_convergent_iterations ########################################
  # Number of converging iterations before stopping SMBO.
  settings$hpo_conv_stop <- .parse_arg(x_config=config$smbo_stop_convergent_iterations,
                                       x_var=smbo_stop_convergent_iterations,
                                       var_name="smbo_stop_convergent_iterations",
                                       type="integer",
                                       optional=TRUE,
                                       default=3)

  .check_number_in_valid_range(x=settings$hpo_conv_stop,
                               var_name="smbo_stop_convergent_iterations",
                               range=c(1, Inf))
  
  ##### smbo_stop_tolerance ####################################################
  # Convergence tolerance
  settings$hpo_convergence_tolerance <- .parse_arg(x_config=config$smbo_stop_tolerance,
                                                   x_var=smbo_stop_tolerance,
                                                   var_name="smbo_stop_tolerance",
                                                   type="numeric",
                                                   optional=TRUE,
                                                   default=1E-2)
  
  .check_number_in_valid_range(x=settings$hpo_convergence_tolerance,
                               var_name="smbo_stop_tolerance",
                               range=c(0.0, 2.0),
                               closed=c(FALSE, TRUE))
  
  
  ##### optimisation_function ##################################################
  # Objective function
  settings$hpo_optimisation_function <- .parse_arg(x_config=config$optimisation_function,
                                                   x_var=optimisation_function,
                                                   var_name="optimisation_function",
                                                   type="character",
                                                   optional=TRUE,
                                                   default="balanced")
  
  .check_parameter_value_is_valid(x=settings$hpo_optimisation_function,
                                  var_name="optimisation_function",
                                  values=.get_available_optimisation_functions())
  
  ##### acquisition_function ###################################################
  # Acquisition function
  settings$hpo_acquisition_function <- .parse_arg(x_config=config$acquisition_function,
                                                  x_var=acquisition_function,
                                                  var_name="acquisition_function",
                                                  type="character",
                                                  optional=TRUE,
                                                  default="expected_improvement")
  
  .check_parameter_value_is_valid(x=settings$hpo_acquisition_function,
                                  var_name="acquisition_function",
                                  values=.get_available_acquisition_functions())
  
  ##### exploration_method #####################################################
  # Exploration method
  settings$hpo_exploration_method <- .parse_arg(x_config=config$exploration_method,
                                                x_var=exploration_method,
                                                var_name="exploration_method",
                                                type="character",
                                                optional=TRUE,
                                                default="successive_halving")
  
  .check_parameter_value_is_valid(x=settings$hpo_exploration_method,
                                  var_name="exploration_method",
                                  values=.get_available_hyperparameter_exploration_methods())
  
  ##### optimisation_metric ####################################################
  # Performance metric for hyperparameter optimisation
  settings$hpo_metric <- .parse_arg(x_config=config$optimisation_metric,
                                    x_var=optimisation_metric,
                                    var_name="optimisation_metric",
                                    type="character_list",
                                    optional=TRUE,
                                    default=NULL)
  
  # Set default metric
  if(is.null(settings$hpo_metric)) settings$hpo_metric <- .get_default_metric(outcome_type=outcome_type)
  
  # Check if the metric is ok. Packed into a for loop to enable multi-metric optimisation in the future
  sapply(settings$hpo_metric,
         metric.check_outcome_type,
         outcome_type=outcome_type)
  
  ##### hyperparameter_learner #################################################
  # Hyperparameter learner
  settings$hpo_hyperparameter_learner <- .parse_arg(x_config=config$hyperparameter_learner,
                                                    x_var=hyperparameter_learner,
                                                    var_name="hyperparameter_learner",
                                                    type="character",
                                                    optional=TRUE,
                                                    default="gaussian_process")
  
  .check_parameter_value_is_valid(x=settings$hpo_hyperparameter_learner,
                                  var_name="hyperparameter_learner",
                                  values=.get_available_hyperparameter_learners())
  
  require_package(x=.required_packages_hyperparameter_learner(settings$hpo_hyperparameter_learner),
                  purpose="to use the requested learner (", settings$hpo_hyperparameter_learner, ") for model-based hyperparameter optimisation",
                  message_type="backend_error")
  
  ##### parallel_hyperparameter_optimisation ###################################
  # Parallelisation switch for parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_hyperparameter_optimisation,
                                     x_var=parallel_hyperparameter_optimisation,
                                     var_name="parallel_hyperparameter_optimisation",
                                     type="character",
                                     optional=TRUE,
                                     default="TRUE")
  
  .check_parameter_value_is_valid(x=settings$do_parallel,
                                  var_name="parallel_hyperparameter_optimisation",
                                  values=c("TRUE", "FALSE", "inner", "outer"))
  
  # Disable if parallel is FALSE
  if(!parallel) settings$do_parallel <- "FALSE"
  
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
#'  pre-processing.
#'@param evaluate_top_level_only (*optional*) Flag that signals that only
#'  evaluation at the most global experiment level is required. Consider a
#'  cross-validation experiment with additional external validation. The global
#'  experiment level consists of data that are used for development, internal
#'  validation and external validation. The next lower experiment level are the
#'  individual cross-validation iterations.
#'
#'  When the flag is `true`, evaluations take place on the global level only,
#'  and no results are generated for the next lower experiment levels. In our
#'  example, this means that results from individual cross-validation iterations
#'  are not computed and shown. When the flag is `false`, results are computed
#'  from both the global layer and the next lower level.
#'
#'  Setting the flag to `true` saves computation time.
#'@param skip_evaluation_elements (*optional*) Specifies which evaluation steps,
#'  if any, should be skipped as part of the evaluation process. Defaults to
#'  `none`, which means that all relevant evaluation steps are performed. It can
#'  have one or more of the following values:
#'
#'  * `none`, `false`: no steps are skipped.
#'
#'  * `all`, `true`: all steps are skipped.
#'
#'  * `auc_data`: data for assessing and plotting the area under the receiver
#'  operating characteristic curve are not computed.
#'
#'  * `calibration_data`: data for assessing and plotting model calibration are
#'  not computed.
#'
#'  * `calibration_info`: data required to assess calibration, such as baseline
#'  survival curves, are not collected. These data will still be present in the
#'  models.
#'
#'  * `confusion_matrix`: data for assessing and plotting a confusion matrix are
#'  not collected.
#'
#'  * `decision_curve_analyis`: data for performing a decision curve analysis
#'  are not computed.
#'
#'  * `feature_expressions`: data for assessing and plotting sample clustering
#'  are not computed.
#'
#'  * `feature_similarity`: data for assessing and plotting feature clusters are
#'  not computed.
#'
#'  * `fs_vimp`: data for assessing and plotting feature selection-based
#'  variable importance are not collected.
#'
#'  * `hyperparameters`: data for assessing model hyperparameters are not
#'  collected. These data will still be present in the models.
#'
#'  * `ice_data`: data for individual conditional expectation and partial
#'  dependence plots are not created.
#'
#'  * `model_performance`: data for assessing and visualising model performance
#'  are not created.
#'
#'  * `model_vimp`: data for assessing and plotting model-based variable
#'  importance are not collected.
#'
#'  * `permutation_vimp`: data for assessing and plotting model-agnostic
#'  permutation variable importance are not computed.
#'
#'  * `prediction_data`: predictions for each sample are not made and exported.
#'
#'  * `risk_stratification_data`: data for assessing and plotting Kaplan-Meier
#'  survival curves are not collected.
#'
#'  * `risk_stratification_info`: data for assessing stratification into risk
#'  groups are not computed.
#'
#'  * `univariate_analysis`: data for assessing and plotting univariate feature
#'  importance are not computed.
#'@param ensemble_method (*optional*) Method for ensembling predictions from
#'  models for the same sample. Available methods are:
#'
#'  * `median` (default): Use the median of the predicted values as the ensemble
#'  value for a sample.
#'
#'  * `mean`: Use the mean of the predicted values as the ensemble value for a
#'  sample.
#'
#'  This parameter is only used if `detail_level` is `ensemble`.
#'
#'@param evaluation_metric (*optional*) One or more metrics for assessing model
#'  performance. See the vignette on performance metrics for the available
#'  metrics.
#'
#'  Confidence intervals (or rather credibility intervals) are computed for each
#'  metric during evaluation. This is done using bootstraps, the number of which
#'  depends on the value of `confidence_level` (Davison and Hinkley, 1997).
#'
#'  If unset, the metric in the `optimisation_metric` variable is used.
#'
#'@param sample_limit (*optional*) Set the upper limit of the number of samples
#'  that are used during evaluation steps. Cannot be less than 20.
#'
#'  This setting can be specified per data element by providing a parameter
#'  value in a named list with data elements, e.g.
#'  `list("sample_similarity"=100, "permutation_vimp"=1000)`.
#'
#'  This parameter can be set for the following data elements:
#'  `sample_similarity` and `ice_data`.
#'
#'@param detail_level (*optional*) Sets the level at which results are computed
#'  and aggregated.
#'
#'  * `ensemble`: Results are computed at the ensemble level, i.e. over all
#'  models in the ensemble. This means that, for example, bias-corrected
#'  estimates of model performance are assessed by creating (at least) 20
#'  bootstraps and computing the model performance of the ensemble model for
#'  each bootstrap.
#'
#'  * `hybrid` (default): Results are computed at the level of models in an
#'  ensemble. This means that, for example, bias-corrected estimates of model
#'  performance are directly computed using the models in the ensemble. If there
#'  are at least 20 trained models in the ensemble, performance is computed for
#'  each model, in contrast to `ensemble` where performance is computed for the
#'  ensemble of models. If there are less than 20 trained models in the
#'  ensemble, bootstraps are created so that at least 20 point estimates can be
#'  made.
#'
#'  * `model`: Results are computed at the model level. This means that, for
#'  example, bias-corrected estimates of model performance are assessed by
#'  creating (at least) 20 bootstraps and computing the performance of the model
#'  for each bootstrap.
#'
#'  Note that each level of detail has a different interpretation for bootstrap
#'  confidence intervals. For `ensemble` and `model` these are the confidence
#'  intervals for the ensemble and an individual model, respectively. That is,
#'  the confidence interval describes the range where an estimate produced by a
#'  respective ensemble or model trained on a repeat of the experiment may be
#'  found with the probability of the confidence level. For `hybrid`, it
#'  represents the range where any single model trained on a repeat of the
#'  experiment may be found with the probability of the confidence level. By
#'  definition, confidence intervals obtained using `hybrid` are at least as
#'  wide as those for `ensemble`. `hybrid` offers the correct interpretation if
#'  the goal of the analysis is to assess the result of a single, unspecified,
#'  model.
#'
#'  `hybrid` is generally computationally less expensive then `ensemble`, which
#'  in turn is somewhat less expensive than `model`.
#'
#'  A non-default `detail_level` parameter can be specified for separate
#'  evaluation steps by providing a parameter value in a named list with data
#'  elements, e.g. `list("auc_data"="ensemble", "model_performance"="hybrid")`.
#'  This parameter can be set for the following data elements: `auc_data`,
#'  `decision_curve_analyis`, `model_performance`, `permutation_vimp`,
#'  `ice_data`, `prediction_data` and `confusion_matrix`.
#'
#'@param estimation_type (*optional*) Sets the type of estimation that should be
#'  possible. This has the following options:
#'
#'  * `point`: Point estimates.
#'
#'  * `bias_correction` or `bc`: Bias-corrected estimates. A bias-corrected
#'  estimate is computed from (at least) 20 point estimates, and `familiar` may
#'  bootstrap the data to create them.
#'
#'  * `bootstrap_confidence_interval` or `bci` (default): Bias-corrected
#'  estimates with bootstrap confidence intervals (Efron and Hastie, 2016). The
#'  number of point estimates required depends on the `confidence_level`
#'  parameter, and `familiar` may bootstrap the data to create them.
#'
#'  As with `detail_level`, a non-default `estimation_type` parameter can be
#'  specified for separate evaluation steps by providing a parameter value in a
#'  named list with data elements, e.g. `list("auc_data"="bci",
#'  "model_performance"="point")`. This parameter can be set for the following
#'  data elements: `auc_data`, `decision_curve_analyis`, `model_performance`,
#'  `permutation_vimp`, `ice_data`, and `prediction_data`.
#'
#'@param aggregate_results (*optional*) Flag that signifies whether results
#'  should be aggregated during evaluation. If `estimation_type` is
#'  `bias_correction` or `bc`, aggregation leads to a single bias-corrected
#'  estimate. If `estimation_type` is `bootstrap_confidence_interval` or `bci`,
#'  aggregation leads to a single bias-corrected estimate with lower and upper
#'  boundaries of the confidence interval. This has no effect if
#'  `estimation_type` is `point`.
#'
#'  The default value is equal to `TRUE` except when assessing metrics to assess
#'  model performance, as the default violin plot requires underlying data.
#'
#'  As with `detail_level` and `estimation_type`, a non-default
#'  `aggregate_results` parameter can be specified for separate evaluation steps
#'  by providing a parameter value in a named list with data elements, e.g.
#'  `list("auc_data"=TRUE, , "model_performance"=FALSE)`. This parameter exists
#'  for the same elements as `estimation_type`.
#'
#'@param confidence_level (*optional*) Numeric value for the level at which
#'  confidence intervals are determined. In the case bootstraps are used to
#'  determine the confidence intervals bootstrap estimation, `familiar` uses the
#'  rule of thumb \eqn{n = 20 / ci.level} to determine the number of required
#'  bootstraps.
#'
#'  The default value is `0.95`.
#'
#'@param bootstrap_ci_method (*optional*) Method used to determine bootstrap
#'  confidence intervals (Efron and Hastie, 2016). The following methods are
#'  implemented:
#'
#'  * `percentile` (default): Confidence intervals obtained using the percentile
#'  method.
#'
#'  * `bc`: Bias-corrected confidence intervals.
#'
#'  Note that the standard method is not implemented because this method is
#'  often not suitable due to non-normal distributions. The bias-corrected and
#'  accelerated (BCa) method is not implemented yet.
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
#'  Unlike for `cluster_similarity_threshold`, more than one value can be
#'  supplied here.
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
#'  is used to stratify the samples into two risk groups. For predicted outcome
#'  values that build a continuous spectrum, the two risk groups in the
#'  development cohort will be roughly equal in size.
#'  
#'  * `mean`: The mean predicted value in the development cohort is used to
#'  stratify the samples into two risk groups.
#'  
#'  * `mean_trim`: As `mean`, but based on the set of predicted values
#'  where the 5% lowest and 5% highest values are discarded. This reduces the
#'  effect of outliers.
#'
#'  * `mean_winsor`: As `mean`, but based on the set of predicted values where
#'  the 5% lowest and 5% highest values are winsorised. This reduces the effect
#'  of outliers.
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
#'@param dynamic_model_loading (*optional*) Enables dynamic loading of models
#'  during the evaluation process, if `TRUE`. Defaults to `FALSE`. Dynamic
#'  loading of models may reduce the overall memory footprint, at the cost of
#'  increased disk or network IO. Models can only be dynamically loaded if they
#'  are found at an accessible disk or network location. Setting this parameter
#'  to `TRUE` may help if parallel processing causes out-of-memory issues during
#'  evaluation.
#'@param parallel_evaluation (*optional*) Enable parallel processing for
#'  hyperparameter optimisation. Defaults to `TRUE`. When set to `FALSE`, this
#'  will disable the use of parallel processing while performing optimisation,
#'  regardless of the settings of the `parallel` parameter. The parameter
#'  moreover specifies whether parallelisation takes place within the evaluation
#'  process steps (`inner`, default), or in an outer loop ( `outer`) over
#'  learners, data subsamples, etc.
#'
#'  `parallel_evaluation` is ignored if `parallel=FALSE`.
#'@param ... Unused arguments.
#'
#'@return List of parameters related to model evaluation.
#'
#'@references 1. Davison, A. C. & Hinkley, D. V. Bootstrap methods and their
#'  application. (Cambridge University Press, 1997).
#'
#'  1. Efron, B. & Hastie, T. Computer Age Statistical Inference. (Cambridge
#'  University Press, 2016).
#'
#'  1. Lausen, B. & Schumacher, M. Maximally Selected Rank Statistics.
#'  Biometrics 48, 73 (1992).
#'
#'  1. Hothorn, T. & Lausen, B. On the exact distribution of maximally selected
#'  rank statistics. Comput. Stat. Data Anal. 43, 121–137 (2003).
#'@md
#'@keywords internal
.parse_evaluation_settings <- function(config=NULL,
                                       data,
                                       parallel,
                                       outcome_type,
                                       hpo_metric,
                                       development_batch_id,
                                       vimp_aggregation_method,
                                       vimp_aggregation_rank_threshold,
                                       prep_cluster_method,
                                       prep_cluster_linkage_method,
                                       prep_cluster_cut_method,
                                       prep_cluster_similarity_threshold,
                                       prep_cluster_similarity_metric,
                                       evaluate_top_level_only=waiver(),
                                       skip_evaluation_elements=waiver(),
                                       ensemble_method=waiver(),
                                       evaluation_metric=waiver(),
                                       sample_limit=waiver(),
                                       detail_level=waiver(),
                                       estimation_type=waiver(),
                                       aggregate_results=waiver(),
                                       confidence_level=waiver(),
                                       bootstrap_ci_method=waiver(),
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
                                       time_max=waiver(),
                                       evaluation_times=waiver(),
                                       dynamic_model_loading=waiver(),
                                       parallel_evaluation=waiver(),
                                       ...){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_event <- batch_id <- NULL
  
  settings <- list()
  
  ##### pool_only ##############################################################
  # Flag that limits the depth of the evaluation.
  settings$pool_only <- .parse_arg(x_config=config$evaluate_top_level_only,
                                   x_var=evaluate_top_level_only,
                                   var_name="evaluate_top_level_only",
                                   type="logical",
                                   optional=TRUE,
                                   default=TRUE)
  
  ##### skip_evaluation_elements ###############################################
  # Specify any specific elements of the evaluation to skip.
  settings$evaluation_data_elements <- .parse_arg(x_config=config$skip_evaluation_elements,
                                                  x_var=skip_evaluation_elements,
                                                  var_name="skip_evaluation_elements",
                                                  type="character_list",
                                                  optional=TRUE,
                                                  default="none")
  
  settings$evaluation_data_elements <- tolower(settings$evaluation_data_elements)
  .check_parameter_value_is_valid(x=settings$evaluation_data_elements,
                                  var_name="skip_evaluation_elements",
                                  values=c(.get_available_data_elements(), "none", "false", "all", "true"))
  
  if(any(settings$evaluation_data_elements %in% c("all", "true"))){
    settings$evaluation_data_elements <- .get_available_data_elements()
  }
  
  if(any(settings$evaluation_data_elements %in% c("none", "false"))){
    settings$evaluation_data_elements <- NULL
  }
  
  # Instead of specifying the elements to skip, we specify the elements to keep.
  settings$evaluation_data_elements <- setdiff(.get_available_data_elements(),
                                               settings$evaluation_data_elements)
  
  if("calibration_data" %in% settings$evaluation_data_elements){
    require_package(x="harmonicmeanp",
                    purpose="to compute p-values for model calibration tests",
                    message_type="backend_warning")
  }
  
  if(length(settings$evaluation_data_elements) == 0) settings$evaluation_data_elements <- NULL
  
  # Check whether plotting packages are available if any data elements are
  # computed.
  if(!is.null(settings$evaluation_data_element)){
    require_package(x=..required_plotting_packages(extended=TRUE),
                    purpose="to create plots",
                    message_type="backend_warning")
  }
  
  ##### ensemble_method ########################################################
  # Method for ensemble predictions
  settings$ensemble_method <- .parse_arg(x_config=config$ensemble_method,
                                         x_var=ensemble_method,
                                         var_name="ensemble_method",
                                         type="character",
                                         optional=TRUE,
                                         default="median")
  
  .check_parameter_value_is_valid(x=settings$ensemble_method,
                                  var_name="ensemble_method",
                                  values=.get_available_ensemble_prediction_methods())
  
  ##### evaluation_metric ######################################################
  # List of performance metrics for evaluation
  settings$metric <- .parse_arg(x_config=config$evaluation_metric,
                                x_var=evaluation_metric,
                                var_name="evaluation_metric",
                                type="character_list",
                                optional=TRUE,
                                default=hpo_metric)
  
  sapply(settings$metric, metric.check_outcome_type, outcome_type=outcome_type)
  
  ##### sample_limit ###########################################################
  # Number of samples that should be analysed.
  settings$sample_limit <- .parse_arg(x_config=config$sample_limit,
                                      x_var=sample_limit,
                                      var_name="sample_limit",
                                      type="list",
                                      optional=TRUE,
                                      default=list())
  
  if(length(settings$sample_limit) == 0){
    # Default - use method-specific settings.
    settings$sample_limit <- NULL
    
  } else if(length(settings$sample_limit) == 1 & is.null(names(settings$sample_limit))){
    
    # Check that the contents are a correctly specified. At least 20 samples
    # should be present.
    .check_number_in_valid_range(settings$sample_limit[[1]],
                                 var_name="sample_limit",
                                 range=c(20L, Inf))
    
    # Add provided detail level to each possible element.
    settings$sample_limit <- lapply(.get_available_data_elements(check_has_sample_limit=TRUE),
                                    function(x, sample_limit) (sample_limit),
                                    sample_limit = settings$sample_limit[[1]])
    
    # Add name of respective data elements.
    names(settings$sample_limit) <- .get_available_data_elements(check_has_sample_limit=TRUE)
    
  } else {
    
    # Check that the list elements are correctly specified.
    sapply(names(settings$sample_limit), .check_parameter_value_is_valid,
           var_name="sample_limit (data element name)",
           values=.get_available_data_elements(check_has_sample_limit=TRUE))
    
    # Check that the list contents are correctly specified.
    sapply(names(settings$sample_limit), function(element_name, x){
      
      .check_number_in_valid_range(x[[element_name]],
                                   var_name=paste0("sample_limit (", element_name, ")"),
                                   range=c(20L, Inf))
      
    }, x = settings$sample_limit)
  }
  
  ##### detail_level ###########################################################
  # Level at which evaluations are computed.
  settings$detail_level <- .parse_arg(x_config=config$detail_level,
                                      x_var=detail_level,
                                      var_name="detail_level",
                                      type="list",
                                      optional=TRUE,
                                      default=list())
  
  if(length(settings$detail_level) == 0){
    # Default - use method-specific settings.
    settings$detail_level <- NULL
    
  } else if(length(settings$detail_level) == 1 & is.null(names(settings$detail_level))){
    
    # Check that the contents are a correctly specified, single string.
    .check_parameter_value_is_valid(x=settings$detail_level[[1]],
                                    var_name="detail_level",
                                    values=c("ensemble", "hybrid", "model"))
    
    # Add provided detail level to each possible element.
    settings$detail_level <- lapply(.get_available_data_elements(check_has_detail_level=TRUE),
                                    function(x, detail_level) (detail_level),
                                    detail_level = settings$detail_level[[1]])
    
    # Add name of respective data elements.
    names(settings$detail_level) <- .get_available_data_elements(check_has_detail_level=TRUE)
                                                                 
  } else {
    
    # Check that the list elements are correctly specified.
    sapply(names(settings$detail_level), .check_parameter_value_is_valid,
           var_name="detail_level (data element name)",
           values=.get_available_data_elements(check_has_detail_level=TRUE))
    
    # Check that the list contents are correctly specified.
    sapply(names(settings$detail_level), function(element_name, x){
      .check_parameter_value_is_valid(x[[element_name]],
                                      var_name=paste0("detail_level (", element_name, ")"),
                                      values=c("ensemble", "hybrid", "model"))
    }, x = settings$detail_level)
  }
  
  ##### estimation_type ########################################################
  # Type of estimation performed.
  settings$estimation_type <- .parse_arg(x_config=config$estimation_type,
                                         x_var=estimation_type,
                                         var_name="estimation_type",
                                         type="list", optional=TRUE,
                                         default=list())
  
  if(length(settings$estimation_type) == 0){
    # Default - use method-specific settings.
    settings$estimation_type <- NULL
    
  } else if(length(settings$estimation_type) == 1 & is.null(names(settings$estimation_type))){
    
    # Check that the contents are a correctly specified, single string.
    .check_parameter_value_is_valid(x=settings$estimation_type[[1]],
                                    var_name="estimation_type",
                                    values=c("point", "bias_correction", "bc", "bootstrap_confidence_interval", "bci"))
    
    # Add provided estimation type to each possible element.
    settings$estimation_type <- lapply(.get_available_data_elements(check_has_estimation_type=TRUE),
                                       function(x, estimation_type) (estimation_type),
                                       estimation_type = settings$estimation_type[[1]])
    
    # Add name of respective data elements.
    names(settings$estimation_type) <- .get_available_data_elements(check_has_estimation_type=TRUE)
    
  } else {
    
    # Check that the list elements are correctly specified.
    sapply(names(settings$estimation_type), .check_parameter_value_is_valid,
           var_name="estimation_type (data element name)",
           values=.get_available_data_elements(check_has_estimation_type=TRUE))
    
    # Check that the list contents are correctly specified.
    sapply(names(settings$estimation_type), function(element_name, x){
      .check_parameter_value_is_valid(x[[element_name]],
                                      var_name=paste0("estimation_type (", element_name, ")"),
                                      values=c("point", "bias_correction", "bc", "bootstrap_confidence_interval", "bci"))
    }, x = settings$estimation_type)
  }
  
  ##### aggregate_results ######################################################
  # Aggregate results.
  settings$aggregate_results <- .parse_arg(x_config=config$aggregate_results,
                                           x_var=aggregate_results,
                                           var_name="aggregate_results",
                                           type="list",
                                           optional=TRUE,
                                           default=list())
  
  if(length(settings$aggregate_results) == 0){
    # Default - use method-specific settings.
    settings$aggregate_results <- NULL
    
  } else if(length(settings$aggregate_results) == 1 & is.null(names(settings$aggregate_results))){
    
    # Check that the contents are a correctly specified, single string.
    .check_parameter_value_is_valid(x=tolower(settings$aggregate_results[[1]]),
                                    var_name="aggregate_results",
                                    values=c("true", "false", "none", "all", "default"))
    
    # Add provided aggregate_results value to each possible element.
    settings$aggregate_results <- lapply(.get_available_data_elements(check_has_estimation_type=TRUE),
                                         function(x, aggregate_results) (aggregate_results),
                                         aggregate_results = tolower(settings$aggregate_results[[1]]))
    
    # Add name of respective data elements.
    names(settings$aggregate_results) <- .get_available_data_elements(check_has_estimation_type=TRUE)
    
  } else {
    
    # Check that the list elements are correctly specified.
    sapply(names(settings$aggregate_results), .check_parameter_value_is_valid,
           var_name="aggregate_results (data element name)",
           values=.get_available_data_elements(check_has_estimation_type=TRUE))
    
    # Check that the list contents are correctly specified.
    sapply(names(settings$aggregate_results), function(element_name, x){
      .check_parameter_value_is_valid(tolower(x[[element_name]]),
                                      var_name=paste0("aggregate_results (", element_name, ")"),
                                      values=c("true", "false", "none", "all", "default"))
    }, x = settings$aggregate_results)
  }
  
  ##### bootstrap_ci_method ####################################################
  # Bootstrap confidence interval.
  settings$bootstrap_ci_method <- .parse_arg(x_config=config$bootstrap_ci_method,
                                             x_var=bootstrap_ci_method,
                                             var_name="bootstrap_ci_method",
                                             type="character",
                                             optional=TRUE,
                                             default="percentile")
  
  .check_parameter_value_is_valid(x=settings$bootstrap_ci_method,
                                  var_name="bootstrap_ci_method",
                                  values=.get_available_bootstrap_confidence_interval_methods())
  
  ##### confidence_level #######################################################
  # Width of the confidence intervals
  settings$confidence_level <- .parse_arg(x_config=config$confidence_level,
                                          x_var=confidence_level,
                                          var_name="confidence_level",
                                          type="numeric",
                                          optional=TRUE,
                                          default=0.95)
  
  .check_number_in_valid_range(x=settings$confidence_level,
                               var_name="confidence_level",
                               range=c(0.0, 1.0),
                               closed=c(FALSE, FALSE))
  
  ##### feature_cluster_method #################################################
  # Feature cluster method
  settings$feature_cluster_method <- .parse_arg(x_config=config$feature_cluster_method,
                                                x_var=feature_cluster_method,
                                                var_name="feature_cluster_method",
                                                type="character",
                                                optional=TRUE,
                                                default=prep_cluster_method)
  
  if(any(c("feature_similarity", "univariate_analysis", "feature_expressions", "permutation_vimp") %in% settings$evaluation_data_elements)
     & !settings$feature_cluster_method %in% c("none", "hclust")){
    require_package(x="cluster",
                    purpose="to use feature similarity",
                    message_type="backend_error")
  }
  
  ##### feature_linkage_method #################################################
  # Feature linkage method
  settings$feature_linkage_method <- .parse_arg(x_config=config$feature_linkage_method,
                                                x_var=feature_linkage_method,
                                                var_name="feature_linkage_method",
                                                type="character",
                                                optional=TRUE,
                                                default=prep_cluster_linkage_method)
  
  ##### feature_cluster_cut_method #############################################
  # Feature cluster cluster cut method
  settings$feature_cluster_cut_method <- .parse_arg(x_config=config$feature_cut_method,
                                                    x_var=feature_cluster_cut_method,
                                                    var_name="feature_cluster_cut_method",
                                                    type="character",
                                                    optional=TRUE,
                                                    default=prep_cluster_cut_method)
  
  ##### feature_similarity_metric ##############################################
  # Feature similarity metric
  settings$feature_similarity_metric <- .parse_arg(x_config=config$feature_similarity_metric,
                                                   x_var=feature_similarity_metric,
                                                   var_name="feature_similarity_metric",
                                                   type="character",
                                                   optional=TRUE,
                                                   default=prep_cluster_similarity_metric)
  
  if(any(c("feature_similarity", "univariate_analysis", "feature_expressions", "permutation_vimp") %in% settings$evaluation_data_elements) &
     settings$feature_similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    if(!require_package(x="VGAM",
                        purpose=paste0("to compute log-likelihood pseudo R2 similarity using the ", settings$feature_similarity_metric, " metric"),
                        message_type="backend_warning")){
      
      settings$feature_similarity_metric <- "spearman"
    }
  }
  
  ##### feature_similarity_threshold ###########################################
  # Feature similarity threshold
  settings$feature_similarity_threshold <- .parse_arg(x_config=config$feature_similarity_threshold,
                                                      x_var=feature_similarity_threshold,
                                                      var_name="feature_similarity_threshold",
                                                      type="numeric_list",
                                                      optional=TRUE,
                                                      default=prep_cluster_similarity_threshold)
  
  .check_cluster_parameters(cluster_method=settings$feature_cluster_method,
                            cluster_linkage=settings$feature_linkage_method,
                            cluster_cut_method=settings$feature_cluster_cut_method,
                            cluster_similarity_threshold=settings$feature_similarity_threshold,
                            cluster_similarity_metric=settings$feature_similarity_metric,
                            var_type="feature",
                            test_required_packages=FALSE)
  
  ##### sample_cluster_method ##################################################
  # Sample cluster method
  settings$sample_cluster_method <- .parse_arg(x_config=config$sample_cluster_method,
                                               x_var=sample_cluster_method,
                                               var_name="sample_cluster_method",
                                               type="character",
                                               optional=TRUE,
                                               default=prep_cluster_method)
  
  if(any(c("sample_similarity", "feature_expressions") %in% settings$evaluation_data_elements) &
     !settings$sample_cluster_method %in% c("none", "hclust")){
    require_package(x="cluster",
                    purpose="to use sample similarity",
                    message_type="backend_error")
  }
  
  ##### sample_linkage_method ##################################################
  # Sample cluster linkage method
  settings$sample_linkage_method <- .parse_arg(x_config=config$sample_linkage_method,
                                               x_var=sample_linkage_method,
                                               var_name="sample_linkage_method",
                                               type="character",
                                               optional=TRUE,
                                               default=prep_cluster_linkage_method)
  
  ##### sample_similarity_metric ##################################################
  # Sample similarity metric
  settings$sample_similarity_metric <- .parse_arg(x_config=config$sample_similarity_metric,
                                                  x_var=sample_similarity_metric,
                                                  var_name="sample_similarity_metric",
                                                  type="character",
                                                  optional=TRUE,
                                                  default="gower_winsor")
  
  if(any(c("sample_similarity", "feature_expressions") %in% settings$evaluation_data_elements) &
     settings$sample_similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    if(!require_package(x="VGAM",
                        purpose=paste0("to compute log-likelihood pseudo R2 similarity using the ", settings$sample_similarity_metric, " metric"),
                        message_type="backend_warning")){
      
      settings$sample_similarity_metric <- "gower_winsor"
    }
  }
  
  .check_cluster_parameters(cluster_method=settings$sample_cluster_method,
                            cluster_linkage=settings$sample_linkage_method,
                            cluster_similarity_metric=settings$sample_similarity_metric,
                            var_type="sample",
                            test_required_packages=FALSE)
  
  ##### eval_aggregation_method ################################################
  # Variable importance aggregation methods
  settings$aggregation <- .parse_arg(x_config=config$eval_aggregation_method,
                                     x_var=eval_aggregation_method,
                                     var_name="eval_aggregation_method",
                                     type="character",
                                     optional=TRUE,
                                     default=vimp_aggregation_method)
  
  rank.check_aggregation_method(method=settings$aggregation)
  
  ##### eval_aggregation_rank_threshold ########################################
  # Variable importance rank threshold (used by some aggregation methods)
  settings$aggr_rank_threshold <- .parse_arg(x_config=config$eval_aggregation_rank_threshold,
                                             x_var=eval_aggregation_rank_threshold,
                                             var_name="eval_aggregation_rank_threshold",
                                             type="integer",
                                             optional=TRUE,
                                             default=vimp_aggregation_rank_threshold)
  
  if(!is.null(settings$aggr_rank_threshold)){
    .check_number_in_valid_range(x=settings$aggr_rank_threshold,
                                 var_name="eval_aggregation_rank_threshold",
                                 range=c(1, Inf))
  }
  
  ##### eval_icc_type ##########################################################
  # Type of ICC computed for univariate analysis.
  settings$icc_type <- .parse_arg(x_config=config$eval_icc_type,
                                  x_var=eval_icc_type,
                                  var_name="eval_icc_type",
                                  type="character",
                                  optional=TRUE,
                                  default="1")
  
  .check_parameter_value_is_valid(x=settings$icc_type,
                                  var_name="eval_icc_type",
                                  values=.get_available_icc_types())
  
  ##### stratification_method ##################################################
  # Method used to set stratification thresholds for Kaplan-Meier analysis
  settings$strat_method <- .parse_arg(x_config=config$stratification_method,
                                      x_var=stratification_method,
                                      var_name="stratification_method",
                                      type="character_list",
                                      optional=TRUE,
                                      default="median")
  
  .check_parameter_value_is_valid(x=settings$strat_method,
                                  var_name="stratification_method",
                                  values=.get_available_stratification_methods())
  
  if("optimised" %in% settings$strat_method){
    if(!require_package(x="maxstat",
                        purpose="to determine an optimal risk threshold",
                        message_type="backend_warning")){
      
      settings$strat_method <- setdiff(settings$strat_method, "optimised")
      if(length(settings$strat_method) == 0) settings$strat_method <- "median"
    }
  }
  
  ##### stratification_threshold ###############################################
  # Quantile stratification thresholds for the "fixed" stratification method.
  # Note that c(0.333, 0.667) means that 3 groups are created: a low risk group
  # (up to 0.333), a moderate risk group (between 0.333 and 0.667), and a high
  # risk group (0.667)
  settings$strat_quant_threshold <- .parse_arg(x_config=config$stratification_threshold,
                                               x_var=stratification_threshold,
                                               var_name="stratification_threshold",
                                               type="numeric_list",
                                               optional=TRUE,
                                               default=c(1/3, 2/3))
  
  sapply(settings$strat_quant_threshold,
         .check_number_in_valid_range,
         var_name="stratification_threshold",
         range=c(0.0, 1.0),
         closed=c(FALSE, FALSE))
  
  ##### time_max ###############################################################
  # Study end time (this is used for plotting, and Uno's concordance index).
  settings$time_max <- .parse_arg(x_config=config$time_max,
                                  x_var=time_max,
                                  var_name="time_max",
                                  type="numeric",
                                  optional=TRUE,
                                  default=NULL)
  
  ##### evaluation_times #######################################################
  # Times at which calibration is evaluated for time-to-event (survival) data.
  settings$eval_times <- .parse_arg(x_config=config$evaluation_times,
                                    evaluation_times,
                                    var_name="evaluation_time",
                                    type="numeric_list",
                                    optional=TRUE,
                                    default=NULL)

  # Update time_max and eval_times only if we are dealing with survival
  # endpoints.
  if(outcome_type %in% c("survival")){
  
    # Identify values for time_max if it has not been provided.
    if(is.null(settings$time_max)){
      if(!is.null(settings$eval_times)){
        # Use maximum evaluation time
        settings$time_max <- max(settings$eval_times)
        
      } else {
        # 98th percentile of all outcome times.
        settings$time_max <- stats::quantile(data[outcome_event==1 & batch_id %in% development_batch_id]$outcome_time, probs=0.98, names=FALSE)
      }
    }
    
    .check_number_in_valid_range(settings$time_max,
                                 var_name="time_max",
                                 range=c(0.0, Inf),
                                 closed=c(FALSE, TRUE))
    
    # Identify evaluation times if they were not provided.
    if(is.null(settings$eval_times)) settings$eval_times <- settings$time_max

    sapply(settings$eval_times,
           .check_number_in_valid_range, 
           var_name="evaluation_times",
           range=c(0.0, Inf),
           closed=c(FALSE, TRUE))
  }
  
  ##### dynamic_model_loading ##################################################
  # Dynamic loading of models during evaluation.
  settings$auto_detach <- .parse_arg(x_config=config$dynamic_model_loading,
                                     x_var=dynamic_model_loading,
                                     var_name="dynamic_model_loading",
                                     type="logical",
                                     optional=TRUE,
                                     default=FALSE)
  
  ##### parallel_evaluation ####################################################
  # Parallelisation switch for parallel processing
  settings$do_parallel <- .parse_arg(x_config=config$parallel_evaluation,
                                     x_var=parallel_evaluation,
                                     var_name="parallel_evaluation",
                                     type="character",
                                     optional=TRUE,
                                     default="TRUE")
  
  .check_parameter_value_is_valid(x=settings$do_parallel,
                                  var_name="parallel_evaluation",
                                  values=c("TRUE", "FALSE", "inner", "outer"))
  
  # Disable if parallel is FALSE
  if(!parallel) settings$do_parallel <- "FALSE"
  
  # Return list of settings
  return(settings)
}



.get_all_parameter_names <- function(){
  
  # Find all possible arguments that could be matched.
  available_parameters <- unique(c(names(as.list(args(.parse_file_paths))),
                                   names(as.list(args(.parse_experiment_settings))),
                                   names(as.list(args(.parse_setup_settings))),
                                   names(as.list(args(.parse_preprocessing_settings))),
                                   names(as.list(args(.parse_feature_selection_settings))),
                                   names(as.list(args(.parse_model_development_settings))),
                                   names(as.list(args(.parse_hyperparameter_optimisation_settings))),
                                   names(as.list(args(.parse_evaluation_settings)))))
  
  # Remove "" and "..." and other arguments that are not parameters that can be
  # specified using ... ..
  available_parameters <- setdiff(available_parameters,
                                  c("", "...", "config", "data", "hpo_metric", "prep_cluster_method",
                                    "prep_cluster_linkage_method", "prep_cluster_cut_method",
                                    "prep_cluster_similarity_threshold", "prep_cluster_similarity_metric"))
  
  return(available_parameters)
}



.get_all_configuration_parent_node_names <- function(){
  return(c("paths", "data", "run", "preprocessing", "feature_selection",
           "model_development", "hyperparameter_optimisation", "evaluation"))
}
