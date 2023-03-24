# This contains the archiving function for creating the zip files. We move it
# here so that CRAN doesn't get antsy. This is not a function that should be
# used (or visible to) users, and is not part of familiar.

test_run_archive_experiment <- function(parameters) {
  # Create data.
  data <- familiar:::test_create_good_data_random_missing(
    outcome_type = parameters$outcome_type)

  # Generate the experiment directory.
  experiment_dir <- file.path(tempdir(), familiar:::rstring(8L))

  # Run the experiment to
  do.call(
    familiar::summon_familiar,
    args = c(
      list(
        "data" = data,
        "experiment_dir" = experiment_dir,
        "parallel" = FALSE),
      parameters))

  # Set the file name of the zip container.
  zip_file_name <- paste0(
    gsub(
      pattern = ".",
      replacement = "_",
      x = as.character(utils::packageVersion("familiar")),
      fixed = TRUE),
    "_", parameters$outcome_type,
    ".zip")

  # Get the old working directory to restore it later.
  current_wd <- getwd()

  # Update the working directory so that we can properly zip using short
  # relative paths.
  setwd(experiment_dir)

  # Create zip file of the experimental directory.
  utils::zip(
    zipfile = file.path(
      current_wd,
      "tests",
      "old_experiments",
      zip_file_name),
    files = "./")

  # Restore working directory.
  setwd(current_wd)

  # Clean up experiment directory.
  unlink(experiment_dir, recursive = TRUE)
}

test_create_experiment_archive <- function(
    outcome_type = c("binomial", "multinomial", "count", "continuous", "survival")) {
  # Creates zip files for testing update_object methods.

  test_generate_experiment_parameters <- coro::generator(function(outcome_type) {
    for (current_outcome_type in outcome_type) {
      # Set learner
      learner <- switch(
        current_outcome_type,
        "binomial" = c("glm_logistic", "lasso"),
        "multinomial" = c("glm", "lasso"),
        "count" = c("glm", "lasso"),
        "continuous" = c("glm_gaussian", "lasso"),
        "survival" = c("cox", "survival_regr_weibull"))

      coro::yield(list(
        "experimental_design" = "bs(fs+mb,3)",
        "outcome_type" = current_outcome_type,
        "fs_method" = c("mim", "concordance"),
        "learner" = learner))
    }
  })

  # Yield current set of parameters.
  config_parameters <- coro::collect(
    test_generate_experiment_parameters(outcome_type = outcome_type))

  cl <- parallel::makeCluster(type = "PSOCK", length(config_parameters))

  # Iterate over parameter sets.
  parallel::parLapply(
    cl = cl,
    X = config_parameters,
    test_run_archive_experiment)

  parallel::stopCluster(cl)
}
