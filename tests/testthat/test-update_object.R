testthat::skip("Testing object updates should be done manually.")

# List files in tests/old_experiments.
archive_files <- list.files(path="./tests/old_experiments", pattern=".zip", full.names=TRUE)

# Set path to the directory where the archive should be unpacked.
archive_directory <- file.path(".", "tests", "old_experiments", "_experiment")

# Iterate over archive files.
for(archive_file in archive_files){
  
  # Determine outcome type by stripping the extension, underscore and version.
  outcome_type <- gsub(x=basename(archive_file),
                       pattern="[[:digit:]]+|[.]zip$|[_]",
                       replacement="")
  
  # Determine the version by stripping the extension and all alpha-characters.
  familiar_version <- gsub(x=basename(archive_file),
                           pattern="[[:alpha:]]+|[.]zip$",
                           replacement="")
  
  # Strip final underscore.
  familiar_version <- gsub(x=familiar_version,
                           pattern="[_]$",
                           replacement="")
  
  # Replace underscores by period and convert to package version object.
  familiar_version <- as.package_version(gsub(x=familiar_version,
                                              pattern="[_]",
                                              replacement="."))
  
  # Avoid running tests if the archived version for some reason exceeds the
  # installed familiar version.
  if(utils::packageVersion("familiar") < familiar_version){
    warning("The installed version of familiar (",
            utils::packageVersion("familiar"),
            ") is older than the archive being tested (",
            familiar_version, ").")
    
    next()
  }
  
  # Unzip directory.
  utils::unzip(archive_file,
               exdir=archive_directory,
               overwrite=TRUE)
  
  # Yield current set of parameters.
  config_parameters <- familiar:::test_generate_experiment_parameters(outcome_type=outcome_type)()
  
  # Recreate data.
  data <- familiar:::test_create_good_data_set_random_na_data(outcome_type=outcome_type)
  
  #### Test 1: Running the experiment again ------------------------------------
  
  message(paste0("-------------------------------------------------------------\n",
                 outcome_type, " (", familiar_version, "): ",
                 "test direct export\n",
                 "-------------------------------------------------------------\n"))
  
  
  # Remove the results directory.
  unlink(file.path(archive_directory, "results"),
         recursive=TRUE)
  
  # Run the experiment to extract the data again.
  do.call(familiar::summon_familiar,
          args=c(list("data"=data,
                      "experiment_dir"=archive_directory,
                      "experiment_data"=list.files(path=archive_directory,
                                                   full.names=TRUE,
                                                   pattern="iterations.RDS"),
                      "parallel"=FALSE),
                 config_parameters))
  
  
  #### Test 2: Running the experiment again, starting with familiar data objects -------
  
  message(paste0("-------------------------------------------------------------\n",
                 outcome_type, " (", familiar_version, "): ",
                 "test export after collecting familiarData objects\n",
                 "-------------------------------------------------------------\n"))
  
  # Remove the results directory.
  unlink(file.path(archive_directory, "results"),
         recursive=TRUE)
  
  # Remove the familiar_collections directory.
  unlink(file.path(archive_directory, "familiar_collections"),
         recursive=TRUE)
  
  # Run the experiment to extract the data again.
  do.call(familiar::summon_familiar,
          args=c(list("data"=data,
                      "experiment_dir"=archive_directory,
                      "experiment_data"=list.files(path=archive_directory,
                                                   full.names=TRUE,
                                                   pattern="iterations.RDS"),
                      "parallel"=FALSE),
                 config_parameters))
  
  #### Test 3: Running the experiment again, starting with ensembles -----------
  
  message(paste0("-------------------------------------------------------------\n",
                 outcome_type, " (", familiar_version, "): ",
                 "test export after creating familiarData objects\n",
                 "-------------------------------------------------------------\n"))
  
  # Remove the results directory.
  unlink(file.path(archive_directory, "results"),
         recursive=TRUE)
  
  # Remove the familiar_collections directory.
  unlink(file.path(archive_directory, "familiar_collections"),
         recursive=TRUE)
  
  # Remove the familiar_data directory.
  unlink(file.path(archive_directory, "familiar_data"),
         recursive=TRUE)
  
  # Run the experiment to extract the data again.
  do.call(familiar::summon_familiar,
          args=c(list("data"=data,
                      "experiment_dir"=archive_directory,
                      "experiment_data"=list.files(path=archive_directory,
                                                   full.names=TRUE,
                                                   pattern="iterations.RDS"),
                      "estimation_type"="point",
                      "parallel"=FALSE),
                 config_parameters))
  
  # Remove temporary directory.
  unlink(file.path(archive_directory),
         recursive=TRUE)
}
