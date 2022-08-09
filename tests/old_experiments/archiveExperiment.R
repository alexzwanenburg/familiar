# This contains the archiving function for creating the zip files. We move it
# here so that CRAN doesn't get antsy. This is not a function that should be
# used (or visible to) users, and is not part of familiar.

test_create_experiment_archive <- function(outcome_type=c("binomial", "multinomial", "count", "continuous", "survival")){
  # Creates zip files for testing update_object methods.
  
  # Yield current set of parameters.
  config_parameters <- familiar:::test_generate_experiment_parameters(outcome_type=outcome_type)
  
  # Iterate over parameter sets.
  coro::loop(for(current_parameters in config_parameters){
    
    # Create data.
    data <- familiar:::test_create_good_data_set_random_na_data(outcome_type=current_parameters$outcome_type)
    
    # Generate the experiment directory.
    experiment_dir <- file.path(tempdir(), familiar:::rstring(8L))
    
    # Run the experiment to
    do.call(summon_familiar, args=c(list("data"=data,
                                         "experiment_dir"=experiment_dir,
                                         "parallel"=FALSE),
                                    current_parameters))
    
    # Set the file name of the zip container.
    zip_file_name <- paste0(gsub(pattern=".",
                                 replacement="_",
                                 x=as.character(utils::packageVersion("familiar")),
                                 fixed=TRUE),
                            "_", current_parameters$outcome_type,
                            ".zip")
    
    # Get the old working directory to restore it later.
    current_wd <- getwd()
    
    # Update the working directory so that we can properly zip using short
    # relative paths.
    setwd(experiment_dir)
    
    # Create zip file of the experimental directory.
    utils::zip(zipfile=file.path(current_wd,
                                 "tests",
                                 "old_experiments",
                                 zip_file_name),
               files="./")
    
    # Restore working directory.
    setwd(current_wd)
    
    # Clean up experiment directory.
    unlink(experiment_dir, recursive=TRUE)
  })
}
