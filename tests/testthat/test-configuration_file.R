testthat::skip_if_not_installed("xml2")

# Find path to configuration file in package.
config <- system.file("config.xml", package="familiar")

# Load the configuration file
config <- familiar:::.load_configuration_file(config)

# Check whether the main branches have the correct names.
# Find names of parent nodes.
config_node_names <- names(config)
expected_node_names <- familiar:::.get_all_configuration_parent_node_names()

testthat::test_that("1. All parent nodes are present in the configuration file.", {
  testthat::expect_equal(all(config_node_names %in% expected_node_names), TRUE)
  testthat::expect_equal(all(expected_node_names %in% config_node_names), TRUE)
})

testthat::test_that("2. All parameters are specified.", {
  config_args <- unique(unlist(sapply(config, names)))
  
  testthat::expect_equal(all(config_args %in% familiar:::.get_all_parameter_names()), TRUE)
  testthat::expect_equal(all(familiar:::.get_all_parameter_names() %in% config_args), TRUE)
  
  if(!all(familiar:::.get_all_parameter_names() %in% config_args)){
    warning(paste0("The following parameters are not specified in the configuration file: ",
                   paste0(setdiff(familiar:::.get_all_parameter_names(), config_args), collapse=", ")))
  }
  
  if(!all(config_args %in% familiar:::.get_all_parameter_names())){
    warning(paste0("The following parameters are specified in the configuration file, but not familiar: ",
                   paste0(setdiff(config_args, familiar:::.get_all_parameter_names()), collapse=", ")))
  }
  
})

for(parent_node in expected_node_names){
  
  # Identify the parsing function for the node.
  FUN <- switch(parent_node,
                paths = familiar:::.parse_file_paths,
                data = familiar:::.parse_experiment_settings,
                run = familiar:::.parse_setup_settings,
                preprocessing = familiar:::.parse_preprocessing_settings,
                feature_selection = familiar:::.parse_feature_selection_settings,
                model_development = familiar:::.parse_model_development_settings,
                hyperparameter_optimisation = familiar:::.parse_hyperparameter_optimisation_settings,
                evaluation = familiar:::.parse_evaluation_settings)
  
  # Find the expected arguments.
  expected_config_args <- names(as.list(args(FUN)))
  
  # Remove "" and "..." and other arguments that are not parameters that can be
  # specified using ... .
  expected_config_args <- intersect(expected_config_args, familiar:::.get_all_parameter_names())
  
  # Remove specific arguments unless they are shared by a specific function.
  if(parent_node != "data"){
    expected_config_args <- setdiff(expected_config_args, c("outcome_type", "development_batch_id"))
  }
  
  if(parent_node != "run"){
    expected_config_args <- setdiff(expected_config_args, "parallel")
  }
  
  if(parent_node != "feature_selection"){
    expected_config_args <- setdiff(expected_config_args, 
                                    c("vimp_aggregation_rank_threshold", "vimp_aggregation_method"))
  }
  
  # Find the argument names.
  config_args <- names(config[[parent_node]])
  
  testthat::test_that(paste0("3. All parameters are specified for the \"", parent_node, "\" node."), {
    
    testthat::expect_equal(all(config_args %in% expected_config_args), TRUE)
    testthat::expect_equal(all(expected_config_args %in% config_args), TRUE)
    
    if(!all(expected_config_args %in% config_args)){
      warning(paste0("The following parameters are not specified in the configuration file under node \"",
                     parent_node, "\": ",
                     paste0(setdiff(expected_config_args, config_args), collapse=", ")))
    }
    
    if(!all(config_args %in% expected_config_args)){
      warning(paste0("The following parameters are specified in the configuration file under node \"",
                     parent_node, "\" but not familiar: ",
                     paste0(setdiff(config_args, expected_config_args), collapse=", ")))
    }
  })
}
