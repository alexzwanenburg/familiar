outcome_type <- "survival"

for(outcome_type in c("continuous", "multinomial",  "survival")){
  
  # Get data.
  data <- familiar::test.create_good_data_set(outcome_type = outcome_type)
  
  # Select simple regression models to train with.
  learner <- switch(
    outcome_type,
    "continuous"="glm_gaussian",
    "multinomial"="glm_multinomial",
    "survival"="cox"
  )
  
  # Data elements to skip.
  skip_data_elements <- c("ice_data", "permutation_vimp")
  
  # Create experiment data.
  experiment_data <- familiar::summon_familiar(
    data=data,
    experimental_design="fs+mb",
    fs_method="mim",
    imputation="simple",
    learner=learner,
    estimation_type="point",
    skip_evaluation_elements=skip_data_elements,
    parallel=FALSE)
  
  # Test both familiarCollection and familiarModel objects.
  familiar_collection_list <- list(
    "collection"=experiment_data$familiarCollection,
    "model"=familiar::as_familiar_collection(
      object=experiment_data$familiarModel[[1]],
      estimation_type="point",
      data_element=setdiff(familiar:::.get_available_data_elements()), skip_data_elements,
      familiar_data_names="train")
  )
  
  #### class names -------------------------------------------------------------
  for(collection in familiar_collection_list){
    class_names <- familiar::get_class_names(collection)
  }
  
  #### data set names ----------------------------------------------------------
  for(collection in familiar_collection_list){
    data_set_names <- familiar::get_data_set_names(collection)
  }
  
  #### feature names -----------------------------------------------------------
  for(collection in familiar_collection_list){
    feature_names <- familiar::get_feature_names(collection)
  }
  
  #### vimp names --------------------------------------------------------------
  for(collection in familiar_collection_list){
    vimp_names <- familiar::get_fs_method_names(collection)
  }
  
  #### learner names -----------------------------------------------------------
  for(collection in familiar_collection_list){
    learner_names <- familiar::get_learner_names(collection)
  }
  
  #### risk group names --------------------------------------------------------
  for(collection in familiar_collection_list){
    risk_group_names <- familiar::get_risk_group_names(collection)
  }
}
