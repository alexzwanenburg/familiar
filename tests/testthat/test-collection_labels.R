outcome_type <- "multinomial"

for(outcome_type in c("continuous", "multinomial",  "survival")){
  
  # Get data.
  data <- familiar:::test.create_good_data_set(outcome_type = outcome_type)
  
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
    imputation_method="simple",
    learner=learner,
    estimation_type="point",
    skip_evaluation_elements=skip_data_elements,
    parallel=FALSE)
  
  # Test both familiarCollection and familiarModel objects.
  familiar_collection_list <- list(
    "collection"=experiment_data$familiarCollection,
    "model"=familiar::as_familiar_collection(
      object=experiment_data$familiarModel,
      data=data,
      familiar_data_names="development",
      estimation_type="point",
      data_element=setdiff(familiar:::.get_available_data_elements(), skip_data_elements))
  )
  
  #### class names -------------------------------------------------------------
  for(collection in familiar_collection_list){
    class_names <- familiar::get_class_names(collection)
    
    testthat::test_that(
      "Class names are correctly read and updated.",
      {
        if(outcome_type %in% c("binomial", "multinomial")){
          # Test equality of the set.
          testthat::expect_setequal(class_names, levels(data@data$outcome))
          
          # Replace class names.
          new_class_names <- paste0("class_", seq_along(class_names))
          collection <- familiar::set_class_names(
            collection,
            old=rev(class_names),
            new=rev(new_class_names))
          
          # Expect that the labels are the same and have the same order as the
          # original labels.
          testthat::expect_equal(
            familiar::get_class_names(collection),
            new_class_names
          )
          
          # Reorder levels.
          collection <- familiar::set_class_names(
            collection,
            order=rev(new_class_names)
          )
          
          # Expect that the labels are now re-ordered.
          testthat::expect_equal(
            familiar::get_class_names(collection),
            rev(new_class_names)
          )
          
        } else {
          testthat::expect_equal(class_names, character(0L))
        }
      }
    )
  }
  
  #### data set names ----------------------------------------------------------
  for(collection in familiar_collection_list){
    data_set_names <- familiar::get_data_set_names(collection)
    
    testthat::test_that(
      "Dataset names are correct",
      {
        testthat::expect_setequal(data_set_names, "development")
      }
    )
  }
  
  #### feature names -----------------------------------------------------------
  for(collection in familiar_collection_list){
    feature_names <- familiar::get_feature_names(collection)
    
    testthat::test_that(
      "Dataset names are correct",
      {
        testthat::expect_equal(all(feature_names %in% familiar:::get_feature_columns(data)), TRUE)
      }
    )
  }
  
  #### vimp names --------------------------------------------------------------
  for(collection in familiar_collection_list){
    vimp_names <- familiar::get_fs_method_names(collection)
    
    testthat::test_that(
      "VIMP names are correct",
      {
        testthat::expect_equal(vimp_names, "mim")
      }
    )
  }
  
  #### learner names -----------------------------------------------------------
  for(collection in familiar_collection_list){
    learner_names <- familiar::get_learner_names(collection)
    
    testthat::test_that(
      "Learner names are correct",
      {
        testthat::expect_equal(learner_names, learner)
      }
    )
  }
  
  #### risk group names --------------------------------------------------------
  for(collection in familiar_collection_list){
    risk_group_names <- familiar::get_risk_group_names(collection)
    
    testthat::test_that(
      "Risk group names are correct",
      {
        if(outcome_type %in% c("survival")){
          testthat::expect_setequal(risk_group_names, )
          
        } else {
          testthat::expect_equal(risk_group_names, character(0L))
        }
      }
    )
  }
}
