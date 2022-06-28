# Skip on CRAN as this test takes quit some time.
testthat::skip_on_cran()

verbose <- FALSE

# Create test parameters.
generate_test_parameters <- coro::generator(function(similarity_metric, similarity_threshold){
  for(outcome_type in c("survival", "binomial", "multinomial", "continuous", "count")){
    for(n_numeric_features in c(4, 3, 2, 1, 0)){
      for(cluster_method in familiar:::.get_available_cluster_methods()){
        for(cluster_linkage in familiar:::.get_available_linkage_methods(cluster_method=cluster_method)){
          for(cluster_cut_method in familiar:::.get_available_cluster_cut_methods(cluster_method=cluster_method)){
            for(cluster_representation_method in familiar:::.get_available_cluster_representation_methods(cluster_method=cluster_method)){
              for(current_similarity_metric in similarity_metric){
                for(current_similarity_threshold in similarity_threshold){
                  coro::yield(list("outcome_type"=outcome_type,
                                   "n_numeric"=n_numeric_features,
                                   "cluster_method"=cluster_method,
                                   "cluster_linkage"=cluster_linkage,
                                   "cluster_cut_method"=cluster_cut_method,
                                   "cluster_representation_method"=cluster_representation_method,
                                   "similarity_metric"=current_similarity_metric,
                                   "similarity_threshold"=current_similarity_threshold))
                }
              }
            }
          }
        }
      }
    }
  }
})

#### Generic test ##############################################################
generic_test <- generate_test_parameters(similarity_metric="spearman",
                                         similarity_threshold=0.8)

while(!coro::is_exhausted(generic_test)){
  
  # Generate parameters.
  parameters <- generic_test()
  
  # Create data,
  data <- familiar:::test_create_synthetic_correlated_data(outcome_type=parameters$outcome_type,
                                                           n_numeric=parameters$n_numeric,
                                                           cluster_size=c(3, 3, 3, 3))
  
  # Create a list of featureInfo objects.
  feature_info_list <- familiar:::.get_feature_info_data(data=data@data,
                                                         file_paths=NULL,
                                                         project_id=character(),
                                                         outcome_type=parameters$outcome_type)[[1]]
  
  # Create cluster skeletons
  feature_info_list <- familiar:::create_cluster_parameter_skeleton(feature_info_list,
                                                                    cluster_method=parameters$cluster_method,
                                                                    cluster_linkage=parameters$cluster_linkage,
                                                                    cluster_cut_method=parameters$cluster_cut_method,
                                                                    cluster_similarity_threshold=parameters$similarity_threshold,
                                                                    cluster_similarity_metric=parameters$similarity_metric,
                                                                    cluster_representation_method=parameters$cluster_representation_method)
  
  # Update feature info with cluster parameters.
  feature_info_list <- familiar:::add_cluster_info(feature_info_list=feature_info_list,
                                                   data=data,
                                                   verbose=verbose)
  
  # Assume that the data is imputed.
  data@preprocessing_level <- "imputation"
  
  # Cluster features.
  data <- familiar:::cluster_features(data=data,
                                      feature_info_list=feature_info_list)
  
}
