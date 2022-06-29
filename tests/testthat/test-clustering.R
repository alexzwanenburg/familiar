# Skip on CRAN as this test takes quit some time.
testthat::skip_on_cran()

verbose <- FALSE

# Create test parameters. This is actually a generator, which makes it easier to
# nest all the different combinations that should be checked.
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

# TODO: test the following:
# 1. Number of clusters that are being formed.
# 2. Signature features are assigned their own cluster.
# 3. Clustering works for special cases with one/two/three features.
# 4. Clusters should be formed by the same base feature.
# 5. Sample-clustering should function correctly, and cluster around different
# batches.



#### Generic test ##############################################################
generic_test <- generate_test_parameters(similarity_metric="spearman",
                                         similarity_threshold=0.8)

while(TRUE){
  
  # Generate parameters.
  parameters <- generic_test()
  if(coro::is_exhausted(parameters)) break()
  
  # Set cluster size
  cluster_size <- c(3, 3, 3, 3)
  
  # Create data,
  data <- familiar:::test_create_synthetic_correlated_data(outcome_type=parameters$outcome_type,
                                                           n_numeric=parameters$n_numeric,
                                                           cluster_size=cluster_size)
  
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
  clustered_data <- familiar:::cluster_features(data=data,
                                                feature_info_list=feature_info_list)
  
  
  # Create clustering table.
  cluster_table <- familiar:::.create_clustering_table(feature_info_list=feature_info_list)
  
  # Perform tests
  testthat::test_that(paste0("Clustering works correctly for the ", parameters$cluster_method, " clustering method, ",
                             "with cluster cut method \"", parameters$cluster_cut_method, "\" and ",
                             "cluster representation method \"", parameters$cluster_representation_method, "\" for",
                             "normal synthetic data with a ", parameters$outcome_type, " outcome."),
                      {
                        # Tests that the number of created clusters is correct.
                        if(parameters$cluster_method == "none"){
                          testthat::expect_equal(familiar:::get_n_features(clustered_data),
                                                 sum(cluster_size))

                        } else {
                          testthat::expect_equal(familiar:::get_n_features(clustered_data),
                                                 length(cluster_size))
                        }
                        
                        # For cluster methods other than none, check that the
                        # clusters are formed by clusters with the same feature.
                        if(parameters$cluster_method != "none"){
                          for(ii in seq_along(cluster_size)){
                            base_feature_name <- paste0("feature_", ii)
                            n_matching_features <- sapply(split(cluster_table, by="cluster_name"), function(x, base_feature_name){
                              sum(grepl(pattern=base_feature_name,
                                        x=x$feature_name))
                            }, base_feature_name=base_feature_name)
                            
                            testthat::expect_equal(any(n_matching_features == cluster_size[ii]), TRUE)
                          }
                        }
                      })
}
