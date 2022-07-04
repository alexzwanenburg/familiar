# Skip on CRAN as this test takes quit some time.
testthat::skip_on_cran()

verbose <- FALSE

# Create test parameters. This is actually a generator, which makes it easier to
# nest all the different combinations that should be checked.
generate_test_parameters <- coro::generator(function(similarity_metric,
                                                     similarity_threshold,
                                                     n_numeric_features,
                                                     cluster_method=NULL,
                                                     cluster_linkage=NULL,
                                                     cluster_cut_method=NULL,
                                                     cluster_representation_method=NULL){
  
  for(n_numeric_features in n_numeric_features){
    
    if(is.null(cluster_method)){
      local_cluster_method <- familiar:::.get_available_cluster_methods()
      
    } else {
      local_cluster_method <- intersect(cluster_method,
                                        familiar:::.get_available_cluster_methods())
    }
    
    for(current_cluster_method in local_cluster_method){
      
      if(is.null(cluster_linkage)){
        local_cluster_linkage <- familiar:::.get_available_linkage_methods(cluster_method=current_cluster_method)
        
      } else {
        local_cluster_linkage <- intersect(cluster_linkage,
                                           familiar:::.get_available_linkage_methods(cluster_method=current_cluster_method))
      }
      
      for(current_cluster_linkage in local_cluster_linkage){
        
        if(is.null(cluster_cut_method)){
          local_cluster_cut_method <- familiar:::.get_available_cluster_cut_methods(cluster_method=current_cluster_method)
          
        } else {
          local_cluster_cut_method <- intersect(cluster_cut_method,
                                                familiar:::.get_available_cluster_cut_methods(cluster_method=current_cluster_method))
        }
        
        for(current_cluster_cut_method in local_cluster_cut_method){
          
          if(is.null(cluster_representation_method)){
            local_cluster_representation_method <- familiar:::.get_available_cluster_representation_methods(cluster_method=current_cluster_method)
          } else {
            local_cluster_representation_method <- intersect(cluster_representation_method,
                                                             familiar:::.get_available_cluster_representation_methods(cluster_method=current_cluster_method))
          }
          
          for(current_cluster_representation_method in local_cluster_representation_method){
            
            # Set outcome type to avoid redundant tests where outcome type does
            # not matter.
            if(current_cluster_representation_method %in% c("best_predictor")){
              available_outcome_type <- c("survival", "binomial", "multinomial", "continuous", "count")
            } else {
              available_outcome_type <- c("binomial")
            }
            
            for(outcome_type in available_outcome_type){
              for(current_similarity_metric in similarity_metric){
                for(current_similarity_threshold in similarity_threshold){
                  coro::yield(list("outcome_type"=outcome_type,
                                   "n_numeric"=n_numeric_features,
                                   "cluster_method"=current_cluster_method,
                                   "cluster_linkage"=current_cluster_linkage,
                                   "cluster_cut_method"=current_cluster_cut_method,
                                   "cluster_representation_method"=current_cluster_representation_method,
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

# Note: use of spearman or other correlation metrics causes failing tests
# because categorical variables are encoded for such metrics.
#
# Note: only test all-numeric, 1 mixed and all-categorical options to limit the
# number of tests that have to be performed.

generic_test <- generate_test_parameters(similarity_metric="mcfadden_r2",
                                         similarity_threshold=0.99,
                                         n_numeric_features=c(4, 3, 0))

while(TRUE){
  
  # Generate parameters.
  parameters <- generic_test()
  if(coro::is_exhausted(parameters)) break()
  
  # Set cluster size
  cluster_size <- c(2, 3, 3, 3)
  
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
                             "cluster representation method \"", parameters$cluster_representation_method, "\" for ",
                             "normal synthetic data with a ", parameters$outcome_type, " outcome."),
                      {
                        # Assert that the number of created clusters is correct.
                        if(parameters$cluster_cut_method == "none" | parameters$cluster_representation_method == "none"){
                          testthat::expect_equal(familiar:::get_n_features(clustered_data),
                                                 sum(cluster_size))

                        } else {
                          testthat::expect_equal(familiar:::get_n_features(clustered_data),
                                                 length(cluster_size))
                        }
                        
                        if(parameters$cluster_cut_method != "none" & parameters$cluster_representation_method != "none"){
                         
                          # Assert that the individual clusters have their expected size.
                          for(ii in seq_along(cluster_size)){
                            # Get the base feature name
                            base_feature_name <- paste0("feature_", ii)
                            
                            # Compute the number of features in each cluster
                            # that start with the base feature name.
                            n_matching_features <- sapply(split(cluster_table, by="cluster_name"), function(x, base_feature_name){
                              sum(grepl(pattern=base_feature_name,
                                        x=x$feature_name))
                            }, base_feature_name=base_feature_name)
                            
                            # Test that the number of features that start with
                            # the base feature name match the expected cluster
                            # size.
                            testthat::expect_equal(any(n_matching_features == cluster_size[ii]), TRUE)
                          }
                          
                          # Check representative feature.
                          if(!parameters$cluster_representation_method %in% c("mean", "none")){
                            for(ii in seq_along(cluster_size)){
                              
                              # Get the base feature name
                              base_feature_name <- paste0("feature_", ii)
                              
                              # Get cluster corresponding to the base feature
                              # name.
                              x <- cluster_table[grepl(pattern=base_feature_name, x=cluster_name)]
                              
                              # Check that only one required feature per cluster
                              # exists.
                              testthat::expect_equal(sum(x$feature_required), 1L)
                              
                              # Create name for representative features and
                              # cluster name.
                              representative_feature <- x[feature_required == TRUE]$feature_name[1]
                              current_cluster_name <- paste0(representative_feature, "_cluster")
                              
                              # Test that the weight for the representative
                              # feature is 1.0.
                              testthat::expect_equal(feature_info_list[[representative_feature]]@cluster_parameters@weight,
                                                     1.0)
                              
                              # Test that the cluster size is as advertised.
                              testthat::expect_equal(feature_info_list[[representative_feature]]@cluster_parameters@cluster_size,
                                                     cluster_size[ii])
                              
                              # Test again that the cluster size is correct, but
                              # now using x.
                              testthat::expect_equal(nrow(x), cluster_size[ii])
                            }
                          }
                          
                          # Check weights.
                          if(!parameters$cluster_representation_method %in% c("none")){
                            for(ii in seq_along(cluster_size)){
                              
                              # Get the base feature name
                              base_feature_name <- paste0("feature_", ii)
                              
                              # Get cluster corresponding to the base feature
                              # name.
                              x <- cluster_table[grepl(pattern=base_feature_name, x=cluster_name)]
                              current_cluster_name <- x$cluster_name[1]
                              
                              # Obtain weights from all features in the cluster.
                              cluster_features <- x$feature_name
                              feature_weights <- sapply(feature_info_list[cluster_features], function(x) (x@cluster_parameters@weight))
                              
                              # Assert that the sum of the weights is 1.0.
                              testthat::expect_equal(sum(feature_weights), 1.0)
                              
                              if(parameters$cluster_representation_method == "mean"){
                                if(any(sapply(feature_info_list[cluster_features], function(x) (x@feature_type)) == "factor")){
                                  # For clusters consisting of categorical or
                                  # mixed features, there is one feature with
                                  # non-zero weight. "mean" is not really
                                  # definable for such feature sets.
                                  testthat::expect_equal(nrow(x[feature_required == TRUE]), 1L)
                                  
                                } else {
                                  # Assert that all weights are the same.
                                  testthat::expect_equal(all(feature_weights == feature_weights[1]), TRUE)
                                }
                              }
                            }
                          }
                        }
                        
                        # Check whether data is correctly presented.
                        for(x in split(cluster_table, by="cluster_name")){
                          
                          # Do not check if more than feature is required.
                          if(sum(x$feature_required) > 1L) next()
                          
                          # Find the representative feature.
                          representative_feature <- x[feature_required == TRUE]$feature_name
                          current_cluster_name <- x$cluster_name[1]
                          
                          # Check that the representative feature has weight
                          # 1.0.
                          testthat::expect_equal(feature_info_list[[representative_feature]]@cluster_parameters@weight, 1.0)
                          
                          # Test that the cluster data for the data and
                          # clustered_data data sets are the same.
                          testthat::expect_equal(clustered_data@data[[current_cluster_name]],
                                                 data@data[[representative_feature]])
                        }
                      })
}


#### One feature dataset -------------------------------------------------------
generic_test <- generate_test_parameters(similarity_metric="mcfadden_r2",
                                         similarity_threshold=0.99,
                                         n_numeric_features=c(4, 0))

while(TRUE){
  
  # Generate parameters.
  parameters <- generic_test()
  if(coro::is_exhausted(parameters)) break()
  
  # Set cluster size. 
  cluster_size <- c(1, 1, 1, 1)
  
  # Create data,
  data <- familiar:::test_create_synthetic_correlated_data(outcome_type=parameters$outcome_type,
                                                           n_numeric=parameters$n_numeric,
                                                           cluster_size=cluster_size)
  
  # Select only first feature.
  data <- familiar:::select_features(data=data, features="feature_1")
  
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
                             "cluster representation method \"", parameters$cluster_representation_method, "\" for ",
                             "normal synthetic data with a single feature column and a ", parameters$outcome_type, " outcome."),
                      {
                        # Test that exactly one cluster is formed.
                        testthat::expect_equal(familiar:::get_n_features(clustered_data), 1L)
                        
                        # Check that the cluster is formed by one feature.
                        testthat::expect_equal(nrow(cluster_table), 1L)
                        
                        # Check that the representative feature has weight
                        # 1.0.
                        testthat::expect_equal(feature_info_list[["feature_1"]]@cluster_parameters@weight, 1.0)
                        
                        # Test that the cluster data for the data and
                        # clustered_data data sets are the same.
                        testthat::expect_equal(clustered_data@data[["feature_1"]],
                                               data@data[["feature_1"]])
                      })
}


#### Two-feature dataset (uncorrelated) ----------------------------------------
generic_test <- generate_test_parameters(similarity_metric="mcfadden_r2",
                                         similarity_threshold=0.99,
                                         n_numeric_features=c(4, 3, 0))

while(TRUE){
  
  # Generate parameters.
  parameters <- generic_test()
  if(coro::is_exhausted(parameters)) break()
  
  # Set cluster size. 
  cluster_size <- c(1, 1, 1, 1)
  
  # Create data,
  data <- familiar:::test_create_synthetic_correlated_data(outcome_type=parameters$outcome_type,
                                                           n_numeric=parameters$n_numeric,
                                                           cluster_size=cluster_size)
  
  # Select the first two features.
  data <- familiar:::select_features(data=data, features=c("feature_1", "feature_2"))
  
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
                             "cluster representation method \"", parameters$cluster_representation_method, "\" for ",
                             "normal synthetic data with two uncorrelated feature columns and a ", parameters$outcome_type, " outcome."),
                      {
                        # Test that exactly two clusters are formed.
                        testthat::expect_equal(familiar:::get_n_features(clustered_data), 2L)
                        
                        # Check whether data is correctly presented.
                        for(x in split(cluster_table, by="cluster_name")){
                          
                          # Find the representative feature.
                          representative_feature <- x[feature_required == TRUE]$feature_name
                          current_cluster_name <- x$cluster_name[1]
                          
                          # Check that the clusters are singular.
                          testthat::expect_equal(nrow(x), 1L)
                          
                          # Check that the representative feature has weight
                          # 1.0.
                          testthat::expect_equal(feature_info_list[[representative_feature]]@cluster_parameters@weight, 1.0)
                          
                          # Test that the cluster data for the data and
                          # clustered_data data sets are the same.
                          testthat::expect_equal(clustered_data@data[[current_cluster_name]],
                                                 data@data[[representative_feature]])
                        }
                      })
}



#### Two-feature dataset (correlated) ----------------------------------------
generic_test <- generate_test_parameters(similarity_metric="mcfadden_r2",
                                         similarity_threshold=0.99,
                                         n_numeric_features=c(4, 0))

while(TRUE){
  
  # Generate parameters.
  parameters <- generic_test()
  if(coro::is_exhausted(parameters)) break()
  
  # Set cluster size. 
  cluster_size <- c(2, 1, 1, 1)
  
  # Create data,
  data <- familiar:::test_create_synthetic_correlated_data(outcome_type=parameters$outcome_type,
                                                           n_numeric=parameters$n_numeric,
                                                           cluster_size=cluster_size)
  
  # Select only first two feature.
  data <- familiar:::select_features(data=data, features=c("feature_1_A", "feature_1_B"))
  
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
                             "cluster representation method \"", parameters$cluster_representation_method, "\" for ",
                             "normal synthetic data with two correlated feature columns and a ", parameters$outcome_type, " outcome."),
                      {
                        
                        if(parameters$cluster_method %in% c("none") | parameters$cluster_cut_method == "none" | parameters$cluster_representation_method == "none"){
                          # Test that exactly two clusters of single features
                          # are formed.
                          testthat::expect_equal(familiar:::get_n_features(clustered_data), 2L)
                          
                        } else {
                          # Test that exactly one cluster of two features is
                          # formed.
                          testthat::expect_equal(familiar:::get_n_features(clustered_data), 1L)
                        }
                      })
}
