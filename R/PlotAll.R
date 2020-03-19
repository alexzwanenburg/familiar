#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####plot_all (collection)#####
setMethod("plot_all", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            if(!is.null(dir_path)){
              dir_path <- encapsulate_path(dir_path)
            }
            
            # Feature univariate p-values (horizontal bars)
            do.call(plot_univariate_importance, args=append(list("object"=object,
                                                                 "dir_path"=dir_path),
                                                            list(...)))
            
            # Feature occurrence (unclustered)
            do.call(plot_feature_occurrence, args=append(list("object"=object,
                                                              "dir_path"=dir_path),
                                                         list(...)))
            
            # Feature ranking (unclustered)
            do.call(plot_feature_ranks, args=append(list("object"=object,
                                                         "dir_path"=dir_path),
                                                    list(...)))
            
            # Feature signature ranking (unclustered)
            do.call(plot_model_signature_ranks, args=append(list("object"=object,
                                                                 "dir_path"=dir_path),
                                                            list(...)))
            
            # Feature similarity heatmap
            do.call(plot_feature_similarity, args=append(list("object"=object,
                                                              "dir_path"=dir_path),
                                                         list(...)))
            
            # Calibration curves
            do.call(plot_calibration_data, args=append(list("object"=object,
                                                            "dir_path"=dir_path),
                                                       list(...)))
            
            # Model performance
            do.call(plot_model_performance, args=append(list("object"=object,
                                                             "dir_path"=dir_path),
                                                        list(...)))
            
            # AUC curve
            do.call(plot_auc_roc_curve, args=append(list("object"=object,
                                                         "dir_path"=dir_path),
                                                    list(...)))
            
            # Kaplan-Meier curves
            do.call(plot_kaplan_meier, args=append(list("object"=object,
                                                        "dir_path"=dir_path),
                                                   list(...)))
            
            # Feature expressions
            
            # Confusion matrix
            
            # Decision curve analysis.
            
          })

#####plot_all (list)#####
setMethod("plot_all", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="all"), list(...)))
            
            return(do.call(plot_all,
                           args=append(list("object"=object,
                                            "dir_path"=dir_path),
                                       list(...))))
          })
