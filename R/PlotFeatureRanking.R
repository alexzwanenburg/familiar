#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#'@title Plot occurrence of highly-ranked features during feature selection.
#'
#'@description This function plots feature occurrence based on variable
#'  importance data obtained during feature selection, which are stored in a
#'  familiarCollection object.
#'
#'@param dir_path (*optional*) Path to the directory where created figures are
#'  saved to. Output is saved in the `variable_importance` subdirectory. If
#'  `NULL` no figures are saved, but are returned instead.
#'@param discrete_palette (*optional*) Palette to use for coloring bar plots, in
#'  case a non-singular variable was provided to the `color_by` argument.
#'@param gradient_palette (*optional*) Palette to use for filling the bars in
#'  case the `color_by` argument is not set. The bars are then coloured
#'  according to the occurrence of features. By default, no gradient is used,
#'  and the bars are not filled according to occurrence. Use `NULL` to fill the
#'  bars using the default palette in `familiar`.
#'@param height (*optional*) Height of the plot. A default value is derived from
#'  number of facets, and the length of the longest feature name (if
#'  `rotate_x_tick_labels` is `TRUE`).
#'@param width (*optional*) Width of the plot. A default value is derived from
#'  the number of facets and the number of features.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates a barplot based on occurrence of the most
#'  important features during feature selection.
#'
#'  The only allowed value for `split_by`, `color_by` or `facet_by` is
#'  `fs_method`.
#'
#'  Available palettes for `discrete_palette` and `gradient_palette` are those
#'  listed by `grDevices::palette.pals()` (requires R >= 4.0.0),
#'  `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'  `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'  palettes of the same name in `grDevices`. If not specified, a default
#'  palette based on palettes in Tableau are used. You may also specify your own
#'  palette by using colour names listed by `grDevices::colors()` or through
#'  hexadecimal RGB strings.
#'
#'  Labeling methods such as `set_feature_names` or `set_fs_method_names` can be
#'  applied to the `familiarCollection` object to update labels, and order the
#'  output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_feature_occurrence
#'@md
#'@rdname plot_feature_occurrence-methods
setGeneric("plot_feature_occurrence",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    show_cluster=TRUE,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=waiver(),
                    x_label="feature",
                    rotate_x_tick_labels=waiver(),
                    y_label=waiver(),
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_feature_occurrence"))

#####plot_feature_occurrence (generic)#####

#'@rdname plot_feature_occurrence-methods
setMethod("plot_feature_occurrence", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object. Note that this
            # will currently fail unless object is a familiarData object, a list
            # of familiarData objects or a familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="fs_vimp"), list(...)))
            
            return(do.call(plot_feature_occurrence,
                           args=append(list("object"=object,
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })


#####plot_feature_occurrence (collection)#####

#'@rdname plot_feature_occurrence-methods
setMethod("plot_feature_occurrence", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Obtain data
            x <- export_fs_vimp(object=object)

            return(do.call(.plot_feature_ranks,
                           args=append(list("object"=object,
                                            "x"=x,
                                            "plot_data"="occurrence",
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "available_splitting"="fs_method",
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })




#'@title Plot variable importance scores of features during feature selection.
#'
#'@description This function plots variable importance based data obtained
#'  during feature selection, which are stored in a familiarCollection object.
#'
#'@param gradient_palette (*optional*) Palette to use for filling the bars in
#'  case the `color_by` argument is not set. The bars are then coloured
#'  according to the aggregated importance of features. By default, no gradient
#'  is used, and the bars are not coloured according to importance. Use `NULL`
#'  to fill the bars using the default palette in `familiar`.
#'
#'@inheritParams plot_feature_occurrence
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates a barplot based on variable importance of
#'  features obtained during feature selection.
#'
#'  The only allowed value for `split_by`, `color_by` or `facet_by` is
#'  `fs_method`.
#'
#'  Available palettes for `discrete_palette` and `gradient_palette` are those
#'  listed by `grDevices::palette.pals()` (requires R >= 4.0.0),
#'  `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'  `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'  palettes of the same name in `grDevices`. If not specified, a default
#'  palette based on palettes in Tableau are used. You may also specify your own
#'  palette by using colour names listed by `grDevices::colors()` or through
#'  hexadecimal RGB strings.
#'
#'  Labeling methods such as `set_feature_names` or `set_fs_method_names` can be
#'  applied to the `familiarCollection` object to update labels, and order the
#'  output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_feature_ranks
#'@md
#'@rdname plot_feature_ranks-methods
setGeneric("plot_feature_ranks",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    show_cluster=TRUE,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=waiver(),
                    x_label="feature",
                    rotate_x_tick_labels=waiver(),
                    y_label=waiver(),
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_feature_ranks"))

#####plot_feature_ranks (generic)#####

#'@rdname plot_feature_ranks-methods
setMethod("plot_feature_ranks", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object. Note that this
            # will currently fail unless object is a familiarData object, a list
            # of familiarData objects or a familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="fs_vimp"), list(...)))
            
            return(do.call(plot_feature_ranks,
                           args=append(list("object"=object,
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })

#####plot_feature_ranks (collection)#####

#'@rdname plot_feature_ranks-methods
setMethod("plot_feature_ranks", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Obtain data
            x <- export_fs_vimp(object=object)
            
            return(do.call(.plot_feature_ranks,
                           args=append(list("object"=object,
                                            "x"=x,
                                            "plot_data"="ranking",
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "available_splitting"="fs_method",
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })



#'@title Plot variable importance scores of signature features obtained after
#'  modelling.
#'
#'@description This function plots variable importance based data obtained after
#'  training models. These data are stored in a familiarCollection object.
#'
#'@param gradient_palette (*optional*) Palette to use for filling the bars in
#'  case the `color_by` argument is not set. The bars are then coloured
#'  according to the aggregated importance of features. By default, no gradient
#'  is used, and the bars are not coloured according to importance. Use `NULL`
#'  to fill the bars using the default palette in `familiar`.
#'
#'@inheritParams plot_feature_occurrence
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates a barplot based on variable importance of
#'  features in model signatures. The variable importances are determined by the
#'  model algorithm, if it has any variable importance methods.
#'
#'  The only allowed values for `split_by`, `color_by` or `facet_by` are
#'  `fs_method` and `learner`.
#'  
#'  Available palettes for `discrete_palette` and `gradient_palette` are those
#'  listed by `grDevices::palette.pals()` (requires R >= 4.0.0),
#'  `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'  `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'  palettes of the same name in `grDevices`. If not specified, a default
#'  palette based on palettes in Tableau are used. You may also specify your own
#'  palette by using colour names listed by `grDevices::colors()` or through
#'  hexadecimal RGB strings.
#'  
#'  Labeling methods such as `set_feature_names` or `set_fs_method_names` can be
#'  applied to the `familiarCollection` object to update labels, and order the
#'  output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_model_signature_ranks
#'@md
#'@rdname plot_model_signature_ranks-methods
setGeneric("plot_model_signature_ranks",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    show_cluster=TRUE,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=waiver(),
                    x_label="feature",
                    rotate_x_tick_labels=waiver(),
                    y_label=waiver(),
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_model_signature_ranks"))


#####plot_model_signature_ranks (generic)#####

#'@rdname plot_model_signature_ranks-methods
setMethod("plot_model_signature_ranks", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="model_vimp"), list(...)))
            
            return(do.call(plot_model_signature_ranks,
                           args=append(list("object"=object,
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })

#####plot_model_signature_ranks (collection)#####

#'@rdname plot_model_signature_ranks-methods
setMethod("plot_model_signature_ranks", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label="feature",
                   rotate_x_tick_labels=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Obtain data
            x <- export_model_vimp(object=object)
            
            return(do.call(.plot_feature_ranks,
                           args=append(list("object"=object,
                                            "x"=x,
                                            "plot_data"="signature_ranking",
                                            "draw"=draw,
                                            "dir_path"=dir_path,
                                            "split_by"=split_by,
                                            "color_by"=color_by,
                                            "facet_by"=facet_by,
                                            "facet_wrap_cols"=facet_wrap_cols,
                                            "show_cluster"=show_cluster,
                                            "available_splitting"=c("fs_method", "learner"),
                                            "ggtheme"=ggtheme,
                                            "discrete_palette"=discrete_palette,
                                            "gradient_palette"=gradient_palette,
                                            "x_label"=x_label,
                                            "rotate_x_tick_labels"=rotate_x_tick_labels,
                                            "y_label"=y_label,
                                            "legend_label"=legend_label,
                                            "plot_title"=plot_title,
                                            "plot_sub_title"=plot_sub_title,
                                            "caption"=caption,
                                            "y_range"=y_range,
                                            "y_n_breaks"=y_n_breaks,
                                            "y_breaks"=y_breaks,
                                            "width"=width,
                                            "height"=height,
                                            "units"=units),
                                       list(...))))
          })


.plot_feature_ranks <- function(object,
                                x,
                                plot_data,
                                draw,
                                dir_path,
                                split_by,
                                color_by,
                                facet_by,
                                facet_wrap_cols,
                                show_cluster,
                                available_splitting,
                                ggtheme,
                                discrete_palette,
                                gradient_palette,
                                x_label,
                                rotate_x_tick_labels,
                                y_label,
                                legend_label,
                                plot_title,
                                plot_sub_title,
                                caption,
                                y_range,
                                y_n_breaks,
                                y_breaks,
                                width,
                                height,
                                units,
                                ...){  

  # Check input for plot_data argument.
  if(!plot_data %in% c("occurrence", "ranking", "signature_ranking")){
    ..error_reached_unreachable_code(".plot_feature_ranks_incorrect_plot_data_argument")
  }
  
  # Do not create plots if there are no data.
  if(is_empty(x)){
    warning(paste("Feature", plot_data, "could not be plotted as the required dataset is empty."))
    return(NULL)
  }
  
  ##### Check input arguments ##################################################
  
  # ggtheme
  if(!any(class(ggtheme) == "theme")) {
    ggtheme <- plotting.get_theme(use_theme=ggtheme)
    
  } else if(is.waive(rotate_x_tick_labels)){
    rotate_x_tick_labels <- FALSE
  }
  
  # y_label
  if(is.waive(y_label)){
    y_label <- ifelse(plot_data=="occurrence", "occurrence", "score")
  }
  
  # y_range
  if(is.null(y_range) & plot_data == "occurrence"){
    # for occurrence plots
    y_range <- c(0.0, 1.0)
    
  } else if(is.null(y_range)){
    # for variable importance score-based plots
    y_range <- c(0.0, max(x$score, na.rm=TRUE))
  }
  
  # Set y_breaks
  if(is.null(y_breaks)){
    plotting.check_input_args(y_range=y_range, y_n_breaks=y_n_breaks)
    
    # Create breaks and update x_range
    y_breaks <- labeling::extended(m=y_n_breaks,
                                   dmin=y_range[1],
                                   dmax=y_range[2],
                                   only.loose=TRUE)
    y_range  <- c(0, tail(y_breaks, n=1))
  }
  
  # rotate_x_tick_labels
  if(is.waive(rotate_x_tick_labels)) rotate_x_tick_labels <- TRUE
  
  # Add default parameters.
  if(is.null(split_by) & is.null(color_by) & is.null(facet_by)){
    split_by <- available_splitting
  }
  
  # Check splitting variables and generate sanitised output.
  split_var_list <- plotting.check_data_handling(x=x, split_by=split_by, color_by=color_by,
                                                 facet_by=facet_by, available=available_splitting)
  
  # Update splitting variables
  split_by <- split_var_list$split_by
  color_by <- split_var_list$color_by
  facet_by <- split_var_list$facet_by
  
  # legend_label
  legend_label <- plotting.create_legend_label(user_label=legend_label, color_by=color_by)
  
  # Perform last checks prior to plotting
  plotting.check_input_args(x_label=x_label,
                            y_label=y_label,
                            legend_label=legend_label,
                            plot_title=plot_title,
                            plot_sub_title=plot_sub_title,
                            caption=caption,
                            rotate_x_tick_labels=rotate_x_tick_labels,
                            facet_wrap_cols=facet_wrap_cols,
                            y_range=y_range,
                            y_breaks=y_breaks)
  
  # Split data
  if(!is.null(split_by)){
    x_split <- split(x, by=split_by)
  } else {
    x_split <- list(x)
  }
  
  # Store plots to list in case no dir_path is provided
  if(is.null(dir_path)){
    plot_list <- list()
  }
  
  # Iterate over splits
  for(x_sub in x_split){
    
    if(is_empty(x_sub)){
      next()
    }
    
    # Generate plot
    p <- .create_feature_rank_plot(x=x_sub,
                                   plot_data=plot_data,
                                   color_by=color_by,
                                   facet_by=facet_by,
                                   facet_wrap_cols=facet_wrap_cols,
                                   show_cluster=show_cluster,
                                   ggtheme=ggtheme,
                                   discrete_palette=discrete_palette,
                                   gradient_palette=gradient_palette,
                                   x_label=x_label,
                                   rotate_x_tick_labels=rotate_x_tick_labels,
                                   y_label=y_label,
                                   legend_label=legend_label,
                                   plot_title=plot_title,
                                   plot_sub_title=plot_sub_title,
                                   caption=caption,
                                   y_range=y_range,
                                   y_breaks=y_breaks)
    
    # Check empty output
    if(is.null(p)){ next() }
    
    # Draw plot
    if(draw){ plotting.draw(plot_or_grob=p) }
    
    # Save and export
    if(!is.null(dir_path)){
      subtype_basis <- ifelse(plot_data == "signature_ranking", "learner_", "feature_selection_")
      
      # Save to file
      if(!is.null(split_by)){
        subtype <- paste0(subtype_basis, plot_data, "_", paste0(sapply(split_by, function(ii, x) (x[[ii]][1]), x=x_sub), collapse="_"))
      } else {
        subtype <- paste0(subtype_basis, plot_data)
      }
      
      # Obtain decent default values for the plot.
      def_plot_dims <- .determine_feature_ranking_plot_dimensions(x=x_sub,
                                                                  facet_by=facet_by,
                                                                  facet_wrap_cols=facet_wrap_cols,
                                                                  rotate_x_tick_labels=rotate_x_tick_labels)
      
      # Save to file.
      do.call(plotting.save_plot_to_file,
              args=append(list("plot_obj"=p,
                               "object"=object,
                               "dir_path"=dir_path,
                               "type"="variable_importance",
                               "subtype"=subtype,
                               "height"=ifelse(is.waive(height), def_plot_dims[1], height),
                               "width"=ifelse(is.waive(width), def_plot_dims[2], width),
                               "units"=ifelse(is.waive(units), "cm", units)),
                          list(...)))
      
    } else {
      # Store as list and export
      plot_list <- append(plot_list, list(p))
    }
  }
  
  # Generate output
  if(is.null(dir_path)){
    return(plot_list)
  } else {
    return(NULL)
  }
}


.create_feature_rank_plot <- function(x, plot_data,
                                      color_by,
                                      facet_by,
                                      facet_wrap_cols,
                                      ggtheme,
                                      show_cluster,
                                      discrete_palette,
                                      gradient_palette,
                                      x_label,
                                      rotate_x_tick_labels,
                                      y_label,
                                      legend_label,
                                      plot_title,
                                      plot_sub_title,
                                      caption,
                                      y_range,
                                      y_breaks){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL
  
  # Create a local copy of x prior to making changes based on plot_data
  x <- data.table::copy(x)
  x$name <- droplevels(x$name)
  
  if(plot_data == "occurrence"){
    data.table::setnames(x, old="occurrence", new="value")
    x <- x[order(-value)]
    
  } else {
    data.table::setnames(x, old="score", new="value")
    x <- x[order(rank)]
  }
  
  # Update the ordering of features so that the features are ordered by
  # increasing score or occurrence
  x$name <- factor(x$name, levels=unique(x$name))
  
  # Generate a guide table
  guide_list <- plotting.create_guide_table(x=x, color_by=color_by, discrete_palette=discrete_palette)

  # Extract data
  x <- guide_list$data
  
  # Check if cluster information should be shown:
  if(show_cluster){
    x <- plotting.add_cluster_name(x=x, color_by=color_by, facet_by=facet_by)
  }
  
  # Perform last checks prior to plotting
  plotting.check_input_args(y_range=y_range,
                            y_breaks=y_breaks)

  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("name"), y=!!sym("value")))
  p <- p + ggtheme
  
  # Add fill colors
  if(!is.null(color_by)){
    
    # Extract guide_table for color
    g_color <- guide_list$guide_color
    
    p <- p + ggplot2::geom_bar(stat="identity", mapping=ggplot2::aes(fill=!!sym("color_breaks")), position="dodge")
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color, values=g_color$color_values, breaks=g_color$color_breaks, drop=FALSE)
    
  } else if(!is.waive(gradient_palette)){
    # A gradient palette is used to colour the bars by value.
    p <- p + ggplot2::geom_bar(stat="identity",
                               mapping=ggplot2::aes(fill=!!sym("value")),
                               show.legend=FALSE)
    
    # Determine gradient order. This is so that bars of more important features
    # are always colored with the high-range colors, independent of the
    # orientation of the score. The correct gradient order will be set using the
    # trans argument, which defines the transformations.
    gradient_order <- "identity"
      
    if(plot_data %in% c("ranking", "signature_ranking")) {
      # Determine best and worst scores.
      best_score <- x[rank == 1]$value[1]
      worst_score <- x[rank == max(x$rank)]$value[1]
      
      # Invert gradient if the worst score is higher than the best score.
      if(best_score < worst_score){
        gradient_order <- "reverse"
      }
    }
    
    # Get gradient colours
    gradient_colours <- plotting.get_palette(x=gradient_palette, palette_type="sequential")
    
    p <- p + ggplot2::scale_fill_gradientn(colors=gradient_colours,
                                           limits=y_range,
                                           trans=gradient_order)
    
  } else {
    # Bars are not coloured based on occurrence/importance.
    p <- p + ggplot2::geom_bar(stat="identity")
  }

  # Set breaks and  limits on the y-axis
  p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
  
  # Determine how things are facetted
  facet_by_list <- plotting.parse_facet_by(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  if(!is.null(facet_by)){
    if(is.null(facet_wrap_cols)){
      # Use a grid
      p <- p + ggplot2::facet_grid(rows=facet_by_list$facet_rows, cols=facet_by_list$facet_cols, labeller="label_context")
    } else {
      p <- p + ggplot2::facet_wrap(facets=facet_by_list$facet_by, labeller="label_context")
    }
  }
  
  # Add clustering information.
  if(show_cluster){
    
    # Obtain default settings.
    text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
    
    if(is.null(color_by)){
      p <- p + ggplot2::geom_text(ggplot2::aes(label=!!sym("cluster_name"), y=0.0),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size,
                                  vjust="inward")
      
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label=!!sym("cluster_name"), y=0.0, group=!!sym("color_breaks")),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size,
                                  vjust="inward",
                                  position=ggplot2::position_dodge(width=0.9))
    }
  } 
  
  # Update labels.
  p <- p + ggplot2::labs(x=x_label, y=y_label, title=plot_title, subtitle=plot_sub_title, caption=caption)

  # Rotate x-axis ticks
  if(rotate_x_tick_labels){
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
  }

  return(p)
}



.determine_feature_ranking_plot_dimensions <- function(x, facet_by, facet_wrap_cols, rotate_x_tick_labels){
  
  # Get plot layout dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Determine the number of features within each facet.
  n_features <- data.table::uniqueN(x=x$name)
  longest_name <- max(sapply(levels(x$name), nchar))
  
  # Assume each feature takes up about 14 points (~5mm) with 2 point (0.07mm)
  # spacing. Then add some room for other plot elements.
  default_width <- n_features * 0.5 + (n_features - 1) * 0.07 + 1.0
  default_width <- max(c(4, default_width))
  
  # Set default height.
  default_height <- 4
  
  # Reserve space for x-axis tick labels. Assume that the typical width of a
  # character is about 5 points (1.8 mm). For the x-axis we only reserve extra
  # space in case the ticks are rotated, otherwise we just assume a typical
  # height of 10 points (3.6 mm).
  x_tick_space <- ifelse(rotate_x_tick_labels, longest_name * 0.18, 0.36)
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height + x_tick_space, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}
