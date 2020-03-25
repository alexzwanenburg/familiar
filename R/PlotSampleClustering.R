#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot heatmaps for pairwise similarity between features.
#'
#'@description This method creates a heatmap based on data stored in a
#'  `familiarCollection` object. Features in the heatmap are ordered so that
#'  more similar features appear together.
#'
#'@param dir_path (*optional*) Path to the directory where created performance
#'  plots are saved to. Output is saved in the `feature_similarity`
#'  subdirectory. If `NULL` no figures are saved, but are returned instead.
#'@param gradient_palette (*optional*) Sequential or divergent palette used to
#'  colour the similarity or distance between features in a heatmap.
#'@param gradient_palette_range (*optional*) Numerical range used to span the
#'  gradient. This should be a range of two values, e.g. `c(0, 1)`. Lower or
#'  upper boundary can be unset by using `NA`. If not set, the full
#'  metric-specific range is used.
#'@param show_feature_dendrogram (*optional*) Show feature dendrogram around the
#'  main panel. Can be `TRUE`, `FALSE`, `NULL`, or a position, i.e. `top`,
#'  `bottom`, `left` and `right`.
#'
#'  If a position is specified, it should be appropriate with regard to the
#'  `x_axis_by` or `y_axis_by` argument. If `x_axis_by` is `sample` (default),
#'  the only valid positions are `top` (default) and `bottom`. Alternatively, if
#'  `y_axis_by` is `feature`, the only valid positions are `right` (default) and
#'  `left`.
#'
#'  A dendrogram can only be drawn from cluster methods that produce dendograms,
#'  such as `hclust`. A dendogram can for example not be constructed using the
#'  partioning around medioids method (`pam`).
#'
#'@param show_sample_dendrogram (*optional*) Show sample dendrogram around the
#'  main panel. Can be `TRUE`, `FALSE`, `NULL`, or a position, i.e. `top`,
#'  `bottom`, `left` and `right`.
#'
#'  If a position is specified, it should be appropriate with regard to the
#'  `x_axis_by` or `y_axis_by` argument. If `y_axis_by` is `sample` (default),
#'  the only valid positions are `right` (default) and `left`. Alternatively, if
#'  `x_axis_by` is `sample`, the only valid positions are `top` (default) and
#'  `bottom`.
#'
#'  A dendrogram can only be drawn from cluster methods that produce dendograms,
#'  such as `hclust`. A dendogram can for example not be constructed using the
#'  partioning around medioids method (`pam`).
#'@param show_outcome (*optional*) Show outcome column(s) or row(s) in the
#'  graph. Can be `TRUE`, `FALSE`, `NULL` or a poistion, i.e. `top`, `bottom`,
#'  `left` and `right`.
#'
#'  If a position is specified, it should be appropriate with regard to the
#'  `x_axis_by` or `y_axis_by` argument. If `y_axis_by` is `sample` (default),
#'  the only valid positions are `left` (default) and `right`. Alternatively, if
#'  `x_axis_by` is `sample`, the only valid positions are `top` (default) and
#'  `bottom`.
#'
#'  The outcome data will be drawn between the main panel and the sample
#'  dendrogram (if any).
#'
#'@param dendrogram_height (*optional*) Height of the dendrogram. The height is
#'  1.5 cm by default. Height is expected to be grid unit (see `grid::unit`),
#'  which also allows for specifying relative heights.
#'@param outcome_height (*optional*) Height of the outcome data column/row. The
#'  height is 0.5 cm by default. Height is expected to be a grid unit (see
#'  `grid::unit`), which also allows for specifying relative heights.
#'@param evaluation_times (*optional*) Times at which the event status of
#'  time-to-event survival outcomes are determined. Only used for `survival`
#'  outcome. If not specified, the values used when creating the underlying
#'  `familiarData` objects are used.
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates area under the ROC curve plots.
#'
#'  Available splitting variables are: `fs_method`, `learner`, and `data_set`.
#'  By default, the data is split by `fs_method` and `learner` and `data_set`,
#'  since the number of samples will typically differ between data sets, even
#'  for the same feature selection method and learner.
#'
#'  The `x_axis_by` and `y_axis_by` arguments determine what data are shown
#'  along which axis. Each argument takes one of `feature` and `sample`, and
#'  both arguments should be unique. By default, features are shown along the
#'  x-axis and samples along the y-axis.
#'
#'  Note that similarity is determined based on the underlying data. Hence the
#'  ordering of features may differ between facets, and tick labels are
#'  maintained for each panel.
#'
#'  Available palettes for `gradient_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'  Labeling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_sample_clustering
#'@md
#'@rdname plot_sample_clustering-methods
setGeneric("plot_sample_clustering",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    x_axis_by=NULL,
                    y_axis_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    ggtheme=NULL,
                    gradient_palette=NULL,
                    gradient_palette_range=waiver(),
                    x_label=waiver(),
                    x_label_shared="column",
                    y_label=waiver(),
                    y_label_shared="row",
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=3,
                    x_breaks=NULL,
                    y_range=NULL,
                    y_n_breaks=3,
                    y_breaks=NULL,
                    rotate_x_tick_labels=waiver(),
                    show_feature_dendrogram=TRUE,
                    show_sample_dendrogram=TRUE,
                    show_normalized_data=TRUE,
                    show_outcome=TRUE,
                    dendrogram_height=grid::unit(1.5, "cm"),
                    outcome_height=grid::unit(0.5, "cm"),
                    evaluation_times=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_sample_clustering"))

#####plot_sample_clustering (generic)#####

#'@rdname plot_sample_clustering-methods
setMethod("plot_sample_clustering", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   x_axis_by=NULL,
                   y_axis_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=waiver(),
                   x_label=waiver(),
                   x_label_shared="column",
                   y_label=waiver(),
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=3,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=3,
                   y_breaks=NULL,
                   rotate_x_tick_labels=waiver(),
                   show_feature_dendrogram=TRUE,
                   show_sample_dendrogram=TRUE,
                   show_normalized_data=TRUE,
                   show_outcome=TRUE,
                   dendrogram_height=grid::unit(1.5, "cm"),
                   outcome_height=grid::unit(0.5, "cm"),
                   evaluation_times=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="feature_expressions"), list(...)))
            
            return(do.call(plot_sample_clustering,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "split_by"=split_by,
                                     "x_axis_by"=x_axis_by,
                                     "y_axis_by"=y_axis_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "ggtheme"=ggtheme,
                                     "gradient_palette"=gradient_palette,
                                     "gradient_palette_range"=gradient_palette_range,
                                     "x_label"=x_label,
                                     "x_label_shared"=x_label_shared,
                                     "y_label"=y_label,
                                     "y_label_shared"=y_label_shared,
                                     "legend_label"=legend_label,
                                     "plot_title"=plot_title,
                                     "plot_sub_title"=plot_sub_title,
                                     "caption"=caption,
                                     "x_range"=x_range,
                                     "x_n_breaks"=x_n_breaks,
                                     "x_breaks"=x_breaks,
                                     "y_range"=y_range,
                                     "y_n_breaks"=y_n_breaks,
                                     "y_breaks"=y_breaks,
                                     "rotate_x_tick_labels"=rotate_x_tick_labels,
                                     "show_feature_dendrogram"=show_feature_dendrogram,
                                     "show_sample_dendrogram"=show_sample_dendrogram,
                                     "show_normalized_data"=show_normalized_data,
                                     "show_outcome"=show_outcome,
                                     "dendrogram_height"=dendrogram_height,
                                     "outcome_height"=outcome_height,
                                     "evaluation_times"=evaluation_times,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })


#####plot_sample_clustering (collection)#####

#'@rdname plot_sample_clustering-methods
setMethod("plot_sample_clustering", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   x_axis_by=NULL,
                   y_axis_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=waiver(),
                   x_label=waiver(),
                   x_label_shared="column",
                   y_label=waiver(),
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=3,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=3,
                   y_breaks=NULL,
                   rotate_x_tick_labels=waiver(),
                   show_feature_dendrogram=TRUE,
                   show_sample_dendrogram=TRUE,
                   show_normalized_data=TRUE,
                   show_outcome=TRUE,
                   dendrogram_height=grid::unit(1.5, "cm"),
                   outcome_height=grid::unit(0.5, "cm"),
                   evaluation_times=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Get input data
            x <- export_feature_expressions(object=object)
            
            # Skip empty data.
            if(is.null(x)) return(NULL)
            if(length(x) == 0) return(NULL)
            
            # Give x names
            names(x) <- as.character(seq_along(x))
            
            # Collect data sets, learners and feature selection methods, so that
            # splitting can be organised.
            y <- lapply(seq_along(x), function(ii, x){
              
              # Check for empty entries
              if(is_empty(x[[ii]]$data)) return(NULL)
              
              # Extract data_set, fs_method and learner columns.
              y <- head(x[[ii]]$data[, c("data_set", "fs_method", "learner")], n=1)
              
              # Insert list_id ii
              y[, "list_id":=as.character(ii)]
              
              return(y)
            }, x=x)
            
            # Concatenate to a single data table.
            y <- data.table::rbindlist(y)
            
            # Update labelling
            y$data_set <- factor(y$data_set, levels=unique(get_data_set_names(object)))
            y$fs_method <- factor(y$fs_method, levels=unique(get_fs_method_names(object)))
            y$learner <- factor(y$learner, levels=unique(get_learner_names(object)))
            
            ##### Check input arguments ----------------------------------------
            
            # ggtheme
            if(!any(class(ggtheme) == "theme")) {
              ggtheme <- plotting.get_theme(use_theme=ggtheme)
              
            } else if(is.waive(rotate_x_tick_labels)){
              rotate_x_tick_labels <- FALSE
            }
            
            # x_label_shared
            if(!is.waive(x_label_shared)){
              plotting.check_input_args(x_label_shared=x_label_shared)
            } else {
              x_label_shared <- "column"
            }
            
            # y_label_shared
            if(!is.waive(y_label_shared)){
              plotting.check_input_args(y_label_shared=y_label_shared)
            } else {
              y_label_shared <- "row"
            }
            
            # x_label
            if(is.waive(x_label)){
              x_label <- NULL
            }
            
            # y_label
            if(is.waive(y_label)){
              y_label <- NULL
            }
            
            # legend_label
            if(is.waive(legend_label)) legend_label <- NULL
            
            # rotate_x_tick_labels
            if(is.waive(rotate_x_tick_labels)) rotate_x_tick_labels <- TRUE
            
            # show_normalized_data
            if(is.logical(show_normalized_data)){
              if(show_normalized_data) show_normalized_data <- "fixed"
              else show_normalized_data <- "none"
              
            } else {
              .check_parameter_value_is_valid(x=show_normalized_data, var_name="show_normalized_data",
                                              values=c("none", "fixed", "set_normalisation"))
            }
            
            # x_axis_by & y_axis_by
            available_axis_variables <- c("feature", "sample")
            if(is.null(x_axis_by) & is.null(y_axis_by)){
              
              # Set both variables.
              x_axis_by <- available_axis_variables[1]
              y_axis_by <- available_axis_variables[2]
              
            } else if(is.null(x_axis_by)) {
              
              .check_parameter_value_is_valid(x=y_axis_by, var_name="y_axis_by", values=available_axis_variables)
              x_axis_by <- setdiff(available_axis_variables, y_axis_by)
              
            } else if(is.null(y_axis_by)) {
              
              .check_parameter_value_is_valid(x=x_axis_by, var_name="x_axis_by", values=available_axis_variables)
              y_axis_by <- setdiff(available_axis_variables, x_axis_by)
              
            } else {
              .check_parameter_value_is_valid(x=x_axis_by, var_name="x_axis_by", values=available_axis_variables)
              .check_parameter_value_is_valid(x=y_axis_by, var_name="y_axis_by", values=available_axis_variables)
            }
            
            .check_value_not_shared(x_axis_by, y_axis_by, "x_axis_by", "y_axis_by")
            
            # Check length of x_axis_by and y_axis_by variables.
            .check_argument_length(x_axis_by, "x_axis_by", min=1, max=1)
            .check_argument_length(y_axis_by, "y_axis_by", min=1, max=1)
            
            # show_feature_dendrogram
            if(is.logical(show_feature_dendrogram)){
              
              if(show_feature_dendrogram) show_feature_dendrogram <- ifelse(x_axis_by == "feature", "top", "right")
              else show_feature_dendrogram <- NULL
              
            } else if(length(show_feature_dendrogram) == 0){
              
              show_feature_dendrogram <- NULL
              
            } else {
              
              if(x_axis_by == "feature"){
                .check_parameter_value_is_valid(x=show_feature_dendrogram, var_name="show_feature_dendrogram",
                                                values=c("top", "bottom"))
              } else {
                .check_parameter_value_is_valid(x=show_feature_dendrogram, var_name="show_feature_dendrogram",
                                                values=c("left", "right"))
              }

              .check_argument_length(show_feature_dendrogram, "show_feature_dendrogram", min=1, max=1)
            }
            
            # Check if cluster objects are present and have the correct hclust
            # class.
            if(!is.null(show_feature_dendrogram)){
              can_draw_feature_dendrogram <- sapply(x, function(list_entry) (inherits(list_entry$feature_cluster_object, "hclust")))
              
              if(all(!can_draw_feature_dendrogram)){
                warning(paste0("Cannot draw the feature dendrogram as the cluster objects are not of the \"hclust\" class. ",
                               "This may occur if partitioning around medioids is used for clustering."))
                
                show_feature_dendrogram <- NULL
              }
            }
            
            
            # show_sample_dendrogram
            if(is.logical(show_sample_dendrogram)){
              
              if(show_sample_dendrogram) show_sample_dendrogram <- ifelse(x_axis_by == "sample", "top", "right")
              else show_sample_dendrogram <- NULL
              
            } else if(length(show_sample_dendrogram) == 0){
              
              show_sample_dendrogram <- NULL
              
            } else {
              
              if(x_axis_by == "sample"){
                .check_parameter_value_is_valid(x=show_sample_dendrogram, var_name="show_sample_dendrogram",
                                                values=c("top", "bottom"))
              } else {
                .check_parameter_value_is_valid(x=show_sample_dendrogram, var_name="show_sample_dendrogram",
                                                values=c("left", "right"))
              }
              
              .check_argument_length(show_sample_dendrogram, "show_sample_dendrogram", min=1, max=1)
            }
            
            # Check if cluster objects are present and have the correct hclust
            # class.
            if(!is.null(show_sample_dendrogram)){
              can_draw_sample_dendrogram <- sapply(x, function(list_entry) (inherits(list_entry$feature_cluster_object, "hclust")))
              
              if(all(!can_draw_sample_dendrogram)){
                warning(paste0("Cannot draw the sample dendrogram as the cluster objects are not of the \"hclust\" class. ",
                               "This may occur if partitioning around medioids is used for clustering."))
                
                can_draw_feature_dendrogram <- NULL
              }
            }
            
            
            # Check if the dendrogram_height argument is correct.
            if(!is.null(show_sample_dendrogram) | !is.null(show_feature_dendrogram)){
              plotting.check_grid_unit(x=dendrogram_height, var_name="dendrogram_height")
            }
            
            
            # Add default splitting variables
            if(is.null(split_by) & is.null(facet_by)){
              
              # Split by feature selection method and learner
              split_by <- c("fs_method", "learner","data_set")
              
              # Facet by dataset
              facet_by <- NULL
            }
            
            # Check splitting variables and generate sanitised output
            split_var_list <- plotting.check_data_handling(x=y,
                                                           split_by=split_by,
                                                           facet_by=facet_by,
                                                           available=c("data_set", "fs_method", "learner"))
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            facet_by <- split_var_list$facet_by
            
            # Check input arguments
            plotting.check_input_args(facet_wrap_cols=facet_wrap_cols,
                                      x_label=x_label,
                                      y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption,
                                      rotate_x_tick_labels=rotate_x_tick_labels)
            
            ##### Create plots -------------------------------------------------
            
            # Split data.
            if(!is.null(split_by)){
              y_split <- split(y, by=split_by, drop=FALSE)
              
            } else {
              y_split <- list("null.name"=y)
            }
            
            # Store plots to list in case dir_path is absent.
            if(is.null(dir_path)){
              plot_list <- list()
            }
            
            # Iterate over data splits.
            for(ii in names(y_split)){
              
              # Skip empty datasets.
              if(is_empty(y_split[[ii]])) next()
              
              # Skip sets with empty similarity data.
              data_are_empty <- sapply(x[y_split[[ii]]$list_id], function(list_entry) (is_empty(list_entry$data)))
              if(all(data_are_empty)) next()
              
              # Generate plot
              p <- .plot_sample_clustering_plot(x=y_split[[ii]],
                                                data=x,
                                                x_axis_by=x_axis_by,
                                                y_axis_by=y_axis_by,
                                                facet_by=facet_by,
                                                facet_wrap_cols=facet_wrap_cols,
                                                ggtheme=ggtheme,
                                                gradient_palette=gradient_palette,
                                                gradient_palette_range=gradient_palette_range,
                                                x_label=x_label,
                                                x_label_shared=x_label_shared,
                                                y_label=y_label,
                                                y_label_shared=y_label_shared,
                                                legend_label=legend_label,
                                                plot_title=plot_title,
                                                plot_sub_title=plot_sub_title,
                                                caption=caption,
                                                x_range=x_range,
                                                x_n_breaks=x_n_breaks,
                                                x_breaks=x_breaks,
                                                y_range=y_range,
                                                y_n_breaks=y_n_breaks,
                                                y_breaks=y_breaks,
                                                rotate_x_tick_labels=rotate_x_tick_labels,
                                                show_feature_dendrogram=show_feature_dendrogram,
                                                show_sample_dendrogram=show_sample_dendrogram,
                                                show_normalized_data=show_normalized_data,
                                                dendrogram_height=dendrogram_height)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw figure.
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                
                # Add plot type as a subtype.
                subtype <- character(0)
                
                # Determine the subtype
                if(!is.null(split_by)){
                  subtype <- c(subtype, as.character(sapply(split_by, function(jj, y) (y[[jj]][1]), y=y_split[[ii]])))
                  subtype <- paste0(subtype, collapse="_")
                }
                
                # Find features
                features <- lapply(x[y_split[[ii]]$list_id], function(list_entry){
                  if(is_empty(list_entry$data)) return(NULL)
                  
                  return(list_entry$feature_order$name)
                })
                
                # Identify unique features:
                features <- unique(unlist(features))
                
                # Find samples
                samples <- lapply(x[y_split[[ii]]$list_id], function(list_entry){
                  if(is_empty(list_entry$data)) return(NULL)
                  
                  return(list_entry$sample_order$name)
                })
                
                # Identify unique samples.
                samples <- unique(unlist(samples))
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_sample_clustering_plot_dimensions(x=y_split[[ii]],
                                                                              x_axis_by=x_axis_by,
                                                                              y_axis_by=y_axis_by,
                                                                              facet_by=facet_by,
                                                                              facet_wrap_cols=facet_wrap_cols,
                                                                              features=features,
                                                                              samples=samples,
                                                                              show_feature_dendrogram=show_feature_dendrogram,
                                                                              show_sample_dendrogram=show_sample_dendrogram,
                                                                              rotate_x_tick_labels=rotate_x_tick_labels)
                
                # Save to file.
                do.call(plotting.save_plot_to_file,
                        args=append(list("plot_obj"=p,
                                         "object"=object,
                                         "dir_path"=dir_path,
                                         "type"="feature_expression",
                                         "subtype"=subtype,
                                         "height"=ifelse(is.waive(height), def_plot_dims[1], height),
                                         "width"=ifelse(is.waive(width), def_plot_dims[2], width),
                                         "units"=ifelse(is.waive(units), "cm", units)),
                                    list(...)))
                
              } else {
                # Store as list for export.
                plot_list <- append(plot_list, list(p))
              }
            }
            
            # Output
            if(is.null(dir_path)){
              return(plot_list)
              
            } else {
              return(NULL)
            }
          })



.plot_sample_clustering_plot <- function(x,
                                         data,
                                         x_axis_by,
                                         y_axis_by,
                                         facet_by,
                                         facet_wrap_cols,
                                         ggtheme,
                                         gradient_palette,
                                         gradient_palette_range,
                                         x_label,
                                         x_label_shared,
                                         y_label,
                                         y_label_shared,
                                         legend_label,
                                         plot_title,
                                         plot_sub_title,
                                         caption,
                                         x_range,
                                         x_n_breaks,
                                         x_breaks,
                                         y_range,
                                         y_n_breaks,
                                         y_breaks,
                                         rotate_x_tick_labels,
                                         show_feature_dendrogram,
                                         show_sample_dendrogram,
                                         show_normalized_data,
                                         dendrogram_height){
  
  # Define elements that need to be shared. Note that "guide" and "strip_y" may
  # be absent.
  elements <- c("guide", "strip_x", "strip_y")
  if(x_label_shared == "overall") { elements <- append(elements, "axis_title_x")}
  if(y_label_shared == "overall") { elements <- append(elements, "axis_title_y")}
  
  # Determine the list identifiers for the current selection of data.
  list_id <- as.character(unique(x$list_id))
  
  # Determine the range of the gradient palette.
  if(is.waive(gradient_palette_range)){
    if(show_normalized_data == "none"){
      # Default to an empty range.
      gradient_palette_range <- c(NA, NA)
      
    } else if(show_normalized_data == "fixed"){
      # Identify the normalisation method used to convert the data.
      normalisation_method <- lapply(list_id, function(ii, data){
        
        # Obtain the normalisation methods for each feature in the current
        # dataset.
        normalisation_method <- sapply(data[[ii]]$feature_info, function(feature){
          return(feature@normalisation_parameters$norm_method)
        })
        
        return(unname(normalisation_method))
        
      }, data=data)
      
      # Select the normalisation methods that we should consider.
      normalisation_method <- unique(unlist(normalisation_method))
      if(all(normalisation_method == "none")){
        normalisation_method <- "none"
      } else {
        normalisation_method <- setdiff(normalisation_method, "none")
      }
      
      # Find the default value.
      gradient_palette_range <- .get_default_normalisation_range_for_plotting(norm_method=normalisation_method)
      
      
    } else if(show_normalized_data == "set_normalisation"){
      # By default show range -3 to 3 standard deviations
      gradient_palette_range <- .get_default_normalisation_range_for_plotting(norm_method="standardisation_winsor")
      
    } else {
      ..error_reached_unreachable_code(paste0(".plot_sample_clustering_plot: encountered unknown value for show_normalized_data: ",
                                              show_normalized_data))
    }
    
  } else if(is.null(gradient_palette_range)){
    # Default to an empty range.
    gradient_palette_range <- c(NA, NA)
  }
  
  # Normalise expression data
  expression_data <- lapply(list_id, function(ii, data, show_normalized_data){
    
    # Normalise expression data
    expression_data <- .normalise_expression_data(x=data[[ii]]$data,
                                                  show_normalized_data=show_normalized_data,
                                                  feature_info=data[[ii]]$feature_info)
    
    return(expression_data)
    
  }, data=data, show_normalized_data=show_normalized_data)
  
  # Name the expression data sets by the list identifiers so that they can be
  # recognised and addressed by name.
  names(expression_data) <- list_id
  
  # Ensure that a gradient palette range is set. This is required because the
  # legend is shared between all facets and .
  if(any(!is.finite(gradient_palette_range))){
    
    # Iterate over expression data to find minimum and maximum
    feature_ranges <- lapply(list_id, function(ii, expression_data, data){
      
      if(is_empty(expression_data[[ii]])){
        return(data.table::data.table("min_value"=numeric(0),
                                      "max_value"=numeric(0))) 
      }
      
      # Find feature value ranges in the current expression data.
      feature_ranges <- lapply(data[[ii]]$feature_info, function(feature, expression_data){
        
        # Only numeric features have a range.
        if(feature@feature_type == "numeric"){
          
          # Find feature values that are finite.
          feature_data <- expression_data[[feature@name]]
          feature_data <- feature_data[is.finite(feature_data)]
          
          if(length(feature_data) == 0){
            return(data.table::data.table("min_value"=numeric(0),
                                          "max_value"=numeric(0)))
          } else {
            return(data.table::data.table("min_value"=as.double(min(feature_data, na.rm=FALSE)),
                                          "max_value"=as.double(max(feature_data, na.rm=FALSE))))
          }
          
        } else {
          return(data.table::data.table("min_value"=numeric(0),
                                        "max_value"=numeric(0)))
        }
      }, expression_data=expression_data[[ii]])
      
      # Combine ranges for all features.
      feature_ranges <- data.table::rbindlist(feature_ranges)
      
      return(feature_ranges)
      
    }, data=data, expression_data=expression_data)
    
    # Combine ranges
    feature_ranges <- data.table::rbindlist(feature_ranges)
    
    if(is_empty(feature_ranges)){
      # Set a default if all features are categorical.
      gradient_palette_range <- c(-1.0, 0.0, 1.0)
      
    } else {
      # Replace the first value of the range, if it is not finite (NA).
      if(!is.finite(gradient_palette_range[1])){
        gradient_palette_range[1] <- min(feature_ranges$min_value)
      }
      
      # Replace the last value of the range, if it is not finite (NA).
      if(!is.finite(tail(gradient_palette_range, n=1))){
        gradient_palette_range[length(gradient_palette_range)] <- max(feature_ranges$max_value)
      }
      
      # Shrink the gradient palette range to two values.
      gradient_palette_range <- c(gradient_palette_range[1],
                                  gradient_palette_range[length(gradient_palette_range)])
    }
  }
  
  # Split by facet. This generates a list of data splits with facetting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols,
                                                      x_label_shared=x_label_shared, y_label_shared=y_label_shared)
  
  # Split data into facets. This is done by row.
  x_split_list <- plotting.split_data_by_facet(x=x, plot_layout_table=plot_layout_table)
  
  # Create plots to join
  figure_list <- list()
  extracted_element_list <- list()
  
  for(ii in seq_along(x_split_list)){
    
    x_split <- x_split_list[[ii]]
    
    # Find the cluster object for features
    feature_cluster_object <- data[[x_split$list_id]]$feature_cluster_object
    
    # Find the cluster object for samples
    sample_cluster_object <- data[[x_split$list_id]]$sample_cluster_object
    
    # Find the feature similarity metric
    feature_similarity_metric <- data[[x_split$list_id]]$feature_similarity_metric
    
    # Find the sample similarity metric
    sample_similarity_metric <- data[[x_split$list_id]]$sample_similarity_metric

    # Complete the expression data
    plot_data <- .complete_expression_table(x=expression_data[[x_split$list_id]],
                                            feature_info=data[[x_split$list_id]]$feature_info,
                                            feature_order=data[[x_split$list_id]]$feature_order,
                                            sample_order=data[[x_split$list_id]]$sample_order,
                                            gradient_palette_range=gradient_palette_range)
    
    # Create expression heatmap
    p_heatmap <- .create_expression_heatmap(x=plot_data,
                                            x_axis_by=x_axis_by,
                                            y_axis_by=y_axis_by,
                                            facet_by=facet_by,
                                            facet_wrap_cols=facet_wrap_cols,
                                            ggtheme=ggtheme,
                                            gradient_palette=gradient_palette,
                                            gradient_palette_range=gradient_palette_range,
                                            x_label=x_label,
                                            y_label=y_label,
                                            legend_label=legend_label,
                                            plot_title=plot_title,
                                            plot_sub_title=plot_sub_title,
                                            caption=caption,
                                            rotate_x_tick_labels=rotate_x_tick_labels,
                                            show_feature_dendrogram=show_feature_dendrogram,
                                            show_sample_dendrogram=show_sample_dendrogram)
    
    # Update theme to remove guide, facet labels, etc., based on notations in
    # dataset x. The axis tick labels are kept, because we cannot guarantee that
    # all heatmaps will have the same feature, samples and respective ordering.
    p_heatmap <- plotting.update_facet_plot_elements(p=p_heatmap,
                                                     x=x_split,
                                                     keep_axis_labels_x=TRUE,
                                                     keep_axis_labels_y=TRUE)
    
    # Extract plot elements from the heatmap.
    extracted_elements <- plotting.extract_plot_elements(p=p_heatmap, elements=elements)
    
    # Remove extracted elements from the heatmap.
    p_heatmap <- plotting.remove_plot_elements(p=p_heatmap, elements=elements)
    
    # Convert to grob.
    g_heatmap <- plotting.to_grob(p_heatmap)
    
    # Add sample dendogram
    if(!is.null(show_sample_dendrogram)){
      
      # Obtain dendogram plotting data as line segments.
      dendro_data <- plotting.dendrogram_as_table(h=sample_cluster_object,
                                                  similarity_metric=sample_similarity_metric)
      
      # Find the right axes settings.
      if(show_sample_dendrogram %in% c("left", "right")){
        dist_range <- x_range
        dist_n_breaks <- x_n_breaks
        dist_breaks <- x_breaks
      } else {
        dist_range <- y_range
        dist_n_breaks <- y_n_breaks
        dist_breaks <- y_breaks
      }
      
      # Plot dendogram
      p_dendro <- .create_expression_dendrogram_plot(x=dendro_data,
                                                    position=show_sample_dendrogram,
                                                    ggtheme=ggtheme,
                                                    dist_range=dist_range,
                                                    dist_n_breaks=dist_n_breaks,
                                                    dist_breaks=dist_breaks,
                                                    plot_height=dendrogram_height,
                                                    rotate_x_tick_labels=rotate_x_tick_labels)
      
      # Determine the axis element
      axis_element <- ifelse(show_sample_dendrogram %in% c("top", "bottom"), "axis-l", "axis-b")
      
      # Extract dendodram gtable, which consists of the panel and the height
      # axis.
      g_sample_dendro <- .gtable_extract(g=plotting.to_grob(p_dendro),
                                         element=c("panel", axis_element),
                                         partial_match=TRUE)
      
      # Insert the dendrogram at the position correct position around the
      # heatmap.
      g_heatmap <- .gtable_insert(g=g_heatmap,
                                  g_new=g_sample_dendro,
                                  where=show_sample_dendrogram,
                                  ref_element="panel",
                                  partial_match=TRUE)
    }
    
    # Add feature dendogram
    if(!is.null(show_feature_dendrogram)){
      # Obtain dendogram plotting data as line segments.
      dendro_data <- plotting.dendrogram_as_table(h=feature_cluster_object,
                                                  similarity_metric=feature_similarity_metric)
      
      # Find the right axes settings.
      if(show_feature_dendrogram %in% c("left", "right")){
        dist_range <- x_range
        dist_n_breaks <- x_n_breaks
        dist_breaks <- x_breaks
      } else {
        dist_range <- y_range
        dist_n_breaks <- y_n_breaks
        dist_breaks <- y_breaks
      }
      
      # Plot dendogram
      p_dendro <- .create_expression_dendrogram_plot(x=dendro_data,
                                                    position=show_feature_dendrogram,
                                                    ggtheme=ggtheme,
                                                    dist_range=dist_range,
                                                    dist_n_breaks=dist_n_breaks,
                                                    dist_breaks=dist_breaks,
                                                    plot_height=dendrogram_height,
                                                    rotate_x_tick_labels=rotate_x_tick_labels)
      
      # Determine the axis element
      axis_element <- ifelse(show_feature_dendrogram %in% c("top", "bottom"), "axis-l", "axis-b")
      
      # Extract dendodram gtable, which consists of the panel and the height
      # axis.
      g_feature_dendro <- .gtable_extract(g=plotting.to_grob(p_dendro),
                                          element=c("panel", axis_element),
                                          partial_match=TRUE)
      
      # Insert the dendrogram at the position correct position around the
      # heatmap.
      g_heatmap <- .gtable_insert(g=g_heatmap,
                                  g_new=g_feature_dendro,
                                  where=show_feature_dendrogram,
                                  ref_element="panel",
                                  partial_match=TRUE)
    }
    
    # Re-introduce plot elements.
    g_heatmap <- plotting.reinsert_plot_elements(g=g_heatmap,
                                                 elements=c("strip_x", "strip_y"),
                                                 grob_list=extracted_elements,
                                                 ggtheme=ggtheme)
    
    # Add combined grob to list
    figure_list <- append(figure_list, list(g_heatmap))
    
    # Add extract elements to the grob_element_list
    extracted_element_list <- .append_new(extracted_element_list, extracted_elements)
  }
  
  # Obtain layout dimensions (rows, cols).
  layout_dims <- plotting.get_plot_layout_dims(plot_layout_table=plot_layout_table)
  
  # Combine features.
  g <- plotting.arrange_figures(grobs=figure_list,
                                n_rows=layout_dims[1],
                                n_cols=layout_dims[2],
                                elements=setdiff(elements, c("strip_x", "strip_y")),
                                element_grobs=extracted_element_list,
                                ggtheme=ggtheme)
  
  return(g)
}



.create_expression_heatmap <- function(x,
                                       x_axis_by,
                                       y_axis_by,
                                       facet_by,
                                       facet_wrap_cols,
                                       ggtheme,
                                       gradient_palette,
                                       gradient_palette_range,
                                       x_label,
                                       y_label,
                                       legend_label,
                                       plot_title,
                                       plot_sub_title,
                                       caption,
                                       rotate_x_tick_labels,
                                       show_feature_dendrogram,
                                       show_sample_dendrogram){
  
  # Determine whether a sequential or divergent palette should be used by default.
  palette_type <- ifelse(length(gradient_palette_range) > 2,
                         "divergent",
                         "sequential")
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                    y=!!sym(y_axis_by),
                                                    fill=!!sym("value")))
  p <- p + ggtheme
  
  if(!is_empty(x)){
    p <- p + ggplot2::geom_raster()
    
  } else {
    # Insert blank for empty datasets.
    p <- p + ggplot2::geom_blank()
  }
  
  
  # Colors
  gradient_colours <- plotting.get_palette(x=gradient_palette, palette_type=palette_type)
  
  # Add gradient palette. If the legend is not shown, legend_label equals NULL.
  p <- p + ggplot2::scale_fill_gradientn(name=legend_label,
                                         colors=gradient_colours,
                                         limits=range(gradient_palette_range),
                                         oob=scales::squish)
  
  
  # Create show_dendrogram that combines show_feature_dendrogram and
  # show_sample_dendrogram.
  show_dendrogram <- c(show_feature_dendrogram, show_sample_dendrogram)
  
  # Show dendrogram determines where tick labels and axis labels are placed. By
  # default axes are located at the bottom and right.
  x_axis_position <- "bottom"
  y_axis_position <- "left"
  if(!is.null(show_dendrogram)){
    if("bottom" %in% show_dendrogram){
      x_axis_position <- "top"
    }
    
    if("left" %in% show_dendrogram){
      y_axis_position <- "right"
    }
  }
  
  # Specify both axes. Note that only the heatmap is shown, without additional
  # space between the plot area and the axes.
  p <- p + ggplot2::scale_x_discrete(position=x_axis_position, expand=c(0, 0))
  p <- p + ggplot2::scale_y_discrete(position=y_axis_position, expand=c(0, 0))
  
  # Set labels.
  p <- p + ggplot2::labs(x=x_label, y=y_label, title=plot_title, subtitle=plot_sub_title, caption=caption)
  
  # Determine how plots are facetted. The actual facets are created in the
  # calling function, not here.
  facet_by_list <- plotting.parse_facet_by(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  if(!is.null(facet_by)){
    if(is.null(facet_wrap_cols)){
      # Use a grid
      p <- p + ggplot2::facet_grid(rows=facet_by_list$facet_rows,
                                   cols=facet_by_list$facet_cols,
                                   labeller="label_context",
                                   drop=TRUE)
    } else {
      p <- p + ggplot2::facet_wrap(facets=facet_by_list$facet_by,
                                   labeller="label_context",
                                   drop=TRUE)
    }
  }
  
  # Rotate x-axis ticks
  if(rotate_x_tick_labels){
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
  }
  
  return(p)
}



.create_expression_dendrogram_plot <- function(x,
                                              position,
                                              ggtheme,
                                              dist_range,
                                              dist_n_breaks,
                                              dist_breaks,
                                              plot_height,
                                              rotate_x_tick_labels){
  
  # Check if there is any data to plot.
  if(is_empty(x)) return(NULL)
  
  # Define the range along the x-axis.
  x_range <- range(x$x_1)
  x_range <- c(x_range[1] - 0.5, x_range[2] + 0.5)
  
  
  # y_range
  if(is.null(dist_range)) dist_range <- range(c(x$y_1, x$y_2))
  
  # y_breaks
  if(is.null(dist_breaks)){
    plotting.check_input_args(y_range=dist_range,
                              y_n_breaks=dist_n_breaks)
    
    # Create breaks and update y_range
    dist_breaks <- labeling::extended(m=dist_n_breaks,
                                   dmin=dist_range[1],
                                   dmax=dist_range[2],
                                   only.loose=TRUE)
    
    dist_range <- c(head(dist_breaks, n=1), tail(dist_breaks, n=1))
  }
  
  plotting.check_input_args(y_range=dist_range,
                            y_breaks=dist_breaks)
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("x_1"),
                                                    y=!!sym("y_1"),
                                                    xend=!!sym("x_2"),
                                                    yend=!!sym("y_2")))
  p <- p + ggtheme
  
  # Plot line segments.
  p <- p + ggplot2::geom_segment(lineend="round")
  
  if(position=="right"){
    p <- p + ggplot2::scale_x_continuous(limits=x_range, expand=c(0, 0))
    p <- p + ggplot2::scale_y_continuous(limits=dist_range, breaks=dist_breaks)
    p <- p + ggplot2::coord_flip()
    
  } else if(position=="bottom"){
    p <- p + ggplot2::scale_x_continuous(limits=x_range, expand=c(0, 0))
    p <- p + ggplot2::scale_y_reverse(limits=rev(dist_range), breaks=rev(dist_breaks))
    
  } else if(position=="left"){
    p <- p + ggplot2::scale_x_continuous(limits=x_range, expand=c(0, 0))
    p <- p + ggplot2::scale_y_reverse(limits=rev(dist_range), breaks=rev(dist_breaks))
    p <- p + ggplot2::coord_flip()
    
  } else if(position=="top"){
    p <- p + ggplot2::scale_x_continuous(limits=x_range, expand=c(0, 0))
    p <- p + ggplot2::scale_y_continuous(limits=dist_range, breaks=dist_breaks)
    
  } else {
    ..error_reached_unreachable_code(paste0(".create_expression_dendrogram_plot: unknown position encountered: ", position))
  }
  
  # Remove some theme elements and reduce margins. The histogram height is left.
  p <- p + ggplot2::theme(panel.grid=ggplot2::element_blank(),
                          panel.background=ggplot2::element_blank(),
                          panel.border=ggplot2::element_blank(),
                          axis.title.x=ggplot2::element_blank(),
                          axis.title.y=ggplot2::element_blank())
  
  if(position %in% c("top", "bottom")){
    # Remove x-axis
    p <- p + ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                            axis.ticks.x=ggplot2::element_blank(),
                            axis.text.x=ggplot2::element_blank())
    
  } else if(position %in% c("left", "right")){
    # Remove y-axis (rotated x-axis in plot)
    p <- p + ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                            axis.ticks.y=ggplot2::element_blank(),
                            axis.text.y=ggplot2::element_blank())
    # Rotate x-labels
    if(rotate_x_tick_labels){
      p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
    }
  }
  
  # Ensure that panel heights/widths are set to the plot object.
  if(position %in% c("top", "bottom")){
    p$custom_grob <- list("heights"=list("name"="panel", "height"=plot_height))
    
  } else if(position %in% c("left", "right")){
    p$custom_grob <- list("widths"=list("name"="panel", "width"=plot_height))
  }
  
  class(p) <- c("familiar_ggplot", class(p))
  
  return(p)
}



.determine_sample_clustering_plot_dimensions <- function(x,
                                                         x_axis_by,
                                                         y_axis_by,
                                                         facet_by,
                                                         facet_wrap_cols,
                                                         features,
                                                         samples,
                                                         show_feature_dendrogram,
                                                         show_sample_dendrogram,
                                                         rotate_x_tick_labels){
  
  # Obtain facetting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Determine the number of elements along the x-axis.
  if(x_axis_by == "feature"){
    x_n_elements <- length(features)
    x_longest_element <- max(sapply(features, nchar))
    y_n_elements <- length(samples)
    y_longest_element <- max(sapply(samples, nchar))
    
  } else {
    x_n_elements <- length(samples)
    x_longest_element <- max(sapply(samples, nchar))
    y_n_elements <- length(features)
    y_longest_element <- max(sapply(features, nchar))
  }
  
  # Assume each x-axis element takes up about 0.5 cm. Then add some room for
  # other plot elements.
  default_width <- x_n_elements * 0.5 + 1.0
  
  # Assume each y-axis element takes up about 0.5 cm as well.
  default_height <- y_n_elements * 0.5 + 1.0
  
  # Reserve space for y-axis and x-axis tick labels. Assume that the typical
  # width of a character is about 5 points (1.8 mm). For the x-axis we only
  # reserve extra space in case the ticks are rotated, otherwise we just
  # assume a typical height of 10 points (3.6 mm).
  y_tick_space <- y_longest_element * 0.18
  x_tick_space <- ifelse(rotate_x_tick_labels, x_longest_element * 0.18, 0.36)
  
  # Reserve space for the dendrograms (1.5 cm)
  dendro_height <- ifelse(any(c("top", "bottom") %in% c(show_feature_dendrogram, show_sample_dendrogram)), 1.5, 0.0)
  dendro_width <- ifelse(any(c("left", "right") %in% c(show_feature_dendrogram, show_sample_dendrogram)), 1.5, 0.0)
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * (default_height + x_tick_space + dendro_height), 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm). We leave some
  # room for the legend on the right.
  width <- min(c(5 + plot_dims[2] * (default_width + y_tick_space + dendro_width), 19))
  
  return(c(height, width))
}



.normalise_expression_data <- function(x,
                                       show_normalized_data,
                                       feature_info){
  
  # Check for empty data
  if(is_empty(x)) return(NULL)
  
  # Apply normalisation
  if(show_normalized_data == "fixed"){
    # Fixed normalization applies normalisation and transformation parameters
    # derived during model creation. Note that extract_feature_expression method
    # already corrects batch differences (if present), and we do not apply batch
    # normalisation as a consequence.
    
    # Transform features
    x <- transform_features(data=x,
                            feature_info_list=feature_info,
                            features=names(feature_info),
                            invert=FALSE)
    
    # Normalise features
    x <- normalise_features(data=x,
                            feature_info_list=feature_info,
                            features=names(feature_info),
                            invert=FALSE)
    
  } else if(show_normalized_data == "set_normalisation"){
    
    # Normalise features within the current dataset.
    for(curr_feat in names(feature_info)){
      x[, (curr_feat):=.normalise(get(curr_feat),
                                  norm_method="standardisation_winsor")]
    }
  }
  
  return(x)
}



.complete_expression_table <- function(x,
                                       feature_info,
                                       feature_order,
                                       sample_order,
                                       gradient_palette_range){
  
  # Replace categorical features by numerical values and scale to 0.05-.95 of
  # the z-range.
  categorical_features <- lapply(feature_info, function(feature){
    if(feature@feature_type == "factor"){
      return(feature@name)
      
    } else{
      return(NULL)
    }
  })
  
  # Identify categorical features
  categorical_features <- unique(unlist(categorical_features))
  
  # Convert to numerical and rescale so that the categorical values lie within
  # the range of the remaining numerical data.
  if(length(categorical_features) > 0){
    
    # Determine the output value range
    output_value_range <- numeric(2)
    output_value_range[1] <- head(gradient_palette_range, n=1) + 0.05 * diff(range(gradient_palette_range))
    output_value_range[2] <- head(gradient_palette_range, n=1) + 0.95 * diff(range(gradient_palette_range))

    # Make a local copy of x to prevent warnings raised by data.table.
    x <- data.table::copy(x)
    
    for(feature in categorical_features){
      # Convert categorical data to numerical data.
      numeric_data <- as.numeric(x[[feature]])
      
      # Determine the range of the input value range.
      input_value_range <- c(1, length(feature_info[[feature]]@levels))
      
      # Convert numeric data to the output range.
      numeric_data <- (numeric_data - input_value_range[1]) / (diff(input_value_range)) * diff(output_value_range) + output_value_range[1]
      
      # Update column
      x[[feature]] <- numeric_data
    }
  }
  
  # Copy x and revert feature_1 and feature_2 so that all pairs are present.
  x <- melt(data=x, measure.vars=names(feature_info), variable.name="feature", value.name="value",
            variable.factor=TRUE, value.factor=FALSE)
  
  # Change subject_id to sample
  data.table::setnames(x=x, old="subject_id", new="sample")
  
  # Set feature and sample order
  x$feature <- factor(x$feature, levels=feature_order$name[order(feature_order$label_order)])
  x$sample <- factor(x$sample, levels=sample_order$name[order(sample_order$label_order)])

  return(x)
}
