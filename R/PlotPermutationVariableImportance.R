#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot permutation variable importance.
#'
#'@description This function plots the data on permutation variable importance
#'  stored in a familiarCollection object.
#'
#'@param draw (*optional*) Draws the plot if TRUE.
#'@param dir_path (*optional*) Path to the directory where created figures are
#'  saved to. Output is saved in the `variable_importance` subdirectory. If NULL
#'  no figures are saved, but are returned instead.
#'@param ggtheme (*optional*) `ggplot` theme to use for plotting.
#'@param discrete_palette (*optional*) Palette used to fill the bars in case a
#'  non-singular variable was provided to the `color_by` argument.
#'@param height (*optional*) Height of the plot. A default value is derived from
#'  the number of features and the number of facets.
#'@param width (*optional*) Width of the plot. A default value is derived from
#'  the number of facets.
#'@param units (*optional*) Plot size unit. Either `cm` (default), `mm` or `in`.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'@inheritDotParams extract_permutation_vimp -object
#'
#'@details This function generates a horizontal barplot that lists features by
#'  the estimated model improvement over that of a dataset where the respective
#'  feature is randomly permuted.
#'
#'  The following splitting variables are available for `split_by`, `color_by`
#'  and `facet_by`:
#'
#'  * `fs_method`: feature selection methods.
#'
#'  * `learner`: learners.
#'
#'  * `data_set`: data sets.
#'
#'  * `metric`: the model performance metrics.
#'
#'  * `evaluation_time`: the evaluation times (survival outcomes only).
#'
#'  * `similarity_threshold`: the similarity threshold used to identify groups
#'  of features to permute simultaneously.
#'
#'  By default, the data is split by `fs_method`, `learner` and `metric`,
#'  faceted by `data_set` and `evaluation_time`, and coloured by
#'  `similarity_threshold`.
#'
#'  Available palettes for `discrete_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'  Labelling methods such as `set_fs_method_names` or `set_feature_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'  Bootstrap confidence intervals (if present) can be shown using various
#'  styles set by `conf_int_style`:
#'
#'  * `point_line` (default): confidence intervals are shown as lines, on which
#'  the point estimate is likewise shown.
#'
#'  * `line` (default): confidence intervals are shown as lines, but the point
#'  estimate is not shown.
#'
#'  * `bar_line`: confidence intervals are shown as lines, with the point
#'  estimate shown as a bar plot with the opacity of `conf_int_alpha`.
#'
#'  * `none`: confidence intervals are not shown. The point estimate is shown as
#'  a bar plot.
#'
#'  For metrics where lower values indicate better model performance, more
#'  negative permutation variable importance values indicate features that are
#'  more important. Because this may cause confusion, values obtained for these
#'  metrics are mirrored around 0.0 for plotting (but not any tabular data
#'  export).
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_permutation_variable_importance
#'@md
#'@rdname plot_permutation_variable_importance-methods
setGeneric("plot_permutation_variable_importance",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    x_label=waiver(),
                    y_label="feature",
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    conf_int_style=c("point_line", "line", "bar_line", "none"),
                    conf_int_alpha=0.4,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_permutation_variable_importance"))

#####plot_permutation_variable_importance (generic)#####

#'@rdname plot_permutation_variable_importance-methods
setMethod("plot_permutation_variable_importance", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   x_label=waiver(),
                   y_label="feature",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   conf_int_style=c("point_line", "line", "bar_line", "none"),
                   conf_int_alpha=0.4,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object, "data_element"="permutation_vimp"),
                                     list(...)))
            
            return(do.call(plot_permutation_variable_importance,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "split_by"=split_by,
                                     "color_by"=color_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "ggtheme"=ggtheme,
                                     "discrete_palette"=discrete_palette,
                                     "x_label"=x_label,
                                     "y_label"=y_label,
                                     "legend_label"=legend_label,
                                     "plot_title"=plot_title,
                                     "plot_sub_title"=plot_sub_title,
                                     "caption"=caption,
                                     "x_range"=x_range,
                                     "x_n_breaks"=x_n_breaks,
                                     "x_breaks"=x_breaks,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "conf_int_style"=conf_int_style,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })

#####plot_permutation_variable_importance (collection)#####

#'@rdname plot_permutation_variable_importance-methods
setMethod("plot_permutation_variable_importance", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   x_label=waiver(),
                   y_label="feature",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   conf_int_style=c("point_line", "line", "bar_line", "none"),
                   conf_int_alpha=0.4,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            value <- ci_low <- ci_up <- NULL
            
            ##### Check input arguments ########################################
            
            # Get input data.
            x <- export_permutation_vimp(object=object, aggregate_results=TRUE)
            
            # Check that the data are not empty (e.g. NULL).
            if(is_empty(x)) return(NULL)
            
            # Check that the data are not evaluated at the model level.
            if(all(sapply(x, function(x) (x@detail_level == "model")))){
              ..warning_no_comparison_between_models()
              return(NULL)
            }
            
            # Obtain data element from list.
            if(is.list(x)){
              if(is_empty(x)) return(NULL)
              
              if(length(x) > 1) ..error_reached_unreachable_code("plot_model_performance: list of data elements contains unmerged elements.")
              
              # Get x directly.
              x <- x[[1]]
            }
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            # Check package requirements for plotting.
            if(!require_package(x=..required_plotting_packages(extended=FALSE),
                                purpose="to create permutation variable importance plots",
                                message_type="warning")){
              return(NULL)
            }

            # ggtheme
            if(!is(ggtheme, "theme")) ggtheme <- plotting.get_theme(use_theme=ggtheme)
            
            # conf_int_style
            if(length(conf_int_style) > 1) conf_int_style <- head(conf_int_style, n=1)
            
            # Set the style of the confidence interval to none, in case no
            # confidence interval data is present.
            if(!x@estimation_type %in% c("bci", "bootstrap_confidence_interval")) conf_int_style <- "none"

            # Encode the similarity_threshold as a factor.
            if(all(is.finite(x@data$similarity_threshold))){
              # In this case the data originates from dendrograms that have been
              # cut at a certain height.
              
              # Obtain unique similarity thresholds.
              similarity_values <- rev(sort(unique(x@data$similarity_threshold)))
              
              # Convert to factor, and add nicely formatted labels.
              x@data$similarity_threshold <- factor(x@data$similarity_threshold,
                                                    levels=similarity_values,
                                                    labels=format(x=similarity_values, nsmall=1L))
              
            } else if(all(is.infinite(x@data$similarity_threshold))) {
              # This happens when data is not based on fixed cuts of a
              # dendrogram.
              x@data$similarity_threshold <- factor(x@data$similarity_threshold,
                                                    levels=c(-Inf, Inf),
                                                    labels=c("clustered", "individual"))
              
              # Remove unused levels.
              x@data$similarity_threshold <- droplevels(x@data$similarity_threshold)
              
            } else {
              stop("Combinations of results from different types of clustering algorithms cannot plotted in one figure.")
            }
            
            
            # Set default splitting variables if none are provided.
            if(is.null(split_by) & is.null(color_by) & is.null(facet_by)){
              split_by <- c("fs_method", "learner", "metric")
              
              facet_by <- c("data_set")
              if(object@outcome_type %in% c("survival")) facet_by <- c(facet_by, "evaluation_time")
              
              color_by <- c("similarity_threshold")
            }
            
            # Set available splitting variables.
            available_splitting_variables <- c("fs_method", "learner", "data_set", "metric", "similarity_threshold")
            if(object@outcome_type %in% c("survival")) available_splitting_variables <- c(available_splitting_variables, "evaluation_time")
            
            # Check splitting variables and generate sanitised output.
            split_var_list <- plotting.check_data_handling(x=x@data,
                                                           split_by=split_by,
                                                           color_by=color_by,
                                                           facet_by=facet_by,
                                                           available=available_splitting_variables)
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            # Parse legend label
            if(is.waive(legend_label)){
              legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                           color_by=color_by)
              
              # Update "similarity threshold" in the legend label to be more
              # specific.
              if(!is.null(color_by)){
                if(grepl(pattern="similarity threshold",
                         x=legend_label$guide_color,
                         fixed=TRUE)){
                  
                  if(all(levels(x@data$similarity_threshold) %in% c("clustered", "individual"))){
                    legend_label$guide_color <- sub(pattern="similarity threshold",
                                                    replacement="clustering",
                                                    x=legend_label$guide_color,
                                                    fixed=TRUE)
                    
                  } else {
                    legend_label$guide_color <- sub(pattern="similarity threshold",
                                                    replacement=paste0(x@similarity_metric, " threshold"),
                                                    x=legend_label$guide_color,
                                                    fixed=TRUE)
                  }
                }
              }
            }
            
            
            # Iterate over the data to identify data that should be reworked.
            # For metrics where higher scores indicate worse performance,
            # permutation variable importance is higher the more negative the
            # value is. For other metrics permutation variable is better the
            # more positive a value is. To facilitate comparisons and avoid
            # confusion, we mirror values for the first type of metrics around
            # 0.0 here.
            x@data <- lapply(split(x@data, by="metric"), function(x, outcome_type){
              
              if(!metric.is_higher_score_better(metric=as.character(x$metric[1]),
                                                outcome_type=outcome_type)){
                # For metrics where lower scores mark better model performance,
                # a feature is more important when the variable importance is
                # more negative.
                x[, "value":=-value]
                
                if(!is.null(x$ci_low) & !is.null(x$ci_up)){
                  # Rename confidence interval columns by exchanging upper and
                  # lower bounds, and then mirror them around 0.0.
                  data.table::setnames(x, old=c("ci_low", "ci_up"), new=c("ci_up", "ci_low"))
                  x[, ":="("ci_low"=-ci_low,
                           "ci_up"=-ci_up)]
                }
              }
              
              return(x)
            },
            outcome_type = object@outcome_type)
            
            # Recombine dataset.
            x@data <- data.table::rbindlist(x@data, use.names=TRUE)
            
            
            if("metric" %in% facet_by | "metric" %in% color_by){
              available_metrics <- "combined"
              
            } else {
              available_metrics <- levels(x@data$metric)
            }
            
            # x_range depends on the 95% confidence intervals of individual
            # metrics (if split by metric), the overall range (if not split by
            # metric), or their respective point estimates (when confidence
            # interval data are absent).
            if(is.null(x_range)){
              # Iterate over metrics to determine the interval.
              x_range <- lapply(split(x@data, by="metric"), function(x, conf_int_style){
                if(conf_int_style == "none"){
                  interval <- data.table::data.table("min_value"=min(c(x$value, 0.0), na.rm=TRUE),
                                                     "max_value"=max(c(x$value, 0.0), na.rm=TRUE))
                  
                } else {
                  interval <- data.table::data.table("min_value"=min(c(x$ci_low, 0.0), na.rm=TRUE),
                                                     "max_value"=max(c(x$ci_up, 0.0), na.rm=TRUE))
                }
                
                return(interval)
              },
              conf_int_style=conf_int_style)
              
              # In case multiple metrics are combined in the same plot
              if("metric" %in% facet_by | "metric" %in% color_by){
                # Concatenate to a single data.table.
                x_range <- data.table::rbindlist(x_range)
                x_range <- list("combined"=data.table::data.table("min_value"=min(x_range$min_value, na.rm=TRUE),
                                                                  "max_value"=max(x_range$max_value, na.rm=TRUE)))
              }
              
            } else if(is.list(x_range)){
              
              # Check whether all metrics are present in the data provided by
              # the user.
              .check_parameter_value_is_valid(x=names(x_range),
                                              var_name="x_range",
                                              values=available_metrics)
              
              .check_argument_length(x=unique(names(x_range)),
                                     var_name="x_range",
                                     min=length(available_metrics),
                                     max=length(available_metrics))
              
              # Convert to the correct 
              x_range <- lapply(x_range, function(x_range){
                plotting.check_input_args(x_range=x_range)
                
                return(data.table::data.table("min_value"=min(x_range),
                                              "max_value"=max(x_range)))
              })
                
            } else {
              # For user-provided input.
              plotting.check_input_args(x_range=x_range)
              
              # Use the same range for each 
              x_range <- lapply(available_metrics, function(metric, x_range){
                return(data.table::data.table("min_value"=min(x_range),
                                              "max_value"=max(x_range)))
              },
              x_range=x_range)
              
              # Update names of the list elements
              names(x_range) <- available_metrics
            }
            
            
            # x_breaks
            if(is.null(x_breaks)){
              plotting.check_input_args(x_n_breaks=x_n_breaks)
              
              # Create x_breaks.
              x_breaks <- lapply(x_range, function(x_range, x_n_breaks){
                
                # Create breaks
                x_breaks <- labeling::extended(m=x_n_breaks,
                                               dmin=x_range$min_value,
                                               dmax=x_range$max_value,
                                               only.loose=TRUE)
                
                return(x_breaks)
              },
              x_n_breaks=x_n_breaks)
              
            } else if(is.list(x_breaks)){
              # Check whether all metrics are present in the data provided by
              # the user.
              .check_parameter_value_is_valid(x=names(x_breaks),
                                              var_name="x_breaks",
                                              values=available_metrics)
              
              .check_argument_length(x=unique(names(x_breaks)),
                                     var_name="x_breaks",
                                     min=length(available_metrics),
                                     max=length(available_metrics))
              
              # Check breaks.
              sapply(x_breaks, function(x_breaks) plotting.check_input_args(x_breaks=x_breaks))
              
            } else {
              plotting.check_input_args(x_breaks=x_breaks)
              
            }
            
            # Update x_range based on x_breaks.
            x_range <- lapply(available_metrics, function(metric, x_range, x_breaks){
              x_range[[metric]]$min_value <- head(x_breaks[[metric]], n=1)
              x_range[[metric]]$max_value <- tail(x_breaks[[metric]], n=1)
              
              return(x_range[[metric]])
            },
            x_range=x_range,
            x_breaks=x_breaks)
            
            # Set names.
            names(x_range) <- available_metrics
            
            # Check general input arguments
            plotting.check_input_args(y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption,
                                      facet_wrap_cols=facet_wrap_cols,
                                      conf_int_alpha=conf_int_alpha,
                                      conf_int_style=conf_int_style,
                                      conf_int_default=c("point_line", "line", "bar_line", "none"))
            
            
            ##### Create plots #################################################
            
            # Split data
            if(!is.null(split_by)){
              x_split <- split(x@data, by=split_by)
            } else {
              x_split <- list(x@data)
            }
            
            # Store plots to list in case no dir_path is provided
            if(is.null(dir_path)) plot_list <- list()
            
            # Iterate over splits
            for(x_sub in x_split){
              
              # Check that the table is not empty.
              if(is_empty(x_sub)) next()
              
              # Check that the table contains finite values.
              if(all(is.na(x_sub$value))) next()
              
              # Generate plot
              p <- .plot_permutation_variable_importance(x=x_sub,
                                                         color_by=color_by,
                                                         facet_by=facet_by,
                                                         facet_wrap_cols=facet_wrap_cols,
                                                         ggtheme=ggtheme,
                                                         discrete_palette=discrete_palette,
                                                         x_label=x_label,
                                                         y_label=y_label,
                                                         legend_label=legend_label,
                                                         plot_title=plot_title,
                                                         plot_sub_title=plot_sub_title,
                                                         caption=caption,
                                                         conf_int_alpha=conf_int_alpha,
                                                         conf_int_style=conf_int_style,
                                                         x_range=x_range,
                                                         x_breaks=x_breaks)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw plot
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                # Save to file
                if(!is.null(split_by)){
                  subtype <- paste0("permutation_", paste0(sapply(split_by, function(ii, x) (x[[ii]][1]), x=x_sub), collapse="_"))
                } else {
                  subtype <- "permutation"
                }
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_permutation_importance_plot_dimensions(x=x_sub,
                                                                                   facet_by=facet_by,
                                                                                   facet_wrap_cols=facet_wrap_cols)
                
                # Save to file.
                do.call(plotting.save_plot_to_file,
                        args=c(list("plot_obj"=p,
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
          })



#' Internal plotting function for permutation variable importance plots
#'
#' @inheritParams plot_permutation_variable_importance
#'
#' @return ggplot plot object.
#' 
#' @md
#' @keywords internal
.plot_permutation_variable_importance <- function(x,
                                                  color_by,
                                                  facet_by,
                                                  facet_wrap_cols,
                                                  ggtheme,
                                                  discrete_palette,
                                                  x_label,
                                                  y_label,
                                                  legend_label,
                                                  plot_title,
                                                  plot_sub_title,
                                                  caption,
                                                  conf_int_style,
                                                  conf_int_alpha,
                                                  x_range,
                                                  x_breaks){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- metric <- similarity_threshold <- order_id <- i.order_id <- NULL
  data_set <- learner <- fs_method <- NULL
  
  # Create local copy
  x <- data.table::copy(x)
  
  # x_label depends on whether a single metric is shown in each plot, or
  # multiple metrics are combined.
  if(is.waive(x_label)){
    if("metric" %in% facet_by | "metric" %in% color_by){
      x_label <- "variable importance"
      
    } else {
      x_label <- as.character(x$metric[1])
    }
  }
  
  # Check the label
  plotting.check_input_args(x_label=x_label)
  
  # Determine available metrics
  if("metric" %in% color_by | "metric" %in% facet_by){
    available_metric <- "combined"
    
  } else {
    available_metric <- as.character(x$metric[1])
  }
  
  # Sort features. In the outer loop iterate over metrics. In the inner loop
  # iterate over threshold values (in reverse). Resolve until order_id is unique
  # for all features.
  x[, "order_id":=1L]
  for(current_data_set in levels(x$data_set)){
    # Break in case all features have an unique order id.
    if(data.table::uniqueN(x$order_id) == data.table::uniqueN(x$feature)) break()
    
    for(current_fs_method in levels(x$fs_method)){
      # Break in case all features have an unique order id.
      if(data.table::uniqueN(x$order_id) == data.table::uniqueN(x$feature)) break()
      
      for(current_learner in levels(x$learner)){
        # Break in case all features have an unique order id.
        if(data.table::uniqueN(x$order_id) == data.table::uniqueN(x$feature)) break()
        
        for(current_metric in levels(x$metric)){
          
          # Break in case all features have an unique order id.
          if(data.table::uniqueN(x$order_id) == data.table::uniqueN(x$feature)) break()
          
          for(current_threshold in rev(levels(x$similarity_threshold))){
            
            for(id_table in split(x[data_set == current_data_set &
                                    fs_method == current_fs_method &
                                    learner == current_learner &
                                    metric == current_metric &
                                    similarity_threshold == current_threshold], by="order_id")){
              if(nrow(id_table) < 2) next()
              
              # Local copy
              id_table <- data.table::copy(id_table)
              
              # Rank by descending value.
              id_table[, "order_id":=order_id + data.table::frank(-value, ties.method="min") - 1L][, mget(c("feature", "order_id"))]
              
              # Update order id in x.
              x[id_table, "order_id":=i.order_id, on="feature"]
            }
            
            # Break in case all features have an unique order id.
            if(data.table::uniqueN(x$order_id) == data.table::uniqueN(x$feature)) break()
          }
        }
      }
    }
  }
  
  # Order features by order_id
  x$feature <- factor(x$feature, levels=rev(unique(x[, mget(c("feature", "order_id"))])[order(order_id)][["feature"]]))
  
  # Generate a guide table
  guide_list <- plotting.create_guide_table(x=x, 
                                            color_by=color_by,
                                            discrete_palette=discrete_palette)
  
  # Extract data
  x <- guide_list$data
  
  # Create basic plot.
  if(!is.null(color_by)){
    
    # Extract guide_table for colour
    g_color <- guide_list$guide_color
    
    if(conf_int_style %in% c("none")){
      
      p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("feature"),
                                                        y=!!sym("value"),
                                                        fill=!!sym("color_breaks")))
      
      p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          guide=ggplot2::guide_legend(reverse=TRUE),
                                          drop=FALSE)
      
    } else if(conf_int_style %in% c("bar_line")){
      
      p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("feature"),
                                                        y=!!sym("value"),
                                                        fill=!!sym("color_breaks"),
                                                        color=!!sym("color_breaks")))
      
      p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          guide=ggplot2::guide_legend(reverse=TRUE),
                                          drop=FALSE)
      
      p <- p + ggplot2::scale_colour_manual(name=legend_label$guide_color,
                                            values=g_color$color_values,
                                            breaks=g_color$color_breaks,
                                            guide=ggplot2::guide_legend(reverse=TRUE),
                                            drop=FALSE)
      
    } else if(conf_int_style %in% c("line", "point_line")){
      
      p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("feature"),
                                                        y=!!sym("value"),
                                                        color=!!sym("color_breaks")))
      
      p <- p + ggplot2::scale_colour_manual(name=legend_label$guide_color,
                                            values=g_color$color_values,
                                            breaks=g_color$color_breaks,
                                            guide=ggplot2::guide_legend(reverse=TRUE),
                                            drop=FALSE)
      
    } else {
      ..error_reached_unreachable_code(".plot_permutation_variable_importance: unknown confidence interval style.")
    }
    
    # Flip coordinates and add theme.
    p <- p + ggplot2::coord_flip()
    p <- p + ggtheme
    
  } else {
    
    # Basic plot.
    p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("feature"),
                                                      y=!!sym("value")))
    
    p <- p + ggplot2::coord_flip()
    p <- p + ggtheme
  }
  
  
  # Add main plotting elements.
  if(conf_int_style %in% c("bar_line")){
    
    p <- p + ggplot2::geom_col(alpha=conf_int_alpha,
                               position="dodge")
    
    p <- p + ggplot2::geom_linerange(mapping=ggplot2::aes(ymin=!!sym("ci_low"),
                                                          ymax=!!sym("ci_up")),
                                     position=ggplot2::position_dodge(width=0.9))
    
  } else if(conf_int_style %in% c("none")){
    
    p <- p + ggplot2::geom_col(position="dodge")
    
  } else if(conf_int_style %in% c("line")){
    
    p <- p + ggplot2::geom_linerange(mapping=ggplot2::aes(ymin=!!sym("ci_low"),
                                                          ymax=!!sym("ci_up")),
                                     position=ggplot2::position_dodge(width=0.8))
    
  } else if (conf_int_style %in% c("point_line")){
    
    p <- p + ggplot2::geom_pointrange(mapping=ggplot2::aes(ymin=!!sym("ci_low"),
                                                           ymax=!!sym("ci_up")),
                                      position=ggplot2::position_dodge(width=0.9))
    
  } else {
    ..error_reached_unreachable_code(".plot_permutation_variable_importance: unknown confidence interval style.")
  }
  
  
  # Set breaks and limits
  x_range <- c(x_range[[available_metric]]$min_value,
               x_range[[available_metric]]$max_value)
  
  p <- p + ggplot2::scale_y_continuous(breaks=x_breaks[[available_metric]],
                                       limits=x_range)
  
  # Determine how things are faceted.
  facet_by_list <- plotting.parse_facet_by(x=x,
                                           facet_by=facet_by,
                                           facet_wrap_cols=facet_wrap_cols)
  
  if(!is.null(facet_by)){
    if(is.null(facet_wrap_cols)){
      # Use a grid
      p <- p + ggplot2::facet_grid(rows=facet_by_list$facet_rows,
                                   cols=facet_by_list$facet_cols,
                                   labeller="label_context")
      
    } else {
      p <- p + ggplot2::facet_wrap(facets=facet_by_list$facet_by,
                                   labeller="label_context")
    }
  }
  
  # Add a line to indicate 0.0, if 0.0 is included in the range.
  if(x_range[1] <= 0.0 & x_range[2] >= 0.0){
    p <- p + ggplot2::geom_hline(yintercept=0.0,
                                 linetype="dotted")
  }
  
  # Update labels. Note that the inversion of x_label and y_label is correct, as the coordinates were flipped
  p <- p + ggplot2::labs(x=y_label,
                         y=x_label,
                         title=plot_title,
                         subtitle=plot_sub_title,
                         caption=caption)
  
  return(p)
}


.determine_permutation_importance_plot_dimensions <- function(x, facet_by, facet_wrap_cols){
  
  # Get plot layout dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x,
                                             facet_by=facet_by,
                                             facet_wrap_cols=facet_wrap_cols)
  
  # Determine the number of features within each facet.
  n_features <- data.table::uniqueN(x=x$feature)
  longest_name <- max(sapply(levels(x$feature), nchar))
  
  # Assume each feature takes up about 14 points (~5mm) with 2 point (0.7mm)
  # spacing. Then add some room for other plot elements.
  default_height <- n_features * 0.5 + (n_features - 1) * 0.07 + 1.0
  
  # Set a default height. Assume that the typical width of a character is about
  # 5 points (1.8mm).
  default_width <- 6 + longest_name * 0.18
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}
