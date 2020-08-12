#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot model performance.
#'
#'@description This method creates plots that show model performance from the
#'  data stored in a familiarCollection object. This method may create several
#'  types of plots, as determined by `plot_type`.
#'
#'@param dir_path (*optional*) Path to the directory where created performance
#'  plots are saved to. Output is saved in the `performance` subdirectory. If
#'  `NULL` no figures are saved, but are returned instead.
#'@param plot_type (*optional*) Type of plot to draw. This is one of `heatmap`
#'  (draws a heatmap), `barplot` (draws a barplot with confidence intervals),
#'  `boxplot` (draws a boxplot) and `violinplot` (draws a violin plot). Defaults
#'  to `violinplot`.
#'
#'  The choice for `plot_type` affects several other arguments, e.g. `color_by`
#'  is not used for `heatmap` and `y_axis_by` is only used by `heatmap`.
#'@param discrete_palette (*optional*) Palette to use to color the different
#'  plot elements in case a value was provided to the `color_by` argument. Only
#'  used when `plot_type` is not `heatmap`.
#'@param gradient_palette (*optional*) Sequential or divergent palette used to
#'  color the raster in `heatmap` plots. This argument is not used for other
#'  `plot_type` value.
#'@param gradient_palette_range (*optional*) Numerical range used to span the
#'  gradient. This should be a range of two values, e.g. `c(0, 1)`. Lower or
#'  upper boundary can be unset by using `NA`. If not set, the full
#'  metric-specific range is used.
#'@param annotate_performance (*optional*) Indicates whether performance in
#'  heatmaps should be annotated with text. Can be `none`, `value` (default), or
#'  `value_ci` (median value plus 95% credibility intervals).
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function plots model performance based on empirical bootstraps,
#'  using various plot representations.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set`,
#'  `evaluation_time` (survival outcome only) and `metric`. The default for
#'  `heatmap` is to split by `metric`, facet by `data_set` and
#'  `evaluation_time`, position `learner` along the x-axis and `fs_method` along
#'  the y-axis. The `color_by` argument is not used. The only valid options for
#'  `x_axis_by` and `y_axis_by` are `learner` and `fs_method`.
#'
#'  For other plot types (`barplot`, `boxplot` and `violinplot`), depends on the
#'  number of learners and feature selection methods:
#'
#'  * *one feature selection method and one learner*: the default is to split by
#'  `metric`, and have `data_set` along the x-axis.
#'
#'  * *one feature selection and multiple learners*: the default is to split by
#'  `metric`, facet by `data_set` and have `learner` along the x-axis.
#'
#'  * *multiple feature selection methods and one learner*: the default is to
#'  split by `metric`, facet by `data_set` and have `fs_method` along the
#'  x-axis.
#'
#'  * *multiple feature selection methods and learners*: the default is to split
#'  by `metric`, facet by `data_set`, colour by `fs_method` and have `learner`
#'  along the x-axis.
#'
#'  If applicable, additional faceting is performed for `evaluation_time`.
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
#'  Labeling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_model_performance
#'@md
#'@rdname plot_model_performance-methods
setGeneric("plot_model_performance",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    x_axis_by=NULL,
                    y_axis_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    plot_type=NULL,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=NULL,
                    gradient_palette_range=waiver(),
                    x_label=waiver(),
                    y_label=waiver(),
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    rotate_x_tick_labels=waiver(),
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    annotate_performance=NULL,
                    ...) standardGeneric("plot_model_performance"))

#####plot_model_performance (generic)#####

#'@rdname plot_model_performance-methods
setMethod("plot_model_performance", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   x_axis_by=NULL,
                   y_axis_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   plot_type=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=waiver(),
                   x_label=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   rotate_x_tick_labels=waiver(),
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   annotate_performance=NULL,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="model_performance"), list(...)))
            
            return(do.call(plot_model_performance,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "split_by"=split_by,
                                     "x_axis_by"=x_axis_by,
                                     "y_axis_by"=y_axis_by,
                                     "color_by"=color_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "ggtheme"=ggtheme,
                                     "plot_type"=plot_type,
                                     "discrete_palette"=discrete_palette,
                                     "gradient_palette"=gradient_palette,
                                     "gradient_palette_range"=gradient_palette_range,
                                     "x_label"=x_label,
                                     "y_label"=y_label,
                                     "legend_label"=legend_label,
                                     "plot_title"=plot_title,
                                     "plot_sub_title"=plot_sub_title,
                                     "caption"=caption,
                                     "rotate_x_tick_labels"=rotate_x_tick_labels,
                                     "y_range"=y_range,
                                     "y_n_breaks"=y_n_breaks,
                                     "y_breaks"=y_breaks,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units,
                                     "annotate_performance"=annotate_performance)))
          })

#####plot_model_performance (collection)#####

#'@rdname plot_model_performance-methods
setMethod("plot_model_performance", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   x_axis_by=NULL,
                   y_axis_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   plot_type=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=waiver(),
                   x_label=waiver(),
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   rotate_x_tick_labels=waiver(),
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   annotate_performance=NULL,
                   ...){
           
            ##### Check input arguments ----------------------------------------
            
            # ggtheme
            if(!is(ggtheme, "theme")) {
              ggtheme <- plotting.get_theme(use_theme=ggtheme)
              
            } else if(is.waive(rotate_x_tick_labels)){
              rotate_x_tick_labels <- FALSE
            }
            
            # rotate_x_tick_labels
            if(is.waive(rotate_x_tick_labels)) rotate_x_tick_labels <- TRUE
            
            # Check plot type.
            if(!is.null(plot_type)){
              .check_parameter_value_is_valid(x=plot_type, var_name="plot_type",
                                              values=c("heatmap", "barplot", "boxplot", "violinplot"))
              
            } else {
              # Set default to violin plot.
              plot_type <- "violinplot"
            }
            
            if(plot_type == "heatmap"){
              # For the heatmap we require aggregated data.
              
              # Load the data.
              x <- export_model_performance(object=object, export_raw=FALSE)
              x <- x$ensemble$data
              
              # Check that the data is not empty.
              if(is_empty(x)) return(NULL)
              
            } else {
              # For other data we require de-aggregated data.
              
              # Load the data.
              x <- export_model_performance(object=object, export_raw=TRUE)
              
              if(is_empty(x$ensemble$bootstrap_data) & !is_empty(x$ensemble$model_data)){
                warning(paste0("Creating a ", plot_type, " requires de-aggregated data, which are not available."))
                return(NULL)
                
              } else if(is_empty(x$ensemble$bootstrap_data)){
                return(NULL)
              }
              
              x <- x$ensemble$bootstrap_data
            }
            
            # 
            if(object@outcome_type %in% c("survival")){
              split_variable <- "evaluation_time"
              
            } else {
              split_variable <- NULL
            }
            
            # Add default splitting variables.
            if(is.null(split_by) & is.null(color_by) & is.null(facet_by) & is.null(x_axis_by) & is.null(y_axis_by)){
              if(plot_type == "heatmap"){
                # Split by metric.
                split_by <- c("metric")
                
                # Set facetting variables.
                facet_by <- c("data_set", split_variable)
                
                # Set x-axis variables.
                x_axis_by <- c("learner")
                
                # Set y-axis variables. This splitting variable is only used in heatmaps.
                y_axis_by <- c("fs_method")
                
              } else {
                # Determine the number of learners and feature_selection methods.
                n_learner <- nlevels(x$learner)
                n_fs_method <- nlevels(x$fs_method)
                
                # Split by metric.
                split_by <- c("metric")
                
                # Set facetting variables.
                if(n_learner > 1 | n_fs_method > 1) facet_by <- c("data_set")
                
                # Set color variables. This splitting variable is only used in
                # non-heatmap plots.
                if(n_learner > 1 & n_fs_method > 1) color_by <- c("fs_method")
                
                # Set x-axis variables.
                if(n_learner == 1 & n_fs_method == 1){
                  x_axis_by <- c("data_set")

                } else if(n_learner == 1 & n_fs_method > 1){
                  x_axis_by <- c("fs_method")
                  
                } else {
                  x_axis_by <- c("learner")
                }
                
                # Add split variable (if any) to facet_by.
                facet_by <- c(facet_by, split_variable)
              }
            }
            
            if(plot_type == "heatmap"){
              # Check splitting variables and generate sanitised output
              
              # Check if the color_by argument is provided.
              if(!is.null(color_by)){
                warning("The color_by argument is ignored for heatmaps.")
                color_by <- NULL
              }
              
              split_var_list <- plotting.check_data_handling(x=x,
                                                             split_by=split_by,
                                                             facet_by=facet_by,
                                                             x_axis_by=x_axis_by,
                                                             y_axis_by=y_axis_by,
                                                             available=c("metric", "data_set", "fs_method", "learner", split_variable))
            } else {
              
              # Check if the y_axis_by argument is provided.
              if(!is.null(y_axis_by)){
                warning("The y_axis_by argument is ignored for non-heatmap plots.")
              }
              
              # Check splitting variables and generate sanitised output
              split_var_list <- plotting.check_data_handling(x=x,
                                                             split_by=split_by,
                                                             color_by=color_by,
                                                             facet_by=facet_by,
                                                             x_axis_by=x_axis_by,
                                                             available=c("metric", "data_set", "fs_method", "learner", split_variable))
            }
           
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            if(!is.null(split_var_list$x_axis_by)) x_axis_by <- split_var_list$x_axis_by
            if(!is.null(split_var_list$y_axis_by)) y_axis_by <- split_var_list$y_axis_by

            if(plot_type == "heatmap"){
              # Check that x_axis_by and y_axis_by only take fs_method or learner.
              if(!x_axis_by %in% c("fs_method", "learner", "data_set", split_variable)) stop("The x_axis_by argument should be one of fs_method, learner or data_set.")
              if(!y_axis_by %in% c("fs_method", "learner", "data_set", split_variable)) stop("The y_axis_by argument should be one of fs_method, learner or data_set.")
            }
            
            # x_label
            if(is.waive(x_label)){
              x_label <- switch(x_axis_by,
                                learner = "learner",
                                fs_method = "feature selection method",
                                data_set = "dataset",
                                metric = "metric",
                                evaluation_time = "time")
            }
            
            # annotate_performance
            if(is.null(annotate_performance)){
              annotate_performance <- "value"
              
            } else if(is.logical(annotate_performance)){
              annotate_performance <- ifelse(annotate_performance, "value", "none")
            }
            .check_parameter_value_is_valid(x=annotate_performance, var_name="annotate_performance",
                                            values=c("none", "value", "value_ci"))
            
            
            plotting.check_input_args(facet_wrap_cols=facet_wrap_cols,
                                      x_label=x_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption,
                                      rotate_x_tick_labels=rotate_x_tick_labels)
            
            ##### Create plots -------------------------------------------------
            
            # Split data.
            if(!is.null(split_by)){
              x_split <- split(x, by=split_by, drop=FALSE)
              
            } else {
              x_split <- list("null.name"=x)
            }
            
            # Store plots to list in case dir_path is absent.
            if(is.null(dir_path)){
              plot_list <- list()
            }
            
            # Iterate over data splits.
            for(ii in names(x_split)){
              
              # Skip empty datasets.
              if(is_empty(x_split[[ii]])) next()
              
              # Generate plot
              p <- .plot_model_performance_plot(x=x_split[[ii]],
                                                x_axis_by=x_axis_by,
                                                y_axis_by=y_axis_by,
                                                color_by=color_by,
                                                facet_by=facet_by,
                                                facet_wrap_cols=facet_wrap_cols,
                                                plot_type=plot_type,
                                                ggtheme=ggtheme,
                                                discrete_palette=discrete_palette,
                                                gradient_palette=gradient_palette,
                                                gradient_palette_range=gradient_palette_range,
                                                x_label=x_label,
                                                y_label=y_label,
                                                legend_label=legend_label,
                                                plot_title=plot_title,
                                                plot_sub_title=plot_sub_title,
                                                caption=caption,
                                                rotate_x_tick_labels=rotate_x_tick_labels,
                                                y_range=y_range,
                                                y_n_breaks=y_n_breaks,
                                                y_breaks=y_breaks,
                                                annotate_performance=annotate_performance,
                                                outcome_type=object@outcome_type)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw figure.
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                
                # Add plot type as a subtype.
                subtype <- plot_type
                
                # Determine the subtype
                if(!is.null(split_by)){
                  subtype <- c(subtype, as.character(sapply(split_by, function(jj, x) (x[[jj]][1]), x=x_split[[ii]])))
                  subtype <- paste0(subtype, collapse="_")
                }
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_model_performance_plot_dimensions(x=x_split[[ii]],
                                                                              plot_type=plot_type,
                                                                              x_axis_by=x_axis_by,
                                                                              y_axis_by=y_axis_by,
                                                                              facet_by=facet_by,
                                                                              facet_wrap_cols=facet_wrap_cols,
                                                                              rotate_x_tick_labels=rotate_x_tick_labels)
                
                # Save to file.
                do.call(plotting.save_plot_to_file,
                        args=append(list("plot_obj"=p,
                                         "object"=object,
                                         "dir_path"=dir_path,
                                         "type"="performance",
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



.plot_model_performance_plot <- function(x,
                                         x_axis_by,
                                         y_axis_by,
                                         color_by,
                                         facet_by,
                                         facet_wrap_cols,
                                         plot_type,
                                         ggtheme,
                                         discrete_palette,
                                         gradient_palette,
                                         gradient_palette_range,
                                         x_label,
                                         y_label,
                                         legend_label,
                                         plot_title,
                                         plot_sub_title,
                                         caption,
                                         rotate_x_tick_labels,
                                         y_range,
                                         y_n_breaks,
                                         y_breaks,
                                         annotate_performance,
                                         outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- metric <- median <- ci_low <- ci_up <- NULL
  
  # Identify metrics in the current dataset.
  metrics <- as.character(unique(x$metric))
  
  # Check y-range for all plots except heatmaps.
  if(plot_type != "heatmap"){
    
    if(is.null(y_range)){
      
      # Obtain default ranges for the metrics.
      metric_ranges <- lapply(metrics, metric.get_metric_default_range, outcome_type=outcome_type)
      
      # Give a name to the list elements.
      names(metric_ranges) <- metrics
      
      # Placeholder range
      y_range <- c(Inf, -Inf)
      
      for(current_metric in metrics){
        metric_range <- metric_ranges[[current_metric]]
        
        # Replace any positive infinite value by the max range in the data.
        if(any(metric_range == Inf)){
          metric_range[metric_range == Inf] <- max(x[metric==current_metric, value], na.rm=TRUE)
        }
        
        # Replace any negative infinite value by the min range in the data.
        if(any(metric_range == -Inf)){
          metric_range[metric_range == -Inf] <- min(x[metric==current_metric, value], na.rm=TRUE)
        }
        
        if(y_range[1] > min(metric_range)){
          y_range[1] <- min(metric_range)
        }
        
        if(y_range[2] < max(metric_range)){
          y_range[2] <- max(metric_range)
        }
      }
      
    } else {
      plotting.check_input_args(y_range=y_range)
    }
    
    # y_breaks
    if(is.null(y_breaks)){
      plotting.check_input_args(y_range=y_range, y_n_breaks=y_n_breaks)
      
      # Create breaks and update x_range
      y_breaks <- labeling::extended(m=y_n_breaks,
                                     dmin=y_range[1],
                                     dmax=y_range[2],
                                     only.loose=TRUE)
      y_range  <- c(0, tail(y_breaks, n=1))
      
    } else {
      plotting.check_input_args(y_breaks=y_breaks)
    }
    
    # y_label for non-heatmap plots
    if(is.waive(y_label)){
      y_label <- ifelse(length(metrics)==1, metrics, "value")
    }
    
    # Create a legend label
    legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                 color_by=color_by)
    
  } else {
    
    # y-label for heatmap plots
    if(is.waive(y_label)){
      y_label <- switch(y_axis_by,
                        learner = "learner",
                        fs_method = "feature selection method",
                        data_set = "dataset",
                        metric = "metric",
                        evaluation_time = "time")
      
    }
    
    # gradient_palette_range
    if(is.waive(gradient_palette_range)){
      if(length(metrics) == 1){
        gradient_palette_range <- metric.get_metric_default_range(metric=metrics,
                                                                  outcome_type=outcome_type)
        gradient_was_provided <- FALSE
        
      } else {
        # If metric for whatever reason is not a single metric.
        gradient_palette_range <- c(NA, NA)
        gradient_was_provided <- FALSE
      }
      
    } else {
      
      # Check for NULL.
      if(is.null(gradient_palette_range)) gradient_palette_range <- c(NA, NA)
      
      gradient_was_provided <- TRUE
    }
    
    # Create a legend label
    legend_label <- ifelse(length(metrics)==1 & is.waive(legend_label), metrics, "value")
  }
  
  # Check remaining input arguments.
  plotting.check_input_args(y_label=y_label,
                            legend_label=legend_label)
  
  # Create basic plot
  p <- ggplot2::ggplot()
  p <- p + ggtheme
  
  if(plot_type == "heatmap"){
    
    ##### heatmap --------------------------------------------------------------
    
    # Create summary data.
    x_bar <- x[, list("median"=stats::median(value, na.rm=TRUE),
                      "ci_up"=stats::quantile(value, probs=0.975, na.rm=TRUE, names=FALSE),
                      "ci_low"=stats::quantile(value, probs=0.025, na.rm=TRUE, names=FALSE)),
               by=c("metric", "data_set", "fs_method", "learner")]
    
    # Determine what direction a metric has.
    if(length(metrics) == 1){
      invert_scale <- !metric.main(metric=metrics, purpose="higher_score_better")
    } else {
      invert_scale <- FALSE
    }
    
    # Determine the type of sequential colorscale. This has no effect if the
    # user provides a colorscale.
    if(length(metrics) == 1 & !gradient_was_provided){
      palette_type <- ifelse(length(gradient_palette_range) > 2,
                             "divergent",
                             "sequential")
    } else {
      palette_type <- "sequential"
    }
    
    # Form heatmap raster.
    p <- p + ggplot2::geom_raster(data=x_bar,
                                  mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                       y=!!sym(y_axis_by),
                                                       fill=!!sym("median")))
    
    # Colors
    gradient_colours <- plotting.get_palette(x=gradient_palette, palette_type=palette_type)
    if(invert_scale) gradient_colours <- rev(gradient_colours)
    
    # Add gradient palette.
    p <- p + ggplot2::scale_fill_gradientn(name=legend_label,
                                           colors=gradient_colours,
                                           limits=range(gradient_palette_range))
    
    # Obtain default settings.
    text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
    
    # Show performance value as text.
    if(annotate_performance == "value"){
      
      # Show median value.
      x_bar[is.finite(median), "performance_text":=plotting.format_number(median)]
      
      # Add to figure.
      p <- p + ggplot2::geom_text(data=x_bar,
                                  mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                       y=!!sym(y_axis_by),
                                                       label=!!sym("performance_text")),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size)
      
    } else if(annotate_performance == "value_ci"){
      # Show median value and credibility interval
      x_bar[is.finite(median), "performance_text":=paste0(plotting.format_number(median),
                                                          "\n(",
                                                          plotting.format_number(ci_low),
                                                          "\u2013",
                                                          plotting.format_number(ci_up),
                                                          ")")]
     
      # Add to figure.
      p <- p + ggplot2::geom_text(data=x_bar,
                                  mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                       y=!!sym(y_axis_by),
                                                       label=!!sym("performance_text")),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size)
    }
    
  } else if(plot_type == "barplot"){
    
    ##### barplot --------------------------------------------------------------
    
    # Create data for bar
    x_bar <- x[, list("median"=stats::median(value, na.rm=TRUE),
                      "ci_up"=stats::quantile(value, probs=0.975, na.rm=TRUE, names=FALSE),
                      "ci_low"=stats::quantile(value, probs=0.025, na.rm=TRUE, names=FALSE)),
               by=c("metric", "data_set", "fs_method", "learner")]
    
    # Generate a guide table
    guide_list <- plotting.create_guide_table(x=x_bar, color_by=color_by, discrete_palette=discrete_palette)
    
    # Extract data
    x_bar <- guide_list$data
    
    # Set breaks and limits
    p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
    
    if(is.null(color_by)){
      # Add barplot
      p <- p + ggplot2::geom_bar(data=x_bar,
                                 mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                      y=!!sym("median")),
                                 stat="identity",
                                 position="dodge")
      
      # Add error bars
      p <- p + ggplot2::geom_errorbar(data=x_bar,
                                      mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                           ymin=!!sym("ci_low"),
                                                           ymax=!!sym("ci_up")),
                                      position=ggplot2::position_dodge(width=0.9),
                                      width=0.20)
      
    } else {
      
      # Extract guide_table for color
      g_color <- guide_list$guide_color
      
      # Add barplot.
      p <- p + ggplot2::geom_bar(data=x_bar,
                                 mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                      y=!!sym("median"),
                                                      fill=!!sym("color_breaks")),
                                 stat="identity",
                                 position=ggplot2::position_dodge(width=0.9))
      
      # Add error bars
      p <- p + ggplot2::geom_errorbar(data=x_bar,
                                      mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                           ymin=!!sym("ci_low"),
                                                           ymax=!!sym("ci_up"),
                                                           group=!!sym("color_breaks")),
                                      position=ggplot2::position_dodge(width=0.9),
                                      width=0.20)
      
      # Set fill colours.
      p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          drop=FALSE)
    }
    
  } else if(plot_type == "boxplot"){
    
    ##### boxplot --------------------------------------------------------------
    
    # Generate a guide table
    guide_list <- plotting.create_guide_table(x=x, color_by=color_by, discrete_palette=discrete_palette)
    
    # Extract data
    x <- guide_list$data
    
    # Set breaks and limits
    p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
    
    if(is.null(color_by)){
      
      # Create boxplot.
      p <- p + ggplot2::geom_boxplot(data=x,
                                     mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                          y=!!sym("value")),
                                     outlier.alpha=0.1)
      
    } else {
      # Extract guide_table for color
      g_color <- guide_list$guide_color
      
      # Create boxplot.
      p <- p + ggplot2::geom_boxplot(data=x,
                                     mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                          y=!!sym("value"),
                                                          colour=!!sym("color_breaks")),
                                     outlier.alpha=0.1)
      
      # Set fill colours.
      p <- p + ggplot2::scale_colour_manual(name=legend_label$guide_color,
                                            values=g_color$color_values,
                                            breaks=g_color$color_breaks,
                                            drop=FALSE)
    }
    
  } else if(plot_type == "violinplot"){
    
    ##### violinplot -----------------------------------------------------------
    
    # Generate a guide table
    guide_list <- plotting.create_guide_table(x=x, color_by=color_by, discrete_palette=discrete_palette)
    
    # Extract data
    x <- guide_list$data
    
    # Set breaks and limits
    p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
    
    if(is.null(color_by)){
      
      # Create boxplot.
      p <- p + ggplot2::geom_violin(data=x,
                                    mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                         y=!!sym("value")),
                                    draw_quantiles=c(0.025, 0.5, 0.975),
                                    scale="width",
                                    position=ggplot2::position_dodge(width = 1.0))
      
    } else {
      # Extract guide_table for color
      g_color <- guide_list$guide_color
      
      # Create boxplot.
      p <- p + ggplot2::geom_violin(data=x,
                                    mapping=ggplot2::aes(x=!!sym(x_axis_by),
                                                         y=!!sym("value"),
                                                         fill=!!sym("color_breaks")),
                                    draw_quantiles=c(0.025, 0.5, 0.975),
                                    scale="width",
                                    position=ggplot2::position_dodge(width = 1.0))
      
      # Set fill colours.
      p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          drop=FALSE)
    }
  }
  
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
  
  # Update labels.
  p <- p + ggplot2::labs(x=x_label, y=y_label, title=plot_title, subtitle=plot_sub_title, caption=caption)
  
  # Rotate x-axis ticks
  if(rotate_x_tick_labels){
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
  }
  
  return(p)
}



.determine_model_performance_plot_dimensions <- function(x,
                                                         plot_type,
                                                         x_axis_by,
                                                         y_axis_by,
                                                         facet_by,
                                                         facet_wrap_cols,
                                                         rotate_x_tick_labels){
  
  # Obtain facetting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Determine the number of elements along the x-axis.
  x_elements <- as.character(unique(x[[x_axis_by]]))
  x_n_elements <- length(x_elements)
  x_longest_element <- max(sapply(x_elements, nchar))
  
  if(plot_type == "heatmap"){
    # For heatmaps.
   
    # Determine the number of elements along the y-axis.
    y_elements <- as.character(unique(x[[y_axis_by]]))
    y_n_elements <- length(y_elements)
    y_longest_element <- max(sapply(y_elements, nchar))

    # Assume each x-axis element takes up about 0.8 cm. Then add some room for
    # other plot elements.
    default_width <- x_n_elements * 0.8 + 1.0
    
    # Assume each y-axis element takes up about 0.8 cm as well.
    default_height <- y_n_elements * 0.8 + 1.0
    
    # Reserve space for y-axis and x-axis tick labels. Assume that the typical
    # width of a character is about 5 points (1.8 mm). For the x-axis we only
    # reserve extra space in case the ticks are rotated, otherwise we just
    # assume a typical height of 10 points (3.6 mm).
    y_tick_space <- y_longest_element * 0.18
    x_tick_space <- ifelse(rotate_x_tick_labels, x_longest_element * 0.18, 0.36)
    
  } else {
    # For non-heatmap plots.
    
    # Assume each x-axis element takes up about 0.8 cm. Then add some room for
    # other plot elements.
    default_width <- x_n_elements * 0.8 + 1.0
    default_width <- max(c(4, default_width))
    
    # Set default height.
    default_height <- 4
    
    # Set tick space for the x-axis and y-axis. Assume that the y-axis tick
    # labels contain 4 digits.
    y_tick_space <- 4 * 0.18
    
    # For the x-axis we only reserve extra space in case the ticks are rotated,
    # otherwise we just assume a typical height of 10 points (3.6 mm).
    x_tick_space <- ifelse(rotate_x_tick_labels, x_longest_element * 0.18, 0.36)
  }
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height + x_tick_space, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width + y_tick_space, 19))
  
  return(c(height, width))
}
