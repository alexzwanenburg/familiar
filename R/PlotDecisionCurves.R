#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot decision curves.
#'
#'@description This method creates decision curves based on data in a
#'  familiarCollection object.
#'
#'@param dir_path (*optional*) Path to the directory where created decision
#'  curve plots are saved to. Output is saved in the `decision_curve_analysis`
#'  subdirectory. If `NULL`, figures are written to the folder, but are returned
#'  instead.
#'@param discrete_palette (*optional*) Palette to use to color the different
#'  plot elements in case a value was provided to the `color_by` argument.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'@inheritDotParams extract_decision_curve_data -object
#'
#'@details This function generates plots for decision curves.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `positive_class` (categorical outcomes) or `evaluation_time` (survival outcomes).
#'  By default, the data is split by `fs_method` and `learner`, with faceting by
#'  `data_set` and colouring by `positive_class` or `evaluation_time`.
#'
#'  Available palettes for `discrete_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'  Bootstrap confidence intervals of the decision curve (if present) can be
#'  shown using various styles set by `conf_int_style`:
#'
#'  * `ribbon` (default): confidence intervals are shown as a ribbon with an
#'  opacity of `conf_int_alpha` around the point estimate of the decision curve.
#'
#'  * `step` (default): confidence intervals are shown as a step function around
#'  the point estimate of the decision curve.
#'
#'  * `none`: confidence intervals are not shown. The point estimate of the
#'  decision curve is shown as usual.
#'
#'  Labelling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'@references 1. Vickers, A. J. & Elkin, E. B. Decision curve analysis: a novel
#'  method for evaluating prediction models. Med. Decis. Making 26, 565–574
#'  (2006).
#'
#'  1. Vickers, A. J., Cronin, A. M., Elkin, E. B. & Gonen, M. Extensions to
#'  decision curve analysis, a novel method for evaluating diagnostic tests,
#'  prediction models and molecular markers. BMC Med. Inform. Decis. Mak. 8, 53
#'  (2008).
#'
#'  1. Vickers, A. J., van Calster, B. & Steyerberg, E. W. A simple,
#'  step-by-step guide to interpreting decision curve analysis. Diagn Progn Res
#'  3, 18 (2019).
#'@exportMethod plot_decision_curve
#'@md
#'@rdname plot_decision_curve-methods
setGeneric("plot_decision_curve",
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
                    y_label=waiver(),
                    legend_label=waiver(),
                    plot_title=waiver(),
                    plot_sub_title=waiver(),
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    export_collection=FALSE,
                    ...) standardGeneric("plot_decision_curve"))

#####plot_decision_curve (generic)#####

#'@rdname plot_decision_curve-methods
setMethod("plot_decision_curve", signature(object="ANY"),
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
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=waiver(),
                   plot_sub_title=waiver(),
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="decision_curve_analyis"),
                                     list(...)))
            
            return(do.call(plot_decision_curve,
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
                                     "y_range"=y_range,
                                     "y_n_breaks"=y_n_breaks,
                                     "y_breaks"=y_breaks,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units,
                                     "export_collection"=export_collection)))
          })


#####plot_decision_curve (collection)#####

#'@rdname plot_decision_curve-methods
setMethod("plot_decision_curve", signature(object="familiarCollection"),
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
                   y_label=waiver(),
                   legend_label=waiver(),
                   plot_title=waiver(),
                   plot_sub_title=waiver(),
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   export_collection=FALSE,
                   ...){

            # Suppress NOTES due to non-standard evaluation in data.table
            curve_type <- ci_low <- ci_up <- net_benefit <- NULL
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
            # Get input data.
            x <- export_decision_curve_analysis_data(object=object, aggregate_results=TRUE)
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            # Check that the data are not evaluated at the model level.
            if(all(sapply(x, function(x) (x@detail_level == "model")))){
              ..warning_no_comparison_between_models()
              return(NULL)
            }
            
            # Obtain data element from list.
            if(is.list(x)){
              if(is_empty(x)) return(NULL)
              
              if(length(x) > 1) ..error_reached_unreachable_code("plot_decision_curve: list of data elements contains unmerged elements.")
              
              # Get x directly.
              x <- x[[1]]
            }
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            # Check package requirements for plotting.
            if(!require_package(x=..required_plotting_packages(extended=FALSE),
                                purpose="to plot decision curves",
                                message_type="warning")){
              return(NULL)
            }
            
            ##### Check input arguments ------------------------------------------------

            # ggtheme
            ggtheme <- .check_ggtheme(ggtheme)
            
            # x_label
            if(is.waive(x_label)) x_label <- "threshold probability"
            
            # y_label
            if(is.waive(y_label)) y_label <- "net benefit"
            
            # x_range
            if(is.null(x_range)) x_range <- c(0.0, 1.0)
            
            # x_breaks
            if(is.null(x_breaks)){
              plotting.check_input_args(x_n_breaks=x_n_breaks)
              
              # Create breaks
              x_breaks <- labeling::extended(m=x_n_breaks,
                                             dmin=x_range[1],
                                             dmax=x_range[2],
                                             only.loose=TRUE)
            }
            
            # conf_int_style
            if(length(conf_int_style) > 1) conf_int_style <- head(conf_int_style, n=1)
            
            # Set the style of the confidence interval to none, in case no
            # confidence interval data is present.
            if(!x@estimation_type %in% c("bci", "bootstrap_confidence_interval")) conf_int_style <- "none"

            # y_range
            if(is.null(y_range)){
              if(conf_int_style != "none"){
                # Base the y-range on the confidence intervals.
                y_range <- c(min(x@data[curve_type == "model" & is.finite(ci_low)]$ci_low),
                             max(x@data[curve_type == "model" & is.finite(ci_up)]$ci_up))
                
              } else {
                # Base the y-range on the range of the benefit.
                y_range <- c(min(c(0.0, min(x@data[curve_type == "model" & is.finite(net_benefit)]$net_benefit))),
                             max(c(0.0, max(x@data[curve_type == "model" & is.finite(net_benefit)]$net_benefit))))
              }
            }
            
            # y_breaks
            if(is.null(y_breaks)){
              plotting.check_input_args(y_n_breaks=y_n_breaks)
              
              # Create breaks
              y_breaks <- labeling::extended(m=y_n_breaks,
                                             dmin=y_range[1],
                                             dmax=y_range[2],
                                             only.loose=TRUE)
              
              # Adapt the y-range.
              y_range <- c(head(y_breaks, n=1),
                           tail(y_breaks, n=1))
            }
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              split_variable <- "positive_class"
              
            } else if(object@outcome_type %in% c("survival")){
              split_variable <- "evaluation_time"
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            # Splitting variables
            if(is.null(split_by) & is.null(facet_by) & is.null(color_by)){
              # Determine the number of learners and feature_selection methods.
              n_learner <- nlevels(x@data$learner)
              n_fs_method <- nlevels(x@data$fs_method)
              
              if(object@outcome_type %in% c("multinomial")){
                n_class_or_time <- nlevels(x@data$positive_class)
                
              } else if(object@outcome_type %in% c("binomial")){
                n_class_or_time <- 1L
              
              } else if(object@outcome_type %in% c("survival")){
                n_class_or_time <- nlevels(x@data$evaluation_time)
                
              } else {
                ..error_outcome_type_not_implemented(object@outcome_type)
              }
              
              if(n_learner > 1 & n_fs_method > 1){
                # Split by learner and feature selection method.
                split_by <- c("fs_method", "learner")
                
                if(n_class_or_time > 1){
                  color_by <- split_variable
                  facet_by <- "data_set"
                  
                } else {
                  color_by <- c("data_set", split_variable)
                }
                
              } else if(n_learner > 1){
                # Implying n_fs_method == 1
                
                if(n_class_or_time > 1){
                  split_by <- c("fs_method", "learner")
                  color_by <- split_variable
                  facet_by <- "data_set"
                  
                } else {
                  split_by <- c("fs_method")
                  color_by <- c("learner")
                  facet_by <- c("data_set", split_variable)
                }
                
              } else if(n_fs_method > 1){
                # Implying n_learner == 1
                
                if(n_class_or_time > 1){
                  split_by <- c("fs_method", "learner")
                  color_by <- split_variable
                  facet_by <- "data_set"
                  
                } else {
                  split_by <- "learner"
                  color_by <- "fs_method"
                  facet_by <- c("data_set", split_variable)
                }
                
              } else {
                # Implying n_learner == n_fs_method == 1
                split_by <- c("fs_method", "learner")
                
                if(n_class_or_time > 1){
                  color_by <- split_variable
                  facet_by <- "data_set"
                  
                } else {
                  color_by <- c("data_set", split_variable)
                }
              }
            }
            
            # Check splitting variables and generate sanitised output
            split_var_list <- plotting.check_data_handling(x=x@data,
                                                           split_by=split_by,
                                                           color_by=color_by,
                                                           facet_by=facet_by,
                                                           available=c("fs_method", "learner", "data_set", split_variable))
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            # Create a legend label
            legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                         color_by=color_by)
            
            # Check input arguments for validity.
            plotting.check_input_args(x_range=x_range,
                                      y_range=y_range,
                                      x_breaks=x_breaks,
                                      y_breaks=y_breaks,
                                      conf_int_alpha=conf_int_alpha,
                                      conf_int_style=conf_int_style,
                                      facet_wrap_cols=facet_wrap_cols,
                                      x_label=x_label,
                                      y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption)
            
            ##### Create plots ---------------------------------------------------------
            
            # Determine if subtitle should be generated.
            autogenerate_plot_subtitle <- is.waive(plot_sub_title)
            
            # Split data
            if(!is.null(split_by)){
              x_split <- split(x@data, by=split_by)
              
            } else {
              x_split <- list("null.name"=x@data)
            }
            
            # Store plots to list in case dir_path is absent.
            if(is.null(dir_path)) plot_list <- list()
            
            # Iterate over splits
            for(ii in names(x_split)){
              
              # Skip empty datasets
              if(is_empty(x_split[[ii]])) next()
              
              if(is.waive(plot_title)) plot_title <- "Decision curve"
              
              # Declare subtitle components.
              additional_subtitle <- NULL
              
              # Add evaluation time as subtitle component if it is not used
              # otherwise.
              if(!"evaluation_time" %in% c(split_by, color_by, facet_by) && object@outcome_type %in% c("survival")){
                additional_subtitle <- c(
                  additional_subtitle,
                  plotting.add_subtitle_time_point(x_split[[ii]]$evaluation_time[1]))
              }
              
              if(autogenerate_plot_subtitle){
                plot_sub_title <- plotting.create_subtitle(
                  split_by = split_by,
                  additional = additional_subtitle,
                  x = x_split[[ii]])
              }
              
              # Generate plot
              p <- .plot_decision_curve_plot(x=x_split[[ii]],
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
                                             x_range=x_range,
                                             x_breaks=x_breaks,
                                             y_range=y_range,
                                             y_breaks=y_breaks,
                                             conf_int_style=conf_int_style,
                                             conf_int_alpha=conf_int_alpha)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw figure.
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_decision_curve_plot_dimensions(
                  x=x_split[[ii]],
                  facet_by=facet_by,
                  facet_wrap_cols=facet_wrap_cols)
                
                # Save to file.
                do.call(
                  plotting.save_plot_to_file,
                  args=c(
                    list(
                      "plot_obj"=p,
                      "object"=object,
                      "dir_path"=dir_path,
                      "type"="decision_curve_analysis",
                      "subtype"="decision_curve",
                      "x"=x_split[[ii]],
                      "split_by"=split_by,
                      "height"=ifelse(is.waive(height), def_plot_dims[1], height),
                      "width"=ifelse(is.waive(width), def_plot_dims[2], width),
                      "units"=ifelse(is.waive(units), "cm", units)),
                    list(...)))
                
              } else {
                # Store as list for export.
                plot_list <- c(plot_list, list(p))
              }
            }
            
            # Generate output
            return(plotting.get_output(dir_path=dir_path,
                                       plot_list=plot_list,
                                       export_collection=export_collection,
                                       object=object))
          })



.plot_decision_curve_plot <- function(x,
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
                                      x_range,
                                      x_breaks,
                                      y_range,
                                      y_breaks,
                                      conf_int_style,
                                      conf_int_alpha){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  curve_type <- NULL
  
  # Generate a guide table.
  guide_list <- plotting.create_guide_table(x=x, color_by=color_by,
                                            discrete_palette=discrete_palette)
  
  # Extract data
  x <- guide_list$data
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x[curve_type == "model"],
                       mapping=ggplot2::aes(x=!!sym("threshold_probability"),
                                            y=!!sym("net_benefit")))
  
  # Add theme
  p <- p + ggtheme
  
  # Add decision curve, the curve for intervention for all, and the curve for no
  # intervention.
  if(is.null(color_by)){
    # Model-based curve without colour-based splitting.
    p <- p + ggplot2::geom_line()
    
    # Intervention for all.
    p <- p + ggplot2::geom_line(data=x[curve_type == "intervention_all"],
                                mapping=ggplot2::aes(x=!!sym("threshold_probability"),
                                                     y=!!sym("net_benefit")))
    
    # Intervention for none.
    p <- p + ggplot2::geom_hline(yintercept=0.0)
    
  } else {
    # Model-based curve with colour-based splitting.
    p <- p + ggplot2::geom_line(mapping=ggplot2::aes(colour=!!sym("color_breaks")))
    
    # Intervention for all.
    # Intervention for all.
    p <- p + ggplot2::geom_line(data=x[curve_type == "intervention_all"],
                                mapping=ggplot2::aes(x=!!sym("threshold_probability"),
                                                     y=!!sym("net_benefit"),
                                                     colour=!!sym("color_breaks")))
    
    # Intervention for none.
    p <- p + ggplot2::geom_hline(yintercept=0.0)
    
    # Extract guidetable for color
    g_color <- guide_list$guide_color
    
    # Set colour and fill (fill may be unused)
    p <- p + ggplot2::scale_colour_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          drop=FALSE)
    
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                        values=g_color$color_values,
                                        breaks=g_color$color_breaks,
                                        drop=FALSE)
  }
  
  # Plot confidence intervals
  if(conf_int_style[1]!="none"){
    if(conf_int_style[1] == "step"){
      if(is.null(color_by)){
        p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("ci_low")),
                                    linetype="dashed")
        
        p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("ci_up")),
                                    linetype="dashed")
        
      } else {
        p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("ci_low"),
                                                         colour=!!sym("color_breaks")),
                                    linetype="dashed")
        
        p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("ci_up"),
                                                         colour=!!sym("color_breaks")),
                                    linetype="dashed")
      }
      
      
      # Remove linetype from the legend.
      p <- p + ggplot2::scale_linetype(guide=FALSE)
      
    } else if(conf_int_style[1] == "ribbon"){
      if(is.null(color_by)){
        p <- p + ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=!!sym("ci_low"),
                                                           ymax=!!sym("ci_up")),
                                      alpha=conf_int_alpha)
        
      } else {
        p <- p + ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=!!sym("ci_low"),
                                                           ymax=!!sym("ci_up"),
                                                           fill=!!sym("color_breaks")),
                                      alpha=conf_int_alpha)
      }
    }
  }
  
  # Update x and y scales
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks)
  p <- p + ggplot2::scale_y_continuous(breaks=y_breaks)
  
  # Labels
  p <- p + ggplot2::labs(x=x_label,
                         y=y_label,
                         title=plot_title,
                         subtitle=plot_sub_title,
                         caption=caption)
  
  # Determine how things are faceted.
  facet_by_list <- plotting.parse_facet_by(x=x,
                                           facet_by=facet_by,
                                           facet_wrap_cols=facet_wrap_cols)
  
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
  
  # Prevent clipping of confidence intervals.
  p <- p + ggplot2::coord_cartesian(xlim=x_range, ylim=y_range)
  
  return(p)
}



.determine_decision_curve_plot_dimensions <- function(x,
                                                      facet_by,
                                                      facet_wrap_cols){
  
  # Obtain faceting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- 4
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}
