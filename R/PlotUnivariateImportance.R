#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot univariate importance.
#'
#'@description This function plots the univariate analysis data stored in a
#'  familiarCollection object.
#'
#'@param draw (*optional*) Draws the plot if TRUE.
#'@param dir_path (*optional*) Path to the directory where created figures are
#'  saved to. Output is saved in the `variable_importance` subdirectory. If NULL
#'  no figures are saved, but are returned instead.
#'@param importance_metric (*optional*) Indicates type of p-value that is shown.
#'  One of `fdr`, `p_value` or `q_value` for FDR-corrected p-values, uncorrected
#'  p-values and q-values respectively. q-values may not be available.
#'@param show_cluster (*optional*) Show which features were clustered together.
#'@param ggtheme (*optional*) `ggplot` theme to use for plotting.
#'@param discrete_palette (*optional*) Palette used to fill the bars in case a
#'  non-singular variable was provided to the `color_by` argument.
#'@param gradient_palette (*optional*) Palette to use for filling the bars in
#'  case the `color_by` argument is not set. The bars are then coloured
#'  according to their importance. By default, no gradient is used, and the bars
#'  are not filled according to importance. Use `NULL` to fill the bars using
#'  the default palette in `familiar`.
#'@param significance_level_shown Position(s) to draw vertical lines indicating
#'  a significance level, e.g. 0.05. Can be NULL to not draw anything.
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
#'
#'@details This function generates a horizontal barplot with the length of the
#'  bars corresponding to the 10-logarithm of the (multiple-testing corrected)
#'  p-value or q-value.
#'
#'  The following splitting variables are available for `split_by`, `color_by`
#'  and `facet_by`:
#'
#'  * `fs_method`: feature selection methods
#'
#'  * `learner`: learners
#'
#'  * `data_set`: data sets
#'
#'  Unlike for plots of feature ranking in feature selection and after modelling
#'  (as assessed by model-specific routines), clusters of features are now found
#'  during creation of underlying `familiarData` objects, instead of through
#'  consensus clustering. Hence, clustering results may differ due to
#'  differences in the underlying datasets.
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
#'  Labeling methods such as `set_fs_method_names` or `set_feature_names` can be
#'  applied to the `familiarCollection` object to update labels, and order the
#'  output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_univariate_importance
#'@md
#'@rdname plot_univariate_importance-methods
setGeneric("plot_univariate_importance",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    importance_metric=c("fdr", "p_value", "q_value"),
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    show_cluster=TRUE,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=waiver(),
                    x_label=waiver(),
                    y_label="feature",
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    significance_level_shown=0.05,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_univariate_importance"))

#####plot_univariate_importance (generic)#####

#'@rdname plot_univariate_importance-methods
setMethod("plot_univariate_importance", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   importance_metric=c("fdr", "p_value", "q_value"),
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label=waiver(),
                   y_label="feature",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   significance_level_shown=0.05,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="univariate_analysis"), list(...)))
            
            return(do.call(plot_univariate_importance,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "importance_metric"=importance_metric,
                                     "split_by"=split_by,
                                     "color_by"=color_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "show_cluster"=show_cluster,
                                     "ggtheme"=ggtheme,
                                     "discrete_palette"=discrete_palette,
                                     "gradient_palette"=gradient_palette,
                                     "x_label"=x_label,
                                     "y_label"=y_label,
                                     "legend_label"=legend_label,
                                     "plot_title"=plot_title,
                                     "plot_sub_title"=plot_sub_title,
                                     "caption"=caption,
                                     "x_range"=x_range,
                                     "x_n_breaks"=x_n_breaks,
                                     "x_breaks"=x_breaks,
                                     "significance_level_shown"=significance_level_shown,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })

#####plot_univariate_importance (collection)#####

#'@rdname plot_univariate_importance-methods
setMethod("plot_univariate_importance", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   importance_metric=c("fdr", "p_value", "q_value"),
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   show_cluster=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=waiver(),
                   x_label=waiver(),
                   y_label="feature",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   significance_level_shown=0.05,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            ##### Check input arguments ########################################

            # Get input data.
            x <- export_univariate_analysis_data(object=object)

            # Determine the metric column.
            metric_col <- .plot_univariate_check_importance_metric(metric=importance_metric, x=x)
            if(is.null(metric_col)){ return(NULL) }
            
            # Update p-values below the machine precision (i.e. p=0.0). 
            x[eval(parse(text=metric_col)) <= 0.0, (metric_col):=2 * .Machine$double.eps]
            
            # Convert p-value column to logarithmic scale
            x[, "log_value":=-log10(get(metric_col))]
            
            # ggtheme
            if(!is(ggtheme, "theme")) {
              ggtheme <- plotting.get_theme(use_theme=ggtheme)
            }

            # significance_level_shown
            if(!is.null(significance_level_shown)){
              sapply(significance_level_shown, .check_number_in_valid_range,
                     var_name="significance_level_shown", range=c(0, 1), closed=c(FALSE, TRUE))
            }

            # x_range
            if(is.null(x_range) & is.null(significance_level_shown)){
              x_range <- c(0, max(x$log_value, na.rm=TRUE))
              
            } else if(is.null(x_range) & !is.null(significance_level_shown)){
              x_range <- c(0, max(c(max(x$log_value, na.rm=TRUE),
                                    max(-log10(significance_level_shown)))))
            }
            
            # x_breaks
            if(is.null(x_breaks)){
              plotting.check_input_args(x_range=x_range, x_n_breaks=x_n_breaks)
              
              # Create breaks and update x_range
              x_breaks <- labeling::extended(m=x_n_breaks,
                                             dmin=x_range[1],
                                             dmax=x_range[2],
                                             only.loose=TRUE)
              x_range  <- c(0, tail(x_breaks, n=1))
            }
            
            # x_label Set default name for x-axis label
            if(is.waive(x_label)){
              label_name <- switch(importance_metric[1],
                                   "fdr" = "FDR-corrected p-value",
                                   "p_value" = "p-value",
                                   "q_value" = "q-value")
              
              x_label <- bquote(-log[10]*"("*.(label_name)*")")
            }

            # Set default splitting variables if none are provided.
            if(is.null(split_by) & is.null(color_by) & is.null(facet_by)){
              split_by <- c("fs_method", "learner")
              color_by <- c("data_set")
            }
            
            # Check splitting variables and generate sanitised output.
            split_var_list <- plotting.check_data_handling(x=x,
                                                           split_by=split_by,
                                                           color_by=color_by,
                                                           facet_by=facet_by,
                                                           available=c("fs_method", "learner", "data_set"))
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            # Parse legend label
            legend_label <- plotting.create_legend_label(user_label=legend_label, color_by=color_by)
            
            # Check general input arguments
            plotting.check_input_args(x_range=x_range,
                                      x_breaks=x_breaks,
                                      x_label=x_label,
                                      y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption,
                                      facet_wrap_cols=facet_wrap_cols)  
            
            
            ##### Create plots #################################################
            
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
              p <- .plot_univariate_importance(x=x_sub,
                                               color_by=color_by,
                                               facet_by=facet_by,
                                               facet_wrap_cols=facet_wrap_cols,
                                               ggtheme=ggtheme,
                                               show_cluster=show_cluster,
                                               discrete_palette=discrete_palette,
                                               gradient_palette=gradient_palette,
                                               x_label=x_label,
                                               y_label=y_label,
                                               legend_label=legend_label,
                                               plot_title=plot_title,
                                               plot_sub_title=plot_sub_title,
                                               caption=caption,
                                               x_range=x_range,
                                               x_breaks=x_breaks,
                                               significance_level_shown=significance_level_shown)
              
              # Check empty output
              if(is.null(p)){ next() }
              
              # Draw plot
              if(draw){ plotting.draw(plot_or_grob=p) }
              
              # Save and export
              if(!is.null(dir_path)){
                # Save to file
                if(!is.null(split_by)){
                  subtype <- paste0("univariate_", paste0(sapply(split_by, function(ii, x) (x[[ii]][1]), x=x_sub), collapse="_"))
                } else {
                  subtype <- "univariate"
                }
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_univariate_importance_plot_dimensions(x=x_sub,
                                                                                  facet_by=facet_by,
                                                                                  facet_wrap_cols=facet_wrap_cols)

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
          })



#' Internal plotting function for univariate plots
#'
#' @inheritParams plot_univariate_importance
#'
#' @return ggplot plot object.
#' 
#' @seealso 
#' * \code{\link{plot_univariate_importance}} for the user interface.
#' @md
#' @keywords internal
.plot_univariate_importance <- function(x,
                                        color_by,
                                        facet_by,
                                        facet_wrap_cols,
                                        ggtheme,
                                        show_cluster,
                                        discrete_palette,
                                        gradient_palette,
                                        x_label,
                                        y_label,
                                        legend_label,
                                        plot_title,
                                        plot_sub_title,
                                        caption,
                                        x_range,
                                        x_breaks,
                                        significance_level_shown){

  # Suppress NOTES due to non-standard evaluation in data.table
  log_value <- NULL
  
  # Create local copy
  x <- data.table::copy(x)

  # Drop levels and reorder table so that features are sorted by the log-values
  x$name <- droplevels(x$name)
  x      <- x[order(log_value)]
  x$name <- factor(x$name, levels=unique(x$name))

  # Generate a guide table
  guide_list <- plotting.create_guide_table(x=x, color_by=color_by, discrete_palette=discrete_palette)
  
  # Extract data
  x <- guide_list$data

  # Check if cluster information should be shown.
  if(show_cluster){
    x <- plotting.add_cluster_name(x=x, color_by=color_by, facet_by=facet_by)
  }
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("name"), y=!!sym("log_value")))
  p <- p + ggplot2::coord_flip()
  p <- p + ggtheme
  
  # Add fill colors
  if(!is.null(color_by)){
    
    # Extract guide_table for color
    g_color <- guide_list$guide_color
    
    p <- p + ggplot2::geom_bar(stat="identity", mapping=ggplot2::aes(fill=!!sym("color_breaks")), position="dodge")
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                        values=g_color$color_values,
                                        breaks=g_color$color_breaks,
                                        drop=FALSE)
    
  } else if(!is.waive(gradient_palette)) {
    # Coloring by log of the p-value
    p <- p + ggplot2::geom_bar(stat="identity", mapping=ggplot2::aes(fill=!!sym("log_value")), show.legend=FALSE)
    p <- p + ggplot2::scale_fill_gradientn(colors=plotting.get_palette(x=gradient_palette, palette_type="sequential"),
                                           limits=x_range)
    
  } else {
    # No colouring of the bars.
    p <- p + ggplot2::geom_bar(stat="identity")
  }

  # Set breaks and limits
  p <- p + ggplot2::scale_y_continuous(breaks=x_breaks, limits=x_range)
  
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
  
  # Show clusters
  if(show_cluster){
    
    # Obtain default settings.
    text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
    
    if(is.null(color_by)){
      p <- p + ggplot2::geom_text(ggplot2::aes(label=!!sym("cluster_name"), y=0.0, angle=270.0),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size,
                                  vjust=-0.50)
      
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label=!!sym("cluster_name"), y=0.0, group=!!sym("color_breaks"), angle=270.0),
                                  colour=text_settings$colour,
                                  family=text_settings$family,
                                  fontface=text_settings$face,
                                  size=text_settings$geom_text_size,
                                  vjust=-0.50,
                                  position=ggplot2::position_dodge(width=0.9))
    }
  }
  
  # Add vertical lines for significance levels.
  if(!is.null(significance_level_shown)){
    for(curr_signif_level in significance_level_shown){
      p <- p + ggplot2::geom_hline(yintercept=-log10(curr_signif_level), linetype="dotted")
    }
  }
  
  # Update labels. Note that the inversion of x_label and y_label is correct, as the coordinates were flipped
  p <- p + ggplot2::labs(x=y_label, y=x_label, title=plot_title, subtitle=plot_sub_title, caption=caption)

  return(p)
}


.determine_univariate_importance_plot_dimensions <- function(x, facet_by, facet_wrap_cols){
  
  # Get plot layout dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)

  # Determine the number of features within each facet.
  n_features <- data.table::uniqueN(x=x$name)
  longest_name <- max(sapply(levels(x$name), nchar))
  
  # Assume each feature takes up about 14 points (~5mm) with 2 point (0.7mm) spacing. Then add some room for other plot elements.
  default_height <- n_features * 0.5 + (n_features - 1) * 0.07 + 1.0

  # Set a default height. Assume that the typical width of a character is about 5 points (1.8mm).
  default_width <- 6 + longest_name * 0.18
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}



.plot_univariate_check_importance_metric <- function(x, metric){
  
  .check_parameter_value_is_valid(x=metric[1], var_name="importance_metric", values=c("fdr", "p_value", "q_value"))
  
  metric_col <- switch(metric[1],
                       "fdr" = "p_value_corrected",
                       "p_value" = "p_value",
                       "q_value" = "q_value")
  
  # Check if the column is present.
  if(is.null(x[[metric_col]])){
    warning(paste0("Univariate importance can not be plotted as the requested metric (",
                   metric[1], ") was not found in the data."))
    
    return(NULL)
  }
  
  # Check if any values are valid.
  if(all(is.na(x[[metric_col]]))){
    warning("Univariate importance can not be plotted as all values are NA.")
    
    return(NULL)
  }
  
  return(metric_col)
}
