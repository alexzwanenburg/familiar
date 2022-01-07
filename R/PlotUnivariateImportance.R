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
#'@param p_adjustment_method (*optional*) Indicates type of p-value that is
#'  shown. One of `holm`, `hochberg`, `hommel`, `bonferroni`, `BH`, `BY`, `fdr`,
#'  `none`, `p_value` or `q_value` for adjusted p-values, uncorrected p-values
#'  and q-values. q-values may not be available.
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
#'@param verbose Flag to indicate whether feedback should be provided for the
#'  plotting.
#'@inheritParams export_univariate_analysis_data
#'@inheritParams export_feature_similarity
#'@inheritParams as_familiar_collection
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'@inheritDotParams extract_univariate_analysis -object -feature_cluster_method -feature_linkage_method -feature_cluster_cut_method -verbose
#'
#'@details This function generates a horizontal barplot with the length of the
#'  bars corresponding to the 10-logarithm of the (multiple-testing corrected)
#'  p-value or q-value.
#'
#'  Features are assessed univariately using one-sample location t-tests after
#'  fitting a suitable regression model. The fitted model coefficient and the
#'  covariance matrix are then used to compute a p-value.
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
#'  Labelling methods such as `set_fs_method_names` or `set_feature_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_univariate_importance
#'@md
#'@rdname plot_univariate_importance-methods
setGeneric("plot_univariate_importance",
           function(object,
                    feature_cluster_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_similarity_threshold=waiver(),
                    draw=FALSE,
                    dir_path=NULL,
                    p_adjustment_method=waiver(),
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
                    verbose=TRUE,
                    ...) standardGeneric("plot_univariate_importance"))

#####plot_univariate_importance (generic)#####

#'@rdname plot_univariate_importance-methods
setMethod("plot_univariate_importance", signature(object="ANY"),
          function(object,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   draw=FALSE,
                   dir_path=NULL,
                   p_adjustment_method=waiver(),
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
                   verbose=TRUE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="univariate_analysis",
                                          "feature_cluster_method"=feature_cluster_method,
                                          "feature_linkage_method"=feature_linkage_method,
                                          "feature_cluster_cut_method"=feature_cluster_cut_method,
                                          "feature_similarity_threshold"=feature_similarity_threshold,
                                          "verbose"=verbose),
                                     list(...)))
            
            return(do.call(plot_univariate_importance,
                           args=list("object"=object,
                                     "feature_cluster_method"=feature_cluster_method,
                                     "feature_linkage_method"=feature_linkage_method,
                                     "feature_cluster_cut_method"=feature_cluster_cut_method,
                                     "feature_similarity_threshold"=feature_similarity_threshold,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "p_adjustment_method"=p_adjustment_method,
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
                                     "units"=units,
                                     "verbose"=verbose)))
          })

#####plot_univariate_importance (collection)#####

#'@rdname plot_univariate_importance-methods
setMethod("plot_univariate_importance", signature(object="familiarCollection"),
          function(object,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   draw=FALSE,
                   dir_path=NULL,
                   p_adjustment_method=waiver(),
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
                   verbose=TRUE,
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            value <- .NATURAL <- NULL
            
            ##### Check data ########################################

            # Get input data.
            x <- export_univariate_analysis_data(object=object,
                                                 p_adjustment_method=p_adjustment_method)
            
            # Use only the univariate data.
            x <- x$univariate
            
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
            
            # Get feature similarity data.
            feature_similarity <- export_feature_similarity(object=object,
                                                            feature_cluster_method=feature_cluster_method,
                                                            feature_linkage_method=feature_linkage_method,
                                                            feature_cluster_cut_method=feature_cluster_cut_method,
                                                            feature_similarity_threshold=feature_similarity_threshold,
                                                            export_dendrogram=FALSE,
                                                            export_ordered_data=FALSE,
                                                            export_clustering=TRUE)[[1]]
            
            ##### Update data ##################################################
            
            # Set default adjust method.
            if(is.waive(p_adjustment_method)) p_adjustment_method <- "fdr"
            
            # Determine the metric column.
            column_data <- .plot_univariate_check_p_adjustment_method(method=p_adjustment_method,
                                                                      x=x@data,
                                                                      verbose=verbose)
            if(is.null(column_data)) return(NULL)
            
            # Check package requirements for plotting.
            if(!require_package(x=..required_plotting_packages(extended=FALSE),
                                purpose="to plot univariate variable importance",
                                message_type="warning")){
              return(NULL)
            }
            
            data.table::setnames(x@data, old=column_data$value_column, new="value")
            
            # Update p-values below the machine precision (i.e. p=0.0). 
            x@data[value <= 0.0, "value":=2 * .Machine$double.eps]
            
            # Convert p-value column to logarithmic scale
            x@data[, "log_value":=-log10(value)]
            
            # ggtheme
            if(!is(ggtheme, "theme")) ggtheme <- plotting.get_theme(use_theme=ggtheme)

            # significance_level_shown
            if(!is.null(significance_level_shown)){
              sapply(significance_level_shown, .check_number_in_valid_range,
                     var_name="significance_level_shown", range=c(0, 1), closed=c(FALSE, TRUE))
            }

            # x_range
            if(is.null(x_range) & is.null(significance_level_shown)){
              x_range <- c(0, max(x@data$log_value, na.rm=TRUE))
              
            } else if(is.null(x_range) & !is.null(significance_level_shown)){
              x_range <- c(0, max(c(max(x@data$log_value, na.rm=TRUE),
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
              label_name <- column_data$label
              
              x_label <- bquote(-log[10]*"("*.(label_name)*")")
            }
            
            # Check show_cluster
            .check_parameter_value_is_valid(show_cluster, var_name="show_cluster", values=c(FALSE, TRUE))
            
            # Clusters cannot be generated in case no cluster information is
            # present.
            if(is_empty(feature_similarity)){
              show_cluster <- FALSE
            }
            
            # Set default splitting variables if none are provided.
            if(is.null(split_by) & is.null(color_by) & is.null(facet_by)){
              split_by <- c("fs_method", "learner")
              color_by <- c("data_set")
            }
            
            # Check splitting variables and generate sanitised output.
            split_var_list <- plotting.check_data_handling(x=x@data,
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
              x_split <- split(x@data, by=split_by)
            } else {
              x_split <- list(x@data)
            }
            
            # Store plots to list in case no dir_path is provided
            if(is.null(dir_path)) plot_list <- list()
            
            # Iterate over splits
            for(x_sub in x_split){
              
              if(is_empty(x_sub)) next()
              
              # Join cluster and univariate data.
              if(show_cluster){
                # Join on common columns. These are feature plus any column in
                # split_by.
                x_temporary <- feature_similarity@data[x_sub, on=.NATURAL]
                
                # Check that the resulting data is not empty, because this would
                # mean that there is e.g. only one feature.
                if(is_empty(x_temporary)){
                  x_temporary <- data.table::copy(x_sub)
                  x_temporary[, ":="("cluster_id"=.I,
                                     "cluster_size"=1L)]
                }
                
                # Replace x_sub
                x_sub <- x_temporary
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
              if(is.null(p)) next()
              
              # Draw plot
              if(draw) plotting.draw(plot_or_grob=p)
              
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
                plot_list <- c(plot_list, list(p))
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
  x$feature <- droplevels(x$feature)
  x <- x[order(log_value)]
  x$feature <- factor(x$feature, levels=unique(x$feature))

  # Generate a guide table
  guide_list <- plotting.create_guide_table(x=x,
                                            color_by=color_by,
                                            discrete_palette=discrete_palette)
  
  # Extract data
  x <- guide_list$data

  # Check if cluster information should be shown.
  if(show_cluster) x <- plotting.add_cluster_name(x=x,
                                                  color_by=color_by,
                                                  facet_by=facet_by)
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x,
                       mapping=ggplot2::aes(x=!!sym("feature"), y=!!sym("log_value")))
  p <- p + ggplot2::coord_flip()
  p <- p + ggtheme
  
  # Add fill colors
  if(!is.null(color_by)){
    
    # Extract guide_table for color
    g_color <- guide_list$guide_color
    
    p <- p + ggplot2::geom_bar(stat="identity",
                               mapping=ggplot2::aes(fill=!!sym("color_breaks")),
                               position="dodge")
    
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                        values=g_color$color_values,
                                        breaks=g_color$color_breaks,
                                        drop=FALSE)
    
  } else if(!is.waive(gradient_palette)) {
    # Coloring by log of the p-value
    p <- p + ggplot2::geom_bar(stat="identity",
                               mapping=ggplot2::aes(fill=!!sym("log_value")),
                               show.legend=FALSE)
    p <- p + ggplot2::scale_fill_gradientn(colors=plotting.get_palette(x=gradient_palette,
                                                                       palette_type="sequential"),
                                           limits=x_range)
    
  } else {
    # No colouring of the bars.
    p <- p + ggplot2::geom_bar(stat="identity")
  }

  # Set breaks and limits
  p <- p + ggplot2::scale_y_continuous(breaks=x_breaks,
                                       limits=x_range)
  
  # Determine how things are facetted
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
      p <- p + ggplot2::geom_hline(yintercept=-log10(curr_signif_level),
                                   linetype="dotted")
    }
  }
  
  # Update labels. Note that the inversion of x_label and y_label is correct, as the coordinates were flipped
  p <- p + ggplot2::labs(x=y_label,
                         y=x_label,
                         title=plot_title, 
                         subtitle=plot_sub_title,
                         caption=caption)

  return(p)
}



.determine_univariate_importance_plot_dimensions <- function(x, facet_by, facet_wrap_cols){
  
  # Get plot layout dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x,
                                             facet_by=facet_by,
                                             facet_wrap_cols=facet_wrap_cols)

  # Determine the number of features within each facet.
  n_features <- data.table::uniqueN(x=x$feature)
  longest_name <- max(sapply(levels(x$feature), nchar))
  
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



.plot_univariate_check_p_adjustment_method <- function(x, method, verbose=TRUE){
  
  .check_parameter_value_is_valid(x=method,
                                  var_name="p_adjustment_method",
                                  values=c(stats::p.adjust.methods, "p_value", "q_value"))
  
  value_column <- switch(method,
                         "holm" = "adjusted_p_value",
                         "hochberg" = "adjusted_p_value",
                         "hommel" = "adjusted_p_value",
                         "bonferroni" = "adjusted_p_value",
                         "BH" = "adjusted_p_value",
                         "BY" = "adjusted_p_value",
                         "fdr" = "adjusted_p_value",
                         "none" = "p_value",
                         "p_value" = "p_value",
                         "q_value" = "q_value")
  
  label_name <- switch(method,
                         "holm" = "Holm corrected p-value",
                         "hochberg" = "Hochberg corrected p-value",
                         "hommel" = "Hommel corrected p-value",
                         "bonferroni" = "Bonferroni corrected p-value",
                         "BH" = "Benjamini-Hochberg corrected p-value",
                         "BY" = "Benjamini-Yekutieli corrected p-value",
                         "fdr" = "FDR-corrected p-value",
                         "none" = "p-value",
                         "p_value" = "p-value",
                         "q_value" = "q-value")
  
  
  # Check if the column is present.
  if(is.null(x[[value_column]])){
    stop(paste0("Univariate importance can not be plotted as the values for the requested p adjustment method (",
                value_column, ") were not found in the data."))
  }
  
  # Check if any values are valid.
  if(all(!is.finite(x[[value_column]]))){
    if(verbose){
      warning("Univariate importance can not be plotted as all values are NA.")
    }
    
    return(NULL)
  }
  
  return(list("value_column"=value_column,
              "label"=label_name))
}
