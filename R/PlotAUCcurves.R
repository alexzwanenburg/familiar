#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot the receiver operating characteristic curve.
#'
#'@description This method creates receiver operating characteristic curves
#'  based on data in a familiarCollection object.
#'
#'@param dir_path (*optional*) Path to the directory where created performance
#'  plots are saved to. Output is saved in the `performance` subdirectory. If
#'  `NULL` no figures are saved, but are returned instead.
#'@param discrete_palette (*optional*) Palette to use to color the different
#'  plot elements in case a value was provided to the `color_by` argument.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates area under the ROC curve plots.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `pos_class`. By default, the data is split by `fs_method` and `learner`,
#'  with facetting by `data_set` and colouring by `pos_class`.
#'
#'  Available palettes for `discrete_palette` are those listed by
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
#'@exportMethod plot_auc_roc_curve
#'@md
#'@rdname plot_auc_roc_curve-methods
setGeneric("plot_auc_roc_curve",
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
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_auc_roc_curve"))

#####plot_auc_roc_curve (generic)#####

#'@rdname plot_auc_roc_curve-methods
setMethod("plot_auc_roc_curve", signature(object="ANY"),
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
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="auc_data"), list(...)))
            
            return(do.call(plot_auc_roc_curve,
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
                                     "x_n_breaks"=x_n_breaks,
                                     "x_breaks"=x_breaks,
                                     "y_n_breaks"=y_n_breaks,
                                     "y_breaks"=y_breaks,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })


#####plot_auc_roc_curve (collection)#####

#'@rdname plot_auc_roc_curve-methods
setMethod("plot_auc_roc_curve", signature(object="familiarCollection"),
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
           plot_title=NULL,
           plot_sub_title=NULL,
           caption=NULL,
           x_n_breaks=5,
           x_breaks=NULL,
           y_n_breaks=5,
           y_breaks=NULL,
           conf_int_style=c("ribbon", "step", "none"),
           conf_int_alpha=0.4,
           width=waiver(),
           height=waiver(),
           units=waiver(),
           ...){
    
    # Get input data.
    x <- export_auc_data(object=object)
    
    # Check for empty data
    if(is.null(x)) return(NULL)
    if(is_empty(x$ensemble)) return(NULL)
                                    
    # Extract the data for the ensemble.
    x <- x$ensemble
    
    ##### Check input arguments ------------------------------------------------
    
    # ggtheme
    if(!inherits(ggtheme, "theme")){
      ggtheme <- plotting.get_theme(use_theme=ggtheme)
    }
    
    # x_label
    if(is.waive(x_label)){
      x_label <- "1 - specificity"
    }
    
    # y_label
    if(is.waive(y_label)){
      y_label <- "sensitivity"
    }
    
    # x_range and y_range
    x_range <- y_range <- c(0.0, 1.0)
    
    # x_breaks
    if(is.null(x_breaks)){
      plotting.check_input_args(x_n_breaks=x_n_breaks)
      
      # Create breaks
      x_breaks <- labeling::extended(m=x_n_breaks,
                                     dmin=x_range[1],
                                     dmax=x_range[2],
                                     only.loose=TRUE)
    }
    
    # y_breaks
    if(is.null(y_breaks)){
      plotting.check_input_args(y_n_breaks=y_n_breaks)
      
      # Create breaks
      y_breaks <- labeling::extended(m=y_n_breaks,
                                     dmin=y_range[1],
                                     dmax=y_range[2],
                                     only.loose=TRUE)
    }
    
    # conf_int_style
    if(length(conf_int_style) > 1) conf_int_style <- head(conf_int_style, n=1)
    
    # Splitting variables
    if(is.null(split_by) & is.null(facet_by) & is.null(color_by)){
      # Determine the number of learners and feature_selection methods.
      n_learner <- nlevels(x$learner)
      n_fs_method <- nlevels(x$fs_method)
      n_pos_class <- nlevels(x$pos_class)
      
      if(n_learner > 1 & n_fs_method > 1){
        # 
        split_by <- c("fs_method", "learner")
        
        if(n_pos_class > 1){
          color_by <- "pos_class"
          facet_by <- "data_set"
          
        } else {
          color_by <- c("data_set", "pos_class")
        }
        
      } else if(n_learner > 1){
        # Implying n_fs_method == 1
        
        if(n_pos_class > 1){
          split_by <- c("fs_method", "learner")
          color_by <- "pos_class"
          facet_by <- "data_set"
          
        } else {
          split_by <- c("fs_method")
          color_by <- c("learner")
          facet_by <- c("data_set", "pos_class")
        }
        
      } else if(n_fs_method > 1){
        # Implying n_learner == 1
        
        if(n_pos_class > 1){
          split_by <- c("fs_method", "learner")
          color_by <- "pos_class"
          facet_by <- "data_set"
          
        } else {
          split_by <- "learner"
          color_by <- "fs_method"
          facet_by <- c("data_set", "pos_class")
        }
        
      } else {
        # Implying n_learner == n_fs_method == 1
        split_by <- c("fs_method", "learner")
        
        if(n_pos_class > 1){
          color_by <- "pos_class"
          facet_by <- "data_set"
          
        } else {
          color_by <- c("data_set", "pos_class")
        }
      }
    }
    
    # Check splitting variables and generate sanitised output
    split_var_list <- plotting.check_data_handling(x=x,
                                                   split_by=split_by,
                                                   color_by=color_by,
                                                   facet_by=facet_by,
                                                   available=c("fs_method", "learner", "data_set", "pos_class"))
    
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
    
    # Split data
    if(!is.null(split_by)){
      x_split <- split(x, by=split_by)
      
    } else {
      x_split <- list("null.name"=x)
    }
    
    # Store plots to list in case dir_path is absent.
    if(is.null(dir_path)) plot_list <- list()
    
    # Iterate over splits
    for(ii in names(x_split)){
      
      # Skip empty datasets
      if(is_empty(x_split[[ii]])) next()
      
      # Generate plot
      p <- .plot_auc_curve_plot(x=x_split[[ii]],
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
        
        subtype <- "auc_roc"
        
        # Determine the subtype
        if(!is.null(split_by)){
          subtype <- c(subtype, as.character(sapply(split_by, function(jj, x) (x[[jj]][1]), x=x_split[[ii]])))
          subtype <- paste0(subtype, collapse="_")
        }
        
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_auc_roc_plot_dimensions(x=x_split[[ii]],
                                                            facet_by=facet_by,
                                                            facet_wrap_cols=facet_wrap_cols)
        
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



.plot_auc_curve_plot <- function(x,
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
  
  # Generate a guide table.
  guide_list <- plotting.create_guide_table(x=x, color_by=color_by,
                                            discrete_palette=discrete_palette)
  
  # Extract data
  x <- guide_list$data
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("x"),
                                                    y=!!sym("y")))
  
  # Add theme
  p <- p + ggtheme
  
  # Add AUC curve
  if(is.null(color_by)){
    # Without colour-based splitting.
    p <- p + ggplot2::geom_line()
    
  } else {
    # With colour-based splitting.
    p <- p + ggplot2::geom_line(mapping=ggplot2::aes(colour=!!sym("color_breaks")))
    
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
      p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("conf_int_lower"), linetype="dashed", na.rm=TRUE))
      p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("conf_int_upper"), linetype="dashed", na.rm=TRUE))
      
    } else if(conf_int_style[1] == "ribbon"){
      
      p <- p + ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=!!sym("conf_int_lower"),
                                                         ymax=!!sym("conf_int_upper"),
                                                         fill=!!sym("color_breaks")),
                                    alpha=conf_int_alpha,
                                    na.rm=TRUE)
      
      # # Create special data for ribbon so that it becomes a step ribbon.
      # x_ribbon <- data.table::rbindlist(lapply(split(x, by=c("color_breaks", facet_by)), .prepare_auc_conf_int_plot_data))
      # 
      # p <- p + ggplot2::geom_ribbon(data=x_ribbon,
      #                               mapping=ggplot2::aes(x=!!sym("x"), ymin=!!sym("conf_int_lower"), ymax=!!sym("conf_int_upper"),
      #                                                    fill=!!sym("color_breaks")), alpha=conf_int_alpha, na.rm=TRUE)
    }
  }
  
  # Update x and y scales
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks, limits=x_range)
  p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
  
  # Labels
  p <- p + ggplot2::labs(x=x_label, y=y_label, title=plot_title, subtitle=plot_sub_title, caption=caption)
  
  # Determine how things are facetted
  facet_by_list <- plotting.parse_facet_by(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  if(!is.null(facet_by)){
    if(is.null(facet_wrap_cols)){
      # Use a grid
      p <- p + ggplot2::facet_grid(rows=facet_by_list$facet_rows, cols=facet_by_list$facet_cols, labeller="label_context", drop=TRUE)
    } else {
      p <- p + ggplot2::facet_wrap(facets=facet_by_list$facet_by, labeller="label_context", drop=TRUE)
    }
  }
  
  return(p)
}



.determine_auc_roc_plot_dimensions <- function(x,
                                               facet_by,
                                               facet_wrap_cols){
  
  # Obtain facetting dimensions
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
  


.prepare_auc_conf_int_plot_data <- function(data){
  # Suppress NOTES due to non-standard evaluation in data.table
  x <- conf_int_lower <- conf_int_upper <- NULL
  
  if(is_empty(data)){ return(data) }
  
  # Make sure that the surv_lower and surv_upper coordinate sets are correct.
  data_1 <- data.table::copy(data)[order(x)]
  data_2 <- data.table::copy(data)[1:nrow(data)-1]
  data_2[, "x":=data_1$x[2:nrow(data)]]
  
  # Combine and order correctly.
  x <- rbind(data_1, data_2)[order(x, -conf_int_lower, -conf_int_upper)]
  
  return(x)
}
