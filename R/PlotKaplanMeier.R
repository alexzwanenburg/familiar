#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot Kaplan-Meier survival curves.
#'
#'@description This function creates Kaplan-Meier survival curves from
#'  stratification data stored in a familiarCollection object.
#'
#'@param dir_path (*optional*) Path to the directory where created figures are
#'  saved to. Output is saved in the `stratification` subdirectory. If `NULL` no
#'  figures are saved, but are returned instead.
#'@param discrete_palette (*optional*) Palette to use to color the different
#'  risk strata in case a non-singular variable was provided to the `color_by`
#'  argument.
#'@param censoring (*optional*) Flag to indicate whether censored samples should
#'  be indicated on the survival curve.
#'@param censor_shape (*optional*) Shape used to indicate censored samples on
#'  the survival curve. Available shapes are documented in the `ggplot2`
#'  vignette *Aesthetic specifications*. By default a plus shape is used.
#'@param show_logrank (*optional*) Specifies whether the results of a logrank
#'  test to assess differences between the risk strata is annotated in the plot.
#'  A log-rank test can only be shown when `color_by` and `linestyle_by` are
#'  either unset, or only contain `risk_group`.
#'@param show_survival_table (*optional*) Specifies whether a survival table is
#'  shown below the Kaplan-Meier survival curves. Survival in the risk strata is
#'  assessed for each of the breaks in `x_breaks`.
#'@param height (*optional*) Height of the plot. A default value is derived from
#'  number of facets and the inclusion of survival tables.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates a Kaplan-Meier survival plot based on risk
#'  group stratification by the learners.
#'
#'  `familiar` does not determine what units the x-axis has or what kind of
#'  survival the y-axis represents. It is therefore recommended to provide
#'  `x_label` and `y_label` arguments.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `risk_group`. By default, separate figures are created for each combination
#'  of `fs_method` and `learner`, with facetting by `data_set`, colouring of the
#'  strata in each individual plot by `risk_group`.
#'  
#'  Available palettes for `discrete_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'  
#'  Labeling methods such as `set_risk_group_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#'@exportMethod plot_kaplan_meier
#'@md
#'@rdname plot_kaplan_meier-methods
setGeneric("plot_kaplan_meier",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    linetype_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    combine_legend=TRUE,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    x_label="time",
                    x_label_shared="column",
                    y_label="survival probability",
                    y_label_shared="row",
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    y_range=c(0, 1),
                    y_n_breaks=5,
                    y_breaks=NULL,
                    conf_int=0.95,
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    censoring=TRUE,
                    censor_shape="plus",
                    show_logrank=TRUE,
                    show_survival_table=TRUE,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_kaplan_meier"))


#####plot_kaplan_meier (generic)#####

#'@rdname plot_kaplan_meier-methods
setMethod("plot_kaplan_meier", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   linetype_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   combine_legend=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   x_label="time",
                   x_label_shared="column",
                   y_label="survival probability",
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=c(0, 1),
                   y_n_breaks=5,
                   y_breaks=NULL,
                   conf_int=0.95,
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   censoring=TRUE,
                   censor_shape="plus",
                   show_logrank=TRUE,
                   show_survival_table=TRUE,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="kaplan_meier_data"), list(...)))
            
            return(do.call(plot_kaplan_meier,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "split_by"=split_by,
                                     "color_by"=color_by,
                                     "linetype_by"=linetype_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "combine_legend"=combine_legend,
                                     "ggtheme"=ggtheme,
                                     "discrete_palette"=discrete_palette,
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
                                     "conf_int"=conf_int,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "censoring"=censoring,
                                     "censor_shape"=censor_shape,
                                     "show_logrank"=show_logrank,
                                     "show_survival_table"=show_survival_table,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })

#####plot_kaplan_meier (collection)#####

#'@rdname plot_kaplan_meier-methods
setMethod("plot_kaplan_meier", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   linetype_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   combine_legend=TRUE,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   x_label="time",
                   x_label_shared="column",
                   y_label="survival probability",
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=c(0, 1),
                   y_n_breaks=5,
                   y_breaks=NULL,
                   conf_int=0.95,
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   censoring=TRUE,
                   censor_shape="plus",
                   show_logrank=TRUE,
                   show_survival_table=TRUE,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Get input data
            x <- export_stratification_data(object=object)
            
            # Check empty input
            # TODO return a warning if outcome_type is survival or competing_risk
            if(is.null(x)){ return(NULL) }
            if(length(x$data) == 0) { return(NULL) }
            
            ##### Check input arguments ########################################
            
            # ggtheme
            if(!any(class(ggtheme) == "theme")) {
              ggtheme <- plotting.get_theme(use_theme=ggtheme)
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
            
            # x_range
            if(is.null(x_range)) { x_range <- c(0, x$time_max) }
            
            # x_breaks
            if(is.null(x_breaks)){
              plotting.check_input_args(x_n_breaks=x_n_breaks)
              
              # Create breaks and update x_range
              x_breaks <- labeling::extended(m=x_n_breaks,
                                             dmin=x_range[1],
                                             dmax=x_range[2],
                                             only.loose=TRUE)
              x_range  <- c(0, tail(x_breaks, n=1))
            }
            
            # y_range
            if(is.null(y_range)){ y_range <- c(0, 1) }
            
            # y_breaks
            if(is.null(y_breaks)){
              plotting.check_input_args(y_n_breaks=y_n_breaks)
              
              # Create breaks and update y_range
              y_breaks <- labeling::extended(m=y_n_breaks, dmin=y_range[1], dmax=y_range[2])
              y_range <- c(0, tail(y_breaks, n=1))
            }
            
            # conf_int
            if(is.null(conf_int)) { conf_int <- 0.0 }
            
            # conf_int_style
            if(length(conf_int_style) > 1){ conf_int_style <- head(conf_int_style, n=1) }
            
            # Store plots to list in case no dir_path is provided
            if(is.null(dir_path)){
              plot_list <- list()
            }
            
            # Iterate over the stratification methods
            for(current_strat_method in names(x$data)){
              
              # Extract data
              x_strat <- x$data[[current_strat_method]]
              
              # Add default splitting variables.
              if(is.null(split_by) & is.null(color_by) & is.null(linetype_by) & is.null(facet_by)){
                split_by <- c("fs_method", "learner")
                color_by <- c("risk_group")
                facet_by <- c("data_set")
              }
              
              # Check splitting variables and generate sanitised output
              split_var_list <- plotting.check_data_handling(x=x_strat,
                                                             split_by=split_by,
                                                             color_by=color_by,
                                                             linetype_by=linetype_by,
                                                             facet_by=facet_by,
                                                             available=c("fs_method", "learner", "data_set", "risk_group"))
              
              # Update splitting variables
              split_by_strat <- split_var_list$split_by
              color_by_strat <- split_var_list$color_by
              facet_by_strat <- split_var_list$facet_by
              linetype_by_strat <- split_var_list$linetype_by
              
              # Create a legend label
              legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                           color_by=color_by_strat,
                                                           linetype_by=linetype_by_strat,
                                                           combine_legend=combine_legend)
              
              # Set show_logrank to FALSE in case color_by and linetype_by
              # contain more than risk_group at most.
              if(!is.null(union(color_by_strat, linetype_by_strat))){
                if(!all(union(color_by_strat, linetype_by_strat) == "risk_group")){
                  show_logrank <- FALSE
                }
              }
              
              # Check input arguments for validity.
              plotting.check_input_args(x_range=x_range,
                                        y_range=y_range,
                                        x_breaks=x_breaks,
                                        y_breaks=y_breaks,
                                        conf_int=conf_int,
                                        conf_int_alpha=conf_int_alpha,
                                        conf_int_style=conf_int_style,
                                        facet_wrap_cols=facet_wrap_cols,
                                        x_label=x_label,
                                        y_label=y_label,
                                        legend_label=legend_label,
                                        plot_title=plot_title,
                                        plot_sub_title=plot_sub_title,
                                        caption=caption)
              
              ##### Create plots ###############################################
 
              # Split data
              if(!is.null(split_by_strat)){
                x_split <- split(x_strat, by=split_by_strat)
              } else {
                x_split <- list(x_strat)
              }
              
              # Create a sub_plot_list for all plots that are generated for the
              # current stratification method.
              if(is.null(dir_path)){
                sub_plot_list <- list()
              }
              
              # Iterate over splits
              for(x_sub in x_split){
                
                if(is_empty(x_sub)){
                  next()
                }
                
                # Create plot
                p <- .plot_kaplan_meier(x=x_sub,
                                        color_by=color_by_strat,
                                        linetype_by=linetype_by_strat,
                                        facet_by=facet_by_strat,
                                        facet_wrap_cols=facet_wrap_cols,
                                        combine_legend=combine_legend,
                                        ggtheme=ggtheme,
                                        discrete_palette=discrete_palette,
                                        x_label=x_label,
                                        x_label_shared=x_label_shared,
                                        y_label=y_label,
                                        y_label_shared=y_label_shared,
                                        legend_label=legend_label,
                                        plot_title=plot_title,
                                        plot_sub_title=plot_sub_title,
                                        caption=caption,
                                        x_range=x_range,
                                        x_breaks=x_breaks,
                                        y_range=y_range,
                                        y_breaks=y_breaks,
                                        conf_int=conf_int,
                                        conf_int_style=conf_int_style,
                                        conf_int_alpha=conf_int_alpha,
                                        censoring=censoring,
                                        censor_shape=censor_shape,
                                        show_logrank=show_logrank,
                                        show_survival_table=show_survival_table)
                
                # Check empty output
                if(is.null(p)){ next() }

                # Draw plot
                if(draw){ plotting.draw(plot_or_grob=p) }
                
                # Save and export
                if(!is.null(dir_path)){
                  # Save to file
                  if(!is.null(split_by)){
                    subtype <- paste0("km_", current_strat_method, "_", paste0(sapply(split_by, function(ii, x) (x[[ii]][1]), x=x_sub), collapse="_"))
                  } else {
                    subtype <- paste0("km_", current_strat_method)
                  }
                
                  # Obtain default plot dimensions
                  def_plot_dims <- .determine_km_plot_default_dimensions(x=x_sub, facet_by=facet_by,
                                                                         facet_wrap_cols=facet_wrap_cols,
                                                                         show_survival_table=show_survival_table)

                  # Save to file.
                  do.call(plotting.save_plot_to_file,
                          args=append(list("plot_obj"=p,
                                           "object"=object,
                                           "dir_path"=dir_path,
                                           "type"="stratification",
                                           "subtype"=subtype,
                                           "height"=ifelse(is.waive(height), def_plot_dims[1], height),
                                           "width"=ifelse(is.waive(width), def_plot_dims[2], width),
                                           "units"=ifelse(is.waive(units), "cm", units)),
                                      list(...)))
                  
                } else {
                  # Store as list
                  sub_plot_list <- append(sub_plot_list, list(p))
                }
              }
              
              # Add sub_plot_list to a list entry for the current stratification method
              if(is.null(dir_path)){
                plot_list[[current_strat_method]] <- sub_plot_list
              }
            }
            
            # Return
            if(is.null(dir_path)){
              return(plot_list)
            } else {
              return(NULL)
            }
          })



.plot_kaplan_meier <- function(x,
                               color_by, linetype_by, facet_by, facet_wrap_cols,
                               combine_legend,
                               ggtheme, discrete_palette,
                               x_label, x_label_shared,
                               y_label, y_label_shared,
                               legend_label, plot_title, plot_sub_title, caption,
                               x_range, x_breaks,
                               y_range, y_breaks,
                               conf_int, conf_int_style, conf_int_alpha,
                               censoring, censor_shape,
                               show_logrank, show_survival_table){

  # Prepare the data for plotting
  km_data <- lapply(split(x, by=unique(c(color_by, linetype_by, facet_by))), .prepare_km_plot_data, conf_int=conf_int, x_range=x_range)
  km_data <- data.table::rbindlist(km_data)
  
  # Define elements that need to be shared. Note that "guide" and "strip_y" may
  # be absent.
  elements <- c("guide", "strip_y")
  if(x_label_shared == "overall") { elements <- append(elements, "axis_title_x")}
  if(y_label_shared == "overall") { elements <- append(elements, "axis_title_y")}

  # Split by facet. This generates a list of data splits with facetting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(x=km_data, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols,
                                                      x_label_shared=x_label_shared, y_label_shared=y_label_shared)
  
  # Split data into facets. This is done by row.
  data_split_list <- plotting.split_data_by_facet(x=x, plot_layout_table=plot_layout_table)
  km_split_list <- plotting.split_data_by_facet(x=km_data, plot_layout_table=plot_layout_table)

  # Create plots to join
  figure_list <- list()
  extracted_element_list <- list()
  for(ii in seq_along(km_split_list)){
    
    # Compute logrank test value
    if(show_logrank){
      h <- learner.perform_log_rank_test(risk_group_table=data_split_list[[ii]], all_groups=TRUE)
    } else {
      h <- NULL
    }
    
    # Kaplan-Meier plots
    p_kaplan_meier <- .create_km_subplot(x=km_split_list[[ii]],
                                         h=h,
                                         color_by=color_by,
                                         linetype_by=linetype_by,
                                         facet_by=facet_by,
                                         facet_wrap_cols=facet_wrap_cols,
                                         combine_legend=combine_legend,
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
                                         conf_int=conf_int,
                                         conf_int_style=conf_int_style,
                                         conf_int_alpha=conf_int_alpha,
                                         censoring=censoring,
                                         censor_shape=censor_shape,
                                         show_logrank=show_logrank)
    
    # Update theme to remove guide, facet labels, based on notations in data set x
    p_kaplan_meier <- plotting.update_facet_plot_elements(p=p_kaplan_meier, x=km_split_list[[ii]])
    
    # Extract plot elements from kaplan-meier plot.
    extracted_elements <- plotting.extract_plot_elements(p=p_kaplan_meier, elements=elements)
    
    # Remove extracted elements from the plot.
    p_kaplan_meier <- plotting.remove_plot_elements(p=p_kaplan_meier, elements=elements)
    
    # Convert to grob
    g_kaplan_meier <- plotting.to_grob(p_kaplan_meier)
    
    if(show_survival_table){
      # Survival tables
      p_survival_table <- .create_survival_table_subplot(x=km_split_list[[ii]],
                                                         color_by=color_by,
                                                         linetype_by=linetype_by,
                                                         ggtheme=ggtheme,
                                                         discrete_palette=discrete_palette,
                                                         x_range=x_range,
                                                         x_breaks=x_breaks)
      
      # Update theme to remove guide, facet labels, based on notations in data set x
      p_survival_table <- plotting.update_facet_plot_elements(p=p_survival_table, x=km_split_list[[ii]])

      # Extract survival gtable, which consists of the panel and the left axis.
      g_survival_table <- .gtable_extract(g=plotting.to_grob(p_survival_table),
                                          element=c("panel", "axis-l"),
                                          partial_match=TRUE)
      
      # Insert survival table into the kaplan-meier table. Use partial matching
      # to match elements from g_survival_table with those in g_kaplan_meier.
      g_kaplan_meier <- .gtable_insert(g=g_kaplan_meier,
                                       g_new=g_survival_table,
                                       where="bottom",
                                       ref_element="xlab-b",
                                       partial_match=TRUE)
      
    }
    
    # Re-introduce plot elements
    g_kaplan_meier <- plotting.reinsert_plot_elements(g=g_kaplan_meier,
                                                      elements="strip_y",
                                                      grob_list=extracted_elements,
                                                      ggtheme=ggtheme)
    
    # Add combined grob to list
    figure_list <- append(figure_list, list(g_kaplan_meier))
    
    # Add extract elements to the grob_element_list
    extracted_element_list <- .append_new(extracted_element_list, extracted_elements)
  }
  
  # Obtain layout dimensions (rows, cols).
  layout_dims <- plotting.get_plot_layout_dims(plot_layout_table=plot_layout_table)
  
  # Combine features.
  g <- plotting.arrange_figures(grobs=figure_list,
                                n_rows=layout_dims[1],
                                n_cols=layout_dims[2],
                                elements=setdiff(elements, "strip_y"),
                                element_grobs=extracted_element_list,
                                ggtheme=ggtheme)
  
  return(g)
}


.prepare_km_plot_data <- function(x, conf_int, x_range){

  # Suppress NOTES due to non-standard evaluation in data.table
  surv <- surv_lower <- surv_upper <- time <- NULL
  
  if(is_empty(x)){ return(NULL) }
  
  # Use survfit to get kaplan-meier curves for each group
  if(conf_int > 0){
    km_fit <- survival::survfit(Surv(outcome_time, outcome_event) ~ 1, data=x, conf.int=conf_int)
  } else {
    km_fit <- survival::survfit(Surv(outcome_time, outcome_event) ~ 1, data=x)
  }
  
  # Extract plotting information
  km_data <- data.table::data.table("time"=km_fit$time, "group_size"=km_fit$n.risk, "n_event"=km_fit$n.event, "n_censor"=km_fit$n.censor,
                                    "surv"=km_fit$surv, "surv_lower"=km_fit$lower,  "surv_upper"=km_fit$upper)
  
  # Add in time = 0 if necessary
  if(min(km_data$time) > 0){
    km_data <- rbind(data.table::data.table("time"=0, "group_size"=km_fit$n, "n_event"=0, "n_censor"=0,
                                            "surv"=1.00, "surv_lower"=km_fit$lower[1], "surv_upper"=1.0),
                     km_data)
  }
  
  # Update absent censoring (notably when surv==0)
  km_data[surv==0 & is.na(surv_lower), "surv_lower":=0.0]
  km_data[surv==0 & is.na(surv_upper), "surv_upper":=0.0]
  
  # In rare circumstances (i.e. single-sample risk groups), surv_lower may be
  # missing at the initial time point.
  km_data[time==0 & is.na(surv_lower), "surv_lower":=0.0]
  
  # Add an entry at the proximal and distal range edges. This prevents the curve
  # from being cut off prematurely, or starting too late.
  if(is_empty(km_data[time==x_range[1]]) & x_range[1] > min(km_data$time) & x_range[1] < max(km_data$time)){
    # Select the closest entry prior to the proximal edge, make changes and
    # introduce it back into the data.
    proximal_data <- tail(km_data[time < x_range[1]][order(time)], n=1)[, ":="("time"=x_range[1], "n_event"=0, "n_censor"=0)]
    km_data <- rbind(km_data, proximal_data)[order(time)]
  }
  
  if(is_empty(km_data[time==x_range[2]]) & x_range[2] > min(km_data$time) & x_range[2] < max(km_data$time)){
    # Do the same for the distal edge.
    distal_data <- tail(km_data[time < x_range[2]][order(time)], n=1)[, ":="("time"=x_range[2], "n_event"=0, "n_censor"=0)]
    km_data <- rbind(km_data, distal_data)[order(time)]
  }

  # Add in data_set, fs_method, learner and risk_group and maintain factor
  # levels If one of these variables has more than one category present, this
  # indicates that this level is ignored.
  if(data.table::uniqueN(x, by="data_set") == 1){
    km_data$data_set   <- factor(x$data_set[1], levels=levels(x$data_set))
  }
  
  if(data.table::uniqueN(x, by="fs_method") == 1){
    km_data$fs_method  <- factor(x$fs_method[1], levels=levels(x$fs_method))
  }

  if(data.table::uniqueN(x, by="learner") == 1){
    km_data$learner    <- factor(x$learner[1], levels=levels(x$learner))
  }
  
  if(data.table::uniqueN(x, by="risk_group") == 1){
    km_data$risk_group <- factor(x$risk_group[1], levels=levels(x$risk_group))
  }
  
  return(km_data)
}



.create_km_subplot <- function(x, h,
                               color_by, linetype_by, facet_by, facet_wrap_cols,
                               combine_legend,
                               ggtheme, discrete_palette,
                               x_label, y_label, legend_label, plot_title, plot_sub_title, caption,
                               x_range, x_breaks,
                               y_range, y_breaks,
                               conf_int, conf_int_style, conf_int_alpha,
                               censoring, censor_shape,
                               show_logrank){

  # Suppress NOTES due to non-standard evaluation in data.table
  n_censor <- NULL
  
  # Generate a guide table to allow integration of guides into a single legend
  guide_list <- plotting.create_guide_table(x=x, color_by=color_by, linetype_by=linetype_by,
                                            discrete_palette=discrete_palette,
                                            combine_legend=combine_legend)
  # Extract data
  x <- guide_list$data
  
  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("time"), y=!!sym("surv")))
  p <- p + ggtheme

  # Create step function
  if(is.null(color_by) & is.null(linetype_by)){
    p <- p + ggplot2::geom_step()
  } else if(!is.null(color_by) & is.null(linetype_by)){
    p <- p + ggplot2::geom_step(mapping=ggplot2::aes(colour=!!sym("color_breaks")))
  } else if(is.null(color_by) & !is.null(linetype_by)){
    p <- p + ggplot2::geom_step(mapping=ggplot2::aes(linetype=!!sym("linetype_breaks")))
  } else {
    p <- p + ggplot2::geom_step(mapping=ggplot2::aes(colour=!!sym("color_breaks"), linetype=!!sym("linetype_breaks")))
  }
  
  # Set colour
  if(!is.null(color_by)){
    # Extract guide_table for color
    g_color <- guide_list$guide_color
    
    p <- p + ggplot2::scale_colour_manual(name=legend_label$guide_color,
                                          values=g_color$color_values,
                                          breaks=g_color$color_breaks,
                                          drop=FALSE)
    
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                        values=g_color$color_values,
                                        breaks=g_color$color_breaks,
                                        drop=FALSE)
  }
  
  # Set line style
  if(!is.null(linetype_by)){
    # Extract guide_table for linetype
    g_linetype <- guide_list$guide_linetype
    
    p <- p + ggplot2::scale_linetype_manual(name=legend_label$guide_linetype,
                                            values=g_linetype$linetype_values,
                                            breaks=g_linetype$linetype_breaks,
                                            drop=FALSE)
  }
  
  if(show_logrank & !is_empty(h)){
    # Parse p-value
    p_value_label <- paste0("p: ", as.character(signif(h$p_value, 2)))
    
    # Obtain default settings.
    text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
    
    # Show in plot
    p <- p + ggplot2::annotate("text",
                               x=x_range[1],
                               y=y_range[1],
                               label=p_value_label,
                               colour=text_settings$colour,
                               family=text_settings$family,
                               fontface=text_settings$face,
                               size=text_settings$geom_text_size,
                               vjust="inward",
                               hjust="inward")
  }
  
  # Plot confidence intervals
  if(is.null(conf_int)) { conf_int <- 0.0 }
  
  if(conf_int > 0.0 & conf_int_style[1]!="none"){
    if(conf_int_style[1] == "step"){
      p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("surv_lower"), linetype="dashed", na.rm=TRUE))
      p <- p + ggplot2::geom_step(mapping=ggplot2::aes(y=!!sym("surv_upper"), linetype="dashed", na.rm=TRUE))
    } else if(conf_int_style[1] == "ribbon"){
      
      # Create special data for ribbon so that it becomes a step ribbon.
      x_ribbon <- data.table::rbindlist(lapply(split(x, by="color_breaks"), .prepare_km_conf_int_plot_data))
      
      p <- p + ggplot2::geom_ribbon(data=x_ribbon,
                                    mapping=ggplot2::aes(x=!!sym("time"), ymin=!!sym("surv_lower"), ymax=!!sym("surv_upper"),
                                                         fill=!!sym("color_breaks")), alpha=conf_int_alpha, na.rm=TRUE)
    }
  }
  
  # Censoring indicators
  if(censoring){
    p <- p + ggplot2::geom_point(data=x[n_censor>0],
                                 mapping=ggplot2::aes(colour=!!sym("color_breaks"),fill=!!sym("color_breaks")),
                                 shape=censor_shape,
                                 show.legend=FALSE)
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


.create_survival_table_subplot <- function(x, color_by, linetype_by, ggtheme,
                                           discrete_palette, x_range, x_breaks){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  time <- id <- missing_entry <- surv <- NULL
  
  # Parse the table to obtain group sizes at x_breaks
  survival_list <- lapply(split(x, by=c(color_by, linetype_by)), function(y, x_breaks){
    
    # Iterate over the breaks to obtain the group size at this time point.
    survival_list <- lapply(x_breaks, function(break_time, y){
      
      # Find the nearest candidate entry
      candidate_entry <- data.table::copy(tail(y[time <= break_time, ], n=1))
      
      # In case the number of survivors is 0, the group size is also 0.
      candidate_entry[surv==0, "group_size":=0]
      
      return(candidate_entry)
    }, y=y)
    
    # Convert to a new data.table
    survival_table <- data.table::rbindlist(survival_list)
    
    # Update time to x_breaks
    survival_table[, "time":=x_breaks]

    return(survival_table)
    
  }, x_breaks=x_breaks)
  
  # Combine into list
  survival_table <- data.table::rbindlist(survival_list)
  
  # Find the names for the groups
  if(is.null(c(color_by, linetype_by))){
    survival_table[, ":="("group_name"="")]
  } else {
    unique_vars <- unique(c(color_by, linetype_by))
    
    # Generate a guide table
    guide_table <- data.table::data.table(expand.grid(lapply(rev(unique_vars), function(ii, x) (levels(x[[ii]])), x=survival_table)))
    
    # Rename variables
    data.table::setnames(x=guide_table, rev(unique_vars))
    
    # Convert to factors
    for(ii in unique_vars){
      guide_table[[ii]] <- factor(guide_table[[ii]], levels=levels(x[[ii]]))
    }
    
    # Order columns according to unique_vars
    data.table::setcolorder(x=guide_table, neworder=unique_vars)
    
    # Order data set by columns
    data.table::setorderv(x=guide_table, cols=unique_vars)
    
    # Set breaks
    breaks <- apply(guide_table, 1, paste, collapse=", ")

    # Update guide_table
    guide_table$group_name <- factor(breaks, levels=breaks)
    
    # Identify missing entries in the guide table. First add an identifier to
    # the guide-table, then determine which entries are empty.
    guide_table[, "id":=.I]
    guide_table$missing_entry <- sapply(split(guide_table, by="id"),
                                        function(x, y, by) (is_empty(merge(x=x, y=y, by=by, all=FALSE))),
                                        y=survival_table, by=unique_vars)

    if(any(guide_table$missing_entry)){
      # Find a suitable prototype and then create a prototype table.
      available_prototype <- head(guide_table[missing_entry==FALSE]$id, 1)
      proto_table <- merge(x=survival_table, y=guide_table[id==available_prototype, c(unique_vars), with=FALSE])
      
      # Iterate over missing entries.
      new_survival_list <- lapply(split(guide_table[missing_entry==TRUE], by="id"), function(x, proto_table, unique_vars){
        
        # Make a copy of the prototype and replace the group size by 0.
        y <- data.table::copy(proto_table)[, "group_size":=0]
        
        # Insert the correct values for each plotting variable that was
        # previously missing.
        for(var in unique_vars){
          y[, (var):=x[[var]]]
        }
        
        return(y)
        
      }, proto_table=proto_table, unique_vars)
      
      # Add the placeholder entries to the table.
      survival_table <- rbindlist(c(list(survival_table), new_survival_list), use.names=TRUE)
      
      # Remove superfluous columns from guide_table
      guide_table[, ":="("id"=NULL, "missing_entry"=NULL)]
    }
    
    # Combine the guide table with the survival table to add in the group names.
    survival_table <- merge(x=survival_table, y=guide_table, by=unique_vars)
  }

  # Obtain default settings.
  text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
  fontsize <- text_settings$fontsize
  fontsize_rel <- text_settings$fontsize_rel
  lineheight <- text_settings$lineheight
  
  # Create plot
  p <- ggplot2::ggplot(data=survival_table, mapping=ggplot2::aes(x=!!sym("time"), y=!!sym("group_name"), label=!!sym("group_size")))
  
  # Annotate survival in strata.
  p <- p + ggplot2::geom_text(colour=text_settings$colour,
                              family=text_settings$family,
                              fontface=text_settings$face,
                              size=text_settings$geom_text_size)
  
  # Adapt axes.
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks, limits=x_range)
  p <- p + ggplot2::scale_y_discrete(breaks=rev(levels(survival_table$group_name)), limits=rev(levels(survival_table$group_name)))
  
  # Set main theme
  p <- p + ggtheme
  
  # Remove some theme elements and reduce margins
  p <- p + ggplot2::theme(panel.grid=ggplot2::element_blank(),
                          panel.border=ggplot2::element_blank(),
                          axis.line = ggplot2::element_blank(),
                          axis.ticks.y=ggplot2::element_blank(),
                          axis.text.x=ggplot2::element_blank(),
                          axis.ticks.x=ggplot2::element_blank(),
                          axis.title.x=ggplot2::element_blank(),
                          axis.title.y=ggplot2::element_blank())

  # Convert to gtable
  g <- ggplot2::ggplotGrob(p)
  
  # Set the height on the panel element to n * fontsize + (n-1) * lineheight
  n_lines <- data.table::uniqueN(survival_table$group_name)
  grob_height <- n_lines * fontsize * fontsize_rel + (n_lines - 1) * fontsize * fontsize_rel * lineheight + 4
  
  # Find the panel
  panel_row_t <- g$layout[g$layout$name == "panel", "t"]
  
  # Set heights of the row
  g$heights[panel_row_t] <- grid::unit(x=grob_height, "pt")
  
  # Set to p
  p$custom_grob <- list("heights"=list("name"="panel", "height"=grid::unit(grob_height, "pt")))
  class(p) <- c("familiar_ggplot", class(p))
  
  return(p)
}


.determine_km_plot_default_dimensions <- function(x, facet_by, facet_wrap_cols, show_survival_table){
  
  # Obtain facetting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- ifelse(show_survival_table, 6, 4)
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}


.prepare_km_conf_int_plot_data <- function(x){
  # Suppress NOTES due to non-standard evaluation in data.table
  time <- surv_lower <- surv_upper <- NULL
  
  if(is_empty(x)){ return(x) }
  
  # Make sure that the surv_lower and surv_upper coordinate sets are correct.
  x <- data.table::copy(x)[order(time)]
  y <- data.table::copy(x)[1:nrow(x)-1]
  y[, "time":=x$time[2:nrow(x)]]

  # Combine and order correctly.
  x <- rbind(x, y)[order(time, -surv_lower, -surv_upper)]
  
  return(x)
}
