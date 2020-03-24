#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot calibration figures.
#'
#'@description This method creates calibration plots from calibration data
#'  stored in a familiarCollection object. For this figures, the expected
#'  (predicted) values are plotted against the observed values. A
#'  well-calibrated model should be close to the identity line.
#'
#'@param dir_path (*optional*) Path to the directory where created calibration
#'  plots are saved to. Output is saved in the `calibration` subdirectory. If
#'  `NULL` no figures are saved, but are returned instead.
#'@param discrete_palette (*optional*) Palette to use to color the different
#'  data points and fit lines in case a non-singular variable was provided to
#'  the `color_by` argument.
#'@param show_density (*optional*) Show point density in top margin of the
#'  figure. If `color_by` is set, this information will not be shown.
#'@param show_calibration_fit (*optional*) Specifies whether the calibration in
#'  the large and calibration slope are annotated in the plot. If `color_by` is
#'  set, this information will not be shown.
#'@param show_goodness_of_fit (*optional*) Specifies whether a the results of
#'  goodness of fit tests are annotated in the plot. If `color_by` is set, this
#'  information will not be shown.
#'@param density_plot_height (*optional*) Height of the density plot. The height
#'  is 1.5 cm by default. Height is expected to be grid unit (see `grid::unit`),
#'  which also allows for specifying relative heights. Will be ignored if
#'  `show_density` is `FALSE`.
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams as_familiar_collection -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates a calibration plot for each model in each
#'  dataset. Any data used for calibration (e.g. baseline survival) is obtained
#'  during model creation.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `evaluation_time` (survival analysis only) and `pos_class` (multinomial
#'  endpoints only). By default, separate figures are created for each
#'  combination of `fs_method` and `learner`, with facetting by `data_set`.
#'
#'  Calibration in survival analysis is performed at set time points so that
#'  survival probabilities can be computed from the model, and compared with
#'  observed survival probabilities. This is done differently depending on the
#'  underlying model. For Cox partial hazards regression models, the base
#'  survival (of the development samples) are used, whereas accelerated failure
#'  time models (e.g. Weibull) and survival random forests can be used to
#'  directly predict survival probabilities at a given time point. For survival
#'  analysis, `evaluation_time` is an additional facet variable (by default).
#'
#'  Calibration for multinomial endpoints is performed in a one-against-all
#'  manner. This yields calibration information for each individual class of the
#'  endpoint. For such endpoints, `pos_class` is an additional facet variable
#'  (by default).
#'
#'  Calibration plots have a density plot in the margin, which shows the density
#'  of the plotted points, ordered by the expected probability or value. For
#'  binomial and multinomial outcomes, the density for positive and negative
#'  classes are shown separately. Note that this information is only provided in
#'  when `color_by` is not used as a splitting variable (i.e. one calibration
#'  plot per facet).
#'
#'  Calibration plots are annotated with the intercept and the slope of a linear
#'  model fitted to the sample points. A well-calibrated model has an intercept
#'  close to 0.0 and a slope of 1.0. Intercept and slope are shown with their
#'  respective 95% confidence intervals. In addition, goodness-of-fit tests may
#'  be shown. For most endpoints these are based on the Hosmer-Lemeshow (HL)
#'  test, but for survival endpoints both the Nam-D'Agostino (ND) and the
#'  Greenwood-Nam-D'Agostino (GND) tests are shown. Note that this information
#'  is only annotated when `color_by` is not used as a splitting variable (i.e.
#'  one calibration plot per facet).
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
#'@references 1. Hosmer, D. W., Hosmer, T., Le Cessie, S. & Lemeshow, S. A
#'  comparison of goodness-of-fit tests for the logistic regression model. Stat.
#'  Med. 16, 965–980 (1997).
#'
#'  1. D’Agostino, R. B. & Nam, B.-H. Evaluation of the Performance of Survival
#'  Analysis Models: Discrimination and Calibration Measures. in Handbook of
#'  Statistics vol. 23 1–25 (Elsevier, 2003).
#'
#'  1. Demler, O. V., Paynter, N. P. & Cook, N. R. Tests of calibration and
#'  goodness-of-fit in the survival setting. Stat. Med. 34, 1659–1680 (2015).
#'
#'@exportMethod plot_calibration_data
#'@md
#'@rdname plot_calibration_data-methods
setGeneric("plot_calibration_data",
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
                    x_label_shared="column",
                    y_label=waiver(),
                    y_label_shared="row",
                    legend_label=waiver(),
                    plot_title=NULL,
                    plot_sub_title=NULL,
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    show_density=TRUE,
                    show_calibration_fit=TRUE,
                    show_goodness_of_fit=TRUE,
                    density_plot_height=grid::unit(1.5, "cm"),
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_calibration_data"))


#####plot_calibration_data (generic)#####

#'@rdname plot_calibration_data-methods
setMethod("plot_calibration_data", signature(object="ANY"),
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
                   x_label_shared="column",
                   y_label=waiver(),
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   show_density=TRUE,
                   show_calibration_fit=TRUE,
                   show_goodness_of_fit=TRUE,
                   density_plot_height=grid::unit(1.5, "cm"),
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="calibration_data"), list(...)))
            
            return(do.call(plot_calibration_data,
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
                                     "show_density"=show_density,
                                     "show_calibration_fit"=show_calibration_fit,
                                     "show_goodness_of_fit"=show_goodness_of_fit,
                                     "density_plot_height"=density_plot_height,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })

#####plot_calibration_data (collection)#####

#'@rdname plot_calibration_data-methods
setMethod("plot_calibration_data", signature(object="familiarCollection"),
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
                   x_label_shared="column",
                   y_label=waiver(),
                   y_label_shared="row",
                   legend_label=waiver(),
                   plot_title=NULL,
                   plot_sub_title=NULL,
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   show_density=TRUE,
                   show_calibration_fit=TRUE,
                   show_goodness_of_fit=TRUE,
                   density_plot_height=grid::unit(1.5, "cm"),
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Get input data
            x <- export_calibration_data(object=object)
            
            # Check empty input
            if(is.null(x)){ return(NULL) }
            if(is_empty(x$data)) { return(NULL) }
            
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
            if(is.null(x_range)){
              x_range <- c(0, 1) 
            }
            
            # x_breaks
            if(is.null(x_breaks)){
              plotting.check_input_args(x_range=x_range,
                                        x_n_breaks=x_n_breaks)
              
              # Create breaks and update x_range
              x_breaks <- labeling::extended(m=x_n_breaks,
                                             dmin=x_range[1],
                                             dmax=x_range[2],
                                             only.loose=TRUE)
              
              x_range  <- c(head(x_breaks, n=1), tail(x_breaks, n=1))
            }
            
            # y_range
            if(is.null(y_range)){ y_range <- x_range }
            
            # y_breaks
            if(is.null(y_breaks)){
              plotting.check_input_args(y_range=y_range,
                                        y_n_breaks=y_n_breaks)
              
              # Create breaks and update y_range
              y_breaks <- labeling::extended(m=y_n_breaks,
                                             dmin=y_range[1],
                                             dmax=y_range[2],
                                             only.loose=TRUE)
              
              y_range <- c(head(y_breaks, n=1), tail(y_breaks, n=1))
            }
            
            # x_label
            if(is.waive(x_label)){
              if(object@outcome_type %in% c("binomial", "multinomial")){
                x_label <- "expected probability"
              } else if(object@outcome_type %in% c("count", "continuous")){
                x_label <- "expected value"
              } else if(object@outcome_type %in% c("survival")){
                x_label <- "expected survival probability"
              } else {
                ..error_no_known_outcome_type(object@outcome_type)
              }
            }
            
            # y_label
            if(is.waive(y_label)){
              if(object@outcome_type %in% c("binomial", "multinomial")){
                y_label <- "observed proportion"
              } else if(object@outcome_type %in% c("count", "continuous")){
                y_label <- "observed value"
              } else if(object@outcome_type %in% c("survival")){
                y_label <- "observed survival proportion"
              } else {
                ..error_no_known_outcome_type(object@outcome_type)
              }
            }

            
            # Add default splitting variables.
            if(is.null(split_by) & is.null(color_by) & is.null(facet_by)){
              split_by <- c("fs_method", "learner")
              
              # Set facetting variables
              facet_by <- c("data_set")
              
              # Add "evaluation_time" as a facetting variable in case of
              # survival analysis.
              if(object@outcome_type %in% c("survival")){
                facet_by <- append(facet_by, "evaluation_time")
                
              } else if(
                object@outcome_type %in% c("multinomial")){
                facet_by <- append(facet_by, "pos_class")
              }
            }
            
            # Obtain available splitting variables. This differ per type of
            # endpoint studied. For survival outcomes, evaluation time is an
            # additional splitting point. For multinomial outcomes, the positive
            # class indicator is an additional splitting variable.
            available_splitting_vars <- c("fs_method", "learner", "data_set")
            if(object@outcome_type %in% c("survival")){
              available_splitting_vars <- append(available_splitting_vars, "evaluation_time")
              
            } else if(object@outcome_type %in% c("multinomial")){
              available_splitting_vars <- append(available_splitting_vars, "pos_class")
            }
            
            # Check splitting variables and generate sanitised output
            split_var_list <- plotting.check_data_handling(x=x$data,
                                                           split_by=split_by,
                                                           color_by=color_by,
                                                           facet_by=facet_by,
                                                           available=available_splitting_vars)
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            # Create a legend label
            legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                         color_by=color_by)
            
            # Update show_density, show_goodness_of_fit and show_calibration_fit
            # variables so that they are FALSE in case color_by is set.
            if(!is.null(color_by)){
              show_calibration_fit <- FALSE
              show_goodness_of_fit <- FALSE
              show_density <- FALSE
            }
            
            # Check input for show_* arguments.
            .check_parameter_value_is_valid(x=show_calibration_fit, var_name="show_calibration_fit", values=c(FALSE, TRUE))
            .check_parameter_value_is_valid(x=show_goodness_of_fit, var_name="show_goodness_of_fit", values=c(FALSE, TRUE))
            .check_parameter_value_is_valid(x=show_density, var_name="show_density", values=c(FALSE, TRUE))
            
            # Check density_plot_height
            plotting.check_grid_unit(x=density_plot_height, var_name="density_plot_height")
            
            # Check input arguments for validity.
            plotting.check_input_args(x_range=x_range,
                                      y_range=y_range,
                                      x_breaks=x_breaks,
                                      y_breaks=y_breaks,
                                      facet_wrap_cols=facet_wrap_cols,
                                      x_label=x_label,
                                      y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption)
            
            ##### Create plots ###############################################
            
            # Split data and supporting data.
            if(!is.null(split_by)){
              x_split <- split(x$data, by=split_by, drop=FALSE)
              linear_test_split <- split(x$linear_test, by=split_by, drop=FALSE)
              gof_test_split <- split(x$gof_test, by=split_by, drop=FALSE)
              
            } else {
              x_split <- list("null.name"=x$data)
              linear_test_split <- list("null.name"=x$linear_test)
              gof_test_split <- list("null.name"=x$gof_test)
            }
            
            # Store plots to list in case no dir_path is provided
            if(is.null(dir_path)){
              plot_list <- list()
            }
            
            # Iterate over splits
            for(ii in names(x_split)){
              
              if(is_empty(x_split[[ii]])){
                next()
              }
              
              # Generate plot
              p <- .plot_calibration_plot(x=x_split[[ii]],
                                          color_by=color_by,
                                          facet_by=facet_by,
                                          facet_wrap_cols=facet_wrap_cols,
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
                                          show_density=show_density,
                                          show_calibration_fit=show_calibration_fit,
                                          show_goodness_of_fit=show_goodness_of_fit,
                                          density_plot_height=density_plot_height,
                                          linear_test=linear_test_split[[ii]],
                                          gof_test=gof_test_split[[ii]],
                                          outcome_type=object@outcome_type)
              
              # Check empty output
              if(is.null(p)){ next() }
              
              # Draw plot
              if(draw){ plotting.draw(plot_or_grob=p) }
              
              # Save and export
              if(!is.null(dir_path)){
                
                # Determine the subtype
                if(!is.null(split_by)){
                  subtype <- paste0(sapply(split_by, function(jj, x) (x[[jj]][1]), x=x_split[[ii]]), collapse="_")
                } else {
                  subtype <- NULL
                }
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_calibration_plot_dimensions(x=x_split[[ii]],
                                                                        facet_by=facet_by,
                                                                        facet_wrap_cols=facet_wrap_cols,
                                                                        show_density=show_density)
                
                # Save to file.
                do.call(plotting.save_plot_to_file,
                        args=append(list("plot_obj"=p,
                                         "object"=object,
                                         "dir_path"=dir_path,
                                         "type"="calibration",
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



.plot_calibration_plot <- function(x,
                                   color_by,
                                   facet_by,
                                   facet_wrap_cols,
                                   ggtheme,
                                   discrete_palette,
                                   x_label,
                                   x_label_shared,
                                   y_label,
                                   y_label_shared,
                                   legend_label,
                                   plot_title,
                                   plot_sub_title,
                                   caption,
                                   x_range,
                                   x_breaks,
                                   y_range,
                                   y_breaks,
                                   show_density,
                                   show_calibration_fit,
                                   show_goodness_of_fit,
                                   density_plot_height,
                                   linear_test,
                                   gof_test,
                                   outcome_type){
  
  # Define elements that need to be shared. Note that "guide" and "strip_y" may
  # be absent.
  elements <- c("guide", "strip_y")
  if(x_label_shared == "overall") { elements <- append(elements, "axis_title_x")}
  if(y_label_shared == "overall") { elements <- append(elements, "axis_title_y")}
  
  # Split by facet. This generates a list of data splits with facetting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols,
                                                      x_label_shared=x_label_shared, y_label_shared=y_label_shared)
  
  # Split data into facets. This is done by row.
  data_facet_list <- plotting.split_data_by_facet(x=x, plot_layout_table=plot_layout_table)
  linear_test_facet_list <- plotting.split_data_by_facet(x=linear_test, plot_layout_table=plot_layout_table)
  gof_test_facet_list <- plotting.split_data_by_facet(x=gof_test, plot_layout_table=plot_layout_table)
  
  # Placeholders for plots.
  figure_list <- list()
  extracted_element_list <- list()
  
  # Iterate over facets
  for(ii in names(data_facet_list)){
    
    # Create calibration plot.
    p_calibration <- .create_calibration_plot(x=data_facet_list[[ii]],
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
                                              show_calibration_fit=show_calibration_fit,
                                              show_goodness_of_fit=show_goodness_of_fit,
                                              linear_test=linear_test_facet_list[[ii]],
                                              gof_test=gof_test_facet_list[[ii]],
                                              outcome_type=outcome_type)
    
    # Update theme to remove guide, facet labels, etc., based on notations in
    # dataset x.
    p_calibration <- plotting.update_facet_plot_elements(p=p_calibration, x=data_facet_list[[ii]])
    
    # Extract plot elements from the calibration plot.
    extracted_elements <- plotting.extract_plot_elements(p=p_calibration, elements=elements)
    
    # Remove extracted elements from the plot.
    p_calibration <- plotting.remove_plot_elements(p=p_calibration, elements=elements)
    
    # Convert to grob.
    g_calibration <- plotting.to_grob(p_calibration)
    
    if(show_density){
      
      if(outcome_type %in% c("binomial", "multinomial")){
        # Procedure for binomial/multinomial outcomes. Separate density plots
        # are created for the positive and negative classes.
        
        # Density of the positive class.
        p_margin_positive <- .create_calibration_density_subplot(x=data_facet_list[[ii]],
                                                                 ggtheme=ggtheme,
                                                                 x_range=x_range,
                                                                 x_breaks=x_breaks,
                                                                 flip=FALSE,
                                                                 plot_height=density_plot_height,
                                                                 outcome_type=outcome_type)
        
        # Density of the negative class.
        p_margin_negative <- .create_calibration_density_subplot(x=data_facet_list[[ii]],
                                                                 ggtheme=ggtheme,
                                                                 x_range=x_range,
                                                                 x_breaks=x_breaks,
                                                                 flip=TRUE,
                                                                 plot_height=density_plot_height,
                                                                 outcome_type=outcome_type)
        
        # Extract panel elements.
        g_margin_positive <- .gtable_extract(g=plotting.to_grob(p_margin_positive),
                                             element="panel",
                                             partial_match=TRUE)
        
        g_margin_negative <- .gtable_extract(g=plotting.to_grob(p_margin_negative),
                                             element="panel",
                                             partial_match=TRUE)
        
        # Insert the panel elements
        g_calibration <- .gtable_insert(g=g_calibration,
                                        g_new=g_margin_negative,
                                        where="top",
                                        ref_element="panel",
                                        partial_match=TRUE)
        
        g_calibration <- .gtable_insert(g=g_calibration,
                                        g_new=g_margin_positive,
                                        where="top",
                                        ref_element="panel",
                                        partial_match=TRUE)
        
      } else {
        # Procedure for normal density plots.
        p_margin <- .create_calibration_density_subplot(x=data_facet_list[[ii]],
                                                        ggtheme=ggtheme,
                                                        x_range=x_range,
                                                        x_breaks=x_breaks,
                                                        flip=FALSE,
                                                        plot_height=density_plot_height,
                                                        outcome_type=outcome_type)
        
        # Extract the panel element from the density plot.
        g_margin <- .gtable_extract(g=plotting.to_grob(p_margin),
                                    element=c("panel"),
                                    partial_match=TRUE)
        
        # Insert in the calibration plot at the top margin.
        g_calibration <- .gtable_insert(g=g_calibration,
                                        g_new=g_margin,
                                        where="top",
                                        ref_element="panel",
                                        partial_match=TRUE)
      }
    }
    
    # Re-introduce plot elements
    g_calibration <- plotting.reinsert_plot_elements(g=g_calibration,
                                                     elements="strip_y",
                                                     grob_list=extracted_elements,
                                                     ggtheme=ggtheme)
    
    # Add combined grob to list
    figure_list <- append(figure_list, list(g_calibration))
    
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



.create_calibration_plot <- function(x,
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
                                     show_calibration_fit,
                                     show_goodness_of_fit,
                                     linear_test,
                                     gof_test,
                                     outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  type <- NULL
  
  # Define formula for casting the linear test data wide. This data will be used
  # to plot linear fits in the plot. The "evaluation_time" column will only
  # appear for survival endpoints, and the "pos_class" column only in
  # binomial/multinomial outcomes, but will only matter for multinomial
  # outcomes.
  if(outcome_type %in% c("survival")){
    cast_formula <- stats::as.formula(paste0(paste("data_set", "fs_method", "learner", "evaluation_time", sep="+"), "~type"))
    
  } else if(outcome_type %in% c("multinomial")){
    cast_formula <- stats::as.formula(paste0(paste("data_set", "fs_method", "learner", "pos_class", sep="+"), "~type"))
    
  } else {
    cast_formula <- stats::as.formula(paste0(paste("data_set", "fs_method", "learner", sep="+"), "~type"))
  }
  
  # Cast wide on the value of the intercept or slope coefficient.
  linear_fit <- dcast(data=linear_test, cast_formula, value.var="coef_value")
  
  # Generate a guide table
  x_guide_list <- plotting.create_guide_table(x=x, color_by=color_by, discrete_palette=discrete_palette)
  fit_guide_list <- plotting.create_guide_table(x=linear_fit, color_by=color_by, discrete_palette=discrete_palette)
  
  # Extract data
  x <- x_guide_list$data

  # Create basic plot
  p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("expected"), y=!!sym("observed")))
  p <- p + ggtheme
  
  # Add identity line
  p <- p + ggplot2::geom_abline(slope=1.0, intercept=0.0, colour="grey80", linetype="dashed")
  
  # Add fill colors
  if(!is.null(color_by)){
    
    # Add scatter for individual data points.
    p <- p + ggplot2::geom_point(mapping=ggplot2::aes(colour=!!sym("color_breaks")))
    
    # Add fit
    if(!is_empty(fit_guide_list$data)){
      p <- p + ggplot2::geom_abline(data=fit_guide_list$data,
                                    mapping=ggplot2::aes(colour=!!sym("color_breaks"),
                                                         intercept=!!sym("offset"),
                                                         slope=!!sym("slope")))
    }
    
    # Set colour.
    p <- p + ggplot2::scale_fill_manual(name=legend_label$guide_color,
                                        values=x_guide_list$guide_color$color_values,
                                        breaks=x_guide_list$guide_color$color_breaks,
                                        drop=FALSE)
    
  } else {
    
    # Add scatter for individual data points.
    p <- p + ggplot2::geom_point()
    
    # Add fit
    if(!is_empty(fit_guide_list$data)){
      p <- p + ggplot2::geom_abline(data=fit_guide_list$data,
                                    mapping=ggplot2::aes(intercept=!!sym("offset"),
                                                         slope=!!sym("slope")))
    }
  }
  
  # Set breaks and  limits on the x and y-axis
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks, limits=x_range)
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
  
  # Annotate calibration information.
  if((show_calibration_fit & !is_empty(linear_test)) | (show_goodness_of_fit & !is_empty(gof_test))){
    
    # Merge test data
    label <- character(0)
    
    if(show_calibration_fit & !is_empty(linear_test)){
      
      # Add calibration-in-the-large.
      label <- append(label, paste0("intercept: ",
                                    round(linear_test[type=="offset"]$coef_value, 2),
                                    " (",
                                    round(linear_test[type=="offset"]$coef_lower, 2),
                                    "\u2013",
                                    round(linear_test[type=="offset"]$coef_upper, 2),
                                    ")"))
      
      # Add calibration slope.
      label <- append(label, paste0("slope: ",
                                    round(linear_test[type=="slope"]$coef_value, 2),
                                    " (",
                                    round(linear_test[type=="slope"]$coef_lower, 2),
                                    "\u2013",
                                    round(linear_test[type=="slope"]$coef_upper, 2),
                                    ")"))
    }
    
    if(show_goodness_of_fit & !is_empty(gof_test)){
      
      # Hosmer-Lemeshow test
      if("hosmer_lemeshow" %in% c(gof_test$type)){
        label <- append(label, paste0("HL-test p: ", signif(gof_test[type=="hosmer_lemeshow"]$p_value, 2)))
      }
      
      # Nam-D'Agostino test
      if("nam_dagostino" %in% c(gof_test$type)){
        label <- append(label, paste0("ND-test p: ", signif(gof_test[type=="nam_dagostino"]$p_value, 2)))
      }
      
      # Greenwood-Nam-D'Agostino test
      if("greenwood_nam_dagostino" %in% c(gof_test$type)){
        label <- append(label, paste0("GND-test p: ", signif(gof_test[type=="greenwood_nam_dagostino"]$p_value, 2)))
      }
    }
    
    # Combine all label elements, and use for annotation
    if(length(label) > 0){
      label <- paste(label, collapse="\n")
      
      # Obtain default settings.
      text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
      
      # Show in plot
      p <- p + ggplot2::annotate("text",
                                 x=x_range[1],
                                 y=y_range[2],
                                 label=label,
                                 colour=text_settings$colour,
                                 family=text_settings$family,
                                 fontface=text_settings$face,
                                 size=text_settings$geom_text_size,
                                 vjust="inward",
                                 hjust="inward")
    }
  }
  
  # Update labels.
  p <- p + ggplot2::labs(x=x_label, y=y_label, title=plot_title, subtitle=plot_sub_title, caption=caption)
  
  return(p)
}


.create_calibration_density_subplot <- function(x, ggtheme, x_range, x_breaks, flip=FALSE, plot_height, outcome_type){
  
  # Determine the dataset for binomial and multinomial endpoints. Density is
  # plotted separately for samples of positive and negative classes.
  if(outcome_type %in% c("binomial", "multinomial")){
    
    # Density plots based on number of positive (flip=FALSE) or negative
    # (flip=TRUE) samples.
    if(!flip){
      p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("expected"), weight=!!sym("n_pos")))
      
    } else {
      p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("expected"), weight=!!sym("n_neg")))
    }
    
  } else {
    p <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=!!sym("expected"), weight=!!sym("n_g")))
  }
  
  # Create plot
  p <- p + ggplot2::geom_density(colour="grey30", fill="grey30")
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks, limits=x_range)
  
  # Set main theme
  p <- p + ggtheme
  
  # Remove some theme elements and reduce margins
  p <- p + ggplot2::theme(panel.grid=ggplot2::element_blank(),
                          panel.background=ggplot2::element_blank(),
                          panel.border=ggplot2::element_blank(),
                          axis.line = ggplot2::element_blank(),
                          axis.ticks.y=ggplot2::element_blank(),
                          axis.text.x=ggplot2::element_blank(),
                          axis.ticks.x=ggplot2::element_blank(),
                          axis.title.x=ggplot2::element_blank(),
                          axis.title.y=ggplot2::element_blank())
  
  if(flip){
    p <- p + ggplot2::scale_y_reverse()
  }
  
  if(outcome_type %in% c("binomial", "multinomial")){
    
    # Obtain default settings.
    text_settings <- plotting.get_geom_text_settings(ggtheme=ggtheme)
    
    # Show class indicator in plot.
    p <- p + ggplot2::annotate("text",
                               x=0.0,
                               y=Inf,
                               label=ifelse(flip, "-", "+"),
                               colour=text_settings$colour,
                               family=text_settings$family,
                               fontface=text_settings$face,
                               size=text_settings$geom_text_size,
                               vjust="inward",
                               hjust="inward")
  }
  
  # Ensure that panel heights are set to the plot object.
  p$custom_grob <- list("heights"=list("name"="panel", "height"=plot_height))
  class(p) <- c("familiar_ggplot", class(p))
  
  return(p)
}



.determine_calibration_plot_dimensions <- function(x,
                                                   facet_by,
                                                   facet_wrap_cols,
                                                   show_density){
  
  # Obtain facetting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x, facet_by=facet_by, facet_wrap_cols=facet_wrap_cols)
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- ifelse(show_density, 6, 5.5)
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}
