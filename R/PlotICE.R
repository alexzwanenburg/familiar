#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#'@title Plot individual conditional expectation plots.
#'
#'@description This method creates individual conditional expectation plots
#'  based on data in a familiarCollection object.
#'
#'@param dir_path (*optional*) Path to the directory where created individual
#'  conditional expectation plots are saved to. Output is saved in the
#'  `explanation` subdirectory. If `NULL`, figures are written to the folder,
#'  but are returned instead.
#'@param discrete_palette (*optional*) Palette to use to colour the different
#'  plot elements in case a value was provided to the `color_by` argument.
#'@param gradient_palette (*optional*) Sequential or divergent palette used to
#'  colour the raster in 2D individual conditional expectation or partial
#'  dependence plots. This argument is not used for 1D plots.
#'@param gradient_palette_range (*optional*) Numerical range used to span the
#'  gradient for 2D plots. This should be a range of two values, e.g. `c(0, 1)`.
#'  By default, values are determined from the data, dependent on the
#'  `value_scales` parameter. This parameter is ignored for 1D plots.
#'@param value_scales (*optional*) Sets scaling of predicted values. This
#'  parameter has several options:
#'
#'  * `fixed` (default): The value axis for all features will have the same
#'  range.
#'
#'  * `feature`: The value axis for each feature will have the same range. This
#'  option is unavailable for 2D plots.
#'
#'  * `figure`: The  value axis for all facets in a figure will have the same
#'  range.
#'
#'  * `facet`: Each facet has its own range. This option is unavailable for 2D
#'  plots.
#'
#'  For 1D plots, this option is ignored if the `y_range` is provided, whereas
#'  for 2D it is ignored if the `gradient_palette_range` is provided.
#'@param show_ice (*optional*) Sets whether individual conditional expectation
#'  plots should be created.
#'@param show_pd (*optional*) Sets whether partial dependence plots should be
#'  created. Note that if an anchor is set for a particular feature, its partial
#'  dependence cannot be shown.
#'@param show_novelty (*optional*) Sets whether novelty is shown in plots.
#'@param anchor_values (*optional*) A single value or a named list or array of
#'  values that are used to centre the individual conditional expectation plot.
#'  A single value is valid if and only if only a single feature is assessed.
#'  Otherwise, values Has no effect if the plot is not shown, i.e.
#'  `show_ice=FALSE`. A partial dependence plot cannot be shown for those
#'  features.
#'
#'@inheritParams as_familiar_collection
#'@inheritParams plot_univariate_importance
#'@inheritParams plotting.check_input_args
#'@inheritParams plotting.check_data_handling
#'@inheritDotParams export_ice_data -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'
#'@details This function generates individual conditional expectation plots.
#'  These plots come in two varieties, namely 1D and 2D. 1D plots show the
#'  predicted value as function of a single feature, whereas 2D plots show the
#'  predicted value as a function of two features.
#'
#'  Available splitting variables are: `feature`, `fs_method`, `learner`,
#'  `data_set` and `positive_class` (categorical outcomes) or `evaluation_time`
#'  (survival outcomes). By default, for 1D ICE plots the data are split by
#'  `feature`, `fs_method` and `learner`, with faceting by `data_set`,
#'  `positive_class` or `evaluation_time`. If only partial dependence is shown,
#'  `positive_class` and `evaluation_time` are used to set colours instead. For
#'  2D plots, by default the data are split by `feature`, `fs_method` and
#'  `learner`, with faceting by `data_set`, `positive_class` or
#'  `evaluation_time`. The `color_by` argument cannot be used with 2D plots, and
#'  attempting to do so causes an error.
#'
#'  The splitting variables indicated by `color_by` are coloured according to
#'  the `discrete_palette` parameter. This parameter is therefore only used for
#'  1D plots. Available palettes for `discrete_palette` and `gradient_palette`
#'  are those listed by `grDevices::palette.pals()` (requires R >= 4.0.0),
#'  `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'  `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'  palettes of the same name in `grDevices`. If not specified, a default
#'  palette based on palettes in Tableau are used. You may also specify your own
#'  palette by using colour names listed by `grDevices::colors()` or through
#'  hexadecimal RGB strings.
#'
#'  Bootstrap confidence intervals of the partial dependence plots can be shown
#'  using various styles set by `conf_int_style`:
#'
#'  * `ribbon` (default): confidence intervals are shown as a ribbon with an
#'  opacity of `conf_int_alpha` around the point estimate of the partial
#'  dependence.
#'
#'  * `step` (default): confidence intervals are shown as a step function around
#'  the point estimate of the partial dependence.
#'
#'  * `none`: confidence intervals are not shown. The point estimate of the
#'  partial dependence is shown as usual.
#'
#'  Note that when bootstrap confidence intervals were computed, they were also
#'  computed for individual samples in individual conditional expectation plots.
#'  To avoid clutter, only point estimates for individual samples are shown.
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
#'@exportMethod plot_ice
#'@md
#'@rdname plot_ice-methods
setGeneric("plot_ice",
           function(object,
                    draw=FALSE,
                    dir_path=NULL,
                    split_by=NULL,
                    color_by=NULL,
                    facet_by=NULL,
                    facet_wrap_cols=NULL,
                    ggtheme=NULL,
                    discrete_palette=NULL,
                    gradient_palette=NULL,
                    gradient_palette_range=NULL,
                    x_label=waiver(),
                    y_label=waiver(),
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
                    value_scales=waiver(),
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    show_ice=TRUE,
                    show_pd=TRUE,
                    show_novelty=TRUE,
                    anchor_values=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    ...) standardGeneric("plot_ice"))

#####plot_ice (generic)#####

#'@rdname plot_ice-methods
setMethod("plot_ice", signature(object="ANY"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=NULL,
                   x_label=waiver(),
                   y_label=waiver(),
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
                   value_scales=waiver(),
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   show_ice=TRUE,
                   show_pd=TRUE,
                   show_novelty=TRUE,
                   anchor_values=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object, "data_element"="ice_data"),
                                     list(...)))
            
            return(do.call(plot_ice,
                           args=list("object"=object,
                                     "draw"=draw,
                                     "dir_path"=dir_path,
                                     "split_by"=split_by,
                                     "color_by"=color_by,
                                     "facet_by"=facet_by,
                                     "facet_wrap_cols"=facet_wrap_cols,
                                     "ggtheme"=ggtheme,
                                     "discrete_palette"=discrete_palette,
                                     "gradient_palette"=gradient_palette,
                                     "gradient_palette_range"=gradient_palette_range,
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
                                     "value_scales"=value_scales,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "show_ice"=show_ice,
                                     "show_pd"=show_pd,
                                     "show_novelty"=show_novelty,
                                     "anchor_values"=anchor_values,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units)))
          })


#####plot_ice (collection)#####

#'@rdname plot_ice-methods
setMethod("plot_ice", signature(object="familiarCollection"),
          function(object,
                   draw=FALSE,
                   dir_path=NULL,
                   split_by=NULL,
                   color_by=NULL,
                   facet_by=NULL,
                   facet_wrap_cols=NULL,
                   ggtheme=NULL,
                   discrete_palette=NULL,
                   gradient_palette=NULL,
                   gradient_palette_range=NULL,
                   x_label=waiver(),
                   y_label=waiver(),
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
                   value_scales=waiver(),
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   show_ice=TRUE,
                   show_pd=TRUE,
                   show_novelty=TRUE,
                   anchor_values=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            
            
            # Get input data.
            ice_data <- export_ice_data(object=object,
                                        aggregate_results=TRUE)
            
            pd_data <- export_partial_dependence_data(object=object,
                                                      aggregate_results=TRUE)
            
            # Check anchor values.
            if(length(ice_data) > 0 & !is.null(anchor_values)){
              
              # Determine feature names.
              feature_names <- unique(c(sapply(ice_data, function(x) (x@identifiers$feature_x)),
                                        sapply(ice_data, function(x) (x@identifiers$feature_y))))
              
              # Check if names are provided to anchor values.
              if(length(feature_names) > 1 & length(names(anchor_values)) == 0){
                stop(paste0("Data for plotting individual conditional expectation plots for more than one feature are present. ",
                            "However, the anchor values for centering the plots could not be assigned because they were not named. ",
                            "Please provide a named vector or list."))
              }
              
              # Check if the names provided to anchor values match existing
              # values.
              if(length(setdiff(names(anchor_values), feature_names)) > 0){
                warning(paste0("One or more feature names specified as anchor values do not exist as features for which data for ",
                               "plotting individual conditional expectation plots were computed: ",
                               paste_s(setdiff(names(anchor_values), feature_names))))
              }
            }
            
            # Check that the data are not empty.
            if(is_empty(ice_data)) return(NULL)
            
            # Check that the data are not evaluated at the model level.
            if(all(sapply(ice_data, function(x) (x@detail_level == "model")))){
              ..warning_no_comparison_between_models()
              return(NULL)
            }
            
            # Obtain data element from list.
            if(is.list(ice_data)){
              if(is_empty(ice_data)) return(NULL)
              
              if(all(sapply(ice_data, is_empty))) return(NULL)
            }
            
            # Check that the data are not empty.
            if(is_empty(ice_data)) return(NULL)
            
            # Update the output so that it is more consistent.
            data <- mapply(.update_ice_and_pd_output,
                           ice_data=ice_data,
                           pd_data=pd_data,
                           MoreArgs=list("outcome_type"=object@outcome_type,
                                         "anchor_values"=anchor_values),
                           SIMPLIFY=FALSE)
          
            # Flatten nested list.
            data <- .flatten_nested_list(data)
            ice_data <- data$ice_data
            pd_data <- data$pd_data
            
            # Check that show_pd and show_ice are not both FALSE.
            if(!show_pd & !show_ice){
              warning(paste0("One or both of \"show_pd\" and \"show_ice\" should be TRUE to plot ",
                             "individual conditional expectation and/or partial dependence plots."))
              return(NULL)
            }
            
            ##### Check input arguments ------------------------------------------------
            
            # ggtheme
            if(!inherits(ggtheme, "theme")) ggtheme <- plotting.get_theme(use_theme=ggtheme)

            # conf_int_style
            if(length(conf_int_style) > 1) conf_int_style <- head(conf_int_style, n=1)
            
            # Set the style of the confidence interval to none, in case no
            # confidence interval data is present.
            if(show_pd){
              if(!all(sapply(pd_data, function(x) (x@estimation_type %in% c("bci", "bootstrap_confidence_interval"))))) conf_int_style <- "none"
              
            } else {
              conf_int_style <- NULL
            }
            
            # Determine whether 1D or 2D plots are shown.
            show_2d <- any(sapply(ice_data, function(x) (!is.null(x@identifiers$feature_y))))
            
            # If the y-axis will contain a feature, only the partial dependence
            # can be shown.
            if(show_2d){
              show_pd <- TRUE
              show_ice <- FALSE
            }
            
            # Determine identifiers that should be dropped frpm plot_data.
            if(show_2d){
              dropped_identifiers <- c("sample", "feature_x_value", "feature_y_value")
            } else {
              dropped_identifiers <- c("sample", "feature_x_value")
            }
            
            # Determine splitting variables present in the dataset.
            if(show_ice){
              plot_data <- identify_element_sets(ice_data,
                                                 ignore_grouping_column=FALSE,
                                                 ignore_list_identifier=FALSE,
                                                 drop_identiers=dropped_identifiers)
              
            } else {
              plot_data <- identify_element_sets(pd_data,
                                                 ignore_grouping_column=FALSE,
                                                 ignore_list_identifier=FALSE,
                                                 drop_identiers=dropped_identifiers)
            }
            
            # Set default splitting variables.
            if(is.null(split_by) & is.null(facet_by) & is.null(color_by)){
              if(show_2d){
                split_by <- c("fs_method", "learner", "feature_x", "feature_y")
                facet_by <- c("data_set", "positive_class", "evaluation_time")
                color_by <- NULL
                
              } else if(show_ice) {
                split_by <- c("fs_method", "learner", "feature_x", "feature_y")
                facet_by <- c("data_set", "positive_class", "evaluation_time")
                color_by <- NULL

              } else if(show_pd) {
                split_by <- c("fs_method", "learner", "feature_x", "feature_y")
                facet_by <- c("data_set")
                color_by <- c("positive_class", "evaluation_time")
              }
            }
            
            # Check that the color_by parameter is not set for 2d plots.
            if(show_2d & !is.null(color_by)){
              stop(paste0("The \"color_by\" parameter cannot be used when creating 2D ",
                          "individual conditional expection and partial dependence plots, ",
                          "as additional colour-coding would making interpretation very difficult."))
            }
            
            # Check splitting variables and generate sanitised output
            split_var_list <- plotting.check_data_handling(x=plot_data,
                                                           split_by=split_by,
                                                           color_by=color_by,
                                                           facet_by=facet_by,
                                                           available=c("fs_method",
                                                                       "learner",
                                                                       "feature_x",
                                                                       "feature_y",
                                                                       "data_set",
                                                                       "positive_class",
                                                                       "evaluation_time"))
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            # Create a legend label
            legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                         color_by=color_by)
            
            # Check input arguments for validity.
            plotting.check_input_args(conf_int_alpha=conf_int_alpha,
                                      conf_int_style=conf_int_style,
                                      facet_wrap_cols=facet_wrap_cols,
                                      x_label=x_label,
                                      y_label=y_label,
                                      legend_label=legend_label,
                                      plot_title=plot_title,
                                      plot_sub_title=plot_sub_title,
                                      caption=caption)
            
            # Set unused data to NULL.
            if(!show_ice) ice_data <- NULL
            if(!show_pd) pd_data <- NULL
            
            # Set value scales
            if(is.waive(value_scales)) value_scales <- "fixed"
            
            # Check if the value scales are correctly defined.
            if(show_2d){
              .check_parameter_value_is_valid(value_scales, var_name="value_scales", values=c("fixed", "figure"))
            } else {
              .check_parameter_value_is_valid(value_scales, var_name="value_scales", values=c("fixed", "figure", "feature", "facet"))
            }
            
            # Set value ranges.
            if(show_2d & !is.null(gradient_palette_range)){
              # The value range is provided by the user.
              value_range <- unique(plot_data[, c("feature_x", "feature_y")])
              value_range[, ":="("min_value"=gradient_palette_range[1],
                                 "max_value"=gradient_palette_range[2])]
              
            } else if(!show_2d & !is.null(y_range)){
              # The value range is provided by the user.
              value_range <- unique(plot_data[, c("feature_x")])
              value_range[, ":="("min_value"=y_range[1],
                                 "max_value"=y_range[2])]
              
            } else if(show_2d & value_scales == "fixed"){
              # The value range is the same for all features.
              value_range <- .create_ice_plot_value_range(x=pd_data,
                                                          scale_method="fixed",
                                                          outcome_type=object@outcome_type)
              
            } else if(!show_2d & value_scales == "fixed"){
              # The value range is the same for all features.
              if(!is.null(show_ice)){
                value_range <- .create_ice_plot_value_range(x=ice_data,
                                                            scale_method="fixed",
                                                            outcome_type=object@outcome_type)
              } else {
                value_range <- .create_ice_plot_value_range(x=pd_data,
                                                            scale_method="fixed",
                                                            outcome_type=object@outcome_type)
              }
              
            } else if(!show_2d & value_scales == "feature"){
              # Every feature has its value range.
              if(!is.null(show_ice)){
                value_range <- .create_ice_plot_value_range(x=ice_data,
                                                            scale_method="feature",
                                                            outcome_type=object@outcome_type)
              } else {
                value_range <- .create_ice_plot_value_range(x=pd_data,
                                                            scale_method="feature",
                                                            outcome_type=object@outcome_type)
              }
              
            } else {
              # We will create value-ranges downstream.
              value_range <- NULL
            }

            
            ##### Create plots ---------------------------------------------------------
            
            # Split data
            if(!is.null(split_by)){
              x_split <- split(plot_data, by=split_by)
              
            } else {
              x_split <- list("null.name"=plot_data)
            }
            
            # Store plots to list in case dir_path is absent.
            if(is.null(dir_path)) plot_list <- list()
            
            # Iterate over splits
            for(ii in names(x_split)){
              
              # Skip empty datasets
              if(is_empty(x_split[[ii]])) next()
              
              # Generate plot
              p <- .plot_ice(x=x_split[[ii]],
                             ice_data=ice_data,
                             pd_data=pd_data,
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
                             x_n_breaks=x_n_breaks,
                             y_range=y_range,
                             y_breaks=y_breaks,
                             y_n_breaks=y_n_breaks,
                             conf_int_style=conf_int_style,
                             conf_int_alpha=conf_int_alpha,
                             show_novelty=show_novelty)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw figure.
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                
                subtype <- ifelse(show_ice, "ice", "pd")
                
                # Determine the subtype
                if(!is.null(split_by)){
                  subtype <- c(subtype, as.character(sapply(split_by, function(jj, x) (x[[jj]][1]), x=x_split[[ii]])))
                  subtype <- paste0(subtype, collapse="_")
                }
                
                # Obtain decent default values for the plot.
                def_plot_dims <- .determine_ice_plot_dimensions(x=x_split[[ii]],
                                                                facet_by=facet_by,
                                                                facet_wrap_cols=facet_wrap_cols)
                
                # Save to file.
                do.call(plotting.save_plot_to_file,
                        args=c(list("plot_obj"=p,
                                    "object"=object,
                                    "dir_path"=dir_path,
                                    "type"="explanation",
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



.plot_ice <- function(x,
                      ice_data,
                      pd_data,
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
                      x_n_breaks,
                      y_range,
                      y_breaks,
                      y_n_breaks,
                      conf_int_style,
                      conf_int_alpha,
                      show_novelty){
  
  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(x=x,
                                                      facet_by=facet_by,
                                                      facet_wrap_cols=facet_wrap_cols)
  
  # Split data into facets. This is done by row.
  data_facet_list <- plotting.split_data_by_facet(x=x,
                                                  plot_layout_table=plot_layout_table)
  
  browser()
  
  # Placeholders for plots.
  figure_list <- list()
  extracted_element_list <- list()
  
  # Iterate over facets
  for(ii in names(data_facet_list)){
    browser()
    # Create calibration plot.
    p_ice <- .create_ice_plot(x=data_facet_list[[ii]],
                              ice_data,
                              pd_data,
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
                              x_n_breaks,
                              y_range,
                              y_breaks,
                              y_n_breaks,
                              conf_int_style,
                              conf_int_alpha,
                              show_novelty)
    
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
                                              conf_int_alpha=conf_int_alpha,
                                              conf_int_style=conf_int_style,
                                              show_calibration_fit=show_calibration_fit,
                                              show_goodness_of_fit=show_goodness_of_fit,
                                              linear_test=linear_test_facet_list[[ii]],
                                              gof_test=gof_test_facet_list[[ii]],
                                              outcome_type=outcome_type,
                                              grouping_column=grouping_column,
                                              is_point=is_point)
    
    # Extract plot elements from the main calibration plot.
    extracted_elements <- plotting.extract_plot_elements(p=p_calibration)
    
    # Remove extracted elements from the plot.
    p_calibration <- plotting.remove_plot_elements(p=p_calibration)
    
    # Rename plot elements.
    g_calibration <- plotting.rename_plot_elements(g=plotting.to_grob(p_calibration),
                                                   extension="main")
    
    if(show_density & gtable::is.gtable(g_calibration) & !is_empty(density_facet_list[[ii]])){
      
      # Procedure for normal density plots.
      p_margin <- .create_calibration_density_subplot(x=density_facet_list[[ii]],
                                                      ggtheme=ggtheme,
                                                      x_range=x_range,
                                                      x_breaks=x_breaks,
                                                      flip=FALSE,
                                                      plot_height=density_plot_height)
      
      # Extract the panel element from the density plot.
      g_margin <- .gtable_extract(g=plotting.to_grob(p_margin),
                                  element=c("panel"),
                                  partial_match=TRUE)
      
      # Insert in the calibration plot at the top margin.
      g_calibration <- .gtable_insert(g=g_calibration,
                                      g_new=g_margin,
                                      where="top",
                                      ref_element="panel-main",
                                      partial_match=TRUE)
    }
    
    # Add combined grob to list
    figure_list <- c(figure_list, list(g_calibration))
    
    # Add extract elements to the grob_element_list
    extracted_element_list <- c(extracted_element_list, list(extracted_elements))
  }
  
  # Update the layout table.
  plot_layout_table <- plotting.update_plot_layout_table(plot_layout_table=plot_layout_table,
                                                         grobs=figure_list,
                                                         x_text_shared=x_label_shared,
                                                         x_label_shared=x_label_shared,
                                                         y_text_shared=y_label_shared,
                                                         y_label_shared=y_label_shared,
                                                         facet_wrap_cols=facet_wrap_cols)
  
  # Combine features.
  g <- plotting.arrange_figures(grobs=figure_list,
                                plot_layout_table=plot_layout_table,
                                element_grobs=extracted_element_list,
                                ggtheme=ggtheme)
  
  return(g)
  
  
  # Suppress NOTES due to non-standard evaluation in data.table
  curve_type <- NULL
  
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
  p <- p + ggplot2::scale_x_continuous(breaks=x_breaks, limits=x_range)
  p <- p + ggplot2::scale_y_continuous(breaks=y_breaks, limits=y_range)
  
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
  
  return(p)
}


.create_ice_plot <- function()



.determine_ice_plot_dimensions <- function(x,
                                           facet_by,
                                           facet_wrap_cols){
  
  # Obtain faceting dimensions
  plot_dims <- plotting.get_plot_layout_dims(x=x,
                                             facet_by=facet_by,
                                             facet_wrap_cols=facet_wrap_cols)
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- 4
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}




.create_ice_plot_value_range <- function(x, scale_method, outcome_type){

  # Suppress NOTES due to non-standard evaluation in data.table
  min_value <- max_value <- NULL
  
  # Obtain value ranges.
  value_range <- lapply(x, ..create_ice_plot_value_range)
  value_range <- data.table::rbindlist(value_range, use.names=TRUE)
  
  if(scale_method %in% c("fixed", "figure")){
    if(outcome_type %in% c("binomial", "multinomial", "survival")){
      # Check if probabilities all lie in the expected range.
      if(all(value_range$min_value >= 0.0) & all(value_range$max_value <= 1.0)){
        value_range[, ":="("min_value"=0.0, "max_value"=1.0)]
        
      } else {
        value_range[, ":="("min_value"=min(min_value),
                           "max_value"=max(max_value))]
      }
      
    } else {
      value_range[, ":="("min_value"=min(min_value),
                         "max_value"=max(max_value))]
    }
  } else if(scale_method == "facet"){
    # Facet scaling does not make the value ranges nice (i.e. map to (0,1) in
    # case of probabilities.)
    value_range[, ":="("min_value"=min(min_value),
                       "max_value"=max(max_value))]
    
  } else if(scale_method == "feature"){
    value_range[, ":="("min_value"=min(min_value),
                       "max_value"=max(max_value)),
                by=c("feature_x")]
    
  } else {
    
  }
  
  return(value_range)
}



..create_ice_plot_value_range <- function(x){
  # Extract value range information.
  browser()
  # Get the x-feature
  value_range <- list("feature_x"=x@identifiers$feature_x)
  
  # Get the y-feature, if present.
  if(!is.null(x@identifiers$feature_y)) value_range <- c(value_range, list("feature_y"=x@identifiers$feature_y))
  
  # Get minimum and maximum values.
  value_range <- c(value_range, list("min_value"=min(x@data$value),
                                     "max_value"=max(x@data$value)))
  
  # Return as data.table.
  return(data.table::as.data.table(value_range))
}
