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
#'  plot elements in case a value was provided to the `color_by` argument. For
#'  2D individual conditional expectation plots without novelty, the initial
#'  colour determines the colour of the points indicating sample values.
#'@param gradient_palette (*optional*) Sequential or divergent palette used to
#'  colour the raster in 2D individual conditional expectation or partial
#'  dependence plots. This argument is not used for 1D plots.
#'@param gradient_palette_range (*optional*) Numerical range used to span the
#'  gradient for 2D plots. This should be a range of two values, e.g. `c(0, 1)`.
#'  By default, values are determined from the data, dependent on the
#'  `value_scales` parameter. This parameter is ignored for 1D plots.
#'@param n_max_samples_shown (*optional*) Maximum number of samples shown in an
#'  individual conditional expectation plot. Defaults to 50. These samples are
#'  randomly picked from the samples present in the ICE data, but the same
#'  samples are consistently picked. Partial dependence is nonetheless computed
#'  from all available samples.
#'@param novelty_range (*optional*) Numerical range used to span the range of
#'  novelty values. This determines the size of the bubbles in 2D, and
#'  transparency of lines in 1D. This should be a range of two values, e.g.
#'  `c(0, 1)`. By default, values are determined from the data, dependent on the
#'  `value_scales` parameter. This parameter is ignored if `show_novelty=FALSE`.
#'@param value_scales (*optional*) Sets scaling of predicted values. This
#'  parameter has several options:
#'
#'  * `fixed` (default): The value axis for all features will have the same
#'  range.
#'
#'  * `feature`: The value axis for each feature will have the same range. This
#'  option is unavailable for 2D plots.
#'
#'  * `figure`: The value axis for all facets in a figure will have the same
#'  range.
#'
#'  * `facet`: Each facet has its own range. This option is unavailable for 2D
#'  plots.
#'
#'  For 1D plots, this option is ignored if the `y_range` is provided, whereas
#'  for 2D it is ignored if the `gradient_palette_range` is provided.
#'@param novelty_scales (*optional*) Sets scaling of novelty values, similar to
#'  the `value_scales` parameter, but with more limited options:
#'
#'  * `fixed` (default): The novelty will have the same range for all features.
#'
#'  * `figure`: The novelty will have the same range for all facets in a figure.
#'
#'@param ice_default_alpha (*optional*) Default transparency (value) of sample
#'  lines in an 1D plot. When novelty is shown, this is the transparency
#'  corresponding to the least novel points. The confidence interval alpha
#'  values is scaled by this value.
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
#'@inheritDotParams extract_ice -object
#'
#'@details This function generates individual conditional expectation plots.
#'  These plots come in two varieties, namely 1D and 2D. 1D plots show the
#'  predicted value as function of a single feature, whereas 2D plots show the
#'  predicted value as a function of two features.
#'
#'  Available splitting variables are: `feature_x`, `feature_y` (2D only),
#'  `fs_method`, `learner`, `data_set` and `positive_class` (categorical
#'  outcomes) or `evaluation_time` (survival outcomes). By default, for 1D ICE
#'  plots the data are split by `feature_x`, `fs_method` and `learner`, with
#'  faceting by `data_set`, `positive_class` or `evaluation_time`. If only
#'  partial dependence is shown, `positive_class` and `evaluation_time` are used
#'  to set colours instead. For 2D plots, by default the data are split by
#'  `feature_x`, `fs_method` and `learner`, with faceting by `data_set`,
#'  `positive_class` or `evaluation_time`. The `color_by` argument cannot be
#'  used with 2D plots, and attempting to do so causes an error. Attempting to
#'  specify `feature_x` or `feature_y` for `color_by` will likewise result in an
#'  error, as multiple features cannot be shown in the same facet.
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
                    novelty_range=NULL,
                    value_scales=waiver(),
                    novelty_scales=waiver(),
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    ice_default_alpha=0.6,
                    n_max_samples_shown=50L,
                    show_ice=TRUE,
                    show_pd=TRUE,
                    show_novelty=TRUE,
                    anchor_values=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    export_collection=FALSE,
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
                   novelty_range=NULL,
                   value_scales=waiver(),
                   novelty_scales=waiver(),
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   ice_default_alpha=0.6,
                   n_max_samples_shown=50L,
                   show_ice=TRUE,
                   show_pd=TRUE,
                   show_novelty=TRUE,
                   anchor_values=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   export_collection=FALSE,
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
                                     "n_max_samples_shown"=n_max_samples_shown,
                                     "novelty_range"=novelty_range,
                                     "value_scales"=value_scales,
                                     "novelty_scales"=novelty_scales,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "ice_default_alpha"=ice_default_alpha,
                                     "show_ice"=show_ice,
                                     "show_pd"=show_pd,
                                     "show_novelty"=show_novelty,
                                     "anchor_values"=anchor_values,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units,
                                     "export_collection"=export_collection)))
          })



#'@title Plot partial dependence.
#'
#'@description This method creates partial dependence plots
#'  based on data in a familiarCollection object.
#'
#'@inheritParams plot_ice
#'@inheritDotParams export_ice_data -object
#'@inheritDotParams ggplot2::ggsave -height -width -units
#'@inheritDotParams extract_ice -object
#'
#'@details This function generates partial dependence plots.
#'  These plots come in two varieties, namely 1D and 2D. 1D plots show the
#'  predicted value as function of a single feature, whereas 2D plots show the
#'  predicted value as a function of two features.
#'
#'  Available splitting variables are: `feature_x`, `feature_y` (2D only),
#'  `fs_method`, `learner`, `data_set` and `positive_class` (categorical
#'  outcomes) or `evaluation_time` (survival outcomes). By default, for 1D ICE
#'  plots the data are split by `feature_x`, `fs_method` and `learner`, with
#'  faceting by `data_set`, `positive_class` or `evaluation_time`. If only
#'  partial dependence is shown, `positive_class` and `evaluation_time` are used
#'  to set colours instead. For 2D plots, by default the data are split by
#'  `feature_x`, `fs_method` and `learner`, with faceting by `data_set`,
#'  `positive_class` or `evaluation_time`. The `color_by` argument cannot be
#'  used with 2D plots, and attempting to do so causes an error. Attempting to
#'  specify `feature_x` or `feature_y` for `color_by` will likewise result in an
#'  error, as multiple features cannot be shown in the same facet.
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
#'  Labelling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#'@return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'@exportMethod plot_ice
#'@md
#'@rdname plot_pd-methods
setGeneric("plot_pd",
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
                    plot_title=waiver(),
                    plot_sub_title=waiver(),
                    caption=NULL,
                    x_range=NULL,
                    x_n_breaks=5,
                    x_breaks=NULL,
                    y_range=NULL,
                    y_n_breaks=5,
                    y_breaks=NULL,
                    novelty_range=NULL,
                    value_scales=waiver(),
                    novelty_scales=waiver(),
                    conf_int_style=c("ribbon", "step", "none"),
                    conf_int_alpha=0.4,
                    show_novelty=TRUE,
                    anchor_values=NULL,
                    width=waiver(),
                    height=waiver(),
                    units=waiver(),
                    export_collection=FALSE,
                    ...) standardGeneric("plot_pd"))


#####plot_pd (generic)#####

#'@rdname plot_pd-methods
setMethod("plot_pd", signature(object="ANY"),
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
                   plot_title=waiver(),
                   plot_sub_title=waiver(),
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   novelty_range=NULL,
                   value_scales=waiver(),
                   novelty_scales=waiver(),
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   show_novelty=TRUE,
                   anchor_values=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   export_collection=FALSE,
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
                                     "novelty_range"=novelty_range,
                                     "value_scales"=value_scales,
                                     "novelty_scales"=novelty_scales,
                                     "conf_int_style"=conf_int_style,
                                     "conf_int_alpha"=conf_int_alpha,
                                     "show_ice"=FALSE,
                                     "show_pd"=TRUE,
                                     "show_novelty"=show_novelty,
                                     "anchor_values"=anchor_values,
                                     "width"=width,
                                     "height"=height,
                                     "units"=units,
                                     "export_collection"=export_collection)))
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
                   plot_title=waiver(),
                   plot_sub_title=waiver(),
                   caption=NULL,
                   x_range=NULL,
                   x_n_breaks=5,
                   x_breaks=NULL,
                   y_range=NULL,
                   y_n_breaks=5,
                   y_breaks=NULL,
                   novelty_range=NULL,
                   value_scales=waiver(),
                   novelty_scales=waiver(),
                   conf_int_style=c("ribbon", "step", "none"),
                   conf_int_alpha=0.4,
                   ice_default_alpha=0.6,
                   n_max_samples_shown=50L,
                   show_ice=TRUE,
                   show_pd=TRUE,
                   show_novelty=TRUE,
                   anchor_values=NULL,
                   width=waiver(),
                   height=waiver(),
                   units=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
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
            
            # Check "n_max_samples_shown".
            if(!is.null(n_max_samples_shown)){
              n_max_samples_shown <- as.integer(n_max_samples_shown)
              .check_number_in_valid_range(x=n_max_samples_shown, var_name="n_max_samples_shown", range=c(0, Inf))
            }
            
            # If the maximum number of ICE samples shown equal 0, set to NULL.
            if(n_max_samples_shown == 0){
              show_ice <- FALSE
              n_max_samples_shown <- NULL
            }
            
            # Update the output so that it is more consistent.
            data <- mapply(.update_ice_and_pd_output,
                           ice_data=ice_data,
                           pd_data=pd_data,
                           MoreArgs=list("outcome_type"=object@outcome_type,
                                         "anchor_values"=anchor_values,
                                         "n_samples"=n_max_samples_shown,
                                         "seed"=sample.int(n=10000L, size=1L)),
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
            
            # Check package requirements for plotting.
            if(!require_package(x=..required_plotting_packages(extended=TRUE),
                                purpose="to create ICE/PD plots",
                                message_type="warning")){
              return(NULL)
            }
            
            ##### Check input arguments ------------------------------------------------
            
            # ggtheme
            ggtheme <- .check_ggtheme(ggtheme)

            # Determine whether 1D or 2D plots are shown.
            show_2d <- any(sapply(ice_data, function(x) (!is.null(x@identifiers$feature_y))))
            
            # If the y-axis will contain a feature, only the partial dependence
            # can be shown.
            if(show_2d){
              show_pd <- TRUE
              show_ice <- FALSE
            }
            
            # conf_int_style
            if(length(conf_int_style) > 1) conf_int_style <- head(conf_int_style, n=1)
            
            # Set the style of the confidence interval to none, in case no
            # confidence interval data is present.
            if(show_pd & !show_2d){
              if(!all(sapply(pd_data, function(x) (x@estimation_type %in% c("bci", "bootstrap_confidence_interval"))))) conf_int_style <- "none"
              
            } else {
              conf_int_style <- "none"
            }
            
            # Determine identifiers that should be dropped from plot_data.
            if(show_2d){
              dropped_identifiers <- c("feature_x_value", "feature_y_value")
            } else if(show_ice) {
              dropped_identifiers <- c("sample", "feature_x_value")
            } else {
              dropped_identifiers <- c("feature_x_value")
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
                
                facet_by <- c("data_set")
                if(object@outcome_type == "multinomial") facet_by <- c(facet_by, "positive_class")
                if(object@outcome_type == "survival") facet_by <- c(facet_by, "evaluation_time")
                
                color_by <- NULL
                
              } else if(show_ice) {
                split_by <- c("fs_method", "learner", "feature_x")
                
                facet_by <- c("data_set")
                if(object@outcome_type == "multinomial") facet_by <- c(facet_by, "positive_class")
                if(object@outcome_type == "survival") facet_by <- c(facet_by, "evaluation_time")
                
                color_by <- NULL

              } else if(show_pd) {
                split_by <- c("fs_method", "learner", "feature_x")
                facet_by <- c("data_set")
                
                color_by <- NULL
                if(object@outcome_type == "multinomial") color_by <- c("positive_class")
                if(object@outcome_type == "survival") color_by <- c("evaluation_time")
              }
            }
            
            # Determine splitting variables.
            available_splitting_vars <- c("fs_method", "learner", "data_set", "feature_x")
            if(show_2d) available_splitting_vars <- c(available_splitting_vars, "feature_y")
            
            if(object@outcome_type %in% c("survival")) available_splitting_vars <- c(available_splitting_vars, "evaluation_time")
              
            if(object@outcome_type %in% c("multinomial")) available_splitting_vars <- c(available_splitting_vars, "positive_class")
            
            
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
                                                           available=available_splitting_vars)
            
            # Update splitting variables
            split_by <- split_var_list$split_by
            color_by <- split_var_list$color_by
            facet_by <- split_var_list$facet_by
            
            if("feature_x" %in% color_by | "feature_y" %in% color_by){
              stop("Features cannot be used to specify colors.")
            }
            
            # Create a legend label
            if(show_2d & is.waive(legend_label)){
              legend_label <- switch(object@outcome_type,
                                     "binomial"="probability",
                                     "multinomial"="probability",
                                     "count"="value",
                                     "continuous"="value",
                                     "survival"="probability",
                                     "competing_risk"="probability")
              
            } else if(!show_2d){
              legend_label <- plotting.create_legend_label(user_label=legend_label,
                                                           color_by=color_by)
            }
            
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
                                                          outcome_type=object@outcome_type,
                                                          confidence_interval=conf_int_style != "none")
              
            } else if(!show_2d & value_scales == "fixed"){
              # The value range is the same for all features.
              if(show_ice){
                value_range <- .create_ice_plot_value_range(x=ice_data,
                                                            scale_method="fixed",
                                                            outcome_type=object@outcome_type,
                                                            confidence_interval=conf_int_style != "none")
              } else {
                value_range <- .create_ice_plot_value_range(x=pd_data,
                                                            scale_method="fixed",
                                                            outcome_type=object@outcome_type,
                                                            confidence_interval=conf_int_style != "none")
              }
              
            } else if(!show_2d & value_scales == "feature"){
              # Every feature has its value range.
              if(show_ice){
                value_range <- .create_ice_plot_value_range(x=ice_data,
                                                            scale_method="feature",
                                                            outcome_type=object@outcome_type,
                                                            confidence_interval=conf_int_style != "none")
              } else {
                value_range <- .create_ice_plot_value_range(x=pd_data,
                                                            scale_method="feature",
                                                            outcome_type=object@outcome_type,
                                                            confidence_interval=conf_int_style != "none")
              }
              
            } else {
              # We will create value-ranges downstream.
              value_range <- NULL
            }
            
            # Set novelty scales
            if(is.waive(novelty_scales)) novelty_scales <- "fixed"
            
            # Check if the novelty scales are correctly defined.
            .check_parameter_value_is_valid(novelty_scales, var_name="novelty_scales", values=c("fixed", "figure"))
            
            if(!is.null(novelty_range)){
              # The value range is provided by the user.
              novelty_range <- unique(plot_data[, c("feature_x", "feature_y")])[, ":="("min_value"=novelty_range[1],
                                                                                       "max_value"=novelty_range[2])]
            } else if(novelty_scales == "fixed"){
              # The novelty range is the same for all features.
              if(show_ice){
                novelty_range <- .create_ice_plot_novelty_range(x=ice_data,
                                                                scale_method="fixed",
                                                                outcome_type=object@outcome_type)
              } else {
                novelty_range <- .create_ice_plot_novelty_range(x=pd_data,
                                                                scale_method="fixed",
                                                                outcome_type=object@outcome_type)
              }
              
            } else {
              novelty_range <- NULL
            }
            
            # Check the transparency value,
            .check_number_in_valid_range(x=ice_default_alpha,
                                         var_name="ice_default_alpha",
                                         range=c(0.0, 1.0))
            
            # Set plot function.
            if(show_2d){
              plot_function <- .create_2d_ice_plot
            } else {
              plot_function <- .create_1d_ice_plot
            }

            
            ##### Create plots -------------------------------------------------
            
            # Determine if subtitle should be generated.
            autogenerate_plot_subtitle <- is.waive(plot_sub_title)
            
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
              
              if(is.waive(plot_title)){
                plot_title <- ifelse(show_ice,
                                     "Individual conditional expectation",
                                     "Partial dependence")
              }
              
              if(autogenerate_plot_subtitle){
                plot_sub_title <- plotting.create_subtitle(split_by=split_by,
                                                           x=x_split[[ii]])
              }
              
              # Generate plot
              p <- .plot_ice(x=x_split[[ii]],
                             ice_data=ice_data,
                             pd_data=pd_data,
                             plot_function=plot_function,
                             color_by=color_by,
                             facet_by=facet_by,
                             facet_wrap_cols=facet_wrap_cols,
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
                             x_range=x_range,
                             x_breaks=x_breaks,
                             x_n_breaks=x_n_breaks,
                             y_range=y_range,
                             y_breaks=y_breaks,
                             y_n_breaks=y_n_breaks,
                             value_scales=value_scales,
                             value_range=value_range,
                             novelty_scales=novelty_scales,
                             novelty_range=novelty_range,
                             conf_int_style=conf_int_style,
                             conf_int_alpha=conf_int_alpha,
                             ice_default_alpha=ice_default_alpha,
                             show_novelty=show_novelty,
                             show_ice=show_ice,
                             show_pd=show_pd,
                             show_2d=show_2d,
                             outcome_type=object@outcome_type)
              
              # Check empty output
              if(is.null(p)) next()
              
              # Draw figure.
              if(draw) plotting.draw(plot_or_grob=p)
              
              # Save and export
              if(!is.null(dir_path)){
                
                # Set initial subtype.
                subtype <- ifelse(show_ice, "ice", "pd")
                if(show_2d) subtype <- c(subtype, "2d")
                
                # Set subtype.
                subtype <- plotting.create_subtype(x=x_split[[ii]],
                                                   subtype=subtype,
                                                   split_by=split_by)
                
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
                plot_list <- c(plot_list, list(p))
              }
            }
            
            # Generate output
            return(plotting.get_output(dir_path=dir_path,
                                       plot_list=plot_list,
                                       export_collection=export_collection,
                                       object=object))
          })



.plot_ice <- function(x,
                      ice_data,
                      pd_data,
                      plot_function,
                      color_by,
                      facet_by,
                      facet_wrap_cols,
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
                      x_range,
                      x_breaks,
                      x_n_breaks,
                      x_label_shared=waiver(),
                      y_range,
                      y_breaks,
                      y_n_breaks,
                      y_label_shared=waiver(),
                      value_scales,
                      value_range,
                      novelty_scales,
                      novelty_range,
                      conf_int_style,
                      conf_int_alpha,
                      ice_default_alpha,
                      show_novelty,
                      show_ice,
                      show_pd,
                      show_2d,
                      outcome_type){
  
  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(x=x,
                                                      facet_by=facet_by,
                                                      facet_wrap_cols=facet_wrap_cols)
  
  # Split data into facets. This is done by row.
  data_facet_list <- plotting.split_data_by_facet(x=x,
                                                  plot_layout_table=plot_layout_table)
  
  # Set value scales.
  if(is.null(value_range) & value_scales == "figure"){
    # Keep only data that are used to set the scale.
    plot_ice_data <- .select_ice_plot_data(x=x, data=ice_data)
    plot_pd_data <- .select_ice_plot_data(x=x, data=pd_data)
    
    if(!is.null(plot_ice_data)){
      value_range <- .create_ice_plot_value_range(x=plot_ice_data,
                                                  scale_method="figure",
                                                  outcome_type=outcome_type,
                                                  confidence_interval=conf_int_style != "none")
    } else {
      value_range <- .create_ice_plot_value_range(x=plot_pd_data,
                                                  scale_method="figure",
                                                  outcome_type=outcome_type,
                                                  confidence_interval=conf_int_style != "none")
    }
  }
  
  # Set novelty scales
  if(is.null(novelty_range) & novelty_scales == "figure"){
    # Keep only data that are used to set the scale.
    plot_ice_data <- .select_ice_plot_data(x=x, data=ice_data)
    plot_pd_data <- .select_ice_plot_data(x=x, data=pd_data)
    
    if(!is.null(plot_ice_data)){
      novelty_range <- .create_ice_plot_novelty_range(x=plot_ice_data,
                                                      scale_method="figure",
                                                      outcome_type=outcome_type)
    } else {
      novelty_range <- .create_ice_plot_novelty_range(x=plot_pd_data,
                                                      scale_method="figure",
                                                      outcome_type=outcome_type)
    }
  }
  
  # Set x_label_shared
  if(is.waive(x_label_shared)){
    # Default value.
    x_label_shared <- "column"
    
    if(length(facet_by) > 1){
      if("feature_x" %in% c(tail(facet_by, n=length(facet_by)-1L))){
        # Same feature is used row-wise, not column-wise.
        x_label_shared <- "individual"
      }
    }
  }
  
  # Set y_label_shared
  if(is.waive(y_label_shared)){
    # Default value.
    y_label_shared <- "row"
    
    if(value_scales == "facet"){
      # If y-axis is scaled per individual facet
      y_label_shared <- "individual"
      
    } else if(show_2d & length(facet_by) > 0) {
      if("feature_y" == facet_by[1]){
        # Same feature is used column-wise, not row-wise.
        y_label_shared <- "individual"
      }
    }
  }
  
  # Check label sharing.
  plotting.check_input_args(x_label_shared=x_label_shared,
                            y_label_shared=y_label_shared)
  
  # Placeholders for plots and plot elements.
  figure_list <- list()
  extracted_element_list <- list()
  
  # Iterate over facets
  for(ii in names(data_facet_list)){
    
    # Set facet_data
    facet_data <- data_facet_list[[ii]]
    
    # Keep only data that are used to set the scale.
    facet_ice_data <- .select_ice_plot_data(x=facet_data, data=ice_data)
    facet_pd_data <- .select_ice_plot_data(x=facet_data, data=pd_data)
    
    p_main <- do.call(plot_function,
                      args=list("facet_data"=facet_data,
                                "ice_data"=facet_ice_data,
                                "pd_data"=facet_pd_data,
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
                                "x_breaks"=x_breaks,
                                "x_n_breaks"=x_n_breaks,
                                "y_range"=y_range,
                                "y_breaks"=y_breaks,
                                "y_n_breaks"=y_n_breaks,
                                "value_scales"=value_scales,
                                "value_range"=value_range,
                                "novelty_range"=novelty_range,
                                "conf_int_style"=conf_int_style,
                                "conf_int_alpha"=conf_int_alpha,
                                "ice_default_alpha"=ice_default_alpha,
                                "show_novelty"=show_novelty,
                                "show_ice"=show_ice,
                                "show_pd"=show_pd,
                                "outcome_type"=outcome_type))
    
    # Extract plot elements from the main calibration plot.
    extracted_elements <- plotting.extract_plot_elements(p=p_main)
    
    # Remove extracted elements from the plot.
    p_main <- plotting.remove_plot_elements(p=p_main)
    
    # Rename plot elements.
    g_calibration <- plotting.rename_plot_elements(g=plotting.to_grob(p_main),
                                                   extension="main")
    
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
}



.create_1d_ice_plot <- function(facet_data,
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
                                y_breaks,
                                y_n_breaks,
                                value_scales,
                                value_range,
                                novelty_range,
                                conf_int_style,
                                conf_int_alpha,
                                ice_default_alpha,
                                show_novelty,
                                show_ice,
                                show_pd,
                                outcome_type,
                                ...){
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_x <- color_breaks <- sample <- NULL
  
  # Flag indicating presence of data.
  data_present <- TRUE
  
  # Get the data that determines the main plot characteristics.
  if(show_ice){
    if(!is_empty(ice_data[[1]])){
      plot_data <- data.table::copy(ice_data[[1]]@data)
      
    } else {
      plot_data <- facet_data
      data_present <- FALSE
    }
    
  } else {
    if(!is_empty(pd_data[[1]])){
      plot_data <- data.table::copy(pd_data[[1]]@data)
      
    } else {
      plot_data <- facet_data
      data_present <- FALSE
    }
  }
  
  # Set value range
  if(is.null(value_range) & value_scales == "facet" & data_present){
    if(show_ice){
      value_range <- .create_ice_plot_value_range(x=ice_data,
                                                  scale_method="facet",
                                                  outcome_type=outcome_type,
                                                  confidence_interval=conf_int_style != "none")
    } else {
      value_range <- .create_ice_plot_value_range(x=pd_data,
                                                  scale_method="facet",
                                                  outcome_type=outcome_type,
                                                  confidence_interval=conf_int_style != "none")
    }
  }
  
  # x_range
  if(is.numeric(plot_data$feature_x_value)){
    if(is.null(x_range)) x_range <- c(min(plot_data$feature_x_value),
                                      max(plot_data$feature_x_value))
    
    # x_breaks
    if(is.null(x_breaks)){
      plotting.check_input_args(x_n_breaks=x_n_breaks)
      
      # Create breaks and update x_range
      x_breaks <- labeling::extended(m=x_n_breaks,
                                     dmin=x_range[1],
                                     dmax=x_range[2],
                                     only.loose=TRUE)
      
      # Update x_range
      x_range  <- c(head(x_breaks, n=1),
                    tail(x_breaks, n=1))
    }
    
    plotting.check_input_args(x_range=x_range)
    
  } else {
    x_range <- NULL
  }
 
  # Find the correct y-range
  value_range <- value_range[feature_x == as.character(plot_data$feature_x[1])]
  if(is_empty(value_range)){
    y_range <- c(NA_real_, NA_real_)
    
  } else {
    y_range <- c(value_range$min_value, value_range$max_value)
    
    # y_breaks
    if(is.null(y_breaks)){
      plotting.check_input_args(y_n_breaks=y_n_breaks)
      
      # Create breaks and update y_range
      y_breaks <- labeling::extended(m=y_n_breaks,
                                     dmin=y_range[1],
                                     dmax=y_range[2],
                                     only.loose=TRUE)
      
      # Update y-range
      y_range <- c(head(y_breaks, n=1),
                   tail(y_breaks, n=1))
    }
  }
  
  plotting.check_input_args(y_range=y_range)
  
  # Set x-label
  if(is.waive(x_label)){
    if(!is.na(plot_data$feature_x[1])){
      x_label <- as.character(plot_data$feature_x[1])
    } else {
      x_label <- NULL
    }
  }
  
  # Set y-label
  if(is.waive(y_label)) y_label <- switch(outcome_type,
                                          "binomial"="probability",
                                          "multinomial"="probability",
                                          "count"="value",
                                          "continuous"="value",
                                          "survival"="survival probability",
                                          "competing_risk"="survival probability")
  
  plotting.check_input_args(x_label=x_label,
                            y_label=y_label)
  
  # Update show_novelty to check for non-finite values in the novelty data.
  show_novelty <- show_novelty & all(is.finite(plot_data$novelty))
  
  if(show_novelty){
    # Find the correct novelty-range
    novelty_range <- novelty_range[feature_x == as.character(plot_data$feature_x[1])]
    
    if(is_empty(novelty_range)){
      novelty_range <- c(NA_real_, NA_real_)
      
    } else {
      novelty_range <- c(novelty_range$min_value, novelty_range$max_value)
    }
  }
  
  # Generate a guide table for ice data to allow integration of guides into a
  # single legend.
  if(show_ice & data_present){
    ice_guide_list <- plotting.create_guide_table(x=ice_data[[1]]@data,
                                                  color_by=color_by,
                                                  discrete_palette=discrete_palette,
                                                  combine_legend=FALSE)
    
    # Add column to force proper grouping of lines.
    ice_guide_list$data[, "color_breaks_sample":=paste0(color_breaks, "_", sample)]
    
  } else {
    ice_guide_list <- NULL
  }
  
  # Generate a guide table for pd data to allow integration of guides into a
  # single legend.
  if(show_pd & data_present){
    pd_guide_list <- plotting.create_guide_table(x=pd_data[[1]]@data,
                                                 color_by=color_by,
                                                 discrete_palette=discrete_palette,
                                                 combine_legend=FALSE)
    
    # Set grouping variable to deal with point-group complaints.
    if(is.factor(pd_guide_list$data$feature_x_value)){
      pd_group_variable <- 1
    } else {
      pd_group_variable <- NULL
    }
    
  } else {
    pd_guide_list <- NULL
  }
  
  
  # Make pd line thicker than ice lines.
  ice_line_size <- 0.5 * ..get_plot_theme_linewidth(ggtheme=ggtheme)
  pd_line_size <- 6 * ice_line_size
  
  # In case only partial dependency plots are shown, update ice_default_alpha
  if(!show_ice) ice_default_alpha <- 1.0
  
  # Create basic plot.
  p <- ggplot2::ggplot(data=plot_data,
                       mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                            y=!!sym("value")))
  p <- p + ggtheme
  
  if(!all(is.finite(y_range))){
    # Blank elements in case the value range is unset, e.g. because the model
    # cannot compute survival probabilities. This happens for some mboost
    # learners.
    p <- p + ggplot2::geom_blank()
    
  } else if(show_novelty){
    # Plot with novelty.
    
    if(utils::packageVersion("ggplot2") >= "3.4.0"){
      # Version 3.4.0 introduces the linewidth element for geom_line, and will
      # produce deprecation warnings if the size argument is used instead.
      
      if(show_ice){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              colour=!!sym("color_breaks"),
              group=!!sym("color_breaks_sample")),
            size=ice_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("sample"),
              alpha=!!sym("novelty")),
            size=ice_line_size)
        }
      }
      
      if(show_pd){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              colour=!!sym("color_breaks"),
              group=pd_group_variable),
            size=pd_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              group=pd_group_variable),
            size=pd_line_size)
        }
      }
    } else {
      # For backward compatibility with versions of ggplot2 before version
      # 3.4.0.
      if(show_ice){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              colour=!!sym("color_breaks"),
              group=!!sym("color_breaks_sample")),
            size=ice_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("sample"),
              alpha=!!sym("novelty")),
            size=ice_line_size)
        }
      }
      
      if(show_pd){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              colour=!!sym("color_breaks"),
              group=pd_group_variable),
            size=pd_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              alpha=!!sym("novelty"),
              group=pd_group_variable),
            size=pd_line_size)
        }
      }
    }
    
    # Invert novelty values, since higher values indicates greater novelty.
    p <- p + ggplot2::scale_alpha(trans="reverse",
                                  limits=novelty_range,
                                  range=c(0.1, 1.0) * ice_default_alpha)
    
  } else {
    # Plot without novelty.
    
    if(utils::packageVersion("ggplot2") >= "3.4.0"){
      # Version 3.4.0 introduces the linewidth element for geom_line, and will
      # produce deprecation warnings if the size argument is used instead.
      if(show_ice){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("color_breaks_sample"),
              colour=!!sym("color_breaks")),
            linewidth=ice_line_size,
            alpha=ice_default_alpha)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("sample")),
            linewidth=ice_line_size,
            alpha=ice_default_alpha)
        }
      }
      
      if(show_pd){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              colour=!!sym("color_breaks"),
              group=pd_group_variable),
            linewidth=pd_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=pd_group_variable),
            linewidth=pd_line_size)
        }
      }
      
    } else {
      # For backwards compatibility with ggplot2 versions prior to version
      # 3.4.0.
      if(show_ice){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("color_breaks_sample"),
              colour=!!sym("color_breaks")),
            size=ice_line_size,
            alpha=ice_default_alpha)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=ice_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=!!sym("sample")),
            size=ice_line_size,
            alpha=ice_default_alpha)
        }
      }
      
      if(show_pd){
        if(!is.null(color_by)){
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              colour=!!sym("color_breaks"),
              group=pd_group_variable),
            size=pd_line_size)
          
        } else {
          # Create lines with alpha.
          p <- p + ggplot2::geom_line(
            data=pd_guide_list$data,
            mapping=ggplot2::aes(
              x=!!sym("feature_x_value"),
              y=!!sym("value"),
              group=pd_group_variable),
            size=pd_line_size)
        }
      }
    }
  }
  
  # Set colour
  if(!is.null(color_by)){
    # Extract guide_table for colour.
    if(show_ice){
      g_color <- ice_guide_list$guide_color
    } else {
      g_color <- pd_guide_list$guide_color
    }
    
    p <- p + ggplot2::scale_colour_manual(
      name=legend_label$guide_color,
      values=g_color$color_values,
      breaks=g_color$color_breaks,
      drop=FALSE)
  }
  
  # Update x and y scales
  if(!is.null(x_range)) p <- p + ggplot2::scale_x_continuous(breaks=x_breaks)
  if(!is.null(y_range)) p <- p + ggplot2::scale_y_continuous(breaks=y_breaks)
  
  # Plot confidence intervals.
  if(conf_int_style[1]!="none"){
    if(conf_int_style[1] == "step"){
      if(is.null(color_by)){
        p <- p + ggplot2::geom_step(data=pd_guide_list$data,
                                    mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                         y=!!sym("value_ci_low")),
                                    linetype="dashed")
        
        p <- p + ggplot2::geom_step(data=pd_guide_list$data,
                                    mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                         y=!!sym("value_ci_up")),
                                    linetype="dashed")
        
      } else {
        p <- p + ggplot2::geom_step(data=pd_guide_list$data,
                                    mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                         y=!!sym("value_ci_low"),
                                                         colour=!!sym("color_breaks")),
                                    linetype="dashed")
        
        p <- p + ggplot2::geom_step(data=pd_guide_list$data,
                                    mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                         y=!!sym("value_ci_up"),
                                                         colour=!!sym("color_breaks")),
                                    linetype="dashed")
      }
      
      
      # Remove linetype from the legend.
      p <- p + ggplot2::scale_linetype(guide=FALSE)
      
    } else if(conf_int_style[1] == "ribbon"){
      
      if(show_novelty) conf_int_alpha <- conf_int_alpha * ice_default_alpha
      
      if(is.null(color_by)){
        p <- p + ggplot2::geom_ribbon(data=pd_guide_list$data,
                                      mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                           ymin=!!sym("value_ci_low"),
                                                           ymax=!!sym("value_ci_up")),
                                      alpha=conf_int_alpha)
        
      } else {
        p <- p + ggplot2::geom_ribbon(data=pd_guide_list$data,
                                      mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                                           ymin=!!sym("value_ci_low"),
                                                           ymax=!!sym("value_ci_up"),
                                                           fill=!!sym("color_breaks")),
                                      alpha=conf_int_alpha)
      }
    }
  }
  
  # Determine how things are facetted.
  facet_by_list <- plotting.parse_facet_by(x=plot_data, 
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
  
  # Update labels.
  p <- p + ggplot2::labs(x=x_label,
                         y=y_label,
                         title=plot_title,
                         subtitle=plot_sub_title,
                         caption=caption)
  
  # Plot to Cartesian coordinates.
  p <- p + ggplot2::coord_cartesian(xlim=x_range, ylim=y_range)
  
  # Rotate x-axis ticks
  # if(rotate_x_tick_labels){
  #   p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
  # }
  
  return(p)
}



.create_2d_ice_plot <- function(facet_data,
                                pd_data,
                                facet_by,
                                facet_wrap_cols,
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
                                x_range,
                                x_breaks,
                                x_n_breaks,
                                y_range,
                                y_breaks,
                                y_n_breaks,
                                value_scales,
                                value_range,
                                novelty_range,
                                conf_int_style,
                                conf_int_alpha,
                                show_novelty,
                                outcome_type,
                                ...){
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_x <- feature_y <- feature_x_value <- feature_y_value <- NULL
  
  # Get the data that determines the plot characteristics.
  if(!is_empty(pd_data[[1]])){
    plot_data <- data.table::copy(pd_data[[1]]@data)[order(feature_x_value, feature_y_value)]
    
  } else {
    plot_data <- facet_data
  }
  
  # Set value range
  if(is.null(value_range) & value_scales == "facet"){
      value_range <- .create_ice_plot_value_range(x=pd_data,
                                                  scale_method="facet",
                                                  outcome_type=outcome_type,
                                                  confidence_interval=conf_int_style != "none")
      
  } else if(is.null(value_range)){
    ..error_reached_unreachable_code(".create_2d_ice_plot: No value range was set, and value_scales was not recognised either.")
  }
  
  # x_range
  if(is.numeric(plot_data$feature_x_value)){
    if(is.null(x_range)) x_range <- c(min(plot_data$feature_x_value, na.rm=TRUE),
                                      max(plot_data$feature_x_value, na.rm=TRUE))
    
    # x_breaks
    if(is.null(x_breaks)){
      plotting.check_input_args(x_n_breaks=x_n_breaks)
      
      # Create breaks and update x_range
      x_breaks <- labeling::extended(m=x_n_breaks,
                                     dmin=x_range[1],
                                     dmax=x_range[2],
                                     only.loose=TRUE)
      
      # Update x_range
      x_range  <- c(head(x_breaks, n=1),
                    tail(x_breaks, n=1))
    }
    
    plotting.check_input_args(x_range=x_range)
    
  } else {
    x_range <- NULL
  }
  
  # y_range
  if(is.numeric(plot_data$feature_y_value)){
    if(is.null(y_range)) y_range <- c(min(plot_data$feature_y_value, na.rm=TRUE),
                                      max(plot_data$feature_y_value, na.rm=TRUE))
    
    # y_breaks
    if(is.null(y_breaks)){
      plotting.check_input_args(y_n_breaks=y_n_breaks)
      
      # Create breaks and update y_range
      y_breaks <- labeling::extended(m=y_n_breaks,
                                     dmin=y_range[1],
                                     dmax=y_range[2],
                                     only.loose=TRUE)
      
      # Update y_range
      y_range  <- c(head(y_breaks, n=1),
                    tail(y_breaks, n=1))
    }
    
    plotting.check_input_args(y_range=y_range)
    
  } else {
    y_range <- NULL
  }
  
  # Find the correct value-range
  value_range <- value_range[feature_x == as.character(plot_data$feature_x[1]) &
                               feature_y == as.character(plot_data$feature_y[1])]
  if(is_empty(value_range)){
    value_range <- c(NA_real_, NA_real_)
    
  } else {
    value_range <- c(value_range$min_value, value_range$max_value)
  }
  
  
  # Update show_novelty to check for non-finite values in the novelty data.
  show_novelty <- show_novelty & all(is.finite(plot_data$novelty))
  
  if(show_novelty){
    # Find the correct novelty-range
    novelty_range <- novelty_range[feature_x == as.character(plot_data$feature_x[1]) &
                                     feature_y == as.character(plot_data$feature_y[1])]
    if(is_empty(novelty_range)){
      novelty_range <- c(NA_real_, NA_real_)
      
    } else {
      novelty_range <- c(novelty_range$min_value, novelty_range$max_value)
    }
  }
  
  # Set x-label
  if(is.waive(x_label)){
    if(!is.na(plot_data$feature_x[1])){
      x_label <- as.character(plot_data$feature_x[1])
    } else {
      x_label <- NULL
    }
  }
  
  # Set y-label
  if(is.waive(y_label)){
    if(!is.na(plot_data$feature_y[1])){
      y_label <- as.character(plot_data$feature_y[1])
    } else {
      y_label <- NULL
    }
  }
  
  plotting.check_input_args(x_label=x_label,
                            y_label=y_label)
  
  # Create basic plot.
  p <- ggplot2::ggplot(data=plot_data,
                       mapping=ggplot2::aes(x=!!sym("feature_x_value"),
                                            y=!!sym("feature_y_value")))
  p <- p + ggtheme
  
  if(!all(is.finite(value_range))){
    # Blank elements in case the value range is unset, e.g. because the model
    # cannot compute survival probabilities. This happens for some mboost
    # learners.
    p <- p + ggplot2::geom_blank()
    p <- p + ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                            axis.ticks.x=ggplot2::element_blank(),
                            axis.text.x=ggplot2::element_blank(),
                            axis.line.y = ggplot2::element_blank(),
                            axis.ticks.y=ggplot2::element_blank(),
                            axis.text.y=ggplot2::element_blank())
    
  } else if(show_novelty){
    # Create point cloud with size of points by novelty -> bubblechart.
    p <- p + ggplot2::geom_point(data=plot_data,
                                 mapping=ggplot2::aes(colour=!!sym("value"),
                                                      size=!!sym("novelty")))
    
    # Invert novelty values, since higher values indicates greater novelty.
    p <- p + ggplot2::scale_size(trans="reverse",
                                 limits=novelty_range)
    
  } else {
    # Set colour for value points.
    if(!is.null(discrete_palette)){
      colour <- plotting.get_palette(x=discrete_palette,
                                     n=1L,
                                     palette_type="qualitative")
      
    } else {
      colour <- "white"
    }
    
    # Specify coordinates for rectangle.
    plot_data[, c("xmin", "xmax"):=..set_edge_points(feature_x_value, range=x_range, type="x"), by="feature_y_value"]
    plot_data[, c("ymin", "ymax"):=..set_edge_points(feature_y_value, range=y_range, type="y"), by="feature_x_value"]
    
    # Insert points. This will help set up the figure - otherwise the plot
    # cannot be drawn.
    p <- p + ggplot2::geom_point()
    
    # Create raster in case novelty is not or cannot be shown.
    p <- p + ggplot2::geom_rect(data=plot_data,
                                mapping=ggplot2::aes(xmin=!!sym("xmin"),
                                                     xmax=!!sym("xmax"),
                                                     ymin=!!sym("ymin"),
                                                     ymax=!!sym("ymax"),
                                                     fill=!!sym("value")))
    
    # Draw points on top.
    p <- p + ggplot2::geom_point(colour=colour)
  }
  
  # Set colours used for plotting 
  gradient_colours <- plotting.get_palette(x=gradient_palette,
                                           palette_type="sequential")
  
  # Add gradient palette as fill.
  p <- p + ggplot2::scale_fill_gradientn(name=legend_label,
                                         colors=gradient_colours,
                                         limits=value_range)
  
  # Update x and y scales
  if(!is.null(x_range)) p <- p + ggplot2::scale_x_continuous(breaks=x_breaks)
  if(!is.null(y_range)) p <- p + ggplot2::scale_y_continuous(breaks=y_breaks)
  
  # Determine how things are facetted.
  facet_by_list <- plotting.parse_facet_by(x=plot_data, 
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
  
  # Update labels.
  p <- p + ggplot2::labs(x=x_label,
                         y=y_label,
                         title=plot_title,
                         subtitle=plot_sub_title,
                         caption=caption)
  
  # Plot to Cartesian coordinates.
  p <- p + ggplot2::coord_cartesian(xlim=x_range, ylim=y_range)
  
  # Rotate x-axis ticks
  # if(rotate_x_tick_labels){
  #   p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(vjust=0.25, hjust=1.0, angle=90.0))
  # }
  
  return(p)
}



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




.create_ice_plot_value_range <- function(x, scale_method, outcome_type, confidence_interval=FALSE){
  # Suppress NOTES due to non-standard evaluation in data.table
  min_value <- max_value <- NULL
  
  if(is_empty(x)) return(c(NA_real_, NA_real_))
  
  # Obtain value ranges.
  value_range <- lapply(x, ..create_ice_plot_value_range, confidence_interval=confidence_interval)
  value_range <- data.table::rbindlist(value_range, use.names=TRUE)
  
  if(scale_method %in% c("fixed", "figure")){
    if(outcome_type %in% c("binomial", "multinomial", "survival")){
      # Check if probabilities all lie in the expected range.
      if(all(value_range$min_value >= 0.0) & all(value_range$max_value <= 1.0)){
        value_range[, ":="("min_value"=0.0,
                           "max_value"=1.0)]
        
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
    ..error_reached_unreachable_code(paste0(".create_ice_plot_value_range: unknown scale_method encountered: ", scale_method))
  }

  return(value_range)
}



..create_ice_plot_value_range <- function(x, confidence_interval){
  # Extract value range information.
  
  # Get the x-feature
  value_range <- list("feature_x"=x@identifiers$feature_x)
  
  # Get the y-feature, if present.
  if(!is.null(x@identifiers$feature_y)) value_range <- c(value_range, list("feature_y"=x@identifiers$feature_y))
  
  # Get minimum and maximum values.
  if(confidence_interval){
    value_range <- c(value_range, list("min_value"=min(x@data$value_ci_low),
                                       "max_value"=max(x@data$value_ci_up)))
  } else {
    value_range <- c(value_range, list("min_value"=min(x@data$value),
                                       "max_value"=max(x@data$value)))
  }

  # Return as data.table.
  return(data.table::as.data.table(value_range))
}



.create_ice_plot_novelty_range <- function(x, scale_method, outcome_type){
  # Suppress NOTES due to non-standard evaluation in data.table
  min_novelty <- max_novelty <- NULL
  
  if(is_empty(x)) return(c(NA_real_, NA_real_))
  
  # Obtain value ranges.
  novelty_range <- lapply(x, ..create_ice_plot_novelty_range)
  novelty_range <- data.table::rbindlist(novelty_range, use.names=TRUE)
  
  if(any(is.finite(novelty_range$min_novelty)) & any(is.finite(novelty_range$max_novelty))){
    novelty_range[, ":="("min_novelty"=min(min_novelty, na.rm=TRUE),
                         "max_novelty"=max(max_novelty, na.rm=TRUE))]
  } else {
    novelty_range[, ":="("min_novelty"=NA_real_,
                         "max_novelty"=NA_real_)]
  }
  
  return(novelty_range)
}



..create_ice_plot_novelty_range <- function(x){
  # Extract novelty range information.
  
  # Get the x-feature
  novelty_range <- list("feature_x"=x@identifiers$feature_x)
  
  # Get the y-feature, if present.
  if(!is.null(x@identifiers$feature_y)) novelty_range <- c(novelty_range, list("feature_y"=x@identifiers$feature_y))
  
  if(any(is.finite(x@data$novelty))){
    # Get minimum and maximum novelty features.
    novelty_range <- c(novelty_range, list("min_novelty"=min(x@data$novelty, na.rm=TRUE),
                                           "max_novelty"=max(x@data$novelty, na.rm=TRUE)))
    
  } else {
    novelty_range <- c(novelty_range, list("min_novelty"=NA_real_,
                                           "max_novelty"=NA_real_))
  }
  
  # Return as data.table.
  return(data.table::as.data.table(novelty_range))
}



.select_ice_plot_data <- function(x, data){
  # Check that data is not empty.
  if(is_empty(data)) return(NULL)

  return(lapply(split(x, by="list_id"), ..select_ice_plot_data, data=data))
}



..select_ice_plot_data <- function(x, data){
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- NULL
  
  # Get list identifier.
  selected_list_index <- x$list_id[1]
  
  # Check if data exists or is NA.
  if(is.na(selected_list_index)) return(NULL)
  
  # Select the correct data element.
  data <- data[[selected_list_index]]
  
  # Select the subset of the data.
  data@data <- data@data[x, on=.NATURAL]
  
  return(data)
}
