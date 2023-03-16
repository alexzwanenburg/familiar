#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# plot_auc_roc_curve (generic) -------------------------------------------------

#' @title Plot the receiver operating characteristic curve.
#'
#' @description This method creates receiver operating characteristic curves
#'  based on data in a familiarCollection object.
#'
#' @param dir_path (*optional*) Path to the directory where the plots of receiver
#'  operating characteristic curves are saved to. Output is saved in the
#'  `performance` subdirectory. If `NULL` no figures are saved, but are returned
#'  instead.
#' @param discrete_palette (*optional*) Palette to use to color the different
#'  plot elements in case a value was provided to the `color_by` argument.
#'
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#' @inheritDotParams extract_auc_data -object
#'
#' @details This function generates area under the ROC curve plots.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `positive_class`. By default, the data is split by `fs_method` and `learner`,
#'  with faceting by `data_set` and colouring by `positive_class`.
#'
#'  Available palettes for `discrete_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'  Bootstrap confidence intervals of the ROC curve (if present) can be shown
#'  using various styles set by `conf_int_style`:
#'
#'  * `ribbon` (default): confidence intervals are shown as a ribbon with an
#'  opacity of `conf_int_alpha` around the point estimate of the ROC curve.
#'
#'  * `step` (default): confidence intervals are shown as a step function around
#'  the point estimate of the ROC curve.
#'
#'  * `none`: confidence intervals are not shown. The point estimate of the ROC
#'  curve is shown as usual.
#'
#'  Labelling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_auc_roc_curve
#' @md
#' @rdname plot_auc_roc_curve-methods
setGeneric(
  "plot_auc_roc_curve",
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    standardGeneric("plot_auc_roc_curve")
  }
)

# plot_auc_roc_curve (general) -------------------------------------------------

#' @rdname plot_auc_roc_curve-methods
setMethod(
  "plot_auc_roc_curve",
  signature(object = "ANY"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "auc_data"),
        list(...)))
    
    return(do.call(
      plot_auc_roc_curve,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "color_by" = color_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "discrete_palette" = discrete_palette,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)


# plot_auc_roc_curve (collection) ----------------------------------------------

#' @rdname plot_auc_roc_curve-methods
setMethod(
  "plot_auc_roc_curve",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(do.call(
      .plot_auc_curve,
      args = list(
        "object" = object,
        "curve_type" = "roc",
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "color_by" = color_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "discrete_palette" = discrete_palette,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



# plot_auc_precision_recall_curve (generic) ------------------------------------

#' @title Plot the precision-recall curve.
#'
#' @description This method creates precision-recall curves based on data in a
#'  familiarCollection object.
#'
#' @inheritParams plot_auc_roc_curve
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#'
#' @details This function generates area under the precision-recall curve plots.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `positive_class`. By default, the data is split by `fs_method` and `learner`,
#'  with faceting by `data_set` and colouring by `positive_class`.
#'
#'  Available palettes for `discrete_palette` are those listed by
#'  `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'  (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'  `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'  name in `grDevices`. If not specified, a default palette based on palettes
#'  in Tableau are used. You may also specify your own palette by using colour
#'  names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'  Bootstrap confidence intervals of the ROC curve (if present) can be shown
#'  using various styles set by `conf_int_style`:
#'
#'  * `ribbon` (default): confidence intervals are shown as a ribbon with an
#'  opacity of `conf_int_alpha` around the point estimate of the ROC curve.
#'
#'  * `step` (default): confidence intervals are shown as a step function around
#'  the point estimate of the ROC curve.
#'
#'  * `none`: confidence intervals are not shown. The point estimate of the ROC
#'  curve is shown as usual.
#'
#'  Labelling methods such as `set_fs_method_names` or `set_data_set_names` can
#'  be applied to the `familiarCollection` object to update labels, and order
#'  the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_auc_precision_recall_curve
#' @md
#' @rdname plot_auc_precision_recall_curve-methods
setGeneric(
  "plot_auc_precision_recall_curve",
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    standardGeneric("plot_auc_precision_recall_curve")
  }
)



# plot_auc_precision_recall_curve (general) ------------------------------------

#' @rdname plot_auc_precision_recall_curve-methods
setMethod(
  "plot_auc_precision_recall_curve",
  signature(object = "ANY"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "auc_data"),
        list(...)))
    
    return(do.call(
      plot_auc_precision_recall_curve,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "color_by" = color_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "discrete_palette" = discrete_palette,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



# plot_auc_precision_recall_curve (collection) ---------------------------------

#' @rdname plot_auc_precision_recall_curve-methods
setMethod(
  "plot_auc_precision_recall_curve",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(do.call(
      .plot_auc_curve,
      args = list(
        "object" = object,
        "curve_type" = "pr",
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "color_by" = color_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "discrete_palette" = discrete_palette,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



.plot_auc_curve <- function(
    object,
    curve_type,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = NULL,
    plot_sub_title = NULL,
    caption = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_n_breaks = 5,
    y_breaks = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
  
  # Get input data.
  x <- export_auc_data(
    object = object,
    aggregate_results = TRUE)
  
  # Check that the data are not empty.
  if (is_empty(x)) return(NULL)
  
  # Check that the data are not evaluated at the model level.
  if (all(sapply(x, function(x) (x@detail_level == "model")))) {
    ..warning_no_comparison_between_models()
    return(NULL)
  }
  
  # Obtain data element from list.
  if (is.list(x)) {
    if (is_empty(x)) return(NULL)
    
    if (length(x) > 1) {
      ..error_reached_unreachable_code(
        ".plot_auc_curve: list of data elements contains unmerged elements.")
    }
    
    # Get x directly.
    x <- x[[1]]
  }
  
  # Check that the data are not empty.
  if (is_empty(x)) return(NULL)
  
  # Check that curve type is correctly specified by the coder.
  .check_parameter_value_is_valid(
    x = curve_type,
    var_name = "curve_type",
    values = c("roc", "pr"))
  
  # Select the correct subset.
  if (curve_type == "roc") {
    x@data <- x@data[curve_type == "roc"]
  } else {
    x@data <- x@data[curve_type == "pr"]
  }
  
  # Check that the data are not empty after selecting the subset.
  if (is_empty(x)) return(NULL)
  
  # Check package requirements for plotting.
  if (!require_package(
    x = ..required_plotting_packages(extended = FALSE),
    purpose = ifelse(curve_type == "roc",
                     "to create AUC-ROC plots",
                     "to create AUC-PR plots"),
    message_type = "warning")) {
    return(NULL)
  }
  
  # Check input arguments ------------------------------------------------------
  
  # ggtheme
  ggtheme <- .check_ggtheme(ggtheme)
  
  # x_label
  if (is.waive(x_label) && curve_type == "roc") x_label <- "1 - specificity"
  if (is.waive(x_label) && curve_type == "pr") x_label <- "recall"
  
  # y_label
  if (is.waive(y_label) && curve_type == "roc") y_label <- "sensitivity"
  if (is.waive(y_label) && curve_type == "pr") y_label <- "precision"
  
  # x_range and y_range
  x_range <- y_range <- c(0.0, 1.0)
  
  # x_breaks
  if (is.null(x_breaks)) {
    .check_input_plot_args(x_n_breaks = x_n_breaks)
    
    # Create breaks
    x_breaks <- labeling::extended(
      m = x_n_breaks,
      dmin = x_range[1],
      dmax = x_range[2],
      only.loose = TRUE)
  }
  
  # y_breaks
  if (is.null(y_breaks)) {
    .check_input_plot_args(y_n_breaks = y_n_breaks)
    
    # Create breaks
    y_breaks <- labeling::extended(
      m = y_n_breaks,
      dmin = y_range[1],
      dmax = y_range[2],
      only.loose = TRUE)
  }
  
  # conf_int_style
  if (length(conf_int_style) > 1) {
    conf_int_style <- head(conf_int_style, n = 1)
  }
  
  # Set the style of the confidence interval to none, in case no confidence
  # interval data is present.
  if (!x@estimation_type %in% c("bci", "bootstrap_confidence_interval")) {
    conf_int_style <- "none"
  }
  
  # Splitting variables
  if (is.null(split_by) && is.null(facet_by) && is.null(color_by)) {
    # Determine the number of learners and feature_selection methods.
    n_learner <- nlevels(x@data$learner)
    n_fs_method <- nlevels(x@data$fs_method)
    n_positive_class <- ifelse(
      object@outcome_type == "binomial",
      1L, nlevels(x@data$positive_class))
    
    if (n_learner > 1 && n_fs_method > 1) {
      split_by <- c("fs_method", "learner")
      
      if (n_positive_class > 1) {
        color_by <- "positive_class"
        facet_by <- "data_set"
      } else {
        color_by <- c("data_set", "positive_class")
      }
      
    } else if (n_learner > 1) {
      # Implying n_fs_method == 1
      
      if (n_positive_class > 1) {
        split_by <- c("fs_method", "learner")
        color_by <- "positive_class"
        facet_by <- "data_set"
      } else {
        split_by <- c("fs_method")
        color_by <- c("learner")
        facet_by <- c("data_set", "positive_class")
      }
      
    } else if (n_fs_method > 1) {
      # Implying n_learner == 1
      
      if (n_positive_class > 1) {
        split_by <- c("fs_method", "learner")
        color_by <- "positive_class"
        facet_by <- "data_set"
      } else {
        split_by <- "learner"
        color_by <- "fs_method"
        facet_by <- c("data_set", "positive_class")
      }
      
    } else {
      # Implying n_learner == n_fs_method == 1
      split_by <- c("fs_method", "learner")
      
      if (n_positive_class > 1) {
        color_by <- "positive_class"
        facet_by <- "data_set"
      } else {
        color_by <- c("data_set", "positive_class")
      }
    }
  }
  
  # Check splitting variables and generate sanitised output
  split_var_list <- .check_plot_splitting_variables(
    x = x@data,
    split_by = split_by,
    color_by = color_by,
    facet_by = facet_by,
    available = c("fs_method", "learner", "data_set", "positive_class"))
  
  # Update splitting variables
  split_by <- split_var_list$split_by
  color_by <- split_var_list$color_by
  facet_by <- split_var_list$facet_by
  
  # Create a legend label
  legend_label <- .create_plot_legend_title(
    user_label = legend_label,
    color_by = color_by)
  
  # Check input arguments for validity.
  .check_input_plot_args(
    x_range = x_range,
    y_range = y_range,
    x_breaks = x_breaks,
    y_breaks = y_breaks,
    conf_int_alpha = conf_int_alpha,
    conf_int_style = conf_int_style,
    facet_wrap_cols = facet_wrap_cols,
    x_label = x_label,
    y_label = y_label,
    legend_label = legend_label,
    plot_title = plot_title,
    plot_sub_title = plot_sub_title,
    caption = caption)
  
  # Create plots ---------------------------------------------------------------
  
  # Determine if subtitle should be generated.
  autogenerate_plot_subtitle <- is.waive(plot_sub_title)
  
  # Split data
  if (!is.null(split_by)) {
    x_split <- split(x@data, by = split_by)
  } else {
    x_split <- list("null.name" = x@data)
  }
  
  # Store plots to list in case dir_path is absent.
  if (is.null(dir_path)) plot_list <- list()
  
  # Iterate over splits
  for (ii in names(x_split)) {
    # Skip empty datasets
    if (is_empty(x_split[[ii]])) next()
    
    if (is.waive(plot_title)) {
      plot_title <- ifelse(
        curve_type == "roc",
        "Receiver operating characteristic curve",
        "Precision-recall curve")
    }
    
    if (autogenerate_plot_subtitle) {
      plot_sub_title <- .create_plot_subtitle(
        split_by = split_by,
        x = x_split[[ii]])
    }
    
    # Generate plot
    p <- ..plot_auc_curve(
      x = x_split[[ii]],
      color_by = color_by,
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      ggtheme = ggtheme,
      discrete_palette = discrete_palette,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      x_range = x_range,
      x_breaks = x_breaks,
      y_range = y_range,
      y_breaks = y_breaks,
      conf_int_style = conf_int_style,
      conf_int_alpha = conf_int_alpha
    )
    
    # Check empty output
    if (is.null(p)) next
    
    # Draw figure.
    if (draw) .draw_plot(plot_or_grob = p)
    
    # Save and export
    if (!is.null(dir_path)) {
      # Obtain decent default values for the plot.
      def_plot_dims <- .determine_auc_roc_plot_dimensions(
        x = x_split[[ii]],
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols)
      
      # Save to file.
      do.call(
        .save_plot_to_file,
        args = c(
          list(
            "plot_or_grob" = p,
            "object" = object,
            "dir_path" = dir_path,
            "type" = "performance",
            "subtype" = paste0("auc_", curve_type),
            "x" = x_split[[ii]],
            "split_by" = split_by,
            "height" = ifelse(is.waive(height), def_plot_dims[1], height),
            "width" = ifelse(is.waive(width), def_plot_dims[2], width),
            "units" = ifelse(is.waive(units), "cm", units)),
          list(...)))
      
    } else {
      # Store as list for export.
      plot_list <- c(plot_list, list(p))
    }
  }
  
  # Generate output
  return(.get_plot_results(
    dir_path = dir_path,
    plot_list = plot_list,
    export_collection = export_collection,
    object = object))
}



..plot_auc_curve <- function(
    x,
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
    conf_int_alpha) {
  # Generate a guide table.
  guide_list <- .create_plot_guide_table(
    x = x, color_by = color_by,
    discrete_palette = discrete_palette)
  
  # Extract data
  x <- guide_list$data
  
  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("x"),
      y = !!sym("y")))
  
  # Add theme
  p <- p + ggtheme
  
  # Add AUC curve
  if (is.null(color_by)) {
    # Without colour-based splitting.
    p <- p + ggplot2::geom_line()
    
  } else {
    # With colour-based splitting.
    p <- p + ggplot2::geom_line(
      mapping = ggplot2::aes(colour = !!sym("color_breaks")))
    
    # Extract guidetable for color
    g_color <- guide_list$guide_color
    
    # Set colour and fill (fill may be unused)
    p <- p + ggplot2::scale_colour_manual(
      name = legend_label$guide_color,
      values = g_color$color_values,
      breaks = g_color$color_breaks,
      drop = FALSE)
    
    p <- p + ggplot2::scale_fill_manual(
      name = legend_label$guide_color,
      values = g_color$color_values,
      breaks = g_color$color_breaks,
      drop = FALSE)
  }
  
  # Plot confidence intervals
  if (conf_int_style[1] != "none") {
    if (conf_int_style[1] == "step") {
      if (is.null(color_by)) {
        p <- p + ggplot2::geom_step(
          mapping = ggplot2::aes(y = !!sym("ci_low")),
          linetype = "dashed")
        
        p <- p + ggplot2::geom_step(
          mapping = ggplot2::aes(y = !!sym("ci_up")),
          linetype = "dashed")
        
      } else {
        p <- p + ggplot2::geom_step(
          mapping = ggplot2::aes(
            y = !!sym("ci_low"),
            colour = !!sym("color_breaks")),
          linetype = "dashed")
        
        p <- p + ggplot2::geom_step(
          mapping = ggplot2::aes(
            y = !!sym("ci_up"),
            colour = !!sym("color_breaks")),
          linetype = "dashed")
      }
      
      # Do not show dashed lines in the legend.
      p <- p + ggplot2::scale_linetype(guide = FALSE)
      
    } else if (conf_int_style[1] == "ribbon") {
      if (is.null(color_by)) {
        p <- p + ggplot2::geom_ribbon(
          mapping = ggplot2::aes(
            ymin = !!sym("ci_low"),
            ymax = !!sym("ci_up")),
          alpha = conf_int_alpha)
        
      } else {
        p <- p + ggplot2::geom_ribbon(
          mapping = ggplot2::aes(
            ymin = !!sym("ci_low"),
            ymax = !!sym("ci_up"),
            fill = !!sym("color_breaks")),
          alpha = conf_int_alpha)
      }
    }
  }
  
  # Update x and y scales
  p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
  p <- p + ggplot2::scale_y_continuous(breaks = y_breaks)
  
  # Labels
  p <- p + ggplot2::labs(
    x = x_label,
    y = y_label,
    title = plot_title,
    subtitle = plot_sub_title,
    caption = caption)
  
  # Determine how things are faceted
  facet_by_list <- .parse_plot_facet_by(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols)
  
  if (!is.null(facet_by)) {
    if (is.null(facet_wrap_cols)) {
      # Use a grid
      p <- p + ggplot2::facet_grid(
        rows = facet_by_list$facet_rows,
        cols = facet_by_list$facet_cols,
        labeller = "label_context",
        drop = TRUE)
      
    } else {
      p <- p + ggplot2::facet_wrap(
        facets = facet_by_list$facet_by,
        labeller = "label_context",
        drop = TRUE)
    }
  }
  
  # Prevent removal of data outside or on plot limits.
  p <- p + ggplot2::coord_cartesian(
    xlim = x_range,
    ylim = y_range)
  
  return(p)
}



.determine_auc_roc_plot_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols) {
  # Obtain faceting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x, 
    facet_by = facet_by, 
    facet_wrap_cols = facet_wrap_cols)
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- 4
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))
  
  return(c(height, width))
}
