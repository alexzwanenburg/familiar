#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_calibration_data (generic) ----------------------------------------------

#' @title Plot calibration figures.
#'
#' @description This method creates calibration plots from calibration data
#'  stored in a familiarCollection object. For this figures, the expected
#'  (predicted) values are plotted against the observed values. A
#'  well-calibrated model should be close to the identity line.
#'
#' @param dir_path (*optional*) Path to the directory where created calibration
#'  plots are saved to. Output is saved in the `calibration` subdirectory. If
#'  `NULL` no figures are saved, but are returned instead.
#' @param discrete_palette (*optional*) Palette to use to color the different
#'  data points and fit lines in case a non-singular variable was provided to
#'  the `color_by` argument.
#' @param show_density (*optional*) Show point density in top margin of the
#'  figure. If `color_by` is set, this information will not be shown.
#' @param show_calibration_fit (*optional*) Specifies whether the calibration in
#'  the large and calibration slope are annotated in the plot. If `color_by` is
#'  set, this information will not be shown.
#' @param show_goodness_of_fit (*optional*) Specifies whether a the results of
#'  goodness of fit tests are annotated in the plot. If `color_by` is set, this
#'  information will not be shown.
#' @param density_plot_height (*optional*) Height of the density plot. The height
#'  is 1.5 cm by default. Height is expected to be grid unit (see `grid::unit`),
#'  which also allows for specifying relative heights. Will be ignored if
#'  `show_density` is `FALSE`.
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#' @inheritDotParams extract_calibration_data -object
#'
#' @details This function generates a calibration plot for each model in each
#'  dataset. Any data used for calibration (e.g. baseline survival) is obtained
#'  during model creation.
#'
#'  Available splitting variables are: `fs_method`, `learner`, `data_set` and
#'  `evaluation_time` (survival analysis only) and `positive_class` (multinomial
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
#'  endpoint. For such endpoints, `positive_class` is an additional facet variable
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
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @references 1. Hosmer, D. W., Hosmer, T., Le Cessie, S. & Lemeshow, S. A
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
#' @exportMethod plot_calibration_data
#' @md
#' @rdname plot_calibration_data-methods
setGeneric(
  "plot_calibration_data",
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
      x_label_shared = "column",
      y_label = waiver(),
      y_label_shared = "row",
      legend_label = waiver(),
      plot_title = waiver(),
      plot_sub_title = waiver(),
      caption = NULL,
      x_range = NULL,
      x_n_breaks = 5,
      x_breaks = NULL,
      y_range = NULL,
      y_n_breaks = 5,
      y_breaks = NULL,
      conf_int_style = c("ribbon", "step", "none"),
      conf_int_alpha = 0.4,
      show_density = TRUE,
      show_calibration_fit = TRUE,
      show_goodness_of_fit = TRUE,
      density_plot_height = grid::unit(1.0, "cm"),
      width = waiver(),
      height = waiver(),
      units = waiver(),
      export_collection = FALSE,
      ...) {
    standardGeneric("plot_calibration_data")
  }
)



# plot_calibration_data (general) ----------------------------------------------

#' @rdname plot_calibration_data-methods
setMethod(
  "plot_calibration_data", 
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
      x_label_shared = "column",
      y_label = waiver(),
      y_label_shared = "row",
      legend_label = waiver(),
      plot_title = waiver(),
      plot_sub_title = waiver(),
      caption = NULL,
      x_range = NULL,
      x_n_breaks = 5,
      x_breaks = NULL,
      y_range = NULL,
      y_n_breaks = 5,
      y_breaks = NULL,
      conf_int_style = c("ribbon", "step", "none"),
      conf_int_alpha = 0.4,
      show_density = TRUE,
      show_calibration_fit = TRUE,
      show_goodness_of_fit = TRUE,
      density_plot_height = grid::unit(1.0, "cm"),
      width = waiver(),
      height = waiver(),
      units = waiver(),
      export_collection = FALSE,
      ...) {
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list("object" = object, "data_element" = "calibration_data"),
        list(...)))

    return(do.call(
      plot_calibration_data,
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
        "x_label_shared" = x_label_shared,
        "y_label" = y_label,
        "y_label_shared" = y_label_shared,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_range" = x_range,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_range" = y_range,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "show_density" = show_density,
        "show_calibration_fit" = show_calibration_fit,
        "show_goodness_of_fit" = show_goodness_of_fit,
        "density_plot_height" = density_plot_height,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



# plot_calibration_data (collection) -------------------------------------------

#' @rdname plot_calibration_data-methods
setMethod(
  "plot_calibration_data",
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
      x_label_shared = "column",
      y_label = waiver(),
      y_label_shared = "row",
      legend_label = waiver(),
      plot_title = waiver(),
      plot_sub_title = waiver(),
      caption = NULL,
      x_range = NULL,
      x_n_breaks = 5,
      x_breaks = NULL,
      y_range = NULL,
      y_n_breaks = 5,
      y_breaks = NULL,
      conf_int_style = c("ribbon", "step", "none"),
      conf_int_alpha = 0.4,
      show_density = TRUE,
      show_calibration_fit = TRUE,
      show_goodness_of_fit = TRUE,
      density_plot_height = grid::unit(1.0, "cm"),
      width = waiver(),
      height = waiver(),
      units = waiver(),
      export_collection = FALSE,
      ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL

    # Make sure the collection object is updated.
    object <- update_object(object = object)

    # Get input data
    x <- export_calibration_data(
      object = object,
      aggregate_results = TRUE)

    # Check that the data are not empty.
    if (is_empty(x$data)) return(NULL)

    # Check that the data are not evaluated at the model level.
    if (all(sapply(x$data, function(x) (x@detail_level == "model")))) {
      ..warning_no_comparison_between_models()
      return(NULL)
    }

    # Get data
    calibration_data <- x$data[[1]]@data
    density_data <- x$density[[1]]@data
    linear_test_data <- x$linear_test[[1]]@data
    gof_test_data <- x$gof_test[[1]]@data

    # Check that the calibration data are not empty.
    if (is_empty(calibration_data)) return(NULL)

    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = TRUE),
      purpose = "to create calibration plots",
      message_type = "warning")) {
      return(NULL)
    }

    # Check input arguments ----------------------------------------------------

    # ggtheme
    ggtheme <- .check_ggtheme(ggtheme)

    # x_label_shared
    if (!is.waive(x_label_shared)) {
      .check_input_plot_args(x_label_shared = x_label_shared)
    } else {
      x_label_shared <- "column"
    }

    # y_label_shared
    if (!is.waive(y_label_shared)) {
      .check_input_plot_args(y_label_shared = y_label_shared)
    } else {
      y_label_shared <- "row"
    }

    # x_range
    if (is.null(x_range)) x_range <- c(0, 1)

    # x_breaks
    if (is.null(x_breaks)) {
      # Check input arguments.
      .check_input_plot_args(
        x_range = x_range,
        x_n_breaks = x_n_breaks)

      # Create breaks and update x_range
      x_breaks <- labeling::extended(
        m = x_n_breaks,
        dmin = x_range[1],
        dmax = x_range[2],
        only.loose = TRUE)

      x_range <- c(head(x_breaks, n = 1), tail(x_breaks, n = 1))
    }

    # y_range
    if (is.null(y_range)) y_range <- c(0, 1)

    # y_breaks
    if (is.null(y_breaks)) {
      # Check input arguments.
      .check_input_plot_args(
        y_range = y_range,
        y_n_breaks = y_n_breaks)

      # Create breaks and update y_range
      y_breaks <- labeling::extended(
        m = y_n_breaks,
        dmin = y_range[1],
        dmax = y_range[2],
        only.loose = TRUE)

      y_range <- c(head(y_breaks, n = 1), tail(y_breaks, n = 1))
    }

    # x_label
    if (is.waive(x_label)) {
      if (object@outcome_type %in% c("binomial", "multinomial")) {
        x_label <- "expected probability"
      } else if (object@outcome_type %in% c("count", "continuous")) {
        x_label <- "expected value"
      } else if (object@outcome_type %in% c("survival")) {
        x_label <- "expected survival probability"
      } else {
        ..error_no_known_outcome_type(object@outcome_type)
      }
    }

    # y_label
    if (is.waive(y_label)) {
      if (object@outcome_type %in% c("binomial", "multinomial")) {
        y_label <- "observed proportion"
      } else if (object@outcome_type %in% c("count", "continuous")) {
        y_label <- "observed value"
      } else if (object@outcome_type %in% c("survival")) {
        y_label <- "observed survival proportion"
      } else {
        ..error_no_known_outcome_type(object@outcome_type)
      }
    }

    # conf_int_style
    if (length(conf_int_style) > 1) {
      conf_int_style <- head(conf_int_style, n = 1)
    }

    # Set the style of the confidence interval to none, in case no confidence
    # interval data is present.
    if (!x$data[[1]]@estimation_type %in% c("bci", "bootstrap_confidence_interval")) {
      conf_int_style <- "none"
    }

    # Add default splitting variables.
    if (is.null(split_by) & is.null(color_by) & is.null(facet_by)) {
      split_by <- c("fs_method", "learner")

      # Set faceting variables
      facet_by <- c("data_set")

      # Add "evaluation_time" as a faceting variable in case of survival
      # analysis.
      if (object@outcome_type %in% c("survival")) {
        facet_by <- c(facet_by, "evaluation_time")
      } else if (
        object@outcome_type %in% c("multinomial")) {
        facet_by <- c(facet_by, "positive_class")
      }
    }

    # Obtain available splitting variables. This differ per type of endpoint
    # studied. For survival outcomes, evaluation time is an additional splitting
    # point. For multinomial outcomes, the positive class indicator is an
    # additional splitting variable.
    available_splitting_vars <- c("fs_method", "learner", "data_set")
    if (object@outcome_type %in% c("survival")) {
      available_splitting_vars <- c(available_splitting_vars, "evaluation_time")
    } else if (object@outcome_type %in% c("multinomial")) {
      available_splitting_vars <- c(available_splitting_vars, "positive_class")
    }

    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = calibration_data,
      split_by = split_by,
      color_by = color_by,
      facet_by = facet_by,
      available = available_splitting_vars)

    # Update splitting variables
    split_by <- split_var_list$split_by
    color_by <- split_var_list$color_by
    facet_by <- split_var_list$facet_by

    # Create a legend label
    legend_label <- plotting.create_legend_label(
      user_label = legend_label,
      color_by = color_by)

    # Update show_density, show_goodness_of_fit and show_calibration_fit
    # variables so that they are FALSE in case color_by is set.
    if (!is.null(color_by)) {
      show_calibration_fit <- FALSE
      show_goodness_of_fit <- FALSE
      show_density <- FALSE
    }

    # Check input for show_* arguments.
    .check_parameter_value_is_valid(
      x = show_calibration_fit,
      var_name = "show_calibration_fit",
      values = c(FALSE, TRUE))
    
    .check_parameter_value_is_valid(
      x = show_goodness_of_fit,
      var_name = "show_goodness_of_fit",
      values = c(FALSE, TRUE))
    
    .check_parameter_value_is_valid(
      x = show_density,
      var_name = "show_density",
      values = c(FALSE, TRUE))

    # Check density_plot_height
    .check_plot_grid_unit(
      x = density_plot_height,
      var_name = "density_plot_height")

    # Check input arguments for validity.
    .check_input_plot_args(
      x_range = x_range,
      y_range = y_range,
      x_breaks = x_breaks,
      y_breaks = y_breaks,
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption)

    # Create plots -------------------------------------------------------------

    # Determine if subtitle should be generated.
    autogenerate_plot_subtitle <- is.waive(plot_sub_title)

    # Split data and supporting data.
    if (!is.null(split_by)) {
      data_split <- split(
        unique(calibration_data[, mget(split_by)]),
        by = split_by,
        drop = TRUE)
      
    } else {
      data_split <- list(NULL)
    }

    # Store plots to list in case no dir_path is provided
    if (is.null(dir_path)) plot_list <- list()

    # Find grouping columns
    grouping_column <- x$data[[1]]@grouping_column
    grouping_column <- setdiff(grouping_column, "expected")

    # Iterate over splits
    for (current_split in data_split) {
      # Default values
      calibration_data_split <- NULL
      density_data_split <- NULL
      linear_test_data_split <- NULL
      gof_test_data_split <- NULL

      if (!is.null(current_split)) {
        calibration_data_split <- calibration_data[current_split, on = .NATURAL]

        # Get split data. This prevents issues in case a dataset with
        # only identical samples is present.
        split_data <- unique(calibration_data_split[, mget(grouping_column)])

        if (show_density && !is_empty(density_data)) {
          density_data_split <- density_data[split_data, on = .NATURAL]
        }
        
        if (show_calibration_fit && !is_empty(linear_test_data)) {
          linear_test_data_split <- linear_test_data[split_data, on = .NATURAL]
        }
        
        if (show_goodness_of_fit && !is_empty(gof_test_data)) {
          gof_test_data_split <- gof_test_data[split_data, on = .NATURAL]
        }
        
      } else {
        calibration_data_split <- calibration_data

        if (show_density && !is_empty(density_data)) {
          density_data_split <- density_data
        }
        
        if (show_calibration_fit && !is_empty(linear_test_data)) {
          linear_test_data_split <- linear_test_data
        }
        
        if (show_goodness_of_fit && !is_empty(gof_test_data)) {
          gof_test_data_split <- gof_test_data
        }
      }

      if (is_empty(calibration_data_split)) next

      if (is.waive(plot_title)) plot_title <- "Calibration plot"

      # Declare subtitle components.
      additional_subtitle <- NULL

      # Add evaluation time as subtitle component if it is not used
      # otherwise.
      if (!"evaluation_time" %in% c(split_by, color_by, facet_by) &&
          object@outcome_type %in% c("survival")) {
        additional_subtitle <- c(
          additional_subtitle,
          .add_time_to_plot_subtitle(calibration_data_split$evaluation_time[1]))
      }

      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          additional = additional_subtitle,
          x = current_split)
      }

      # Generate plot
      p <- .plot_calibration_plot(
        x = calibration_data_split,
        color_by = color_by,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        ggtheme = ggtheme,
        discrete_palette = discrete_palette,
        x_label = x_label,
        x_label_shared = x_label_shared,
        y_label = y_label,
        y_label_shared = y_label_shared,
        legend_label = legend_label,
        plot_title = plot_title,
        plot_sub_title = plot_sub_title,
        caption = caption,
        x_range = x_range,
        x_breaks = x_breaks,
        y_range = y_range,
        y_breaks = y_breaks,
        conf_int_alpha = conf_int_alpha,
        conf_int_style = conf_int_style,
        show_density = show_density,
        show_calibration_fit = show_calibration_fit,
        show_goodness_of_fit = show_goodness_of_fit,
        density_plot_height = density_plot_height,
        linear_test = linear_test_data_split,
        gof_test = gof_test_data_split,
        density = density_data_split,
        outcome_type = object@outcome_type,
        grouping_column = x$linear_test[[1]]@grouping_column,
        is_point = x$data[[1]]@estimation_type %in% c("point"))

      # Check empty output
      if (is.null(p)) next

      # Draw plot
      if (draw) plotting.draw(plot_or_grob = p)

      # Save and export
      if (!is.null(dir_path)) {
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_calibration_plot_dimensions(
          x = calibration_data_split,
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols,
          show_density = show_density)

        # Save to file.
        do.call(
          plotting.save_plot_to_file,
          args = c(
            list(
              "plot_obj" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "calibration",
              "x" = current_split,
              "split_by" = split_by,
              "height" = ifelse(is.waive(height), def_plot_dims[1], height),
              "width" = ifelse(is.waive(width), def_plot_dims[2], width),
              "units" = ifelse(is.waive(units), "cm", units)),
            list(...)))
        
      } else {
        # Store as list and export
        plot_list <- c(plot_list, list(p))
      }
    }

    # Generate output
    return(plotting.get_output(
      dir_path = dir_path,
      plot_list = plot_list,
      export_collection = export_collection,
      object = object))
  }
)



.plot_calibration_plot <- function(
    x,
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
    conf_int_alpha,
    conf_int_style,
    show_density,
    show_calibration_fit,
    show_goodness_of_fit,
    density_plot_height,
    linear_test,
    gof_test,
    density,
    outcome_type,
    grouping_column,
    is_point) {
  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols)

  # Split data into facets. This is done by row.
  data_facet_list <- .split_data_by_plot_facet(
    x = x,
    plot_layout_table = plot_layout_table)
  linear_test_facet_list <- .split_data_by_plot_facet(
    x = linear_test,
    plot_layout_table = plot_layout_table)
  gof_test_facet_list <- .split_data_by_plot_facet(
    x = gof_test,
    plot_layout_table = plot_layout_table)
  density_facet_list <- .split_data_by_plot_facet(
    x = density,
    plot_layout_table = plot_layout_table)

  # Placeholders for plots.
  figure_list <- list()
  extracted_element_list <- list()

  # Iterate over facets
  for (ii in names(data_facet_list)) {
    # Create calibration plot.
    p_calibration <- .create_calibration_plot(
      x = data_facet_list[[ii]],
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
      conf_int_alpha = conf_int_alpha,
      conf_int_style = conf_int_style,
      show_calibration_fit = show_calibration_fit,
      show_goodness_of_fit = show_goodness_of_fit,
      linear_test = linear_test_facet_list[[ii]],
      gof_test = gof_test_facet_list[[ii]],
      outcome_type = outcome_type,
      grouping_column = grouping_column,
      is_point = is_point)

    # Extract plot elements from the main calibration plot.
    extracted_elements <- plotting.extract_plot_elements(p = p_calibration)

    # Remove extracted elements from the plot.
    p_calibration <- plotting.remove_plot_elements(p = p_calibration)

    # Rename plot elements.
    g_calibration <- plotting.rename_plot_elements(
      g = plotting.to_grob(p_calibration),
      extension = "main")

    if (show_density && 
        gtable::is.gtable(g_calibration) &&
        !is_empty(density_facet_list[[ii]])) {
      # Procedure for normal density plots.
      p_margin <- .create_calibration_density_subplot(
        x = density_facet_list[[ii]],
        ggtheme = ggtheme,
        x_range = x_range,
        x_breaks = x_breaks,
        flip = FALSE,
        plot_height = density_plot_height)

      # Extract the panel element from the density plot.
      g_margin <- .gtable_extract(
        g = plotting.to_grob(p_margin),
        element = c("panel"),
        partial_match = TRUE)

      # Insert in the calibration plot at the top margin.
      g_calibration <- .gtable_insert(
        g = g_calibration,
        g_new = g_margin,
        where = "top",
        ref_element = "panel-main",
        partial_match = TRUE,
        spacer = list("b" = plotting.get_panel_spacing(ggtheme = ggtheme, axis = "x")))
    }

    # Add combined grob to list
    figure_list <- c(
      figure_list,
      list(g_calibration))

    # Add extract elements to the grob_element_list
    extracted_element_list <- c(
      extracted_element_list,
      list(extracted_elements))
  }

  # Update the layout table.
  plot_layout_table <- plotting.update_plot_layout_table(
    plot_layout_table = plot_layout_table,
    grobs = figure_list,
    x_text_shared = x_label_shared,
    x_label_shared = x_label_shared,
    y_text_shared = y_label_shared,
    y_label_shared = y_label_shared,
    facet_wrap_cols = facet_wrap_cols)

  # Combine features.
  g <- plotting.arrange_figures(
    grobs = figure_list,
    plot_layout_table = plot_layout_table,
    element_grobs = extracted_element_list,
    ggtheme = ggtheme)

  return(g)
}



.create_calibration_plot <- function(
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
    conf_int_alpha,
    conf_int_style,
    show_calibration_fit,
    show_goodness_of_fit,
    grouping_column,
    linear_test,
    gof_test,
    outcome_type,
    is_point) {
  # Suppress NOTES due to non-standard evaluation in data.table
  type <- NULL

  # Define formula for casting the linear test data wide. This data will be used
  # to plot linear fits in the plot. The "evaluation_time" column will only
  # appear for survival endpoints, and the "positive_class" column only in
  # binomial/multinomial outcomes, but will only matter for multinomial
  # outcomes.
  cast_formula <- stats::as.formula(paste0(
    paste(setdiff(grouping_column, "type"), collapse = "+"),
    "~type"))
  
  if (!is_empty(linear_test)) {
    # Cast wide on the value of the intercept or slope coefficient.
    linear_fit <- data.table::dcast(
      data = linear_test,
      cast_formula,
      value.var = "value")
    
  } else {
    linear_fit <- NULL
  }


  # Generate a guide table for the data
  x_guide_list <- plotting.create_guide_table(
    x = x,
    color_by = color_by,
    discrete_palette = discrete_palette)
  fit_guide_list <- plotting.create_guide_table(
    x = linear_fit,
    color_by = color_by,
    discrete_palette = discrete_palette)

  # Extract data
  x <- x_guide_list$data

  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("expected"),
      y = !!sym("observed")))

  # Add theme
  p <- p + ggtheme

  # Add identity line
  p <- p + ggplot2::geom_abline(
    slope = 1.0,
    intercept = 0.0,
    colour = "grey80",
    linetype = "dashed")

  # Add fill colors
  if (!is.null(color_by)) {
    if (is_point) {
      # Add scatter for individual data points.
      p <- p + ggplot2::geom_point(
        mapping = ggplot2::aes(colour = !!sym("color_breaks")))
      
    } else {
      if (utils::packageVersion("ggplot2") >= "3.4.0") {
        # Version 3.4.0 introduces the linewidth element for geom_line, and will
        # produce deprecation warnings if the size argument is used instead.

        # Add line for individual data points.
        p <- p + ggplot2::geom_line(
          mapping = ggplot2::aes(colour = !!sym("color_breaks")),
          linewidth = ..get_plot_theme_linewidth(ggtheme = ggtheme) * 3)
        
      } else {
        # For backward compatibility with ggplot2 versions prior to 3.4.0
        p <- p + ggplot2::geom_line(
          mapping = ggplot2::aes(colour = !!sym("color_breaks")),
          size = ..get_plot_theme_linewidth(ggtheme = ggtheme) * 3)
      }
    }


    # Add fit
    if (!is_empty(fit_guide_list$data)) {
      p <- p + ggplot2::geom_abline(
        data = fit_guide_list$data,
        mapping = ggplot2::aes(
          colour = !!sym("color_breaks"),
          intercept = !!sym("offset"),
          slope = !!sym("slope")))
    }

    # Set colour.
    p <- p + ggplot2::scale_colour_manual(
      name = legend_label$guide_color,
      values = x_guide_list$guide_color$color_values,
      breaks = x_guide_list$guide_color$color_breaks,
      drop = FALSE)

    p <- p + ggplot2::scale_fill_manual(
      name = legend_label$guide_color,
      values = x_guide_list$guide_color$color_values,
      breaks = x_guide_list$guide_color$color_breaks,
      drop = FALSE)
    
  } else {
    if (is_point) {
      p <- p + ggplot2::geom_point()
      
    } else {
      if (utils::packageVersion("ggplot2") >= "3.4.0") {
        # Version 3.4.0 introduces the linewidth element for geom_line, and will
        # produce deprecation warnings if the size argument is used instead.

        # Add scatter for individual data points.
        p <- p + ggplot2::geom_line(
          linewidth = ..get_plot_theme_linewidth(ggtheme = ggtheme) * 3)
        
      } else {
        # For backward compatibility with ggplot2 versions prior to version
        # 3.4.0.
        p <- p + ggplot2::geom_line(
          size = ..get_plot_theme_linewidth(ggtheme = ggtheme) * 3)
      }
    }

    # Add fit
    if (!is_empty(fit_guide_list$data)) {
      p <- p + ggplot2::geom_abline(
        data = fit_guide_list$data,
        mapping = ggplot2::aes(
          intercept = !!sym("offset"),
          slope = !!sym("slope")))
    }
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

      # Remove linetype from the legend.
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

  # Set breaks and  limits on the x and y-axis
  p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
  p <- p + ggplot2::scale_y_continuous(breaks = y_breaks)

  # Determine how things are faceted.
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
        labeller = "label_context")
      
    } else {
      p <- p + ggplot2::facet_wrap(
        facets = facet_by_list$facet_by,
        labeller = "label_context")
    }
  }

  # Annotate calibration information.
  if ((show_calibration_fit && !is_empty(linear_test)) ||
      (show_goodness_of_fit && !is_empty(gof_test))) {
    # Merge test data
    label <- character(0)

    if (show_calibration_fit && !is_empty(linear_test)) {
      # Add calibration-in-the-large.
      if (all(c("ci_low", "ci_up") %in% colnames(linear_test))) {
        label <- c(
          label,
          paste0(
            "intercept: ",
            format(round(linear_test[type == "offset"]$value, 2), nsmall = 2),
            " (",
            format(round(linear_test[type == "offset"]$ci_low, 2), nsmall = 2),
            "\u2013",
            ifelse(round(linear_test[type == "offset"]$ci_up, 2) < 0, " ", ""),
            format(round(linear_test[type == "offset"]$ci_up, 2), nsmall = 2),
            ")"))

        # Add calibration slope.
        label <- c(
          label,
          paste0(
            "slope: ",
            format(round(linear_test[type == "slope"]$value, 2), nsmall = 2),
            " (",
            format(round(linear_test[type == "slope"]$ci_low, 2), nsmall = 2),
            "\u2013",
            ifelse(round(linear_test[type == "slope"]$ci_up, 2) < 0, " ", ""),
            format(round(linear_test[type == "slope"]$ci_up, 2), nsmall = 2),
            ")"))
        
      } else {
        label <- c(
          label,
          paste0(
            "intercept: ",
            format(round(linear_test[type == "offset"]$value, 2), nsmall = 2)))

        # Add calibration slope.
        label <- c(
          label,
          paste0(
            "slope: ",
            format(round(linear_test[type == "slope"]$value, 2), nsmall = 2)))
      }
    }

    if (show_goodness_of_fit && !is_empty(gof_test)) {
      # Hosmer-Lemeshow test
      if ("hosmer_lemeshow" %in% c(gof_test$type)) {
        label <- c(
          label,
          paste0(
            "HL-test p: ",
            format(signif(gof_test[type == "hosmer_lemeshow"]$p_value, 2), nsmall = 2)))
      }

      # Nam-D'Agostino test
      if ("nam_dagostino" %in% c(gof_test$type)) {
        label <- c(
          label,
          paste0(
            "ND-test p: ",
            format(signif(gof_test[type == "nam_dagostino"]$p_value, 2), nsmall = 2)))
      }

      # Greenwood-Nam-D'Agostino test
      if ("greenwood_nam_dagostino" %in% c(gof_test$type)) {
        label <- c(
          label,
          paste0(
            "GND-test p: ",
            format(signif(gof_test[type == "greenwood_nam_dagostino"]$p_value, 2), nsmall = 2)))
      }
    }

    # Combine all label elements, and use for annotation
    if (length(label) > 0) {
      label <- paste(label, collapse = "\n")

      # Obtain default settings.
      text_settings <- plotting.get_geom_text_settings(ggtheme = ggtheme)

      # Show in plot
      p <- p + ggplot2::annotate(
        "text",
        x = x_range[1],
        y = y_range[2],
        label = label,
        colour = text_settings$colour,
        family = text_settings$family,
        fontface = text_settings$face,
        size = text_settings$geom_text_size,
        vjust = "inward",
        hjust = "inward")
    }
  }

  # Update labels.
  p <- p + ggplot2::labs(
    x = x_label,
    y = y_label,
    title = plot_title,
    subtitle = plot_sub_title,
    caption = caption)

  # Prevent clipping of confidence intervals.
  p <- p + ggplot2::coord_cartesian(
    xlim = x_range,
    ylim = y_range)

  return(p)
}



.create_calibration_density_subplot <- function(
    x,
    ggtheme,
    x_range,
    x_breaks,
    flip = FALSE,
    plot_height) {
  # Create plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("expected"),
      weight = !!sym("frequency")))
  
  p <- p + ggplot2::geom_density(bw = 0.075)
  p <- p + ggplot2::scale_x_continuous(
    breaks = x_breaks,
    limits = x_range)

  # Set main theme
  p <- p + ggtheme

  # Remove some theme elements and reduce margins
  p <- p + ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank())

  if (flip) {
    p <- p + ggplot2::scale_y_reverse()
  }

  # Ensure that panel heights are set to the plot object.
  p$custom_grob <- list("heights" = list("name" = "panel", "height" = plot_height))
  class(p) <- c("familiar_ggplot", class(p))

  return(p)
}



.determine_calibration_plot_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols,
    show_density) {
  # Obtain facetting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols)

  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- ifelse(show_density, 6, 5.5)

  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))

  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))

  return(c(height, width))
}
