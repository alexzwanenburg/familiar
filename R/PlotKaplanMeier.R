#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_kaplan_meier (generic) --------------------------------------------------

#' @title Plot Kaplan-Meier survival curves.
#'
#' @description This function creates Kaplan-Meier survival curves from
#'   stratification data stored in a familiarCollection object.
#'
#' @param dir_path (*optional*) Path to the directory where created figures are
#'   saved to. Output is saved in the `stratification` subdirectory. If `NULL`
#'   no figures are saved, but are returned instead.
#' @param discrete_palette (*optional*) Palette to use to color the different
#'   risk strata in case a non-singular variable was provided to the `color_by`
#'   argument.
#' @param censoring (*optional*) Flag to indicate whether censored samples
#'   should be indicated on the survival curve.
#' @param censor_shape (*optional*) Shape used to indicate censored samples on
#'   the survival curve. Available shapes are documented in the `ggplot2`
#'   vignette *Aesthetic specifications*. By default a plus shape is used.
#' @param show_logrank (*optional*) Specifies whether the results of a logrank
#'   test to assess differences between the risk strata is annotated in the
#'   plot. A log-rank test can only be shown when `color_by` and `linestyle_by`
#'   are either unset, or only contain `risk_group`.
#' @param show_survival_table (*optional*) Specifies whether a survival table is
#'   shown below the Kaplan-Meier survival curves. Survival in the risk strata
#'   is assessed for each of the breaks in `x_breaks`.
#' @param height (*optional*) Height of the plot. A default value is derived
#'   from number of facets and the inclusion of survival tables.
#' @param confidence_level (*optional*) Confidence level for the strata in the
#'   plot.
#'
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#' @inheritDotParams extract_risk_stratification_data -object
#'
#' @details This function generates a Kaplan-Meier survival plot based on risk
#'   group stratification by the learners.
#'
#'   `familiar` does not determine what units the x-axis has or what kind of
#'   survival the y-axis represents. It is therefore recommended to provide
#'   `x_label` and `y_label` arguments.
#'
#'   Available splitting variables are: `fs_method`, `learner`, `data_set`,
#'   `risk_group` and `stratification_method`. By default, separate figures are
#'   created for each combination of `fs_method` and `learner`, with faceting by
#'   `data_set`, colouring of the strata in each individual plot by
#'   `risk_group`.
#'
#'   Available palettes for `discrete_palette` are those listed by
#'   `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'   (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'   `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'   name in `grDevices`. If not specified, a default palette based on palettes
#'   in Tableau are used. You may also specify your own palette by using colour
#'   names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'   Greenwood confidence intervals of the Kaplan-Meier curve can be shown using
#'   various styles set by `conf_int_style`:
#'
#'  * `ribbon` (default): confidence intervals are shown as a ribbon with an
#'   opacity of `conf_int_alpha` around the point estimate of the Kaplan-Meier
#'   curve.
#'
#'  * `step` (default): confidence intervals are shown as a step function around
#'   the point estimate of the Kaplan-Meier curve.
#'
#'  * `none`: confidence intervals are not shown. The point estimate of the ROC
#'   curve is shown as usual.
#'
#'   Labelling methods such as `set_risk_group_names` or `set_data_set_names`
#'   can be applied to the `familiarCollection` object to update labels, and
#'   order the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_kaplan_meier
#' @md
#' @rdname plot_kaplan_meier-methods
setGeneric(
  "plot_kaplan_meier",
  function(
      object,
      draw = FALSE,
      dir_path = NULL,
      split_by = NULL,
      color_by = NULL,
      linetype_by = NULL,
      facet_by = NULL,
      facet_wrap_cols = NULL,
      combine_legend = TRUE,
      ggtheme = NULL,
      discrete_palette = NULL,
      x_label = "time",
      x_label_shared = "column",
      y_label = "survival probability",
      y_label_shared = "row",
      legend_label = waiver(),
      plot_title = waiver(),
      plot_sub_title = waiver(),
      caption = NULL,
      x_range = NULL,
      x_n_breaks = 5,
      x_breaks = NULL,
      y_range = c(0, 1),
      y_n_breaks = 5,
      y_breaks = NULL,
      confidence_level = NULL,
      conf_int_style = c("ribbon", "step", "none"),
      conf_int_alpha = 0.4,
      censoring = TRUE,
      censor_shape = "plus",
      show_logrank = TRUE,
      show_survival_table = TRUE,
      width = waiver(),
      height = waiver(),
      units = waiver(),
      export_collection = FALSE,
      ...) {
    standardGeneric("plot_kaplan_meier")
  }
)



# plot_kaplan_meier (general) --------------------------------------------------

#' @rdname plot_kaplan_meier-methods
setMethod(
  "plot_kaplan_meier",
  signature(object = "ANY"),
  function(
      object,
      draw = FALSE,
      dir_path = NULL,
      split_by = NULL,
      color_by = NULL,
      linetype_by = NULL,
      facet_by = NULL,
      facet_wrap_cols = NULL,
      combine_legend = TRUE,
      ggtheme = NULL,
      discrete_palette = NULL,
      x_label = "time",
      x_label_shared = "column",
      y_label = "survival probability",
      y_label_shared = "row",
      legend_label = waiver(),
      plot_title = waiver(),
      plot_sub_title = waiver(),
      caption = NULL,
      x_range = NULL,
      x_n_breaks = 5,
      x_breaks = NULL,
      y_range = c(0, 1),
      y_n_breaks = 5,
      y_breaks = NULL,
      confidence_level = NULL,
      conf_int_style = c("ribbon", "step", "none"),
      conf_int_alpha = 0.4,
      censoring = TRUE,
      censor_shape = "plus",
      show_logrank = TRUE,
      show_survival_table = TRUE,
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
          "data_element" = "risk_stratification_data"),
        list(...)))
    
    return(do.call(
      plot_kaplan_meier,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "color_by" = color_by,
        "linetype_by" = linetype_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "combine_legend" = combine_legend,
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
        "confidence_level" = confidence_level,
        "conf_int_style" = conf_int_style,
        "conf_int_alpha" = conf_int_alpha,
        "censoring" = censoring,
        "censor_shape" = censor_shape,
        "show_logrank" = show_logrank,
        "show_survival_table" = show_survival_table,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



# plot_kaplan_meier (collection) -----------------------------------------------

#' @rdname plot_kaplan_meier-methods
setMethod(
  "plot_kaplan_meier",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    linetype_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    combine_legend = TRUE,
    ggtheme = NULL,
    discrete_palette = NULL,
    x_label = "time",
    x_label_shared = "column",
    y_label = "survival probability",
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_range = NULL,
    x_n_breaks = 5,
    x_breaks = NULL,
    y_range = c(0, 1),
    y_n_breaks = 5,
    y_breaks = NULL,
    confidence_level = NULL,
    conf_int_style = c("ribbon", "step", "none"),
    conf_int_alpha = 0.4,
    censoring = TRUE,
    censor_shape = "plus",
    show_logrank = TRUE,
    show_survival_table = TRUE,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    risk_group <- .NATURAL <- NULL
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)

    # Get input data
    x <- export_risk_stratification_data(
      object = object,
      export_strata = FALSE)
    
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

      if (length(x) > 1) ..error_reached_unreachable_code(
        "plot_kaplan_meier: list of data elements contains unmerged elements.")

      # Get x directly.
      x <- x[[1]]
    }

    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)
    if (!all_predictions_valid(x@data, outcome_type = "survival")) return(NULL)

    # Remove non-valid risk groups
    x@data <- x@data[!is.na(risk_group)]

    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = TRUE),
      purpose = "to create kaplan-meier survival curves",
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
    if (is.null(x_range)) x_range <- c(0, x@time)

    # x_breaks
    if (is.null(x_breaks)) {
      .check_input_plot_args(x_n_breaks = x_n_breaks)

      # Create breaks and update x_range
      x_breaks <- labeling::extended(
        m = x_n_breaks,
        dmin = x_range[1],
        dmax = x_range[2],
        only.loose = TRUE)

      x_range <- c(0, tail(x_breaks, n = 1))
    }

    # y_range
    if (is.null(y_range)) y_range <- c(0, 1)

    # y_breaks
    if (is.null(y_breaks)) {
      .check_input_plot_args(y_n_breaks = y_n_breaks)

      # Create breaks and update y_range
      y_breaks <- labeling::extended(
        m = y_n_breaks,
        dmin = y_range[1],
        dmax = y_range[2])

      y_range <- c(0, tail(y_breaks, n = 1))
    }

    # confidence level
    if (!is.null(confidence_level)) {
      .check_number_in_valid_range(
        x = confidence_level,
        var_name = "confidence_level",
        range = c(0.0, 1.0),
        closed = c(FALSE, FALSE))

      # Attach to object.
      x@confidence_level <- confidence_level
    }


    # conf_int_style
    if (length(conf_int_style) > 1) {
      conf_int_style <- head(conf_int_style, n = 1)
    }

    # Store plots to list in case no dir_path is provided
    if (is.null(dir_path)) plot_list <- list()

    # Add default splitting variables.
    if (is.null(split_by) &&
        is.null(color_by) &&
        is.null(linetype_by) &&
        is.null(facet_by)) {
      split_by <- c("fs_method", "learner", "stratification_method")
      color_by <- c("risk_group")
      facet_by <- c("data_set")
    }

    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x@data,
      split_by = split_by,
      color_by = color_by,
      linetype_by = linetype_by,
      facet_by = facet_by,
      available = c(
        "fs_method", "learner", "data_set",
        "risk_group", "stratification_method"))

    # Update splitting variables
    split_by <- split_var_list$split_by
    facet_by <- split_var_list$facet_by
    color_by <- split_var_list$color_by
    linetype_by <- split_var_list$linetype_by

    # Create a legend label
    legend_label <- plotting.create_legend_label(
      user_label = legend_label,
      color_by = color_by,
      linetype_by = linetype_by,
      combine_legend = combine_legend)

    # Set show_logrank to FALSE in case color_by and linetype_by
    # contain more than risk_group at most.
    if (!is.null(union(color_by, linetype_by))) {
      if (!all(union(color_by, linetype_by) == "risk_group")) {
        show_logrank <- FALSE
      }
    }

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

    # Create plots -------------------------------------------------------------

    # Determine if subtitle should be generated.
    autogenerate_plot_subtitle <- is.waive(plot_sub_title)

    # Split data and supporting data.
    if (!is.null(split_by)) {
      data_split <- split(
        unique(x@data[, mget(split_by)]),
        by = split_by,
        drop = TRUE)
      
    } else {
      data_split <- list(NULL)
    }

    # Store plots in a list in case no dir_path is provided
    if (is.null(dir_path)) plot_list <- list()

    # Iterate over splits
    for (current_split in data_split) {
      # Generate split.
      if (!is.null(current_split)) {
        x_split <- methods::new(
          "familiarDataElementRiskStratification",
          x,
          data = x@data[current_split, on = .NATURAL])
        
      } else {
        x_split <- x
      }

      if ("stratification_method" %in% split_by) {
        # Check that only relevant risk groups are present.
        x_split@data$risk_group <- droplevels(x_split@data$risk_group)
      }

      if (is_empty(x_split)) next
      if (!any_predictions_valid(x_split@data, outcome_type = "survival")) {
        return(NULL)
      }

      if (is.waive(plot_title)) {
        plot_title <- "Kaplan-Meier survival curve"
      }

      # Declare subtitle components.
      additional_subtitle <- NULL

      # Add evaluation time as subtitle component if it is not used
      # otherwise.
      if (!"evaluation_time" %in% c(split_by, color_by, linetype_by, facet_by)) {
        additional_subtitle <- c(
          additional_subtitle,
          .add_time_to_plot_subtitle(x_split@time))
      }

      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          additional = additional_subtitle,
          x = current_split)
      }

      # Create plot
      p <- .plot_kaplan_meier(
        x = x_split,
        color_by = color_by,
        linetype_by = linetype_by,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        combine_legend = combine_legend,
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
        conf_int_style = conf_int_style,
        conf_int_alpha = conf_int_alpha,
        censoring = censoring,
        censor_shape = censor_shape,
        show_logrank = show_logrank,
        show_survival_table = show_survival_table)

      # Check empty output
      if (is.null(p)) next

      # Draw plot
      if (draw) plotting.draw(plot_or_grob = p)

      # Save and export
      if (!is.null(dir_path)) {
        # Obtain default plot dimensions
        def_plot_dims <- .determine_km_plot_default_dimensions(
          x = x_split@data,
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols,
          show_survival_table = show_survival_table)

        # Save to file.
        do.call(
          plotting.save_plot_to_file,
          args = c(
            list(
              "plot_obj" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "stratification",
              "x" = current_split,
              "split_by" = split_by,
              "height" = ifelse(is.waive(height), def_plot_dims[1], height),
              "width" = ifelse(is.waive(width), def_plot_dims[2], width),
              "units" = ifelse(is.waive(units), "cm", units)),
            list(...)))
        
      } else {
        # Store as list
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



.plot_kaplan_meier <- function(
    x,
    color_by,
    linetype_by,
    facet_by,
    facet_wrap_cols,
    combine_legend,
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
    conf_int_style,
    conf_int_alpha,
    censoring,
    censor_shape,
    show_logrank,
    show_survival_table) {
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- NULL

  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- .get_plot_layout_table(
    x = x@data,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols)

  # Define the split in data required for faceting.
  data_split <- split(
    plot_layout_table,
    by = c("col_id", "row_id"),
    sorted = TRUE)

  # Create plots to join
  figure_list <- list()
  extracted_element_list <- list()
  for (current_split in data_split) {
    # Generate the split in case there is a faceting variable.
    if (!is.null(facet_by)) {
      x_split <- methods::new(
        "familiarDataElementRiskStratification",
        x,
        data = x@data[current_split, on = .NATURAL]
      )
    } else {
      x_split <- x
    }

    # Compute strata for the current split.
    strata <- .compute_risk_stratification_curves(
      x = x_split,
      time_range = x_range)

    # Compute logrank test value.
    if (show_logrank) {
      test_data <- .compute_risk_stratification_tests(
        x = x_split,
        time_range = x_range
      )$logrank
    } else {
      test_data <- NULL
    }

    # Kaplan-Meier plots
    p_kaplan_meier <- .create_km_subplot(
      x = strata,
      h = test_data,
      color_by = color_by,
      linetype_by = linetype_by,
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      combine_legend = combine_legend,
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
      conf_int_alpha = conf_int_alpha,
      censoring = censoring,
      censor_shape = censor_shape,
      show_logrank = show_logrank)

    # Extract plot elements from the Kaplan-Meier plot.
    extracted_elements <- .extract_plot_grobs(p = p_kaplan_meier)

    # Remove extracted elements from the Kaplan-Meier plot.
    p_kaplan_meier <- .remove_plot_grobs(p = p_kaplan_meier)

    # Rename plot elements.
    g_kaplan_meier <- .rename_plot_grobs(
      g = plotting.to_grob(p_kaplan_meier),
      extension = "main")

    if (show_survival_table && gtable::is.gtable(g_kaplan_meier)) {
      # Survival tables
      p_survival_table <- .create_survival_table_subplot(
        x = strata,
        color_by = color_by,
        linetype_by = linetype_by,
        ggtheme = ggtheme,
        discrete_palette = discrete_palette,
        x_range = x_range,
        x_breaks = x_breaks)

      # Extract survival gtable, which consists of the panel and the left axis.
      g_survival_table <- .gtable_extract(
        g = plotting.to_grob(p_survival_table),
        element = c("panel", "axis-l"),
        partial_match = TRUE)

      # Insert survival table into the kaplan-meier table. Use partial matching
      # to match elements from g_survival_table with those in g_kaplan_meier.
      g_kaplan_meier <- .gtable_insert(
        g = g_kaplan_meier,
        g_new = g_survival_table,
        where = "bottom",
        ref_element = "xlab-b-main",
        partial_match = TRUE)
    }

    # Add combined grob to list
    figure_list <- c(figure_list, list(g_kaplan_meier))

    # Add extract elements to the extracted_element_list
    extracted_element_list <- c(extracted_element_list, list(extracted_elements))
  }

  # Update the layout table. Note that the axis text and labels share the same
  # behaviour.
  plot_layout_table <- .update_plot_layout_table(
    plot_layout_table = plot_layout_table,
    grobs = figure_list,
    x_text_shared = x_label_shared,
    x_label_shared = x_label_shared,
    y_text_shared = y_label_shared,
    y_label_shared = y_label_shared,
    facet_wrap_cols = facet_wrap_cols)

  # Combine features.
  g <- .arrange_plot_grobs(
    grobs = figure_list,
    plot_layout_table = plot_layout_table,
    element_grobs = extracted_element_list,
    ggtheme = ggtheme)

  return(g)
}



.create_km_subplot <- function(
    x,
    h,
    color_by,
    linetype_by,
    facet_by,
    facet_wrap_cols,
    combine_legend,
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
    conf_int_alpha,
    censoring,
    censor_shape,
    show_logrank) {
  # Suppress NOTES due to non-standard evaluation in data.table
  n_censor <- risk_group_1 <- NULL
  
  if (!is_empty(x)) x <- x@data
  if (!is_empty(h)) h <- h@data

  # Generate a guide table to allow integration of guides into a single legend
  guide_list <- plotting.create_guide_table(
    x = x,
    color_by = color_by,
    linetype_by = linetype_by,
    discrete_palette = discrete_palette,
    combine_legend = combine_legend)
  
  # Extract data
  x <- guide_list$data

  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("time"),
      y = !!sym("survival")))
  p <- p + ggtheme

  # Create step function
  if (is.null(color_by) && is.null(linetype_by)) {
    p <- p + ggplot2::geom_step()
    
  } else if (!is.null(color_by) && is.null(linetype_by)) {
    p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
      colour = !!sym("color_breaks")))
    
  } else if (is.null(color_by) && !is.null(linetype_by)) {
    p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
      linetype = !!sym("linetype_breaks")))
    
  } else {
    p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
      colour = !!sym("color_breaks"),
      linetype = !!sym("linetype_breaks")))
  }

  # Set colour
  if (!is.null(color_by)) {
    # Extract guide_table for color
    g_color <- guide_list$guide_color

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

  # Set line style
  if (!is.null(linetype_by)) {
    # Extract guide_table for linetype
    g_linetype <- guide_list$guide_linetype

    p <- p + ggplot2::scale_linetype_manual(
      name = legend_label$guide_linetype,
      values = g_linetype$linetype_values,
      breaks = g_linetype$linetype_breaks,
      drop = FALSE)
  }

  if (show_logrank && !is_empty(h)) {
    # Parse p-value
    p_value_label <- paste0(
      "p: ", as.character(signif(h[risk_group_1 == "all"]$p_value, 2)))

    # Obtain default settings.
    text_settings <- .get_plot_geom_text_settings(ggtheme = ggtheme)

    # Show in plot
    p <- p + ggplot2::annotate(
      "text",
      x = x_range[1],
      y = y_range[1],
      label = p_value_label,
      colour = text_settings$colour,
      family = text_settings$family,
      fontface = text_settings$face,
      size = text_settings$geom_text_size,
      vjust = "inward",
      hjust = "inward")
  }

  # Plot confidence intervals
  if (conf_int_style[1] != "none" && !is_empty(x)) {
    if (conf_int_style[1] == "step") {
      if (is.null(color_by)) {
        p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
          y = !!sym("ci_low"),
          linetype = "ci_up",
          na.rm = TRUE))

        p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
          y = !!sym("ci_low"),
          linetype = "ci_up",
          na.rm = TRUE))
        
      } else {
        p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
          y = !!sym("ci_low"),
          linetype = "ci_up",
          colour = !!sym("color_breaks"),
          na.rm = TRUE))

        p <- p + ggplot2::geom_step(mapping = ggplot2::aes(
          y = !!sym("ci_low"),
          linetype = "ci_up",
          colour = !!sym("color_breaks"),
          na.rm = TRUE))
      }
      
    } else if (conf_int_style[1] == "ribbon") {
      if (is.null(color_by)) {
        # Create special data for ribbon so that it becomes a step ribbon.
        x_ribbon <- .prepare_km_conf_int_plot_data(x = x)

        p <- p + ggplot2::geom_ribbon(
          data = x_ribbon,
          mapping = ggplot2::aes(
            x = !!sym("time"),
            ymin = !!sym("ci_low"),
            ymax = !!sym("ci_up")),
          alpha = conf_int_alpha,
          na.rm = TRUE)
        
      } else {
        # Create special data for ribbon so that it becomes a step ribbon.
        x_ribbon <- data.table::rbindlist(lapply(
          split(x, by = "color_breaks"),
          .prepare_km_conf_int_plot_data))
        
        p <- p + ggplot2::geom_ribbon(
          data = x_ribbon,
          mapping = ggplot2::aes(
            x = !!sym("time"),
            ymin = !!sym("ci_low"),
            ymax = !!sym("ci_up"),
            fill = !!sym("color_breaks")),
          alpha = conf_int_alpha,
          na.rm = TRUE)
      }
    }
  }

  # Censoring indicators
  if (censoring) {
    if (is.null(color_by)) {
      p <- p + ggplot2::geom_point(
        data = x[n_censor > 0],
        shape = censor_shape,
        show.legend = FALSE)
      
    } else {
      p <- p + ggplot2::geom_point(
        data = x[n_censor > 0],
        mapping = ggplot2::aes(
          colour = !!sym("color_breaks"),
          fill = !!sym("color_breaks")),
        shape = censor_shape,
        show.legend = FALSE)
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

  # Determine how things are facetted.
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

  # Plot to Cartesian coordinates.
  p <- p + ggplot2::coord_cartesian(
    xlim = x_range,
    ylim = y_range)

  return(p)
}



.create_survival_table_subplot <- function(
    x,
    color_by,
    linetype_by,
    ggtheme,
    discrete_palette,
    x_range,
    x_breaks) {
  # Suppress NOTES due to non-standard evaluation in data.table
  id <- missing_entry <- NULL

  # Find the names for the groups
  if (is.null(c(color_by, linetype_by))) {
    # Parse the table to obtain group sizes at x_breaks
    survival_table <- ..get_survival_breakpoints(
      x@data,
      x_breaks = x_breaks)

    survival_table[, ":="("group_name" = "")]
    
  } else {
    # Parse the table to obtain group sizes at x_breaks
    survival_list <- lapply(
      split(x@data, by = c(color_by, linetype_by)),
      ..get_survival_breakpoints,
      x_breaks = x_breaks)

    # Combine into list
    survival_table <- data.table::rbindlist(
      survival_list, 
      use.names = TRUE)

    unique_vars <- unique(c(color_by, linetype_by))

    # Generate a guide table
    guide_table <- data.table::data.table(expand.grid(lapply(
      rev(unique_vars),
      function(ii, x) (levels(x[[ii]])),
      x = survival_table)))

    # Rename variables
    data.table::setnames(
      x = guide_table,
      rev(unique_vars))

    # Convert to factors
    for (ii in unique_vars) {
      guide_table[[ii]] <- factor(
        x = guide_table[[ii]],
        levels = levels(x@data[[ii]]))
    }

    # Order columns according to unique_vars
    data.table::setcolorder(
      x = guide_table,
      neworder = unique_vars)

    # Order data set by columns
    data.table::setorderv(
      x = guide_table,
      cols = unique_vars)

    # Set breaks
    breaks <- apply(guide_table, 1, paste, collapse = ", ")

    # Update guide_table
    guide_table$group_name <- factor(breaks, levels = breaks)

    # Identify missing entries in the guide table. First add an identifier to
    # the guide-table, then determine which entries are empty.
    guide_table[, "id" := .I]
    guide_table$missing_entry <- sapply(
      split(guide_table, by = "id"),
      function(x, y, by) {
        return(is_empty(merge(
          x = x,
          y = y,
          by = by,
          all = FALSE)))
      },
      y = survival_table,
      by = unique_vars)

    if (any(guide_table$missing_entry)) {
      # Find a suitable prototype and then create a prototype table.
      available_prototype <- head(guide_table[missing_entry == FALSE]$id, 1)

      proto_table <- merge(
        x = survival_table,
        y = guide_table[id == available_prototype, mget(unique_vars)])

      # Iterate over missing entries.
      new_survival_list <- lapply(
        split(guide_table[missing_entry == TRUE], by = "id"),
        function(x, proto_table, unique_vars) {
          # Make a copy of the prototype and replace the group size by 0.
          y <- data.table::copy(proto_table)[, "group_size" := 0]
          
          # Insert the correct values for each plotting variable that was
          # previously missing.
          for (var in unique_vars) {
            y[, (var) := x[[var]]]
          }

          return(y)
        },
        proto_table = proto_table,
        unique_vars = unique_vars)

      # Add the placeholder entries to the table.
      survival_table <- data.table::rbindlist(
        c(list(survival_table), new_survival_list),
        use.names = TRUE)

      # Remove superfluous columns from guide_table
      guide_table[, ":="(
        "id" = NULL,
        "missing_entry" = NULL)]
    }

    # Combine the guide table with the survival table to add in the group names.
    survival_table <- merge(
      x = survival_table,
      y = guide_table,
      by = unique_vars)
  }

  # Obtain default settings.
  text_settings <- .get_plot_geom_text_settings(ggtheme = ggtheme)
  fontsize <- text_settings$fontsize
  fontsize_rel <- text_settings$fontsize_rel
  lineheight <- text_settings$lineheight

  # Create plot
  p <- ggplot2::ggplot(
    data = survival_table,
    mapping = ggplot2::aes(
      x = !!sym("time"),
      y = !!sym("group_name"),
      label = !!sym("group_size")))

  # Annotate survival in strata.
  p <- p + ggplot2::geom_text(
    colour = text_settings$colour,
    family = text_settings$family,
    fontface = text_settings$face,
    size = text_settings$geom_text_size)

  # Adapt axes.
  p <- p + ggplot2::scale_x_continuous(
    breaks = x_breaks,
    limits = x_range)

  p <- p + ggplot2::scale_y_discrete(
    breaks = rev(levels(survival_table$group_name)),
    limits = rev(levels(survival_table$group_name)))

  # Set main theme
  p <- p + ggtheme

  # Remove some theme elements and reduce margins
  p <- p + ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank())

  # Convert to gtable
  g <- ggplot2::ggplotGrob(p)

  # Set the height on the panel element to n * fontsize + (n-1) * lineheight
  n_lines <- data.table::uniqueN(survival_table$group_name)
  grob_height <- n_lines * fontsize * fontsize_rel + 
    (n_lines - 1) * fontsize * fontsize_rel * lineheight + 4

  # Find the panel
  panel_row_t <- g$layout[g$layout$name == "panel", "t"]

  # Set heights of the row
  g$heights[panel_row_t] <- grid::unit(x = grob_height, "pt")

  # Set to p
  p$custom_grob <- list(
    "heights" = list(
      "name" = "panel",
      "height" = grid::unit(grob_height, "pt")))

  class(p) <- c("familiar_ggplot", class(p))

  return(p)
}



..get_survival_breakpoints <- function(x, x_breaks) {
  # Suppress NOTES due to non-standard evaluation in data.table
  time <- survival <- NULL

  # Iterate over the breaks to obtain the group size at this time point.
  survival_list <- lapply(
    x_breaks,
    function(break_time, x) {
      # Find the nearest candidate entry
      candidate_entry <- data.table::copy(tail(x[time <= break_time, ], n = 1))

      # In case the number of survivors is 0, the group size is also 0.
      candidate_entry[survival == 0, "group_size" := 0]

      return(candidate_entry)
    },
    x = x)

  # Convert to a new data.table
  survival_table <- data.table::rbindlist(
    survival_list,
    use.names = TRUE)

  # Update time to x_breaks
  survival_table[, "time" := x_breaks]

  return(survival_table)
}



.determine_km_plot_default_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols,
    show_survival_table) {
  # Obtain facetting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols)

  # Set default height and width for each subplot (in cm).
  default_width <- 6
  default_height <- ifelse(show_survival_table, 6, 4)

  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * default_height, 27.7))

  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * default_width, 19))

  return(c(height, width))
}



.prepare_km_conf_int_plot_data <- function(x) {
  # Suppress NOTES due to non-standard evaluation in data.table
  time <- ci_low <- ci_up <- NULL

  if (is_empty(x)) return(x)

  # Make sure that the surv_lower and surv_upper coordinate sets are correct.
  x <- data.table::copy(x)[order(time)]
  y <- data.table::copy(x)[1:nrow(x) - 1]
  y[, "time" := x$time[2:nrow(x)]]

  # Combine and order correctly.
  x <- rbind(x, y)[order(time, -ci_low, -ci_up)]

  return(x)
}
