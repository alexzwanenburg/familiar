#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# plot_confusion_matrix (generic) ----------------------------------------------

#' @title Plot confusion matrix.
#'
#' @description This method creates confusion matrices based on data in a
#'   familiarCollection object.
#'
#' @param dir_path (*optional*) Path to the directory where created confusion
#'   matrixes are saved to. Output is saved in the `performance` subdirectory.
#'   If `NULL` no figures are saved, but are returned instead.
#' @param discrete_palette (*optional*) Palette used to colour the confusion
#'   matrix. The colour depends on whether each cell of the confusion matrix is
#'   on the diagonal (observed outcome matched expected outcome) or not.
#' @param show_alpha (*optional*) Interpreting confusion matrices is made easier
#'   by setting the opacity of the cells. `show_alpha` takes the following
#'   values:
#'
#'  * `none`: Cell opacity is not altered. Diagonal and off-diagonal cells are
#'   completely opaque and transparent, respectively. Same as
#'   `show_alpha=FALSE`.
#'
#'  * `by_class`: Cell opacity is normalised by the number of instances for each
#'   observed outcome class in each confusion matrix.
#'
#'  * `by_matrix` (default): Cell opacity is normalised by the number of
#'   instances in the largest observed outcome class in each confusion matrix.
#'   Same as `show_alpha=TRUE`
#'
#'  * `by_figure`: Cell opacity is normalised by the number of instances in the
#'   largest observed outcome class across confusion matrices in different
#'   facets.
#'
#'  * `by_all`: Cell opacity is normalised by the number of instances in the
#'   largest observed outcome class across all confusion matrices.
#'
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#' @inheritDotParams extract_confusion_matrix -object
#'
#' @details This function generates area under the ROC curve plots.
#'
#'   Available splitting variables are: `fs_method`, `learner` and `data_set`.
#'   By default, the data is split by `fs_method` and `learner`, with facetting
#'   by `data_set`.
#'
#'   Available palettes for `discrete_palette` are those listed by
#'   `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'   (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'   `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'   name in `grDevices`. If not specified, a default palette based on palettes
#'   in Tableau are used. You may also specify your own palette by using colour
#'   names listed by `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'   Labeling methods such as `set_fs_method_names` or `set_data_set_names` can
#'   be applied to the `familiarCollection` object to update labels, and order
#'   the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_confusion_matrix
#' @md
#' @rdname plot_confusion_matrix-methods
setGeneric(
  "plot_confusion_matrix",
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
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
    rotate_x_tick_labels = waiver(),
    show_alpha = TRUE,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    standardGeneric("plot_confusion_matrix")
  }
)



# plot_confusion_matrix (general) ----------------------------------------------

#' @rdname plot_confusion_matrix-methods
setMethod(
  "plot_confusion_matrix",
  signature(object = "ANY"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
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
    rotate_x_tick_labels = waiver(),
    show_alpha = TRUE,
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
          "data_element" = "confusion_matrix"),
        list(...)))
    
    return(do.call(
      plot_confusion_matrix,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
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
        "rotate_x_tick_labels" = rotate_x_tick_labels,
        "show_alpha" = show_alpha,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)


# plot_confusion_matrix (collection) -------------------------------------------

#' @rdname plot_confusion_matrix-methods
setMethod(
  "plot_confusion_matrix",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
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
    rotate_x_tick_labels = waiver(),
    show_alpha = TRUE,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    observed_outcome <- expected_outcome <- count <- class_matches <- total_observed <- NULL

    # Make sure the collection object is updated.
    object <- update_object(object = object)

    # Get input data.
    x <- export_confusion_matrix_data(object = object)

    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)

    # Obtain data element from list.
    if (is.list(x)) {
      if (is_empty(x)) return(NULL)

      if (length(x) > 1) {
        ..error_reached_unreachable_code(
          "plot_model_performance: list of data elements contains unmerged elements.")
      }

      # Get x directly.
      x <- x[[1]]
    }

    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)

    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = FALSE),
      purpose = "to plot confusion matrices",
      message_type = "warning")) {
      return(NULL)
    }

    # Check input arguments ----------------------------------------------------

    # ggtheme
    ggtheme <- .check_ggtheme(ggtheme)

    # rotate_x_tick_labels
    if (is.waive(rotate_x_tick_labels)) {
      rotate_x_tick_labels <- FALSE
    }

    outcome_name <- get_outcome_name(object)

    # x_label
    if (is.waive(x_label)) {
      x_label <- "expected"
      if (length(outcome_name) > 0) {
        x_label <- paste(x_label, outcome_name, sep = " ")
      }
    }

    # y_label
    if (is.waive(y_label)) {
      y_label <- "observed"
      if (length(outcome_name) > 0) {
        y_label <- paste(y_label, outcome_name, sep = " ")
      }
    }

    # show_alpha
    if (is.logical(show_alpha)) {
      show_alpha <- ifelse(show_alpha, "by_matrix", "none")
    }

    .check_parameter_value_is_valid(
      x = show_alpha,
      var_name = "show_alpha",
      values = c("none", "by_class", "by_matrix", "by_figure", "by_all"))

    # Splitting variables
    if (is.null(split_by) && is.null(facet_by)) {
      split_by <- c("fs_method", "learner")
      facet_by <- c("data_set")
    }

    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x@data,
      split_by = split_by,
      facet_by = facet_by,
      available = c("fs_method", "learner", "data_set"))

    # Update splitting variables
    split_by <- split_var_list$split_by
    facet_by <- split_var_list$facet_by

    # Create a legend label
    legend_label <- .create_plot_legend_title(user_label = legend_label)

    # Check input arguments for validity.
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      rotate_x_tick_labels = rotate_x_tick_labels)

    # Add a class_matches column
    x@data[, "class_matches" := observed_outcome == expected_outcome]

    if (show_alpha == "none") {
      # Full opacity on diagonal, full transparancy for off-diagonal cells.
      x@data[, "alpha" := as.double(class_matches)]
      
    } else {
      # Determine the alpha level (opacity) of the fills in the confusion
      # matrix. This is determined by the number of instances of observed
      # classes.
      max_observations <- x@data[, list(
        "total_observed" = sum(count)),
        by = c("observed_outcome", facet_by, split_by)]

      if (show_alpha == "by_class") {
        # Nothing extra needed
      } else if (show_alpha == "by_matrix") {
        # Find the maximum observations in each facet and split, i.e. normalise
        # per matrix.
        if (!is.null(facet_by) | !is.null(split_by)) {
          max_observations[, "total_observed" := max(total_observed), by = c(facet_by, split_by)]
        }
      } else if (show_alpha == "by_figure") {
        # Find the maximum observations over all facets.
        if (!is.null(split_by)) {
          max_observations[, "total_observed" := max(total_observed), by = c(split_by)]
        }
      } else if (show_alpha == "by_all") {
        # Find the maximum observations over all plots.
        max_observations[, "total_observed" := max(total_observed)]
        
      } else {
        ..error_reached_unreachable_code("plot_confusion_matrix: unknown show_alpha value.")
      }

      # Merge back into x and compute the alpha level (opacity).
      x@data <- merge(
        x = x@data,
        y = max_observations,
        by = c("observed_outcome", facet_by, split_by))
      
      x@data[, "alpha" := count / total_observed]
    }
    
    
    
    # Create plots -------------------------------------------------------------

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

      if (is.waive(plot_title)) plot_title <- "Confusion matrix"

      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          x = x_split[[ii]])
      }

      # Generate plot
      p <- .plot_confusion_matrix_plot(
        x = x_split[[ii]],
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
        rotate_x_tick_labels = rotate_x_tick_labels)

      # Check empty output
      if (is.null(p)) next

      # Draw figure.
      if (draw) .draw_plot(plot_or_grob = p)

      # Save and export
      if (!is.null(dir_path)) {
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_confusion_matrix_plot_dimensions(
          x = x_split[[ii]],
          class_levels = get_outcome_class_levels(object),
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols,
          rotate_x_tick_labels = rotate_x_tick_labels)

        # Save to file.
        do.call(
          .save_plot_to_file,
          args = c(
            list(
              "plot_or_grob" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "performance",
              "subtype" = "confusion_matrix",
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
    return(plotting.get_output(
      dir_path = dir_path,
      plot_list = plot_list,
      export_collection = export_collection,
      object = object))
  }
)



.plot_confusion_matrix_plot <- function(
    x,
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
    rotate_x_tick_labels) {
  # Find the colours
  discrete_colours <- .get_palette(
    x = discrete_palette,
    palette_type = "qualitative",
    n = 2)

  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("expected_outcome"),
      y = !!sym("observed_outcome"),
      fill = !!sym("class_matches"),
      alpha = !!sym("alpha")))
  
  # Add theme
  p <- p + ggtheme

  # Plot the heatmap
  p <- p + ggplot2::geom_raster()

  # Colour the diagonal in the heatmap
  p <- p + ggplot2::scale_fill_manual(
    values = discrete_colours,
    breaks = c(FALSE, TRUE),
    drop = FALSE)

  # Set alpha scale.
  p <- p + ggplot2::scale_alpha_continuous(
    range = c(0.0, 1.0),
    limits = c(0.0, 1.0))

  # Labels
  p <- p + ggplot2::labs(
    x = x_label,
    y = y_label,
    title = plot_title,
    subtitle = plot_sub_title,
    caption = caption)

  # Obtain default settings.
  text_settings <- .get_plot_geom_text_settings(ggtheme = ggtheme)

  # Annotate the number of pairs in the figure.
  p <- p + ggplot2::geom_text(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("expected_outcome"),
      y = !!sym("observed_outcome"),
      label = !!sym("count"),
      alpha = 1.0),
    colour = text_settings$colour,
    family = text_settings$family,
    fontface = text_settings$face,
    size = text_settings$geom_text_size)

  # Determine how things are facetted
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

  # Rotate x-axis ticks
  if (rotate_x_tick_labels) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        vjust = 0.25,
        hjust = 1.0,
        angle = 90.0))
  }

  # Suppress legends
  p <- p + ggplot2::theme(legend.position = "none")

  return(p)
}



.determine_confusion_matrix_plot_dimensions <- function(
    x,
    class_levels,
    facet_by,
    facet_wrap_cols,
    rotate_x_tick_labels) {
  # Determine the number of elements along the x-axis.
  n_elements <- length(class_levels)
  longest_element <- max(sapply(class_levels, nchar))

  # Assume each x-axis element takes up about 0.5 cm. Then add some room for
  # other plot elements.
  default_width <- n_elements * 1.0 + 1.0

  # Assume each y-axis element takes up about 0.5 cm as well.
  default_height <- n_elements * 1.0 + 1.0

  # Reserve space for y-axis and x-axis tick labels. Assume that the typical
  # width of a character is about 5 points (1.8 mm). For the x-axis we only
  # reserve extra space in case the ticks are rotated, otherwise we just
  # assume a typical height of 10 points (3.6 mm).
  y_tick_space <- longest_element * 0.18
  x_tick_space <- ifelse(rotate_x_tick_labels, longest_element * 0.18, 0.36)

  # Obtain facetting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x,
    facet_by = facet_by, 
    facet_wrap_cols = facet_wrap_cols)

  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * (default_height) + x_tick_space, 27.7))

  # Set overall plot width, but limit to small-margin A4 (19 cm). We leave some
  # room for the legend on the right.
  width <- min(c(2 + plot_dims[2] * (default_width) + y_tick_space, 19))

  return(c(height, width))
}
