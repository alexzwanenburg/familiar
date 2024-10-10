#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# plot_variable_importance (generic) -------------------------------------------

#' @title Plot variable importance scores of features during feature selection
#'   or after training a model.
#'
#' @description This function plots variable importance based data obtained
#'   during feature selection or after training a model, which are stored in a
#'   `familiarCollection` object.
#'
#' @param type Determine what variable importance should be shown. Can be
#'   `feature_selection` or `model` for the variable importance after the
#'   feature selection step and after the model training step, respectively.
#' @param dir_path (*optional*) Path to the directory where created figures are
#'   saved to. Output is saved in the `variable_importance` subdirectory. If
#'   `NULL` no figures are saved, but are returned instead.
#' @param show_cluster (*optional*) Show which features were clustered together.
#'   Currently not available in combination with variable importance obtained
#'   during feature selection.
#' @param gradient_palette (*optional*) Palette for filling bars if the
#'   `color_by` argument is not set. By default, bars are not coloured. If
#'   `gradient_palette` is set, the palette will colour bars according to
#'   feature importance. Use `NULL` to fill the bars using the `familiar`
#'   default palette. Other palettes are supported by the `paletteer` package,
#'   `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'   (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'   `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'   name in `grDevices`. You may also specify your own palette by providing a
#'   vector of colour names listed by `grDevices::colors()` or through
#'   hexadecimal RGB strings.
#' @param height (*optional*) Height of the plot. A default value is derived
#'   from number of facets, and the length of the longest feature name (if
#'   `rotate_x_tick_labels` is `TRUE`).
#' @param width (*optional*) Width of the plot. A default value is derived from
#'   the number of facets and the number of features.
#' @inheritParams export_fs_vimp
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units -path -filename -plot
#' @inheritDotParams extract_fs_vimp -object -aggregation_method -rank_threshold
#'
#' @details This function generates a barplot based on variable importance of
#'   features.
#'
#'   The only allowed values for `split_by`, `color_by` or `facet_by` are
#'   `fs_method` and `learner`, but note that `learner` has no effect when
#'   plotting variable importance of features acquired during feature selection.
#'
#'   Labeling methods such as `set_feature_names` or `set_fs_method_names` can
#'   be applied to the `familiarCollection` object to update labels, and order
#'   the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_variable_importance
#' @export plot_feature_selection_occurrence
#' @export plot_feature_selection_variable_importance
#' @export plot_model_signature_occurrence
#' @export plot_model_signature_variable_importance
#' @md
#' @rdname plot_variable_importance-methods
setGeneric(
  "plot_variable_importance",
  function(
    object,
    type,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    aggregation_method = waiver(),
    rank_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    show_cluster = TRUE,
    ggtheme = NULL,
    discrete_palette = NULL,
    gradient_palette = waiver(),
    x_label = "feature",
    rotate_x_tick_labels = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("plot_variable_importance")
  }
)



# plot_variable_importance (general) -------------------------------------------

#' @rdname plot_variable_importance-methods
setMethod(
  "plot_variable_importance", 
  signature(object = "ANY"),
  function(
    object,
    type,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    aggregation_method = waiver(),
    rank_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    show_cluster = TRUE,
    ggtheme = NULL,
    discrete_palette = NULL,
    gradient_palette = waiver(),
    x_label = "feature",
    rotate_x_tick_labels = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    # Set the data element.
    data_element <- switch(
      type,
      "feature_selection" = "fs_vimp",
      "model" = "model_vimp"
    )
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = data_element,
          "feature_cluster_method" = feature_cluster_method,
          "feature_linkage_method" = feature_linkage_method,
          "feature_cluster_cut_method" = feature_cluster_cut_method,
          "feature_similarity_threshold" = feature_similarity_threshold,
          "aggregation_method" = aggregation_method,
          "rank_threshold" = rank_threshold
        ),
        list(...)
      )
    )

    return(do.call(
      plot_variable_importance,
      args = c(
        list(
          "object" = object,
          "type" = type,
          "feature_cluster_method" = feature_cluster_method,
          "feature_linkage_method" = feature_linkage_method,
          "feature_cluster_cut_method" = feature_cluster_cut_method,
          "feature_similarity_threshold" = feature_similarity_threshold,
          "aggregation_method" = aggregation_method,
          "rank_threshold" = rank_threshold,
          "draw" = draw,
          "dir_path" = dir_path,
          "split_by" = split_by,
          "color_by" = color_by,
          "facet_by" = facet_by,
          "facet_wrap_cols" = facet_wrap_cols,
          "show_cluster" = show_cluster,
          "ggtheme" = ggtheme,
          "discrete_palette" = discrete_palette,
          "gradient_palette" = gradient_palette,
          "x_label" = x_label,
          "rotate_x_tick_labels" = rotate_x_tick_labels,
          "y_label" = y_label,
          "legend_label" = legend_label,
          "plot_title" = plot_title,
          "plot_sub_title" = plot_sub_title,
          "caption" = caption,
          "y_range" = y_range,
          "y_n_breaks" = y_n_breaks,
          "y_breaks" = y_breaks,
          "width" = width,
          "height" = height,
          "units" = units,
          "export_collection" = export_collection
        ),
        list(...)
      )
    ))
  }
)



# plot_variable_importance (familiarCollection) --------------------------------

#' @rdname plot_variable_importance-methods
setMethod(
  "plot_variable_importance",
  signature(object = "familiarCollection"),
  function(
    object,
    type,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    aggregation_method = waiver(),
    rank_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    show_cluster = TRUE,
    ggtheme = NULL,
    discrete_palette = NULL,
    gradient_palette = waiver(),
    x_label = "feature",
    rotate_x_tick_labels = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(.plot_variable_importance(
      object = object,
      type = type,
      feature_cluster_method = feature_cluster_method,
      feature_linkage_method = feature_linkage_method,
      feature_cluster_cut_method = feature_cluster_cut_method,
      feature_similarity_threshold = feature_similarity_threshold,
      aggregation_method = aggregation_method,
      rank_threshold = rank_threshold,
      draw = draw,
      dir_path = dir_path,
      split_by = split_by,
      color_by = color_by,
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      show_cluster = show_cluster,
      ggtheme = ggtheme,
      discrete_palette = discrete_palette,
      gradient_palette = gradient_palette,
      x_label = x_label,
      rotate_x_tick_labels = rotate_x_tick_labels,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      y_range = y_range,
      y_n_breaks = y_n_breaks,
      y_breaks = y_breaks,
      width = width,
      height = height,
      units = units,
      export_collection = export_collection,
      ...
    ))
  }
)



#' @rdname plot_variable_importance-methods
plot_feature_selection_occurrence <- function(...) {
  return(do.call(
    plot_variable_importance,
    args = c(
      list(
        "type" = "feature_selection",
        "aggregation_method" = "stability"
      ),
      list(...)
    )
  ))
}



#' @rdname plot_variable_importance-methods
plot_feature_selection_variable_importance <- function(...) {
  return(do.call(
    plot_variable_importance,
    args = c(
      list("type" = "feature_selection"),
      list(...)
    )
  ))
}



#' @rdname plot_variable_importance-methods
plot_model_signature_occurrence <- function(...) {
  return(do.call(
    plot_variable_importance,
    args = c(
      list(
        "type" = "model",
        "aggregation_method" = "stability"
      ),
      list(...)
    )
  ))
}


#' @rdname plot_variable_importance-methods
plot_model_signature_variable_importance <- function(...) {
  return(do.call(
    plot_variable_importance,
    args = c(
      list("type" = "model"),
      list(...)
    )
  ))
}



.plot_variable_importance <- function(
    object,
    type,
    feature_cluster_method,
    feature_linkage_method,
    feature_cluster_cut_method,
    feature_similarity_threshold,
    aggregation_method,
    rank_threshold,
    draw,
    dir_path,
    split_by,
    color_by,
    facet_by,
    facet_wrap_cols,
    show_cluster,
    ggtheme,
    discrete_palette,
    gradient_palette,
    x_label,
    rotate_x_tick_labels,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    y_range,
    y_n_breaks,
    y_breaks,
    width,
    height,
    units,
    export_collection = FALSE,
    ...
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  data_set <- NULL

  # Get input data.
  if (type == "feature_selection") {
    x <- export_fs_vimp(
      object = object,
      aggregation_method = aggregation_method,
      rank_threshold = rank_threshold,
      aggregate_results = TRUE
    )

    available_splitting <- "fs_method"
    
  } else if (type == "model") {
    x <- export_model_vimp(
      object = object,
      aggregation_method = aggregation_method,
      rank_threshold = rank_threshold,
      aggregate_results = TRUE
    )

    available_splitting <- c("fs_method", "learner")
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".plot_variable_importance: unknown value for type (", type, ")"
    ))
  }

  # Check that the data are not empty.
  if (is_empty(x)) return(NULL)

  # Obtain data element from list.
  if (is.list(x)) {
    if (is_empty(x)) return(NULL)

    if (length(x) > 1L) {
      ..error_reached_unreachable_code(
        ".plot_variable_importance: list of data elements contains unmerged elements."
      )
    }

    # Get x directly.
    x <- x[[1L]]
  }

  # Check that the data are not empty.
  if (is_empty(x)) return(NULL)

  # Check package requirements for plotting.
  if (!require_package(
    x = ..required_plotting_packages(extended = FALSE),
    purpose = ifelse(
      type == "feature_selection",
      "to plot variable importance determined through feature selection methods",
      "to plot model-based variable importance"
    ),
    message_type = "warning"
  )) {
    return(NULL)
  }

  # Check input arguments ------------------------------------------------------
  
  # Check show_cluster
  .check_parameter_value_is_valid(
    x = show_cluster, 
    var_name = "show_cluster",
    values = c(FALSE, TRUE)
  )

  if (show_cluster) {
    # Get feature similarity data.
    feature_similarity <- export_feature_similarity(
      object = object,
      feature_cluster_method = feature_cluster_method,
      feature_linkage_method = feature_linkage_method,
      feature_cluster_cut_method = feature_cluster_cut_method,
      feature_similarity_threshold = feature_similarity_threshold,
      export_dendrogram = FALSE,
      export_ordered_data = FALSE,
      export_clustering = TRUE
    )[[1L]]
    
  } else {
    feature_similarity <- NULL
  }

  # ggtheme
  ggtheme <- .check_ggtheme(ggtheme)

  # rotate_x_tick_labels
  if (is.waive(rotate_x_tick_labels)) {
    rotate_x_tick_labels <- FALSE
  }

  # Clusters cannot be generated in case no cluster information is
  # present.
  if (is_empty(feature_similarity) || type == "feature_selection") {
    show_cluster <- FALSE
  }

  # y_label
  if (is.waive(y_label)) {
    y_label <- ifelse(x@rank_aggregation_method == "stability", "occurrence", "score")
  }

  # y_range
  if (is.null(y_range) && x@rank_aggregation_method == "stability") {
    # for occurrence plots
    y_range <- c(0.0, 1.0)
    
  } else if (is.null(y_range)) {
    # for variable importance score-based plots
    y_range <- c(0.0, max(x@data$score, na.rm = TRUE))
  }

  # Set y_breaks
  if (is.null(y_breaks)) {
    .check_input_plot_args(
      y_range = y_range,
      y_n_breaks = y_n_breaks
    )

    # Create breaks and update x_range
    y_breaks <- labeling::extended(
      m = y_n_breaks,
      dmin = y_range[1L],
      dmax = y_range[2L],
      only.loose = TRUE
    )
    
    y_range <- c(0.0, tail(y_breaks, n = 1L))
  }

  # Add default parameters.
  if (is.null(split_by) && is.null(color_by) && is.null(facet_by)) {
    split_by <- available_splitting
  }

  # Check splitting variables and generate sanitised output.
  split_var_list <- .check_plot_splitting_variables(
    x = x@data,
    split_by = split_by,
    color_by = color_by,
    facet_by = facet_by,
    available = available_splitting
  )

  # Update splitting variables
  split_by <- split_var_list$split_by
  color_by <- split_var_list$color_by
  facet_by <- split_var_list$facet_by

  # legend_label
  legend_label <- .create_plot_legend_title(
    user_label = legend_label,
    color_by = color_by
  )

  # Perform last checks prior to plotting
  .check_input_plot_args(
    x_label = x_label,
    y_label = y_label,
    legend_label = legend_label,
    plot_title = plot_title,
    plot_sub_title = plot_sub_title,
    caption = caption,
    rotate_x_tick_labels = rotate_x_tick_labels,
    facet_wrap_cols = facet_wrap_cols,
    y_range = y_range,
    y_breaks = y_breaks
  )

  # Create plots ---------------------------------------------------------------

  # Determine if subtitle should be generated.
  autogenerate_plot_subtitle <- is.waive(plot_sub_title)

  # Split data
  if (!is.null(split_by)) {
    x_split <- split(x@data, by = split_by)
  } else {
    x_split <- list(x@data)
  }

  # Store plots to list in case no dir_path is provided
  if (is.null(dir_path)) plot_list <- list()

  # Iterate over splits
  for (x_sub in x_split) {
    if (is_empty(x_sub)) next

    # Rename "name" column to "feature".
    x_sub <- data.table::copy(x_sub)
    data.table::setnames(
      x = x_sub,
      old = "name",
      new = "feature"
    )

    # Join cluster and univariate data.
    if (show_cluster) {
      if (type == "model") {
        x_temporary <- merge(
          x = x_sub,
          y = feature_similarity@data,
          by.x = c("feature", available_splitting, "ensemble_model_name"),
          by.y = c("feature", available_splitting, "ensemble_model_name"),
          allow.cartesian = TRUE
        )

        # Model-based ranking does not aggregate along data-set.
        x_temporary <- x_temporary[data_set %in% c(unique(x_temporary$data_set)[1L])]
      }

      # Check that the resulting data is not empty, because this would
      # mean that there is e.g. only one feature.
      if (is_empty(x_temporary)) {
        x_temporary <- data.table::copy(x_sub)
        x_temporary[, ":="(
          "cluster_id" = .I,
          "cluster_size" = 1L
        )]
      }

      # Replace x_sub
      x_sub <- x_temporary
    }

    if (is.waive(plot_title)) {
      plot_title <- ifelse(
        type == "feature_selection",
        "Feature selection-based variable importance",
        "Model-based variable importance"
      )
    }

    if (autogenerate_plot_subtitle) {
      plot_sub_title <- .create_plot_subtitle(
        split_by = split_by,
        additional = list("aggregation_method" = x@rank_aggregation_method),
        x = x_sub
      )
    }

    # Generate plot
    p <- .create_feature_rank_plot(
      x = x_sub,
      type = type,
      aggregation_method = x@rank_aggregation_method,
      color_by = color_by,
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      show_cluster = show_cluster,
      ggtheme = ggtheme,
      discrete_palette = discrete_palette,
      gradient_palette = gradient_palette,
      x_label = x_label,
      rotate_x_tick_labels = rotate_x_tick_labels,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      y_range = y_range,
      y_breaks = y_breaks
    )

    # Check empty output
    if (is.null(p)) next

    # Draw plot
    if (draw) .draw_plot(plot_or_grob = p)

    # Save and export
    if (!is.null(dir_path)) {
      # Obtain decent default values for the plot.
      def_plot_dims <- .determine_feature_ranking_plot_dimensions(
        x = x_sub,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        rotate_x_tick_labels = rotate_x_tick_labels
      )

      # Save to file.
      do.call(
        .save_plot_to_file,
        args = c(
          list(
            "plot_or_grob" = p,
            "object" = object,
            "dir_path" = dir_path,
            "type" = "variable_importance",
            "subtype" = ifelse(type == "feature_selection", "feature_selection", "learner"),
            "x" = x_sub,
            "split_by" = split_by,
            "additional" = list("aggregation_method" = x@rank_aggregation_method),
            "height" = ifelse(is.waive(height), def_plot_dims[1L], height),
            "width" = ifelse(is.waive(width), def_plot_dims[2L], width),
            "units" = ifelse(is.waive(units), "cm", units)
          ),
          list(...)
        )
      )
    } else {
      # Store as list and export
      plot_list <- c(plot_list, list(p))
    }
  }

  # Generate output
  return(.get_plot_results(
    dir_path = dir_path,
    plot_list = plot_list,
    export_collection = export_collection,
    object = object
  ))
}



.create_feature_rank_plot <- function(
    x,
    type,
    aggregation_method,
    color_by,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    show_cluster,
    discrete_palette,
    gradient_palette,
    x_label,
    rotate_x_tick_labels,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    y_range,
    y_breaks
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Create a local copy of x prior to making changes based on plot_data
  x <- data.table::copy(x)
  x$feature <- droplevels(x$feature)

  # Order by ascending rank.
  x <- x[order(rank)]

  # Update the ordering of features so that the features are ordered by
  # increasing score or occurrence
  x$feature <- factor(x$feature, levels = unique(x$feature))

  # Generate a guide table
  guide_list <- .create_plot_guide_table(
    x = x,
    color_by = color_by,
    discrete_palette = discrete_palette
  )

  # Extract data
  x <- guide_list$data

  # Check if cluster information should be shown:
  if (show_cluster) {
    x <- .add_plot_cluster_name(
      x = x,
      color_by = color_by,
      facet_by = facet_by
    )
  }

  # Perform last checks prior to plotting
  .check_input_plot_args(
    y_range = y_range,
    y_breaks = y_breaks
  )

  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("feature"),
      y = !!sym("score")
    )
  )
  p <- p + ggtheme
  
  # Add fill colours.
  if (!is.null(color_by)) {
    # Extract guide_table for color
    g_color <- guide_list$guide_color

    p <- p + ggplot2::geom_bar(
      stat = "identity",
      mapping = ggplot2::aes(fill = !!sym("color_breaks")),
      position = "dodge"
    )

    p <- p + ggplot2::scale_fill_manual(
      name = legend_label$guide_color,
      values = g_color$color_values,
      breaks = g_color$color_breaks,
      drop = FALSE
    )
    
  } else if (!is.waive(gradient_palette)) {
    # A gradient palette is used to colour the bars by value.
    p <- p + ggplot2::geom_bar(
      stat = "identity",
      mapping = ggplot2::aes(fill = !!sym("value")),
      show.legend = FALSE
    )

    # Determine gradient order. This is so that bars of more important features
    # are always colored with the high-range colors, independent of the
    # orientation of the score. The correct gradient order will be set using the
    # trans argument, which defines the transformations.
    gradient_order <- "identity"

    # Determine best and worst scores.
    best_score <- x[rank == 1L]$score[1L]
    worst_score <- x[rank == max(x$rank)]$score[1L]

    # Invert gradient if the worst score is higher than the best score.
    if (best_score < worst_score) gradient_order <- "reverse"

    # Get gradient colours
    gradient_colours <- .get_palette(
      x = gradient_palette,
      palette_type = "sequential"
    )

    p <- p + ggplot2::scale_fill_gradientn(
      colors = gradient_colours,
      limits = y_range,
      trans = gradient_order
    )
    
  } else {
    # Bars are not coloured based on occurrence/importance.
    p <- p + ggplot2::geom_bar(stat = "identity")
  }

  # Set breaks and  limits on the y-axis
  p <- p + ggplot2::scale_y_continuous(
    breaks = y_breaks,
    limits = y_range
  )

  # Determine how things are facetted
  facet_by_list <- .parse_plot_facet_by(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols
  )

  if (!is.null(facet_by)) {
    if (is.null(facet_wrap_cols)) {
      # Use a grid
      p <- p + ggplot2::facet_grid(
        rows = facet_by_list$facet_rows,
        cols = facet_by_list$facet_cols,
        labeller = "label_context"
      )
      
    } else {
      p <- p + ggplot2::facet_wrap(
        facets = facet_by_list$facet_by,
        labeller = "label_context"
      )
    }
  }

  # Add clustering information.
  if (show_cluster) {
    # Obtain default settings.
    text_settings <- .get_plot_geom_text_settings(ggtheme = ggtheme)

    if (is.null(color_by)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          label = !!sym("cluster_name"),
          y = 0.0
        ),
        colour = text_settings$colour,
        family = text_settings$family,
        fontface = text_settings$face,
        size = text_settings$geom_text_size,
        vjust = "inward"
      )
      
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          label = !!sym("cluster_name"),
          y = 0.0,
          group = !!sym("color_breaks")
        ),
        colour = text_settings$colour,
        family = text_settings$family,
        fontface = text_settings$face,
        size = text_settings$geom_text_size,
        vjust = "inward",
        position = ggplot2::position_dodge(width = 0.9)
      )
    }
  }

  # Update labels.
  p <- p + ggplot2::labs(
    x = x_label,
    y = y_label,
    title = plot_title,
    subtitle = plot_sub_title,
    caption = caption
  )

  # Rotate x-axis ticks
  if (rotate_x_tick_labels) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        vjust = 0.25, 
        hjust = 1.0, 
        angle = 90.0
      )
    )
  }

  return(p)
}



.determine_feature_ranking_plot_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols,
    rotate_x_tick_labels
) {
  # Get plot layout dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols
  )

  # Determine the number of features within each facet.
  n_features <- data.table::uniqueN(x = x$feature)
  longest_name <- max(sapply(levels(x$feature), nchar))

  # Assume each feature takes up about 14 points (~5mm) with 2 point (0.07mm)
  # spacing. Then add some room for other plot elements.
  default_width <- n_features * 0.5 + (n_features - 1L) * 0.07 + 1.0
  default_width <- max(c(4.0, default_width))

  # Set default height.
  default_height <- 4.0

  # Reserve space for x-axis tick labels. Assume that the typical width of a
  # character is about 5 points (1.8 mm). For the x-axis we only reserve extra
  # space in case the ticks are rotated, otherwise we just assume a typical
  # height of 10 points (3.6 mm).
  x_tick_space <- ifelse(rotate_x_tick_labels, longest_name * 0.18, 0.36)

  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2.0 + plot_dims[1L] * default_height + x_tick_space, 27.7))

  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2.0 + plot_dims[2L] * default_width, 19.0))

  return(c(height, width))
}
