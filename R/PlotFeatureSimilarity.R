#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# plot_feature_similarity (generic) --------------------------------------------

#' @title Plot heatmaps for pairwise similarity between features.
#'
#' @description This method creates a heatmap based on data stored in a
#'   `familiarCollection` object. Features in the heatmap are ordered so that
#'   more similar features appear together.
#'
#' @param dir_path (*optional*) Path to the directory where created performance
#'   plots are saved to. Output is saved in the `feature_similarity`
#'   subdirectory. If `NULL` no figures are saved, but are returned instead.
#' @param gradient_palette (*optional*) Sequential or divergent palette used to
#'   colour the similarity or distance between features in a heatmap.
#' @param gradient_palette_range (*optional*) Numerical range used to span the
#'   gradient. This should be a range of two values, e.g. `c(0, 1)`. Lower or
#'   upper boundary can be unset by using `NA`. If not set, the full
#'   metric-specific range is used.
#' @param show_dendrogram (*optional*) Show dendrogram around the main panel.
#'   Can be `TRUE`, `FALSE`, `NULL`, or a position, i.e. `top`, `bottom`, `left`
#'   and `right`. Up to two positions may be provided, but only as long as the
#'   dendrograms are not on opposite sides of the heatmap: `top` and `bottom`,
#'   and `left` and `right` cannot be used together.
#'
#'   A dendrogram can only be drawn from cluster methods that produce
#'   dendrograms, such as `hclust`. A dendrogram can for example not be
#'   constructed using the partitioning around medioids method (`pam`).
#'
#'   By default, a dendrogram is drawn to the top and right of the panel.
#' @param dendrogram_height (*optional*) Height of the dendrogram. The height is
#'   1.5 cm by default. Height is expected to be grid unit (see `grid::unit`),
#'   which also allows for specifying relative heights.
#' @inheritParams export_feature_similarity
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units
#' @inheritDotParams extract_feature_similarity -object -feature_cluster_method -feature_linkage_method -feature_cluster_cut_method -feature_similarity_threshold
#'
#' @details This function generates area under the ROC curve plots.
#'
#'   Available splitting variables are: `fs_method`, `learner`, and `data_set`.
#'   By default, the data is split by `fs_method` and `learner`, with facetting
#'   by `data_set`.
#'
#'   Note that similarity is determined based on the underlying data. Hence the
#'   ordering of features may differ between facets, and tick labels are
#'   maintained for each panel.
#'
#'   Available palettes for `gradient_palette` are those listed by
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
#' @exportMethod plot_feature_similarity
#' @md
#' @rdname plot_feature_similarity-methods
setGeneric(
  "plot_feature_similarity",
  function(
    object,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    gradient_palette_range = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 3,
    y_breaks = NULL,
    rotate_x_tick_labels = waiver(),
    show_dendrogram = c("top", "right"),
    dendrogram_height = grid::unit(1.5, "cm"),
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    standardGeneric("plot_feature_similarity")
  }
)



# plot_feature_similarity (general) --------------------------------------------

#' @rdname plot_feature_similarity-methods
setMethod(
  "plot_feature_similarity",
  signature(object = "ANY"),
  function(
    object,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    gradient_palette_range = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 3,
    y_breaks = NULL,
    rotate_x_tick_labels = waiver(),
    show_dendrogram = c("top", "right"),
    dendrogram_height = grid::unit(1.5, "cm"),
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
          "data_element" = "feature_similarity",
          "feature_cluster_method" = feature_cluster_method,
          "feature_linkage_method" = feature_linkage_method,
          "feature_cluster_cut_method" = feature_cluster_cut_method,
          "feature_similarity_threshold" = feature_similarity_threshold),
        list(...)))

    return(do.call(
      plot_feature_similarity,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "gradient_palette" = gradient_palette,
        "gradient_palette_range" = gradient_palette_range,
        "x_label" = x_label,
        "x_label_shared" = x_label_shared,
        "y_label" = y_label,
        "y_label_shared" = y_label_shared,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "y_range" = y_range,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "rotate_x_tick_labels" = rotate_x_tick_labels,
        "show_dendrogram" = show_dendrogram,
        "dendrogram_height" = dendrogram_height,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection)))
  }
)



# plot_feature_similarity (collection) -----------------------------------------

#' @rdname plot_feature_similarity-methods
setMethod(
  "plot_feature_similarity",
  signature(object = "familiarCollection"),
  function(
    object,
    feature_cluster_method = waiver(),
    feature_linkage_method = waiver(),
    feature_cluster_cut_method = waiver(),
    feature_similarity_threshold = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    gradient_palette_range = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 3,
    y_breaks = NULL,
    rotate_x_tick_labels = waiver(),
    show_dendrogram = c("top", "right"),
    dendrogram_height = grid::unit(1.5, "cm"),
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...) {
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Get input data
    x <- export_feature_similarity(
      object = object,
      feature_cluster_method = feature_cluster_method,
      feature_linkage_method = feature_linkage_method,
      feature_cluster_cut_method = feature_cluster_cut_method,
      feature_similarity_threshold = feature_similarity_threshold,
      export_dendrogram = FALSE,
      export_ordered_data = FALSE)

    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)

    # Obtain data element from list.
    if (is.list(x)) {
      if (length(x) > 1) {
        ..error_reached_unreachable_code(
          "plot_feature_similarity: list of data elements contains unmerged elements.")
      }

      # Get x directly.
      x <- x[[1]]
    }

    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)

    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = TRUE),
      purpose = "to plot feature similarity heatmaps",
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

    # x_label
    if (is.waive(x_label)) x_label <- NULL

    # y_label
    if (is.waive(y_label)) y_label <- NULL

    # legend_label
    if (is.waive(legend_label)) legend_label <- NULL

    # show_dendrogram
    if (is.logical(show_dendrogram)) {
      if (show_dendrogram) {
        show_dendrogram <- c("top", "right")
      } else {
        show_dendrogram <- NULL
      }
      
    } else if (length(show_dendrogram) == 0) {
      show_dendrogram <- NULL
      
    } else {
      .check_parameter_value_is_valid(
        x = show_dendrogram,
        var_name = "show_dendrogram",
        values = c("top", "bottom", "left", "right"))

      # Check that bottom and top, and left and right do not appear together.
      if (all(c("top", "bottom") %in% show_dendrogram)) {
        stop("Dendrograms can not be drawn both above and below the plot.")
      } else if (all(c("left", "right") %in% show_dendrogram)) {
        stop("Dendrograms can not be drawn both to the left and right of the plot.")
      }
    }

    if (!x@cluster_method %in% c("hclust", "agnes", "diana")) show_dendrogram <- NULL

    # Check if the dendrogram_height argument is correct.
    if (!is.null(show_dendrogram)) {
      .check_plot_grid_unit(x = dendrogram_height, var_name = "dendrogram_height")
    }

    # Add default splitting variables
    if (is.null(split_by) & is.null(facet_by)) {
      # Split by feature selection method and learner
      split_by <- c("fs_method", "learner")

      # Facet by dataset
      facet_by <- "data_set"
    }

    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x@data,
      split_by = split_by,
      facet_by = facet_by,
      available = c("data_set", "fs_method", "learner"))

    # Update splitting variables
    split_by <- split_var_list$split_by
    facet_by <- split_var_list$facet_by

    # Check input arguments
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      rotate_x_tick_labels = rotate_x_tick_labels)

    ##### Create plots -------------------------------------------------

    # Determine if subtitle should be generated.
    autogenerate_plot_subtitle <- is.waive(plot_sub_title)

    # Split data.
    if (!is.null(split_by)) {
      x_split <- split(x@data, by = split_by, drop = FALSE)
    } else {
      x_split <- list("null.name" = x@data)
    }

    # Store plots to list in case no dir_path is provided
    if (is.null(dir_path)) plot_list <- list()

    # Iterate over splits
    for (x_sub in x_split) {
      if (is_empty(x_sub)) next

      if (is.waive(plot_title)) plot_title <- "Feature similarity"

      if (autogenerate_plot_subtitle) {
        plot_sub_title <- plotting.create_subtitle(
          split_by = split_by,
          additional = list("metric" = x@similarity_metric),
          x = x_sub)
      }

      # Generate plot
      p <- .plot_feature_similarity_plot(
        x = x_sub,
        data = x,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        ggtheme = ggtheme,
        gradient_palette = gradient_palette,
        gradient_palette_range = gradient_palette_range,
        x_label = x_label,
        x_label_shared = x_label_shared,
        y_label = y_label,
        y_label_shared = y_label_shared,
        legend_label = legend_label,
        plot_title = plot_title,
        plot_sub_title = plot_sub_title,
        caption = caption,
        y_range = y_range,
        y_n_breaks = y_n_breaks,
        y_breaks = y_breaks,
        rotate_x_tick_labels = rotate_x_tick_labels,
        show_dendrogram = show_dendrogram,
        dendrogram_height = dendrogram_height)

      # Check empty output
      if (is.null(p)) next

      # Draw figure.
      if (draw) plotting.draw(plot_or_grob = p)

      # Save and export
      if (!is.null(dir_path)) {
        # Identify unique features:
        features <- unique(c(x_sub$feature_name_1, x_sub$feature_name_2))

        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_feature_similarity_plot_dimensions(
          x = x_sub,
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols,
          features = as.character(features),
          show_dendrogram = show_dendrogram,
          rotate_x_tick_labels = rotate_x_tick_labels)

        # Save to file.
        do.call(
          plotting.save_plot_to_file,
          args = c(
            list(
              "plot_obj" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "feature_similarity",
              "subtype" = "similarity",
              "x" = x_sub,
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



.plot_feature_similarity_plot <- function(
    x,
    data,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    gradient_palette,
    gradient_palette_range,
    x_label,
    x_label_shared,
    y_label,
    y_label_shared,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    y_range,
    y_n_breaks,
    y_breaks,
    rotate_x_tick_labels,
    show_dendrogram,
    dendrogram_height) {
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- NULL
  
  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- plotting.get_plot_layout_table(
    x = x,
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
        "familiarDataElementFeatureSimilarity",
        data,
        data = x[current_split, on = .NATURAL, nomatch = NULL])
      
    } else {
      x_split <- methods::new(
        "familiarDataElementFeatureSimilarity",
        data,
        data = x)
    }

    # Add in clustering information and a dendrogram.
    x_split <- .append_feature_similarity_dendrogram(x_split)
    x_split <- .append_feature_similarity_clustering(x_split)

    # Find the cluster object
    dendrogram <- x_split@dendrogram

    # Complete the similarity data
    similarity_data <- .complete_feature_similarity_table(
      x = x_split@data,
      similarity_metric = x_split@similarity_metric)

    # Create similarity heatmap
    p_heatmap <- .create_feature_similarity_heatmap(
      x = similarity_data,
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      ggtheme = ggtheme,
      gradient_palette = gradient_palette,
      gradient_palette_range = gradient_palette_range,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      rotate_x_tick_labels = rotate_x_tick_labels,
      show_dendrogram = show_dendrogram,
      similarity_metric = x_split@similarity_metric)

    # Extract plot elements from the heatmap.
    extracted_elements <- plotting.extract_plot_elements(p = p_heatmap)

    # Remove extracted elements from the heatmap.
    p_heatmap <- plotting.remove_plot_elements(p = p_heatmap)

    # Rename plot elements.
    g_heatmap <- plotting.rename_plot_elements(
      g = plotting.to_grob(p_heatmap),
      extension = "main")

    # Add dendrogram
    if (!is.null(show_dendrogram) && inherits(dendrogram, "hclust")) {
      # Obtain dendrogram plotting data as line segments.
      dendro_data <- plotting.dendrogram_as_table(
        h = dendrogram,
        similarity_metric = x_split@similarity_metric
      )

      for (position in show_dendrogram) {
        # Plot dendrogram
        p_dendro <- .create_feature_similarity_dendrogram_plot(
          x = dendro_data,
          position = position,
          ggtheme = ggtheme,
          y_range = y_range,
          y_n_breaks = y_n_breaks,
          y_breaks = y_breaks,
          plot_height = dendrogram_height,
          rotate_x_tick_labels = rotate_x_tick_labels)

        # Determine the axis element
        axis_element <- ifelse(position %in% c("top", "bottom"), "axis-l", "axis-b")

        # Extract dendrogram gtable, which consists of the panel and the height
        # axis.
        g_dendro <- .gtable_extract(
          g = plotting.to_grob(p_dendro),
          element = c("panel", axis_element),
          partial_match = TRUE)

        # Insert the dendrogram at the position correct position around the
        # heatmap.
        g_heatmap <- .gtable_insert(
          g = g_heatmap,
          g_new = g_dendro,
          where = position,
          ref_element = "panel-main",
          partial_match = TRUE)
      }
    }

    # Add combined grob to list
    figure_list <- c(figure_list, list(g_heatmap))

    # Add extract elements to the extracted_element_list
    extracted_element_list <- c(extracted_element_list, list(extracted_elements))
  }

  # Update the layout table.
  plot_layout_table <- plotting.update_plot_layout_table(
    plot_layout_table = plot_layout_table,
    grobs = figure_list,
    x_text_shared = FALSE,
    x_label_shared = x_label_shared,
    y_text_shared = FALSE,
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



.create_feature_similarity_heatmap <- function(
    x,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    gradient_palette,
    gradient_palette_range,
    x_label,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    rotate_x_tick_labels,
    show_dendrogram,
    similarity_metric) {
  
  if (is.null(gradient_palette_range) && !is.null(similarity_metric)) {
    # Find the palette range.
    gradient_palette_range <- get_similarity_range(similarity_metric = similarity_metric)
  }

  # Determine whether a sequential or divergent palette should be used by default.
  palette_type <- ifelse(
    length(gradient_palette_range) > 2,
    "divergent",
    "sequential")

  # Should the palette be inverted? This is because for some metrics, clusters
  # are those with least distance, not highest similarity.
  invert_palette <- FALSE
  if (!is.null(similarity_metric)) {
    invert_palette <- is_default_distance(similarity_metric = similarity_metric)
  }

  # Create basic plot
  p <- ggplot2::ggplot(data = x, mapping = ggplot2::aes(
    x = !!sym("feature_name_1"),
    y = !!sym("feature_name_2"),
    fill = !!sym("value")))
  p <- p + ggtheme

  if (!is_empty(x)) {
    p <- p + ggplot2::geom_raster()
  } else {
    # Insert blank for empty datasets.
    p <- p + ggplot2::geom_blank()
  }


  # Colors
  gradient_colours <- .get_palette(
    x = gradient_palette,
    palette_type = palette_type,
    diverge_to_white = TRUE)
  if (invert_palette) gradient_colours <- rev(gradient_colours)

  if (length(gradient_palette_range) > 0) {
    # Add gradient palette. If the legend is not shown, legend_label equals
    # NULL.
    p <- p + ggplot2::scale_fill_gradientn(
      name = legend_label,
      colors = gradient_colours,
      limits = range(gradient_palette_range),
      oob = scales::squish)
  }

  # Show dendrogram determines where tick labels and axis labels are placed. By
  # default axes are located at the bottom and right.
  x_axis_position <- "bottom"
  y_axis_position <- "left"
  if (!is.null(show_dendrogram)) {
    if ("bottom" %in% show_dendrogram) {
      x_axis_position <- "top"
    }

    if ("left" %in% show_dendrogram) {
      y_axis_position <- "right"
    }
  }

  # Specify both axes. Note that only the heatmap is shown, without additional
  # space between the plot area and the axes.
  p <- p + ggplot2::scale_x_discrete(position = x_axis_position, expand = c(0, 0))
  p <- p + ggplot2::scale_y_discrete(position = y_axis_position, expand = c(0, 0))

  # Set labels.
  p <- p + ggplot2::labs(
    x = x_label,
    y = y_label,
    title = plot_title,
    subtitle = plot_sub_title,
    caption = caption)

  # Determine how plots are facetted. The actual facets are created in the
  # calling function, not here.
  facet_by_list <- plotting.parse_facet_by(
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

  return(p)
}



.create_feature_similarity_dendrogram_plot <- function(
    x,
    position,
    ggtheme,
    y_range,
    y_n_breaks,
    y_breaks,
    plot_height,
    rotate_x_tick_labels) {
  # Check if there is any data to plot.
  if (is_empty(x)) return(NULL)
  
  # Define the range along the x-axis.
  x_range <- range(x$x_1)
  x_range <- c(x_range[1] - 0.5, x_range[2] + 0.5)

  # y_range
  if (is.null(y_range)) y_range <- range(c(x$y_1, x$y_2))

  # y_breaks
  if (is.null(y_breaks)) {
    .check_input_plot_args(
      y_range = y_range,
      y_n_breaks = y_n_breaks)

    # Create breaks and update y_range
    y_breaks <- labeling::extended(
      m = y_n_breaks,
      dmin = y_range[1],
      dmax = y_range[2],
      only.loose = TRUE)

    y_range <- c(
      head(y_breaks, n = 1),
      tail(y_breaks, n = 1))
  }

  .check_input_plot_args(
    y_range = y_range,
    y_breaks = y_breaks)

  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("x_1"),
      y = !!sym("y_1"),
      xend = !!sym("x_2"),
      yend = !!sym("y_2")))
  p <- p + ggtheme
  
  # Plot line segments.
  p <- p + ggplot2::geom_segment(lineend = "round")
  
  if (position == "right") {
    p <- p + ggplot2::scale_x_continuous(
      limits = x_range, 
      expand = c(0, 0))
    p <- p + ggplot2::scale_y_continuous(
      limits = y_range, 
      breaks = y_breaks)
    p <- p + ggplot2::coord_flip()
    
  } else if (position == "bottom") {
    p <- p + ggplot2::scale_x_continuous(
      limits = x_range, 
      expand = c(0, 0))
    p <- p + ggplot2::scale_y_reverse(
      limits = rev(y_range),
      breaks = rev(y_breaks))
    
  } else if (position == "left") {
    p <- p + ggplot2::scale_x_continuous(
      limits = x_range, 
      expand = c(0, 0))
    p <- p + ggplot2::scale_y_reverse(
      limits = rev(y_range),
      breaks = rev(y_breaks))
    p <- p + ggplot2::coord_flip()
    
  } else if (position == "top") {
    p <- p + ggplot2::scale_x_continuous(
      limits = x_range, 
      expand = c(0, 0))
    p <- p + ggplot2::scale_y_continuous(
      limits = y_range, 
      breaks = y_breaks)
  } else {
    ..error_reached_unreachable_code(paste0(
      ".create_feature_similarity_dendrogram_plot: unknown position encountered: ",
      position))
  }

  # Remove some theme elements and reduce margins. The histogram height is left.
  p <- p + ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank())

  if (position %in% c("top", "bottom")) {
    # Remove x-axis
    p <- p + ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    )
  } else if (position %in% c("left", "right")) {
    # Remove y-axis (rotated x-axis in plot)
    p <- p + ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
    # Rotate x-labels
    if (rotate_x_tick_labels) {
      p <- p + ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          vjust = 0.25, 
          hjust = 1.0, 
          angle = 90.0))
    }
  }

  # Ensure that panel heights/widths are set to the plot object.
  if (position %in% c("top", "bottom")) {
    p$custom_grob <- list("heights" = list("name" = "panel", "height" = plot_height))
  } else if (position %in% c("left", "right")) {
    p$custom_grob <- list("widths" = list("name" = "panel", "width" = plot_height))
  }

  class(p) <- c("familiar_ggplot", class(p))

  return(p)
}



.determine_feature_similarity_plot_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols,
    features,
    rotate_x_tick_labels,
    show_dendrogram) {
  # Obtain facetting dimensions
  plot_dims <- plotting.get_plot_layout_dims(
    x = x,
    facet_by = facet_by, 
    facet_wrap_cols = facet_wrap_cols)

  # Determine the number of elements along the x-axis.
  x_n_elements <- y_n_elements <- length(features)
  x_longest_element <- y_longest_element <- max(sapply(features, nchar))

  # Assume each x-axis element takes up about 0.5 cm. Then add some room for
  # other plot elements.
  default_width <- x_n_elements * 0.5 + 1.0

  # Assume each y-axis element takes up about 0.5 cm as well.
  default_height <- y_n_elements * 0.5 + 1.0

  # Reserve space for y-axis and x-axis tick labels. Assume that the typical
  # width of a character is about 5 points (1.8 mm). For the x-axis we only
  # reserve extra space in case the ticks are rotated, otherwise we just
  # assume a typical height of 10 points (3.6 mm).
  y_tick_space <- y_longest_element * 0.18
  x_tick_space <- ifelse(rotate_x_tick_labels, x_longest_element * 0.18, 0.36)

  # Reserve space for the dendrograms (1.5 cm)
  dendro_height <- ifelse(any(c("top", "bottom") %in% show_dendrogram), 1.5, 0.0)
  dendro_width <- ifelse(any(c("left", "right") %in% show_dendrogram), 1.5, 0.0)

  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2 + plot_dims[1] * (default_height + x_tick_space + dendro_height), 27.7))

  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2 + plot_dims[2] * (default_width + y_tick_space + dendro_width), 19))

  return(c(height, width))
}


.complete_feature_similarity_table <- function(x, similarity_metric) {
  # Check for empty file.
  if (is_empty(x)) {
    return(NULL)
  }

  # Copy x and revert feature_name_1 and feature_name_2 so that all pairs are
  # present.
  y <- data.table::copy(x)
  data.table::setnames(y,
    old = c("feature_name_1", "feature_name_2", "label_order_1", "label_order_2"),
    new = c("feature_name_2", "feature_name_1", "label_order_2", "label_order_1")
  )

  # Combine.
  x <- rbind(x, y, use.names = TRUE)

  # Determine order of features.
  feature_1_order <- unique(x[, c("feature_name_1", "label_order_1")])
  feature_2_order <- unique(x[, c("feature_name_2", "label_order_2")])

  # Add self-paired features.
  features <- unique(x$feature_name_1)
  y <- x[rep(1, length(features))]
  y[, ":="("feature_name_1" = features,
    "feature_name_2" = features,
    "value" = 1.0,
    "label_order_1" = NULL,
    "label_order_2" = NULL)]
  y <- merge(x = y, y = feature_1_order, by = "feature_name_1", all = FALSE)
  y <- merge(x = y, y = feature_2_order, by = "feature_name_2", all = FALSE)

  # Combine.
  x <- rbind(x, y, use.names = TRUE)

  # Reorder features
  x$feature_name_1 <- factor(
    x = x$feature_name_1, 
    levels = feature_1_order$feature_name_1[order(feature_1_order$label_order_1)])
  x$feature_name_2 <- factor(
    x = x$feature_name_2,
    levels = feature_2_order$feature_name_2[order(feature_2_order$label_order_2)])

  return(x)
}
