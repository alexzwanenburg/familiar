..required_plotting_packages <- function(extended = FALSE) {
  plot_packages <- c("ggplot2", "labeling", "scales", "rlang")

  if (extended) plot_packages <- c(plot_packages, "gtable")

  return(plot_packages)
}



#' Familiar ggplot2 theme
#'
#' This is the default theme used for plots created by familiar. The theme uses
#' `ggplot2::theme_light` as the base template.
#'
#' @param base_size Base font size in points. Size of other plot text elements
#'   is based off this.
#' @param base_family Font family used for text elements.
#' @param base_line_size Base size for line elements, in points.
#' @param base_rect_size Base size for rectangular elements, in points.
#'
#' @return A complete plotting theme.
#' @export
theme_familiar <- function(
    base_size = 10,
    base_family = "",
    base_line_size = 0.5,
    base_rect_size = 0.5) {
  
  # The default familiar theme is based on ggplot2::theme_light.
  ggtheme <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size)

  # Set colour to black.
  ggtheme$axis.text$colour <- "black"
  ggtheme$axis.ticks$colour <- "black"
  ggtheme$axis.line <- ggplot2::element_line(
    colour = "black",
    lineend = "square")

  # Legend does not have a legend or border, and reserves less space.
  ggtheme$legend.background <- ggplot2::element_blank()
  ggtheme$legend.key <- ggplot2::element_blank()
  ggtheme$legend.key.size <- grid::unit(base_size * 1.1, "pt")
  ggtheme$legend.justification <- c("left", "center")
  ggtheme$legend.margin <- grid::unit(c(0.0, 0.0, 0.0, 0.0), "pt")

  # Panel does not have a background, grid, or border.
  ggtheme$panel.background <- ggplot2::element_blank()
  ggtheme$panel.border <- ggplot2::element_blank()
  ggtheme$panel.grid <- ggplot2::element_blank()

  # Avoid removing some elements altogether by directly assigning NULL.
  ggtheme["panel.grid.major"] <- list(NULL)
  ggtheme["panel.grid.minor"] <- list(NULL)

  # Minimal plot margins
  ggtheme$plot.margin <- grid::unit(c(1.0, 1.0, 1.0, 1.0), "pt")

  # The plot does not have a background
  ggtheme$plot.background <- ggplot2::element_blank()

  # Make title a bit smaller, and bold.
  ggtheme$plot.title <- ggplot2::element_text(
    face = "bold",
    size = ggplot2::rel(1.1),
    hjust = 0.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2))

  # Make subtitle a bit smaller.
  ggtheme$plot.subtitle <- ggplot2::element_text(
    size = ggplot2::rel(0.8),
    hjust = 0.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2))

  # Make caption a bit smaller.
  ggtheme$plot.caption <- ggplot2::element_text(
    size = ggplot2::rel(0.7),
    hjust = 1.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2))

  # Make tag bold.
  ggtheme$plot.tag <- ggplot2::element_text(
    face = "bold",
    hjust = 0,
    vjust = 0.7)

  # Make strip text black.
  ggtheme$strip.text <- ggplot2::element_text(
    size = ggplot2::rel(0.8),
    colour = "grey10",
    margin = grid::unit(
      c(base_size / 4, base_size / 4, base_size / 4, base_size / 4), "pt"))

  # Remove strip background
  ggtheme$strip.background <- ggplot2::element_blank()

  return(ggtheme)
}



.check_ggtheme <- function(ggtheme) {
  # Check if the provided theme is a suitable theme.
  if (inherits(ggtheme, "theme")) {
    # Check if the theme is complete.
    if (!attr(ggtheme, "complete")) {
      stop(paste0(
        "The plotting theme is not complete. The most likely cause is lack ",
        "of a valid template, such as theme_familiar or ggplot2::theme_light. ",
        "Note that ggplot2::theme is designed to tweak existing themes when creating a plot."))
    }
    
  } else if (is.null(ggtheme)) {
    ggtheme <- theme_familiar(base_size = 9)
    
  } else {
    # Get the specified theme.
    ggtheme_fun <- switch(
      ggtheme,
      "default" = theme_familiar,
      "theme_familiar" = theme_familiar,
      "theme_gray" = ggplot2::theme_gray,
      "theme_grey" = ggplot2::theme_grey,
      "theme_bw" = ggplot2::theme_bw,
      "theme_linedraw" = ggplot2::theme_linedraw,
      "theme_light" = ggplot2::theme_light,
      "theme_dark" = ggplot2::theme_dark,
      "theme_minimal" = ggplot2::theme_minimal,
      "theme_classic" = ggplot2::theme_classic)

    if (is.null(ggtheme_fun)) {
      stop(paste0(
        "The selected theme is not the default theme, or a standard ggplot2 theme. Found: ", ggtheme))
    }

    # Generate theme
    ggtheme <- ggtheme_fun(base_size = 9)
  }

  return(ggtheme)
}



#' Checks and sanitizes splitting variables for plotting.
#'
#' @param x data.table or data.frame containing the data used for splitting.
#' @param split_by (*optional*) Splitting variables. This refers to column names
#'   on which datasets are split. A separate figure is created for each split.
#'   See details for available variables.
#' @param color_by (*optional*) Variables used to determine fill colour of plot
#'   objects. The variables cannot overlap with those provided to the `split_by`
#'   argument, but may overlap with other arguments. See details for available
#'   variables.
#' @param linetype_by (*optional*) Variables that are used to determine the
#'   linetype of lines in a plot. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   Sett details for available variables.
#' @param facet_by (*optional*) Variables used to determine how and if facets of
#'   each figure appear. In case the `facet_wrap_cols` argument is `NULL`, the
#'   first variable is used to define columns, and the remaing variables are
#'   used to define rows of facets. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   See details for available variables.
#' @param x_axis_by (*optional*) Variable plotted along the x-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `y_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param y_axis_by (*optional*) Variable plotted along the y-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `x_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param available Names of columns available for splitting.
#'
#' @details This internal function allows some flexibility regarding the exact
#'   input. Allowed splitting variables should be defined by the available
#'   argument.
#'
#' @return A sanitized list of splitting variables.
#' @md
#' @keywords internal
.check_plot_splitting_variables <- function(
    x,
    split_by = NULL,
    color_by = NULL,
    linetype_by = NULL,
    facet_by = NULL,
    x_axis_by = NULL,
    y_axis_by = NULL,
    available = NULL) {
  # Find unique variables
  splitting_vars <- c(split_by, color_by, linetype_by, facet_by, x_axis_by, y_axis_by)

  if (is.null(available) && length(splitting_vars) == 0) {
    return(list())
    
  } else if (is.null(available) && length(splitting_vars) > 0) {
    stop(paste0(
      "The current plot has no required splitting variables defined, but ",
      paste_s(splitting_vars),
      ifelse(length(splitting_vars) == 1, " was assigned.", " were assigned.")))
  }

  # Filter available down to those present in the data.
  filter_available <- intersect(available, colnames(x))

  # Filter available down to those that have more than one variable
  filter_available <- filter_available[
    sapply(
      filter_available,
      function(ii, x) (data.table::uniqueN(x = x, by = ii) > 1),
      x = x)]
  
  if (is.null(filter_available)) {
    return(list())
    
  } else if (!all(filter_available %in% splitting_vars)) {
    missing_vars <- filter_available[!filter_available %in% splitting_vars]
    stop(paste0(
      "The current plot requires ",
      paste_s(filter_available),
      ifelse(length(filter_available) > 1, " as splitting variables", " as a splitting_variable"),
      ", but ",
      paste_s(missing_vars),
      ifelse(length(missing_vars) == 1, " was not assigned.", " were not assigned.")))
  }

  # Update available
  available <- filter_available

  # Generate output
  output_list <- list()

  # Update split_by
  if (!is.null(split_by) && any(split_by %in% available)) {
    output_list$split_by <- intersect(split_by, available)
  }

  # Update color_by
  if (!is.null(color_by) && any(color_by %in% available)) {
    output_list$color_by <- intersect(color_by, available)
  }

  # update linetype_by
  if (!is.null(linetype_by) && any(linetype_by %in% available)) {
    output_list$linetype_by <- intersect(linetype_by, available)
  }

  # update facet_by
  if (!is.null(facet_by) && any(facet_by %in% available)) {
    output_list$facet_by <- intersect(facet_by, available)
  }

  # update x_axis_by
  if (!is.null(x_axis_by) && any(x_axis_by %in% available)) {
    output_list$x_axis_by <- intersect(x_axis_by, available)
  }

  # update y_axis_by
  if (!is.null(y_axis_by) && any(y_axis_by %in% available)) {
    output_list$y_axis_by <- intersect(y_axis_by, available)
  }

  # Check split_by variable.
  .check_value_not_shared(output_list$split_by, output_list$color_by, "split_by", "color_by")
  .check_value_not_shared(output_list$split_by, output_list$linetype_by, "split_by", "linetype_by")
  .check_value_not_shared(output_list$split_by, output_list$facet_by, "split_by", "facet_by")
  .check_value_not_shared(output_list$split_by, output_list$x_axis_by, "split_by", "x_axis_by")
  .check_value_not_shared(output_list$split_by, output_list$y_axis_by, "split_by", "y_axis_by")

  # Check x_axis_by variable and y_axis_by variables.
  .check_value_not_shared(output_list$x_axis_by, output_list$y_axis_by, "x_axis_by", "y_axis_by")

  # Check length of x_axis_by and y_axis_by variables.
  .check_argument_length(output_list$x_axis_by, "x_axis_by", min = 0, max = 1)
  .check_argument_length(output_list$y_axis_by, "y_axis_by", min = 0, max = 1)

  return(output_list)
}



.parse_plot_facet_by <- function(x, facet_by, facet_wrap_cols) {
  if (is.null(facet_by)) {
    return(list())
    
  } else if (length(facet_by) == 1) {
    if (is.null(facet_wrap_cols)) {
      return(list("facet_cols" = quos(!!ensym(facet_by))))
    } else {
      return(list("facet_by" = quos(!!ensym(facet_by))))
    }
    
  } else {
    if (is.null(facet_wrap_cols)) {
      facet_col <- facet_by[1]
      facet_rows <- facet_by[2:length(facet_by)]
      return(list(
        "facet_cols" = quos(!!ensym(facet_col)),
        "facet_rows" = quos(!!!parse_exprs(facet_rows))))
      
    } else {
      return(list("facet_by" = quos(!!!parse_exprs(facet_by))))
    }
  }
}



.create_plot_subtitle <- function(
    x, 
    split_by = NULL, 
    additional = NULL) {
  # Do not create a subtitle if there is no subtitle to be created.
  subtitle <- NULL

  # Generate subtitle from splitting variables and data.
  if (!is.null(split_by)) {
    subtitle <- c(
      subtitle,
      sapply(
        split_by,
        function(name, x) {
          split_variable_name <- name

          if (split_variable_name == "fs_method") {
            split_variable_name <- "VIMP method"
          } else if (split_variable_name == "data_set") {
            split_variable_name <- "data set"
          } else if (split_variable_name == "evaluation_time") {
            split_variable_name <- "time point"
          }

          # Remove all underscores.
          split_variable_name <- gsub(
            x = split_variable_name, 
            pattern = "_",
            replacement = " ",
            fixed = TRUE)

          # Parse to an elementary string.
          split_variable_name <- paste0(split_variable_name, ": ", x[[name]][1])

          return(split_variable_name)
        },
        x = x))
  }

  # Generate additional strings from additional.
  if (!is.null(additional)) {
    subtitle <- c(
      subtitle,
      mapply(
        function(name, value) {
          # Remove all underscores.
          split_variable_name <- gsub(
            x = name, 
            pattern = "_",
            replacement = " ",
            fixed = TRUE)

          # Parse to an elementary string.
          split_variable_name <- paste0(split_variable_name, ": ", value)
        },
        name = names(additional),
        value = additional))
  }

  # Check if any subtitle was generated.
  if (is.null(subtitle)) return(NULL)

  # Combine into single string.
  subtitle <- paste_s(subtitle)

  return(subtitle)
}



.add_time_to_plot_subtitle <- function(value) {
  return(list("time point" = value))
}



.create_plot_subtype <- function(
    x,
    subtype = NULL, 
    split_by = NULL, 
    additional = NULL) {
  # Generate additional terms for the subtype, based on splits.
  if (!is.null(split_by)) {
    subtype <- c(
      subtype,
      as.character(sapply(
        split_by,
        function(jj, x) (x[[jj]][1]),
        x = x)))
  }

  if (!is.null(additional)) {
    subtype <- c(
      subtype,
      sapply(additional, function(jj) as.character(jj[1])))
  }

  if (is.null(subtype)) return(NULL)

  # Combine into a single string.
  subtype <- paste0(subtype, collapse = "_")

  return(subtype)
}



.add_plot_cluster_name <- function(
    x, 
    color_by = NULL, 
    facet_by = NULL, 
    singular_cluster_character = "\u2014") {
  # Suppress NOTES due to non-standard evaluation in data.table
  cluster_size <- cluster_id <- feature <- new_cluster_id <- cluster_name <- NULL

  ..integer_to_char <- function(x) {
    # Initialise placeholders
    x_remain <- x
    new_string <- character(0)

    while (ceiling(x_remain / 26) > 0) {
      # Determine the modulo.
      mod <- x_remain %% 26

      # Find if mod is equal to 0, which would indicate Z.
      mod <- ifelse(mod == 0, 26, mod)

      # Add letter
      new_string <- c(new_string, LETTERS[mod])

      # Update the remain variable
      x_remain <- (x_remain - mod) / 26
    }

    return(paste(rev(new_string), collapse = ""))
  }

  # Identify splitting variables
  splitting_vars <- unique(c(color_by, facet_by))

  # Split x by splitting variables.
  if (length(splitting_vars) > 0) {
    x <- split(x, by = splitting_vars)
  } else {
    x <- list(x)
  }

  # Iterate and add cluster names.
  x <- lapply(
    x,
    function(y) {
      # Check if x is empty.
      if (is_empty(y)) return(NULL)
      
      # This is for backward compatibility.
      if (!all(c("cluster_id", "cluster_size") %in% colnames(y))) {
        # Add cluster name
        y[, "cluster_name" := singular_cluster_character]
        
        return(y)
      }
      
      # Only determine cluster_name for those clusters that have cluster_size >
      # 1. Also, the most important features should receive a higher replacement
      # cluster_id.
      y_short <- y[cluster_size > 1, mget(c("feature", "cluster_id"))]
      
      if (!is_empty(y_short)) {
        # Remove unused levels for the name column. The levels of name are
        # ordered according to importance.
        y_short <- droplevels(y_short)
        
        # Set placeholder cluster id
        y_short[, "new_cluster_id" := NA_integer_]
        
        new_id <- 1L
        for (current_feature in levels(y_short$feature)) {
          # Provide new cluster id in case none exists.
          if (is.na(y_short[feature == current_feature, ]$new_cluster_id[1])) {
            # Find the old cluster id.
            old_cluster_id <- y_short[feature == current_feature, ]$cluster_id[1]
            
            # Update all entries with the same old cluster id.
            y_short[cluster_id == old_cluster_id, "new_cluster_id" := new_id]
            
            # Increment new cluster id.
            new_id <- new_id + 1L
          }
        }
        
        # Determine cluster name based on id.
        y_short[, "cluster_name" := ..integer_to_char(new_cluster_id), by = "feature"]
        
        # Drop redundant columns
        y_short[, ":="(
          "cluster_id" = NULL, 
          "new_cluster_id" = NULL)]
        
        # Merge with y.
        y <- merge(
          x = y, 
          y = y_short,
          by = "feature",
          all = TRUE)
        
        # Mark singular clusters
        y[is.na(cluster_name), "cluster_name" := singular_cluster_character]
        
      } else {
        # Mark singular clusters
        y[, "cluster_name" := singular_cluster_character]
      }
      
      return(y)
    }
  )
  
  x <- data.table::rbindlist(x, use.names = TRUE)
  
  return(x)
}


.split_data_by_plot_facet <- function(x, plot_layout_table = NULL, ...) {
  if (is_empty(x)) return(NULL)
  
  if (is.null(plot_layout_table)) {
    plot_layout_table <- do.call(
      .get_plot_layout_table,
      args = c(
        list("x" = x),
        list(...)))
  }

  # Derive facet_by
  facet_by <- setdiff(
    colnames(plot_layout_table),
    c("col_id", "row_id"))

  if (length(facet_by > 0)) {
    # Merge the plot_layout_table into x. This will keep things in order. All
    # levels are kept.
    x <- merge(
      x = x,
      y = plot_layout_table,
      by = facet_by,
      all = TRUE)
    
  } else {
    x <- cbind(x, plot_layout_table)
  }

  # Split data by row, then column
  split_data <- split(
    x, 
    by = c("col_id", "row_id"), 
    sorted = TRUE)

  return(split_data)
}



.get_plot_layout_dims <- function(plot_layout_table = NULL, ...) {
  # Create the plot_layout_table if it is not provided.
  if (is.null(plot_layout_table)) {
    plot_layout_table <- do.call(
      .get_plot_layout_table, 
      args = list(...))
  }

  # Return (nrows, ncols)
  return(c(
    max(plot_layout_table$row_id),
    max(plot_layout_table$col_id)))
}



.get_plot_layout_table <- function(x, facet_by, facet_wrap_cols) {
  if (is.null(facet_by)) {
    # Simple 1x1 layout without facets.
    plot_layout_table <- data.table::data.table(
      "col_id" = 1L,
      "row_id" = 1L)
    
  } else if (is.null(facet_wrap_cols)) {
    # Generate a plot_layout_table and order it
    plot_layout_table <- expand.grid(
      lapply(
        facet_by,
        function(column, x) levels(x[[column]]),
        x = x),
      KEEP.OUT.ATTRS = FALSE)
    
    plot_layout_table <- data.table::as.data.table(plot_layout_table)
    data.table::setnames(plot_layout_table, facet_by)
    data.table::setorderv(x = plot_layout_table, cols = facet_by)

    # Find the number of columns
    n_cols <- length(unique(x[[facet_by[1]]]))

    # Add column id to the plot_layout_table
    plot_layout_table[, "col_id" := .GRP, by = get(facet_by[1])]

    if (length(facet_by) > 1) {
      # Find the number of rows
      n_levels <- sapply(
        facet_by[2:length(facet_by)],
        function(ii, x) {
          if (is.factor(x[[ii]])) {
            return(nlevels(x[[ii]]))
          } else {
            return(length(unique(x[[ii]])))
          }
        },
        x = x)
      n_rows <- prod(n_levels)

      # Add row id to the plot_layout_table
      facet_row_cols <- facet_by[2:length(facet_by)]
      plot_layout_table[, "row_id" := .GRP, by = mget(facet_row_cols)]
      
    } else {
      # There is only one row
      n_rows <- 1
      plot_layout_table[, "row_id" := 1L]
    }
  } else {
    # Generate a plot_layout_table, and order
    plot_layout_table <- unique(x[, (facet_by), with = FALSE], by = facet_by)
    data.table::setorderv(x = plot_layout_table, cols = facet_by)

    # Number of columns is provided using facet_wrap_cols.
    len_table <- nrow(plot_layout_table)
    n_cols <- facet_wrap_cols
    n_rows <- ceiling(len_table / n_cols)

    # Generate the column and row positions.
    col_ids <- rep(seq_len(n_cols), times = n_rows)[seq_len(len_table)]
    row_ids <- rep(seq_len(n_rows), each = n_cols)[seq_len(len_table)]

    # Add column and row ids to the plot_layout_table.
    plot_layout_table[, ":="(
      "col_id" = col_ids,
      "row_id" = row_ids)]
  }

  return(plot_layout_table)
}



.update_plot_layout_table <- function(
    plot_layout_table,
    grobs,
    x_text_shared = "overall",
    x_label_shared = "overall",
    y_text_shared = "overall",
    y_label_shared = "overall",
    facet_wrap_cols = NULL) {
  # Suppress NOTES due to non-standard evaluation in data.table
  col_id <- row_id <- is_present <- fraction_present <- NULL

  # Update the layout table by adding a figure id and determining if the grob
  # is present.
  plot_layout_table[, ":="(
    "figure_id" = .I,
    "is_present" = sapply(grobs, gtable::is.gtable))]
  
  # Drop panels in the plot.
  if (!is.null(facet_wrap_cols)) {
    # Keep only panels that are present.
    plot_layout_table <- plot_layout_table[is_present == TRUE]
  } else {
    # Drop rows and columns from the table that do not contain any data.
    empty_columns <- plot_layout_table[, list(
      fraction_present = sum(is_present) / .N),
      by = "col_id"]
    empty_columns <- empty_columns[fraction_present == 0.0]$col_id
    
    if (length(empty_columns) > 0) {
      plot_layout_table <- plot_layout_table[!col_id %in% empty_columns]
    }

    empty_rows <- plot_layout_table[, list(
      fraction_present = sum(is_present) / .N),
      by = "row_id"]
    empty_rows <- empty_rows[fraction_present == 0.0]$row_id
    
    if (length(empty_rows) > 0) {
      plot_layout_table <- plot_layout_table[!row_id %in% empty_rows]
    }
  }

  # Check that any part of the plot is remaining
  if (is_empty(plot_layout_table)) return(plot_layout_table)

  if (!is.null(facet_wrap_cols)) {
    # Number of columns is provided using facet_wrap_cols.
    len_table <- nrow(plot_layout_table)
    n_cols <- facet_wrap_cols
    n_rows <- ceiling(len_table / n_cols)

    # Generate the column and row positions.
    col_ids <- rep(seq_len(n_cols), times = n_rows)[seq_len(len_table)]
    row_ids <- rep(seq_len(n_rows), each = n_cols)[seq_len(len_table)]

    # Set default elements
    plot_layout_table[, ":="(
      "col_id" = col_ids,
      "row_id" = row_ids,
      "has_strip_x" = TRUE,
      "has_strip_y" = FALSE,
      "has_axis_text_x" = x_text_shared %in% c("individual", "FALSE"),
      "has_axis_text_y" = y_text_shared %in% c("individual", "FALSE"),
      "has_axis_label_x" = x_label_shared == "individual",
      "has_axis_label_y" = y_label_shared == "individual")]

    for (current_col_id in seq_len(n_cols)) {
      # Determine the bottom row.
      max_row_id <- max(plot_layout_table[col_id == current_col_id]$row_id)

      # Set x labels and text. Note that even when "overall" is set, axis text
      # should stick to the panels.
      if (x_text_shared %in% c("column", "overall", "TRUE")) {
        plot_layout_table[
          col_id == current_col_id & row_id == max_row_id,
          "has_axis_text_x" := TRUE]
      }
      if (x_label_shared == "column") {
        plot_layout_table[
          col_id == current_col_id & row_id == max_row_id,
          "has_axis_label_x" := TRUE]
      }
    }

    # Set y labels and text. Note that even when "overall" is set, axis text
    # should stick to the panels.
    if (y_text_shared %in% c("row", "overall", "TRUE")) {
      plot_layout_table[col_id == 1L, "has_axis_text_y" := TRUE]
    }
    if (y_label_shared == "row") {
      plot_layout_table[col_id == 1L, "has_axis_label_y" := TRUE]
    }
    
  } else {
    # Update the column and row ids.
    plot_layout_table[, "col_id" := .GRP, by = "col_id"]
    plot_layout_table[, "row_id" := .GRP, by = "row_id"]

    # Set default elements
    plot_layout_table[, ":="(
      "has_strip_x" = FALSE,
      "has_strip_y" = FALSE,
      "has_axis_text_x" = x_text_shared %in% c("individual", "FALSE"),
      "has_axis_text_y" = y_text_shared %in% c("individual", "FALSE"),
      "has_axis_label_x" = x_label_shared == "individual",
      "has_axis_label_y" = y_label_shared == "individual")]
    
    # Determine the number of rows and columns
    n_cols <- max(plot_layout_table$col_id)
    n_rows <- max(plot_layout_table$row_id)

    # Add strips
    if (n_rows > 1) plot_layout_table[col_id == n_cols, "has_strip_y" := TRUE]
    if (n_cols > 1) plot_layout_table[row_id == 1L, "has_strip_x" := TRUE]

    # Add axis text. Note that even when "overall" is set, axis text should
    # stick to the panels.
    if (x_text_shared %in% c("column", "overall", "TRUE")) {
      plot_layout_table[row_id == n_rows, "has_axis_text_x" := TRUE]
    }
    if (y_text_shared %in% c("row", "overall", "TRUE")) {
      plot_layout_table[col_id == 1L, "has_axis_text_y" := TRUE]
    }

    # Add axis labels
    if (x_label_shared == "column") {
      plot_layout_table[row_id == n_rows, "has_axis_label_x" := TRUE]
    }
    if (y_label_shared == "row") {
      plot_layout_table[col_id == 1L, "has_axis_label_y" := TRUE]
    }
  }

  return(plot_layout_table)
}



..get_plot_theme_linewidth <- function(ggtheme = NULL) {
  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)

  # Since ggplot 3.4.0, the width of a line is determined by linewidth instead
  # of size.
  if (utils::packageVersion("ggplot2") >= "3.4.0") {
    linewidth <- ggtheme$line$linewidth
  } else {
    linewidth <- ggtheme$line$size
  }

  return(linewidth)
}



..get_plot_element_spacing <- function(ggtheme = NULL, axis, theme_element) {
  # Obtain spacing from a ggtheme element

  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)

  # Get spacing for the specific axis, if present.
  spacing <- ggtheme[[paste0(theme_element, ".", axis)]]

  # Get spacing for the main element
  if (is.null(spacing)) spacing <- ggtheme[[theme_element]]

  # If no spacing is provided, produce 0.0 length spacing.
  if (!grid::is.unit(spacing)) spacing <- grid::unit(0.0, "pt")

  return(spacing)
}



.get_plot_panel_spacing <- function(ggtheme = NULL, axis) {
  # Obtain spacing between panels. This determines distance between facets.
  return(..get_plot_element_spacing(
    ggtheme = ggtheme,
    axis = axis,
    theme_element = "panel.spacing"))
}


.get_plot_legend_spacing <- function(ggtheme = NULL, axis) {
  # Obtain spacing between legend and the main panel.
  return(..get_plot_element_spacing(
    ggtheme = ggtheme,
    axis = axis,
    theme_element = "legend.box.spacing"))
}



plotting.get_geom_text_settings <- function(ggtheme = NULL) {
  # Import formatting settings from the provided ggtheme.

  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)

  # Find the text size for the table. This is based on text sizes in the
  # ggtheme.
  fontsize <- ggtheme$text$size
  fontsize_rel <- 1.0

  # Attempt to base the text size on the general axis.text attribute.
  if (!is.null(ggtheme$axis.text$size)) {
    if (inherits(ggtheme$axis.text$size, "rel")) {
      # Find the relative text size of axis text.
      fontsize_rel <- as.numeric(ggtheme$axis.text$size)
    } else {
      # Set absolute text size.
      fontsize <- ggtheme$axis.text$size
      fontsize_rel <- 1.0
    }
  }

  # Attempt to refine the text size using the axis.text.y attribute in
  # particular.
  if (!is.null(ggtheme$axis.text.y$size)) {
    if (inherits(ggtheme$axis.text.y$size, "rel")) {
      # Set relative text size of axis text
      fontsize_rel <- as.numeric(ggtheme$axis.text.y$size)
    } else {
      # Set absolute text size.
      fontsize <- as.numeric(ggtheme$axis.text.y$size)
      fontsize_rel <- 1.0
    }
  }

  # Update the text size using the magical ggplot2 point size (ggplot2:::.pt).
  geom_text_size <- fontsize * fontsize_rel / 2.845276

  # Obtain lineheight
  lineheight <- ggtheme$text$lineheight
  if (!is.null(ggtheme$axis.text$lineheight)) lineheight <- ggtheme$axis.text$lineheight
  if (!is.null(ggtheme$axis.text.y$lineheight)) lineheight <- ggtheme$axis.text.y$lineheight

  # Obtain family
  fontfamily <- ggtheme$text$family
  if (!is.null(ggtheme$axis.text$family)) fontfamily <- ggtheme$axis.text$family
  if (!is.null(ggtheme$axis.text.y$family)) fontfamily <- ggtheme$axis.text.y$family
  if (!is.null(ggtheme$axis.text.x$family)) fontfamily <- ggtheme$axis.text.x$family

  # Obtain face
  fontface <- ggtheme$text$face
  if (!is.null(ggtheme$axis.text$face)) fontface <- ggtheme$axis.text$face
  if (!is.null(ggtheme$axis.text.y$face)) fontface <- ggtheme$axis.text.y$face
  if (!is.null(ggtheme$axis.text.x$face)) fontface <- ggtheme$axis.text.x$face

  # Obtain colour
  colour <- ggtheme$text$colour
  if (!is.null(ggtheme$axis.text$colour)) colour <- ggtheme$axis.text$colour
  if (!is.null(ggtheme$axis.text.y$colour)) colour <- ggtheme$axis.text.y$colour
  if (!is.null(ggtheme$axis.text.x$colour)) colour <- ggtheme$axis.text.x$colour

  return(list(
    "geom_text_size" = geom_text_size,
    "fontsize" = fontsize,
    "fontsize_rel" = fontsize_rel,
    "colour" = colour,
    "family" = fontfamily,
    "face" = fontface,
    "lineheight" = lineheight))
}



plotting.compile_figure_data <- function(
    grobs,
    element_grobs,
    plot_layout_table,
    keep_axis_text_x = FALSE,
    keep_axis_text_y = FALSE,
    ggtheme = NULL) {
  # Suppress NOTES due to non-standard evaluation in data.table
  figure_id <- is_present <- col_id <- row_id <- NULL
  
  # Check whether the plot layout table is empty.
  if (is_empty(plot_layout_table)) {
    return(NULL)
  }

  # Create a placeholder list. This is done to prevent losing a connection
  # between figure id and the length of the list in case entire columns or rows
  # were removed from the plot_layout_table.
  figure_list <- replicate(n = max(plot_layout_table$figure_id), NULL)

  # Iterate over the plot layout table to compile all the data required to
  # create the sub-figures.

  for (ii in plot_layout_table$figure_id) {
    removed_axis_text_x <- removed_axis_text_y <- FALSE

    current_figure_list <- list()
    if (plot_layout_table[figure_id == ii]$is_present) {
      # Collect the main dataset.
      current_figure_list$main <- grobs[[ii]]

      # Collect additional plot data.
      if (plot_layout_table[figure_id == ii]$has_strip_x) {
        current_figure_list$strip_x <- element_grobs[[ii]]$strip_x
      }
      if (plot_layout_table[figure_id == ii]$has_strip_y) {
        current_figure_list$strip_y <- element_grobs[[ii]]$strip_y
      }

      # Collect labels for the x-axis
      if (plot_layout_table[figure_id == ii]$has_axis_label_x) {
        current_figure_list$axis_label_b <- element_grobs[[ii]]$axis_label_b
        current_figure_list$axis_label_t <- element_grobs[[ii]]$axis_label_t
      }

      # Collect labels for the y-axis.
      if (plot_layout_table[figure_id == ii]$has_axis_label_y) {
        current_figure_list$axis_label_l <- element_grobs[[ii]]$axis_label_l
        current_figure_list$axis_label_r <- element_grobs[[ii]]$axis_label_r
      }

      # Collect axis text data for the x-axis.
      if (plot_layout_table[figure_id == ii]$has_axis_text_x && !keep_axis_text_x) {
        current_figure_list$axis_text_b <- element_grobs[[ii]]$axis_text_b
        current_figure_list$axis_text_t <- element_grobs[[ii]]$axis_text_t
        
      } else if (!keep_axis_text_x) {
        removed_axis_text_x <- TRUE

        current_figure_list$axis_text_b <- element_grobs[[ii]]$axis_text_b_nt
        current_figure_list$axis_text_t <- element_grobs[[ii]]$axis_text_t_nt
      }

      # Collect axis text data for the y-axis.
      if (plot_layout_table[figure_id == ii]$has_axis_text_y && !keep_axis_text_y) {
        current_figure_list$axis_text_l <- element_grobs[[ii]]$axis_text_l
        current_figure_list$axis_text_r <- element_grobs[[ii]]$axis_text_r
        
      } else if (!keep_axis_text_y) {
        removed_axis_text_y <- TRUE

        current_figure_list$axis_text_l <- element_grobs[[ii]]$axis_text_l_nt
        current_figure_list$axis_text_r <- element_grobs[[ii]]$axis_text_r_nt
      }
      
    } else {
      # In this case the main plot data is not present.
      replacement_grob <- plotting.create_empty_grob(
        g = grobs[[plot_layout_table[is_present == TRUE]$figure_id[1]]],
        keep_implicit = TRUE
      )

      # Identify existing grobs from the same row and from the same column.
      current_row_id <- plot_layout_table[figure_id == ii]$row_id
      current_col_id <- plot_layout_table[figure_id == ii]$col_id
      same_row_figure_id <- plot_layout_table[is_present == TRUE & row_id == current_row_id]$figure_id[1]
      same_col_figure_id <- plot_layout_table[is_present == TRUE & col_id == current_col_id]$figure_id[1]

      # Set the replacement dataset as the main dataset.
      current_figure_list$main <- replacement_grob

      if (plot_layout_table[figure_id == ii]$has_strip_x) {
        current_figure_list$strip_x <- element_grobs[[same_col_figure_id]]$strip_x
      }
      if (plot_layout_table[figure_id == ii]$has_strip_y) {
        current_figure_list$strip_y <- element_grobs[[same_row_figure_id]]$strip_y
      }

      # Collect labels for the x-axis.
      if (plot_layout_table[figure_id == ii]$has_axis_label_x) {
        current_figure_list$axis_label_b <- element_grobs[[same_col_figure_id]]$axis_label_b
        current_figure_list$axis_label_t <- element_grobs[[same_col_figure_id]]$axis_label_t
      }

      # Collect labels for the y-axis
      if (plot_layout_table[figure_id == ii]$has_axis_label_y) {
        current_figure_list$axis_label_l <- element_grobs[[same_row_figure_id]]$axis_label_l
        current_figure_list$axis_label_r <- element_grobs[[same_row_figure_id]]$axis_label_r
      }
    }

    # Merge elements with the main element.
    g <- plotting.reinsert_plot_elements(
      grob_list = current_figure_list,
      ggtheme = ggtheme)

    if (removed_axis_text_x) {
      g <- plotting.update_axis_text_elements(
        g = g,
        type = "heights")
    }

    if (removed_axis_text_y) {
      g <- plotting.update_axis_text_elements(
        g = g,
        type = "widths")
    }

    # Add data to the figure list.
    figure_list[[ii]] <- g
  }

  return(figure_list)
}



plotting.add_global_plot_elements <- function(
    grobs,
    element_grobs,
    plot_layout_table,
    ggtheme) {
  # Suppress NOTES due to non-standard evaluation in data.table
  is_present <- NULL

  figure_list <- list()

  # Select a figure that is present.
  present_figure_id <- plot_layout_table[is_present == TRUE, ]$figure_id[1]

  if (!gtable::is.gtable(grobs)) {
    ..error_reached_unreachable_code(
      "plotting.compile_plot_wide_data: grob is not a gtable.")
  }
  
  # Add main grob
  figure_list$main <- grobs

  # Add guide.
  figure_list$guide <- element_grobs[[present_figure_id]]$guide

  # Determine if axis labels need to be added.
  if (all(plot_layout_table$has_axis_label_x == FALSE)) {
    figure_list$axis_label_b <- element_grobs[[present_figure_id]]$axis_label_b
    figure_list$axis_label_t <- element_grobs[[present_figure_id]]$axis_label_t
  }

  if (all(plot_layout_table$has_axis_label_y == FALSE)) {
    figure_list$axis_label_l <- element_grobs[[present_figure_id]]$axis_label_l
    figure_list$axis_label_r <- element_grobs[[present_figure_id]]$axis_label_r
  }

  # Add title, subtitle and caption.
  figure_list$title <- element_grobs[[present_figure_id]]$title
  figure_list$subtitle <- element_grobs[[present_figure_id]]$subtitle
  figure_list$caption <- element_grobs[[present_figure_id]]$caption

  # Insert global elements.
  g <- plotting.reinsert_plot_elements(
    grob_list = figure_list,
    ggtheme = ggtheme)

  return(g)
}



#' Feature arrangement
#'
#' @param grobs list of graphic objects (grobs)
#' @param plot_layout_table layout table
#' @param panel_elements elements that should be added to each panel
#' @param figure_elements elements that are added to the figure as a whole
#' @param element_grobs grobs of the elements
#' @param ggtheme ggtheme
#'
#' @return a single gtable
#'
#' @noRd
plotting.arrange_figures <- function(
    grobs,
    plot_layout_table,
    element_grobs,
    ggtheme) {
  # Suppress NOTES due to non-standard evaluation in data.table
  col_id <- row_id <- NULL

  figure_data <- plotting.compile_figure_data(
    grobs = grobs,
    element_grobs = element_grobs,
    plot_layout_table = plot_layout_table,
    ggtheme = ggtheme
  )

  if (is_empty(figure_data)) return(NULL)

  # Placeholder for final figure
  g <- NULL

  # Determine the number of rows and columns.
  n_rows <- max(plot_layout_table$row_id)
  n_cols <- max(plot_layout_table$col_id)

  # Iterate over rows and columns.
  for (ii in seq_len(n_rows)) {
    # Create a placeholder.
    g_current_row <- NULL

    # Populate the current row with figures.
    for (jj in seq_len(n_cols)) {
      selected_figure_id <- plot_layout_table[col_id == jj & row_id == ii]$figure_id

      # Check if the iterator exceeds the maximum number of available figures.
      if (length(selected_figure_id) == 0) break

      # Select the current grob.
      current_grob <- figure_data[[selected_figure_id]]

      if (is.null(g_current_row)) {
        # If the current row is still empty, copy the first figure.
        g_current_row <- current_grob
        
      } else {
        # If the current row is not empty, use cbind.gtable to combine figures
        # column-wise.

        # First insert a column that spaces the facets.
        g_current_row <- gtable::gtable_add_cols(
          g_current_row,
          widths = .get_plot_panel_spacing(ggtheme = ggtheme, axis = "x"))

        # Add the figure to the current figure.
        g_current_row <- cbind(g_current_row, current_grob)
      }
    }

    # Populate the figure.
    if (is.null(g)) {
      # If the figure is still empty, copy the current row.
      g <- g_current_row
      
    } else {
      # Use rbind.gtable to combine rows.
      g <- rbind(g, g_current_row)
    }

    # Check if the iterator exceeds the maximum number of available figures.
    if (length(selected_figure_id) == 0) break
  }

  # Identify data that should be re-inserted.
  g <- plotting.add_global_plot_elements(
    grobs = g,
    element_grobs = element_grobs,
    plot_layout_table = plot_layout_table,
    ggtheme = ggtheme)

  return(g)
}


plotting.rename_plot_elements <- function(g = g, extension = "main") {
  if (is.null(g)) return(g)

  # Main panel
  g <- .gtable_rename_element(
    g = g,
    old = "panel",
    new = paste0("panel-", extension),
    partial_match = TRUE, 
    allow_missing = TRUE)

  # Left axis text and label
  g <- .gtable_rename_element(
    g = g, 
    old = "axis-l",
    new = paste0("axis-l-", extension), 
    partial_match = TRUE, 
    allow_missing = TRUE)
  g <- .gtable_rename_element(
    g = g, 
    old = "ylab-l",
    new = paste0("ylab-l-", extension), 
    partial_match = TRUE, 
    allow_missing = TRUE)

  # Bottom axis text and label
  g <- .gtable_rename_element(
    g = g, 
    old = "axis-b", 
    new = paste0("axis-b-", extension), 
    partial_match = TRUE,
    allow_missing = TRUE)
  g <- .gtable_rename_element(
    g = g,
    old = "xlab-b", 
    new = paste0("xlab-b-", extension),
    partial_match = TRUE, 
    allow_missing = TRUE)

  # Right axis text and label
  g <- .gtable_rename_element(
    g = g, 
    old = "axis-r", 
    new = paste0("axis-r-", extension), 
    partial_match = TRUE, 
    allow_missing = TRUE)
  g <- .gtable_rename_element(
    g = g, 
    old = "ylab-r", 
    new = paste0("ylab-r-", extension), 
    partial_match = TRUE, 
    allow_missing = TRUE)
  
  # Top axis text and label
  g <- .gtable_rename_element(
    g = g,
    old = "axis-t",
    new = paste0("axis-t-", extension),
    partial_match = TRUE,
    allow_missing = TRUE)
  g <- .gtable_rename_element(
    g = g, 
    old = "xlab-t",
    new = paste0("xlab-t-", extension),
    partial_match = TRUE, 
    allow_missing = TRUE)

  return(g)
}



plotting.extract_plot_elements <- function(p) {
  element_list <- list()

  # Convert to grobs
  g <- plotting.to_grob(p)

  # Export list of elements.
  if (is.null(g)) return(element_list)

  # Update the names of the plot elements.
  g <- plotting.rename_plot_elements(
    g = g,
    extension = "main")

  # Legend
  element_list$guide <- .gtable_extract(
    g = g,
    element = "guide-box",
    drop_empty = TRUE)

  # Axis label
  element_list$axis_label_b <- .gtable_extract(
    g = g, 
    element = "xlab-b-main", 
    drop_empty = TRUE)
  element_list$axis_label_t <- .gtable_extract(
    g = g, 
    element = "xlab-t-main", 
    drop_empty = TRUE)
  element_list$axis_label_l <- .gtable_extract(
    g = g, 
    element = "ylab-l-main", 
    drop_empty = TRUE)
  element_list$axis_label_r <- .gtable_extract(
    g = g, 
    element = "ylab-r-main", 
    drop_empty = TRUE)

  # Strip x
  element_list$strip_x <- .gtable_extract(
    g = g, 
    element = "strip-t", 
    partial_match = TRUE, 
    drop_empty = TRUE)
  element_list$strip_y <- .gtable_extract(
    g = g, 
    element = "strip-r", 
    partial_match = TRUE, 
    drop_empty = TRUE)

  # Axis text (with text)
  element_list$axis_text_b <- .gtable_extract(
    g = g, 
    element = "axis-b-main", 
    partial_match = TRUE,
    drop_empty = TRUE)
  element_list$axis_text_t <- .gtable_extract(
    g = g, element = "axis-t-main",
    partial_match = TRUE,
    drop_empty = TRUE)
  element_list$axis_text_l <- .gtable_extract(
    g = g, 
    element = "axis-l-main",
    partial_match = TRUE,
    drop_empty = TRUE)
  element_list$axis_text_r <- .gtable_extract(
    g = g,
    element = "axis-r-main", 
    partial_match = TRUE, 
    drop_empty = TRUE)

  # Title, subtitle and caption
  element_list$title <- .gtable_extract(
    g = g,
    element = "title",
    partial_match = FALSE,
    drop_empty = TRUE)
  element_list$subtitle <- .gtable_extract(
    g = g, 
    element = "subtitle",
    partial_match = FALSE,
    drop_empty = TRUE)
  element_list$caption <- .gtable_extract(
    g = g,
    element = "caption",
    partial_match = FALSE,
    drop_empty = TRUE)

  # Update plot by removing the axis text, title, subtitle and captions.
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank())

  # Convert to grobs
  g <- plotting.to_grob(p)

  # Update the names of the plot elements.
  g <- plotting.rename_plot_elements(
    g = g,
    extension = "main")

  # Axis text (without text)
  element_list$axis_text_b_nt <- .gtable_extract(
    g = g, 
    element = "axis-b-main", 
    partial_match = TRUE, 
    drop_empty = TRUE)
  element_list$axis_text_t_nt <- .gtable_extract(
    g = g, 
    element = "axis-t-main", 
    partial_match = TRUE, 
    drop_empty = TRUE)
  element_list$axis_text_l_nt <- .gtable_extract(
    g = g, 
    element = "axis-l-main", 
    partial_match = TRUE, 
    drop_empty = TRUE)
  element_list$axis_text_r_nt <- .gtable_extract(
    g = g, 
    element = "axis-r-main", 
    partial_match = TRUE, 
    drop_empty = TRUE)

  # Title, subtitle, caption (without text)
  element_list$title_nt <- .gtable_extract(
    g = g,
    element = "title",
    partial_match = FALSE,
    drop_empty = TRUE)
  element_list$subtitle_nt <- .gtable_extract(
    g = g, 
    element = "subtitle",
    partial_match = FALSE, 
    drop_empty = TRUE)
  element_list$caption_nt <- .gtable_extract(
    g = g, 
    element = "caption", 
    partial_match = FALSE, 
    drop_empty = TRUE)

  return(element_list)
}


plotting.remove_plot_elements <- function(p) {
  # Remove elements that were extracted as a grob from plots.

  # Check whether p is a ggplot.
  if (!inherits(p, "ggplot")) return(p)

  # Remove all relevant elements
  p <- p + ggplot2::theme(
    strip.background.x = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    strip.background.y = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    legend.position = "none",
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank())

  return(p)
}



plotting.reinsert_plot_elements <- function(
    g = NULL, 
    elements = NULL, 
    grob_list, 
    ggtheme) {
  if (is.null(g)) {
    g <- grob_list$main
    elements <- names(grob_list)
  }

  if (is.null(g)) return(g)

  # Re-insert guides.
  if ("guide" %in% elements && !is.null(grob_list$guide)) {
    # Find legend position
    legend_position <- ggtheme$legend.position

    if (legend_position == "right") {
      # Align to right of the plot, and iterate inward to find valid reference elements.
      for (ref_element in c("strip-r", "ylab-r", "axis-r", "panel-main", "panel")) {
        if (.gtable_element_in_layout(
          g = g, 
          element = ref_element, 
          partial_match = TRUE)) {
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(
            g = g,
            g_new = grob_list$guide,
            ref_element = ref_element,
            along_element = "panel",
            spacer = list("l" = .get_plot_legend_spacing(ggtheme = ggtheme, axis = "y")),
            where = legend_position,
            partial_match_ref = TRUE,
            partial_match_along = TRUE,
            update_dimensions = FALSE)
          
          break
        }
      }
      
    } else if (legend_position == "left") {
      # Align to left of the plot, and iterate inward to find valid reference
      # elements.
      for (ref_element in c("strip-l", "ylab-l", "axis-l", "panel-main", "panel")) {
        if (.gtable_element_in_layout(
          g = g, 
          element = ref_element, 
          partial_match = TRUE)) {
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(
            g = g,
            g_new = grob_list$guide,
            ref_element = ref_element,
            along_element = "panel",
            spacer = list("r" = .get_plot_legend_spacing(ggtheme = ggtheme, axis = "y")),
            where = legend_position,
            partial_match_ref = TRUE,
            partial_match_along = TRUE,
            update_dimensions = FALSE)

          break
        }
      }
      
    } else if (legend_position == "bottom") {
      # Align to bottom of the plot, and iterate inward to find valid reference
      # elements.
      for (ref_element in c("strip-b", "xlab-b", "axis-b", "panel-main", "panel")) {
        if (.gtable_element_in_layout(
          g = g,
          element = ref_element, 
          partial_match = TRUE)) {
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(
            g = g,
            g_new = grob_list$guide,
            ref_element = ref_element,
            along_element = "panel",
            spacer = list("t" = .get_plot_legend_spacing(ggtheme = ggtheme, axis = "x")),
            where = legend_position,
            partial_match_ref = TRUE,
            partial_match_along = TRUE,
            update_dimensions = FALSE)

          break
        }
      }
      
    } else if (legend_position == "top") {
      # Align to top of the plot, and iterate inward to find valid reference
      # elements.
      for (ref_element in c("strip-t", "xlab-t", "axis-t", "panel-main", "panel")) {
        if (.gtable_element_in_layout(
          g = g, 
          element = ref_element, 
          partial_match = TRUE)) {
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(
            g = g,
            g_new = grob_list$guide,
            ref_element = ref_element,
            along_element = "panel",
            spacer = list("b" = .get_plot_legend_spacing(ggtheme = ggtheme, axis = "x")),
            where = legend_position,
            partial_match_ref = TRUE,
            partial_match_along = TRUE,
            update_dimensions = FALSE)

          break
        }
      }
    }
  }

  # Insert strip with facet text (for columns)
  if ("strip_x" %in% elements && !is.null(grob_list$strip_x)) {
    # Align top of the plot, and iterate inward to find valid reference elements.
    for (ref_element in c("xlab-t", "axis-t", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$strip_x,
          ref_element = ref_element,
          along_element = "panel",
          where = "top",
          partial_match_ref = TRUE,
          partial_match_along = TRUE)

        break
      }
    }
  }


  # Insert strip with facet text (for rows)
  if ("strip_y" %in% elements && !is.null(grob_list$strip_y)) {
    # Align to right of the plot, and iterate inward to find valid reference
    # elements.
    for (ref_element in c("ylab-r", "axis-r", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element,
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$strip_y,
          ref_element = ref_element,
          along_element = "panel",
          where = "right",
          partial_match_ref = TRUE,
          partial_match_along = TRUE)

        break
      }
    }
  }

  # Insert bottom x-axis text
  if ("axis_text_b" %in% elements && !is.null(grob_list$axis_text_b)) {
    # Align to bottom of the plot, and iterate inward to find valid reference
    # elements.
    for (ref_element in c("panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_text_b,
          ref_element = ref_element,
          along_element = "panel-main",
          where = "bottom",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  # Insert top x-axis text
  if ("axis_text_t" %in% elements && !is.null(grob_list$axis_text_t)) {
    # Align to bottom of the plot, and iterate inward to find valid reference
    # elements.
    for (ref_element in c("panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_text_t,
          ref_element = ref_element,
          along_element = "panel-main",
          where = "top",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  # Insert left y-axis text
  if ("axis_text_l" %in% elements && !is.null(grob_list$axis_text_l)) {
    # Align to bottom of the plot, and iterate inward to find valid reference
    # elements.
    for (ref_element in c("panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_text_l,
          ref_element = ref_element,
          along_element = "panel-main",
          where = "left",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  # Insert right y-axis text
  if ("axis_text_r" %in% elements && !is.null(grob_list$axis_text_r)) {
    # Align to bottom of the plot, and iterate inward to find valid reference
    # elements.
    for (ref_element in c("panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_text_r,
          ref_element = ref_element,
          along_element = "panel-main",
          where = "right",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  # Insert y-axis label to the left.
  if ("axis_label_l" %in% elements && !is.null(grob_list$axis_label_l)) {
    # Align to left of the plot, and iterate inward to find valid reference elements.
    for (ref_element in c("axis-l-main", "axis-l", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_label_l,
          ref_element = ref_element,
          along_element = "panel",
          where = "left",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }


  # Insert y-axis label to the right.
  if ("axis_label_r" %in% elements && !is.null(grob_list$axis_label_r)) {
    # Align to right of the plot, and iterate inward to find valid reference elements.
    for (ref_element in c("axis-r-main", "axis-r", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_label_r,
          ref_element = ref_element,
          along_element = "panel",
          where = "right",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)
        
        break
      }
    }
  }

  # Insert x-axis label to the bottom.
  if ("axis_label_b" %in% elements && !is.null(grob_list$axis_label_b)) {
    # Align to bottom of the plot, and iterate inward to find valid reference elements.
    for (ref_element in c("axis-b-main", "axis-b", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element,
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_label_b,
          ref_element = ref_element,
          along_element = "panel",
          where = "bottom",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  # Insert x-axis label to the top.
  if ("axis_label_t" %in% elements && !is.null(grob_list$axis_label_b)) {
    # Align to bottom of the plot, and iterate inward to find valid reference elements.
    for (ref_element in c("axis-t-main", "axis-t", "panel-main", "panel")) {
      if (.gtable_element_in_layout(
        g = g, 
        element = ref_element, 
        partial_match = TRUE)) {
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(
          g = g,
          g_new = grob_list$axis_label_t,
          ref_element = ref_element,
          along_element = "panel",
          where = "top",
          attempt_replace = TRUE,
          partial_match_ref = FALSE,
          partial_match_along = FALSE)

        break
      }
    }
  }

  if ("subtitle" %in% elements && !is.null(grob_list$subtitle)) {
    # Insert subtitle label to the top.
    g <- .gtable_insert_along(
      g = g,
      g_new = grob_list$subtitle,
      ref_element = "subtitle",
      where = "top",
      attempt_replace = TRUE,
      partial_match_ref = FALSE,
      partial_match_along = FALSE)
  }


  # Insert title label to the top.
  if ("title" %in% elements && !is.null(grob_list$title)) {
    g <- .gtable_insert_along(
      g = g,
      g_new = grob_list$title,
      ref_element = "title",
      where = "top",
      attempt_replace = TRUE,
      partial_match_ref = FALSE,
      partial_match_along = FALSE)
  }


  # Insert caption to the bottom.
  if ("caption" %in% elements && !is.null(grob_list$caption)) {
    g <- .gtable_insert_along(
      g = g,
      g_new = grob_list$caption,
      ref_element = "caption",
      where = "bottom",
      attempt_replace = TRUE,
      partial_match_ref = FALSE,
      partial_match_along = FALSE)
  }

  return(g)
}



plotting.replace_missing_grobs <- function(grobs) {
  # Check which grobs are not gtables.
  missing_grobs <- !sapply(grobs, inherits, "gtable")

  if (all(missing_grobs)) stop("All grobs were missing.")
  if (!any(missing_grobs)) return(grobs)

  # Select a replacement grob.
  replacement_grob <- grobs[[which(!missing_grobs)[1]]]

  # Replace all table elements by zeroGrobs.
  replacement_grob$grobs <- replicate(
    length(replacement_grob),
    ggplot2::zeroGrob(),
    simplify = FALSE)

  # Insert the empty replacement grob for missing elements.
  grobs[missing_grobs] <- replicate(
    sum(missing_grobs),
    replacement_grob,
    simplify = FALSE)
  
  return(grobs)
}



plotting.create_empty_grob <- function(g, keep_implicit = FALSE) {
  repl_grob <- g

  # Replace grobs by empty grobs.
  repl_grob$grobs <- replicate(
    length(g),
    ggplot2::zeroGrob(),
    simplify = FALSE)
  
  # Identify the panel grobs.
  if (keep_implicit) {
    for (grob_id in which(grepl(pattern = "panel", x = repl_grob$layout$name))) {
      # Determine the location of the panel.
      position <- as.list(repl_grob$layout[grob_id, c("t", "l", "b", "r")])

      if (position$t == position$b) {
        # Identify the height of the original panel.
        if (grid::is.unit(g$grobs[[grob_id]]$heights)) {
          grob_height <- g$grobs[[grob_id]]$heights
        } else if (grid::is.unit(g$grobs[[grob_id]]$height)) {
          grob_height <- g$grobs[[grob_id]]$height
        } else if (grid::is.unit(g$heights[position$t])) {
          grob_height <- g$heights[position$t]
        } else {
          grob_height <- grid::unit(1.0, "null")
        }

        if (as.numeric(grob_height) == 0) {
          grob_height <- grid::unit(1.0, "null")
        }

        # Set the hight of the new panel.
        repl_grob$grobs[[grob_id]]$height <- grob_height
      }

      if (position$l == position$r) {
        # Identify the height of the original panel.
        if (grid::is.unit(g$grobs[[grob_id]]$widths)) {
          grob_width <- g$grobs[[grob_id]]$widths
        } else if (grid::is.unit(g$grobs[[grob_id]]$width)) {
          grob_width <- g$grobs[[grob_id]]$width
        } else if (grid::is.unit(g$widths[position$l])) {
          grob_width <- g$widths[position$l]
        } else {
          grob_width <- grid::unit(1.0, "null")
        }

        if (as.numeric(grob_width) == 0) {
          grob_width <- grid::unit(1.0, "null")
        }

        # Set the hight of the new panel.
        repl_grob$grobs[[grob_id]]$width <- grob_width
      }
    }
  }

  return(repl_grob)
}



plotting.update_axis_text_elements <- function(g, type) {
  if (type == "widths") {
    elements_main <- c("axis-l-main", "axis-r-main")
    elements_side <- c("axis-l", "axis-r")
  } else if (type == "heights") {
    elements_main <- c("axis-t-main", "axis-b-main")
    elements_side <- c("axis-t", "axis-b")
  }

  for (grob_id in which(g$layout$name %in% elements_main)) {
    # Determine the location of the panel.
    position <- as.list(g$layout[grob_id, c("t", "l", "b", "r")])

    if (type == "widths" && position$l != position$r) next
    if (type == "heights" && position$b != position$t) next

    if (type == "widths" && 
        any(g$layout$name %in% elements_side & g$layout$l == position$l)) {
      g$grobs[[grob_id]]$widths <- grid::unit(1.0, "npc")
    }

    if (type == "heights" && 
        any(g$layout$name %in% elements_side & g$layout$t == position$t)) {
      g$grobs[[grob_id]]$heights <- grid::unit(1.0, "npc")
    }
  }

  return(g)
}



plotting.to_grob <- function(plots_or_grobs) {
  # Convert to list if the input is a single grob or
  unlist_grobs <- FALSE
  if (grid::is.grob(plots_or_grobs) || ggplot2::is.ggplot(plots_or_grobs)) {
    plots_or_grobs <- list(plots_or_grobs)

    # Set a flag so that we unlist the results after conversion.
    unlist_grobs <- TRUE
  }

  # Initialise list of grobs
  grobs <- list()

  for (p in plots_or_grobs) {
    if (inherits(p, "familiar_ggplot")) {
      # Convert to grob
      g <- suppressWarnings(tryCatch(
        ggplot2::ggplotGrob(p),
        error = identity))
      
      if (inherits(g, "error")) g <- NULL

      # Make changes to g according to p$custom_grob.
      if (!is.null(p$custom_grob)) {
        if (!is.null(p$custom_grob$heights)) {
          # Iterate over the elements that need to be updated.
          for (ii in seq_len(length(p$custom_grob$heights$name))) {
            # Extract name and height
            name <- p$custom_grob$heights$name[ii]
            height <- p$custom_grob$heights$height[ii]

            # Find the intended grob.
            grob_index <- which(g$layout$name == name)

            # Update the height in the layout table.
            g$heights[g$layout[grob_index, "t"]] <- height

            # Update (or set) the height of the grob.
            g$grobs[[grob_index]]$heights <- height
          }
        }

        if (!is.null(p$custom_grob$widths)) {
          # Iterate over the elements that need to be updated.
          for (ii in seq_len(length(p$custom_grob$widths$name))) {
            name <- p$custom_grob$widths$name[ii]
            width <- p$custom_grob$widths$width[ii]

            # Find the intended grob.
            grob_index <- which(g$layout$name == name)

            # Update the height in the layout table.
            g$widths[g$layout[grob_index, "l"]] <- width

            # Update (or set) the height of the grob.
            g$grobs[[grob_index]]$widths <- width
          }
        }
      }
    } else if (inherits(p, "ggplot")) {
      # Convert to grob
      g <- suppressWarnings(tryCatch(
        ggplot2::ggplotGrob(p),
        error = identity))
      
      if (inherits(g, "error")) g <- NULL
      
    } else if (inherits(p, "grob")) {
      # Assign grob
      g <- p
      
    } else {
      warning(paste0(
        "Could not convert an object of class ", class(p), " to a grob."))
      g <- NULL
    }

    grobs <- c(grobs, list(g))
  }

  if (unlist_grobs) grobs <- grobs[[1]]

  return(grobs)
}


plotting.create_legend_label <- function(
    user_label, 
    color_by = NULL,
    linetype_by = NULL, 
    combine_legend = FALSE) {
  # Sent for inspection
  .check_input_plot_args(
    legend_label = user_label,
    combine_legend = combine_legend)

  if (is.null(color_by) && is.null(linetype_by)) {
    # No splitting variables are used
    return(list(
      "guide_color" = NULL,
      "guide_linetype" = NULL))
  }

  # Collect required list entries
  req_entries <- character(0)
  if (!is.null(color_by)) {
    req_entries <- c(req_entries, "guide_color")
  }
  if (!is.null(linetype_by)) {
    req_entries <- c(req_entries, "guide_linetype")
  }

  # Waiver: no user input
  if (is.waive(user_label)) {
    if (combine_legend) {
      legend_label <- gsub(
        x = paste0(unique(c(color_by, linetype_by)), collapse = " & "),
        pattern = "_",
        replacement = " ",
        fixed = TRUE)

      return(list(
        "guide_color" = legend_label,
        "guide_linetype" = legend_label))
      
    } else {
      # Colour labels
      if (!is.null(color_by)) {
        color_guide_label <- gsub(
          x = paste0(color_by, collapse = " & "),
          pattern = "_",
          replacement = " ",
          fixed = TRUE)
        
      } else {
        color_guide_label <- NULL
      }

      # Linetype labels
      if (!is.null(linetype_by)) {
        linetype_guide_label <- gsub(
          x = paste0(linetype_by, collapse = " & "),
          pattern = "_",
          replacement = " ",
          fixed = TRUE)
        
      } else {
        linetype_guide_label <- NULL
      }

      return(list(
        "guide_color" = color_guide_label,
        "guide_linetype" = linetype_guide_label))
    }
  } else if (is.null(user_label)) {
    # NULL input

    return(list(
      "guide_color" = NULL,
      "guide_linetype" = NULL))
    
  } else if (is.list(user_label)) {
    # List input

    # Check entries for existence
    for (current_entry in req_entries) {
      if (!current_entry %in% names(user_label)) {
        stop(paste0(
          "A legend name is missing for ", current_entry, 
          ". Please set this name to a \"", current_entry, 
          "\" list element, e.g. list(\"",  current_entry, 
          "\"=\"some name\", ...)."))
      }
    }

    # Select required entries
    user_label <- user_label[names(user_label) %in% req_entries]

    # Check that all entries are the same
    if (combine_legend && length(req_entries) >= 2) {
      if (!all(sapply(
        user_label[2:length(user_label)],
        identical,
        user_label[[1]]))) {
        stop(paste0(
          "Not all provided legend names are identical, but identical legend ",
          "names are required for combining the legend."))
      }
    }

    return(user_label)
    
  } else if (length(req_entries) >= 2 && !combine_legend) {
    # Single input where multiple is required

    stop(paste0(
      "Multiple legend names are required, but only one is provided. ",
      "Please return a list with ",
      paste0("\"", req_entries, "\"", collapse = ", "), " elements."))
    
  } else {
    # Single input

    return(list(
      "guide_color" = user_label, 
      "guide_linetype" = user_label))
  }
}



plotting.create_guide_table <- function(
    x, 
    color_by = NULL, 
    linetype_by = NULL, 
    discrete_palette = NULL, 
    combine_legend = TRUE) {

  .get_guide_tables <- function(x, color_by, linetype_by, discrete_palette) {
    # Suppress NOTES due to non-standard evaluation in data.table
    color_id <- linetype_id <- NULL

    # Select unique variables
    unique_vars <- unique(c(color_by, linetype_by))

    # Check whether there are any unique splitting variables
    if (is.null(unique_vars)) return(NULL)

    # Generate a guide table
    guide_table <- data.table::data.table(expand.grid(lapply(
      rev(unique_vars), 
      function(ii, x) (levels(x[[ii]])), 
      x = x)))

    # Rename variables
    data.table::setnames(x = guide_table, rev(unique_vars))

    # Convert to factors
    for (ii in unique_vars) {
      guide_table[[ii]] <- factor(
        x = guide_table[[ii]],
        levels = levels(x[[ii]]))
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

    # Extend guide table
    if (!is.null(color_by)) {
      # Generate breaks
      guide_table$color_breaks <- factor(
        x = breaks,
        levels = breaks)

      # Define colour groups
      guide_table[, "color_id" := .GRP, by = color_by]

      # Get the palette to use.
      discr_palette <- .get_palette(
        x = discrete_palette,
        n = max(guide_table$color_id),
        palette_type = "qualitative")

      # Assign colour values
      guide_table[, "color_values" := discr_palette[color_id]]
    }

    if (!is.null(linetype_by)) {
      # Generate breaks
      guide_table$linetype_breaks <- factor(
        x = breaks,
        levels = breaks)

      # Define linetype groups
      guide_table[, "linetype_id" := .GRP, by = linetype_by]

      # Get the palette to use
      line_palette <- scales::linetype_pal()(max(guide_table$linetype_id))

      # Assign linetypes
      guide_table[, "linetype_values" := line_palette[linetype_id]]
    }

    return(guide_table)
  }

  if (is_empty(x)) return(list("data" = x))

  # Extract guide tables
  if (combine_legend) {
    guide_list <- list(
      "guide_color" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette),
      "guide_linetype" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette))
    
  } else {
    guide_list <- list(
      "guide_color" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = NULL, 
        discrete_palette = discrete_palette),
      "guide_linetype" = .get_guide_tables(
        x = x, 
        color_by = NULL, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette))
  }

  # Filter out lists corresponding to missing split variables
  guide_list <- guide_list[!sapply(list(color_by, linetype_by), is.null)]

  if (length(guide_list) == 0) return(list("data" = x))

  # Initialise return list
  return_list <- list()

  # Add break column of the remaining lists to x
  for (guide_type in names(guide_list)) {
    if (guide_type == "guide_color") {
      # Add color_breaks to x
      if (combine_legend) {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(unique(c(color_by, linetype_by)), "color_breaks"))],
          by = unique(c(color_by, linetype_by)),
          all.x = TRUE,
          all.y = FALSE)
        
      } else {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(color_by, "color_breaks"))],
          by = color_by,
          all.x = TRUE, 
          all.y = FALSE)
      }

      # Return guide_color
      return_list[[guide_type]] <- guide_list[[guide_type]]
      
    } else if (guide_type == "guide_linetype") {
      # Add linetype_breaks to x
      if (combine_legend) {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(unique(c(color_by, linetype_by)), "linetype_breaks"))],
          by = unique(c(color_by, linetype_by)),
          all.x = TRUE, 
          all.y = FALSE)
        
      } else {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(linetype_by, "linetype_breaks"))],
          by = linetype_by,
          all.x = TRUE, 
          all.y = FALSE)
      }

      # Return guide_linetype
      return_list[[guide_type]] <- guide_list[[guide_type]]
    }
  }

  # Add updated data
  return_list$data <- x

  return(return_list)
}


plotting.draw <- function(plot_or_grob) {
  if (ggplot2::is.ggplot(plot_or_grob)) {
    show(plot_or_grob)
    
  } else if (grid::is.grob(plot_or_grob)) {
    grid::grid.newpage()
    grid::grid.draw(plot_or_grob)
    
  } else {
    stop("Plot could not be drawn.")
  }
  
  return(invisible(NULL))
}



plotting.save_plot_to_file <- function(
    plot_obj,
    object,
    dir_path,
    type,
    x,
    subtype = NULL,
    split_by = NULL,
    additional = NULL,
    filename = NULL,
    device = "png",
    ...) {
  # ... are passed to ggplot2::ggsave

  # Check if the plot object exists
  if (is.null(plot_obj)) return(NULL)

  # Check if directory exists
  if (is.encapsulated_path(dir_path)) {
    file_dir <- normalizePath(
      file.path(dir_path, object@name, type),
      mustWork = FALSE)
    
  } else {
    file_dir <- normalizePath(dir_path, mustWork = FALSE)
  }

  if (!dir.exists(file_dir)) {
    dir.create(file_dir, recursive = TRUE)
  }

  if (!is.null(filename)) {
    # These are the file extensions supported by ggsave (v3.4.0).
    file_extensions <- c(
      "eps", "ps", "tex", "pdf", "svg", "emf", "wmf",
      "png", "jpg", "jpeg", "bmp", "tiff"
    )

    # Test if a file extension is present.
    device_present <- endswith_any(
      filename, 
      suffix = paste0(".", file_extensions))
    
    if (any(device_present)) {
      # Update device indicated by the filename.
      device <- head(file_extensions[device_present], n = 1)

      # Remove device from filename.
      filename <- sub_last(
        pattern = paste0(".", device),
        replacement = "",
        x = filename)
    }

    # Extend the filename if multiple plots are created from the same data.
    if (!is.null(split_by)) {
      subtype <- paste0(
        as.character(sapply(split_by, function(jj, x) (x[[jj]][1]), x = x)),
        collapse = "_")

      filename <- paste0(filename, subtype, collapse = "_")
    }
    
  } else {
    # Set subtype.
    subtype <- .create_plot_subtype(
      x = x,
      subtype = subtype,
      split_by = split_by,
      additional = additional)

    # Combine type and subtype as the filename.
    filename <- paste0(
      type,
      ifelse(is.null(subtype), "", paste0("_", subtype)))
  }

  for (current_device in device) {
    # Add in extension again.
    filename <- paste0(
      filename,
      ".",
      current_device)

    # There may be an issue with a cold RStudio where the plotting devices have
    # not started.
    tryCatch(
      
      # Call ggsave to save the data
      suppressMessages(
        do.call(
          ggplot2::ggsave,
          args = c(
            list(
              "filename" = filename,
              "plot" = plot_obj,
              "device" = current_device,
              "path" = file_dir),
            list(...)))),
      error = function(err) {
        logger_warning(
          paste0(
            "Could not create plot ",
            filename,
            ". The OS may not allow long file names."))
      })
  }
  
  return(invisible(NULL))
}



plotting.get_output <- function(
    dir_path = NULL,
    plot_list = NULL,
    export_collection = FALSE,
    object = NULL) {
  
  # Do not return plot information.
  if (!is.null(dir_path)) plot_list <- NULL

  if (export_collection) {
    return(list(
      "collection" = object,
      "plot_list" = plot_list))
    
  } else {
    return(plot_list)
  }
}



plotting.format_number <- function(x, digits = 3) {
  # Find the base-10 integer of the data.
  x_base <- floor(log10(abs(x)))
  x_base <- x_base[is.finite(x_base)]

  # Determine the largest base.
  common_base <- ifelse(length(x_base) > 0, max(x_base), 0)

  # Round numbers.
  x <- round(x / 10^(1 + common_base - digits)) * 10^(1 + common_base - digits)

  # Format output.
  return(format(x, digits = digits, trim = TRUE))
}



plotting.nice_range <- function(input_range, x) {
  # Shrink input range to first and last value
  input_range <- c(
    head(input_range, n = 1),
    tail(input_range, n = 1))

  # Find values in input_range that should be replaced.
  replace_index <- is.na(input_range)

  # Return input range if no updating is required.
  if (!any(replace_index)) return(input_range)

  # Find range of values in x.
  value_range <- range(x)

  # Replace missing elements of the input range.
  input_range[replace_index] <- value_range[replace_index]

  # Make the input range nice
  nice_range <- range(labeling::extended(
    dmin = input_range[1],
    dmax = input_range[2],
    m = 5,
    only.loose = TRUE))

  # Update the input_range with nice values
  input_range[replace_index] <- nice_range[replace_index]

  return(input_range)
}



plotting.dendrogram_as_table <- function(h, similarity_metric) {
  # Suppress NOTES due to non-standard evaluation in data.table
  x_1 <- y_1 <- x_2 <- NULL

  # Convert to dendrogram
  h <- stats::as.dendrogram(h)

  # Determine the metric range.
  metric_range <- get_similarity_range(
    similarity_metric = similarity_metric, 
    as_distance = TRUE)

  # Convert dendogram to a list of connectors that can later be used for
  # plotting. Note that we do not know where the origin should be located on the
  # x-axis. We will correct for that later.
  connectors <- .decompose_dendrogram(
    h = h,
    parent_height = max(metric_range))

  # Combine into single data.table.
  connectors <- data.table::rbindlist(connectors)

  # Keep only nodes with finite parent height (y_1). Depending on the metric,
  # this means that the connector with the origin may not be drawn.
  connectors <- connectors[is.finite(y_1)]

  # Return null if there are no connectors to be drawn.
  if (is_empty(connectors)) return(NULL)

  # Reposition the left-most leaf to 0.0.
  min_leaf_pos <- min(c(connectors$x_1, connectors$x_2))
  connectors[, ":="(
    "x_1" = x_1 - min_leaf_pos,
    "x_2" = x_2 - min_leaf_pos)]
  
  return(connectors)
}


.decompose_dendrogram <- function(
    h, 
    parent_height = Inf, 
    parent_x = NA, 
    leafs_visited = 0) {
  # Decompose dendogram. The function is designed to iterate through a
  # dendogram, and obtain the connector between node (h) and its parent, as well
  # as the connectors between the node and its children h[[1]] and h[[2]],
  # unless it has no children.

  dend_attr <- attributes(h)

  if (is.na(parent_x)) {
    parent_x <- ifelse(is.null(dend_attr$midpoint), 0.0, dend_attr$midpoint)
  }

  if (is.null(dend_attr$midpoint)) {
    # This indicates that the node has no children.

    # Connector from parent.
    conn_parent_child <- data.table::data.table(
      "x_1" = parent_x,
      "y_1" = parent_height,
      "x_2" = parent_x,
      "y_2" = dend_attr$height,
      "feature" = dend_attr$label)

    return(list(conn_parent_child))
  }

  # Connector from parent.
  conn_parent_child <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = parent_height,
    "x_2" = parent_x,
    "y_2" = dend_attr$height,
    "feature" = NA_character_)
  
  # Left child node x-axis location
  if (!is.null(attributes(h[[1]])$midpoint)) {
    child_l_pos <- leafs_visited + attributes(h[[1]])$midpoint
  } else {
    child_l_pos <- leafs_visited
  }

  # Connector to left leaf.
  conn_child_l_leaf <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = dend_attr$height,
    "x_2" = child_l_pos,
    "y_2" = dend_attr$height,
    "feature" = NA_character_)

  # Right child node x-axis location
  if (!is.null(attributes(h[[2]])$midpoint) &&
      !is.null(attributes(h[[1]])$members)) {
    child_r_pos <- leafs_visited + attributes(h[[1]])$members + attributes(h[[2]])$midpoint
  } else if (!is.null(attributes(h[[1]])$members)) {
    child_r_pos <- leafs_visited + attributes(h[[1]])$members
  } else {
    child_r_pos <- leafs_visited
  }

  # Connector to right leaf.
  conn_child_r_leaf <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = dend_attr$height,
    "x_2" = child_r_pos,
    "y_2" = dend_attr$height,
    "feature" = NA_character_)

  # Add data.tables as list elements.
  connector_list <- list(
    conn_parent_child, conn_child_l_leaf, conn_child_r_leaf)

  # Left leaf
  if (!is.null(h[[1]])) {
    left_leaf_connectors <- .decompose_dendrogram(
      h = h[[1]],
      parent_height = dend_attr$height,
      parent_x = child_l_pos,
      leafs_visited = leafs_visited)

    # Append to list
    connector_list <- append(connector_list, left_leaf_connectors)
  }

  # Right leaf
  if (!is.null(h[[2]])) {
    right_leaf_connectors <- .decompose_dendrogram(
      h = h[[2]],
      parent_height = dend_attr$height,
      parent_x = child_r_pos,
      leafs_visited = ifelse(
        !is.null(attributes(h[[1]])$members),
        leafs_visited + attributes(h[[1]])$members, 
        leafs_visited)
    )

    # Append to list
    connector_list <- append(connector_list, right_leaf_connectors)
  }

  return(connector_list)
}



plotting.combine_guides <- function(g, ggtheme, no_empty = TRUE) {
  # Find how tables should be organised.
  guide_position <- ggtheme$legend.position

  # Check if the guide position can be interpreted
  if (!all(guide_position %in% c("none", "left", "right", "bottom", "top"))) {
    stop(paste0(
      "plotting.combine_guides: Guide position (legend.position in the ggplot2 ",
      "theme) is expect to be one of none, left, right, bottom, top."))
  }

  if (guide_position == "none") return(NULL)

  # If necessary, check that all guides are present as a gtable.
  if (no_empty) {
    if (!all(sapply(g, gtable::is.gtable))) {
      stop(paste0(
        "plotting.combine_guides: One of the guides in the g argument ",
        "is not a gtable object."))
    }
  }

  # Check if all guides are missing.
  if (!any(sapply(g, gtable::is.gtable))) return(NULL)

  # Keep only guides that are gtables.
  g <- g[sapply(g, gtable::is.gtable)]

  # Find widths and heights
  widths <- lapply(g, gtable::gtable_width)
  widths <- do.call(grid::unit.c, widths)

  heights <- lapply(g, gtable::gtable_height)
  heights <- do.call(grid::unit.c, heights)

  if (guide_position %in% c("left", "right", "none")) {
    # Concatenate the widths.
    widths <- max(widths)

    # Provide the matrix to order the guides.
    order_matrix <- matrix(
      data = seq_along(g),
      nrow = length(g), 
      ncol = 1)

    # Create a grob matrix
    g_matrix <- matrix(
      data = g, 
      ncol = 1)
    
  } else {
    # Concatenate the heights.
    heights <- max(heights)

    # Provide the matrix to order the guides.
    order_matrix <- matrix(
      data = seq_along(g), 
      nrow = 1, 
      ncol = length(g))

    # Create a grob matrix
    g_matrix <- matrix(
      data = g, 
      nrow = 1)
  }

  # Create a gtable that combines all guide-boxes.
  g <- gtable::gtable_matrix(
    name = "guide-box",
    grobs = g_matrix,
    widths = widths,
    heights = heights,
    z = order_matrix,
    respect = TRUE,
    clip = "inherit")

  # Wrap the combined guides into a single grob.
  g <- gtable::gtable_matrix(
    name = "guide-box",
    grobs = matrix(list(g), nrow = 1, ncol = 1),
    widths = sum(widths),
    heights = sum(heights),
    respect = TRUE,
    clip = "inherit")

  return(g)
}



..set_edge_points <- function(x, range, type) {
  # Function used to determine edge points, such as used for ggplot2::geom_rect.
  if (!is.numeric(x)) {
    x <- as.numeric(x)
    range <- c(0.5, length(x) + 0.5)
  }

  if (length(x) > 1) {
    # Make sure x is sorted ascendingly.
    sort_index <- sort(x, index.return = TRUE)$ix
    x <- x[sort_index]

    # Compute difference between subsequent values.
    diff_x <- diff(x)

    # Compute edges.
    xmax <- c(
      head(x, n = length(x) - 1L) + diff_x / 2.0,
      tail(x, n = 1L) + tail(diff_x, n = 1L) / 2.0
    )
    xmin <- c(
      head(x, n = 1L) - head(diff_x, n = 1L) / 2.0,
      tail(x, n = length(x) - 1L) - diff_x / 2.0
    )

    # Shuffle back to input order.
    xmax[sort_index] <- xmax
    xmin[sort_index] <- xmin
    
  } else {
    xmin <- range[1]
    xmax <- range[2]
  }

  edge_points <- list(xmin, xmax)
  if (type == "x") {
    names(edge_points) <- c("xmin", "xmax")
  } else if (type == "y") {
    names(edge_points) <- c("ymin", "ymax")
  } else {
    ..error_reached_unreachable_code(paste0(
      "..set_edge_points: unknown type specified: ", type))
  }

  return(edge_points)
}
