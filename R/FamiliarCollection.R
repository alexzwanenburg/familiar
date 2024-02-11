#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL





# set_data_set_names -----------------------------------------------------------

#' @title Name datasets for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by setting data labels.
#'
#' @details Labels convert internal naming of data sets to the requested label
#'   at export or when plotting. Currently assigned labels can be found using
#'   the \code{get_data_set_names} method.
#'
#' @param x A familiarCollection object.
#' @param old (optional) Set of old labels to replace.
#' @param new Set of replacement labels. The number of replacement labels should
#'   be equal to the number of provided old labels or the full number of labels.
#'   If a subset of labels is to be replaced, both \code{old} and \code{new}
#'   should be provided.
#' @param order (optional) Ordered set of replacement labels. This is used to
#'   provide the order in which the labels should be placed, which affects e.g.
#'   levels in a plot. If the ordering is not explicitly provided, the old
#'   ordering is used.
#'
#' @return A familiarCollection object with custom names for the data sets.
#' @export
#' @aliases set_data_set_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_data_set_names}} for obtaining
#' currently assigned labels.
#' @md
setMethod(
  "set_data_set_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL,
    order = NULL) {
    
    x <- .set_labels(
      x = x,
      old_label = old,
      new_label = new,
      new_order = order,
      upd_slot = "data_set_labels")
    
    return(x)
  }
)



# set_learner_names ------------------------------------------------------------

#' @title Rename learners for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by providing names for the learners.
#'
#' @details Labels convert the internal naming for learners to the requested
#'   label at export or when plotting. This enables the use of more specific
#'   naming, e.g. changing \code{random_forest_rfsrc} to \code{Random Forest}.
#'   Currently assigned labels can be found using the \code{get_learner_names}
#'   method.
#'
#' @inheritParams set_data_set_names,familiarCollection-method
#' @return A familiarCollection object with custom labels for the learners.
#' @export
#' @aliases set_learner_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_learner_names}} for obtaining
#' currently assigned labels.
#' @md
setMethod(
  "set_learner_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL,
    order = NULL) {
    
    x <- .set_labels(
      x = x,
      old_label = old, 
      new_label = new,
      new_order = order,
      upd_slot = "learner_labels")
    
    return(x)
  }
)



# set_fs_method_names ----------------------------------------------------------

#' @title Rename feature selection methods for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by providing names for the feature selection
#'   methods.
#'
#' @details Labels convert the internal naming for feature selection methods to
#'   the requested label at export or when plotting. This enables the use of
#'   more specific naming, e.g. changing \code{mim} to \code{Mutual Information
#'   Maximisation}. Currently assigned labels can be found using the
#'   \code{get_fs_method_names} method.
#'
#' @inheritParams set_data_set_names,familiarCollection-method
#' @return A familiarCollection object with updated labels.
#' @export
#' @aliases set_fs_method_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_fs_method_names}} for obtaining
#' currently assigned labels.
#' @md
setMethod(
  "set_fs_method_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL, 
    order = NULL) {
    
    x  <- .set_labels(
      x = x,
      old_label = old,
      new_label = new,
      new_order = order,
      upd_slot = "fs_method_labels")
    
    return(x)
  }
)



# set_feature_names ------------------------------------------------------------

#' @title Rename features for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by providing names for features.
#'
#' @details Labels convert the internal naming for features to the requested
#'   label at export or when plotting. This enables customisation without
#'   redoing the analysis with renamed input data. Currently assigned labels can
#'   be found using the \code{get_feature_names} method.
#'
#' @inheritParams set_data_set_names,familiarCollection-method
#' @return A familiarCollection object with updated labels.
#' @export
#' @aliases set_feature_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_feature_names}} for obtaining
#' currently assigned feature names.
#' @md
setMethod(
  "set_feature_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL,
    order = NULL) {
    
    x <- .set_labels(
      x = x,
      old_label = old,
      new_label = new,
      new_order = order,
      upd_slot = "feature_labels")
    
    return(x)
  }
)



# set_risk_group_names ---------------------------------------------------------

#' @title Rename risk groups for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by providing names for risk groups in survival
#'   analysis.
#'
#' @details Labels convert the internal naming for risk groups to the requested
#'   label at export or when plotting. This enables customisation of risk group
#'   names. Currently assigned labels can be found using the
#'   \code{get_risk_group_names} method.
#'
#' @inheritParams set_data_set_names,familiarCollection-method
#' @return A familiarCollection object with updated labels.
#' @export
#' @aliases set_risk_group_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_risk_group_names}} for obtaining
#' currently assigned risk group labels.
#' @md
setMethod(
  "set_risk_group_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL,
    order = NULL) {
    
    x <- .set_labels(
      x = x,
      old_label = old,
      new_label = new,
      new_order = order,
      upd_slot = "km_group_labels")
    
    return(x)
  }
)



# set_class_names --------------------------------------------------------------

#' @title Rename outcome classes for plotting and export
#'
#' @description Tabular exports and figures created from a familiarCollection
#'   object can be customised by providing names for outcome classes.
#'
#' @details Labels convert the internal naming for class levels to the requested
#'   label at export or when plotting. This enables customisation of class
#'   names. Currently assigned labels can be found using the
#'   \code{get_class_names} method.
#'
#' @inheritParams set_data_set_names,familiarCollection-method
#' @return A familiarCollection object with updated labels.
#' @export
#' @aliases set_class_names
#' @seealso * \linkS4class{familiarCollection} for information concerning the
#' familiarCollection class. * \code{\link{get_class_names}} for obtaining
#' currently assigned class names.
#' @md
setMethod(
  "set_class_names",
  signature(x = "familiarCollection"),
  function(
    x,
    old = NULL,
    new = NULL,
    order = NULL) {
    
    x <- .set_labels(
      x = x,
      old_label = old,
      new_label = new,
      new_order = order,
      upd_slot = "class_labels")
    
    return(x)
  }
)



# get_data_set_names -----------------------------------------------------------

#' @title Get current name of datasets
#'
#' @description Datasets in familiarCollection objects can have custom names for
#'   export and plotting. This function retrieves the currently assigned names.
#'
#' @details Labels convert internal naming of data sets to the requested label
#'   at export or when plotting. Labels can be changed using the
#'   \code{set_data_set_names} method.
#'
#' @param x A familiarCollection object.
#'
#' @return An ordered array of dataset name labels.
#' @export
#' @aliases get_data_set_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_data_set_names}} for updating the name of datasets and their ordering.
#' @md
setMethod(
  "get_data_set_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "data_set_labels",
      get_levels = FALSE))
  }
)



# get_learner_names ------------------------------------------------------------

#' @title Get current learner name labels
#'
#' @description Learners in familiarCollection objects can have custom names for
#'   export and plotting. This function retrieves the currently assigned names.
#'
#' @details Labels convert internal naming of learners to the requested label at
#'   export or when plotting. Labels can be changed using the
#'   \code{set_learner_names} method.
#'
#' @inheritParams get_data_set_names,familiarCollection-method
#'
#' @return An ordered array of learner name labels.
#' @export
#' @aliases get_learner_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_learner_names}} for updating the name of learners and their ordering.
#' @md
setMethod(
  "get_learner_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "learner_labels",
      get_levels = FALSE))
  }
)



# get_fs_method_names ----------------------------------------------------------

#' @title Get current feature selection method name labels
#'
#' @description Feature selection methods in familiarCollection objects can have
#'   custom names for export and plotting. This function retrieves the currently
#'   assigned names.
#'
#' @details Labels convert internal naming of feature selection methods to the
#'   requested label at export or when plotting. Labels can be changed using the
#'   \code{set_fs_method_names} method.
#'
#' @inheritParams get_data_set_names,familiarCollection-method
#'
#' @return An ordered array of feature selection method name labels.
#' @export
#' @aliases get_fs_method_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_fs_method_names}} for updating the name of feature selection methods and their ordering.
#' @md
setMethod(
  "get_fs_method_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "fs_method_labels",
      get_levels = FALSE))
  }
)



# get_feature_names ------------------------------------------------------------

#' @title Get current feature labels
#'
#' @description Features in familiarCollection objects can have custom names for
#'   export and plotting. This function retrieves the currently assigned names.
#'
#' @details Labels convert internal naming of features to the requested label at
#'   export or when plotting. Labels can be changed using the
#'   \code{set_feature_names} method.
#'
#' @inheritParams get_data_set_names,familiarCollection-method
#'
#' @return An ordered array of feature labels.
#' @export
#' @aliases get_feature_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_feature_names}} for updating the name and ordering of features.
#' @md
setMethod(
  "get_feature_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "feature_labels",
      get_levels = FALSE))
  }
)



# get_risk_group_names ---------------------------------------------------------

#' @title Get current risk group labels
#'
#' @description Risk groups in familiarCollection objects can have custom names
#'   for export and plotting. This function retrieves the currently assigned
#'   names.
#'
#' @details Labels convert internal naming of risk groups to the requested label
#'   at export or when plotting. Labels can be changed using the
#'   \code{set_risk_group_names} method.
#'
#' @inheritParams get_data_set_names,familiarCollection-method
#'
#' @return An ordered array of risk group labels.
#' @export
#' @aliases get_risk_group_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_risk_group_names}} for updating the name and ordering of risk groups.
#' @md
setMethod(
  "get_risk_group_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "km_group_labels",
      get_levels = FALSE))
  }
)



# get_class_names --------------------------------------------------------------

#' @title Get outcome class labels
#'
#' @description Outcome classes in familiarCollection objects can have custom
#'   names for export and plotting. This function retrieves the currently
#'   assigned names.
#'
#' @details Labels convert internal class names to the requested label at export
#'   or when plotting. Labels can be changed using the \code{set_class_names}
#'   method.
#'
#' @inheritParams get_data_set_names,familiarCollection-method
#'
#' @return An ordered array of class labels.
#' @export
#' @aliases get_class_names
#' @seealso
#' * \linkS4class{familiarCollection} for information concerning the familiarCollection class.
#' * \code{\link{set_class_names}} for updating the name and ordering of classes.
#' @md
setMethod(
  "get_class_names",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "class_labels",
      get_levels = FALSE))
  }
)



# get_data_set_name_levels -----------------------------------------------------
setMethod(
  "get_data_set_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "data_set_labels",
      get_levels = TRUE))
  }
)



# get_learner_name_levels ------------------------------------------------------
setMethod(
  "get_learner_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "learner_labels",
      get_levels = TRUE))
  }
)



# get_fs_method_name_levels ----------------------------------------------------
setMethod(
  "get_fs_method_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "fs_method_labels",
      get_levels = TRUE))
  }
)



# get_feature_name_levels ------------------------------------------------------
setMethod(
  "get_feature_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "feature_labels",
      get_levels = TRUE))
  }
)



# get_risk_group_name_levels ---------------------------------------------------
setMethod(
  "get_risk_group_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "km_group_labels",
      get_levels = TRUE)) 
  }
)



# get_class_name_levels --------------------------------------------------------
setMethod(
  "get_class_name_levels",
  signature(x = "familiarCollection"),
  function(x) {
    
    return(.get_labels(
      x = x,
      upd_slot = "class_labels",
      get_levels = TRUE))
  }
)


# add_package_version (collection) ---------------------------------------------
setMethod(
  "add_package_version",
  signature(object = "familiarCollection"),
  function(object) {
    
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)



# save (collection) ------------------------------------------------------------
setMethod(
  "save",
  signature(
    list = "familiarCollection",
    file = "character"),
  function(list, file) {
    .save(object = list, dir_path = file)
  }
)



# get_object_name (collection) -------------------------------------------------
setMethod(
  "get_object_name",
  signature(object = "familiarCollection"),
  function(
    object,
    abbreviated = FALSE) {
    
    # Get the full name of the object
    object_name <- object@name
    
    return(object_name)
  }
)



# show (collection) ------------------------------------------------------------
setMethod(
  "show",
  signature(object = "familiarCollection"),
  function(object) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Create an initial descriptor.
    cat(paste0(
      "A collection of datasets (", object@name, "; ",
      .familiar_version_string(object), "):\n"))
    
    lapply(
      object@data_sets,
      function(x) cat(paste0("  ", x, "\n")))
    
    # Outcome details
    cat("\nThe collection contains data for the following outcome:\n")
    show(object@outcome_info)
  }
)



# .set_labels ------------------------------------------------------------------
setMethod(
  ".set_labels",
  signature(x = "familiarCollection"),
  function(
    x,
    old_label,
    new_label,
    new_order,
    upd_slot) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    label <- NULL
    
    # Make sure the collection object is updated.
    x <- update_object(object = x)
    
    # Read the slot
    label_table <- slot(x, upd_slot)
    
    # Generate the slot
    if (is.null(label_table)) {
      x <- .construct_label_table(x = x, upd_slot = upd_slot)
      
      # Read the label table
      label_table <- slot(x, upd_slot)
    }
    
    ## Label update ------------------------------------------------------------
    if (!is.null(new_label)) {
      
      # Check that the length of new_label is not 0
      if (length(new_label) == 0) {
        stop("The number of \"new\" labels should be larger than 0.")
      }
      
      # Check for duplicates in new_label
      if (anyDuplicated(new_label)) {
        warning("The \"new\" labels contain duplicate values.")
      }
      
      # Get old labels, if they were not provided.
      if (is.null(old_label)) {
        
        # Check if we can safely assume that the new labels are matching 
        if (length(new_label) != nrow(label_table)) {
          stop(paste0(
            "If \"old\" is not provided explicitly, \"new\" should have the ",
            "same length as the currently existing labels."))
        }
        
        # Assume that old is the currently known label
        old_label <- label_table$label
      }
      
      # Check for duplicates in old_label
      if (anyDuplicated(old_label)) {
        stop("The provided \"old\" labels contain duplicate values.")
      }
      
      # Check that length of new_label and old_label are the same
      if (length(new_label) != length(old_label)) {
        stop("The same number of \"old\" and \"new\" labels should be provided.")
      }
      
      # Check if all old_label entries are actually existing labels.
      unrecognised_labels <- old_label[!old_label %in% label_table$label]
      if (length(unrecognised_labels) > 0) {
        stop(paste0(
          "The following old labels were not found: ",
          paste_s(unrecognised_labels)))
      }
      
      # Generate a data.table using old_label and new_label
      replacement_table <- data.table::data.table(
        "label" = old_label,
        "new_label" = new_label)
      
      # Merge both tables
      label_table <- merge(
        x = label_table,
        y = replacement_table,
        by = "label",
        all.x = TRUE)
      
      # Copy old labels to the new_label column for labels that were not
      # provided previously
      label_table[is.na(new_label), "new_label" := label]
      
      # Rename colums
      data.table::setnames(
        x = label_table,
        old = c("label", "new_label"),
        new = c("old_label", "label"))
      
      # Drop the "old_label" column as it is no longer required
      label_table[, "old_label" := NULL]
    }
    
    ## Label order update ------------------------------------------------------
    if (!is.null(new_order)) {
      # Update label order based on new_order
      
      # Check that new_order has the same length as the label_table
      if (length(new_order) != length(unique(label_table$label))) {
        stop("\"order\" should match the number of labels.")
      }
      
      # Check that there are no duplicates in new_order.
      if (anyDuplicated(new_order)) {
        warning("\"order\" contains duplicate entries.")
      }
      
      # Check that new_order has the same class the label.
      if (!is_any(new_order, class(label_table$label))) {
        stop(paste0(
          "\"order should have the same class as the label, i.e. ",
          paste(class(label_table$label), collapse = " or "), "."))
      }
      
      # Check that there are no elements of new_order that do not appear as a label
      unrecognised_labels <- new_order[!new_order %in% label_table$label]
      if (length(unrecognised_labels) > 0) {
        stop(paste0(
          "The following labels were not found for ordering: ",
          paste_s(unrecognised_labels)))
      }
      
      # Find the order by matching the label column in the label table with the
      # provided labels in new_order
      new_label_order <- match(
        label_table$label,
        new_order)
      
      # Update the label order
      label_table[, "label_order" := new_label_order]
    }
    
    # Insert the updated label table into its slot
    slot(x, upd_slot) <- label_table
    
    # Return the input object
    return(x)
  }
)



# .construct_label_table -------------------------------------------------------
setMethod(
  ".construct_label_table",
  signature(x = "familiarCollection"),
  function(x, upd_slot) {
    # Constructs a new label table if required.
    
    # Do not generate a new table with labels if it already exists
    if (!is.null(slot(x, upd_slot))) return(x)
    
    if (upd_slot == "data_set_labels") {
      data <- slot(x, "data_sets")
      
    } else if (upd_slot == "learner_labels") {
      data <- slot(x, "learner")
      
    } else if (upd_slot == "fs_method_labels") {
      data <- slot(x, "fs_method")
      
    } else if (upd_slot == "feature_labels") {
      data <- unique(c(
        slot(x, "required_features"),
        x@fs_vimp$vimp_table$name))
      
    } else if (upd_slot == "km_group_labels") {
      data <- unique(c(
        "low", "moderate", "high",
        unlist(lapply(x@km_data, function(x) (levels(x@data$group))))))
      
    } else if (upd_slot == "class_labels") {
      data <- get_outcome_class_levels(x)
      
    } else {
      stop("Slot is not available for familiarCollection objects.")
    }
    
    # Add the new label table to the slot
    slot(x, upd_slot) <- data.table::data.table(
      "internal" = data,
      "label" = data,
      "label_order" = seq_len(length(data)))
    
    return(x)
  }
)



# .get_labels ------------------------------------------------------------------
setMethod(
  ".get_labels",
  signature(x = "familiarCollection"),
  function(
    x,
    upd_slot,
    get_levels = FALSE) {
    # Get ordered levels (i.e. internal column from the label table) from the
    # specific slot
    
    # Suppress NOTES due to non-standard evaluation in data.table
    label_order <- NULL
    
    # Make sure the collection object is updated.
    x <- update_object(object = x)
    
    # Determine if the slot has been set.
    if (is.null(slot(x, upd_slot))) {
      x <- .construct_label_table(x = x, upd_slot = upd_slot)
    }
    
    # Read the label table
    label_table <- slot(x, upd_slot)
    
    # Order table by label order
    label_table <- label_table[order(label_order)]
    
    if (get_levels) {
      # Return ordered levels if get_levels is TRUE
      return(label_table$internal)
      
    } else {
      # Return ordered labels
      return(label_table$label)
    }
  }
)
