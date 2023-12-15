#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# promote_detector -------------------------------------------------------------
setMethod(
  "promote_detector",
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    learner <- object@learner

    if (learner %in% .get_available_isolation_forest_detectors()) {
      # Isolation forest
      object <- methods::new("familiarIsolationForest", object)
      
    } else if (learner %in% .get_available_none_detectors()) {
      object <- methods::new("familiarNoneNoveltyDetector", object)
    }

    # Add package version.
    object <- add_package_version(object = object)

    # Returned object can be a standard familiarNoveltyDetector.
    return(object)
  }
)



.get_detector_hyperparameters <- function(
    data,
    detector,
    names_only = FALSE) {
  # Create familiarNoveltyDetector
  fam_detector <- methods::new(
    "familiarNoveltyDetector",
    learner = detector)

  # Set up the specific model.
  fam_detector <- promote_detector(object = fam_detector)

  # Model hyperparameters
  detector_hyperparameters <- get_default_hyperparameters(
    object = fam_detector,
    data = data)

  # Extract names from parameter list
  if (names_only) detector_hyperparameters <- names(detector_hyperparameters)

  return(detector_hyperparameters)
}



.check_novelty_detector_available <- function(detector, as_flag = FALSE) {
  # Create familiarNoveltyDetector
  fam_detector <- methods::new(
    "familiarNoveltyDetector",
    learner = detector)

  # Set up the specific novelty detector
  fam_detector <- promote_detector(fam_detector)

  # Check validity.
  detector_available <- is_available(fam_detector)

  if (as_flag) return(detector_available)

  # Check if the familiar model has been successfully promoted.
  if (!is_subclass(class(fam_detector)[1], "familiarNoveltyDetector")) {
    stop(paste0(
      detector, " is not a valid novelty detector. ",
      "Please check the documentation for available novelty detectors."))
  }

  # Check that the required package can be loaded.
  require_package(
    x = fam_detector,
    purpose = paste0("to train a novelty detector using the ", detector, " algorithm"),
    message_type = "backend_error"
  )
}
