# Development

## Major changes:
* Added novelty detection:
    * Added `novelty_features` parameter that can be used to specify features that should be used for novelty detection.
* Added `show` methods for objects that are typically written to drive, such as familiarModel, familiarEnsemble, familiarData and familiarCollection objects.
* Added `update_object` methods that allow for backward compatibility when updating slots of respective objects.
* Added support for series-like data. These can be time series, or multiple measurements were the outcome of interest may change. Subsampling, e.g. through cross-validation or bootstraps still respects samples. This means that different series instances of the same sample are always kept together for subsampling. The series column in the data set can be set using the `series_id_column` parameter. This required changes to what iteration data is stored. This should not cause any issues with post-hoc analyses, but is not **not backward compatible** when updating familiar prior to completing the modelling and evaluation process.

## Minor changes:
* The default method for bootstrap confidence intervals (`bootstrap_ci_method`) is now the percentile (`percentile`) method, which replaces the bias-corrected (`bc`) method.
* The value returned for the bias-corrected bootstrap confidence interval method is now the bias-corrected median, not the point estimate. This harmonises the behaviour of the percentile and bias-corrected confidence interval methods. The bias-corrected median can be viewed as an optimism correction of the point estimate.
* Several attribute slots for S4 familiarModel, familiarEnsemble, familiarData and familiarCollection objects were removed, revised or added. Changes are backward compatible due to the new `update_object` method.
* Lambda parameters of Box-Cox and Yeo-Johnson transformations are now determined using `stats::optimise`. The previous, fixed, settings were sensible for Box-Cox, but the Yeo-Johnson method benefits from a wider selection. This does not affect backward compatibility.

## Bug fixes:
* Fixed an error that would cause hyperparameter optimisation to not select the optimal set of hyperparameters.
* Fixed an error that would cause feature selection to fail when all features in the data are also set to be in the signature.
* Fixed an error that occurred when attempting to create risk groups from models that were not successfully trained.
* Fixed an error in ComBat batch normalisation caused by invariant or NA features in one or more batches.
* Fixed a bug that would incorrectly assign samples to wrong subsamples (e.g. in-bag or out-of-bag data). This only occurred if the same sample identifier exists in different batches.
* Fixed an error that occurred prior to hyperparameter optimisation because a model-dependent hyperparameter required to create a metric object may not have been set.
* Fixed an error when attempting to perform parallel processing with familiar installed on a non-standard library path.
* Fixed an issue where the `verbose` argument was not respected when determining clusters of features.
* Fixed an issue where absence of censoring for time-to-event data would lead to models not being created.
* Fixed an issue where NA would not be removed from the results in the `extract_from_slot` function.
* Fixed an issue where information would be missing in `familiarEnsemble` objects because the first `familiarModel` in the ensemble was not trained. This information cannot be added retroactively.

# Version 0.0.0.53 (Pre-release)

## Major changes:
* All metrics are now implemented as S4 objects, with associated methods. Moreover all metrics now have unit tests.
* All plotting algorithms now have unit tests which should increase stability of the code. Resulting code fixes are **not backward compatible**: you may need to recreate the `familiarData` objects for binomial endpoints.
* Hyperparameter optimisation now has additional parameters:
    * optimisation_determine_vimp: Allows for determining variable importance for each of the bootstraps used during hyperparameter optimisation to avoid positive biases.
    * optimisation_function: replaces the objective parameter.
    * smbo_stop_tolerance: tolerance for a optimisation score to be convergent.
    * acquisition_function: an acquisition function can now be selected.
* Hyperparameter optimisation can now be performed using multiple optimisation metrics instead of one.

## Minor changes:
* Data computation for individual models can now be explicitly set using the compute_model_data parameter.
* Many bugs were fixed.
