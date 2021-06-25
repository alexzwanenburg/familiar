# Development

## Major changes

* All models are now trimmed to remove unnecessary objects such as nested
environments, copies of training data, etc. This should cause an overall smaller
memory footprint. Note that this does not necessarily imply anonymisation of the
data. Notably, k-nearest neighbour learners still maintain a copy of the
training dataset internally.

* Naive Bayes and k-nearest neighbour learners now use the `e1071` package
instead of `klaR`, which has been deprecated from familiar. Some hyperparameters
changed accordingly. See the *learners* vignette.

* Added individual conditional expectation and partial dependence plots.

## Minor changes

* A `sample_limit` parameter was added to limit the number of samples used
during evaluation. This parameter can be specified for the `sample_similarity`
evaluation step.

* Class levels for categorical outcomes, if not explicitly specified using the
`class_levels` parameter, are now sorted before being set based on the data.
Previously class levels would be set based on order of appearance.

## Bug fixes

* Fixed a bug that caused clustered features not to be exported for the purpose
of reporting (aggregate) variable importance.

* Fixed a bug that caused model-based variable importance to be exported when
calling the `export_fs_vimp` method with any `object` that is not a
`familiarCollection` object.

* Fixed a bug that prevented rank aggregation method and thresholds from being
set while exporting variable importance using `export_fs_vimp` or
`export_model_vimp`.

* Fixed an error that occurred when attempting to fit calibration data with `NA`
values.

# Version 0.0.0.54 (Pre-release)

## Major changes:

* Added novelty detection. An isolation forest is created at the same time as
the main model using the same data. It can then be used to (prospectively)
identify samples that are dissimilar to the training samples, and for which the
model may need to extrapolate.

    * Added a `novelty_features` parameter that can be used to specify features
    that should be used for novelty detection, in addition to those already used
    in the model.

* Added `update_object` methods that allow for backward compatibility when
updating slots of respective objects.

* Added support for series-like data. These can be time series, or multiple
measurements were the outcome of interest may change. Subsampling, e.g. through
cross-validation or bootstraps still respects samples. This means that different
series instances of the same sample are always kept together for subsampling.
The series column in the data set can be set using the `series_id_column`
parameter. This required changes to what iteration data is stored. This should
not cause any issues with post-hoc analyses, but is not **not backward
compatible** when updating familiar prior to completing the modelling and
evaluation process.

* Improved flexibility of the evaluation process that is conducted to explain
and assess models:
    * Added `dynamic_model_loading` parameter that supports dynamic loading of
    models to an ensemble. This reduces the memory footprint at the cost of IO
    overhead as the models are read from the disk or network when required. By
    default, all models are statically attached to an ensemble.
    
    * Added `skip_evaluation_elements` parameter that allows skipping one or
    more steps of the evaluation process. This is useful if some evaluations are
    not relevant.
    
    * Updated `parallel_hyperparameter_optimisation` and `parallel_evaluation`
    to allow for specifying whether parallelisation should take place inside
    (`inner`) or outside (`outer`) the respective processes. For `outer` the
    parallelisation takes place over different subsamples, learners, etc. This
    may provide an increase in processing speed, at the cost of less feedback
    and a higher memory footprint.
    
    * All evaluation steps now produce `familiarDataElement` elements. This is
    not **not backward compatible**. `familiarData` and `familiarCollection`
    objects created using previous versions need to be created anew. enables
    flexibility in terms of how data and models are handled for computation.
    Specifically, many evaluation steps that focus on models can be evaluated at
    the ensemble (`ensemble`), model (`model`) or an intermediate level
    (`hybrid`) by setting the `detail_level` parameter. The `hybrid` level
    differs from `ensemble` in that the individual model predictions are used
    instead of the prediction of the ensemble itself. Likewise, the type of
    estimation can now be flexibly chosen for several evaluation steps by
    setting the `estimation_type` parameter.
    
    * Several parameters are now deprecated:
    
        * `compute_model_data` has been completely deprecated. This can now be
        specified using the new `detail_level` parameter.
        
        * `compute_model_ci` has been completely deprecated. This can now be
        specified using the new `estimation_type` parameter.
        
        * `compute_ensemble_ci` has been completely deprecated. This can now be
        specified using the new `estimation_type` parameter.
        
        * `aggregate_ci` has been replaced by `aggregate_results`. Aside from
        bootstrap confidence intervals, underlying results for bias-corrected
        estimates can now be aggregated.
        
* Calibration plots have been completely revamped to now include confidence
intervals. Moreover, calibration plots based on `bootstrap_confidence_interval`
and `bias_corrected` estimation types no longer show points, but are based on
interpolation to a regular grid after computing a loess model. `point` estimates
are unaltered. Density plots have also been revised to use a fixed standard
deviation of 0.075, which prevents some of the erratic behaviour seen previously
when most expected probability values were clustered closely.

* Clustering based on feature similarity during the evaluation process can now
be specified after similarity has been computed, i.e. through
`export_feature_similarity` and `plot_feature_similarity`. This allows for
changing clustering parameters after the analysis.

* Parallel processing now supports mini-batching. Many processes are actually
fast to compute and repeated IO to cluster nodes noticably slows down the process.
Mini-batches transfers data to nodes in one go for local sequential processing.
In addition, processes that can mini-batch are now measured, and an optimal
number of nodes is selected based on IO and process times. This should
significantly speed up processes with low process time compared to IO time.

* Hyperparameter optimisation now predicts the time taken for training using a
specific set of hyperparameters. It uses the predicted time to optimise
assignment to nodes for parallel processing. This eliminates an issue where
hyperparameter optimisation with parallel nodes could take significantly longer
than simple sequential optimisation. In addition, hyperparameter optimisation
now has several new or changed configuration parameters:
    
    * `smbo_random_initialisation` is no longer a logical (`TRUE` or `FALSE`)
    but takes `fixed_subsample`, `fixed` or `random` as values.
    `fixed_subsample` generates initial hyperparameters from the same default
    hyperparameter grid as `fixed`, but unlike `fixed` does not exhaustively
    search all options. `random` creates random sets of initial hyperparameters.
    
    * `smbo_n_random_sets` can now be used to set the number of hyperparameters
    sets created for the `fixed_subsample` and `random` methods.

* `kernlab` is no longer used as backend for computing support vector machine
learners due to lack of stability. Unit testing showed consistent freezing. We
now use the SVM of `e1071` which we found to be stable. SVM models created using
previous versions of familiar are no longer compatible.

## Minor changes:

* The default method for bootstrap confidence intervals (`bootstrap_ci_method`)
is now the percentile (`percentile`) method, which replaces the bias-corrected
(`bc`) method.

* The value returned for the bias-corrected bootstrap confidence interval method
is now the bias-corrected median, not the point estimate. This harmonises the
behaviour of the percentile and bias-corrected confidence interval methods. The
bias-corrected median can be viewed as an optimism correction of the point
estimate.

* Several attribute slots for S4 `familiarModel`, `familiarEnsemble`,
`familiarData` and `familiarCollection` objects were removed, revised or added.
Changes are backward compatible due to the new `update_object` method.

* Lambda parameters of Box-Cox and Yeo-Johnson transformations are now
determined using `stats::optimise`. The previous, fixed, settings were sensible
for Box-Cox, but the Yeo-Johnson method benefits from a wider selection. This
does not affect backward compatibility.

* The `as_data_object` method can now be used with `familiarModel` and
`familiarEnsemble` objects to check whether the input data can be correctly
formatted, and will provide meaningful errors if not.

* Added `show` methods for objects that are typically written to drive, such as
`familiarModel`, `familiarEnsemble`, `familiarData` and `familiarCollection`
objects.

* Added `plot_auc_precision_recall_curve` method to plot precision-recall
curves.

* Random forests created using the `rfsrc` package are now anonymous forests,
i.e. training data are not stored with the model. In addition, we explicitly
generate and store a random seed, so that the forest can be regrown for
determining variable importance.

## Bug fixes:
* Fixed an error that would cause hyperparameter optimisation to not select the
optimal set of hyperparameters.

* Fixed an error that would cause feature selection to fail when all features in
the data are also set to be in the signature.

* Fixed an error that occurred when attempting to create risk groups from models
that were not successfully trained.

* Fixed an error in ComBat batch normalisation caused by invariant or NA
features in one or more batches.

* Fixed a bug that would incorrectly assign samples to wrong subsamples (e.g.
in-bag or out-of-bag data). This only occurred if the same sample identifier
exists in different batches.

* Fixed an error that occurred prior to hyperparameter optimisation because a
model-dependent hyperparameter required to create a metric object may not have
been set.

* Fixed an error when attempting to perform parallel processing with familiar
installed on a non-standard library path.

* Fixed an issue where the `verbose` argument was not respected when forming
clusters of features.

* Fixed an issue where absence of censoring for time-to-event data would lead to
models not being created.

* Fixed an issue where NA would not be removed from the results in the
`extract_from_slot` function.

* Fixed an issue where information would be missing in `familiarEnsemble`
objects because the first `familiarModel` in the ensemble was not trained. This
information cannot be added retroactively.

* Fixed an issue that would cause `export_permutation_vimp` to export the wrong
data when called by the user.

* Fixed an issue that would cause an error when a decision curve was plotted
with a confidence interval but without requiring multiple line colours.

* Fixed an issue that would cause an error when a receiver operating
characteristic curve was plotted with a confidence interval but without
requiring multiple line colours.

* Fixed an error that occurred when categorical outcome levels are numeric, e.g.
0 and 1. For some learners, such as `svm`, this caused an indexing error as the
outcome levels were interpreted as indices instead of column names.

* Fixed an error during batch normalisation when there is a mismatch between
features with feature information and actual features present in the dataset.

* Fixed an issue that would cause an error when computing a pseudo-R2 similarity
score between two arrays that each have one unique value.

# Version 0.0.0.53 (Pre-release)

## Major changes:
* All metrics are now implemented as S4 objects, with associated methods.
Moreover all metrics now have unit tests.

* All plotting algorithms now have unit tests which should increase stability of
the code. Resulting code fixes are **not backward compatible**: you may need to
recreate the `familiarData` objects for binomial endpoints.

* Hyperparameter optimisation now has additional parameters:
    * `optimisation_determine_vimp`: Allows for determining variable importance
    for each of the bootstraps used during hyperparameter optimisation to avoid
    positive biases.
    * `optimisation_function`: replaces the objective parameter.
    * `smbo_stop_tolerance`: tolerance for a optimisation score to be convergent.
    * `acquisition_function`: an acquisition function can now be selected.

* Hyperparameter optimisation can now be performed using multiple optimisation metrics instead of one.



## Minor changes:
* Data computation for individual models can now be explicitly set using the
`compute_model_data` parameter.

* Many bugs were fixed.
