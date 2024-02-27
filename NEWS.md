# Version 2.0.0 (Development)

## Major changes

- Some functionality was deprecated because of redundancy and stability issues:

  - The `count` outcome type has been deprecated. `count` is a subset of `continuous` outcomes. Its previous implementation did not provide any benefits over   `continuous`.

  - Gradient boosting using the `mboost` package was deprecated. Use `xgboost` instead.

  - The `qvalue` package for computing q-values was deprecated.

  - The `VGAM` package, which has been soft-deprecated since version 1.3.0, has now fully been deprecated.

  - The variable hunting feature selection method for random forests was removed due to stability issues in unit tests.

## Minor changes

- The `evaluation_elements` configuration parameter was added to allow for specifying which evaluation steps should be performed.

# Version 1.5.0 (Development)

## Major changes

- The source code now uses the `tidyverse` code style.

- Power transformation is now handled by the `power.transform` package. This package replaces the internal routines that were previously used.

## Bug fixes

- Fixed errors due to parsing columns called `else`, `for`, `function`, `if`, `in`, or `while`, which are reserved names in `R`.

- Fixed errors when creating feature or similarity plots caused by sample or feature names matching internal column names.

# Version 1.4.6 (Talented Toad)

## Bug fixes

- Fixes unused arguments appearing in documentation.

# Version 1.4.5 (Reminiscing Rat)

## Bug fixes

- Creating data objects (`as_data_object`) using naive learners now works and no longer throws an error.

- `filename`, `path` and `plot` arguments are not passed to `ggplot2::ggsave` for saving, and are now no longer present in the documentation of plot functions.

# Version 1.4.4 (Quixotic Quail)

## Bug fixes

- Prevented an error that could occur when computing net benefit for decision curves of models that would predict class probabilities of exactly 1. This was a very rare error, as it would only occur if predicted class probabilities would have at most two distinct values, one of which is 1.0.

- Prevented an issue that could occur when computing linear calibration fits where the fit can be computed without residual errors. This would prevent the t-statistic and p-value from being correctly computed for binomial, multinomial and survival outcomes.

- Prevented an issue when computing linear calibration fits when all the expected values are the same. The model will then lack a slope. We now add a slope of 0, with an infinite confidence interval, if this is the case.

# Version 1.4.3 (Puzzled Prawn)

## Bug fixes

- Prevented an error due to an overzealous check for hyperparameters being present for training a model.

# Version 1.4.2 (Omnicompetent Owl)

## Bug fixes

- Fixed an error that could occur when creating a lasso model for imputation using just a single feature.

# Version 1.4.1 (Nefarious Newt)

## Minor changes

- Robust methods for power transformations were added, based on the work of Raymaekers and Rousseeuw (Transforming variables to central normality. Mach Learn. 2021. doi:10.1007/s10994-021-05960-5). These methods are `yeo_johnson_robust` and `box_cox_robust`.

- A robust normalisation method, based on Huber's M-estimators for location and scale, was added: `standardisation_robust`.

- Improved efficiency of aggregating and computing point estimates for evaluation steps. It may occur that for each grouping (e.g. samples for pairwise sample similarity), multiple values are available that should be aggregated to a point estimate. Previously we split on all unique combinations of grouping column, and process each split separately. This is a valid approach, but can occur significant overhead when this forms a large number (>100k) splits. We now first determine which data (if any) require computation of a (bias-corrected) point estimate because of grouping. Often, each split would only contain a single instance which forms a point estimate on its own. Extra computation is avoided for these cases.

- Plots now always show the evaluation time point. This is relevant for, for example, calibration plots, where both the observed and expected (predicted) probabilities are time-dependent, and will change depending on the time point.

- Improved support for providing a file name for storing a plot. The plotting device is now changed based on the file name, if it has an extension. In case multiple plots would be created, e.g. due to splitting on some grouping variable, such as the underlying dataset, the provided file name is used as a base.

- Methods for setting labels previously could update the ordering of the labels for `familiarCollection` objects, which could produce unexpected changes. Setting new labels now does not change the label order. Use the `order` argument to update the order of the labels.

## Bug fixes

- Fixed an error that would occur when attempting to create risk group labels for a `familiarCollection` object that is composed of externally provided `familiarData` objects.

- Fixed an issue that would prevent a `familiarCollection` object from being returned if an experiment was run using a temporary folder.

- Fixed an issue with apply functions in familiar taking long to aggregate their results.

- Fixed an issue that would prevent Kaplan-Meier curves to be plotted when more than three risk strata where present.

- Fixed an error that would occur if Kaplan-Meier curves were plotted for more than one stratification method and different risk groups.

- Fixed an issue that could potentially cause matching wrong transformation and normalisation parameter values when forming ensemble models. This may have affect sample cluster plots, which uses this information.

# Version 1.4.0 (Misanthropic Muskrat)

## Major changes

- Hyperparameter optimisation now trains naive models if none of the hyperparameter sets lead to models that perform better than these models. Previously a model was trained regardless of whether such a model would actually be better than a naive model. Naive models, for example, predict the majority class or median value, depending on the problem.

## Minor changes

- Metrics for assessing performance of regression models, such as mean squared error, can now be computed in winsorised or trimmed (truncated) forms. These can be specified by appending `_winsor` or `_trim` as a suffix to the metric name. Winsorising clips the predicted values for 5% of the instances with the most extreme absolute errors prior to computing the performance metric, whereas trimming removes these instances. The result of either option is that for many metrics, the assessed model performance is less skewed by rare outliers.

- Two additional optimisation functions were defined to assess suitability of hyperparameter sets:

  - `model_balanced_estimate`: seeks to maximise the estimate of the balanced IB and OOB score. This is similar to the `balanced` score, and in fact uses a hyperparameter learner to predict said score (not available for random search).
  
  - `model_balanced_estimate_minus_sd`: seeks to maximise the estimate of the balanced IB and OOB score, minus its estimated standard deviation. This is similar to the `balanced` score, but takes into account its estimated spread. Note that like `model_estimate_minus_sd`, the width of the distribution of balanced scores is more difficult to determine than its estimate.

- The `balanced` optimisation function now adds a penalty when the trained model on the training data performs worse then a naive model.

- A new exploration method for hyperparameter optimisation was added, namely `single_shot`. As the name suggests, this performs a single pass on the challenger and incumbent models during each intensification iteration. This is also the new default. Extensive tests have shown that the use of single-shot selection led to comparable performance.

- Convergence checks for hyperparameter sets now depend on the validation optimisation score, as this is more stable than the summary score for some `optimisation_function` methods, such as `model_estimate_minus_sd`. More over the tolerance has been changed to allow for values above `0.01` for sample sizes smaller than `100`. This prevents convergence issues where the expected statistical fluctuation for small sample sizes would easily break convergence checks, and hence force long searches for suitable hyperparameters.

- The default familiar plotting theme is now exported as `theme_familiar`. This allows for changing tweaking the default theme, for example, setting a larger font size, or selecting a different font family. After making changing to theme, it can be provided as the `ggtheme` argument.

## Bug fixes

- `ggtheme` is now checked for completeness, which prevents errors with unclear causes or solutions.

- We previously checked that any coefficients of a regression model could be estimated. This could lead to large models being formed where all features were insufficiently converged, even if this led to a meaningless model. We now check that all (instead of any) coefficients could be estimated for GLM, Cox and survival regression models.

- Fixed an error caused by unsuccessfully retraining an anonymous random forest for variable importance estimations.

- Fixed errors due to introduction of `linewidth` elements in version 3.4.0 of `ggplot2`. Versions of `ggplot2` prior to 3.4.0 are still supported.

# Version 1.3.0 (Loquacious Limpet)

## Major changes

- Improved speed of fitting generalised models by switching to the `fastglm` and `nnet` packages. This not only affects learners, but also univariate importance and pseudo-R<sup>2</sup> similarity metrics. This difference is most notable for datasets with large numbers of samples.

- Normalised mutual information (`mutual_information`) is now the default similarity metric. The previous default similarity metric (`mcfadden_r2`) was found to handle sparse features poorly. The implementation is based on the `praznik` package.

## Minor changes

- Mutual information-based variable importance methods will now use the mutual information computation as implemented in `praznik` by default.

- The signature size range for hyperparameter optimisation is now determined by the variable importance method. Familiar will now determine a valid range based on the features ranked by the variable importance method. For univariate methods, these are typically all features. However, multivariate variable importance methods often select a smaller subset of features. The size of that subset is now used as the default maximum range, instead of the number of features present in the entire data set. As a bonus, the same code is used to determine the size of the signature and to set the signature. This avoids issues due to discrepancies between the two that would produce errors in older versions.

- Trimming now also trims trimmed functions, which on occasion could still contain large environments.

- Defining a replacement for a trimmed model now may not take more than 1 minute. For some model types (e.g. `nnet::multinom`) creating the replacement co-variance matrix can be computationally expensive for larger datasets. If a replacement can not be created in time, no replacement function is created. Parts of the code where that may produce issues now capture errors related to this issue.

- Hyperparameter optimisation now takes suspension of the R process into account when determining the optimisation time limit.

- Reference levels for categorical features are now set to the most frequent level for categorical features that are automatically detected. This behaviour can be specified using the `reference_method` parameter.

## Bug fixes

- Models were unnecessarily trimmed during hyperparameter optimisation, which unnecessarily slowed down the optimisation process.

- Fixed an error that may occur if the signature size was larger than the number of available features when using the `random` variable importance method.

- Fixed an issue with high pseudo-R<sup>2</sup> similarity being computed between two numeric features where one numeric feature consisted of only few unique values.

- Relaxed distance requirements for assigning all features to the same cluster using silhouette-based clustering. Since pseudo-R<sup>2</sup> are now computed using approximative methods, distance for assigning all features to the same cluster can deviate somewhat from 0. This would be noticeable when exactly two features are present.

- Fixed an error due to not passing a `cl` argument when performing evaluation steps with `detail_level="model"` and parallel processing.

- Fixed an issue that could cause the hyperparameter optimisation algorithm to become myopic and focus on hyperparameter sets that generally fail, except for a few instances.

# Version 1.2.1 (Kindhearted Koala)

## Minor changes

- On the relatively rare occasion where a model fails to yield one or more valid predictions, all evaluation steps concerning model performance will not produce results. This affects AUC curves, calibration, confusion matrices, decision curves, model performance plots, and Kaplan-Meier plots.

## Bug fixes

- Fixed a rare issue where `VGAM::vglm` would fail to create a model for computing McFadden's R<sup>2</sup>, and subsequently cause an error.

- Fixed an issue where parallel clusters created outside of familiar would not receive the required data to be used for parallel processing.

- Fixed an issue where invalid predictions (i.e. NA or infinite values) would produce errors when attempting to compute stratification thresholds.

# Version 1.2.0 (Jocund Jackdaw)

## Major changes

- Several pre-processing steps, i.e. transformation, normalisation, batch normalisation, imputation and clustering have been re-implemented as objects. This allows for better portability between experiments, improved flexibility and extensibility to newer methods, and better forward compatibility.

- Variable importance / feature selection is now implemented in an object-oriented fashion. Variable importance data written to the file system are now `vimpTable` objects, instead of data collected in a loose list. The `aggregate_vimp_table` and `get_vimp_table` methods can be used to aggregate multiple variable importance tables and retrieve the variable importance table as a `data.table` respectively.

- It is now possible to warm-start different experiments by providing an `experimentData` object to the `summon_familiar` function using the `experiment_data` argument. Such objects are created using the `precompute_data_assignment`, `precompute_feature_info` and `precompute_vimp` functions. These functions assign instances to training, validation and test data, derive feature processing parameters and compute variable importance respectively. `precompute_vimp` and `precompute_feature_info` includes any previous steps. 

## Minor changes

- It is now possible to set the number of bootstraps that are initially explored for hyperparameter optimisation using the `smbo_initial_bootstraps` configuration parameter. The default value is `1`, indicating that the initial hyperparameter sets are initially evaluated on a single bootstrap.

- Updated objects now both show initial and current familiar versions.

## Bug fixes

- Consistency of S4 objects is more actively checked in exported methods and functions by calling `update_object`. This prevents errors attempting when attempting to use a more recent version of familiar than the one used to create the objects. Previously these checks were only rigorously performed for objects that were loaded internally, and not those directly loaded by the user.

- The `n_dim` parameter for isolation forests is now correctly set for datasets without any features.

- Pseudo-R<sup>2</sup> similarity metrics now correctly produce a value of 1.0 for exact fits. Previously these could produce infinite log-likelihoods, and return default value of 0.0 (no similarity).

- A default value of the `time_max` parameter can now be determined even if training cohorts are not explicitly set.

# Version 1.1.4 (Innovative Iguana)

## Bug fixes

- Fixed an issue with LASSO-based imputation that lead to errors when attempting to impute values using LASSO models that were trained on a single feature.

# Version 1.1.3 (Hoarse Horse)

## Bug fixes

- Fixed an issue with LASSO-based imputation when aggregating from LASSO models with different required features. This is a temporary solution, that will be tackled more comprehensively in version 1.2.0.

- Fixed an issue with the parenthesis check in nested experimental designs.

- Fixed an issue that would prevent data where the same instance appears multiple times (e.g. bootstraps) from being properly evaluated.

# Version 1.1.2 (Gregarious Gopher)

## Bug fixes

- Fixed an issue that would cause all features to be used for hyperparameter optimisation for `signature_only` and `random` feature selection methods.

# Version 1.1.1 (Flamboyant Flamingo)

## Minor changes

-   Random forest-based variable importance methods now have variants that use the default values provided by the underlying algorithms. These can be recognised by the suffix `_default`. This is mainly done to avoid long hyperparameter optimisation times for such methods during feature selection.

## Bug fixes

-   Hyperparameter time limit and time taken are now always correctly parsed to minutes instead of seconds, minutes, or rarely, hours.

-   Familiar model objects now correctly show that no novelty detector was trained with `novelty_detector="none"`.

-   Fixed missing documentation for the `optimisation_function` configuration parameter.

-   Confidence intervals in calibration plots are no longer cropped to the [0,1] range. Previously, the estimate itself was not cropped, whereas its confidence interval was.

-   `xgboost` models now still work after being loaded from a file.

# Version 1.1.0 (Enchanting Earthworm)

## Major changes

-   Added `train_familiar` function that trains (and returns) models, but skips evaluation steps. This function is essentially a wrapper around `summon_familiar`.
-   Multivariate feature selection / variable importance methods such as `multivariate_regression`, `mrmr` and `lasso` now respect signature features set using the `signature` configuration parameter. Features provided in `signature` are always selected for the resulting model, and were therefore ignored during feature selection for both univariate and multivariate method. This has changed, so that multivariate methods now use the signature features as the basic set and attempt to identify any additional suitable features. Signature features are still ignored for univariate methods.
-   Many learners now allow for sample weighting to correct for class imbalances. By default this is done by weighting using inverse sample weights. This can be changed to an effective number method by setting model hyperparameters.
-   Hyperparameter optimisation is now less greedy during intensification steps when using `successive_halving` or `stochastic_reject` as exploration methods. Fewer bootstraps are assessed during intensification steps if there are any bootstraps that have only partially been sampled by the hyperparameter sets under evaluation. This should accelerate the optimisation process considerably. The (so far untested) rationale is that the hyperparameter learners should generally be able to accurately model the optimisation score of hyperparameter sets using locally sparse data.
    -   By default, a maximum of 20 bootstraps are now used to evaluate hyperparameter sets. This is down from the default of 50 used previously. This saves time spent on computing variable importance.
    -   It is now moreover possible to limit the time (in minutes) spent on optimisation using the `smbo_time_limit` parameter. Optimisation will stop once this limit has been exceeded. Note that familiar does not actively kill ongoing optimisation processes, but waits until they complete before stopping optimisation. Actively killing processes would require a general overhaul of the parallelisation routines used in familiar, which is complex and not an urgent priority.
-   Models now show warnings and errors encountered while (attempting to) train the model. This allows for identifying potential issues with the underlying data, and model-specific issues.
    -   In case hyperparameters cannot be obtained for a model due to errors encountered while training the models, these errors are now reported.
-   R version 4.0.0 or newer is now required, instead of 3.4.0.

## Minor changes

-   Added `summary`, `vcov` and `coef` method for `familiarModel` objects. These respectively apply `summary`, `vcov` and `coef` to the stored model.
-   Added relative absolute error, relative squared error and root relative squared error as performance metrics.
-   Models to predict the goodness (*optimisation score*) of hyperparameter sets are now object-oriented. This change is not visible to the user.
-   Additional options are now available to as `optimisation_function` to determine optimisation and overall summary scores of hyperparameter sets. Newly introduced are:
    -   `validation_minus_sd`: The mean performance on out-of-bag data minus its standard deviation.

    -   `validation_25th_percentile`: The 25th percentile of model performance on out-of-bag data.

    -   `model_estimate`: The estimated model performance inferred by the hyperparameter model that was previously used to identify new candidate hyperparameter sets. Not available for random search.

    -   `model_estimate_minus_sd`: The estimated model performance minus its standard deviation. Not available for random search.
-   The default `optimisation_function` is now `validation`.
-   The default `smbo_stop_tolerance` now depends on the number of samples and varies between `0.01` for 100 or fewer samples, and `0.001` for 10000 or more samples.
-   Additional data are now exported after optimising hyperparameters, namely the iteration step during which performance data were obtained, the time taken by the optimisation process, the learner used to learn how well hyperparameter sets perform, and the optimisation function. This is in addition to the score table and parameter tables that were already exported previously.
-   The *stringi* package has been phased out, and is no longer suggested or imported.

## Bug fixes

-   Fixed some missing verbosity settings.

-   Fixed a deprecation warning in the *xgboost* package for DART boosters, that appeared for versions 1.4.0 and newer.

-   Trained models now contain information for features used only for missing data inference.

# Version 1.0.2 (Dolorous Dragon)

## Bug fixes

-   Capture a rare error produced by the *maxstat* package when determining optimal risk stratification thresholds.

-   Fixed unexpected behaviour in interactions between *project_dir* and *experiment_dir* configuration parameters.

# Version 1.0.1 (Capricious Cat)

## Bug fixes

-   Fixed a bug that prevented computation of model variable importance when data was not loaded. This affected learners based on *glmnet*, *randomForestSRC* and *ranger* packages.

-   Fixed a bug that prevented familiar from identifying iteration files that were created previously for the same experiment.

-   Fixed a bug that would only occur during unit testing that was due to feature importance files being considered present without these files actually existing.

# Version 1.0.0 (Boisterous Bullfrog)

## Major changes

-   All models are now trimmed to remove unnecessary objects such as nested environments, copies of training data, etc. This should cause an overall smaller memory footprint. Note that this does not necessarily imply anonymisation of the data. Notably, k-nearest neighbour learners still maintain a copy of the training dataset internally.

-   Naive Bayes and k-nearest neighbour learners now use the `e1071` package instead of `klaR`, which has been deprecated from familiar. Some hyperparameters changed accordingly. See the *learners* vignette.

-   Added individual conditional expectation and partial dependence plots. These plots show the response of a model across a range of values for a particular feature.

-   Hyperparameter optimisation now allows for more flexibility in setting the exploration method. The exploration method determines how less promising hyperparameter sets are pruned during intensification steps. The exploration method can be set using the `exploration_method` argument. Familiar currently supports the following options:

    -   `successive_halving` (default): The set of alternative parameter sets is pruned by removing the worst performing half of the sets after each step. The set of investigated parameter sets gets progressively smaller.

    -   `stochastic_reject`: The set of alternative parameter sets is pruned by comparing the performance of each parameter set with that of the incumbent *best* parameter set using a paired Wilcoxon test. This was the previous default.

    -   `none`: The set of alternative parameter sets is not pruned.

-   We can now also change the hyperparameter learner used to infer suitability of candidate hyperparameter sets for further exploration. The learner can be set using the `hyperparameter_learner` argument, and supports the following options:

    -   `gaussian_process` (default): Creates a localised approximate Gaussian deterministic Gaussian Processes.

    -   `bayesian_additive_regression_trees` or `bart`: Uses Bayesian Additive Regression Trees for inference. Unlike standard random forests, BART allows for estimating posterior distributions directly and can extrapolate.

    -   `random_forest`: Creates a random forest for inference. A weakness of random forests is their lack of extrapolation beyond observed values, which limits their usefulness in exploiting promising areas of hyperparameter space. This was the previously supported option.

    -   `random` or `random_search`: Forgoes the use of models to steer optimisation. Instead, a random search is performed. This means the hyperparameter space is sampled at random.

-   Three new vignettes have been added. The first vignette is an introductory vignette on how to get started with familiar. The second vignette describes how familiar can be used prospectively. The third vignette describes evaluation and explanation steps in familiar and how they are implemented. All other vignettes have been reviewed and updated.

-   A `predict` method was added to allow for direct inference of estimated values for one or more instances. Functionality is the same as other `predict` methods, but data (`newdata`) should always be provided. Familiar does not store development data with the model.

-   The RServe backend was retired, as recent versions of the RServe and RSclient packages produced errors that could not be resolved.

## Minor changes

-   A `sample_limit` parameter was added to limit the number of samples used during evaluation. This parameter can be specified for the `sample_similarity` evaluation step.

-   Class levels for categorical outcomes, if not explicitly specified using the `class_levels` parameter, are now sorted before being set based on the data. Previously class levels would be set based on order of appearance.

-   Added `mean`, `mean_trim` and `mean_winsor` as methods to set risk-group stratification thresholds.

-   Novelty detector algorithms are now implemented as S4 classes. This was primarily done to make it easier to add additional methods.

-   `novelty_detector` and `detector_parameters` configuration parameters were added.

-   Isolation forests are now grown with a decreased memory footprint.

-   Presence of instances with a survival time of 0 or lower will produce a warning. Though familiar itself will handle such instances just fine, other packages will produce errors. Since familiar captures such errors and handles them internally, a warning is provided to indicate potential issues.

-   The hyperparameter optimisation algorithm performs an improved search of the local neighbourhood of good hyperparameter sets. Previously large parts of local neighbourhoods were ignored as their utility may not have exceeded that of the seed set. The algorithm is now no longer myopic. Instead, the seed set is used as starting point for exploration, and a random path through hyperparameter space is charted. The most promising hyperparameter sets are chosen after repeating this procedure several times and for several seed points.

-   The `smbo_intensify_stop_p_value` parameter was renamed to `smbo_stochastic_reject_p_value`.

-   The `eval_times` argument used in plot and export methods was renamed to `evaluation_times` to match that of the synonymous configuration parameter.

-   Receiver operating characteristic and precision-recall curves are now plotted exactly in circumstances that allow for it. Previously an interpolated version of the curves was always shown.

-   The default divergent palettes used for feature cluster and sample cluster heatmaps now diverge to white instead of black.

-   Familiar will now actively check whether packages are installed.

-   The default number of cores used for parallel processing is now 2 by default, instead of all cores - 1. This can be changed by setting the `parallel_nr_cores` parameter.

## Bug fixes

-   `detail_level`, `sample_limit`, `estimation_type` and `aggregate_results` arguments used during evaluation and explanation are now propagated from `familiarModel` and `familiarEnsemble` objects instead of reverting to default values.

-   Fixed a bug that caused features not incorporated in models to not be exported for the purpose of reporting (aggregate) variable importance.

-   Fixed a bug that caused clustered features not to be exported for the purpose of reporting (aggregate) variable importance.

-   Fixed a bug that caused model-based variable importance to be exported when calling the `export_fs_vimp` method with any `object` that is not a `familiarCollection` object.

-   Fixed a bug that prevented rank aggregation method and thresholds from being set while exporting variable importance using `export_fs_vimp` or `export_model_vimp`.

-   Fixed an error that occurred when attempting to fit calibration data with `NA` values.

-   Fixed an error that occurred when attempting to interpolate survival probabilities when collecting pooled data.

-   Fixed an issue with models from the `glmnet` package not training for rare classes / events or censoring in small datasets.

-   Fixed an issue that could cause main panels in composite plots that consist of one row of facets and have a legend guide to be have the same height as the guide.

-   Fixed two issues that could cause errors when plotting a single survival curve.

-   Points on receiver operating characteristic and precision-recall curves are now always correctly ordered.

-   Fixed a bug that would prevent `hpo_metric`, `vimp_aggregation_method` and `vimp_aggregation_rank_threshold` arguments from being set as a function argument.

-   Fixed an error when trying to pass undeclared arguments to the `..train` method of `familiarModel` objects using `callNextMethod()`.

# Version 0.0.0.54 (Pre-release)

## Major changes:

-   Added novelty detection. An isolation forest is created at the same time as the main model using the same data. It can then be used to (prospectively) identify samples that are dissimilar to the training samples, and for which the model may need to extrapolate.

    -   Added a `novelty_features` parameter that can be used to specify features that should be used for novelty detection, in addition to those already used in the model.

-   Added `update_object` methods that allow for backward compatibility when updating slots of respective objects.

-   Added support for series-like data. These can be time series, or multiple measurements were the outcome of interest may change. Subsampling, e.g. through cross-validation or bootstraps still respects samples. This means that different series instances of the same sample are always kept together for subsampling. The series column in the data set can be set using the `series_id_column` parameter. This required changes to what iteration data is stored. This should not cause any issues with post-hoc analyses, but is not **not backward compatible** when updating familiar prior to completing the modelling and evaluation process.

-   Improved flexibility of the evaluation process that is conducted to explain and assess models:

    -   Added `dynamic_model_loading` parameter that supports dynamic loading of models to an ensemble. This reduces the memory footprint at the cost of IO overhead as the models are read from the disk or network when required. By default, all models are statically attached to an ensemble.

    -   Added `skip_evaluation_elements` parameter that allows skipping one or more steps of the evaluation process. This is useful if some evaluations are not relevant.

    -   Updated `parallel_hyperparameter_optimisation` and `parallel_evaluation` to allow for specifying whether parallelisation should take place inside (`inner`) or outside (`outer`) the respective processes. For `outer` the parallelisation takes place over different subsamples, learners, etc. This may provide an increase in processing speed, at the cost of less feedback and a higher memory footprint.

    -   All evaluation steps now produce `familiarDataElement` elements. This is not **not backward compatible**. `familiarData` and `familiarCollection` objects created using previous versions need to be created anew. enables flexibility in terms of how data and models are handled for computation. Specifically, many evaluation steps that focus on models can be evaluated at the ensemble (`ensemble`), model (`model`) or an intermediate level (`hybrid`) by setting the `detail_level` parameter. The `hybrid` level differs from `ensemble` in that the individual model predictions are used instead of the prediction of the ensemble itself. Likewise, the type of estimation can now be flexibly chosen for several evaluation steps by setting the `estimation_type` parameter.

    -   Several parameters are now deprecated:

        -   `compute_model_data` has been completely deprecated. This can now be specified using the new `detail_level` parameter.

        -   `compute_model_ci` has been completely deprecated. This can now be specified using the new `estimation_type` parameter.

        -   `compute_ensemble_ci` has been completely deprecated. This can now be specified using the new `estimation_type` parameter.

        -   `aggregate_ci` has been replaced by `aggregate_results`. Aside from bootstrap confidence intervals, underlying results for bias-corrected estimates can now be aggregated.

-   Calibration plots have been completely revamped to now include confidence intervals. Moreover, calibration plots based on `bootstrap_confidence_interval` and `bias_corrected` estimation types no longer show points, but are based on interpolation to a regular grid after computing a loess model. `point` estimates are unaltered. Density plots have also been revised to use a fixed standard deviation of 0.075, which prevents some of the erratic behaviour seen previously when most expected probability values were clustered closely.

-   Clustering based on feature similarity during the evaluation process can now be specified after similarity has been computed, i.e. through `export_feature_similarity` and `plot_feature_similarity`. This allows for changing clustering parameters after the analysis.

-   Parallel processing now supports mini-batching. Many processes are actually fast to compute and repeated IO to cluster nodes noticably slows down the process. Mini-batches transfers data to nodes in one go for local sequential processing. In addition, processes that can mini-batch are now measured, and an optimal number of nodes is selected based on IO and process times. This should significantly speed up processes with low process time compared to IO time.

-   Hyperparameter optimisation now predicts the time taken for training using a specific set of hyperparameters. It uses the predicted time to optimise assignment to nodes for parallel processing. This eliminates an issue where hyperparameter optimisation with parallel nodes could take significantly longer than simple sequential optimisation. In addition, hyperparameter optimisation now has several new or changed configuration parameters:

    -   `smbo_random_initialisation` is no longer a logical (`TRUE` or `FALSE`) but takes `fixed_subsample`, `fixed` or `random` as values. `fixed_subsample` generates initial hyperparameters from the same default hyperparameter grid as `fixed`, but unlike `fixed` does not exhaustively search all options. `random` creates random sets of initial hyperparameters.

    -   `smbo_n_random_sets` can now be used to set the number of hyperparameters sets created for the `fixed_subsample` and `random` methods.

-   `kernlab` is no longer used as backend for computing support vector machine learners due to lack of stability. Unit testing showed consistent freezing. We now use the SVM of `e1071` which we found to be stable. SVM models created using previous versions of familiar are no longer compatible.

## Minor changes:

-   The default method for bootstrap confidence intervals (`bootstrap_ci_method`) is now the percentile (`percentile`) method, which replaces the bias-corrected (`bc`) method.

-   The value returned for the bias-corrected bootstrap confidence interval method is now the bias-corrected median, not the point estimate. This harmonises the behaviour of the percentile and bias-corrected confidence interval methods. The bias-corrected median can be viewed as an optimism correction of the point estimate.

-   Several attribute slots for S4 `familiarModel`, `familiarEnsemble`, `familiarData` and `familiarCollection` objects were removed, revised or added. Changes are backward compatible due to the new `update_object` method.

-   Lambda parameters of Box-Cox and Yeo-Johnson transformations are now determined using `stats::optimise`. The previous, fixed, settings were sensible for Box-Cox, but the Yeo-Johnson method benefits from a wider selection. This does not affect backward compatibility.

-   The `as_data_object` method can now be used with `familiarModel` and `familiarEnsemble` objects to check whether the input data can be correctly formatted, and will provide meaningful errors if not.

-   Added `show` methods for objects that are typically written to drive, such as `familiarModel`, `familiarEnsemble`, `familiarData` and `familiarCollection` objects.

-   Added `plot_auc_precision_recall_curve` method to plot precision-recall curves.

-   Random forests created using the `rfsrc` package are now anonymous forests, i.e. training data are not stored with the model. In addition, we explicitly generate and store a random seed, so that the forest can be regrown for determining variable importance.

## Bug fixes:

-   Fixed an error that would cause hyperparameter optimisation to not select the optimal set of hyperparameters.

-   Fixed an error that would cause feature selection to fail when all features in the data are also set to be in the signature.

-   Fixed an error that occurred when attempting to create risk groups from models that were not successfully trained.

-   Fixed an error in ComBat batch normalisation caused by invariant or NA features in one or more batches.

-   Fixed a bug that would incorrectly assign samples to wrong subsamples (e.g. in-bag or out-of-bag data). This only occurred if the same sample identifier exists in different batches.

-   Fixed an error that occurred prior to hyperparameter optimisation because a model-dependent hyperparameter required to create a metric object may not have been set.

-   Fixed an error when attempting to perform parallel processing with familiar installed on a non-standard library path.

-   Fixed an issue where the `verbose` argument was not respected when forming clusters of features.

-   Fixed an issue where absence of censoring for time-to-event data would lead to models not being created.

-   Fixed an issue where NA would not be removed from the results in the `extract_from_slot` function.

-   Fixed an issue where information would be missing in `familiarEnsemble` objects because the first `familiarModel` in the ensemble was not trained. This information cannot be added retroactively.

-   Fixed an issue that would cause `export_permutation_vimp` to export the wrong data when called by the user.

-   Fixed an issue that would cause an error when a decision curve was plotted with a confidence interval but without requiring multiple line colours.

-   Fixed an issue that would cause an error when a receiver operating characteristic curve was plotted with a confidence interval but without requiring multiple line colours.

-   Fixed an error that occurred when categorical outcome levels are numeric, e.g. 0 and 1. For some learners, such as `svm`, this caused an indexing error as the outcome levels were interpreted as indices instead of column names.

-   Fixed an error during batch normalisation when there is a mismatch between features with feature information and actual features present in the dataset.

-   Fixed an issue that would cause an error when computing a pseudo-R2 similarity score between two arrays that each have one unique value.

# Version 0.0.0.53 (Pre-release)

## Major changes:

-   All metrics are now implemented as S4 objects, with associated methods. Moreover all metrics now have unit tests.

-   All plotting algorithms now have unit tests which should increase stability of the code. Resulting code fixes are **not backward compatible**: you may need to recreate the `familiarData` objects for binomial endpoints.

-   Hyperparameter optimisation now has additional parameters:

    -   `optimisation_determine_vimp`: Allows for determining variable importance for each of the bootstraps used during hyperparameter optimisation to avoid positive biases.
    -   `optimisation_function`: replaces the objective parameter.
    -   `smbo_stop_tolerance`: tolerance for a optimisation score to be convergent.
    -   `acquisition_function`: an acquisition function can now be selected.

-   Hyperparameter optimisation can now be performed using multiple optimisation metrics instead of one.

## Minor changes:

-   Data computation for individual models can now be explicitly set using the `compute_model_data` parameter.

-   Many bugs were fixed.
