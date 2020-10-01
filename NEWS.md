# Version 0.0.0.53 (Pre-release)

## Major changes:
* All metrics are now implemented as S4 objects, with associated methods. Moreover all metrics now have unit tests.
* All plotting algorithms now have unit tests which should increase stability of the code. Resulting code fixes are **not backward compatible**: you may need to recreate the familiarData objects for binomial endpoints.
* Hyperparameter optimisation now has additional parameters:
    * optimisation_determine_vimp: Allows for determining variable importance for each of the bootstraps used during hyperparameter optimisation to avoid positive biases.
    * optimisation_function: replaces the objective parameter.
    * smbo_stop_tolerance: tolerance for a optimisation score to be convergent.
    * acquisition_function: an acquisition function can now be selected.
* Hyperparameter optimisation can now be performed using multiple optimisation metrics instead of one.

## Minor changes:
* Data computation for individual models can now be explicitly set using the compute_model_data parameter.
* Many bugs were fixed.