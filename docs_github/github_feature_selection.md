Feature selection methods
================
Alex Zwanenburg
2022-01-28

<img src="../vignettes/familiar.svg" align="right" width="120"/>

-   [Configuration options](#configuration-options)
-   [Providing parameters for feature
    selection](#providing-parameters-for-feature-selection)
-   [Overview of feature selection
    methods](#overview-of-feature-selection-methods)
    -   [Correlation methods](#correlation-methods)
    -   [Concordance methods](#concordance-methods)
    -   [CORElearn methods](#corelearn-methods)
    -   [Mutual information-based
        methods](#mutual-information-based-methods)
        -   [Mutual information
            maximisation](#mutual-information-maximisation)
        -   [Mutual information feature
            selection](#mutual-information-feature-selection)
        -   [Minimum redundancy maximum
            relevance](#minimum-redundancy-maximum-relevance)
    -   [Univariate and multivariate regression
        methods](#univariate-and-multivariate-regression-methods)
        -   [Univariate regression](#univariate-regression)
        -   [Multivariate regression](#multivariate-regression)
    -   [Lasso, ridge and elastic net
        regression](#lasso-ridge-and-elastic-net-regression)
    -   [Random forest-based methods](#random-forest-based-methods)
        -   [Permutation importance](#permutation-importance)
        -   [Holdout permutation
            importance](#holdout-permutation-importance)
        -   [Minimum depth variable
            selection](#minimum-depth-variable-selection)
        -   [Variable hunting](#variable-hunting)
        -   [Impurity importance](#impurity-importance)
    -   [Special methods](#special-methods)
        -   [No feature selection](#no-feature-selection)
        -   [Random feature selection](#random-feature-selection)
        -   [Signature only](#signature-only)
-   [Aggregating variable importance](#aggregating-variable-importance)
    -   [Notation](#notation)
    -   [No rank aggregation](#no-rank-aggregation)
    -   [Mean rank aggregation](#mean-rank-aggregation)
    -   [Median rank aggregation](#median-rank-aggregation)
    -   [Best rank aggregation](#best-rank-aggregation)
    -   [Worst rank aggregation](#worst-rank-aggregation)
    -   [Stability rank aggregation](#stability-rank-aggregation)
    -   [Exponential rank aggregation](#exponential-rank-aggregation)
    -   [Borda rank aggregation](#borda-rank-aggregation)
    -   [Enhanced borda rank
        aggregation](#enhanced-borda-rank-aggregation)
    -   [Truncated borda rank
        aggregation](#truncated-borda-rank-aggregation)
    -   [Truncated enhanced borda rank
        aggregation](#truncated-enhanced-borda-rank-aggregation)
-   [References](#references)

Feature selection methods in familiar measure variable importance in a
univariate or multivariate setting.

| **method**                                           | **tag**                                    | **binomial** | **multinomial** | **continuous** | **count** | **survival** |
|:-----------------------------------------------------|:-------------------------------------------|:------------:|:---------------:|:--------------:|:---------:|:------------:|
| **correlation**                                      |                                            |              |                 |                |           |              |
| Pearson’s *r*                                        | `pearson`                                  |              |                 |       ×        |     ×     |      ×       |
| Spearman’s *ρ*                                       | `spearman`                                 |              |                 |       ×        |     ×     |      ×       |
| Kendall’s *τ*                                        | `kendall`                                  |              |                 |       ×        |     ×     |      ×       |
| **concordance**                                      |                                            |              |                 |                |           |              |
| concordance<sup>a</sup>                              | `concordance`                              |      ×       |        ×        |       ×        |     ×     |      ×       |
| **CORElearn**                                        |                                            |              |                 |                |           |              |
| information gain ratio                               | `gain_ratio`                               |      ×       |        ×        |                |           |              |
| gini-index                                           | `gini`                                     |      ×       |        ×        |                |           |              |
| minimum description length                           | `mdl`                                      |      ×       |        ×        |                |           |              |
| ReliefF with exponential weighting of distance ranks | `relieff_exp_rank`                         |      ×       |        ×        |       ×        |     ×     |              |
| **mutual information**                               |                                            |              |                 |                |           |              |
| mutual information maximisation                      | `mim`                                      |      ×       |        ×        |       ×        |     ×     |      ×       |
| mutual information features selection                | `mifs`                                     |      ×       |        ×        |       ×        |     ×     |      ×       |
| minimum redundancy maximum relevance                 | `mrmr`                                     |      ×       |        ×        |       ×        |     ×     |      ×       |
| **univariate regression**                            |                                            |              |                 |                |           |              |
| univariate regression                                | `univariate_regression`                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| **multivariate regression**                          |                                            |              |                 |                |           |              |
| multivariate regression                              | `multivariate_regression`                  |      ×       |        ×        |       ×        |     ×     |      ×       |
| **lasso regression**                                 |                                            |              |                 |                |           |              |
| general<sup>a</sup>                                  | `lasso`                                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                             | `lasso_binomial`                           |      ×       |                 |                |           |              |
| multi-logistic                                       | `lasso_multinomial`                        |              |        ×        |                |           |              |
| normal (gaussian)                                    | `lasso_gaussian`                           |              |                 |       ×        |           |              |
| poisson                                              | `lasso_poisson`                            |              |                 |                |     ×     |              |
| cox                                                  | `lasso_cox`                                |              |                 |                |           |      ×       |
| **ridge regression**                                 |                                            |              |                 |                |           |              |
| general<sup>a</sup>                                  | `ridge`                                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                             | `ridge_binomial`                           |      ×       |                 |                |           |              |
| multi-logistic                                       | `ridge_multinomial`                        |              |        ×        |                |           |              |
| normal (gaussian)                                    | `ridge_gaussian`                           |              |                 |       ×        |           |              |
| poisson                                              | `ridge_poisson`                            |              |                 |                |     ×     |              |
| cox                                                  | `ridge_cox`                                |              |                 |                |           |      ×       |
| **elastic net regression**                           |                                            |              |                 |                |           |              |
| general<sup>a,b</sup>                                | `elastic_net`                              |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic<sup>b</sup>                                 | `elastic_net_binomial`                     |      ×       |                 |                |           |              |
| multi-logistic<sup>b</sup>                           | `elastic_net_multinomial`                  |              |        ×        |                |           |              |
| normal (gaussian)<sup>b</sup>                        | `elastic_net_gaussian`                     |              |                 |       ×        |           |              |
| poisson<sup>b</sup>                                  | `elastic_net_poisson`                      |              |                 |                |     ×     |              |
| cox<sup>b</sup>                                      | `elastic_net_cox`                          |              |                 |                |           |      ×       |
| **random forest (RFSRC) variable importance**        |                                            |              |                 |                |           |              |
| permutation<sup>b</sup>                              | `random_forest_permutation`                |      ×       |        ×        |       ×        |     ×     |      ×       |
| minimum depth<sup>b</sup>                            | `random_forest_minimum_depth`              |      ×       |        ×        |       ×        |     ×     |      ×       |
| variable hunting<sup>b</sup>                         | `random_forest_variable_hunting`           |      ×       |        ×        |       ×        |     ×     |      ×       |
| hold-out <sup>b</sup>                                | `random_forest_holdout`                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| **random forest (ranger) variable importance**       |                                            |              |                 |                |           |              |
| permutation<sup>b</sup>                              | `random_forest_ranger_permutation`         |      ×       |        ×        |       ×        |     ×     |      ×       |
| hold-out permutation<sup>b</sup>                     | `random_forest_ranger_holdout_permutation` |      ×       |        ×        |       ×        |     ×     |      ×       |
| impurity<sup>b</sup>                                 | `random_forest_ranger_impurity`            |      ×       |        ×        |       ×        |     ×     |      x       |
| **special methods**                                  |                                            |              |                 |                |           |              |
| no selection                                         | `none`                                     |      ×       |        ×        |       ×        |     ×     |      ×       |
| random selection                                     | `random`                                   |      ×       |        ×        |       ×        |     ×     |      ×       |
| signature only                                       | `signature_only`                           |      ×       |        ×        |       ×        |     ×     |      ×       |

Overview of feature selection methods. <sup>a</sup> This is a general
method where an appropriate specific method will be chosen, or multiple
distributions or linking families are tested in an attempt to find the
best option. <sup>b</sup> This method requires hyperparameter
optimisation.

## Configuration options

Feature selection methods and related options can be provided within the
`feature_selection` tag in the *xml* file or as function argument.

|      **tag** / **argument**       | **description**                                                                                                                                                                                      | **default**  |
|:---------------------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------:|
|            `fs_method`            | The desired feature selection method. Multiple selection methods may be provided at the same time. This setting has no default and must be provided.                                                 | – (required) |
|       `fs_method_parameter`       | Several feature selection methods have hyperparameters that can be set and/or optimised.                                                                                                             | – (optional) |
|     `vimp_aggregation_method`     | The aggregation method used to aggregate feature ranks over different bootstraps.                                                                                                                    |   `borda`    |
| `vimp_aggregation_rank_threshold` | Several aggregation methods count features if they have a rank below the threshold, i.e. are among the most important features. If `NULL`, a dynamic threshold is decided through Otsu-thresholding. |    `NULL`    |
|   `parallel_feature_selection`    | Enables parallel processing for feature selection. Ignored if `parallel=FALSE`.                                                                                                                      |    `TRUE`    |

## Providing parameters for feature selection

Some of the feature selection methods, notably those based on random
forests and (penalised) regression, have parameters that can be set.
These parameters are mentioned under the respective entries in the
[Overview of feature selection
methods](#overview-of-feature-selection-methods) section. Moreover, some
of these parameters are model parameters. In this case, these parameters
are optimised using hyperparameter optimisation, which is described in
the *learning algorithms and hyperparameter optimisation* vignette.

The syntax for such parameters is the same as for hyperparameter
optimisation. For the `multivariate_regression` feature selection method
the `alpha` parameter (which determines feature drop-out during forward
selection) may be provided as follows using the configuration file:

    <fs_method_parameter>
      <multivariate_regression>
        <alpha>0.05</alpha>
      </multivariate_regression>
    </fs_method_parameter>

Or as a nested list passed as the `fs_method_parameter` argument to
`summon_familiar`:

    fs_method_parameter = list("multivariate_regression"=list("alpha"=0.05))

# Overview of feature selection methods

The feature selection methods implemented in familiar are described in
more detail in this section.

## Correlation methods

Correlation methods determine variable importance by assessing the
correlation between a feature and the outcome of interest. High
(anti-)correlation indicates an important feature, whereas low
(anti-)correlation indicates that a feature is not directly related to
the outcome. Correlation-based variable importance is determined using
the `cor` function of the `stats` package that is part of the R core
distribution (R Core Team 2019).

Three correlation coefficients can be computed:

-   `pearson`: Pearson’s *r*
-   `spearman`: Spearman’s *ρ*
-   `kendall`: Kendall’s *τ*

To compute correlation of features with survival outcomes, only samples
with an event are considered.

## Concordance methods

Concordance methods assess how well the ordering of feature values
corresponds to the ordering of the outcome. The method internally refers
to the `gini` method for binomial and multinomial outcomes and to the
`kendall` method for continuous and count outcomes. For survival
outcomes, concordance is measured using the `concordance_index`.

## CORElearn methods

Familiar provides an interface to several feature selection methods
implemented in the `CORElearn` package. These methods are the
Information Gain Ratio (`gain_ratio`), the Gini-index (`gini`), Minimum
Description Length (`mdl`) and ReliefF and rReliefF with exponential
distance rank weighting (`relieff_exp_rank`).

## Mutual information-based methods

Mutual information *I* is a measure of interdependency between two
variables *x* and *y*. In the context of feature selection, *x* is a
feature vector and *y* is the outcome vector.

Computing mutual information requires that probability distributions of
*x* and *y* are known. In practice we don’t know either one. For
categorical *x* and *y* we can use the sample estimates instead. For
continuous or mixed data, the situation is more complex.

In familiar we therefore use the following three approaches to compute
mutual information:

1.  For binomial and multinomial outcomes mutual information is computed
    using sample estimates. In case of continuous *x*, these are
    discretised into ⌈2*n*<sup>1/3</sup>⌉ bins, with *n* the number of
    samples, after which computation is conducted as if *x* was a
    categorical variable.

2.  For continuous and count outcomes, we use the approximation proposed
    by De Jay et al. after Gel’fand and Yaglom (Gel′fand and Yaglom
    1959; De Jay et al. 2013):
    *I* =  − 0.5log (1−*ρ*(*x*,*y*)<sup>2</sup>+*ϵ*), with *ρ*(*x*,*y*)
    Spearman’s correlation coefficient and *ϵ* a small positive number
    to prevent log (0).

3.  For survival outcomes the second method is adapted for use with a
    concordance index:
    *I* =  − 0.5log (1−(2\*(*c**i*−0.5))<sup>2</sup>+*ϵ*), with *c**i*
    the concordance index.

We opted to adapt the approach based on the outcome type as this ensures
that a single consistent approach is used to assess all feature data in
an analysis, thus making results comparable.

### Mutual information maximisation

The `mim` method is a univariate method that ranks each feature by its
mutual information with the outcome.

### Mutual information feature selection

Mutual information feature selection (MIFS) finds a feature set that
maximises mutual information (Battiti 1994). This is done using forward
selection. As in mutual information maximisation, mutual information
*I*<sub>*y*, *j*</sub> between each feature and the outcome is computed.
Starting from a potential pool of all features, the feature with the
highest mutual information is selected and removed from the pool.

The rest proceeds iteratively. The mutual information
*I*<sub>*s*, 1*j*</sub> between the previously selected feature and the
remaining features is computed. This mutual information is also called
*redundancy*. The feature with the highest mutual information with the
outcome and least redundancy (i.e. maximum
*I*<sub>*y*, *j*</sub> − *I*<sub>*s*, 1*j*</sub>) is selected next, and
removed from the pool of remaining features. Then the mutual information
*I*<sub>*s*, 2*j*</sub> between this feature and remaining features is
computed, and the feature that maximises
*I*<sub>*y*, *j*</sub> − *I**s*, 1*j* − *I*<sub>*s*, 2*j*</sub> is
selected, and so forth.

The iterative process stops if there is no feature *j* for which
*I*<sub>*y*, *j*</sub> − ∑<sub>*i* ∈ *S*</sub>*I*<sub>*s*, *i**j*</sub> \> 0,
with *S* being the subset of selected features, or all features have
been exhausted.

To reduce the number of required computations, the implementation in
familiar actively filters out any feature *j* for which
*I*<sub>*y*, *j*</sub> − ∑<sub>*i* ∈ *S*</sub>*I*<sub>*s*, *i**j*</sub> ≤ 0
at the earliest instance, as the
∑<sub>*i* ∈ *S*</sub>*I*<sub>*s*, *i**j*</sub> term will monotonously
increase.

### Minimum redundancy maximum relevance

Minimum redundancy maximum relevance (mRMR) feature selection is similar
to MIFS but differs in the way redundancy is used during optimisation
(Peng, Long, and Ding 2005). Whereas for MIFS the optimisation criterion
is
*I*<sub>*y*, *j*</sub> − ∑<sub>*i* ∈ *S*</sub>*I*<sub>*s*, *i**j*</sub>,
in mRMR the optimisation criterion is
$I\_{y,j} - \\frac{1} {\\left\| S \\right\|} \\sum\_{i\\in S} I\_{s,ij}$,
with \|*S*\| the number of features already selected.

Unlike in MIFS, the
$\\frac{1}{\\left\|S\\right\|}\\sum\_{i\\in S}I\_{s,ij}$ term is not
monotonically increasing. Consequently, features cannot be safely
filtered. To limit computational complexity, we still remove features
for which
$I\_{y,j} - \\frac{1} {\\left\| S \\right\| + 3} \\sum\_{i\\in S} I\_{s,ij} \\leq 0$,
as such features are unlikely to be selected.

## Univariate and multivariate regression methods

Univariate and multivariate regression perform feature selection by
performing regression using a feature or set of features as predictors.
The performance of the regression model is then measured using a metric.
Training and testing of regression models are repeated multiple times
using bootstraps. For each bootstrap, the in-bag samples are used for
training and the out-of-bag samples are using for testing.

This also defines the parameters of both methods, which are shown in the
table below.

| **parameter**        | **tag**       |      **values**      | **optimised** | **comments**                                                                                                                                                                                                                                                                                                 |
|:---------------------|:--------------|:--------------------:|:-------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| regression learner   | `learner`     | dependent on outcome |      no       | Any generalised linear regression model from the *learning algorithms and hyperparameter optimisation* vignette can be selected. Default values are `glm_logistic` for binomial, `glm_multinomial` for multinomial, `glm_gaussian` for continuous, `glm_poisson` for count, and `cox` for survival outcomes. |
| performance metric   | `metric`      | dependent on outcome |      no       | Any metric from the *performance metrics* vignette can be selected. Default values are `auc_roc` for binomial and multinomial, `mse` for continuous, `msle` for count and `concordance_index` for survival outcomes                                                                                          |
| number of bootstraps | `n_bootstrap` |      ℤ ∈ \[1,∞)      |      no       | The default value is 10.                                                                                                                                                                                                                                                                                     |
| drop-out alpha level | `alpha`       |     ℝ ∈ \[0,1\]      |      no       | The default value is 0.05. Only used in multivariate regression.                                                                                                                                                                                                                                             |

### Univariate regression

In the univariate regression method, a regression model is built with
each feature separately using the in-bag data of the bootstrap. Then
this model is evaluated using the metric, expressed using an objective
representation (see *computing the objective score* in the *learning
algorithms and hyperparameter optimisation* vignette). The objective
representation *s*<sup>\*</sup> is computed on both in-bag (IB) and
out-of-bag (OOB) data. Subsequently the `balanced` objective score *f*
is computed:
*f* = *s*<sub>*O**O**B*</sub><sup>\*</sup> − \|*s*<sub>*O**O**B*</sub><sup>\*</sup>−*s*<sub>*I**B*</sub><sup>\*</sup>\|.

The objective score *f* is subsequently averaged over all bootstraps to
obtain the variable importance of a feature.

### Multivariate regression

The procedure described for univariate regression forms the first step
in multivariate regression. The rest follows forward selection. The most
important feature is assigned to the subset of selected features and
removed from the set of available features. Separate regression models
are then built with each remaining feature and all the feature(s) in the
selected feature subset as predictors. Thus, the subset of selected
features iteratively increases in size until no features are remaining
or the objective score no longer increases.

To limit mostly redundant computation, features that are unlikely to be
selected are actively removed. To do so, the standard deviation of the
objective score over the bootstraps is computed for each feature. The
(one-sided, upper-tail) quantile *q* corresponding to the alpha-level
indicated by parameter `alpha` is subsequently computed. If the obtained
mean objective score is *q* standard deviations or more below the best
objective score, the feature is removed.

## Lasso, ridge and elastic net regression

Penalised regression is also a form of feature selection, as it selects
an ‘optimal’ set of features to create a regression model. As features
are usually normalised as part of pre-processing, the magnitude of each
coefficient can be interpreted as its importance. All three shrinkage
methods are implemented using the `glmnet` package (Hastie, Tibshirani,
and Friedman 2009; Simon et al. 2011).

Only elastic net regression has a model hyperparameter that requires
optimisation, but other parameters may be set as well, as shown in the
table below:

| **parameter**       | **tag**      |                       **values**                        |    **optimised**    | **comments**                                                                                                                                                                                   |
|:--------------------|:-------------|:-------------------------------------------------------:|:-------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| family              | `family`     | `gaussian`, `binomial`, `poisson`, `multinomial`, `cox` | continuous outcomes | For continuous outcomes `gaussian` and `poisson` may be tested. The family is not optimised when it is specified, e.g. `lasso_gaussian`. For other outcomes only one applicable family exists. |
| elastic net penalty | `alpha`      |                       ℝ ∈ \[0,1\]                       |     elastic net     | This penalty is fixed for ridge regression (`alpha = 0`) and lasso (`alpha = 1`).                                                                                                              |
| optimal lambda      | `lambda_min` |               `lambda.1se`, `lambda.min`                |         no          | Default is `lambda.min`.                                                                                                                                                                       |
| number of CV folds  | `n_folds`    |                      ℤ ∈ \[3,*n*\]                      |         no          | Default is 3 if *n* \< 30, ⌊*n*/10⌋ if 30 ≤ *n* ≤ 200 and 20 if *n* \> 200.                                                                                                                    |
| normalisation       | `normalise`  |                     `FALSE`, `TRUE`                     |         no          | Default is `FALSE`, as normalisation is part of pre-processing in familiar.                                                                                                                    |

## Random forest-based methods

Several feature selection methods are based on random forests. All these
methods require that a random forest model exists. Hence, `familiar`
will train a random forest based on the training data. Random forest
learners have a set of hyperparameters that are optimised prior to
training, and these make up most of the method-specific parameters.
These parameters, which are slightly different for `ranger`-based and
`randomForestSRC`-based methods, are shown below.

| **parameter**                                | **tag**           |                                                   **values**                                                   | **optimised** | **comments**                                                                                                                                                                             |
|:---------------------------------------------|:------------------|:--------------------------------------------------------------------------------------------------------------:|:-------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| number of trees                              | `n_tree`          |                                                   ℤ ∈ \[0,∞)                                                   |      yes      | This parameter is expressed on the log<sub>2</sub> scale, i.e. the actual input value will be 2<sup>`n_tree`</sup> (Oshiro, Perez, and Baranauskas 2012). The default range is \[4,10\]. |
| subsampling fraction                         | `sample_size`     |                                                  ℝ ∈ (0,1.0\]                                                  |      yes      | Fraction of available data that is used for to create a single tree. The default range is \[2/*m*,1.0\], with *m* the number of samples.                                                 |
| number of features at each node              | `m_try`           |                                                ℝ ∈ \[0.0,1.0\]                                                 |      yes      | Familiar ensures that there is always at least one candidate feature.                                                                                                                    |
| node size                                    | `node_size`       |                                                   ℤ ∈ \[1,∞)                                                   |      yes      | Minimum number of unique samples in terminal nodes. The default range is \[5,⌊*m*/3⌋\], with *m* the number of samples.                                                                  |
| maximum tree depth                           | `tree_depth`      |                                                   ℤ ∈ \[1,∞)                                                   |      yes      | Maximum depth to which trees are allowed to grow. The default range is \[1,10\].                                                                                                         |
| number of split points                       | `n_split`         |                                                   ℤ ∈ \[0,∞)                                                   |      no       | By default, splitting is deterministic and has one split point (0).                                                                                                                      |
| splitting rule (`randomForestSRC` only)      | `split_rule`      | `gini`, `auc`, `entropy`, `mse`, `quantile.regr`, `la.quantile.regr`, `logrank`, `logrankscore`, `bs.gradient` |      no       | Default splitting rules are `gini` for `binomial` and `multinonial` outcomes, `mse` for `continuous` and `count` outcomes and `logrank` for `survival` outcomes.                         |
| splitting rule (`ranger` only)               | `split_rule`      |                `gini`, `hellinger`, `extratrees`, `beta`, `variance`, `logrank`, `C`, `maxstat`                |      no       | Default splitting rules are `gini` for `binomial` and `multinomial` outcomes and `maxstat` for `continuous`, `count` and `survival` outcomes.                                            |
| significance split threshold (`ranger` only) | `alpha`           |                                                 ℝ ∈ (0.0,1.0\]                                                 |   `maxstat`   | Minimum significance level for further splitting. The default range is \[10<sup>−6</sup>,1.0\]                                                                                           |
| variable hunting cross-validation folds      | `fs_vh_fold`      |                                                   ℤ ∈ \[2,∞)                                                   |      no       | Number of cross-validation folds for the `random_forest_variable_hunting` method. The default is 5.                                                                                      |
| variable hunting step size                   | `fs_vh_step_size` |                                                   ℤ ∈ \[1,∞)                                                   |      no       | Step size for the `random_forest_variable_hunting` method. The default is 1.                                                                                                             |
| variable hunting iterations                  | `fs_vh_n_rep`     |                                                   ℤ ∈ \[1,∞)                                                   |      no       | Number of Monte Carlo iterations for the `random_forest_variable_hunting` method. The default is 50.                                                                                     |

### Permutation importance

The permutation importance method is implemented by
`random_forest_permutation` (`randomForestSRC` package) and
`random_forest_ranger_permutation` (`ranger` package). In short, this
method functions as follows \[Ishwaran2007-va\]. As usual, each tree in
the random forest is constructed using the in-bag samples of a bootstrap
of the data. The predictive performance of each model is first measured
using the out-of-bag data. Subsequently, the out-of-bag instances for
each feature are randomly permuted, and predictive performance is
assessed again. The difference between the normal performance and the
permuted performance is used as a measure of the variable importance.
For important features, this difference is large, whereas for irrelevant
features the difference is negligible or even negative.

### Holdout permutation importance

This variant on permutation importance
(`random_forest_ranger_holdout_permutation`) is implemented using
`ranger::holdoutRF`. Instead of using out-of-bag to compute feature
importance, two cross-validation folds are used. A random forest is
trained on either fold, and variable importance determined on the other
(Janitza, Celik, and Boulesteix 2018).

The hold-out variable importance method implemented in the
`randomForestSRC` package (`random_forest_holdout`) is implemented using
`randomForestSRC::holdout.vimp`. It is similar to the previous variant,
but does not cross-validation folds. Instead, out-of-bag prediction
errors for models trained with and without each feature are compared.

### Minimum depth variable selection

Important features tend to appear closer to the root of trees in random
forests. Therefore, the position of each feature within a tree is
assessed in minimum depth variable selection (Ishwaran et al. 2010).

### Variable hunting

Variable hunting is implemented using the variable hunting algorithm
implemented in `randomForestSRC`. Ishwaran suggest using it when minimum
depth variable selection leads to high computational load, or a larger
set of variables should be found (Ishwaran et al. 2010).

The variable hunting selection method has several parameters which can
be set.

### Impurity importance

At each node, the data is split into (two) subsets, which connects to
two branches. After splitting, each single subset is purer than the
parent dataset. As a concrete example, in regression problems the
variance of each of the subsets is lower than that of the data prior to
splitting. The decrease in variance specifically, or the decrease of
impurity generally, is then used to assess feature importance.

`familiar` uses the `impurity_corrected` importance measure, which is
unbiased to the number of split points of a feature and its distribution
(Nembrini, König, and Wright 2018).

## Special methods

Familiar offers several methods that are special in that they are not
feature selection methods in the sense that they determine a variable
importance that can be used for establishing feature rankings.

### No feature selection

As the name suggests, the `none` method avoids feature selection
altogether. All features are passed into a model. Feature order is
randomly shuffled prior to building a model to avoid influence of the
provided feature order.

### Random feature selection

The `random` method randomly draws features prior to model building. It
does not assign a random variable importance to a feature. New features
are drawn each time a model is built. All features are available for the
draw, but only *m* features are drawn. Here *m* is the signature size
that is usually optimised by hyperparameter optimisation.

### Signature only

When configuring familiar, any number of features can be set as a model
signature using the `signature` configuration parameter. However, more
features may be added to this signature through feature selection. To
make sure that only the provided features enter a model, the
`signature_only` method may be used.

# Aggregating variable importance

In case of feature selection or modelling in the presence of resampling
(e.g. bootstraps), the ranks of features may need to be aggregated
across the different instances (Wald et al. 2012). The rank aggregation
methods shown in the table below can be used for this purpose. Several
methods require a threshold to indicate the size of the set of most
highly ranked features, which can be set by specifying the
`vimp_aggregation_rank_threshold` configuration parameter.

| **aggregation method**           | **tag**                    |  **comments**  |
|:---------------------------------|:---------------------------|:--------------:|
| none                             | `none`                     |                |
| mean rank                        | `mean`                     |                |
| median rank                      | `median`                   |                |
| best rank                        | `best`                     |                |
| worst rank                       | `worst`                    |                |
| stability selection              | `stability`                | uses threshold |
| exponential selection            | `exponential`              | uses threshold |
| borda ranking                    | `borda`                    |                |
| enhanced borda ranking           | `enhanced_borda`           | uses threshold |
| truncated borda ranking          | `truncated_borda`          | uses threshold |
| enhanced truncated borda ranking | `enhanced_truncated_borda` | uses threshold |

## Notation

Let *N* be the number of ranking experiments that should be aggregated.
Feature *i* for experiment *j* of *N* then has rank
*r*<sub>*i**j*</sub>. A lower rank indicates a more important feature.
Some features may not receive a score during a ranking experiment, for
example for multivariate variable importance methods such as lasso
regression, or by use of a threshold *τ*. This is designated by
*δ*<sub>*i**j*</sub>, which is 0 if the feature is absent, and 1 if it
is present.

In case a threshold is used, *δ*<sub>*i**j*</sub> = 1 if
*r*<sub>*i**j*</sub> ≤ *τ*, and 0 otherwise.

Thus, for each experiment $m_j = \\sum^M\_{i=1} \\delta\_{ij}$ features
are ranked, out of *M* features. *m*<sub>*j*</sub> is then also the
maximum rank found in experiment *j*.

Aggregating ranks for each feature results in an aggregate rank score
*s*<sub>*i*</sub>. Features are subsequently ranked according to this
method-specific score to arrive at an aggregate feature rank
*r*<sub>*i*</sub>.

## No rank aggregation

The `none` option does not aggregate ranks. Rather, scores are
aggregated by computing the average score of a feature over all
experiments that contain it. Ranks are then computed from the aggregated
scores.

## Mean rank aggregation

The mean rank aggregation method ranks features by computing the mean
rank of a feature across all experiments that contain it.

$$s_i = \\frac{\\sum^{N}\_{j=1} \\delta\_{ij} r\_{ij}}{\\sum^{N}\_{j=1} \\delta\_{ij}}$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in ascending order.

## Median rank aggregation

The median rank aggregation method ranks features by computing the
median rank of a feature across all experiments that contain it.

$$s_i = \\underset{j \\in N, \\, \\delta\_{ij}=1}{\\textrm{median}}(r\_{ij})$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in ascending order.

## Best rank aggregation

The best rank aggregation method ranks features by the best rank that a
feature has across all experiments that contain it.

$$s_i =  \\underset{j \\in N, \\, \\delta\_{ij}=1}{\\textrm{min}} (r\_{ij})$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in ascending order.

## Worst rank aggregation

The worst rank aggregation method ranks features by the worst rank that
a feature has across all instances that contain it.

$$s_i =  \\underset{j \\in N, \\, \\delta\_{ij}=1}{\\textrm{max}} (r\_{ij})$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in ascending order.

## Stability rank aggregation

The stability aggregation method ranks features by their occurrence
within the set of highly ranked features across all experiments. Our
implementation generalises the method originally proposed by Meinshausen
and Bühlmann (Meinshausen and Bühlmann 2010).

This method uses threshold *τ* to designate the highly ranked features.
Thus *δ*<sub>*i**j*</sub> = 1 if *r*<sub>*i**j*</sub> ≤ *τ*, and 0
otherwise.

The aggregate rank score is computed as:

$$s_i = \\frac{1}{N} \\sum^N\_{j=1} \\delta\_{ij}$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order, as more commonly occurring
features are considered more important.

## Exponential rank aggregation

The exponential aggregation method ranks features by the sum of the
negative exponentials of their normalised ranks in instances where they
occur within the set of highly ranked features. This method was
originally suggested by Haury et al. (Haury, Gestraud, and Vert 2011).

This method uses threshold *τ* to designate the highly ranked features.
Thus *δ*<sub>*i**j*</sub> = 1 if *r*<sub>*i**j*</sub> ≤ *τ*, and 0
otherwise.

$$s_i = \\sum^N\_{j=1} \\delta\_{ij} \\exp({-r\_{ij} / \\tau)}$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order.

## Borda rank aggregation

Borda rank aggregation ranks a feature by the sum of normalised ranks
(the borda score) across all experiments that contain it. In case every
experiment contains all features, the result is equivalent to the mean
aggregation method (Wald et al. 2012).

$$s_i = \\sum^N\_{j=1} \\frac{m_j - r\_{ij} + 1}{m_j}$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order.

## Enhanced borda rank aggregation

Enhanced borda rank aggregation combines borda rank aggregation with
stability rank aggregation. The borda score is multiplied by the
occurrence of the feature within the set of highly ranked features
across all experiments (Wald et al. 2012).

This method uses threshold *τ* to designate the highly ranked features
for the purpose of computing the occurrence. Thus
*δ*<sub>*i**j*</sub> = 1 if *r*<sub>*i**j*</sub> ≤ *τ*, and 0 otherwise.

$$s_i = \\left( \\frac{1}{N} \\sum^N\_{j=1} \\delta\_{ij} \\right) \\left( \\sum^N\_{j=1}
\\frac{m_j - r\_{ij} + 1}{m_j} \\right)$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order.

## Truncated borda rank aggregation

Truncated borda rank aggregation is borda rank aggregation performed
with only the set of most highly ranked features in each instance.

This method uses threshold *τ* to designate the highly ranked features.
Thus *δ*<sub>*i**j*</sub> = 1 if *r*<sub>*i**j*</sub> ≤ *τ*, and 0
otherwise.

$$s_i = \\sum^N\_{j=1} \\delta\_{ij} \\frac{\\tau - r\_{ij} + 1}{\\tau}$$

Note that compared to the borda method, the number of ranked features in
an experiment *m*<sub>*j*</sub> is replaced by threshold *τ*.

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order.

## Truncated enhanced borda rank aggregation

Truncated enhanced borda rank aggregation is enhanced borda aggregation
performed with only the set of most highly ranked features in each
experiment.

This method uses threshold *τ* to designate the highly ranked features.
Thus *δ*<sub>*i**j*</sub> = 1 if *r*<sub>*i**j*</sub> ≤ *τ*, and 0
otherwise.

$$s_i = \\left( \\frac{1}{N} \\sum^N\_{j=1} \\delta\_{ij} \\right) \\left( \\sum^N\_{j=1}
\\delta\_{ij} \\frac{\\tau - r\_{ij} + 1}{\\tau} \\right)$$

The aggregate rank of features is then determined by sorting aggregate
scores *s*<sub>*i*</sub> in descending order.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Battiti1994-ja" class="csl-entry">

Battiti, R. 1994. “Using Mutual Information for Selecting Features in
Supervised Neural Net Learning.” *IEEE Trans. Neural Netw.* 5 (4):
537–50. <https://doi.org/10.1109/72.298224>.

</div>

<div id="ref-De_Jay2013-yl" class="csl-entry">

De Jay, Nicolas, Simon Papillon-Cavanagh, Catharina Olsen, Nehme
El-Hachem, Gianluca Bontempi, and Benjamin Haibe-Kains. 2013. “<span
class="nocase">mRMRe</span>: An R Package for Parallelized <span
class="nocase">mRMR</span> Ensemble Feature Selection.” *Bioinformatics*
29 (18): 2365–68. <https://doi.org/10.1093/bioinformatics/btt383>.

</div>

<div id="ref-Gelfand1959-de" class="csl-entry">

Gel′fand, I M, and A M Yaglom. 1959. “Calculation of the Amount of
Information about a Random Function Contained in Another Such Function.”
In *Eleven Papers on Analysis, Probability and Topology*, edited by E B
Dynkin, I M Gel’fand, A O Gel’fond, and M A Krasnosel’skii, 12:199–246.
American Mathematical Society Translations: Series 2. Providence, Rhode
Island: American Mathematical Society.
<https://doi.org/10.1090/trans2/012/09>.

</div>

<div id="ref-Hastie2009-ed" class="csl-entry">

Hastie, Trevor, Robert Tibshirani, and Jerome Friedman. 2009. *The
Elements of Statistical Learning: Data Mining, Inference, and
Prediction*. Second Edition. Springer Series in Statistics. New York,
NY, United States: Springer Science+Business Media, LLC.
<https://doi.org/10.1007/978-0-387-84858-7>.

</div>

<div id="ref-Haury2011-zd" class="csl-entry">

Haury, Anne-Claire, Pierre Gestraud, and Jean-Philippe Vert. 2011. “The
Influence of Feature Selection Methods on Accuracy, Stability and
Interpretability of Molecular Signatures.” *PLoS One* 6 (12): e28210.
<https://doi.org/10.1371/journal.pone.0028210>.

</div>

<div id="ref-Ishwaran2010-zv" class="csl-entry">

Ishwaran, Hemant, Udaya B Kogalur, Eiran Z Gorodeski, Andy J Minn, and
Michael S Lauer. 2010. “High-Dimensional Variable Selection for Survival
Data.” *J. Am. Stat. Assoc.* 105 (489): 205–17.
<https://doi.org/10.1198/jasa.2009.tm08622>.

</div>

<div id="ref-Janitza2018-kl" class="csl-entry">

Janitza, Silke, Ender Celik, and Anne-Laure Boulesteix. 2018. “A
Computationally Fast Variable Importance Test for Random Forests for
High-Dimensional Data.” *Adv. Data Anal. Classif.* 12 (4): 885–915.
<https://doi.org/10.1007/s11634-016-0276-4>.

</div>

<div id="ref-Meinshausen2010-do" class="csl-entry">

Meinshausen, Nicolai, and Peter Bühlmann. 2010. “Stability Selection.”
*J. R. Stat. Soc. Series B Stat. Methodol.* 72 (4): 417–73.
<https://doi.org/10.1111/j.1467-9868.2010.00740.x>.

</div>

<div id="ref-Nembrini2018-ay" class="csl-entry">

Nembrini, Stefano, Inke R König, and Marvin N Wright. 2018. “The Revival
of the Gini Importance?” *Bioinformatics* 34 (21): 3711–18.
<https://doi.org/10.1093/bioinformatics/bty373>.

</div>

<div id="ref-Oshiro2012-mq" class="csl-entry">

Oshiro, Thais Mayumi, Pedro Santoro Perez, and José Augusto Baranauskas.
2012. “How Many Trees in a Random Forest?” In *Machine Learning and Data
Mining in Pattern Recognition*, 154–68. Springer Berlin Heidelberg.
<https://doi.org/10.1007/978-3-642-31537-4_13>.

</div>

<div id="ref-Peng2005-oo" class="csl-entry">

Peng, Hanchuan, Fuhui Long, and Chris Ding. 2005. “Feature Selection
Based on Mutual Information: Criteria of Max-Dependency, Max-Relevance,
and Min-Redundancy.” *IEEE Trans. Pattern Anal. Mach. Intell.* 27 (8):
1226–38. <https://doi.org/10.1109/TPAMI.2005.159>.

</div>

<div id="ref-rcore2018" class="csl-entry">

R Core Team. 2019. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Simon2011-ih" class="csl-entry">

Simon, Noah, Jerome Friedman, Trevor Hastie, and Rob Tibshirani. 2011.
“Regularization Paths for Cox’s Proportional Hazards Model via
Coordinate Descent.” *J. Stat. Softw.* 39 (5): 1–13.
<https://doi.org/10.18637/jss.v039.i05>.

</div>

<div id="ref-Wald2012-zk" class="csl-entry">

Wald, R, T M Khoshgoftaar, D Dittman, W Awada, and A Napolitano. 2012.
“An Extensive Comparison of Feature Ranking Aggregation Techniques in
Bioinformatics.” In *2012 IEEE 13th International Conference on
Information Reuse Integration (IRI)*, 377–84.
<https://doi.org/10.1109/IRI.2012.6303034>.

</div>

</div>

<div class="footer">
<br>
<a rel="license" href="https://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="../vignettes/CC4_0_BY_88x31.png" /></a>
This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
Cite as: Alex Zwanenburg. familiar: Vignettes and Documentation (2021). <a href="https://github.com/alexzwanenburg/familiar">https://github.com/alexzwanenburg/familiar</a>
</div>
