Learning algorithms and hyperparameter optimisation
================
Alex Zwanenburg
2022-02-02

<img src="../vignettes/familiar.svg" align="right" width="120"/>

-   [Configuration options](#configuration-options)
-   [Generalised linear models](#generalised-linear-models)
    -   [Linear models for binomial
        outcomes](#linear-models-for-binomial-outcomes)
    -   [Linear models for multinomial
        outcomes](#linear-models-for-multinomial-outcomes)
    -   [Linear models for continuous and count-type
        outcomes](#linear-models-for-continuous-and-count-type-outcomes)
    -   [Linear models for survival
        outcomes](#linear-models-for-survival-outcomes)
-   [Lasso, ridge and elastic net
    regression](#lasso-ridge-and-elastic-net-regression)
-   [Boosted generalised linear models and regression
    trees](#boosted-generalised-linear-models-and-regression-trees)
-   [Extreme gradient boosted linear models and
    trees](#extreme-gradient-boosted-linear-models-and-trees)
-   [Random forest (RFSRC)](#random-forest-rfsrc)
-   [Random forest (ranger)](#random-forest-ranger)
-   [Naive Bayes](#naive-bayes)
-   [*k*-nearest neighbours](#k-nearest-neighbours)
-   [Support vector machines](#support-vector-machines)
-   [Hyperparameter optimization](#hyperparameter-optimization)
    -   [Predicting run time of model](#predicting-run-time-of-model)
    -   [Computing the optimisation
        score](#computing-the-optimisation-score)
    -   [Predicting optimisation score for new hyperparameter
        sets](#predicting-optimisation-score-for-new-hyperparameter-sets)
    -   [Acquisition functions for utility of hyperparameter
        sets](#acquisition-functions-for-utility-of-hyperparameter-sets)
    -   [Exploring challenger sets](#exploring-challenger-sets)
    -   [Providing hyperparameters
        manually](#providing-hyperparameters-manually)
    -   [Configuration options for hyperparameter
        optimisation](#configuration-options-for-hyperparameter-optimisation)
-   [Model recalibration](#model-recalibration)
-   [References](#references)

Learning algorithms create models that relate input data to the outcome.
Development data is used to train models, after which they can be used
to predict outcomes for new data. Familiar implements several commonly
used algorithms. This vignette first describes the learners and their
hyperparameters. Then, hyperparameter optimisation is described in more
detail.

| **learner**                                | **tag**                           | **binomial** | **multinomial** | **continuous** | **count** | **survival** |
|:-------------------------------------------|:----------------------------------|:------------:|:---------------:|:--------------:|:---------:|:------------:|
| **generalised linear models**              |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `glm`                             |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `glm_logistic`                    |      ×       |                 |                |           |              |
| cauchy                                     | `glm_cauchy`                      |      ×       |                 |                |           |              |
| complementary log-log                      | `glm_loglog`                      |      ×       |                 |                |           |              |
| normal                                     | `glm_probit`                      |      ×       |                 |                |           |              |
| multinomial                                | `glm_multinomial`                 |              |        ×        |                |           |              |
| log-normal                                 | `glm_log`, `glm_log_gaussian`     |              |                 |       ×        |           |              |
| normal (gaussian)                          | `glm_gaussian`                    |              |                 |       ×        |           |              |
| inverse gaussian                           | `glm_inv_gaussian`                |              |                 |       ×        |           |              |
| log-poisson                                | `glm_log_poisson`                 |              |                 |                |     ×     |              |
| poisson                                    | `glm_poisson`                     |              |                 |                |     ×     |              |
| cox                                        | `cox`                             |              |                 |                |           |      ×       |
| exponential                                | `survival_regr_exponential`       |              |                 |                |           |      ×       |
| gaussian                                   | `survival_regr_gaussian`          |              |                 |                |           |      ×       |
| logistic                                   | `survival_regr_logistic`          |              |                 |                |           |      ×       |
| log-logistic                               | `survival_regr_loglogistic`       |              |                 |                |           |      ×       |
| log-normal                                 | `survival_regr_lognormal`         |              |                 |                |           |      ×       |
| survival regression<sup>a</sup>            | `survival_regr`                   |              |                 |                |           |      ×       |
| weibull                                    | `survival_regr_weibull`           |              |                 |                |           |      ×       |
| **lasso regression models**                |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `lasso`                           |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `lasso_binomial`                  |      ×       |                 |                |           |              |
| multi-logistic                             | `lasso_multinomial`               |              |        ×        |                |           |              |
| normal (gaussian)                          | `lasso_gaussian`                  |              |                 |       ×        |           |              |
| poisson                                    | `lasso_poisson`                   |              |                 |                |     ×     |              |
| cox                                        | `lasso_cox`                       |              |                 |                |           |      ×       |
| **ridge regression models**                |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `ridge`                           |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `ridge_binomial`                  |      ×       |                 |                |           |              |
| multi-logistic                             | `ridge_multinomial`               |              |        ×        |                |           |              |
| normal (gaussian)                          | `ridge_gaussian`                  |              |                 |       ×        |           |              |
| poisson                                    | `ridge_poisson`                   |              |                 |                |     ×     |              |
| cox                                        | `ridge_cox`                       |              |                 |                |           |      ×       |
| **elastic net regression models**          |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `elastic_net`                     |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `elastic_net_binomial`            |      ×       |                 |                |           |              |
| multi-logistic                             | `elastic_net_multinomial`         |              |        ×        |                |           |              |
| normal (gaussian)                          | `elastic_net_gaussian`            |              |                 |       ×        |           |              |
| poisson                                    | `elastic_net_poisson`             |              |                 |                |     ×     |              |
| cox                                        | `elastic_net_cox`                 |              |                 |                |           |      ×       |
| **boosted linear models**                  |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `boosted_glm`                     |      ×       |                 |       ×        |     ×     |      ×       |
| area under the curve                       | `boosted_glm_auc`                 |      ×       |                 |                |           |              |
| cauchy                                     | `boosted_glm_cauchy`              |      ×       |                 |                |           |              |
| complementary log-log                      | `boosted_glm_loglog`              |      ×       |                 |                |           |              |
| logistic                                   | `boosted_glm_logistic`            |      ×       |                 |                |           |              |
| log-logistic                               | `boosted_glm_log`                 |      ×       |                 |                |           |              |
| normal                                     | `boosted_glm_probit`              |      ×       |                 |                |           |              |
| gaussian                                   | `boosted_glm_gaussian`            |              |                 |       ×        |           |              |
| huber loss                                 | `boosted_glm_huber`               |              |                 |       ×        |           |              |
| laplace                                    | `boosted_glm_laplace`             |              |                 |       ×        |           |              |
| poisson                                    | `boosted_glm_poisson`             |              |                 |                |     ×     |              |
| concordance index                          | `boosted_glm_cindex`              |              |                 |                |           |      ×       |
| cox                                        | `boosted_glm_cox`                 |              |                 |                |           |      ×       |
| log-logistic                               | `boosted_glm_loglog`              |              |                 |                |           |      ×       |
| log-normal                                 | `boosted_glm_lognormal`           |              |                 |                |           |      ×       |
| rank-based estimation                      | `boosted_glm_gehan`               |              |                 |                |           |      ×       |
| survival regression<sup>a</sup>            | `boosted_glm_surv`                |              |                 |                |           |      ×       |
| weibull                                    | `boosted_glm_weibull`             |              |                 |                |           |      ×       |
| **extreme gradient boosted linear models** |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `xgboost_lm`                      |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `xgboost_lm_logistic`             |      ×       |        ×        |       ×        |           |              |
| gamma                                      | `xgboost_lm_gamma`                |              |                 |       ×        |           |              |
| gaussian                                   | `xgboost_lm_gaussian`             |              |                 |       ×        |     x     |              |
| poisson                                    | `xgboost_lm_poisson`              |              |                 |                |     ×     |              |
| cox                                        | `xgboost_lm_cox`                  |              |                 |                |           |      ×       |
| **random forests**                         |                                   |              |                 |                |           |              |
| random forest (RFSRC)                      | `random_forest_rfsrc`             |      ×       |        ×        |       ×        |     ×     |      ×       |
| random forest (ranger)                     | `random_forest_ranger`            |      ×       |        ×        |       ×        |     ×     |      ×       |
| **boosted regression trees**               |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `boosted_tree`                    |      ×       |                 |       ×        |     ×     |      ×       |
| area under the curve                       | `boosted_tree_auc`                |      ×       |                 |                |           |              |
| cauchy                                     | `boosted_tree_cauchy`             |      ×       |                 |                |           |              |
| complementary log-log                      | `boosted_tree_loglog`             |      ×       |                 |                |           |              |
| logistic                                   | `boosted_tree_logistic`           |      ×       |                 |                |           |              |
| log-logistic                               | `boosted_tree_log`                |      ×       |                 |                |           |              |
| normal                                     | `boosted_tree_probit`             |      ×       |                 |                |           |              |
| gaussian                                   | `boosted_tree_gaussian`           |              |                 |       ×        |           |              |
| huber loss                                 | `boosted_tree_huber`              |              |                 |       ×        |           |              |
| laplace                                    | `boosted_tree_laplace`            |              |                 |       ×        |           |              |
| poisson                                    | `boosted_tree_poisson`            |              |                 |                |     ×     |              |
| concordance index                          | `boosted_tree_cindex`             |              |                 |                |           |      ×       |
| cox                                        | `boosted_tree_cox`                |              |                 |                |           |      ×       |
| log-logistic                               | `boosted_tree_loglog`             |              |                 |                |           |      ×       |
| log-normal                                 | `boosted_tree_lognormal`          |              |                 |                |           |      ×       |
| rank-based estimation                      | `boosted_tree_gehan`              |              |                 |                |           |      ×       |
| survival regression<sup>a</sup>            | `boosted_tree_surv`               |              |                 |                |           |      ×       |
| weibull                                    | `boosted_tree_weibull`            |              |                 |                |           |      ×       |
| **extreme gradient boosted trees**         |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `xgboost_tree`                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `xgboost_tree_logistic`           |      ×       |        ×        |       ×        |           |              |
| gamma                                      | `xgboost_tree_gamma`              |              |                 |       ×        |           |              |
| gaussian                                   | `xgboost_tree_gaussian`           |              |                 |       ×        |     x     |              |
| poisson                                    | `xgboost_tree_poisson`            |              |                 |                |     ×     |              |
| cox                                        | `xgboost_tree_cox`                |              |                 |                |           |      ×       |
| **extreme gradient boosted DART trees**    |                                   |              |                 |                |           |              |
| general<sup>a</sup>                        | `xgboost_dart`                    |      ×       |        ×        |       ×        |     ×     |      ×       |
| logistic                                   | `xgboost_dart_logistic`           |      ×       |        ×        |       ×        |           |              |
| gamma                                      | `xgboost_dart_gamma`              |              |                 |       ×        |           |              |
| gaussian                                   | `xgboost_dart_gaussian`           |              |                 |       ×        |     x     |              |
| poisson                                    | `xgboost_dart_poisson`            |              |                 |                |     ×     |              |
| cox                                        | `xgboost_dart_cox`                |              |                 |                |           |      ×       |
| **bayesian models**                        |                                   |              |                 |                |           |              |
| naive bayes                                | `naive_bayes`                     |      ×       |        ×        |                |           |              |
| **nearest neighbour models**               |                                   |              |                 |                |           |              |
| k-nearest neighbours<sup>b</sup>           | `k_nearest_neighbours_*`, `knn_*` |      ×       |        ×        |       x        |     x     |              |
| **support vector machines**                |                                   |              |                 |                |           |              |
| *C*-classification<sup>c</sup>             | `svm_c_*`                         |      ×       |        ×        |                |           |              |
| *ν*-classification/ regression<sup>c</sup> | `svm_nu_*`                        |      ×       |        ×        |       ×        |     ×     |              |
| *ϵ*-regression<sup>c</sup>                 | `svm_eps_*`                       |              |                 |       ×        |     ×     |              |
| linear kernel<sup>d</sup>                  | `svm_*_linear`                    |      ×       |        ×        |       ×        |     ×     |              |
| polynomial kernel<sup>d</sup>              | `svm_*_polynomial`                |      ×       |        ×        |       ×        |     ×     |              |
| radial kernel<sup>d</sup>                  | `svm_*_radial`                    |      ×       |        ×        |       ×        |     ×     |              |
| sigmoid kernel<sup>d</sup>                 | `svm_*_sigmoid`                   |      ×       |        ×        |       ×        |     ×     |              |

Overview of learners implemented in familiar. <sup>a</sup> These
learners test multiple distributions or linking families and attempt to
find the best option. <sup>b</sup> k-nearest neighbours learners allow
for setting the distance metric using `*`. If omitted (e.g. `knn`), the
kernel is either `euclidean` (numeric features) or `gower` (mixed
features). <sup>c</sup> The SVM kernel is indicated by `*`. If omitted
(e.g. `svm_c`), the radial basis function is used as kernel.
<sup>d</sup> SVM type is indicated by `*`, and must be one of `c`, `nu`,
or `epsilon`.

## Configuration options

Learners, their hyperparameters, and parallelisation options can be
specified using the tags or arguments below:

|    **tag** / **argument**    | **description**                                                                                                                                                                                                                                                                 | **default**  |
|:----------------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------:|
|          `learner`           | The desired learner. Multiple learners may be provided at the same time. This setting has no default and must be provided.                                                                                                                                                      | – (required) |
|       `hyperparameter`       | Each learner has one or more hyperparameters which can be specified. Sequential model-based optimisation is used to identify hyperparameter values from the data unless these are specifically specified here. See the section on hyperparameter optimisation for more details. | – (optional) |
| `parallel_model_development` | Enables parallel processing for model development. Ignored if `parallel=FALSE`.                                                                                                                                                                                                 |    `TRUE`    |

Configuration options for model development.

## Generalised linear models

Generalised linear models (GLM) are easy to understand, use and share.
In many situations, GLM may be preferred over more complex models as
they are easier to report and validate. The most basic GLM is the linear
model. The linear model for an outcome variable *y*<sub>*j*</sub> and a
single predictor variable *x*<sub>*j*</sub> for sample *j* is:

*y*<sub>*j*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*j*</sub> + *ϵ*<sub>*j*</sub>
Here, *β*<sub>0</sub> and *β*<sub>1</sub> are both model coefficients.
*β*<sub>0</sub> is also called the model intercept. *ϵ*<sub>*j*</sub> is
an error term that describes the difference between the predicted and
the actual response for sample *j*. When a linear model is developed,
the *β*-coefficients are set in such manner that the mean-squared error
over the sample population is minimised.

The above model is a univariate model as it includes only a single
predictor. A multivariate linear model includes multiple predictors
**X**<sub>**j**</sub> and is denoted as:

*y*<sub>*j*</sub> = **β****X**<sub>**j**</sub> + *ϵ*<sub>*j*</sub>

**β****X**<sub>**j**</sub> is the linear predictor. The GLM generalises
this linear predictor through a linking function *g* (Nelder and
Wedderburn 1972):

*y*<sub>*j*</sub> = *g*(**β****X**) + *ϵ*<sub>*j*</sub>

For example, binomial outcomes are commonly modelled using logistic
regression with the `logit` linking function. Multiple linking functions
are available in familiar and are detailed below.

### Linear models for binomial outcomes

Linear models for binomial outcomes derive from the `stats` package
which is part of the R core distribution (R Core Team 2019).
Hyperparameters for these models include linkage functions and are shown
in the table below.

| **parameter**    | **tag**     |                **values**                | **optimised** | **comments**                                                                     |
|:-----------------|:------------|:----------------------------------------:|:-------------:|:---------------------------------------------------------------------------------|
| signature size   | `sign_size` |              ℤ ∈ \[1,*n*\]               |      yes      | –                                                                                |
| linking function | `family`    | `logistic`, `probit`, `loglog`, `cauchy` |  `glm` only   | The linking function is not optimised when it is specified, e.g. `glm_logistic`. |

### Linear models for multinomial outcomes

The linear model for multinomial outcomes is implemented using the
`VGAM::vglm` function (T. W. Yee and Wild 1996; T. Yee 2010; Thomas W.
Yee 2015). Hyperparameters for this model are shown in the table below.

| **parameter**    | **tag**     |  **values**   | **optimised** | **comments**                                  |
|:-----------------|:------------|:-------------:|:-------------:|:----------------------------------------------|
| signature size   | `sign_size` | ℤ ∈ \[1,*n*\] |      yes      | –                                             |
| linking function | `family`    | `multinomial` |      no       | There is only one linking function available. |

### Linear models for continuous and count-type outcomes

Linear models for continuous and count-type outcomes derive from the
`stats` package of the R core distribution (R Core Team 2019).
Hyperparameters for these models include linkage functions and are shown
in the table below.

| **parameter**    | **tag**     |                              **values**                              | **optimised** | **comments**                                                                                                                                                                            |
|:-----------------|:------------|:--------------------------------------------------------------------:|:-------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size   | `sign_size` |                            ℤ ∈ \[1,*n*\]                             |      yes      | –                                                                                                                                                                                       |
| linking function | `family`    | `gaussian`, `log_gaussian`, `inv_gaussian`, `poisson`, `log_poisson` |  `glm` only   | The linking function is not optimised when it is specified, e.g. `glm_poisson`. `gaussian`, `log_gaussian`, `inv_gaussian` linking functions are not available for count-type outcomes. |

### Linear models for survival outcomes

Linear models for survival outcomes are divided into semi-parametric and
parametric models. The semi-parametric Cox proportional hazards model
(Cox 1972) is based on the `survival::coxph` function (Therneau and
Grambsch 2000). Tied survival times are resolved using the default
method by Efron (1977).

Various fully parametric models are also available, which differ in the
assumed distribution of the outcome, i.e. the linking function. The
parametric models are all based on `survival::survreg` (Therneau and
Grambsch 2000).

Hyperparameters for semi-parametric and parametric survival models are
shown in the table below.

| **parameter**    | **tag**     |                                  **values**                                  |    **optimised**     | **comments**                                                                                                                                                 |
|:-----------------|:------------|:----------------------------------------------------------------------------:|:--------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size   | `sign_size` |                                ℤ ∈ \[1,*n*\]                                 |         yes          | –                                                                                                                                                            |
| linking function | `family`    | `weibull`, `exponential`, `gaussian`, `logistic`, `lognormal`, `loglogistic` | `survival_regr` only | The linking function is not optimised when it is specified, e.g. `survival_regr_weibull`. The non-parametric `cox` learner does not have a linking function. |

## Lasso, ridge and elastic net regression

Generalised linear models can be problematic as there is no inherent
limit to model complexity, which can easily lead to overfitting.
Penalised regression, or shrinkage, methods address this issue by
penalising model complexity.

Three shrinkage methods are implemented in `familiar`, namely ridge
regression, lasso and elastic net, which are all implemented using the
`glmnet` package (Hastie, Tibshirani, and Friedman 2009; Simon et al.
2011). The set of hyperparameters is shown in the table below. The
optimal `lambda` parameter is determined by cross-validation as part of
the `glmnet::cv.glmnet` function, and is not directly determined using
hyperparameter optimisation.

| **parameter**       | **tag**      |         **values**         | **optimised** | **comments**                                                                      |
|:--------------------|:-------------|:--------------------------:|:-------------:|:----------------------------------------------------------------------------------|
| signature size      | `sign_size`  |       ℤ ∈ \[1,*n*\]        |      yes      | –                                                                                 |
| elastic net penalty | `alpha`      |        ℝ ∈ \[0,1\]         |  elastic net  | This penalty is fixed for ridge regression (`alpha = 0`) and lasso (`alpha = 1`). |
| optimal lambda      | `lambda_min` | `lambda.1se`, `lambda.min` |      no       | Default is `lambda.min`.                                                          |
| number of CV folds  | `n_folds`    |       ℤ ∈ \[3,*n*\]        |      no       | Default is 3 if *n* \< 30, ⌊*n*/10⌋ if 30 ≤ *n* ≤ 200 and 20 if *n* \> 200.       |
| normalisation       | `normalise`  |      `FALSE`, `TRUE`       |      no       | Default is `FALSE`, as normalisation is part of pre-processing in familiar.       |

## Boosted generalised linear models and regression trees

Boosting is a procedure which combines many weak learners to form a more
powerful panel (Schapire 1990; Hastie, Tibshirani, and Friedman 2009).
Boosting learners are implemented using the `mboost` package (Bühlmann
and Hothorn 2007; Hothorn et al. 2010; Hofner, Boccuto, and Göker 2015).
Both linear regression (`mboost::glmboost`) and regression trees
(`mboost::blackboost`) are implemented. The hyperparameters are shown in
the table below.

| **parameter**                  | **tag**            |                                                                               **values**                                                                               | **optimised** | **comments**                                                                                                                                                                                                                    |
|:-------------------------------|:-------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size                 | `sign_size`        |                                                                             ℤ ∈ \[1,*n*\]                                                                              |      yes      | –                                                                                                                                                                                                                               |
| family                         | `family`           | `logistic`, `probit`, `bin_loglog`, `cauchy`, `log`, `auc`, `gaussian`, `huber`, `laplace`, `poisson`, `cox`, `weibull`, `lognormal`, `surv_loglog`, `gehan`, `cindex` | general tags  | The family is not optimised when it is specified, e.g. `boosted_tree_gaussian`.                                                                                                                                                 |
| boosting iterations            | `n_boost`          |                                                                               ℝ ∈ \[0,∞)                                                                               |      yes      | This parameter is expressed on the log<sub>10</sub> scale, i.e. the actual input value will be 10<sup>`n_boost`</sup>. The default range is \[0,3\].                                                                            |
| learning rate                  | `learning_rate`    |                                                                              ℝ ∈ \[−∞,0\]                                                                              |      yes      | This parameter is expressed on the log<sub>10</sub> scale. The default range is \[−5,0\].                                                                                                                                       |
| maximum tree depth             | `tree_depth`       |                                                                             ℤ ∈ \[1,*n*\]                                                                              |  trees only   | Maximum depth to which trees are allowed to grow.                                                                                                                                                                               |
| minimum sum of instance weight | `min_child_weight` |                                                                               ℝ ∈ \[0,∞)                                                                               |  trees only   | Minimal instance weight required for further branch partitioning, or the number of instances required in each node. This parameter is expressed on the log<sub>10</sub> scale with a  − 1 offset. The default range is \[0,2\]. |
| significance split threshold   | `alpha`            |                                                                             ℝ ∈ (0.0,1.0\]                                                                             |  trees only   | Minimum significance level for further splitting. The default range is \[10<sup>−6</sup>,1.0\]                                                                                                                                  |

Optimising the number of boosting iterations may be slow as models with
a large number of boosting iterations take longer to learn and assess.
If this is an issue, the `n_boost` parameter should be set to a smaller
range, or provided with a fixed value.

Low learning rates may require an increased number of boosting
iterations.

Also note that hyperparameter optimisation time depends on the type of
family chosen, e.g. `cindex` is considerably slower than `cox`.

## Extreme gradient boosted linear models and trees

Boosting is a procedure which combines many weak learners to form a more
powerful panel (Schapire 1990; Hastie, Tibshirani, and Friedman 2009).
Extreme gradient boosting is a gradient boosting implementation that was
highly successful in several machine learning competitions. Learners are
implemented using the `xgboost` package (Chen and Guestrin 2016). Three
types are implemented: regression based boosting (`xgboost_lm`),
regression-tree based boosting (`xgboost_tree`) and regression-tree
based boosting with drop-outs (`xgboost_dart`).

| **parameter**                  | **tag**            |                                                **values**                                                 |  **optimised**   | **comments**                                                                                                                                                                                                                                                                                                                                                                  |
|:-------------------------------|:-------------------|:---------------------------------------------------------------------------------------------------------:|:----------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size                 | `sign_size`        |                                               ℤ ∈ \[1,*n*\]                                               |       yes        | –                                                                                                                                                                                                                                                                                                                                                                             |
| family                         | `learn_objective`  | `gaussian,` `continuous_logistic`, `multinomial_logistic`, `binomial_logistic`, `poisson`, `gamma`, `cox` |   general tags   | The family is not optimised when it is specified, e.g. `xgboost_lm_linear`.                                                                                                                                                                                                                                                                                                   |
| boosting iterations            | `n_boost`          |                                                ℝ ∈ \[0,∞)                                                 |       yes        | This parameter is expressed on the log<sub>10</sub> scale, i.e. the actual value will be 10<sup>`n_boost`</sup>. The default range is \[0,3\].                                                                                                                                                                                                                                |
| learning rate                  | `learning_rate`    |                                                ℝ ∈ (−∞,0\]                                                |       yes        | This parameter is expressed on the log<sub>10</sub> scale. The default range is \[−5,0\].                                                                                                                                                                                                                                                                                     |
| L1 regularisation              | `alpha`            |                                                ℝ ∈ \[−6,∞)                                                |       yes        | This parameter is expressed on the log<sub>10</sub> scale with a 10<sup>−6</sup> offset. The default range is \[−6,3\].                                                                                                                                                                                                                                                       |
| L2 regularisation              | `lambda`           |                                                ℝ ∈ \[−6,∞)                                                |       yes        | This parameter is expressed on the log<sub>10</sub> scale with a 10<sup>−6</sup> offset. The default range is \[−6,3\].                                                                                                                                                                                                                                                       |
| maximum tree depth             | `tree_depth`       |                                                ℤ ∈ \[1,∞)                                                 | `gbtree`, `dart` | Maximum depth to which trees are allowed to grow. The default range is \[1,10\].                                                                                                                                                                                                                                                                                              |
| subsampling fraction           | `sample_size`      |                                               ℝ ∈ (0,1.0\]                                                | `gbtree`, `dart` | Fraction of available data that is used for to create a single tree. The default range is \[2/*m*,1.0\], with *m* the number of samples.                                                                                                                                                                                                                                      |
| minimum sum of instance weight | `min_child_weight` |                                                ℝ ∈ \[0,∞)                                                 | `gbtree`, `dart` | Minimal instance weight required for further branch partitioning, or the number of instances required in each node. This parameter is expressed on the log<sub>10</sub> scale with a  − 1 offset. The default range is \[0,2\].                                                                                                                                               |
| min. splitting error reduction | `gamma`            |                                                ℝ ∈ \[−6,∞)                                                | `gbtree`, `dart` | Minimum error reduction required to allow splitting. This parameter is expressed on the log<sub>10</sub> scale with a 10<sup>−6</sup> offset. The default range is \[−6,3\]. `continuous` and `count`-type outcomes are normalised to the \[0,1\] range prior to model fitting to deal with scaling issues. Values are converted back to the original scale after prediction. |
| DART sampling algorithm        | `sample_type`      |                                           `uniform`, `weighted`                                           |      `dart`      | –                                                                                                                                                                                                                                                                                                                                                                             |
| drop-out rate                  | `rate_drop`        |                                                ℝ ∈ \[0,1)                                                 |      `dart`      | –                                                                                                                                                                                                                                                                                                                                                                             |

## Random forest (RFSRC)

Random forests (explain) (Breiman 2001). The random forest learner is
implemented through the `randomForestSRC` package, which provides a
unified interface for different types of forests (Ishwaran et al. 2008,
2011).

An outcome variable that represents count-type data is first transformed
using a log (*x*+1) transformation. Predicted responses are then
transformed to the original scale using the inverse exp (*x*) − 1
transformation.

Hyperparameters for random forest learners are shown in the table below.

| **parameter**                   | **tag**       |                                                   **values**                                                   | **optimised** | **comments**                                                                                                                                                                             |
|:--------------------------------|:--------------|:--------------------------------------------------------------------------------------------------------------:|:-------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size                  | `sign_size`   |                                                 ℤ ∈ \[1,*n*\]                                                  |      yes      | –                                                                                                                                                                                        |
| number of trees                 | `n_tree`      |                                                   ℤ ∈ \[0,∞)                                                   |      yes      | This parameter is expressed on the log<sub>2</sub> scale, i.e. the actual input value will be 2<sup>`n_tree`</sup> (Oshiro, Perez, and Baranauskas 2012). The default range is \[4,10\]. |
| subsampling fraction            | `sample_size` |                                                  ℝ ∈ (0,1.0\]                                                  |      yes      | Fraction of available data that is used for to create a single tree. The default range is \[2/*m*,1.0\], with *m* the number of samples.                                                 |
| number of features at each node | `m_try`       |                                                ℝ ∈ \[0.0,1.0\]                                                 |      yes      | Familiar ensures that there is always at least one candidate feature.                                                                                                                    |
| node size                       | `node_size`   |                                                   ℤ ∈ \[1,∞)                                                   |      yes      | Minimum number of unique samples in terminal nodes. The default range is \[5,⌊*m*/3⌋\], with *m* the number of samples.                                                                  |
| maximum tree depth              | `tree_depth`  |                                                   ℤ ∈ \[1,∞)                                                   |      yes      | Maximum depth to which trees are allowed to grow. The default range is \[1,10\].                                                                                                         |
| number of split points          | `n_split`     |                                                   ℤ ∈ \[0,∞)                                                   |      no       | By default, splitting is deterministic and has one split point (0).                                                                                                                      |
| splitting rule                  | `split_rule`  | `gini`, `auc`, `entropy`, `mse`, `quantile.regr`, `la.quantile.regr`, `logrank`, `logrankscore`, `bs.gradient` |      no       | Default splitting rules are `gini` for `binomial` and `multinonial` outcomes, `mse` for `continuous` and `count` outcomes and `logrank` for `survival` outcomes.                         |

Note that optimising that optimising the number of trees can be slow, as
big forests take longer to construct and perform more computations for
predictions. Hence hyperparameter optimisation may be sped up by
limiting the range of the `n_tree` parameter, or setting a single value.

## Random forest (ranger)

The second implementation of random forests comes from the `ranger`
package (Wright and Ziegler 2017). It is generally faster than the
implementation in `randomForestSRC` and allows for different splitting
rules, such as maximally selected rank statistics (Lausen and Schumacher
1992; Wright, Dankowski, and Ziegler 2017) and concordance index-based
splitting (Schmid, Wright, and Ziegler 2016). Hyperparameters for the
random forest are shown in the table below.

| **parameter**                   | **tag**       |                         **values**                          | **optimised** | **comments**                                                                                                                                                                             |
|:--------------------------------|:--------------|:-----------------------------------------------------------:|:-------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size                  | `sign_size`   |                        ℤ ∈ \[1,*n*\]                        |      yes      | –                                                                                                                                                                                        |
| number of trees                 | `n_tree`      |                         ℤ ∈ \[0,∞)                          |      yes      | This parameter is expressed on the log<sub>2</sub> scale, i.e. the actual input value will be 2<sup>`n_tree`</sup> (Oshiro, Perez, and Baranauskas 2012). The default range is \[4,10\]. |
| subsampling fraction            | `sample_size` |                        ℝ ∈ (0,1.0\]                         |      yes      | Fraction of available data that is used for to create a single tree. The default range is \[2/*m*,1.0\], with *m* the number of samples.                                                 |
| number of features at each node | `m_try`       |                       ℝ ∈ \[0.0,1.0\]                       |      yes      | Familiar ensures that there is always at least one candidate feature.                                                                                                                    |
| node size                       | `node_size`   |                         ℤ ∈ \[1,∞)                          |      yes      | Minimum number of unique samples in terminal nodes. The default range is \[5,⌊*m*/3⌋\], with *m* the number of samples.                                                                  |
| maximum tree depth              | `tree_depth`  |                         ℤ ∈ \[1,∞)                          |      yes      | Maximum depth to which trees are allowed to grow. The default range is \[1,10\].                                                                                                         |
| splitting rule                  | `split_rule`  | `gini`, `extratrees`, `variance`, `logrank`, `C`, `maxstat` |      no       | Default splitting rules are `gini` for `binomial` and `multinomial` outcomes and `maxstat` for `continuous`, `count` and `survival` outcomes.                                            |
| significance split threshold    | `alpha`       |                       ℝ ∈ (0.0,1.0\]                        |   `maxstat`   | Minimum significance level for further splitting. The default range is \[10<sup>−6</sup>,1.0\]                                                                                           |

Note that optimising the number of trees can be slow, as big forests
take longer to construct and perform more computations for predictions.
Hence hyperparameter optimisation may be sped up by limiting the range
of the `n_tree` parameter or setting a single value.

## Naive Bayes

The naive Bayes classifier uses Bayes rule to predict posterior
probabilities. The naive Bayes classifier uses the `e1071::naiveBayes`
function (Meyer et al. 2021). The hyperparameters of the classifier are
shown in the table below.

| **parameter**     | **tag**     |  **values**   | **optimised** | **comments**                   |
|:------------------|:------------|:-------------:|:-------------:|:-------------------------------|
| signature size    | `sign_size` | ℤ ∈ \[1,*n*\] |      yes      | –                              |
| laplace smoothing | `laplace`   | ℝ ∈ \[0.0,∞)  |      yes      | The default range is \[0,10\]. |

## *k*-nearest neighbours

*k*-nearest neighbours is a simple clustering algorithm that classifies
samples based on the classes of their *k* nearest neighbours in feature
space. The `e1071::gknn` function is used to implement *k*-nearest
neighbours (Meyer et al. 2021). The hyperparameters are shown in the
table below.

| **parameter**                | **tag**           |               **values**               | **optimised** | **comments**                                                                     |
|:-----------------------------|:------------------|:--------------------------------------:|:-------------:|:---------------------------------------------------------------------------------|
| signature size               | `sign_size`       |             ℤ ∈ \[1,*n*\]              |      yes      | –                                                                                |
| number of nearest neighbours | `k`               |             ℤ ∈ \[1,*m*\]              |      yes      | The default range is \[1,⌈2*m*<sup>1/3</sup>⌉\], with *m* the number of samples. |
| distance metric              | `distance_metric` | all metrics supported by `proxy::dist` |      yes      | The default set of metrics is `gower`, `euclidean` and `manhattan`.              |

## Support vector machines

Support vector machines were originally defined to find optimal margins
between classes for classification problems. Support vector machines
were popularized after the use of the kernel trick was described. Using
the kernel trick the calculations are performed in an implicit
high-dimensional feature space, which is considerably more efficient
than explicit calculations (Boser, Guyon, and Vapnik 1992).

Familiar implements SVM using `e1071::svm`. We tried `kernlab::ksvm`,
but this function would reproducibly freeze during unit testing. By
default, both features and outcome data are scaled internally. This was
used to derive the default hyperparameter ranges specified in the table
below.

| **parameter**        | **tag**     |                 **values**                  |           **optimised**           | **comments**                                                                                                                                                                                                          |
|:---------------------|:------------|:-------------------------------------------:|:---------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| signature size       | `sign_size` |                ℤ ∈ \[1,*n*\]                |                yes                | –                                                                                                                                                                                                                     |
| SVM kernel           | `kernel`    | `linear`, `polynomial`, `radial`, `sigmoid` |                no                 | Default is `radial`, unless specified as part of the learner’s name.                                                                                                                                                  |
| *C*                  | `c`         |                      ℝ                      |                yes                | The cost for violation of constraints is expressed on a log<sub>10</sub> scale, i.e. the actual value is 10<sup>`c`</sup>. The default range is \[−5,3\].                                                             |
| *ϵ*                  | `epsilon`   |                      ℝ                      | `continuous` and `count` outcomes | The error tolerance *ϵ* is only used for regression SVM. *ϵ* is expressed on a log<sub>10</sub> scale. The default range is \[−5,1\].                                                                                 |
| *ν*                  | `nu`        |                      ℝ                      |           `nu` SVM type           | `nu` provides upper bounds for the fraction of training errors and lower bounds for the fraction of support vectors (Chang and Lin 2011). It is expressed on a log<sub>10</sub> scale. The default range is \[−5,1\]. |
| inverse kernel width | `gamma`     |                      ℝ                      |        non-linear kernels         | This parameter specifies the inverse of the kernel width. It is expressed on the log<sub>10</sub> scale. The default range is \[−9,3\].                                                                               |
| polynomial degree    | `degree`    |                 ℤ ∈ \[1,∞)                  |         polynomial kernel         | The default range is \[1,5\].                                                                                                                                                                                         |
| kernel offset        | `offset`    |                 ℝ ∈ \[0,∞)                  |  polynomial and sigmoid kernels   | Negative values are not allowed. The default range is \[0,1\].                                                                                                                                                        |

Several types of SVM algorithms exist for classification and regression.
The following SVM types are implemented:

-   *C*-classification: `c`
-   *ν*-classification: `nu`
-   *ν*-regression: `nu`
-   *ϵ*-regression: `eps`

The following kernels are implemented:

-   Linear kernel: `linear`
-   Polynomial kernel: `polynomial`
-   Radial basis function kernel: `radial`
-   Hyperbolic tangent (sigmoid) kernel: `sigmoid`

# Hyperparameter optimization

Hyperparameter optimisation is conducted to select model parameters that
are more likely to lead to generalisable results. The main
hyperparameter optimisation framework used by familiar is based on
sequential model-based optimisation (SMBO) (Hutter, Hoos, and
Leyton-Brown 2011), but with significant updates and extensions.

Overall, the following steps are conducted to optimise hyperparameters
for a given learner and dataset:

-   A set of bootstraps of the development dataset is made. This allows
    for training the models using the in-bag data and assessing
    performance using out-of-bag data. This is independent of any
    bootstraps that may have been defined as part of the experimental
    design. In case a subsampling method is specified in the
    experimental design, the corresponding internal development dataset
    is bootstrapped.

-   Variable importance is determined for each of the in-bag datasets to
    avoid positively biasing the optimisation process through
    information leakage.

-   An initial exploration of the hyperparameter space is conducted. A
    model is trained using each hyperparameter set and a bootstrap
    subsample. It is then assessed by computing an optimisation score
    based on model performance in the out-of-bag, and optionally, in-bag
    data.

-   Exploration of the hyperparameter space then continues by
    iteratively comparing the best known hyperparameter set against
    challenger hyperparameter sets in so-called intensify steps. The
    challengers are selected by predicting the expected model
    performance for a new hyperparameter set and computing its utility
    (Shahriari et al. 2016). In addition, we predict model run times,
    and actively prune potential slow challengers. Utility is computed
    using an acquisition function, of which several are available in
    familiar. The set of challenger hyperparameter sets may moreover be
    iteratively pruned, dependent on the exploration method.

-   Exploration stops if no significant improvement to model performance
    could be established by further exploring the hyperparameter space,
    the total number of iterations has been reached, or bootstraps
    samples have been exhausted.

## Predicting run time of model

Models take a certain time to train. Familiar is actively measuring this
time during hyperparameter optimisation for two reasons: first, to
optimise assignment of jobs to parallel nodes; and secondly, to prune
potential challenger sets that produce models that tend to run longer
than the best-known model. The strictness increases with increased
exploration.

Let *m* be the number of bootstraps used to quantify the most visited
hyperparameter sets, and *M* the total number of bootstrap samples that
can be visited. Then let *t*<sub>`opt`</sub> be the runtime of the best
known model. The maximum time that a challenger hyperparameter set is
allowed for training is then empirically set to:

$$t\_{\\texttt{max}} = \\left(5 - 4 \\frac{m}{M} \\right) t\_{\\texttt{opt}} $$

Hence, *t*<sub>`max`</sub> converges to *t*<sub>`opt`</sub> for
*m* → *M*.

If maximum runtime is relatively insignificant,
i.e. *t*<sub>`max`</sub> \< 10.0 seconds, a threshold of
*t*<sub>`max`</sub> = 10.0 is used.

If the maximum runtime is not known, i.e. none of the hyperparameter
sets evaluated so far produced a valid model, the maximum time threshold
is set to infinite. In effect, no hyperparameter sets are pruned based
on expected run time.

A random forest is trained to infer runtimes for (new) hyperparameter
sets, based on the runtimes observed for visited hyperparameter sets.
The random forest subsequently infers runtime for a challenger
hyperparameter set. The runtime estimate is compared against
*t*<sub>`max`</sub>, and if it exceeds the threshold, it is rejected and
not evaluated.

## Computing the optimisation score

The optimisation score *s* determines how good a set of hyperparameters
is. An optimisation score is computed in two steps. First an objective
score is computed to assess model performance for a set of
hyperparameters. The optimisation score is computed subsequently.

An objective score *s*′ is computed from the performance metric value
for a specific hyperparameter set for in-bag (IB; *s*′<sub>IB</sub>) and
out-of-bag data (OOB; *s*′<sub>OOB</sub>). The objective score always
lies in the interval \[−1.0,1.0\]. An objective score of 1.0 always
indicates the best possible score. A score of 0.0 indicates that the
hyperparameter set leads to the same degree of performance as the best
educated guess, i.e. the majority class (for binomial and multinomial
outcomes), the median value (for continuous and count outcomes), or tied
risk or survival times (for survival outcomes). Objective scores that
are either missing or below  − 1.0 are truncated to the value of  − 1.0.
In case multiple metrics are used to assess model performance, the mean
of the respective objective scores is used.

The optimisation score *s* is subsequently computed using one of the
following functions:

1.  `max_validation`: *s* = *s*′<sub>OOB</sub>. A commonly used
    criterion that tries to maximise the score on the OOB validation
    data.

2.  `balanced` (default):
    *s* = *s*′<sub>OOB</sub> − \|*s*′<sub>OOB</sub>−*s*′<sub>IB</sub>\|.
    A variation on `max_validation` with a penalty for differences in
    performance between the IB and OOB data. The underlying idea is that
    a good set of hyperparameters should lead to models that perform
    well on both development and validation data.

3.  `strong_balance`:
    *s* = *s*′<sub>OOB</sub> − 2\|*s*′<sub>OOB</sub>−*s*′<sub>IB</sub>\|.
    A variant of `balanced` with a stronger penalty term.

The optimisation score is then used to select the best known
hyperparameter set, i.e. the set that minimises the optimisation score,
and to identify candidate hyperparameter sets as described in the
following section. The optimisation function is set using the
`optimisation_function` argument.

## Predicting optimisation score for new hyperparameter sets

Any point in the hyperparameter space has a single, scalar, optimisation
score value that is *a priori* unknown. During the optimisation process,
the algorithm samples from the hyperparameter space by selecting
hyperparameter sets and computing the optimisation score for one or more
bootstraps. For each hyperparameter set the resulting scores are
distributed around the actual value.

A key point of Bayesian optimisation is the ability to estimate the
usefulness or utility of new hyperparameter sets. This ability is
facilitated by modelling the optimisation score of new hyperparameter
sets using the optimisation scores of observed hyperparameter sets.

The following models can be used for this purpose:

-   `gaussian_process` (default): Creates a localised approximate
    Gaussian deterministic Gaussian Process. This is implemented using
    the `laGP` package (Gramacy 2016).

-   `bayesian_additive_regression_trees` or `bart`: Uses Bayesian
    Additive Regression Trees for inference. Unlike standard random
    forests, BART allows for estimating posterior distributions directly
    and can extrapolate. BART is implemented using the `BART` package
    (Sparapani, Spanbauer, and McCulloch 2021).

-   `random_forest`: Creates a random forest for inference. A random
    forest was originally used in the SMBO algorithm by Hutter et al.
    (Hutter, Hoos, and Leyton-Brown 2011). A weakness of random forests
    is their lack of extrapolation beyond observed values, which limits
    their usefulness in exploiting promising areas of hyperparameter
    space somewhat.

In addition, familiar can perform random search (`random` or
`random_search`). This forgoes the use of models to steer optimisation.
Instead, the hyperparameter space is sampled at random.

The learner used to predict optimisation scores can be specified using
the `hyperparameter_learner` parameter.

## Acquisition functions for utility of hyperparameter sets

The expected values and the posterior values of optimisation scores from
the models are used to compute utility of new hyperparameter sets using
an acquisition function. The following acquisition functions are
available in familiar (Shahriari et al. 2016). Let *ν* = *f*(**x**) be
the posterior distribution of hyperparameter set **x**, and *τ* the best
observed optimisation score. Let also *μ*(**x**) and *σ*(**x**) be the
sample mean and sample standard deviation for set **x** (implicitly for
round *m*):

-   `improvement_probability`: The probability of improvement quantifies
    the probability that the expected optimisation score for a set **x**
    is better than the best observed optimisation score *τ*:

    $$\\alpha(\\mathbf{x}) = P(\\nu > \\tau) = \\Phi \\left(\\frac{\\mu(\\mathbf{x}) - \\tau} {\\sigma(\\mathbf{x})}\\right)$$

    Here *Φ* is the cumulative distribution function of a normal
    distribution. Note that this acquisition function is typically prone
    to convergence to local minima, although in our algorithm this may
    be mitigated by the alternating random search strategy.

-   `improvement_empirical_probability`: Similar to
    `improvement_probability`, but based directly on optimisation scores
    predicted by the individual decision trees:

    $$\\alpha(\\mathbf{x}) = P(\\nu > \\tau) = \\frac{1}{N} \\sum\_{i=1}^N \\left\[\\nu_i > \\tau\\right\]$$

    with \[…\] denoting an Iverson bracket, which takes the value 1 if
    the condition specified by … is true, and 0 otherwise.

-   `expected_improvement` (default): Expected improvement is based on
    an improvement function, and is be computed as follows:

    $$\\alpha(\\mathbf{x})=(\\mu(\\mathbf{x})-\\tau) \\Phi \\left(\\frac{\\mu(\\mathbf{x}) - \\tau}{\\sigma(\\mathbf{x})}\\right) + \\sigma(\\mathbf{x}) \\phi\\left(\\frac{\\mu(\\mathbf{x}) - \\tau}{\\sigma(\\mathbf{x})}\\right)$$

    with *ϕ* denoting the normal probability density function. If
    *σ*(**x**) = 0, *α*(**x**) = 0.

-   `upper_confidence_bound`: This acquisition function is based on the
    upper confidence bound of the distribution, and is defined as
    (Srinivas et al. 2012):

    *α*(**x**,*t*) = *μ*(**x**) + *β*<sub>*t*</sub>*σ*(**x**)

    Here the parameter *β*<sub>*t*</sub> depends on the current round
    *t*, which in our implementation would be roughly equivalent to the
    maximum number of bootstraps performed for any of the hyperparameter
    sets, minus 1. The *β*<sub>*t*</sub> parameter is then defined as
    *β*<sub>*t*</sub> = 2log (\|*D*\|(*t*+1)<sup>2</sup>*π*<sup>2</sup>/6*δ*)
    (Srinivas et al. 2012). Here, *δ* ∈ \[0,1\] and \|*D*\| the
    dimensionality of the hyperparameter space. For their experiments
    Srinivas et al. used *δ* = 0.1, and noted that scaling down
    *β*<sub>*t*</sub> by a factor 5 improves the algorithm. Hence, we
    use
    $\\beta_t= \\frac{2}{5} \\log\\left(\\frac{5}{3} \|D\|(t+1)^2\\pi^2\\right)$.

-   `bayes_upper_confidence_bound`: Proposed by Kaufmann et al.
    (Kaufmann, Cappé, and Garivier 2012), the Bayesian upper confidence
    bound is defined as:

    $$\\alpha(\\mathbf{x}, t)=Q \\left(1-\\frac{1}{(t + 1) \\log(n)^c}, \\nu_t\\right)$$

    with *Q* being a quantile function. Note that we here deviate from
    the original definition by substituting the original *t* by *t* + 1,
    and not constraining the posterior distribution *ν* to a known
    function. In the original, *n* defines the number of rounds, with
    *t* the current round. In our algorithm, this is interpreted as *n*
    being the total number of bootstraps, and *t* the maximum number of
    bootstraps already performed for any of the hyperparameter sets.
    Hence our implementation is the same, aside from the lack of
    constraint on the posterior distribution. Based on the simulations
    and recommendations by Kaufmann et al. we choose parameter *c* = 0.

The acquisition function can be specified using the
`acquisition_function` parameter.

## Exploring challenger sets

After selecting challenger hyperparameter sets, the optimisation score
of models trained using these hyperparameter sets are compared against
the best-known score in a run-off intensify step. During this step,
models will be trained on new bootstrap data, compared, and then trained
on another set of bootstraps, and so on. Pruning the set of challenger
hyperparameters after each iteration reduces computational load.
Familiar implements the following methods:

-   `successive_halving` (default): The set of alternative parameter
    sets is pruned by removing the worst performing half of the sets
    after each step (Jamieson and Talwalkar 2016). The set of
    investigated parameter sets gets progressively smaller.

-   `stochastic_reject`: The set of alternative parameter sets is pruned
    by comparing the performance of each parameter set with that of the
    incumbent best parameter set using a paired Wilcoxon test.

-   `none`: The set of alternative parameter sets is not pruned.

The method used to steer exploration can be set using the
`exploration_method` parameter.

## Providing hyperparameters manually

It is possible to set hyperparameters manually. This can be used to
change hyperparameters that are fixed by default, to set a fixed value
for randomised hyperparameters, or to provide a different search range
for randomised hyperparameters.

Hyperparameters can be provided using the `hyperparameter` tag. For the
`glm_logistic` learner an example tag may for example look as follows:

    <hyperparameter>
      <glm_logistic>
        <sign_size>5</sign_size>
      </glm_logistic>
    </hyperparameter>

Or as a nested list passed as the `hyperparameter` argument to
`summon_familiar`:

    hyperparameter = list("glm_logistic"=list("sign_size"=5))

More than one value can be provided. The behaviour changes depending on
whether the hyperparameter is categorical or numeric variable. In case
of categorical hyperparameters, the provided values define the search
range. For numerical hyperparameters, providing two values sets the
bounds of the search range, whereas providing more than two values will
define the search range itself.

## Configuration options for hyperparameter optimisation

Hyperparameter optimization may be configured using the tags/arguments
in the table below.

|         **tag** / **argument**         | **description**                                                                                                                                                                                                                 |                                                     **default**                                                     |
|:--------------------------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------:|
|       `optimisation_bootstraps`        | Maximum number of bootstraps created for optimisation                                                                                                                                                                           |                                                        `50`                                                         |
|     `optimisation_determine_vimp`      | If `TRUE`, compute variable importance for each bootstrap. If `FALSE` use variable importance computed during the feature selection step.                                                                                       |                                                       `TRUE`                                                        |
|      `smbo_random_initialisation`      | If `random` initial parameter sets are generated randomly from default ranges. If `fixed` or `fixed_subsample`, the initial parameter sets are based on a grid in parameter space.                                              |                                                  `fixed_subsample`                                                  |
|          `smbo_n_random_sets`          | Sets the number of hyperparameter sets drawn for initialisation. Ignored if `smbo_random_initialisation` is `fixed`. If `smbo_random_initialisation` is `fixed_subsample`, the number of selected hyperparameters may be lower. |                                                        `100`                                                        |
|         `max_smbo_iterations`          | Maximum number of intensify iterations of the SMBO algorithm                                                                                                                                                                    |                                                        `20`                                                         |
|   `smbo_stop_convergent_iterations`    | Number of subsequent convergent SMBO iterations required to stop hyperparameter optimisation early.                                                                                                                             |                                                         `3`                                                         |
|         `smbo_stop_tolerance`          | Tolerance for recent optimisation scores to determine convergence                                                                                                                                                               |                                                       `0.01`                                                        |
|         `smbo_step_bootstraps`         | Number of bootstraps used within each step of an intensify iteration.                                                                                                                                                           |                                                         `3`                                                         |
|         `smbo_intensify_steps`         | Number of intensify steps within each intensify iteration.                                                                                                                                                                      |                                                         `5`                                                         |
|         `optimisation_metric`          | The metric used for optimisation, e.g. `auc`. See the vignette on performance metrics for available options. More than one metric may be specified.                                                                             | `auc_roc` (`binomial`, `multinomial`); `mse` (`continuous`); `msle` (`count`); and `concordance_index` (`survival`) |
|        `optimisation_function`         | The optimisation function used (see [Computing the optimisation score](#computing-the-optimisation-score)).                                                                                                                     |                                                     `balanced`                                                      |
|        `hyperparameter_learner`        | Learner used to predict optimisation scores for new hyperparameter sets (see [Predicting optimisation score for new hyperparameter sets](#predicting-optimisation-score-for-new-hyperparameter-sets)).                          |                                                 `gaussian_process`                                                  |
|         `acquisition_function`         | The function used to quantify utility of hyperparameter sets (see [Acquisition functions for utility of hyperparameter sets](#acquisition-functions-for-utility-of-hyperparameter-sets))                                        |                                               `expected_improvement`                                                |
|          `exploration_method`          | Method used to explore challenger hyperparameter sets (see [Exploring challenger sets](#exploring-challenger-sets))                                                                                                             |                                                `successive_halving`                                                 |
|    `smbo_stochastic_reject_p_value`    | The p-value level for stochastic pruning                                                                                                                                                                                        |                                                       `0.05`                                                        |
| `parallel_hyperparameter_optimisation` | Enables parallel processing for hyperparameter optimisation. Ignored if `parallel=FALSE`.                                                                                                                                       |                                                       `TRUE`                                                        |

Configuration options for hyperparameter optimisation.

# Model recalibration

Even though learners may be good at discrimination, model calibration
can be lacklustre. Some learners are therefore recalibrated as follows:

1.  3-fold cross-validation is performed. Responses are predicted using
    a new model trained on each training fold.

2.  Three recalibration models are trained using the responses for the
    training folds as input (Niculescu-Mizil and Caruana 2005).

3.  The three recalibration models are then applied to (new) responses
    of the full model, and the resulting values averaged for each
    sample.

The following learners currently undergo recalibration:

| **learner**                                 |      **outcome**      | **recalibration model** |
|:--------------------------------------------|:---------------------:|:------------------------|
| `xgboost_lm`, `xgboost_lm_logistic`         | binomial, multinomial | `glm_logistic`          |
| `xgboost_tree`, `xgboost_tree_logistic`     | binomial, multinomial | `glm_logistic`          |
| `xgboost_lm_cox`                            |       survival        | `glm_cox`               |
| `xgboost_tree_cox`                          |       survival        | `glm_cox`               |
| `boosted_glm_cindex`, `boosted_glm_gehan`   |       survival        | `glm_cox`               |
| `boosted_tree_cindex`, `boosted_tree_gehan` |       survival        | `glm_cox`               |

Familiar currently does not recalibrate models by inverting the model
calibration curves, but may do so in the future.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Boser1992-nk" class="csl-entry">

Boser, Bernhard E, Isabelle M Guyon, and Vladimir N Vapnik. 1992. “A
Training Algorithm for Optimal Margin Classifiers.” In *Proceedings of
the Fifth Annual Workshop on Computational Learning Theory*, 144–52.
ACM.

</div>

<div id="ref-Breiman2001-ao" class="csl-entry">

Breiman, Leo. 2001. “Random Forests.” *Mach. Learn.* 45 (1): 5–32.

</div>

<div id="ref-Buhlmann2007-ku" class="csl-entry">

Bühlmann, Peter, and Torsten Hothorn. 2007. “Boosting Algorithms:
Regularization, Prediction and Model Fitting.” *Stat. Sci.* 22 (4):
477–505.

</div>

<div id="ref-Chang2011-ck" class="csl-entry">

Chang, Chih-Chung, and Chih-Jen Lin. 2011. “LIBSVM: A Library for
Support Vector Machines.” *ACM Trans. Intell. Syst. Technol.* 2 (3):
27:1–27.

</div>

<div id="ref-Chen2016-lo" class="csl-entry">

Chen, Tianqi, and Carlos Guestrin. 2016. “XGBoost: A Scalable Tree
Boosting System.” In *Proceedings of the 22nd ACM SIGKDD International
Conference on Knowledge Discovery and Data Mining*, 785–94.

</div>

<div id="ref-Cox1972-fc" class="csl-entry">

Cox, D R. 1972. “Regression Models and Life-Tables.” *J. R. Stat. Soc.
Series B Stat. Methodol.* 34 (2): 187–202.

</div>

<div id="ref-Efron1977-ww" class="csl-entry">

Efron, Bradley. 1977. “The Efficiency of Cox’s Likelihood Function for
Censored Data.” *J. Am. Stat. Assoc.* 72 (359): 557–65.

</div>

<div id="ref-Gramacy2016-aa" class="csl-entry">

Gramacy, Robert B. 2016. “<span class="nocase">laGP</span>: Large-Scale
Spatial Modeling via Local Approximate Gaussian Processes in R.”
*Journal of Statistical Software* 72 (1): 1–46.

</div>

<div id="ref-Hastie2009-ed" class="csl-entry">

Hastie, Trevor, Robert Tibshirani, and Jerome Friedman. 2009. *The
Elements of Statistical Learning: Data Mining, Inference, and
Prediction*. Second Edition. Springer Series in Statistics. New York,
NY, United States: Springer Science+Business Media, LLC.

</div>

<div id="ref-Hofner2015-tt" class="csl-entry">

Hofner, Benjamin, Luigi Boccuto, and Markus Göker. 2015. “Controlling
False Discoveries in High-Dimensional Situations: Boosting with
Stability Selection.” *BMC Bioinformatics* 16 (May): 144.

</div>

<div id="ref-Hothorn2010-cu" class="csl-entry">

Hothorn, Torsten, Peter Bühlmann, Thomas Kneib, Matthias Schmid, and
Benjamin Hofner. 2010. “Model-Based Boosting 2.0.” *J. Mach. Learn.
Res.* 11 (Aug): 2109–13.

</div>

<div id="ref-Hutter2011-ea" class="csl-entry">

Hutter, Frank, Holger H Hoos, and Kevin Leyton-Brown. 2011. “Sequential
Model-Based Optimization for General Algorithm Configuration.” In
*Learning and Intelligent Optimization*, edited by Carlos A Coello
Coello, 6683:507–23. Lecture Notes in Computer Science. Berlin,
Heidelberg: Springer Berlin Heidelberg.

</div>

<div id="ref-Ishwaran2008-hz" class="csl-entry">

Ishwaran, Hemant, Udaya B Kogalur, Eugene H Blackstone, and Michael S
Lauer. 2008. “Random Survival Forests.” *Ann. Appl. Stat.* 2 (3):
841–60.

</div>

<div id="ref-Ishwaran2011-gu" class="csl-entry">

Ishwaran, Hemant, Udaya B Kogalur, Xi Chen, and Andy J Minn. 2011.
“Random Survival Forests for High-Dimensional Data.” *Stat. Anal. Data
Min.* 4 (1): 115–32.

</div>

<div id="ref-Jamieson2016-fq" class="csl-entry">

Jamieson, Kevin, and Ameet Talwalkar. 2016. “Non-Stochastic Best Arm
Identification and Hyperparameter Optimization.” In *Proceedings of the
19th International Conference on Artificial Intelligence and
Statistics*, edited by Arthur Gretton and Christian C Robert, 51:240–48.
Proceedings of Machine Learning Research. Cadiz, Spain: PMLR.

</div>

<div id="ref-Kaufmann2012-kh" class="csl-entry">

Kaufmann, Emilie, Olivier Cappé, and Aurélien Garivier. 2012. “On
Bayesian Upper Confidence Bounds for Bandit Problems.” In *Artificial
Intelligence and Statistics*, 592–600.

</div>

<div id="ref-Lausen1992-qh" class="csl-entry">

Lausen, Berthold, and Martin Schumacher. 1992. “Maximally Selected Rank
Statistics.” *Biometrics* 48 (1): 73.

</div>

<div id="ref-Meyer2021-aq" class="csl-entry">

Meyer, David, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel, and
Friedrich Leisch. 2021. *E1071: Misc Functions of the Department of
Statistics, Probability Theory Group (Formerly: E1071), TU Wien*.
<https://CRAN.R-project.org/package=e1071>.

</div>

<div id="ref-Nelder1972-rs" class="csl-entry">

Nelder, J A, and R W M Wedderburn. 1972. “Generalized Linear Models.”
*J. R. Stat. Soc. Ser. A* 135 (3): 370–84.

</div>

<div id="ref-Niculescu-Mizil2005-kj" class="csl-entry">

Niculescu-Mizil, Alexandru, and Rich Caruana. 2005. “Predicting Good
Probabilities with Supervised Learning.” In *Proceedings of the 22nd
International Conference on Machine Learning*, 625–32. ACM.

</div>

<div id="ref-Oshiro2012-mq" class="csl-entry">

Oshiro, Thais Mayumi, Pedro Santoro Perez, and José Augusto Baranauskas.
2012. “How Many Trees in a Random Forest?” In *Machine Learning and Data
Mining in Pattern Recognition*, 154–68. Springer Berlin Heidelberg.

</div>

<div id="ref-rcore2018" class="csl-entry">

R Core Team. 2019. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Schapire1990-vn" class="csl-entry">

Schapire, Robert E. 1990. “The Strength of Weak Learnability.” *Mach.
Learn.* 5 (2): 197–227.

</div>

<div id="ref-Schmid2016-ie" class="csl-entry">

Schmid, Matthias, Marvin N Wright, and Andreas Ziegler. 2016. “On the
Use of Harrell’s C for Clinical Risk Prediction via Random Survival
Forests.” *Expert Syst. Appl.* 63 (November): 450–59.

</div>

<div id="ref-Shahriari2016-kx" class="csl-entry">

Shahriari, B, K Swersky, Z Wang, R P Adams, and N de Freitas. 2016.
“Taking the Human Out of the Loop: A Review of Bayesian Optimization.”
*Proc. IEEE* 104 (1): 148–75.

</div>

<div id="ref-Simon2011-ih" class="csl-entry">

Simon, Noah, Jerome Friedman, Trevor Hastie, and Rob Tibshirani. 2011.
“Regularization Paths for Cox’s Proportional Hazards Model via
Coordinate Descent.” *J. Stat. Softw.* 39 (5): 1–13.

</div>

<div id="ref-Sparapani2021-aa" class="csl-entry">

Sparapani, Rodney, Charles Spanbauer, and Robert McCulloch. 2021.
“Nonparametric Machine Learning and Efficient Computation with Bayesian
Additive Regression Trees: The BART R Package.” *Journal of Statistical
Software* 97 (1): 1–66.

</div>

<div id="ref-Srinivas2012-hq" class="csl-entry">

Srinivas, N, A Krause, S M Kakade, and M W Seeger. 2012.
“Information-Theoretic Regret Bounds for Gaussian Process Optimization
in the Bandit Setting.” *IEEE Trans. Inf. Theory* 58 (5): 3250–65.

</div>

<div id="ref-Therneau2000-jv" class="csl-entry">

Therneau, Terry M, and Patricia M Grambsch. 2000. *Modeling Survival
Data: Extending the Cox Model*. Statistics for Biology and Health. New
York: Springer Science & Business Media.

</div>

<div id="ref-Wright2017-sj" class="csl-entry">

Wright, Marvin N, Theresa Dankowski, and Andreas Ziegler. 2017.
“Unbiased Split Variable Selection for Random Survival Forests Using
Maximally Selected Rank Statistics.” *Stat. Med.* 36 (8): 1272–84.

</div>

<div id="ref-Wright2017-wc" class="csl-entry">

Wright, Marvin N, and Andreas Ziegler. 2017. “Ranger : A Fast
Implementation of Random Forests for High Dimensional Data in c++ and
R.” *J. Stat. Softw.* 77 (1).

</div>

<div id="ref-Yee1996-ql" class="csl-entry">

Yee, T W, and C J Wild. 1996. “Vector Generalized Additive Models.” *J.
R. Stat. Soc. Series B Stat. Methodol.* 58 (3): 481–93.

</div>

<div id="ref-Yee2010-rg" class="csl-entry">

Yee, Thomas. 2010. “The VGAM Package for Categorical Data Analysis.”
*Journal of Statistical Software* 32 (10): 1–34.

</div>

<div id="ref-Yee2015-bw" class="csl-entry">

Yee, Thomas W. 2015. *Vector Generalized Linear and Additive Models:
With an Implementation in R*. Springer.

</div>

</div>

<div class="footer">
<br>
<a rel="license" href="https://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="../vignettes/CC4_0_BY_88x31.png" /></a>
This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
Cite as: Alex Zwanenburg. familiar: Vignettes and Documentation (2021). <a href="https://github.com/alexzwanenburg/familiar">https://github.com/alexzwanenburg/familiar</a>
</div>
