Introducing familiar
================
Alex Zwanenburg
2022-02-02

<img src="../vignettes/familiar.svg" align="right" width="120"/>

-   [Familiar in brief](#familiar-in-brief)
    -   [Installing familiar](#installing-familiar)
    -   [Pipeline](#pipeline)
    -   [Supported outcomes](#supported-outcomes)
-   [Running familiar](#running-familiar)
    -   [Configuring familiar](#configuring-familiar)
    -   [Preparing your data](#preparing-your-data)
        -   [Identifier columns](#identifier-columns)
        -   [Outcome columns](#outcome-columns)
        -   [Feature columns](#feature-columns)
    -   [Experimental designs](#experimental-designs)
-   [References](#references)

``` r
library(familiar)
library(data.table)
```

Familiar is a package that allows for end-to-end machine learning of
tabular data, with subsequent evaluation and explanation of models. This
vignette provides an overview of its functionality and how to configure
and run an experiment.

# Familiar in brief

This section provides installation instructions, a brief overview of the
package, and the pipeline encapsulated by the `summon_familiar` function
that is used to run an experiment.

## Installing familiar

Stable versions of familiar can be installed from CRAN.
`dependencies=TRUE` prevents being prompted to install packages when
using familiar.

``` r
install.packages("familiar",
                 dependencies=TRUE)
```

It can also be installed directly from the GitHub repository:

``` r
require(devtools)
devtools::install_github("https://github.com/alexzwanenburg/familiar",
                         dependencies=TRUE)
```

## Pipeline

The pipeline implemented in familiar follows a standard machine learning
process. A development dataset is used to perform the steps listed
below. Many aspects of these steps can be configured, but the overall
process is fixed:

-   **Data processing**: Features in the development dataset are
    assessed during this step:

    -   General feature information: Are features categorical (e.g. has
        the values `FALSE`, `TRUE`) or numeric? Which levels does a
        categorical or ordinal feature have?

    -   Invariance: Which features are invariant and should be dropped?

    -   Transformation: How should numeric features be transformed using
        a power transformation to make these features behave more
        according to a normal distribution?

    -   Normalisation: How should numeric features be normalised to
        reduce differences in scale between features the dataset? Note
        that familiar also allows for normalisation at the batch level
        to remove systematic differences in feature values between
        different batches or cohorts.

    -   Robustness: Should non-robust features, assessed using repeated
        measurements, be filtered?

    -   Importance: Should generally unimportant features be filtered
        after univariate analysis?

    -   Imputation: How should missing feature values be imputed?

    -   Redundancy clustering: Which features are similar and should be
        clustered together?

-   **Feature selection**: Which features are important for the endpoint
    of interest? Familiar supports various univariate and multivariate
    feature selection methods (see the *Feature selection methods*
    vignette). Note that feature selection, at least in familiar, is a
    misnomer. Instead of selecting features, in the sense of selecting
    the features to be included in a model by a learner, features in the
    data are ranked according to their importance. Actual feature
    selection is conducted during hyperparameter optimisation.

-   **Hyperparameter optimisation**: Most learners have hyperparameters,
    which are parameters that determine a specific aspect of the model
    created by the learner. Examples are the number of trees in a random
    forest, the width of the radial kernel in support vector machines,
    and the number of features in the signature of a model. Such
    parameters may significantly influence model performance. During
    hyperparameter optimisation, the aim is to find the set of
    hyperparameters that leads to a generalisable model. Since
    hyperparameter spaces can be high-dimensional, familiar uses
    Bayesian optimisation for efficiently exploring hyperparameter
    space. The *learning algorithms and hyperparameter optimisation*
    vignette describes model-specific hyperparameters and hyperparameter
    optimisation in more detail.

-   **Model training**: During the final model training step, the
    development data are fitted using the previously determined set of
    hyperparameters. By default, the models are trimmed after creation
    to remove extraneous information such as copies of the development
    data. The model objects that are created in this step contain more
    than just the model. Notably, the following information is included
    to allow for prospective use and evaluation:

    -   Feature information, as generated during the data processing
        step, is stored to allow for preparing datasets in the same
        manner as the development dataset and for checking if new
        datasets are formatted as expected. It is also used to create
        default ranges for individual conditional expectation and
        partial dependence plots.

    -   Outcome information is stored. This is primarily used to check
        whether outcome data in new datasets are formatted in accordance
        with the development data. It is also used in computing several
        performance metrics.

    -   A novelty detector is trained to detect out-of-distribution
        samples and assess when a model starts extrapolating. The
        novelty detector is currently based on extended isolation
        forests in the `isotree` package Cortes (2021).

    -   Models used to recalibrate the output of specific models (see
        *Learning algorithm* vignette) are stored.

    -   Calibration information is added. This currently is only done
        for survival analysis, for which we store baseline survival
        curves Royston and Altman (2013).

    -   Risk stratification thresholds used for assigning risk strata
        are stored.

After training the models, the models are assessed using the development
and any validation datasets. Models, and results from this analysis are
written to a local directory.

## Supported outcomes

Familiar supports modelling and evaluation of several types of
endpoints:

-   Categorical endpoints, where the outcome consists of two or more
    classes. Familiar distinguishes between two-class (`binomial`) and
    multi-class (`multinomial`) outcomes. These differ in that fewer
    feature selection methods and learners are available for multi-class
    outcomes. Additionally some evaluation and explanation steps will
    assess all classes separately in a one-against-all fashion for
    multi-class outcomes, whereas for two-class outcomes only the
    *positive* class is assessed.

-   Numerical endpoints, where the outcome consists of numeric values.
    Count-like `count` outcomes and generic numerical `continuous`
    outcomes are supported. If you are unsure that your outcome is
    generated through some counting or event mechanism, it may be safer
    to use the more generic `continuous` option.

-   Survival endpoints, where the outcome consists of a pair of time and
    event status variables. Familiar supports right-censored
    time-to-event data (`survival`).

Other endpoints are not supported. Handling of competing risk survival
endpoints is planned for future releases.

# Running familiar

The end-to-end pipeline is implemented in the `summon_familiar`
function. This is the main function to use.

In the example below, we use the *iris* dataset, specify some minimal
configuration parameters, and run the experiment. In practice, you may
need to specify some additional configuration parameters, see the
[Configuring familiar](#configuring-familiar) section.

``` r
# Example experiment using the iris dataset.
# You may want to specify a different path for experiment_dir.
# This is where results are written to.
familiar::summon_familiar(data=iris,
                          experiment_dir=file.path(tempdir(), "familiar_1"),
                          outcome_type="multinomial",
                          outcome_column="Species",
                          experimental_design="fs+mb",
                          cluster_method="none",
                          fs_method="mrmr",
                          learner="glm",
                          parallel=FALSE)
```

It is also possible to use a formula instead. This is generally feasible
only for datasets with few features:

``` r
# Example experiment using a formula interface.
# You may want to specify a different path for experiment_dir.
# This is where results are written to.
familiar::summon_familiar(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                          data=iris,
                          experiment_dir=file.path(tempdir(), "familiar_2"),
                          outcome_type="multinomial",
                          experimental_design="fs+mb",
                          cluster_method="none",
                          fs_method="mrmr",
                          learner="glm",
                          parallel=FALSE)
```

Data does not need to be loaded prior to calling `summon_familiar`. A
path to a *csv* file can also be provided. The data can also be a
`data.frame` or `data.table` contained in an *RDS* or *RData* file.
Other data formats are currently not supported. If categorical features
are encoded using integer values, it is recommended to load the data and
manually encode them, as is explained in the [Preparing your
data](#preparing-your-data) section.

``` r
# Example experiment using a csv datafile.
# Note that because the file does not exist,
# you will not be able to execute the code as is.
familiar::summon_familiar(data="path_to_data/iris.csv",
                          experiment_dir=file.path(tempdir(), "familiar_3"),
                          outcome_type="multinomial",
                          outcome_column="Species",
                          class_levels=c("setosa", "versicolor", "virginica"),
                          experimental_design="fs+mb",
                          cluster_method="none",
                          fs_method="mrmr",
                          learner="glm",
                          parallel=FALSE)
```

For reproducibility purposes, it may be useful to configure
`summon_familiar` using the configuration *xml* file. In that case, we
will point to a data file using the `data_file` parameter.

``` r
# Example experiment using a configuration file.
# Note that because the file does not exist,
# you will not be able to execute the code as is.
familiar::summon_familiar(config="path_to_configuration_file/config.xml")
```

Configuration parameters may also be mixed between parameters specified
in the *xml* file and function arguments. Function arguments supersede
parameters specified in the *xml* file:

``` r
# Example experiment using a csv datafile, but with additional arguments.
# Note that because the configuration file does not exist,
# you will not be able to execute the code as is.
familiar::summon_familiar(config="path_to_configuration_file/config.xml",
                          data=iris,
                          parallel=FALSE)
```

## Configuring familiar

Familiar is highly configurable. Parameters can be specified in two
ways:

1.  Using a configuration file. An empty copy of the configuration file
    can be obtained using the `familiar::get_xml_config` function. The
    `familiar::summon_familiar` function should subsequently be called
    by specifying the `config` argument.

2.  By specifying function arguments for the `familiar::summon_familiar`
    function.

All configuration parameters are documented in the help file of the
`familiar::summon_familiar` function. Often, the default settings
suffice. The parameters below should always be specified:

-   `experimental_design`: Specifies the design of the experiment. This
    is described more extensively further in the vignette, in the
    [Experimental designs](#experimental-designs) section.

-   `fs_method`: Specify one or more feature selection methods. See the
    *Feature selection methods* vignette for available methods.

-   `learner`: Specify one or more learners used to create models. See
    the *learning algorithms and hyperparameter optimisation* vignette
    for available learners.

Though not always required, specifying the following parameters is
recommended or situationally required:

-   `experiment_dir`: This specifies the drive location where files
    generated during the experiment are written to. This includes files
    containing the trained models, which we usually want to preserve. If
    this location is not specified, such files are temporarily written
    to the temporary R directory, and subsequently removed.

-   `outcome_column`: Specifies the name of the column that contains the
    outcome values. In case of survival outcomes two columns should be
    specified that indicate time and event status, respectively. For
    survival outcomes familiar determines which columns contain time and
    event data. The `outcome_column` parameter is not required in case
    the formula interface is used.

-   `outcome_type`: Specifies the type of outcome being modelled. Should
    be one of the outcome types mentioned above in the [Supported
    outcomes](#supported-outcomes) section. If not specified, it can
    potentially be inferred from the data contained in the column(s)
    specified by the `outcome_column` parameter.

-   `class_levels`: Specify the class levels of two-class (`binomial`)
    and multi-class (`multinomial`) outcomes. For two-class outcomes,
    the second level specifies the class regarded as the positive class.
    The values should match values present in the outcome column
    Specifying this argument is not necessary in case the outcome column
    is encoded as a factor. If left unspecified, the unique values in
    the outcome column are used as values.

-   `event_indicator`, `censoring_indicator`,
    `competing_risk_indicator`: Specifies the values that should be used
    as event, censoring, and competing risk indicators for survival
    analysis, respectively. Familiar uses default values for censoring
    (e.g. `0`, `FALSE`, `no`) and event (e.g. `1`, `TRUE`, `yes`) status
    otherwise. Note that the `competing_risk` outcome type will be fully
    implemented in a future release.

-   `batch_id_column`, `sample_id_column`, `series_id_column`: Specifies
    the names of the columns containing batch, sample, and series
    identifiers respectively. These are described in more detail in the
    [Preparing your data](#preparing-your-data) section.

## Preparing your data

Familiar processes tabular data. In this case, a table consists of rows
that represent instances, and columns that represent features and
additional information. This is a very common representation for tabular
data. Let us look at the *colon* dataset found in the survival package,
which contains data from a clinical trial to assess a new anti-cancer
drug in patients with colon cancer:

``` r
# Get the colon dataset.
data <- data.table::as.data.table(survival::colon)[etype==1]

# Drop some irrelevant columns.
data[, ":="("node4"=NULL, "etype"=NULL)]

knitr::kable(data[1:5])
```

|  id | study | rx      | sex | age | obstruct | perfor | adhere | nodes | status | differ | extent | surg | time |
|----:|------:|:--------|----:|----:|---------:|-------:|-------:|------:|-------:|-------:|-------:|-----:|-----:|
|   1 |     1 | Lev+5FU |   1 |  43 |        0 |      0 |      0 |     5 |      1 |      2 |      3 |    0 |  968 |
|   2 |     1 | Lev+5FU |   1 |  63 |        0 |      0 |      0 |     1 |      0 |      2 |      3 |    0 | 3087 |
|   3 |     1 | Obs     |   0 |  71 |        0 |      0 |      1 |     7 |      1 |      2 |      2 |    0 |  542 |
|   4 |     1 | Lev+5FU |   0 |  66 |        1 |      0 |      0 |     6 |      1 |      2 |      3 |    1 |  245 |
|   5 |     1 | Obs     |   1 |  69 |        0 |      0 |      0 |    22 |      1 |      2 |      3 |    1 |  523 |

Here we see that each row contains a separate instance.

### Identifier columns

The `id` and `study` columns are identifier columns. Familiar
distinguishes four different types of identifiers:

-   Batch identifiers are used to identify data belonging to a batch,
    cohort or specific dataset. This is typically used for specifying
    external validation datasets (using the `validation_batch_id`
    parameter). It also used to define the batches for batch
    normalisation. The name of the column containing batch identifiers
    (if any) can be specified using the `batch_id_column` parameter. If
    no column with batch identifiers is specified, all instances are
    assumed to belong to the same batch. In the *colon* dataset, the
    `study` column is a batch identifier column.

-   Sample identifiers are used to identify data belonging to a single
    sample, such as a patient, subject, customer, etc. Sample
    identifiers are used to ensure that instances from the same sample
    are not inadvertently spread across development and validation data
    subsets created for cross-validation or bootstrapping. This prevents
    information leakage, as instances from the same sample are often
    related – knowing one instance of a sample would make it easy to
    predict another, thus increasing the risk of overfitting. The name
    of the column containing sample identifiers can be specified using
    the `sample_id_column` parameter. If not specified, it is assumed
    that each instance forms a separate sample. In the *colon* dataset,
    the `id` column contains sample identifiers.

-   Within a sample, it is possible to have multiple series, for example
    due to measurements at different locations in the same sample. A
    series differs from repeated measurements. While for series the
    outcome value may change, this is not allowed for repeated
    measurements. The column containing series identifiers may be
    specified by providing the column name as the `series_id_column`
    parameter. If not set, all instances of a sample with a different
    outcome value will be assigned a unique identifier.

-   Within a sample, or series, it is possible to have repeated
    measurements, where one or more feature values may change but the
    outcome value does not. Such instances can for example used to
    assess feature robustness. Repeated measurement identifiers are
    automatically assigned for instances that have the same batch,
    sample and series identifiers.

### Outcome columns

The *colon* dataset also contains two outcome columns: `time` and
`status` that define (censoring) time and survival status respectively.
Survival status are encoded as `0` for alive, censored patients and `1`
for patients that passed away after treatment. Note that these
correspond to default values present in familiar. It is not necessary to
pass these values as `censoring_indicator` and `event_indicator`
parameters.

### Feature columns

The remaining columns in the *colon* dataset represent features. There
are two numeric features, `age` and `nodes`, a categorical feature `rx`
and several categorical and ordinal features encoded with integer
values. Familiar will automatically detect and encode features that
consist of `character`, `logical` or `factor` type. However, it will not
automatically convert the features encoded with integer values. This is
by design – familiar cannot determine whether a feature with integer
values is intended to be a categorical feature or not. Should
categorical features that are encoded with integers be present in your
dataset, you should manually encode such values in the data prior to
passing the data to familiar. For the *colon* dataset, this could be
done as follows:

``` r
# Categorical features
data$sex <- factor(x=data$sex, levels=c(0, 1), labels=c("female", "male"))
data$obstruct <- factor(data$obstruct, levels=c(0, 1), labels=c(FALSE, TRUE))
data$perfor <- factor(data$perfor, levels=c(0, 1), labels=c(FALSE, TRUE))
data$adhere <- factor(data$adhere, levels=c(0, 1), labels=c(FALSE, TRUE))
data$surg <- factor(data$surg, levels=c(0, 1), labels=c("short", "long"))

# Ordinal features
data$differ <- factor(data$differ, levels=c(1, 2, 3), labels=c("well", "moderate", "poor"), ordered=TRUE)
data$extent <- factor(data$extent, levels=c(1, 2, 3, 4), labels=c("submucosa", "muscle",  "serosa", "contiguous_structures"), ordered=TRUE)

knitr::kable(data[1:5])
```

|  id | study | rx      | sex    | age | obstruct | perfor | adhere | nodes | status | differ   | extent | surg  | time |
|----:|------:|:--------|:-------|----:|:---------|:-------|:-------|------:|-------:|:---------|:-------|:------|-----:|
|   1 |     1 | Lev+5FU | male   |  43 | FALSE    | FALSE  | FALSE  |     5 |      1 | moderate | serosa | short |  968 |
|   2 |     1 | Lev+5FU | male   |  63 | FALSE    | FALSE  | FALSE  |     1 |      0 | moderate | serosa | short | 3087 |
|   3 |     1 | Obs     | female |  71 | FALSE    | FALSE  | TRUE   |     7 |      1 | moderate | muscle | short |  542 |
|   4 |     1 | Lev+5FU | female |  66 | TRUE     | FALSE  | FALSE  |     6 |      1 | moderate | serosa | long  |  245 |
|   5 |     1 | Obs     | male   |  69 | FALSE    | FALSE  | FALSE  |    22 |      1 | moderate | serosa | long  |  523 |

Manual encoding also has the advantage that ordinal features can be
specified. Familiar cannot determine whether features with `character`
type values have an associated order and will encode these as regular
categorical variables. Another advantage is that manual encoding allows
for specifying the reference level, i.e. the level to which other levels
of a feature are compared in regression models. Otherwise, the reference
level is taken as the first level after sorting the levels.

## Experimental designs

The experimental design defines how data analysis is performed. Familiar
allows for various designs, from very straightforward training on a
single dataset, to complex nested cross-validation with external
validation. Experimental design is defined using the
`experimental_design` parameter and consists of basic workflow
components and subsampling methods. The basic workflow components are:

-   `fs`: positions the feature selection step. This component should
    always be present, even if `fs_method="none"`. Moreover, note that
    the feature selection step only determines variable importance.
    Actual feature selection takes place after optimisation for model
    hyperparameters determines the optimal number of features.

-   `mb`: positions the model building step. This component should
    always be present.

-   `ev`: positions the external validation step. This should be used in
    conjunction with the `validation_batch_id` parameter to specify
    which batches/cohorts should be used for external validation. Unlike
    `fs` and `mb` components, `ev` is optional.

Each basic workflow component can only appear once in the experimental
design. It is possible to form an experiment using just the basic
workflow components, i.e. `fs+mb` or `fs+mb+ev`. In these experiments,
feature selection is directly followed by modelling, with external
validation of the model on one or more validation cohorts for
`fs+mb+ev`. These options correspond to TRIPOD type 1a and 3,
respectively. TRIPOD analysis types 1b and 2 require more complicated
experimental designs, which are facilitated by subsampling.

Hyperparameter optimisation does not require explicit specification.
Hyperparameter optimisation is conducted when required to determine
variable importance and prior to building a model.

Subsampling methods are used to (randomly) sample the data that are not
used for external validation, and divide these data into internal
development and validation sets. Thus the dataset as a whole is at most
divided into three parts: internal development, internal validation and
external validation. Familiar implements the following subsampling
methods:

-   `bs(x,n)`: (stratified) .632 bootstrap, with `n` the number of
    bootstraps. Bootstrapping randomly samples the data with
    replacement, and on average assigns 63.2% of the samples to the new
    subsampled subset to form the in-bag dataset with the same size as
    the original dataset. Remaining, unselected samples form the
    out-of-bag dataset. All pre-processing steps and hyperparameter
    optimisation (if any) are performed using the in-bag data.

-   `bt(x,n)`: (stratified) .632 bootstrap, with `n` the number of
    bootstraps. Functions like `bs`, but pre-processing parameters and
    hyperparameters (if any) are inherited from the enveloping layer.
    That is, for `bt(fs+mb,20)+ev` twenty bootstraps are created from
    the development dataset, and feature selection and modelling are
    performed on the in-bag data. However, pre-processing parameters and
    hyperparameters are determined on the **main development** dataset.
    The most practical application of `bt` is for repeating feature
    selection multiple times (e.g. `bt(fs,50)+mb+ev`), as this allows
    for aggregating variable importance and reducing the effect of
    random selection.

-   `cv(x,n,p)`: (stratified) `n`-fold cross-validation, repeated `p`
    times. `p` equals 1 by default. Cross-validation randomly assigns
    samples to `n` folds. Cross-validation forms `n` experiments where
    one fold is assigned as a validation fold, and the remainder as
    training folds. All pre-processing steps and hyperparameter
    optimisation (if any) are performed using data in the training
    folds.

-   `lv(x)`: leave-one-out-cross-validation. This is the same as
    `n`-fold cross-validations with `n` the number of samples.

-   `ip(x)`: imbalance partitioning for addressing class imbalances in
    the dataset. This creates subsets of the data with balanced classes
    and can be used in conjunction with `binomial` and `multinomial`
    outcomes. All pre-processing steps and hyperparameter optimisation
    are determined within the partitions. The number of partitions
    generated depends on the imbalance correction method (specified
    using the `imbalance_correction_method` parameter). Imbalance
    partitioning does not generate validation sets.

The `x` argument of subsample methods can contain one or more of the
workflow components. Moreover, it is possible to nest subsample methods.
For example, `experiment_design="cv(bt(fs,50)+mb,5)+ev"` would create a
5-fold cross-validation of the development dataset, with each set of
training folds again subsampled for feature selection. After aggregating
variable importance obtained over 50 bootstraps, a model is trained
within each set of training folds, resulting in 5 models overall. The
ensemble of these models is then evaluated on an external dataset.

Other designs, such as `experiment_design="bs(fs+mb,400)+ev"` allow for
building large ensembles, and capturing the posterior distribution of
the model predictions.

As a final remark: Though it is possible to encapsulate the external
validation (`ev`) workflow component in a subsampler, this is completely
unnecessary. Unlike the feature selection (`fs`) and modelling (`mb`)
components, `ev` is passive, and only indicates whether external
validation should be performed.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Cortes2021-aa" class="csl-entry">

Cortes, David. 2021. *Isotree: Isolation-Based Outlier Detection*.
<https://CRAN.R-project.org/package=isotree>.

</div>

<div id="ref-Royston2013-ch" class="csl-entry">

Royston, Patrick, and Douglas G Altman. 2013. “External Validation of a
Cox Prognostic Model: Principles and Methods.” *BMC Med. Res. Methodol.*
13 (March): 33.

</div>

</div>

<div class="footer">
<br>
<a rel="license" href="https://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="../vignettes/CC4_0_BY_88x31.png" /></a>
This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
Cite as: Alex Zwanenburg. familiar: Vignettes and Documentation (2021). <a href="https://github.com/alexzwanenburg/familiar">https://github.com/alexzwanenburg/familiar</a>
</div>
