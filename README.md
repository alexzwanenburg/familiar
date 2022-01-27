# Familiar <img src="icon/familiar.svg" align="right" width="120"/>

## Overview

Familiar is an R-package that allows for end-to-end machine learning of tabular
data, with subsequent evaluation and explanation of models.

## Installing familiar

Stable versions of familiar can be installed from CRAN. `dependencies=TRUE` prevents being prompted to install packages when using familiar.

    install.packages("familiar", dependencies=TRUE)

Familiar can also be installed directly from the GitHub repository:

    require(devtools)
    devtools::install_github("https://github.com/alexzwanenburg/familiar", dependencies=TRUE)

## More information

The package vignettes contain additional information concerning familiar:

* [Introducing familiar](docs_github/github_introduction.md)

* [Feature selection methods](docs_github/github_feature_selection.md)

* [Learners and hyperparameter optimisation](docs_github/github_learners.md)

* [Model performance metrics](docs_github/github_performance_metrics.md)

* [Using familiar to evaluate and explain models](docs_github/github_evaluation_and_explanation.md)

* [Using familiar prospectively](docs_github/github_prospective_use.md)
