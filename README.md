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

* [Introducing familiar](https://cran.r-project.org/web/packages/familiar/vignettes/introduction_precompiled.html)

* [Feature selection methods](https://cran.r-project.org/web/packages/familiar/vignettes/feature_selection_precompiled.html)

* [Learners and hyperparameter optimisation](https://cran.r-project.org/web/packages/familiar/vignettes/learners_precompiled.html)

* [Model performance metrics](https://cran.r-project.org/web/packages/familiar/vignettes/performance_metrics_precompiled.html)

* [Using familiar to evaluate and explain models](https://cran.r-project.org/web/packages/familiar/vignettes/evaluation_and_explanation_precompiled.html)

* [Using familiar prospectively](https://cran.r-project.org/web/packages/familiar/vignettes/prospective_use_precompiled.html)
