Performance metrics
================
Alex Zwanenburg
2022-02-02

<img src="../vignettes/familiar.svg" align="right" width="120"/>

-   [Categorical outcomes](#categorical-outcomes)
    -   [Area under the receiver-operating
        curve](#area-under-the-receiver-operating-curve)
    -   [Brier score](#brier-score)
    -   [Contingency table-based
        metrics](#contingency-table-based-metrics)
        -   [Accuracy](#accuracy)
        -   [Balanced accuracy](#balanced-accuracy)
        -   [Balanced error rate](#balanced-error-rate)
        -   [F1 score](#f1-score)
        -   [False discovery rate](#false-discovery-rate)
        -   [Informedness](#informedness)
        -   [Cohen’s kappa](#cohens-kappa)
        -   [Markedness](#markedness)
        -   [Matthews correlation
            efficient](#matthews-correlation-efficient)
        -   [Negative predictive value](#negative-predictive-value)
        -   [Positive predictive value](#positive-predictive-value)
        -   [Recall](#recall)
        -   [Specificity](#specificity)
        -   [Youden’s J statistic](#youdens-j-statistic)
-   [Regression outcomes](#regression-outcomes)
    -   [Explained variance](#explained-variance)
    -   [Mean absolute error](#mean-absolute-error)
    -   [Mean log absolute error](#mean-log-absolute-error)
    -   [Mean squared error](#mean-squared-error)
    -   [Mean squared log error](#mean-squared-log-error)
    -   [Median absolute error](#median-absolute-error)
    -   [R<sup>2</sup> score](#r2-score)
    -   [Root mean square error](#root-mean-square-error)
    -   [Root mean square log error](#root-mean-square-log-error)
-   [Survival outcomes](#survival-outcomes)
    -   [Concordance index](#concordance-index)
-   [References](#references)

When we train a model, we usually want to know how good the model is.
Model performance is assessed using different metrics that quantify how
well a model discriminates cases, stratifies groups or predicts values.
`familiar` implements metrics that are typically used to assess the
performance of categorical, regression and survival models.

# Categorical outcomes

Performance metrics for models with categorical outcomes,
i.e. `binomial` and `multinomial` are listed below.

| **method**                              | **tag**                                              | **averaging** |
|:----------------------------------------|:-----------------------------------------------------|:-------------:|
| accuracy                                | `accuracy`                                           |               |
| area under the receiver-operating curve | `auc`, `auc_roc`                                     |               |
| balanced accuracy                       | `bac`, `balanced_accuracy`                           |               |
| balanced error rate                     | `ber`, `balanced_error_rate`                         |               |
| Brier score                             | `brier`                                              |               |
| Cohen’s kappa                           | `kappa`, `cohen_kappa`                               |               |
| f1 score                                | `f1_score`                                           |       ×       |
| false discovery rate                    | `fdr`, `false_discovery_rate`                        |       ×       |
| informedness                            | `informedness`                                       |       ×       |
| markedness                              | `markedness`                                         |       ×       |
| Matthews’ correlation coefficient       | `mcc`, `matthews_correlation_coefficient`            |               |
| negative predictive value               | `npv`                                                |       ×       |
| positive predictive value               | `precision`, `ppv`                                   |       ×       |
| recall                                  | `recall`, `sensitivity`, `tpr`, `true_positive_rate` |       ×       |
| specificity                             | `specificity`, `tnr`, `true_negative_rate`           |       ×       |
| Youden’s J                              | `youden_j`, `youden_index`                           |       ×       |

Overview of performance metrics for categorical outcomes. Some
contingency table-based metrics require averaging for `multinomial`
outcomes as their original definition only covers `binomial` problems
with two classes.

## Area under the receiver-operating curve

The area under the receiver-operating curve is quite literally that. It
is the area under the curve created by plotting the true positive rate
(sensitivity) against the false positive rate (1-specificity). TPR and
FPR are derived from a contingency table, which is created by comparing
predicted class probabilities against a threshold. The
receiver-operating curve is created by iterating over1 threshold values.
The AUC of a model that predicts perfectly is 1.0, while 0.5 indicates
predictions that are no better than random.

The implementation in `familiar` does not use the ROC curve to compute
the AUC. Instead, an algebraic equation by Hand and Till (2001) is used.
For `multinomial` outcomes the AUC is computed for each pairwise
comparison of outcome classes, and averaged (Hand and Till 2001).

## Brier score

The Brier score (Brier 1950) is a measure of deviation of predicted
probabilities from the ideal situation where the probability for class
*a* is 1.0 if the observed class is *a* and 0.0 if it is not *a*. Hence,
it can be viewed as a measure of calibration as well. A value of 0.0 is
optimal.

The implementation in `familiar` iterates over all outcome classes in a
one-versus-all approach, as originally devised by Brier (1950). For
`binomial` outcomes, the score is divided by 2, so that it falls in the
\[0.0,1.0\] range.

## Contingency table-based metrics

A contingency table or confusion matrix displays the observed and
predicted classes. When dealing with two classes, e.g. *a* and *b*, one
of the classes is usually termed ‘positive’ and the other ‘negative.’
For example, let *b* be the ‘positive’ class. Then we can define the
following four categories:

-   True positive (*T**P*): *b* is predicted and observed.
-   True negative (*T**N*): *a* is predicted and observed.
-   False negative (*F**N*): *b* was observed, but *a* was predicted.
-   False positive (*F**P*): *a* was observed, but *b* was predicted.

A contingency table contains the occurrence of each of the four cases.
If a model is good, most samples will be either true positive or true
negative. Models that are not as good may have larger numbers of false
positives and/or false negatives.

Metrics based on the contingency table use two or more of the four
categories to characterise model performance. The extension from two
classes (`binomial`) to more (`multinomial`) is often not trivial. For
many metrics, `familiar` uses a one-versus-all approach. Here, all
classes are iteratively used as the ‘positive’ class, with the rest
grouped as ‘negative.’ Three options can be used to obtain performance
values for `multinomial` problems, with an implementation similar to
that of `scikit.learn`:

-   `micro`: The number of true positives, true negatives, false
    positives and false negatives are computed for each class iteration,
    and then summed over all classes. The score is calculated
    afterwards.

-   `macro`: A score is computed for each class iteration, and then
    averaged.

-   `weighted`: A score is computed for each class iteration, and the
    averaged with a weight corresponding to the number of samples with
    the observed ‘positive’ class, i.e. the prevalence.

By default, `familiar` uses `macro`, but the averaging procedure may be
selected through appending `_micro`, `_macro` or `_weighted` to the name
of the metric. For example, `recall_micro` will compute the recall
metric using `micro` averaging.

Averaging only applies to `multinomial` outcomes. No averaging is
performed for `binomial` problems with two classes. In this case
`familiar` will always consider the second class level to correspond to
the ‘positive’ class.

### Accuracy

Accuracy quantifies the number of correctly predicted classes:
*s*<sub>*a**c**c*</sub> = (*T**P*+*T**N*)/(*T**P*+*T**N*+*F**P*+*F**N*).
The extension to more than 2 classes is trivial. No averaging is
performed for the accuracy metric.

### Balanced accuracy

Accuracy is known to be sensitive to imbalances in the class
distribution. A balanced accuracy was therefore defined (Brodersen et
al. 2010), which is the averaged within-class true positive rate (also
known as recall or sensitivity):
*s*<sub>*b**a**c*</sub> = 0.5(*T**P*/(*T**P*+*F**N*)+*T**N*/(*T**N*+*F**P*)).

The extension to more than 2 classes involves summation of in-class true
positive rate *T**P*/(*T**P*+*F**N*) for each positive class and
subsequent division by the number of classes. No averaging is performed
for balanced accuracy.

### Balanced error rate

The balanced error rate is closely related to balanced accuracy,
i.e. instead of the in-class true positive rate, the in-class false
negative rate is used:
*s*<sub>*b**e**r*</sub> = 0.5(*F**N*/(*T**P*+*F**N*)+*F**P*/(*T**N*+*F**P*)).

The extension to more than 2 classes involves summation of in-class
false negative rate *F**N*/(*T**P*+*F**N*) for each positive class and
subsequent division by the number of classes. No averaging is performed
for balanced error rate.

### F1 score

The F1 score is the harmonic mean of precision and sensitivity:
*s*<sub>*f*1</sub> = 2 *T**P*/(2 *T**P*+*F**P*+*F**N*).

The metric is not invariant to class permutation. Averaging is therefore
performed for `multinomial` outcomes.

### False discovery rate

The false discovery rate quantifies the proportion of false positives
among all predicted positives, i.e. the Type I error:
*s*<sub>*f**d**r*</sub> = *F**P*/(*T**P*+*F**P*).

The metric is not invariant to class permutation. Averaging is therefore
performed for `multinomial` outcomes.

### Informedness

Informedness is a generalisation of Youden’s J statistic (Powers 2011).
Informedness can be extended to multiple classes, and no averaging is
therefore required.

For `binomial` problems, informedness and the Youden J statistic are the
same.

### Cohen’s kappa

Cohen’s kappa coefficient is a measure of correspondence between the
observed and predicted classes (Cohen 1960). Cohen’s kappa coefficient
is invariant to class permutations and no averaging is performed for
Cohen’s kappa.

### Markedness

Markedness is related to the precision or positive predictive value
(Powers 2011).

### Matthews correlation efficient

Matthews’ correlation coefficient measures the correlation between
observed and predicted classes \[Matthews1975-kh\].

An extension to multiple classes, i.e. multinomial outcomes, was devised
by Gorodkin (2004).

### Negative predictive value

The negative predictive value (NPV) is the fraction of predicted
negative classes that were also observed to be negative:
*s*<sub>*n**p**v*</sub> = *T**N*/(*T**N*+*F**N*).

The NPV is not invariant to class permutations. Averaging is performed
for `multinomial` outcomes.

### Positive predictive value

The positive predictive value (PPV) is the fraction of predicted
positive classes that were also observed to be positive:
*s*<sub>*p**p**v*</sub> = *T**P*/(*T**P*+*F**P*).

The PPV is also referred to as precision. The PPV is not invariant to
class permutations. Averaging is performed for `multinomial` outcomes.
`micro`-averaging effectively computes the accuracy.

### Recall

Recall, also known as sensitivity or true positive rate, is the fraction
of observed positive classes that were also predicted to be positive:
*s*<sub>*r**e**c**a**l**l*</sub> = *T**P*/(*T**P*+*F**N*).

Recall is not invariant to class permutations and averaging is performed
for `multinomial` outcomes. Both `micro` and `weighted` averaging
effectively compute the accuracy.

### Specificity

Specificity, also known as the true negative rate, is the fraction of
observed negative classes that were also predicted to be negative:
*s*<sub>*s**p**e**c*</sub> = *T**N*/(*T**N*+*F**P*).

Specificity is not invariant to class permutations and averaging is
performed for `multinomial` outcomes.

### Youden’s J statistic

Youden’s J statistic (Youden 1950) is the sum of recall and specificity
minus 1:
*s*<sub>*y**o**u**d**e**n*</sub> = *T**P*/(*T**P*+*F**N*) + *T**N*/(*T**N*+*F**P*) − 1.

Youden’s J statistic is not invariant to class permutations and
averaging is performed for `multinomial` outcomes.

For `binomial` problems, informedness and the Youden J statistic are the
same.

# Regression outcomes

Performance metrics for models with regression outcomes, i.e. `count`
and `continuous`, are listed below.

| **method**                 | **tag**                               |
|:---------------------------|:--------------------------------------|
| explained variance         | `explained_variance`                  |
| mean absolute error        | `mae`, `mean_absolute_error`          |
| mean log absolute error    | `mlae`, `mean_log_absolute_error`     |
| mean squared error         | `mse`, `mean_squared_error`           |
| mean squared log error     | `msle`, `mean_squared_log_error`      |
| median absolute error      | `medae`, `median_absolute_error`      |
| R<sup>2</sup> score        | `r2_score`, `r_squared`               |
| root mean square error     | `rmse`, `root_mean_square_error`      |
| root mean square log error | `rmsle`, `root_mean_square_log_error` |

Let *y* be the set of observed values, and *ŷ* the corresponding
predicted values. The error is then *ϵ* = *y* − *ŷ*.

## Explained variance

The explained variance is defined as 1 − Var(*ϵ*)/Var(*y*). This metric
is not sensitive to differences in offset between observed and predicted
values.

## Mean absolute error

The mean absolute error is defined as
$1/N \\sum_i^N \\left\|\\epsilon_i\\right\|$, with *N* the number of
samples.

## Mean log absolute error

The mean log absolute error is defined as
$1/N \\sum_i^N \\log(\\left\|\\epsilon_i\\right\| + 1)$.

## Mean squared error

The mean squared error is defined as
$1/N \\sum_i^N \\left(\\epsilon_i \\right)^2$.

## Mean squared log error

Mean squared log error is defined as
$1/N \\sum_i^N \\left(\\log \\left(y_i + 1\\right) - \\log\\left(\\hat{y}\_i + 1\\right)\\right)^2$.
Note that this score only applies to observed and predicted values in
the positive domain. It is not defined for negative values.

## Median absolute error

The median absolute error is the median of absolute error \|*ϵ*\|.

## R<sup>2</sup> score

The R<sup>2</sup> score is defined as:
$$R^2 = 1 - \\frac{\\sum_i^N(\\epsilon_i)^2}{\\sum_i^N(y_i - \\bar{y})^2}$$
Here *ȳ* denotes the mean value of *y*.

## Root mean square error

The root mean square error is defined as
$\\sqrt{1/N \\sum_i^N \\left(\\epsilon_i \\right)^2}$.

## Root mean square log error

The root mean square log error is defined as
$\\sqrt{1/N \\sum_i^N \\left(\\log \\left(y_i + 1\\right) - \\log\\left(\\hat{y}\_i + 1\\right)\\right)^2}$.
Note that this score only applies to observed and predicted values in
the positive domain. It is not defined for negative values.

# Survival outcomes

Performance metrics for models with survival outcomes, i.e. `survival`,
are listed below.

| **method**        | **tag**                                                                        |
|:------------------|:-------------------------------------------------------------------------------|
| concordance index | `concordance_index`, `c_index`, `concordance_index_harrell`, `c_index_harrell` |

## Concordance index

The concordance index assesses ordering between observed and predicted
values. Let *T* be observed times, *c* the censoring status (0: no
observed event; 1: event observed) and *T̂* predicted times. Concordance
between all pairs of values is determined as follows (Pencina and
D’Agostino 2004):

-   Concordant: a pair is concordant if
    *T*<sub>*i*</sub> \< *T*<sub>*j*</sub> and
    *T̂*<sub>*i*</sub> \< *T̂*<sub>*j*</sub> (provided
    *c*<sub>*i*</sub> = 1), or if *T*<sub>*i*</sub> \> *T*<sub>*j*</sub>
    and *T̂*<sub>*i*</sub> \> *T̂*<sub>*j*</sub> (provided
    *c*<sub>*j*</sub> = 1).
-   Discordant: a pair is discordant if
    *T*<sub>*i*</sub> \< *T*<sub>*j*</sub> and
    *T̂*<sub>*i*</sub> \> *T̂*<sub>*j*</sub> (provided
    *c*<sub>*i*</sub> = 1), or if *T*<sub>*i*</sub> \> *T*<sub>*j*</sub>
    and *T̂*<sub>*i*</sub> \< *T̂*<sub>*j*</sub> (provided
    *c*<sub>*j*</sub> = 1).
-   Tied: a pair is tied if *T̂*<sub>*i*</sub> = *T̂*<sub>*j*</sub>,
    provided that *c*<sub>*i*</sub> = *c*<sub>*j*</sub> = 1.
-   Not comparable: otherwise. This occurs, for example, if sample *i*
    was censored before an event was observed in sample *j*, or both
    samples were censored.

The concordance index is then computed as:
$$ci = \\frac{n\_{concord} + 0.5 n\_{tied}}{n\_{concord} + n\_{discord} + n\_{tied}}$$

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Brier1950-lo" class="csl-entry">

Brier, Glenn W. 1950. “Verification of Forecasts Expressed in Terms of
Probability.” *Mon. Weather Rev.* 78 (1): 1–3.

</div>

<div id="ref-Brodersen2010-vb" class="csl-entry">

Brodersen, K H, C S Ong, K E Stephan, and J M Buhmann. 2010. “The
Balanced Accuracy and Its Posterior Distribution.” In *2010 20th
International Conference on Pattern Recognition*, 3121–24.

</div>

<div id="ref-Cohen1960-kc" class="csl-entry">

Cohen, Jacob. 1960. “A Coefficient of Agreement for Nominal Scales.”
*Educ. Psychol. Meas.* 20 (1): 37–46.

</div>

<div id="ref-Gorodkin2004-tx" class="csl-entry">

Gorodkin, J. 2004. “Comparing Two k-Category Assignments by a k-Category
Correlation Coefficient.” *Comput. Biol. Chem.* 28 (5-6): 367–74.

</div>

<div id="ref-Hand2001-ij" class="csl-entry">

Hand, David J, and Robert J Till. 2001. “A Simple Generalisation of the
Area Under the ROC Curve for Multiple Class Classification Problems.”
*Mach. Learn.* 45 (2): 171–86.

</div>

<div id="ref-Pencina2004-ii" class="csl-entry">

Pencina, Michael J, and Ralph B D’Agostino. 2004. “Overall C as a
Measure of Discrimination in Survival Analysis: Model Specific
Population Value and Confidence Interval Estimation.” *Stat. Med.* 23
(13): 2109–23.

</div>

<div id="ref-Powers2011-jt" class="csl-entry">

Powers, David Martin. 2011. “Evaluation: From Precision, Recall and
f-Measure to ROC, Informedness, Markedness and Correlation.”
*International Journal of Machine Learning Technology* 2 (1): 37–63.

</div>

<div id="ref-Youden1950-no" class="csl-entry">

Youden, W J. 1950. “Index for Rating Diagnostic Tests.” *Cancer* 3 (1):
32–35.

</div>

</div>

<div class="footer">
<br>
<a rel="license" href="https://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="../vignettes/CC4_0_BY_88x31.png" /></a>
This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
Cite as: Alex Zwanenburg. familiar: Vignettes and Documentation (2021). <a href="https://github.com/alexzwanenburg/familiar">https://github.com/alexzwanenburg/familiar</a>
</div>
