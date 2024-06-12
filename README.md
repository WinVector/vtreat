
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![DOI](https://joss.theoj.org/papers/10.21105/joss.00584/status.svg)](https://doi.org/10.21105/joss.00584)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1173313.svg)](https://doi.org/10.5281/zenodo.1173313)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vtreat)](https://cran.r-project.org/package=vtreat)
[![status](https://tinyverse.netlify.com/badge/vtreat)](https://CRAN.R-project.org/package=vtreat)

`vtreat` is a `data.frame` processor/conditioner (available [for
`R`](https://github.com/WinVector/vtreat), and [for
`Python`](https://github.com/WinVector/pyvtreat)) that prepares
real-world data for supervised machine learning or predictive modeling
in a statistically sound manner.

A nice video lecture on what sorts of problems `vtreat` solves can be
found [here](https://youtu.be/sniHkkrAsOc?t=42).

`vtreat` takes an input `data.frame` that has a specified column called
“the outcome variable” (or “y”) that is the quantity to be predicted
(and must not have missing values). Other input columns are possible
explanatory variables (typically numeric or categorical/string-valued,
these columns may have missing values) that the user later wants to use
to predict “y”. In practice such an input `data.frame` may not be
immediately suitable for machine learning procedures that often expect
only numeric explanatory variables, and may not tolerate missing values.

To solve this, `vtreat` builds a transformed `data.frame` where all
explanatory variable columns have been transformed into a number of
numeric explanatory variable columns, without missing values. The
`vtreat` implementation produces derived numeric columns that capture
most of the information relating the explanatory columns to the
specified “y” or dependent/outcome column through a number of numeric
transforms (indicator variables, impact codes, prevalence codes, and
more). This transformed `data.frame` is suitable for a wide range of
supervised learning methods from linear regression, through gradient
boosted machines.

The idea is: you can take a `data.frame` of messy real world data and
easily, faithfully, reliably, and repeatably prepare it for machine
learning using documented methods using `vtreat`. Incorporating `vtreat`
into your machine learning workflow lets you quickly work with very
diverse structured data.

In all cases (classification, regression, unsupervised, and multinomial
classification) the intent is that `vtreat` transforms are essentially
one liners.

The preparation commands are organized as follows:

- **Regression**: [`R` regression example, fit/prepare
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression_FP.md),
  [`R` regression example, design/prepare/experiment
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression.md),
  [`Python` regression
  example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Regression/Regression.md).
- **Classification**: [`R` classification example, fit/prepare
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FP.md),
  [`R` classification example, design/prepare/experiment
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.md),
  [`Python` classification
  example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Classification/Classification.md).
- **Unsupervised tasks**: [`R` unsupervised example, fit/prepare
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised_FP.md),
  [`R` unsupervised example, design/prepare/experiment
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised.md),
  [`Python` unsupervised
  example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Unsupervised/Unsupervised.md).
- **Multinomial classification**: [`R` multinomial classification
  example, fit/prepare
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample_FP.md),
  [`R` multinomial classification example, design/prepare/experiment
  interface](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md),
  [`Python` multinomial classification
  example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Multinomial/MultinomialExample.md).

In all cases: variable preparation is intended to be a “one liner.”

These current revisions of the examples are designed to be small, yet
complete. So as a set they have some overlap, but the user can rely
mostly on a single example for a single task type.

For more detail please see here: [arXiv:1611.09477
stat.AP](https://arxiv.org/abs/1611.09477) (the documentation describes
the `R` version, however all of the examples can be found worked in
`Python`
[here](https://github.com/WinVector/pyvtreat/tree/master/Examples/vtreat_paper1)).

`vtreat` is available as an [`R`
package](https://github.com/WinVector/vtreat), and also as a
[`Python`/`Pandas` package](https://github.com/WinVector/vtreat).

![](https://github.com/WinVector/vtreat/raw/master/tools/vtreat.png)

(logo: Julie Mount, source: “The Harvest” by Boris Kustodiev 1914)

Even with modern machine learning techniques (random forests, support
vector machines, neural nets, gradient boosted trees, and so on) or
standard statistical methods (regression, generalized regression,
generalized additive models) there are *common* data issues that can
cause modeling to fail. vtreat deals with a number of these in a
principled and automated fashion.

In particular vtreat emphasizes a concept called “y-aware
pre-processing” and implements:

- Treatment of missing values through safe replacement plus indicator
  column (a simple but very powerful method when combined with
  downstream machine learning algorithms).
- Treatment of novel levels (new values of categorical variable seen
  during test or application, but not seen during training) through
  sub-models (or impact/effects coding of pooled rare events).
- Explicit coding of categorical variable levels as new indicator
  variables (with optional suppression of non-significant indicators).
- Treatment of categorical variables with very large numbers of levels
  through sub-models (again [impact/effects
  coding](https://win-vector.com/2012/07/23/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)).
- (optional) User specified significance pruning on levels coded into
  effects/impact sub-models.
- Correct treatment of nested models or sub-models through data split
  (see
  [here](https://winvector.github.io/vtreat/articles/vtreatOverfit.html))
  or through the generation of “cross validated” data frames (see
  [here](https://winvector.github.io/vtreat/articles/vtreatCrossFrames.html));
  these are issues similar to what is required to build statistically
  efficient stacked models or super-learners).
- Safe processing of “wide data” (data with very many variables, often
  driving common machine learning algorithms to over-fit) through [out
  of sample per-variable significance estimates and user controllable
  pruning](https://winvector.github.io/vtreat/articles/vtreatSignificance.html)
  (something we have lectured on previously
  [here](https://github.com/WinVector/WinVector.github.io/tree/master/DS)
  and
  [here](https://win-vector.com/2014/02/01/bad-bayes-an-example-of-why-you-need-hold-out-testing/)).
- Collaring/Winsorizing of unexpected out of range numeric inputs.
- (optional) Conversion of all variables into effects (or “y-scale”)
  units (through the optional `scale` argument to `vtreat::prepare()`,
  using some of the ideas discussed
  [here](https://win-vector.com/2014/06/02/skimming-statistics-papers-for-the-ideas-instead-of-the-complete-procedures/)).
  This allows correct/sensible application of principal component
  analysis pre-processing in a machine learning context.
- Joining in additional training distribution data (which can be useful
  in analysis, called “catP” and “catD”).

The idea is: even with a sophisticated machine learning algorithm there
are *many* ways messy real world data can defeat the modeling process,
and vtreat helps with at least ten of them. We emphasize: these problems
are already in your data, you simply build better and more reliable
models if you attempt to mitigate them. Automated processing is no
substitute for actually looking at the data, but vtreat supplies
efficient, reliable, documented, and tested implementations of many of
the commonly needed transforms.

To help explain the methods we have prepared some documentation:

- The [vtreat package
  overall](https://winvector.github.io/vtreat/index.html).
- [Preparing data for analysis using R
  white-paper](https://winvector.github.io/DataPrep/EN-CNTNT-Whitepaper-Data-Prep-Using-R.pdf)
- The [types of new
  variables](https://winvector.github.io/vtreat/articles/vtreatVariableTypes.html)
  introduced by vtreat processing (including how to limit down to domain
  appropriate variable types).
- Statistically sound treatment of the nested modeling issue introduced
  by any sort of pre-processing (such as vtreat itself): [nested
  over-fit
  issues](https://winvector.github.io/vtreat/articles/vtreatOverfit.html)
  and a general [cross-frame
  solution](https://winvector.github.io/vtreat/articles/vtreatCrossFrames.html).
- [Principled ways to pick significance based pruning
  levels](https://winvector.github.io/vtreat/articles/vtreatSignificance.html).

Data treatments are “y-aware” (use distribution relations between
independent variables and the dependent variable). For binary
classification use `designTreatmentsC()` and for numeric regression use
`designTreatmentsN()`.

After the design step, `prepare()` should be used as you would use
model.matrix. `prepare()` treated variables are all numeric and never
take the value NA or +-Inf (so are very safe to use in modeling).

In application we suggest splitting your data into three sets: one for
building vtreat encodings, one for training models using these
encodings, and one for test and model evaluation.

The purpose of `vtreat` library is to reliably prepare data for
supervised machine learning. We try to leave as much as possible to the
machine learning algorithms themselves, but cover most of the truly
necessary typically ignored precautions. The library is designed to
produce a `data.frame` that is entirely numeric and takes common
precautions to guard against the following real world data issues:

- Categorical variables with very many levels.

  We re-encode such variables as a family of indicator or dummy
  variables for common levels plus an additional [impact
  code](https://win-vector.com/2012/07/23/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)
  (also called “effects coded”). This allows principled use (including
  smoothing) of huge categorical variables (like zip-codes) when
  building models. This is critical for some libraries (such as
  `randomForest`, which has hard limits on the number of allowed
  levels).

- Rare categorical levels.

  Levels that do not occur often during training tend not to have
  reliable effect estimates and contribute to over-fit. vtreat helps
  with 2 precautions in this case. First the `rareLevel` argument
  suppresses levels with this count our below from modeling, except
  possibly through a grouped contribution. Also with enough data vtreat
  attempts to estimate out of sample performance of derived variables.
  Finally we suggest users reserve a portion of data for vtreat design,
  separate from any data used in additional training, calibration, or
  testing.

- Novel categorical levels.

  A common problem in deploying a classifier to production is: new
  levels (levels not seen during training) encountered during model
  application. We deal with this by encoding categorical variables in a
  possibly redundant manner: reserving a dummy variable for all levels
  (not the more common all but a reference level scheme). This is in
  fact the correct representation for regularized modeling techniques
  and lets us code novel levels as all dummies simultaneously zero
  (which is a reasonable thing to try). This encoding while limited is
  cheaper than the fully Bayesian solution of computing a weighted sum
  over previously seen levels during model application.

- Missing/invalid values NA, NaN, +-Inf.

  Variables with these issues are re-coded as two columns. The first
  column is clean copy of the variable (with missing/invalid values
  replaced with either zero or the grand mean, depending on the user
  chose of the `scale` parameter). The second column is a dummy or
  indicator that marks if the replacement has been performed. This is
  simpler than imputation of missing values, and allows the downstream
  model to attempt to use missingness as a useful signal (which it often
  is in industrial data).

- Extreme values.

  Variables can be restricted to stay in ranges seen during training.
  This can defend against some run-away classifier issues during model
  application.

- Constant and near-constant variables.

  Variables that “don’t vary” or “nearly don’t vary” are suppressed.

- Need for estimated single-variable model effect sizes and
  significances.

  It is a dirty secret that even popular machine learning techniques
  need some variable pruning (when exposed to very wide data frames, see
  [here](https://win-vector.com/2014/02/01/bad-bayes-an-example-of-why-you-need-hold-out-testing/)
  and [here](https://www.youtube.com/watch?v=X_Rn3EOEjGE)). We make the
  necessary effect size estimates and significances easily available and
  supply initial variable pruning.

The above are all awful things that often lurk in real world data.
Automating these steps ensures they are easy enough that you actually
perform them and leaves the analyst time to look for additional data
issues. For example this allowed us to essentially automate a number of
the steps taught in chapters 4 and 6 of [*Practical Data Science with R*
(Zumel, Mount; Manning
2014)](https://win-vector.com/practical-data-science-with-r/) into a
[very short
worksheet](https://winvector.github.io/KDD2009/KDD2009RF.html) (though
we think for understanding it is *essential* to work all the steps by
hand as we did in the book). The 2nd edition of *Practical Data Science
with R* covers using `vtreat` in `R` in chapter 8 “Advanced Data
Preparation.”

The idea is: `data.frame`s prepared with the `vtreat` library are
somewhat safe to train on as some precaution has been taken against all
of the above issues. Also of interest are the `vtreat` variable
significances (help in initial variable pruning, a necessity when there
are a large number of columns) and `vtreat::prepare(scale=TRUE)` which
re-encodes all variables into effect units making them suitable for
y-aware dimension reduction (variable clustering, or principal component
analysis) and for geometry sensitive machine learning techniques
(k-means, knn, linear SVM, and more). You may want to do more than the
`vtreat` library does (such as Bayesian imputation, variable clustering,
and more) but you certainly do not want to do less.

There have been a number of recent substantial improvements to the
library, including:

- Out of sample scoring.
- Ability to use `parallel`.
- More general calculation of effect sizes and significances.

Some of our related articles (which should make clear some of our
motivations, and design decisions):

- [Modeling trick: impact coding of categorical variables with many
  levels](https://win-vector.com/2012/07/23/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)
- [A bit more on impact
  coding](https://win-vector.com/2012/08/02/a-bit-more-on-impact-coding/)
- [vtreat: designing a package for variable
  treatment](https://win-vector.com/2014/08/07/vtreat-designing-a-package-for-variable-treatment/)
- [A comment on preparing data for
  classifiers](https://win-vector.com/2014/12/04/a-comment-on-preparing-data-for-classifiers/)
- [Nina Zumel presenting on
  vtreat](https://www.slideshare.net/ChesterChen/vtreat)
- [What is new in the vtreat
  library?](https://win-vector.com/2015/05/07/what-is-new-in-the-vtreat-library/)
- [How do you know if your data has
  signal?](https://win-vector.com/2015/08/10/how-do-you-know-if-your-data-has-signal/)

Examples of current best practice using `vtreat` (variable coding,
train, test split) can be found
[here](https://winvector.github.io/vtreat/articles/vtreatOverfit.html)
and [here](https://winvector.github.io/KDD2009/KDD2009RF.html).

Some small examples:

We attach our packages.

``` r
library("vtreat")
 #  Loading required package: wrapr
packageVersion("vtreat")
 #  [1] '1.6.5'
citation('vtreat')
 #  To cite package 'vtreat' in publications use:
 #  
 #    Mount J, Zumel N (2024). _vtreat: A Statistically Sound 'data.frame'
 #    Processor/Conditioner_. https://github.com/WinVector/vtreat/,
 #    https://winvector.github.io/vtreat/.
 #  
 #  A BibTeX entry for LaTeX users is
 #  
 #    @Manual{,
 #      title = {vtreat: A Statistically Sound 'data.frame' Processor/Conditioner},
 #      author = {John Mount and Nina Zumel},
 #      year = {2024},
 #      note = {https://github.com/WinVector/vtreat/, https://winvector.github.io/vtreat/},
 #    }
```

A small categorical example.

``` r
# categorical example
set.seed(23525)

# we set up our raw training and application data
dTrainC <- data.frame(
  x = c('a', 'a', 'a', 'b', 'b', NA, NA),
  z = c(1, 2, 3, 4, NA, 6, NA),
  y = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE))

dTestC <- data.frame(
  x = c('a', 'b', 'c', NA), 
  z = c(10, 20, 30, NA))

# we perform a vtreat cross frame experiment
# and unpack the results into treatmentsC
# and dTrainCTreated
unpack[
  treatmentsC = treatments,
  dTrainCTreated = crossFrame
  ] <- mkCrossFrameCExperiment(
  dframe = dTrainC,
  varlist = setdiff(colnames(dTrainC), 'y'),
  outcomename = 'y',
  outcometarget = TRUE,
  verbose = FALSE)

# the treatments include a score frame relating new
# derived variables to original columns
treatmentsC$scoreFrame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees', 'recommended')] %.>%
  knitr::kable(.)
```

| origName | varName   | code  |       rsq |       sig | extraModelDegrees | recommended |
|:---------|:----------|:------|----------:|----------:|------------------:|:------------|
| x        | x_catP    | catP  | 0.1669568 | 0.2064389 |                 2 | FALSE       |
| x        | x_catB    | catB  | 0.2547883 | 0.1185814 |                 2 | TRUE        |
| z        | z         | clean | 0.2376018 | 0.1317602 |                 0 | TRUE        |
| z        | z_isBAD   | isBAD | 0.2960654 | 0.0924840 |                 0 | TRUE        |
| x        | x_lev_NA  | lev   | 0.2960654 | 0.0924840 |                 0 | FALSE       |
| x        | x_lev_x_a | lev   | 0.1300057 | 0.2649038 |                 0 | FALSE       |
| x        | x_lev_x_b | lev   | 0.0060673 | 0.8096724 |                 0 | FALSE       |

``` r

# the treated frame is a "cross frame" which
# is a transform of the training data built 
# as if the treatment were learned on a different
# disjoint training set to avoid nested model
# bias and over-fit.
dTrainCTreated %.>%
  head(.) %.>%
  knitr::kable(.)
```

| x_catP |      x_catB |   z | z_isBAD | x_lev_NA | x_lev_x_a | x_lev_x_b | y     |
|-------:|------------:|----:|--------:|---------:|----------:|----------:|:------|
|   0.50 |   0.0000000 |   1 |       0 |        0 |         1 |         0 | FALSE |
|   0.40 |  -0.4054484 |   2 |       0 |        0 |         1 |         0 | FALSE |
|   0.40 | -10.3089860 |   3 |       0 |        0 |         1 |         0 | TRUE  |
|   0.20 |   8.8049919 |   4 |       0 |        0 |         0 |         1 | FALSE |
|   0.25 |  -9.2104404 |   3 |       1 |        0 |         0 |         1 | TRUE  |
|   0.25 |   9.2104404 |   6 |       0 |        1 |         0 |         0 | TRUE  |

``` r

# Any future application data is prepared with
# the prepare method.
dTestCTreated <- prepare(treatmentsC, dTestC, pruneSig=NULL)

dTestCTreated %.>%
  head(.) %.>%
  knitr::kable(.)
```

|    x_catP |     x_catB |    z | z_isBAD | x_lev_NA | x_lev_x_a | x_lev_x_b |
|----------:|-----------:|-----:|--------:|---------:|----------:|----------:|
| 0.4285714 | -0.9807709 | 10.0 |       0 |        0 |         1 |         0 |
| 0.2857143 | -0.2876737 | 20.0 |       0 |        0 |         0 |         1 |
| 0.0714286 |  0.0000000 | 30.0 |       0 |        0 |         0 |         0 |
| 0.2857143 |  9.6158638 |  3.2 |       1 |        1 |         0 |         0 |

A small numeric example.

``` r
# numeric example
set.seed(23525)

# we set up our raw training and application data
dTrainN <- data.frame(
  x = c('a', 'a', 'a', 'a', 'b', 'b', NA, NA),
  z = c(1, 2, 3, 4, 5, NA, 7, NA), 
  y = c(0, 0, 0, 1, 0, 1, 1, 1))

dTestN <- data.frame(
  x = c('a', 'b', 'c', NA), 
  z = c(10, 20, 30, NA))

# we perform a vtreat cross frame experiment
# and unpack the results into treatmentsN
# and dTrainNTreated
unpack[
  treatmentsN = treatments,
  dTrainNTreated = crossFrame
  ] <- mkCrossFrameNExperiment(
  dframe = dTrainN,
  varlist = setdiff(colnames(dTrainN), 'y'),
  outcomename = 'y',
  verbose = FALSE)

# the treatments include a score frame relating new
# derived variables to original columns
treatmentsN$scoreFrame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees')] %.>%
  knitr::kable(.)
```

| origName | varName   | code  |       rsq |       sig | extraModelDegrees |
|:---------|:----------|:------|----------:|----------:|------------------:|
| x        | x_catP    | catP  | 0.4047085 | 0.0899406 |                 2 |
| x        | x_catN    | catN  | 0.2822908 | 0.1753958 |                 2 |
| x        | x_catD    | catD  | 0.0209693 | 0.7322571 |                 2 |
| z        | z         | clean | 0.2880952 | 0.1701892 |                 0 |
| z        | z_isBAD   | isBAD | 0.3333333 | 0.1339746 |                 0 |
| x        | x_lev_NA  | lev   | 0.3333333 | 0.1339746 |                 0 |
| x        | x_lev_x_a | lev   | 0.2500000 | 0.2070312 |                 0 |

``` r

# the treated frame is a "cross frame" which
# is a transform of the training data built 
# as if the treatment were learned on a different
# disjoint training set to avoid nested model
# bias and over-fit.
dTrainNTreated %.>%
  head(.) %.>%
  knitr::kable(.)
```

|     x_catN |    x_catD |   z | z_isBAD | x_lev_NA | x_lev_x_a | x_catP |   y |
|-----------:|----------:|----:|--------:|---------:|----------:|-------:|----:|
| -0.2666667 | 0.5000000 |   1 |       0 |        0 |         1 |    0.6 |   0 |
| -0.5000000 | 0.0000000 |   2 |       0 |        0 |         1 |    0.5 |   0 |
| -0.0666667 | 0.5000000 |   3 |       0 |        0 |         1 |    0.6 |   0 |
| -0.5000000 | 0.0000000 |   4 |       0 |        0 |         1 |    0.5 |   1 |
|  0.4000000 | 0.7071068 |   5 |       0 |        0 |         0 |    0.2 |   0 |
| -0.4000000 | 0.7071068 |   3 |       1 |        0 |         0 |    0.2 |   1 |

``` r

# Any future application data is prepared with
# the prepare method.
dTestNTreated <- prepare(treatmentsN, dTestN, pruneSig=NULL)

dTestNTreated %.>%
  head(.) %.>%
  knitr::kable(.)
```

| x_catP | x_catN |    x_catD |         z | z_isBAD | x_lev_NA | x_lev_x_a |
|-------:|-------:|----------:|----------:|--------:|---------:|----------:|
| 0.5000 |  -0.25 | 0.5000000 | 10.000000 |       0 |        0 |         1 |
| 0.2500 |   0.00 | 0.7071068 | 20.000000 |       0 |        0 |         0 |
| 0.0625 |   0.00 | 0.7071068 | 30.000000 |       0 |        0 |         0 |
| 0.2500 |   0.50 | 0.0000000 |  3.666667 |       1 |        1 |         0 |

Related work:

- Cohen J, Cohen P (1983). Applied Multiple Regression/Correlation
  Analysis For The Behavioral Sciences. 2 edition. Lawrence Erlbaum
  Associates, Inc. ISBN 0-89859-268-2.
- [“A preprocessing scheme for high-cardinality categorical attributes
  in classification and prediction
  problems”](https://dl.acm.org/doi/10.1145/507533.507538) Daniele
  Micci-Barreca; ACM SIGKDD Explorations, Volume 3 Issue 1, July 2001
  Pages 27-32.
- [“Modeling Trick: Impact Coding of Categorical Variables with Many
  Levels”](https://win-vector.com/2012/07/23/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)
  Nina Zumel; Win-Vector blog, 2012.
- “Big Learning Made Easy – with Counts!”, Misha Bilenko, Cortana
  Intelligence and Machine Learning Blog, 2015.

## Installation

To install, from inside `R` please run:

``` r
install.packages("vtreat")
```

## Note

Notes on controlling `vtreat`’s cross-validation plans can be found
[here](https://github.com/WinVector/vtreat/blob/master/Examples/CustomizedCrossPlan/CustomizedCrossPlan.md).

Note: `vtreat` is meant only for “tame names”, that is: variables and
column names that are also valid *simple* (without quotes) `R` variables
names.
