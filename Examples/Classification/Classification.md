Classification
================

# Using [vtreat](https://github.com/WinVector/vtreat) with Classification Problems

Nina Zumel and John Mount

updated February 2020

Note this is a description of the [`R` version of
`vtreat`](https://github.com/WinVector/vtreat), the same example for the
[`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat)
can be found
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/Classification/Classification.md).

## Preliminaries

Load modules/packages.

``` r
library(rqdatatable)
```

    ## Loading required package: wrapr

    ## Loading required package: rquery

``` r
library(vtreat)
packageVersion('vtreat')
```

    ## [1] '1.6.0'

``` r
suppressPackageStartupMessages(library(ggplot2))
library(WVPlots)
```

Generate example data.

  - `y` is a noisy sinusoidal function of the variable `x`
  - `yc` is the output to be predicted: whether `y` is \> 0.5.
  - Input `xc` is a categorical variable that represents a
    discretization of `y`, along some `NA`s
  - Input `x2` is a pure noise variable with no relationship to the
    output

<!-- end list -->

``` r
set.seed(2020)

make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    return(d)
}

d = make_data(500)

d %.>%
  head(.) %.>%
  knitr::kable(.)
```

|          x |           y | xc          |          x2 | yc    |
| ---------: | ----------: | :---------- | ----------: | :---- |
|   1.884861 |   1.0717646 | level\_1    |   0.0046504 | TRUE  |
|   1.507742 |   0.9958029 | level\_1    | \-1.2287497 | TRUE  |
| \-5.490116 |   0.8315705 | level\_1    | \-0.1405980 | TRUE  |
|         NA |   0.6007655 | level\_0.5  | \-0.2073270 | TRUE  |
|         NA | \-0.8339836 | NA          | \-0.9215306 | FALSE |
|         NA | \-0.5329006 | level\_-0.5 |   0.3604742 | FALSE |

### Some quick data exploration

Check how many levels `xc` has, and their distribution (including `NA`)

``` r
unique(d['xc'])
```

    ##             xc
    ## 1      level_1
    ## 4    level_0.5
    ## 5         <NA>
    ## 6   level_-0.5
    ## 27     level_0
    ## 269 level_-1.5

``` r
table(d$xc, useNA = 'always')
```

    ## 
    ## level_-0.5 level_-1.5    level_0  level_0.5    level_1       <NA> 
    ##         94          1         85         98        103        119

Find the mean value of `yc`

``` r
mean(d[['yc']])
```

    ## [1] 0.324

Plot of `yc` versus `x`.

``` r
ggplot(d, aes(x=x, y=as.numeric(yc))) + 
  geom_line()
```

    ## Warning: Removed 7 rows containing missing values (geom_path).

![](Classification_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Build a transform appropriate for classification problems.

Now that we have the data, we want to treat it prior to modeling: we
want training data where all the input variables are numeric and have no
missing values or `NA`s.

First create the data treatment transform design object, in this case a
treatment for a binomial classification problem.

We use the training data `d` to fit the transform and return a treated
training set: completely numeric, with no missing values.

``` r
unpack[
  transform = treatments,
  d_prepared = crossFrame
  ] <- vtreat::mkCrossFrameCExperiment(
    dframe = d,                                    # data to learn transform from
    varlist = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE                           # outcome of interest
  )
```

    ## [1] "vtreat 1.6.0 start initial treatment design Wed Mar 11 16:28:39 2020"
    ## [1] " start cross frame work Wed Mar 11 16:28:40 2020"
    ## [1] " vtreat::mkCrossFrameCExperiment done Wed Mar 11 16:28:40 2020"

Notice that `d_prepared` now only includes derived variables and the
outcome `yc`. The derived variables will be discussed below.

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD |  xc\_catP |    xc\_catB |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| ----------: | -------: | --------: | ----------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
|   1.8848606 |        0 | 0.2102102 |   14.206543 |   0.0046504 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
|   1.5077419 |        0 | 0.2005988 |   14.139786 | \-1.2287497 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-5.4901159 |        0 | 0.2005988 |   14.139786 | \-0.1405980 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.1276897 |        1 | 0.1891892 |    1.219475 | \-0.2073270 |           0 |                              0 |                    0 |                       1 |                    0 | TRUE  |
| \-0.3929879 |        1 | 0.2402402 | \-12.844663 | \-0.9215306 |           1 |                              0 |                    0 |                       0 |                    0 | FALSE |
| \-0.2908461 |        1 | 0.1766467 | \-12.563128 |   0.3604742 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |

Note that for the training data `d`: `crossFrame` is **not** the same as
`prepare(transform, d)`; the second call can lead to nested model bias
in some situations, and is **not** recommended. For other, later data,
not seen during transform design `transform.preprare(o)` is an
appropriate step.

`vtreat` version `1.5.1` and newer issue a warning if you call the
incorrect transform pattern on your original training
    data:

``` r
d_prepared_wrong <- prepare(transform, d)
```

    ## Warning in prepare.treatmentplan(transform, d): possibly called prepare() on
    ## same data frame as designTreatments*()/mkCrossFrame*Experiment(), this can lead
    ## to over-fit. To avoid this, please use mkCrossFrame*Experiment$crossFrame.

### The Score Frame

Now examine the score frame, which gives information about each new
variable, including its type, which original variable it is derived
from, its (cross-validated) significance as a one-variable linear model
for the outcome,and the (cross-validated) R-squared of its corresponding
linear model.

``` r
score_frame <- transform$scoreFrame

cols = c("varName", "origName", "code", "rsq", "sig", "varMoves", "default_threshold", "recommended")
knitr::kable(score_frame[,cols])
```

| varName                        | origName | code  |       rsq |       sig | varMoves | default\_threshold | recommended |
| :----------------------------- | :------- | :---- | --------: | --------: | :------- | -----------------: | :---------- |
| x                              | x        | clean | 0.0005756 | 0.5470919 | TRUE     |               0.10 | FALSE       |
| x\_isBAD                       | x        | isBAD | 0.0000771 | 0.8255885 | TRUE     |               0.20 | FALSE       |
| xc\_catP                       | xc       | catP  | 0.0008468 | 0.4652101 | TRUE     |               0.20 | FALSE       |
| xc\_catB                       | xc       | catB  | 0.7883578 | 0.0000000 | TRUE     |               0.20 | TRUE        |
| x2                             | x2       | clean | 0.0026075 | 0.2000083 | TRUE     |               0.10 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | 0.1750095 | 0.0000000 | TRUE     |               0.04 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | 0.1328708 | 0.0000000 | TRUE     |               0.04 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | 0.1185254 | 0.0000000 | TRUE     |               0.04 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | 0.0644178 | 0.0000000 | TRUE     |               0.04 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | 0.4701626 | 0.0000000 | TRUE     |               0.04 | TRUE        |

Note that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level, plus `NA`
    (`xc_lev_*`)
  - the value of a (cross-validated) one-variable model for `yc` as a
    function of `xc` (`xc_catB`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_catP`)

The variable `x` has been converted to two new variables:

  - a clean version of `x` that has no missing values or `NaN`s
  - a variable indicating when `x` was `NA` in the original data
    (`x_isBAD`).

Any or all of these new variables are available for downstream modeling.

The `recommended` column indicates which variables are non constant
(`varMoves` == TRUE) and have a significance value smaller than
`default_threshold`. See the section *Deriving the Default Thresholds*
below for the reasoning behind such a default threshold. Recommended
columns are intended as advice about which variables appear to be most
likely to be useful in a downstream model. This advice attempts to be
conservative, to reduce the possibility of mistakenly eliminating
variables that may in fact be useful (although, obviously, it can still
mistakenly eliminate variables that have a real but non-linear
relationship to the output, as is the case with `x`, in our example).

Let’s look at the variables that are and are not recommended:

``` r
# recommended variables
score_frame[score_frame[['recommended']], 'varName', drop = FALSE]  %.>%
  knitr::kable(.)
```

|    | varName                        |
| -- | :----------------------------- |
| 4  | xc\_catB                       |
| 6  | xc\_lev\_NA                    |
| 7  | xc\_lev\_x\_level\_minus\_0\_5 |
| 8  | xc\_lev\_x\_level\_0           |
| 9  | xc\_lev\_x\_level\_0\_5        |
| 10 | xc\_lev\_x\_level\_1           |

``` r
# not recommended variables
score_frame[!score_frame[['recommended']], 'varName', drop = FALSE] %.>%
  knitr::kable(.)
```

|   | varName  |
| - | :------- |
| 1 | x        |
| 2 | x\_isBAD |
| 3 | xc\_catP |
| 5 | x2       |

## A Closer Look at `catB` variables

Variables of type `catB` are the outputs of a one-variable regularized
logistic regression of a categorical variable (in our example, `xc`)
against the centered output on the (cross-validated) treated training
data.

Let’s see whether `xc_catB` makes a good one-variable model for `yc`. It
has a large AUC:

``` r
WVPlots::ROCPlot(
  frame = d_prepared,
  xvar = 'xc_catB',
  truthVar = 'yc',
  truthTarget = TRUE,
  title = 'performance of xc_catB variable')
```

![](Classification_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

This indicates that `xc_catB` is strongly predictive of the outcome.
Negative values of `xc_catB` correspond strongly to negative outcomes,
and positive values correspond strongly to positive outcomes.

``` r
WVPlots::DoubleDensityPlot(
  frame = d_prepared,
  xvar = 'xc_catB',
  truthVar = 'yc',
  title = 'performance of xc_catB variable')
```

![](Classification_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The values of `xc_catB` are in “link space”.

Variables of type `catB` are useful when dealing with categorical
variables with a very large number of possible levels. For example, a
categorical variable with 10,000 possible values potentially converts to
10,000 indicator variables, which may be unwieldy for some modeling
methods. Using a single numerical variable of type `catB` may be a
preferable alternative.

## Using the Prepared Data in a Model

Of course, what we really want to do with the prepared training data is
to fit a model jointly with all the (recommended) variables. Let’s try
fitting a logistic regression model to `d_prepared`.

``` r
model_vars <- score_frame$varName[score_frame$recommended]
# to use all the variables:
# model_vars <- score_frame$varName

f <- wrapr::mk_formula('yc', model_vars, outcome_target = TRUE)

model = glm(f, data = d_prepared)

# now predict
d_prepared['prediction'] = predict(
  model,
  newdata = d_prepared, 
  type = 'response')

# look at the ROC curve (on the training data)
WVPlots::ROCPlot(
  frame = d_prepared,
  xvar = 'prediction',
  truthVar = 'yc',
  truthTarget = TRUE,
  title = 'Performance of logistic regression model on training data')
```

![](Classification_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Now apply the model to new data.

``` r
# create the new data
dtest <- make_data(450)

# prepare the new data with vtreat
dtest_prepared = prepare(transform, dtest)
# dtest %.>% transform is an alias for prepare(transform, dtest)

# apply the model to the prepared data
dtest_prepared['prediction'] = predict(
  model,
  newdata = dtest_prepared,
  type = 'response')

WVPlots::ROCPlot(
  frame = dtest_prepared,
  xvar = 'prediction',
  truthVar = 'yc',
  truthTarget = TRUE,
  title = 'Performance of logistic regression model on test data')
```

![](Classification_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Parameters for `BinomialOutcomeTreatment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications.

``` r
suppressPackageStartupMessages(library(printr))
args("mkCrossFrameCExperiment")
```

    ## function (dframe, varlist, outcomename, outcometarget, ..., weights = c(), 
    ##     minFraction = 0.02, smFactor = 0, rareCount = 0, rareSig = 1, 
    ##     collarProb = 0, codeRestriction = NULL, customCoders = NULL, 
    ##     scale = FALSE, doCollar = FALSE, splitFunction = NULL, ncross = 3, 
    ##     forceSplit = FALSE, catScaling = TRUE, verbose = TRUE, parallelCluster = NULL, 
    ##     use_parallel = TRUE, missingness_imputation = NULL, imputation_map = NULL) 
    ## NULL

Some parameters of note include:

**codeRestriction**: The types of synthetic variables that `vtreat` will
(potentially) produce. By default, all possible applicable types will be
produced. See *Types of prepared variables* below.

**minFraction** (default: 0.02): For categorical variables, indicator
variables (type `lev`) are only produced for levels that are present at
least `minFraction` of the time. A consequence of this is that
1/`minFraction` is the maximum number of indicators that will be
produced for a given categorical variable. To make sure that *all*
possible indicator variables are produced, set `minFraction = 0`

**splitFunction**: The cross validation method used by `vtreat`. Most
people won’t have to change this.

**ncross** (default: 3): The number of folds to use for cross-validation

**missingness\_imputation**: The function or value that vtreat uses to
impute or “fill in” missing numerical values. The default is `mean`. To
change the imputation function or use different functions/values for
different columns, see the [Imputation
example](https://github.com/WinVector/vtreat/blob/master/Examples/Imputation/Imputation.md).

**customCoders**: For passing in user-defined transforms for custom data
preparation. Won’t be needed in most situations, but see
[here](http://www.win-vector.com/blog/2017/09/custom-level-coding-in-vtreat/)
for an example of applying a GAM transform to input variables.

## Types of prepared variables

**clean**: Produced from numerical variables: a clean numerical variable
with no `NAs` or missing values

**lev**: Produced from categorical variables, one for each (common)
level: for each level of the variable, indicates if that level was “on”

**catP**: Produced from categorical variables: indicates how often each
level of the variable was “on” (its prevalence)

**catB**: Produced from categorical variables: score from a
one-dimensional model of the centered output as a function of the
variable

**isBAD**: Produced for numerical variables: an indicator variable that
marks when the original variable was missing or `NaN`.

More on the coding types can be found
[here](https://winvector.github.io/vtreat/articles/vtreatVariableTypes.html).

### Example: Produce only a subset of variable types

In this example, suppose you only want to use indicators and continuous
variables in your model; in other words, you only want to use variables
of types (`clean_copy`, `missing_indicator`, and `indicator_code`), and
no `catB` or `prevalence_code` variables.

``` r
unpack[
  transform_thin = treatments,
  d_prepared_thin = crossFrame
  ] <- vtreat::mkCrossFrameCExperiment(
    dframe = d,                                    # data to learn transform from
    varlist = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE,                          # outcome of interest
    codeRestriction = c('lev',                     # transforms we want
                        'clean',
                        'isBAD')
  )
```

    ## [1] "vtreat 1.6.0 start initial treatment design Wed Mar 11 16:28:42 2020"
    ## [1] " start cross frame work Wed Mar 11 16:28:42 2020"
    ## [1] " vtreat::mkCrossFrameCExperiment done Wed Mar 11 16:28:42 2020"

``` r
score_frame_thin <- transform_thin$scoreFrame

# no catB or catP
d_prepared_thin %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| ----------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
|   1.8848606 |        0 |   0.0046504 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
|   1.5077419 |        0 | \-1.2287497 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-5.4901159 |        0 | \-0.1405980 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.0453530 |        1 | \-0.2073270 |           0 |                              0 |                    0 |                       1 |                    0 | TRUE  |
| \-0.0453530 |        1 | \-0.9215306 |           1 |                              0 |                    0 |                       0 |                    0 | FALSE |
| \-0.4926751 |        1 |   0.3604742 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |

``` r
# no catB or catP
knitr::kable(score_frame_thin[,cols])
```

| varName                        | origName | code  |       rsq |       sig | varMoves | default\_threshold | recommended |
| :----------------------------- | :------- | :---- | --------: | --------: | :------- | -----------------: | :---------- |
| x                              | x        | clean | 0.0005756 | 0.5470919 | TRUE     |         0.16666667 | FALSE       |
| x\_isBAD                       | x        | isBAD | 0.0000771 | 0.8255885 | TRUE     |         0.33333333 | FALSE       |
| x2                             | x2       | clean | 0.0026075 | 0.2000083 | TRUE     |         0.16666667 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | 0.1750095 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | 0.1328708 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | 0.1185254 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | 0.0644178 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | 0.4701626 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |

## Deriving the Default Thresholds

While machine learning algorithms are generally tolerant to a reasonable
number of irrelevant or noise variables, too many irrelevant variables
can lead to serious overfit; see [this
article](http://www.win-vector.com/blog/2014/02/bad-bayes-an-example-of-why-you-need-hold-out-testing/)
for an extreme example, one we call “Bad Bayes”. The default threshold
is an attempt to eliminate obviously irrelevant variables early.

Imagine that you have a pure noise dataset, where none of the *n* inputs
are related to the output. If you treat each variable as a one-variable
model for the output, and look at the significances of each model, these
significance-values will be uniformly distributed in the range \[0:1\].
You want to pick a weakest possible significance threshold that
eliminates as many noise variables as possible. A moment’s thought
should convince you that a threshold of *1/n* allows only one variable
through, in expectation.

This leads to the general-case heuristic that a significance threshold
of *1/n* on your variables should allow only one irrelevant variable
through, in expectation (along with all the relevant variables). Hence,
*1/n* used to be our recommended threshold, when we originally developed
the R version of `vtreat`.

We noticed, however, that this biases the filtering against numerical
variables, since there are at most two derived variables (of types
*clean* and *is\_BAD*) for every numerical variable in the original
data. Categorical variables, on the other hand, are expanded to many
derived variables: several indicators (one for every common level), plus
a *catB* and a *catP*. So we now reweight the thresholds.

Suppose you have a (treated) data set with *ntreat* different types of
`vtreat` variables (`clean`, `lev`, etc). There are *nT* variables of
type *T*. Then the default threshold for all the variables of type *T*
is *1/(ntreat nT)*. This reweighting helps to reduce the bias against
any particular type of variable. The heuristic is still that the set of
recommended variables will allow at most one noise variable into the set
of candidate variables.

As noted above, because `vtreat` estimates variable significances using
linear methods by default, some variables with a non-linear relationship
to the output may fail to pass the threshold. In this case, you may not
wish to filter the variables to be used in the models to only
recommended variables (as we did in the main example above), but instead
use all the variables, or select the variables to use by your own
criteria.

## Conclusion

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

These current revisions of the examples are designed to be small, yet
complete. So as a set they have some overlap, but the user can rely
mostly on a single example for a single task type.
