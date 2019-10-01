Classification
================

# Using [vtreat](https://github.com/WinVector/vtreat) with Classification Problems

Nina Zumel and John Mount September 2019

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

    ## Loading required package: rquery

``` r
library(vtreat)
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
| \-2.620747 | \-0.4162074 | level\_-0.5 | \-0.6604022 | FALSE |
|  10.714563 | \-0.7431123 | level\_-0.5 | \-0.2577693 | FALSE |
|   4.138288 | \-0.7402798 | level\_-0.5 |   0.5093482 | FALSE |
|         NA |   0.9386129 | level\_1    |   2.5480866 | TRUE  |
|         NA |   1.0942563 | level\_1    |   1.1413150 | TRUE  |
|         NA |   0.1037898 | level\_0    |   0.8132199 | FALSE |

### Some quick data exploration

Check how many levels `xc` has, and their distribution (including `NA`)

``` r
unique(d['xc'])
```

    ##           xc
    ## 1 level_-0.5
    ## 4    level_1
    ## 6    level_0
    ## 7  level_0.5
    ## 8       <NA>

``` r
table(d$xc, useNA = 'always')
```

    ## 
    ## level_-0.5    level_0  level_0.5    level_1       <NA> 
    ##         89         86         96        102        127

Find the mean value of `yc`

``` r
mean(d[['yc']])
```

    ## [1] 0.326

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

We use the training data `d` to fit the transform and the return a
treated training set: completely numeric, with no missing values.

``` r
transform_design = vtreat::mkCrossFrameCExperiment(
    dframe = d,                                    # data to learn transform from
    varlist = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE                           # outcome of interest
)
```

    ## [1] "vtreat 1.4.7 start initial treatment design Tue Oct  1 10:35:47 2019"
    ## [1] " start cross frame work Tue Oct  1 10:35:47 2019"
    ## [1] " vtreat::mkCrossFrameCExperiment done Tue Oct  1 10:35:47 2019"

``` r
transform <- transform_design$treatments
d_prepared <- transform_design$crossFrame
score_frame <- transform$scoreFrame
score_frame$recommended <- score_frame$varMoves & (score_frame$sig < 1/nrow(score_frame))
```

Note that for the training data `d`: `transform_design$crossFrame` is
**not** the same as `transform.prepare(d)`; the second call can lead to
nested model bias in some situations, and is **not** recommended. For
other, later data, not seen during transform design
`transform.preprare(o)` is an appropriate step.

Now examine the score frame, which gives information about each new
variable, including its type, which original variable it is derived
from, its (cross-validated) correlation with the outcome, and its
(cross-validated) significance as a one-variable linear model for the
outcome.

``` r
knitr::kable(score_frame)
```

| varName                        | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code  | recommended |
| :----------------------------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :---- | :---------- |
| x                              | TRUE     | 0.0000588 | 0.8472121 | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0070953 | 0.0343077 | FALSE      |                 0 | x        | isBAD | TRUE        |
| xc\_catP                       | TRUE     | 0.0075139 | 0.0294077 | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_catB                       | TRUE     | 0.7988184 | 0.0000000 | TRUE       |                 4 | xc       | catB  | TRUE        |
| x2                             | TRUE     | 0.0031878 | 0.1560070 | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1903343 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1255315 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1207530 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0773074 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.4599270 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |

Note that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level (`xc_lev_*`)
  - the value of a (cross-validated) one-variable model for `yc` as a
    function of `xc` (`xc_catB`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_catP`)
  - a variable indicating when `xc` was `NA` in the original data
    (`xc_lev_NA` for categorical variables, `x_isBAD` for continuous
    variables).

Any or all of these new variables are available for downstream modeling.

The `recommended` column indicates which variables are non constant
(`varMoves` == TRUE) and have a significance value smaller than
`1/nrow(score_frame)`. See the section *Deriving the Default Thresholds*
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
| 2  | x\_isBAD                       |
| 3  | xc\_catP                       |
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

|   | varName |
| - | :------ |
| 1 | x       |
| 5 | x2      |

Notice that `d_prepared` only includes derived variables and the outcome
`y`:

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD |  xc\_catP |   xc\_catB |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| ----------: | -------: | --------: | ---------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
| \-2.6207469 |        0 | 0.1856287 | \-12.61272 | \-0.6604022 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
|  10.7145633 |        0 | 0.1831832 | \-12.60092 | \-0.2577693 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
|   4.1382878 |        0 | 0.1831832 | \-12.60092 |   0.5093482 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
| \-0.5287519 |        1 | 0.2132132 |   14.20699 |   2.5480866 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.5759212 |        1 | 0.2042042 |   14.15015 |   1.1413150 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.3783658 |        1 | 0.1766467 | \-12.56313 |   0.8132199 |           0 |                              0 |                    1 |                       0 |                    0 | FALSE |

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

![](Classification_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

![](Classification_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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
f <- wrapr::mk_formula('yc', model_vars, outcome_target = TRUE)

model = glm(f, data = d_prepared)

# now predict
d_prepared['prediction'] = predict(
  model,
  newdata = d_prepared, 
  type = 'response')
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type
    ## == : prediction from a rank-deficient fit may be misleading

``` r
# look at the ROC curve (on the training data)
WVPlots::ROCPlot(
  frame = d_prepared,
  xvar = 'prediction',
  truthVar = 'yc',
  truthTarget = TRUE,
  title = 'Performance of logistic regression model on training data')
```

![](Classification_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Now apply the model to new data.

``` r
# create the new data
dtest <- make_data(450)

# prepare the new data with vtreat
dtest_prepared = prepare(transform, dtest)

# apply the model to the prepared data
dtest_prepared['prediction'] = predict(
  model,
  newdata = dtest_prepared,
  type = 'response')
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type
    ## == : prediction from a rank-deficient fit may be misleading

``` r
WVPlots::ROCPlot(
  frame = dtest_prepared,
  xvar = 'prediction',
  truthVar = 'yc',
  truthTarget = TRUE,
  title = 'Performance of logistic regression model on test data')
```

![](Classification_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Parameters for `BinomialOutcomeTreatment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications.

``` r
suppressPackageStartupMessages(library(printr))
help("mkCrossFrameCExperiment")
```

    ## Run categorical cross-frame experiment.
    ## 
    ## Description:
    ## 
    ##      Builds a 'designTreatmentsC' treatment plan and a data frame
    ##      prepared from 'dframe' that is "cross" in the sense each row is
    ##      treated using a treatment plan built from a subset of dframe
    ##      disjoint from the given row. The goal is to try to and supply a
    ##      method of breaking nested model bias other than splitting into
    ##      calibration, training, test sets.
    ## 
    ## Usage:
    ## 
    ##      mkCrossFrameCExperiment(dframe, varlist, outcomename, outcometarget, ...,
    ##        weights = c(), minFraction = 0.02, smFactor = 0, rareCount = 0,
    ##        rareSig = 1, collarProb = 0, codeRestriction = NULL,
    ##        customCoders = NULL, scale = FALSE, doCollar = FALSE,
    ##        splitFunction = NULL, ncross = 3, forceSplit = FALSE,
    ##        catScaling = TRUE, verbose = TRUE, parallelCluster = NULL,
    ##        use_parallel = TRUE)
    ##      
    ## Arguments:
    ## 
    ##   dframe: Data frame to learn treatments from (training data), must
    ##           have at least 1 row.
    ## 
    ##  varlist: Names of columns to treat (effective variables).
    ## 
    ## outcomename: Name of column holding outcome variable.
    ##           dframe[[outcomename]] must be only finite non-missing values.
    ## 
    ## outcometarget: Value/level of outcome to be considered "success", and
    ##           there must be a cut such that
    ##           dframe[[outcomename]]==outcometarget at least twice and
    ##           dframe[[outcomename]]!=outcometarget at least twice.
    ## 
    ##      ...: no additional arguments, declared to forced named binding of
    ##           later arguments
    ## 
    ##  weights: optional training weights for each row
    ## 
    ## minFraction: optional minimum frequency a categorical level must have
    ##           to be converted to an indicator column.
    ## 
    ## smFactor: optional smoothing factor for impact coding models.
    ## 
    ## rareCount: optional integer, allow levels with this count or below to
    ##           be pooled into a shared rare-level.  Defaults to 0 or off.
    ## 
    ##  rareSig: optional numeric, suppress levels from pooling at this
    ##           significance value greater.  Defaults to NULL or off.
    ## 
    ## collarProb: what fraction of the data (pseudo-probability) to collar
    ##           data at if doCollar is set during 'prepare.treatmentplan'.
    ## 
    ## codeRestriction: what types of variables to produce (character array of
    ##           level codes, NULL means no restriction).
    ## 
    ## customCoders: map from code names to custom categorical variable
    ##           encoding functions (please see <URL:
    ##           https://github.com/WinVector/vtreat/blob/master/extras/CustomLevelCoders.md>).
    ## 
    ##    scale: optional if TRUE replace numeric variables with regression
    ##           ("move to outcome-scale").
    ## 
    ## doCollar: optional if TRUE collar numeric variables by cutting off
    ##           after a tail-probability specified by collarProb during
    ##           treatment design.
    ## 
    ## splitFunction: (optional) see vtreat::buildEvalSets .
    ## 
    ##   ncross: optional scalar>=2 number of cross-validation rounds to
    ##           design.
    ## 
    ## forceSplit: logical, if TRUE force cross-validated significance
    ##           calculations on all variables.
    ## 
    ## catScaling: optional, if TRUE use glm() linkspace, if FALSE use lm()
    ##           for scaling.
    ## 
    ##  verbose: if TRUE print progress.
    ## 
    ## parallelCluster: (optional) a cluster object created by package
    ##           parallel or package snow.
    ## 
    ## use_parallel: logical, if TRUE use parallel methods.
    ## 
    ## Value:
    ## 
    ##      list with treatments and crossFrame
    ## 
    ## See Also:
    ## 
    ##      'designTreatmentsC', 'designTreatmentsN', 'prepare.treatmentplan'
    ## 
    ## Examples:
    ## 
    ##      set.seed(23525)
    ##      zip <- paste('z',1:100)
    ##      N <- 200
    ##      d <- data.frame(zip=sample(zip,N,replace=TRUE),
    ##                      zip2=sample(zip,20,replace=TRUE),
    ##                      y=runif(N))
    ##      del <- runif(length(zip))
    ##      names(del) <- zip
    ##      d$y <- d$y + del[d$zip2]
    ##      d$yc <- d$y>=mean(d$y)
    ##      cC <- mkCrossFrameCExperiment(d,c('zip','zip2'),'yc',TRUE,
    ##        rareCount=2,rareSig=0.9)
    ##      cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip_catB)  # poor
    ##      cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip2_catB) # better
    ##      treatments <- cC$treatments
    ##      dTrainV <- cC$crossFrame

Some parameters of note include:

**codeRestriction**: The types of synthetic variables that `vtreat` will
(potentially) produce. See *Types of prepared variables* below.

**minFraction**: For categorical variables, indicator variables (type
`indicator_code`) are only produced for levels that are present at least
`minFraction` of the time. A consequence of this is that 1/`minFraction`
is the maximum number of indicators that will be produced for a given
categorical variable. To make sure that *all* possible indicator
variables are produced, set `minFraction = 0`

**splitFunction**: The cross validation method used by `vtreat`. Most
people won’t have to change this.

**ncross**: The number of folds to use for cross-validation

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
level of the variable was “on”

**catB**: Produced from categorical variables: score from a
one-dimensional model of the centered output as a function of the
variable

**is\_BAD**: Produced for both numerical and categorical variables: an
indicator variable that marks when the original variable was missing or
`NaN`.

More on the coding types can be found
[here](https://winvector.github.io/vtreat/articles/vtreatVariableTypes.html).

### Example: Produce only a subset of variable types

In this example, suppose you only want to use indicators and continuous
variables in your model; in other words, you only want to use variables
of types (`clean_copy`, `missing_indicator`, and `indicator_code`), and
no `catB` or `prevalence_code` variables.

``` r
transform_design_thin = vtreat::mkCrossFrameCExperiment(
    dframe = d,                                    # data to learn transform from
    varlist = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE,                          # outcome of interest
    codeRestriction = c('lev',                     # transforms we want
                        'clean',
                        'isBAD')
)
```

    ## [1] "vtreat 1.4.7 start initial treatment design Tue Oct  1 10:35:50 2019"
    ## [1] " start cross frame work Tue Oct  1 10:35:50 2019"
    ## [1] " vtreat::mkCrossFrameCExperiment done Tue Oct  1 10:35:50 2019"

``` r
transform_thin <- transform_design_thin$treatments
d_prepared_thin <- transform_design_thin$crossFrame
score_frame_thin <- transform_thin$scoreFrame

d_prepared_thin %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| ----------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
| \-2.6207469 |        0 | \-0.6604022 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
|  10.7145633 |        0 | \-0.2577693 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
|   4.1382878 |        0 |   0.5093482 |           0 |                              1 |                    0 |                       0 |                    0 | FALSE |
| \-0.4528789 |        1 |   2.5480866 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.4528789 |        1 |   1.1413150 |           0 |                              0 |                    0 |                       0 |                    1 | TRUE  |
| \-0.5602661 |        1 |   0.8132199 |           0 |                              0 |                    1 |                       0 |                    0 | FALSE |

``` r
knitr::kable(score_frame_thin)
```

| varName                        | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code  |
| :----------------------------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :---- |
| x                              | TRUE     | 0.0000588 | 0.8472121 | FALSE      |                 0 | x        | clean |
| x\_isBAD                       | TRUE     | 0.0070953 | 0.0343077 | FALSE      |                 0 | x        | isBAD |
| x2                             | TRUE     | 0.0031878 | 0.1560070 | FALSE      |                 0 | x2       | clean |
| xc\_lev\_NA                    | TRUE     | 0.1903343 | 0.0000000 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1255315 | 0.0000000 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1207530 | 0.0000000 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0773074 | 0.0000000 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_1           | TRUE     | 0.4599270 | 0.0000000 | FALSE      |                 0 | xc       | lev   |

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
through, in expectation. This leads to the general-case heuristic that a
significance threshold of *1/n* on your variables should allow only one
irrelevant variable through, in expectation (along with all the relevant
variables). Hence, *1/n* used to be our recommended threshold, when we
developed the R version of `vtreat`.

As noted above, because `vtreat` estimates variable significances using
linear methods by default, some variables with a non-linear relationship
to the output may fail to pass the threshold. So it may make sense for
the data scientist to filter (or not) as they will.

The variables can be appraised in a non-linear fashion as follows:

``` r
d %.>%
  value_variables_C(dframe = .,
                  varlist = setdiff(colnames(.), c('y', 'yc')), 
                  outcomename = 'yc',
                  outcometarget = TRUE) %.>%
  knitr::kable(.)
```

|    |         rsq | count |       sig | var |
| -- | ----------: | ----: | --------: | :-- |
| x  | 0.007095258 |     2 | 0.0686153 | x   |
| x2 | 0.007484178 |     3 | 0.0891902 | x2  |
| xc | 0.800235926 |     2 | 0.0000000 | xc  |

More on non-linear variable scoring can be found
[here](https://cran.r-project.org/web/packages/vtreat/vignettes/VariableImportance.html).

## Conclusion

In all cases (classification, regression, unsupervised, and multinomial
classification) the intent is that `vtreat` transforms are essentially
one liners.

The preparation commands are organized as follows:

  - **Regression**: [`R` regression
    example](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression.md),
    [`Python` regression
    example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Regression/Regression.md).
  - **Classification**: [`R` classification
    example](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.md),
    [`Python` classification
    example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Classification/Classification.md).
  - **Unsupervised tasks**: [`R` unsupervised
    example](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised.md),
    [`Python` unsupervised
    example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Unsupervised/Unsupervised.md).
  - **Multinomial classification**: [`R` multinomial classification
    example](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md),
    [`Python` multinomial classification
    example](https://github.com/WinVector/pyvtreat/blob/master/Examples/Multinomial/MultinomialExample.md).

These current revisions of the examples are designed to be small, yet
complete. So as a set they have some overlap, but the user can rely
mostly on a single example for a single task type.
