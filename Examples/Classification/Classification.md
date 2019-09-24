Classification
================

# Using vtreat with Classification Problems

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
  - `yc` is the output to be predicted: : whether `y` is \> 0.5.
  - Input `xc` is a categorical variable that represents a
    discretization of `y`, along some `NaN`s
  - Input `x2` is a pure noise variable with no relationship to the
    output

<!-- end list -->

``` r
make_data <- function(nrows) {
    d <- data.frame(x = 0.1*(0:(nrows-1)))
    d['y'] = sin(d['x']) + 0.1*rnorm(n = nrow(d))
    d[4:10, 'x'] = NA                # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrow(d))
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    d['yc'] = d[['y']]>0.5
    return(d)
}

d = make_data(500)

d %.>%
  head(.) %.>%
  knitr::kable(.)
```

|   x |           y | xc         |          x2 | yc    |
| --: | ----------: | :--------- | ----------: | :---- |
| 0.0 | \-0.0826741 | level\_0   |   0.3857671 | FALSE |
| 0.1 |   0.1017586 | level\_0   | \-1.1640223 | FALSE |
| 0.2 |   0.1737507 | level\_0   | \-0.0657797 | FALSE |
|  NA |   0.3325365 | level\_0.5 |   0.8838345 | FALSE |
|  NA |   0.3057789 | level\_0.5 | \-1.9488825 | FALSE |
|  NA |   0.5252693 | level\_0.5 | \-0.2835894 | TRUE  |

### Some quick data exploration

Check how many levels `xc` has, and their disribution (including `NaN`)

``` r
unique(d['xc'])
```

    ##            xc
    ## 1     level_0
    ## 4   level_0.5
    ## 11    level_1
    ## 35 level_-0.5
    ## 41       <NA>

``` r
table(d$xc, useNA = 'always')
```

    ## 
    ## level_-0.5    level_0  level_0.5    level_1       <NA> 
    ##         91         72        107        108        122

Find the mean value of `yc`

``` r
mean(d[['yc']])
```

    ## [1] 0.344

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
missing values or `NaN`s.

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

    ## [1] "vtreat 1.4.6 start initial treatment design Mon Sep 23 16:59:37 2019"
    ## [1] " start cross frame work Mon Sep 23 16:59:38 2019"
    ## [1] " vtreat::mkCrossFrameCExperiment done Mon Sep 23 16:59:38 2019"

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
| x                              | TRUE     | 0.0028781 | 0.1734908 | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0062802 | 0.0443739 | FALSE      |                 0 | x        | isBAD | TRUE        |
| xc\_catP                       | TRUE     | 0.0326265 | 0.0000046 | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_catB                       | TRUE     | 0.7723257 | 0.0000000 | TRUE       |                 4 | xc       | catB  | TRUE        |
| x2                             | TRUE     | 0.0000521 | 0.8547289 | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1906246 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1352168 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1039575 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0579703 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.4579140 | 0.0000000 | FALSE      |                 0 | xc       | lev   | TRUE        |

Note that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level (`xc_lev_level_*`)
  - the value of a (cross-validated) one-variable model for `yc` as a
    function of `xc` (`xc_catB`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_catP`)
  - a variable indicating when `xc` was `NaN` in the original data
    (`xc_lev_NA` for categorical variables, `x_isBAD` for continuous
    variables).

Any or all of these new variables are available for downstream modeling.

The `recommended` column indicates which variables are non constant
(`varMoves` == True) and have a significance value smaller than
`1/nrow(score_frame)`. See the section *Deriving the Default Threholds*
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

|        x | x\_isBAD |  xc\_catP |    xc\_catB |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| -------: | -------: | --------: | ----------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
|  0.00000 |        0 | 0.1497006 | \-12.491461 |   0.3857671 |           0 |                              0 |                    1 |                       0 |                    0 | FALSE |
|  0.10000 |        0 | 0.1497006 | \-12.491461 | \-1.1640223 |           0 |                              0 |                    1 |                       0 |                    0 | FALSE |
|  0.20000 |        0 | 0.1381381 | \-12.386111 | \-0.0657797 |           0 |                              0 |                    1 |                       0 |                    0 | FALSE |
| 25.27515 |        1 | 0.2065868 |    1.259511 |   0.8838345 |           0 |                              0 |                    0 |                       1 |                    0 | FALSE |
| 25.39482 |        1 | 0.2162162 |    1.104857 | \-1.9488825 |           0 |                              0 |                    0 |                       1 |                    0 | FALSE |
| 25.27515 |        1 | 0.2065868 |    1.259511 | \-0.2835894 |           0 |                              0 |                    0 |                       1 |                    0 | TRUE  |

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
and positive values correspond strongly to postive outcomes.

``` r
WVPlots::DoubleDensityPlot(
  frame = d_prepared,
  xvar = 'xc_catB',
  truthVar = 'yc',
  title = 'performance of xc_catB variable')
```

![](Classification_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The values of `xc_catB` are in “link space”. We can visualize the
relationship a little better by converting the logistic score to a
probability.

``` r
expit <- function(x) { 
  1/(1+exp(-x)) 
}

logit <- function(p) {
  log(p/(1-p))
}

offset = logit(mean(d_prepared$yc))
d_prepared$xc_catB_prob = expit(d_prepared$xc_catB + offset)

WVPlots::DoubleDensityPlot(
  frame = d_prepared,
  xvar = 'xc_catB_prob',
  truthVar = 'yc',
  title = 'performance of xc_catB variable')                                
```

![](Classification_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Variables of type `catB` are useful when dealing with categorical
variables with a very large number of possible levels. For example, a
categorical variable with 10,000 possible values potentially converts to
10,000 indicator variables, which may be unwieldy for some modeling
methods. Using a single numerical variable of type `catB` may be a
preferable alternative.

\#%% md

## Using the Prepared Data in a Model

Of course, what we really want to do with the prepared training data is
to fit a model jointly with all the (recommended) variables. Let’s try
fitting a logistic regression model to `d_prepared`.

\#%%

import sklearn.linear\_model import seaborn

not\_variables = \[‘y’, ‘yc’, ‘prediction’\] model\_vars = \[v for v in
d\_prepared.columns if v not in set(not\_variables)\]

fitter = sklearn.linear\_model.LogisticRegression()
fitter.fit(d\_prepared\[model\_vars\], d\_prepared\[‘yc’\])

# now predict

d\_prepared\[‘prediction’\] =
fitter.predict\_proba(d\_prepared\[model\_vars\])\[:, 1\]

# look at the ROC curve (on the training data)

wvpy.util.plot\_roc(prediction=d\_prepared\[‘prediction’\],
istrue=d\_prepared\[‘yc’\], title = ‘Performance of logistic regression
model on training data’)

\#%% md

Now apply the model to new data.

\#%%

# create the new data

dtest = make\_data(450)

# prepare the new data with vtreat

dtest\_prepared = transform.transform(dtest)

# apply the model to the prepared data

dtest\_prepared\[‘prediction’\] =
fitter.predict\_proba(dtest\_prepared\[model\_vars\])\[:, 1\]

wvpy.util.plot\_roc(prediction=dtest\_prepared\[‘prediction’\],
istrue=dtest\_prepared\[‘yc’\], title = ‘Performance of logistic
regression model on test data’)

\#%% md

## Parameters for `BinomialOutcomeTreatment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications.

\#%%

vtreat.vtreat\_parameters()

\#%% md

**coders**: The types of synthetic variables that `vtreat` will
(potentially) produce. See *Types of prepared variables* below.

**filter\_to\_recommended**: When True, prepared data only includes
variables marked as “recommended” in score frame. When False, prepared
data includes all variables. See the Example below.

**indicator\_min\_fraction**: For categorical variables, indicator
variables (type `indicator_code`) are only produced for levels that are
present at least `indicator_min_fraction` of the time. A consequence of
this is that 1/`indicator_min_fraction` is the maximum number of
indicators that will be produced for a given categorical variable. To
make sure that *all* possible indicator variables are produced, set
`indicator_min_fraction = 0`

**cross\_validation\_plan**: The cross validation method used by
`vtreat`. Most people won’t have to change this. *TODO: make some
examples of OrderedCrossPlan and StratifiedCrossPlan to link to here*

**cross\_validation\_k**: The number of folds to use for
cross-validation

**user\_transforms**: For passing in user-defined transforms for custom
data preparation. Won’t be needed in most situations, but see
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/UserCoders/UserCoders.ipynb)
for an example of applying a GAM transform to input variables.

**sparse\_indicators**: When True, use a (Pandas) sparse representation
for indicator variables. This representation is compatible with
`sklearn`; however, it may not be compatible with other modeling
packages. When False, use a dense representation.

### Example: Use all variables to model, not just recommended

\#%%

transform\_all = vtreat.BinomialOutcomeTreatment( outcome\_name=‘yc’, \#
outcome variable outcome\_target=True, \# outcome of interest
cols\_to\_copy=\[‘y’\], \# columns to “carry along” but not treat as
input variables params = vtreat.vtreat\_parameters({
‘filter\_to\_recommended’: False }) )

transform\_all.fit\_transform(d, d\[‘yc’\]).columns

\#%%

transform\_all.score\_frame\_

\#%% md

Note that the prepared data produced by `fit_transform()` includes all
the variables, including those that were not marked as “recommended”.

## Types of prepared variables

**clean\_copy**: Produced from numerical variables: a clean numerical
variable with no `NaNs` or missing values

**indicator\_code**: Produced from categorical variables, one for each
(common) level: for each level of the variable, indicates if that level
was “on”

**prevalence\_code**: Produced from categorical variables: indicates how
often each level of the variable was “on”

**catB**: Produced from categorical variables: score from a
one-dimensional model of the centered output as a function of the
variable

**missing\_indicator**: Produced for both numerical and categorical
variables: an indicator variable that marks when the original variable
was missing or `NaN`

**deviation\_code**: not used by `BinomialOutcomeTreatment`

**impact\_code**: not used by `BinomialOutcomeTreatment`

### Example: Produce only a subset of variable types

In this example, suppose you only want to use indicators and continuous
variables in your model; in other words, you only want to use variables
of types (`clean_copy`, `missing_indicator`, and `indicator_code`), and
no `catB` or `prevalence_code` variables.

\#%%

transform\_thin = vtreat.BinomialOutcomeTreatment( outcome\_name=‘yc’,
\# outcome variable outcome\_target=True, \# outcome of interest
cols\_to\_copy=\[‘y’\], \# columns to “carry along” but not treat as
input variables params = vtreat.vtreat\_parameters({
‘filter\_to\_recommended’: False, ‘coders’: {‘clean\_copy’,
‘missing\_indicator’, ‘indicator\_code’, } }) )

transform\_thin.fit\_transform(d, d\[‘yc’\]).head()

\#%%

transform\_thin.score\_frame\_

\#%% md

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
*1/n* used to be our recommended threshold, when we developed the R
version of `vtreat`.

We noticed, however, that this biases the filtering against numerical
variables, since there are at most two derived variables (of types
*clean\_copy* and *missing\_indicator* for every numerical variable in
the original data. Categorical variables, on the other hand, are
expanded to many derived variables: several indicators (one for every
common level), plus a *catB* and a *prevalence\_code*. So we now
reweight the thresholds.

Suppose you have a (treated) data set with *ntreat* different types of
`vtreat` variables (`clean_copy`, `indicator_code`, etc). There are *nT*
variables of type *T*. Then the default threshold for all the variables
of type *T* is *1/(ntreat nT)*. This reweighting helps to reduce the
bias against any particular type of variable. The heuristic is still
that the set of recommended variables will allow at most one noise
variable into the set of candidate variables.

As noted above, because `vtreat` estimates variable significances using
linear methods by default, some variables with a non-linear relationship
to the output may fail to pass the threshold. Setting the
`filter_to_recommended` parameter to False will keep all derived
variables in the treated frame, for the data scientist to filter (or
not) as they will.

\#%%
