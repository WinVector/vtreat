Multinomial Classification (Fit Prepare Notation)
================

# Using [vtreat](https://github.com/WinVector/vtreat) with Multinomial Classification Problems

Nina Zumel and John Mount October 2019

This article documents `vtreat`’s [“fit\_prepare”
variation](https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_prepare_api.md)
for classification problems. This API was inspired by the
[`pyvtreat`](https://github.com/WinVector/pyvtreat) API, which was in
turn based on the `.fit()`, `.transform()`, `.fit_transform()` workflow
of `scikit-learn` in `Python`.

The same example in the original `R` `vtreat` notation can be found
[here](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md).

The same example in the [`Python` version of
`vtreat`](https://github.com/WinVector/pyvtreat) can be found
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/Multinomial/MultinomialExample.md).

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
  - `yc` is the output to be predicted: `y`‘s quantized value as
    ’large’, ‘liminal’, or ‘small’.
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
    d['yc'] = ifelse(d[['y']] > 0.5, 'large', ifelse(d[['y']] < -0.5, 'small', 'liminal'))
    return(d)
}

d = make_data(500)

d %.>%
  head(.) %.>%
  knitr::kable(.)
```

|          x |           y | xc          |          x2 | yc    |
| ---------: | ----------: | :---------- | ----------: | :---- |
|   1.884861 |   1.0717646 | level\_1    |   0.0046504 | large |
|   1.507742 |   0.9958029 | level\_1    | \-1.2287497 | large |
| \-5.490116 |   0.8315705 | level\_1    | \-0.1405980 | large |
|         NA |   0.6007655 | level\_0.5  | \-0.2073270 | large |
|         NA | \-0.8339836 | NA          | \-0.9215306 | small |
|         NA | \-0.5329006 | level\_-0.5 |   0.3604742 | small |

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
table(d$xc, useNA = 'ifany')
```

    ## 
    ## level_-0.5 level_-1.5    level_0  level_0.5    level_1       <NA> 
    ##         94          1         85         98        103        119

Show the distribution of `yc`

``` r
table(d$yc, useNA = 'ifany')
```

    ## 
    ##   large liminal   small 
    ##     162     164     174

## Build a transform appropriate for classification problems.

Now that we have the data, we want to treat it prior to modeling: we
want training data where all the input variables are numeric and have no
missing values or `NA`s.

First create the data treatment transform design object, in this case a
treatment for a multinomial classification problem.

We use the training data `d` to fit the transform and the return a
treated training set: completely numeric, with no missing values.

``` r
transform_spec <- vtreat::MultinomialOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcome_name = 'yc'                             # outcome variable
)
```

Now call the `fit_prepare()` function with the training data `d` to fit
the transform and also return a treated training set. The
`fit_prepare()` function returns the fitted data treatment object (as
`treatments`) and a statistically correct treated training set (as
`cross_frame`) for training the model. The `cross_frame` is guaranteed
to be completely numeric, with no missing values.

``` r
# the unpack notation is a multiassignment operator
# see https://winvector.github.io/wrapr/articles/unpack_multiple_assignment.html
# for more details
unpack[
  treatment_plan = treatments,
  d_prepared = cross_frame
  ] <- fit_prepare(transform_spec, d)     
```

Notice that `d_prepared` now only includes derived variables and the
outcome `yc`. The derived variables will be discussed below.

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD | xc\_catP |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | large\_xc\_catB | liminal\_xc\_catB | small\_xc\_catB | yc    |
| ----------: | -------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | --------------: | ----------------: | --------------: | :---- |
|   1.8848606 |        0 |    0.206 |   0.0046504 |           0 |                              0 |                    0 |                       0 |                    1 |       14.169093 |      \-12.5759659 |    \-12.8410398 | large |
|   1.5077419 |        0 |    0.206 | \-1.2287497 |           0 |                              0 |                    0 |                       0 |                    1 |       14.181970 |      \-12.8643483 |    \-12.6639435 | large |
| \-5.4901159 |        0 |    0.206 | \-0.1405980 |           0 |                              0 |                    0 |                       0 |                    1 |       14.169093 |      \-12.5759659 |    \-12.8410398 | large |
| \-0.2704873 |        1 |    0.196 | \-0.2073270 |           0 |                              0 |                    0 |                       1 |                    0 |        1.096309 |         0.3616142 |    \-12.7584224 | large |
| \-0.2704873 |        1 |    0.238 | \-0.9215306 |           1 |                              0 |                    0 |                       0 |                    0 |     \-12.940744 |      \-12.7354342 |      14.1933572 | small |
| \-0.2704873 |        1 |    0.188 |   0.3604742 |           0 |                              1 |                    0 |                       0 |                    0 |     \-12.773690 |         0.5879950 |       0.8699281 | small |

Note that for the training data `d`: `crossFrame` is **not** the same as
`prepare(transform, d)`; the second call can lead to nested model bias
in some situations, and is **not** recommended. For other, later data,
not seen during transform design `transform.preprare(o)` is an
appropriate step.

`vtreat` version `1.5.1` and newer issue a warning if you call the
incorrect transform pattern on your original training
    data:

``` r
d_prepared_wrong <- prepare(treatment_plan, d)
```

    ## Warning in treatmentplan$transform(dframe = dframe, ...): possibly called
    ## transform() on same data frame as fit(), this can lead to over-fit. To avoid
    ## this, please use fit_transform().

### The Score Frame

Now examine the score frame, which gives information about each new
variable, including its type, which original variable it is derived
from, its (cross-validated) correlation with the outcome, and its
(cross-validated) significance as a one-variable linear model for the
outcome.

``` r
score_frame = get_score_frame(treatment_plan)

# only show a subset of the columns
cols = c("varName", "origName", "code", "outcome_level", "rsq", "sig", "varMoves", "default_threshold", "recommended")
knitr::kable(score_frame[,cols])
```

| varName                        | origName | code  | outcome\_level |       rsq |       sig | varMoves | default\_threshold | recommended |
| :----------------------------- | :------- | :---- | :------------- | --------: | --------: | :------- | -----------------: | :---------- |
| x                              | x        | clean | large          | 0.0005756 | 0.5470919 | TRUE     |         0.03333333 | FALSE       |
| x\_isBAD                       | x        | isBAD | large          | 0.0000771 | 0.8255885 | TRUE     |         0.06666667 | FALSE       |
| x2                             | x2       | clean | large          | 0.0026075 | 0.2000083 | TRUE     |         0.03333333 | FALSE       |
| xc\_catP                       | xc       | catP  | large          | 0.0002361 | 0.6997734 | TRUE     |         0.06666667 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | large          | 0.1750095 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | large          | 0.1185254 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | large          | 0.0644178 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | large          | 0.4701626 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | large          | 0.1328708 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| x                              | x        | clean | liminal        | 0.0022396 | 0.2338771 | TRUE     |         0.03333333 | FALSE       |
| x\_isBAD                       | x        | isBAD | liminal        | 0.0020122 | 0.2591571 | TRUE     |         0.06666667 | FALSE       |
| x2                             | x2       | clean | liminal        | 0.0023614 | 0.2215731 | TRUE     |         0.03333333 | FALSE       |
| xc\_catP                       | xc       | catP  | liminal        | 0.4410001 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_NA                    | xc       | lev   | liminal        | 0.1769596 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | liminal        | 0.3615209 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | liminal        | 0.0041777 | 0.1039765 | TRUE     |         0.01333333 | FALSE       |
| xc\_lev\_x\_level\_1           | xc       | lev   | liminal        | 0.1492650 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | liminal        | 0.0076508 | 0.0277896 | TRUE     |         0.01333333 | FALSE       |
| x                              | x        | clean | small          | 0.0048286 | 0.0773262 | TRUE     |         0.03333333 | FALSE       |
| x\_isBAD                       | x        | isBAD | small          | 0.0022777 | 0.2250506 | TRUE     |         0.06666667 | FALSE       |
| x2                             | x2       | clean | small          | 0.0000045 | 0.9569844 | TRUE     |         0.03333333 | FALSE       |
| xc\_catP                       | xc       | catP  | small          | 0.3117712 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| xc\_lev\_NA                    | xc       | lev   | small          | 0.5132320 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | small          | 0.1265119 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | small          | 0.1488474 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | small          | 0.1576977 | 0.0000000 | TRUE     |         0.01333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | small          | 0.0387613 | 0.0000006 | TRUE     |         0.01333333 | TRUE        |
| large\_xc\_catB                | xc       | catB  | large          | 0.7904963 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| liminal\_xc\_catB              | xc       | catB  | liminal        | 0.5786498 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |
| small\_xc\_catB                | xc       | catB  | small          | 0.7914976 | 0.0000000 | TRUE     |         0.06666667 | TRUE        |

Note that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level, plus `NA`
    (`xc_lev_*`)
  - the value of a (cross-validated) one-variable model for *each level*
    of `yc` as a function of `xc` (`*_xc_catB`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_catP`)

The variable `x` has been converted to two new variables:

  - a clean version of `x` that has no missing values or `NaN`s
  - a variable indicating when `x` was `NA` in the original data
    (`x_isBAD`).

Any or all of these new variables are available for downstream modeling.

Note that unlike `mkCrossFrameCExperiment` (binomial classification),
`mkCrossFrameMExperiment` produces multiple `catB` variables for a
single categorical variable: one for each possible outcome class. Each
`catB` variable is the output of a one-variable regularized logistic
regression of the original variable against a single target outcome
class. In other words, `mkCrossFrameMExperiment` treats multiclass
classification as multiple “one against rest” classification problems.
For a more detailed discussion of the `catB` variables, see the [binary
classification
example](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.Rmd).

Similarly, the `rsq` and `sig` columns report the variable’s
cross-validated correlation and its cross-validated significance as a
one-variable linear model for each target outcome.

This means that for multiclass classification problems, the score frame
has multiple rows per new variable. In this example, the score frame has
30 rows for 12 variables.

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

Since each variable has multiple recommendations, one can consider a
variable to be recommended if it is recommended for any of the outcome
targets: an OR of all the recommendations.

## Examining variables

To select variables we either make our selection in terms of new
variables as
follows.

``` r
good_new_variables = unique(score_frame[score_frame[['recommended']], 'varName', drop = TRUE])
good_new_variables
```

    ## [1] "xc_lev_NA"                "xc_lev_x_level_0"        
    ## [3] "xc_lev_x_level_0_5"       "xc_lev_x_level_1"        
    ## [5] "xc_lev_x_level_minus_0_5" "xc_catP"                 
    ## [7] "large_xc_catB"            "liminal_xc_catB"         
    ## [9] "small_xc_catB"

Or in terms of original variables as
follows.

``` r
good_original_variables = unique(score_frame[score_frame[['recommended']], 'origName', drop = TRUE])
good_original_variables
```

    ## [1] "xc"

In each case we must call `unique()` as each variable (derived or
original) is potentially qualified against each possible outcome.

## Using the Prepared Data in a Model

Of course, what we really want to do with the prepared training data is
to fit a model jointly with all the (recommended) variables. Let’s try
fitting a logistic regression model to `d_prepared`.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     unpack

    ## The following object is masked from 'package:wrapr':
    ## 
    ##     unpack

    ## Loaded glmnet 3.0-2

``` r
# use only the recommended variables for this example
model_vars <- good_new_variables

model <- glmnet(x = as.matrix(d_prepared[, model_vars, drop=FALSE]), 
               y = d_prepared[['yc']],
               family = 'multinomial')
```

``` r
# convenience functions for predicting and adding predictions to original data frame

add_predictions <- function(d_prepared, model_vars, model) {
  preds <- predict(
    model, 
    newx = as.matrix(d_prepared[, model_vars, drop=FALSE]),
    s = min(model$lambda),  # in practice we would cross-validated for a good s
    type = 'response')
  preds <- as.data.frame(preds[, , 1])
  preds$prob_on_predicted_class <- apply(preds, 1, max)
  preds$predict <- NA_character_
  for(col in colnames(preds)) {
    alter = is.na(preds$predict) & preds[[col]] >= preds$prob_on_predicted_class
    preds$predict[alter] <- col
  }
  d_prepared <- cbind(d_prepared, preds)
  d_prepared
}

add_value_by_column <- function(d_prepared, name_column, new_column) {
  vals = unique(d_prepared[[name_column]])
  d_prepared[[new_column]] <- NA_real_
  for(col in vals) {
    match <- d_prepared[[name_column]] == col
    d_prepared[[new_column]][match] <- d_prepared[match, col, drop=TRUE]
  }
  d_prepared
}
```

``` r
# now predict
d_prepared <- add_predictions(d_prepared, model_vars, model)
d_prepared <- add_value_by_column(d_prepared, 'yc', 'prob_on_correct_class')

to_print <- c('yc', 'predict', 'large','liminal','small', 'prob_on_predicted_class', 'prob_on_correct_class')
d_prepared[, to_print, drop = FALSE] %.>%
  head(.) %.>%
  knitr::kable(.)
```

| yc    | predict |     large |   liminal |     small | prob\_on\_predicted\_class | prob\_on\_correct\_class |
| :---- | :------ | --------: | --------: | --------: | -------------------------: | -----------------------: |
| large | large   | 0.9999509 | 0.0000491 | 0.0000000 |                  0.9999509 |                0.9999509 |
| large | large   | 0.9999472 | 0.0000528 | 0.0000000 |                  0.9999472 |                0.9999472 |
| large | large   | 0.9999509 | 0.0000491 | 0.0000000 |                  0.9999509 |                0.9999509 |
| large | large   | 0.5977609 | 0.4020513 | 0.0001879 |                  0.5977609 |                0.5977609 |
| small | small   | 0.0000012 | 0.0000695 | 0.9999292 |                  0.9999292 |                0.9999292 |
| small | small   | 0.0001415 | 0.3623118 | 0.6375467 |                  0.6375467 |                0.6375467 |

``` r
table(truth = d_prepared$yc, prediction = d_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     162       0     0
    ##   liminal    39     104    21
    ##   small       0      10   164

In the above confusion matrix, the entry `[row, column]` gives the
number of true items of `class[row]` that also have prediction of
`class[column]`. In other words, the entry `['large', 'liminal']` gives
the number of ‘large’ items predicted to be ‘liminal’.

Now apply the model to new data.

``` r
# create the new data
dtest <- make_data(450)

# prepare the new data with vtreat
dtest_prepared = prepare(treatment_plan, dtest)
# dtest %.>% transform is an alias for prepare(transform, dtest)

dtest_prepared <- add_predictions(dtest_prepared, model_vars, model)
dtest_prepared <- add_value_by_column(dtest_prepared, 'yc', 'prob_on_correct_class')

dtest_prepared[, to_print, drop = FALSE] %.>%
  head(.) %.>%
  knitr::kable(.)
```

| yc      | predict |     large |   liminal |     small | prob\_on\_predicted\_class | prob\_on\_correct\_class |
| :------ | :------ | --------: | --------: | --------: | -------------------------: | -----------------------: |
| small   | small   | 0.0001459 | 0.4512153 | 0.5486388 |                  0.5486388 |                0.5486388 |
| small   | small   | 0.0001459 | 0.4512153 | 0.5486388 |                  0.5486388 |                0.5486388 |
| small   | small   | 0.0000021 | 0.0001456 | 0.9998523 |                  0.9998523 |                0.9998523 |
| liminal | liminal | 0.0000054 | 0.9996821 | 0.0003125 |                  0.9996821 |                0.9996821 |
| large   | large   | 0.9999566 | 0.0000434 | 0.0000000 |                  0.9999566 |                0.9999566 |
| liminal | large   | 0.6084001 | 0.3914308 | 0.0001691 |                  0.6084001 |                0.3914308 |

``` r
table(truth = dtest_prepared$yc, prediction = dtest_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     150       0     0
    ##   liminal    38      67    38
    ##   small       0       0   157

## Parameters for `mkCrossFrameMExperiment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications.

``` r
multinomial_parameters()
```

    ## $minFraction
    ## [1] 0.02
    ## 
    ## $smFactor
    ## [1] 0
    ## 
    ## $rareCount
    ## [1] 0
    ## 
    ## $rareSig
    ## [1] 1
    ## 
    ## $collarProb
    ## [1] 0
    ## 
    ## $codeRestriction
    ## NULL
    ## 
    ## $customCoders
    ## NULL
    ## 
    ## $scale
    ## [1] FALSE
    ## 
    ## $doCollar
    ## [1] FALSE
    ## 
    ## $splitFunction
    ## NULL
    ## 
    ## $ncross
    ## [1] 3
    ## 
    ## $forceSplit
    ## [1] FALSE
    ## 
    ## $catScaling
    ## [1] FALSE
    ## 
    ## $y_dependent_treatments
    ## [1] "catB"
    ## 
    ## $verbose
    ## [1] FALSE
    ## 
    ## $use_parallel
    ## [1] TRUE
    ## 
    ## $missingness_imputation
    ## NULL
    ## 
    ## $imputation_map
    ## NULL
    ## 
    ## $check_for_duplicate_frames
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "multinomial_parameters"

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
of types (`clean`, `isBAD`, and `lev`), and no `catB` or `catP`
variables.

``` r
# make a new parameter list, overriding the default for codeRestriction
newparams = multinomial_parameters(
  list(codeRestriction = c('lev', 'clean', 'isBad'))
)

transform_spec <- vtreat::MultinomialOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcome_name = 'yc',                            # outcome variable
    params = newparams                              # parameter list
)

unpack[
  treatment_plan_thin = treatments,
  d_prepared_thin = cross_frame
  ] <- fit_prepare(transform_spec, d)     


d_prepared_thin %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | yc    |
| ----------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | :---- |
|   1.8848606 |   0.0046504 |           0 |                              0 |                    0 |                       0 |                    1 | large |
|   1.5077419 | \-1.2287497 |           0 |                              0 |                    0 |                       0 |                    1 | large |
| \-5.4901159 | \-0.1405980 |           0 |                              0 |                    0 |                       0 |                    1 | large |
| \-0.2704873 | \-0.2073270 |           0 |                              0 |                    0 |                       1 |                    0 | large |
| \-0.2704873 | \-0.9215306 |           1 |                              0 |                    0 |                       0 |                    0 | small |
| \-0.2704873 |   0.3604742 |           0 |                              1 |                    0 |                       0 |                    0 | small |

``` r
score_frame_thin = get_score_frame(treatment_plan_thin)

# no catB or catP
knitr::kable(score_frame_thin[,cols])
```

| varName                        | origName | code  | outcome\_level |       rsq |       sig | varMoves | default\_threshold | recommended |
| :----------------------------- | :------- | :---- | :------------- | --------: | --------: | :------- | -----------------: | :---------- |
| x                              | x        | clean | large          | 0.0005756 | 0.5470919 | TRUE     |         0.08333333 | FALSE       |
| x2                             | x2       | clean | large          | 0.0026075 | 0.2000083 | TRUE     |         0.08333333 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | large          | 0.1750095 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | large          | 0.1185254 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | large          | 0.0644178 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | large          | 0.4701626 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | large          | 0.1328708 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| x                              | x        | clean | liminal        | 0.0022396 | 0.2338771 | TRUE     |         0.08333333 | FALSE       |
| x2                             | x2       | clean | liminal        | 0.0023614 | 0.2215731 | TRUE     |         0.08333333 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | liminal        | 0.1769596 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | liminal        | 0.3615209 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | liminal        | 0.0041777 | 0.1039765 | TRUE     |         0.03333333 | FALSE       |
| xc\_lev\_x\_level\_1           | xc       | lev   | liminal        | 0.1492650 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | liminal        | 0.0076508 | 0.0277896 | TRUE     |         0.03333333 | TRUE        |
| x                              | x        | clean | small          | 0.0048286 | 0.0773262 | TRUE     |         0.08333333 | TRUE        |
| x2                             | x2       | clean | small          | 0.0000045 | 0.9569844 | TRUE     |         0.08333333 | FALSE       |
| xc\_lev\_NA                    | xc       | lev   | small          | 0.5132320 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0           | xc       | lev   | small          | 0.1265119 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_0\_5        | xc       | lev   | small          | 0.1488474 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_1           | xc       | lev   | small          | 0.1576977 | 0.0000000 | TRUE     |         0.03333333 | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | xc       | lev   | small          | 0.0387613 | 0.0000006 | TRUE     |         0.03333333 | TRUE        |

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
*clean* and *isBAD*) for every numerical variable in the original data.
Categorical variables, on the other hand, are expanded to many derived
variables: several indicators (one for every common level), plus a
*catB* and a *catP*. So we now reweight the thresholds.

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
