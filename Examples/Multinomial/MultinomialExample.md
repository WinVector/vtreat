Multinomial Classification
================

# Using [vtreat](https://github.com/WinVector/vtreat) with Multinomial Classification Problems

Nina Zumel and John Mount October 2019

Note this is a description of the [`R` version of
`vtreat`](https://github.com/WinVector/vtreat), the same example for the
[`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat)
can be found
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/Multinomial/MultinomialExample.md).

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
  - `yc` is the output to be predicted: `y`‘s quantized value as
    ’large’, ‘liminal’, or ‘small’.
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
    d['yc'] = ifelse(d[['y']] > 0.5, 'large', ifelse(d[['y']] < -0.5, 'small', 'liminal'))
    return(d)
}

d = make_data(500)

d %.>%
  head(.) %.>%
  knitr::kable(.)
```

|         x |           y | xc          |          x2 | yc      |
| --------: | ----------: | :---------- | ----------: | :------ |
| 3.0695143 |   0.1012494 | level\_0    |   0.2686272 | liminal |
| 0.5503708 |   0.6178242 | level\_0.5  |   0.3114085 | large   |
| 1.1043702 |   0.9991143 | level\_1    | \-1.7158117 | large   |
|        NA | \-0.5676860 | level\_-0.5 | \-0.2520499 | small   |
|        NA |   0.8505856 | level\_1    | \-0.4297444 | large   |
|        NA | \-1.0006259 | NA          | \-0.1064941 | small   |

### Some quick data exploration

Check how many levels `xc` has, and their distribution (including `NA`)

``` r
unique(d['xc'])
```

    ##           xc
    ## 1    level_0
    ## 2  level_0.5
    ## 3    level_1
    ## 4 level_-0.5
    ## 6       <NA>

``` r
table(d$xc, useNA = 'ifany')
```

    ## 
    ## level_-0.5    level_0  level_0.5    level_1       <NA> 
    ##         98         85         93        101        123

Show the distribution of `yc`

``` r
table(d$yc, useNA = 'ifany')
```

    ## 
    ##   large liminal   small 
    ##     153     165     182

## Build a transform appropriate for classification problems.

Now that we have the data, we want to treat it prior to modeling: we
want training data where all the input variables are numeric and have no
missing values or `NA`s.

First create the data treatment transform design object, in this case a
treatment for a binomial classification problem.

We use the training data `d` to fit the transform and the return a
treated training set: completely numeric, with no missing values.

``` r
transform_design = vtreat::mkCrossFrameMExperiment(
    d,                                    # data to learn transform from
    setdiff(colnames(d), c('y', 'yc')),   # columns to transform
    'yc'                                  # outcome variable
)
transform <- transform_design$treat_m
d_prepared <- transform_design$cross_frame
score_frame <- transform_design$score_frame
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

| varName                        | varMoves |       rsq |       sig | outcome\_level | needsSplit | extraModelDegrees | origName | code  | recommended |
| :----------------------------- | :------- | --------: | --------: | :------------- | :--------- | ----------------: | :------- | :---- | :---------- |
| x                              | TRUE     | 0.0026764 | 0.1991902 | large          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0000226 | 0.9060401 | large          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0000587 | 0.8491972 | large          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                       | TRUE     | 0.0236742 | 0.0001343 | large          | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                    | TRUE     | 0.1732225 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1128791 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0522128 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.4985401 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1326932 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                              | TRUE     | 0.0000038 | 0.9610829 | liminal        | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0001018 | 0.7994693 | liminal        | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0018235 | 0.2822063 | liminal        | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                       | TRUE     | 0.5083095 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                    | TRUE     | 0.1851534 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.3584151 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0096759 | 0.0132440 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1467090 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0039249 | 0.1146390 | liminal        | FALSE      |                 0 | xc       | lev   | FALSE       |
| x                              | TRUE     | 0.0024793 | 0.2023078 | small          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0001909 | 0.7235258 | small          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0023251 | 0.2169384 | small          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                       | TRUE     | 0.5298248 | 0.0000000 | small          | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                    | TRUE     | 0.5011343 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1321613 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.1464341 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1610951 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0439306 | 0.0000001 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_catB                | TRUE     | 0.7927112 | 0.0000000 | large          | TRUE       |                 4 | xc       | catB  | TRUE        |
| liminal\_xc\_catB              | TRUE     | 0.5886232 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catB  | TRUE        |
| small\_xc\_catB                | TRUE     | 0.7961205 | 0.0000000 | small          | TRUE       |                 4 | xc       | catB  | TRUE        |

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
below for the reasoning behind the default thresholds. Recommended
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

    ## [1] "xc_catP"                  "xc_lev_NA"               
    ## [3] "xc_lev_x_level_0"         "xc_lev_x_level_0_5"      
    ## [5] "xc_lev_x_level_1"         "xc_lev_x_level_minus_0_5"
    ## [7] "large_xc_catB"            "liminal_xc_catB"         
    ## [9] "small_xc_catB"

Or in terms of original variables as
follows.

``` r
good_original_variables = unique(score_frame[score_frame[['recommended']], 'origName', drop = TRUE])
good_original_variables
```

    ## [1] "xc"

Notice, in each case we must call `unique()` as each variable (derived
or original) is potentially qualified against each possible outcome.

Notice that `d_prepared` only includes derived variables and the outcome
`yc`:

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD | xc\_catP |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | large\_xc\_catB | liminal\_xc\_catB | small\_xc\_catB | yc      |
| ----------: | -------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | --------------: | ----------------: | --------------: | :------ |
|   3.0695143 |        0 |    0.170 |   0.2686272 |           0 |                              0 |                    1 |                       0 |                    0 |    \-12.3441417 |        13.8873251 |     \-12.718220 | liminal |
|   0.5503708 |        0 |    0.186 |   0.3114085 |           0 |                              0 |                    0 |                       1 |                    0 |       0.7856958 |         0.6082512 |     \-12.823934 | large   |
|   1.1043702 |        0 |    0.202 | \-1.7158117 |           0 |                              0 |                    0 |                       0 |                    1 |      14.2904660 |      \-12.6715839 |     \-12.862470 | large   |
| \-0.6383522 |        1 |    0.196 | \-0.2520499 |           0 |                              1 |                    0 |                       0 |                    0 |    \-12.6652439 |         0.5433675 |        1.008039 | small   |
| \-0.6383522 |        1 |    0.202 | \-0.4297444 |           0 |                              0 |                    0 |                       0 |                    1 |      14.2324788 |      \-12.6625341 |     \-12.727084 | large   |
| \-0.6383522 |        1 |    0.246 | \-0.1064941 |           1 |                              0 |                    0 |                       0 |                    0 |    \-12.7060658 |      \-12.8207773 |       14.157356 | small   |

## Using the Prepared Data in a Model

Of course, what we really want to do with the prepared training data is
to fit a model jointly with all the (recommended) variables. Let’s try
fitting a logistic regression model to `d_prepared`.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 3.0-2

``` r
model_vars <- score_frame$varName[score_frame$recommended]

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

| yc      | predict |     large |   liminal |     small | prob\_on\_predicted\_class | prob\_on\_correct\_class |
| :------ | :------ | --------: | --------: | --------: | -------------------------: | -----------------------: |
| liminal | liminal | 0.0000672 | 0.9999105 | 0.0000223 |                  0.9999105 |                0.9999105 |
| large   | large   | 0.6682928 | 0.3315924 | 0.0001147 |                  0.6682928 |                0.6682928 |
| large   | large   | 0.9998971 | 0.0001027 | 0.0000002 |                  0.9998971 |                0.9998971 |
| small   | small   | 0.0001084 | 0.2781019 | 0.7217897 |                  0.7217897 |                0.7217897 |
| large   | large   | 0.9999045 | 0.0000953 | 0.0000002 |                  0.9999045 |                0.9999045 |
| small   | small   | 0.0000188 | 0.0000494 | 0.9999318 |                  0.9999318 |                0.9999318 |

``` r
table(truth = d_prepared$yc, prediction = d_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     150       3     0
    ##   liminal    34      92    39
    ##   small       0       0   182

In the above confusion matrix, the entry `[row, column]` gives the
number of true items of `class[row]` that also have prediction of
`class[column]`. In other words, the entry `['large', 'liminal']` gives
the number of ‘large’ items predicted to be ‘liminal’.

Now apply the model to new data.

``` r
# create the new data
dtest <- make_data(450)

# prepare the new data with vtreat
dtest_prepared = prepare(transform, dtest)

dtest_prepared <- add_predictions(dtest_prepared, model_vars, model)
dtest_prepared <- add_value_by_column(dtest_prepared, 'yc', 'prob_on_correct_class')

dtest_prepared[, to_print, drop = FALSE] %.>%
  head(.) %.>%
  knitr::kable(.)
```

| yc      | predict |     large |   liminal |     small | prob\_on\_predicted\_class | prob\_on\_correct\_class |
| :------ | :------ | --------: | --------: | --------: | -------------------------: | -----------------------: |
| liminal | liminal | 0.0001903 | 0.9997447 | 0.0000650 |                  0.9997447 |                0.9997447 |
| small   | small   | 0.0000199 | 0.0000501 | 0.9999301 |                  0.9999301 |                0.9999301 |
| large   | large   | 0.5633487 | 0.4365679 | 0.0000834 |                  0.5633487 |                0.5633487 |
| small   | small   | 0.0000199 | 0.0000501 | 0.9999301 |                  0.9999301 |                0.9999301 |
| liminal | liminal | 0.0001903 | 0.9997447 | 0.0000650 |                  0.9997447 |                0.9997447 |
| large   | large   | 0.9997643 | 0.0002355 | 0.0000001 |                  0.9997643 |                0.9997643 |

``` r
table(truth = dtest_prepared$yc, prediction = dtest_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     163       0     0
    ##   liminal    33      76    39
    ##   small       0       0   139

## Parameters for `mkCrossFrameMExperiment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications.

``` r
suppressPackageStartupMessages(library(printr))
help("mkCrossFrameMExperiment")
```

    ## Function to build multi-outcome vtreat cross frame and treatment plan.
    ## 
    ## Description:
    ## 
    ##      Please see 'vignette("MultiClassVtreat", package = "vtreat")'
    ##      <URL:
    ##      https://winvector.github.io/vtreat/articles/MultiClassVtreat.html>.
    ## 
    ## Usage:
    ## 
    ##      mkCrossFrameMExperiment(
    ##        dframe,
    ##        varlist,
    ##        outcomename,
    ##        ...,
    ##        weights = c(),
    ##        minFraction = 0.02,
    ##        smFactor = 0,
    ##        rareCount = 0,
    ##        rareSig = 1,
    ##        collarProb = 0,
    ##        codeRestriction = NULL,
    ##        customCoders = NULL,
    ##        scale = FALSE,
    ##        doCollar = FALSE,
    ##        splitFunction = NULL,
    ##        ncross = 3,
    ##        forceSplit = FALSE,
    ##        catScaling = FALSE,
    ##        y_dependent_treatments = c("catB"),
    ##        verbose = FALSE,
    ##        parallelCluster = NULL,
    ##        use_parallel = TRUE,
    ##        missingness_imputation = NULL,
    ##        imputation_map = NULL
    ##      )
    ##      
    ## Arguments:
    ## 
    ##   dframe: data to learn from
    ## 
    ##  varlist: character, vector of indpendent variable column names.
    ## 
    ## outcomename: character, name of outcome column.
    ## 
    ##      ...: not used, declared to forced named binding of later arguments
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
    ##           data at if doCollar is set during 'prepare.multinomial_plan'.
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
    ## y_dependent_treatments: character what treatment types to build
    ##           per-outcome level.
    ## 
    ##  verbose: if TRUE print progress.
    ## 
    ## parallelCluster: (optional) a cluster object created by package
    ##           parallel or package snow.
    ## 
    ## use_parallel: logical, if TRUE use parallel methods.
    ## 
    ## missingness_imputation: function of signature f(values: numeric,
    ##           weights: numeric), simple missing value imputer.
    ## 
    ## imputation_map: map from column names to functions of signature
    ##           f(values: numeric, weights: numeric), simple missing value
    ##           imputers.
    ## 
    ## Value:
    ## 
    ##      list(cross_frame, treatments_0, treatments_m)
    ## 
    ## See Also:
    ## 
    ##      'prepare.multinomial_plan'

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
transform_design_thin = vtreat::mkCrossFrameMExperiment(
    d,                                    # data to learn transform from
    setdiff(colnames(d), c('y', 'yc')),   # columns to transform
    'yc',                                 # outcome variable
    codeRestriction = c('lev',            # transforms we want
                        'clean',
                        'isBAD')
)
transform_thin <- transform_design_thin$treat_m
d_prepared_thin <- transform_design_thin$cross_frame
score_frame_thin <- transform_design_thin$score_frame
score_frame_thin$recommended <- score_frame_thin$varMoves & (score_frame_thin$sig < 1/nrow(score_frame))


d_prepared_thin %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 | large\_xc\_catB | liminal\_xc\_catB | small\_xc\_catB | yc      |
| ----------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | --------------: | ----------------: | --------------: | :------ |
|   3.0695143 |        0 |   0.2686272 |           0 |                              0 |                    1 |                       0 |                    0 |     \-12.487242 |        13.9423848 |    \-12.6925980 | liminal |
|   0.5503708 |        0 |   0.3114085 |           0 |                              0 |                    0 |                       1 |                    0 |        1.055855 |         0.3556558 |    \-12.7438912 | large   |
|   1.1043702 |        0 | \-1.7158117 |           0 |                              0 |                    0 |                       0 |                    1 |       14.261893 |      \-12.7468072 |    \-12.7438912 | large   |
| \-0.6383522 |        1 | \-0.2520499 |           0 |                              1 |                    0 |                       0 |                    0 |     \-12.607346 |         0.2648236 |       0.9795043 | small   |
| \-0.6383522 |        1 | \-0.4297444 |           0 |                              0 |                    0 |                       0 |                    1 |       14.246651 |      \-12.7468072 |    \-12.9344430 | large   |
| \-0.6383522 |        1 | \-0.1064941 |           1 |                              0 |                    0 |                       0 |                    0 |     \-12.811738 |      \-13.0145100 |      14.1449331 | small   |

``` r
knitr::kable(score_frame_thin)
```

| varName                        | varMoves |       rsq |       sig | outcome\_level | needsSplit | extraModelDegrees | origName | code  | recommended |
| :----------------------------- | :------- | --------: | --------: | :------------- | :--------- | ----------------: | :------- | :---- | :---------- |
| x                              | TRUE     | 0.0026764 | 0.1991902 | large          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0000226 | 0.9060401 | large          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0000587 | 0.8491972 | large          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1732225 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1128791 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0522128 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.4985401 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1326932 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                              | TRUE     | 0.0000038 | 0.9610829 | liminal        | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0001018 | 0.7994693 | liminal        | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0018235 | 0.2822063 | liminal        | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1851534 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.3584151 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0096759 | 0.0132440 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1467090 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0039249 | 0.1146390 | liminal        | FALSE      |                 0 | xc       | lev   | FALSE       |
| x                              | TRUE     | 0.0024793 | 0.2023078 | small          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0001909 | 0.7235258 | small          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0023251 | 0.2169384 | small          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.5011343 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1321613 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.1464341 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1610951 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0439306 | 0.0000001 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_catB                | TRUE     | 0.7913416 | 0.0000000 | large          | TRUE       |                 4 | xc       | catB  | TRUE        |
| liminal\_xc\_catB              | TRUE     | 0.5888541 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catB  | TRUE        |
| small\_xc\_catB                | TRUE     | 0.7980369 | 0.0000000 | small          | TRUE       |                 4 | xc       | catB  | TRUE        |

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
