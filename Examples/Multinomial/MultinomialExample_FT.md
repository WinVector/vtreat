Multinomial Classification Fit Transform Notation
================

# Using [vtreat](https://github.com/WinVector/vtreat) with Multinomial Classification Problems

Nina Zumel and John Mount October 2019

Note this the “fit\_transform” variation (a notation closer to that used
in [`pyvtreat`](https://github.com/WinVector/pyvtreat)) of the
description of the [`R` version of
`vtreat`](https://github.com/WinVector/vtreat). The original `vtreat`
notation version can be found
[here](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md).
The same example for the [`Python` version of
`vtreat`](https://github.com/WinVector/pyvtreat) can be found
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

|           x |           y | xc          |          x2 | yc      |
| ----------: | ----------: | :---------- | ----------: | :------ |
| \-0.7840876 | \-0.5717529 | level\_-0.5 | \-0.0227714 | small   |
| \-2.5296060 | \-0.3573784 | level\_-0.5 |   0.4932385 | liminal |
| \-5.5373208 |   0.5699362 | level\_0.5  |   0.5064529 | large   |
|          NA | \-0.2956794 | level\_-0.5 | \-0.4408826 | liminal |
|          NA | \-1.1804133 | NA          | \-0.2036141 | small   |
|          NA | \-0.5376251 | level\_-0.5 |   0.6698015 | small   |

### Some quick data exploration

Check how many levels `xc` has, and their distribution (including `NA`)

``` r
unique(d['xc'])
```

    ##            xc
    ## 1  level_-0.5
    ## 3   level_0.5
    ## 5        <NA>
    ## 13    level_0
    ## 17    level_1

``` r
table(d$xc, useNA = 'ifany')
```

    ## 
    ## level_-0.5    level_0  level_0.5    level_1       <NA> 
    ##        103         76         89        114        118

Show the distribution of `yc`

``` r
table(d$yc, useNA = 'ifany')
```

    ## 
    ##   large liminal   small 
    ##     166     165     169

## Build a transform appropriate for classification problems.

Now that we have the data, we want to treat it prior to modeling: we
want training data where all the input variables are numeric and have no
missing values or `NA`s.

First create the data treatment transform design object, in this case a
treatment for a binomial classification problem.

We use the training data `d` to fit the transform and the return a
treated training set: completely numeric, with no missing values.

``` r
transform_design = vtreat::MultinomialOutcomeTreatment(
    var_list = setdiff(colnames(d), c('y', 'yc')),  # columns to transform
    outcome_name = 'yc',                             # outcome variable
    cols_to_copy = c('y', 'yc')
)

# learn transform from data
d_prepared <-  transform_design$fit_transform(d)

# get statistics on the variables
score_frame <- transform_design$score_frame()
score_frame$recommended <- score_frame$varMoves & (score_frame$sig < 1/nrow(score_frame))
```

Note that for the training data `d`: `transform_design$fit_transform(d)`
is **not** the same as `transform_design$fit(d)$transform(d)`; the
second call can lead to nested model bias in some situations, and is
**not** recommended. For other, later data, not seen during transform
design `transform_design$transform(o)` is an appropriate step.

Now examine the score frame, which gives information about each new
variable, including its type, which original variable it is derived
from, its (cross-validated) correlation with the outcome, and its
(cross-validated) significance as a one-variable linear model for the
outcome.

``` r
knitr::kable(score_frame)
```

| varName                                 | varMoves |       rsq |       sig | outcome\_level | needsSplit | extraModelDegrees | origName | code  | recommended |
| :-------------------------------------- | :------- | --------: | --------: | :------------- | :--------- | ----------------: | :------- | :---- | :---------- |
| x                                       | TRUE     | 0.0013948 | 0.3464151 | large          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                                | TRUE     | 0.0089647 | 0.0169860 | large          | FALSE      |                 0 | x        | isBAD | TRUE        |
| x2                                      | TRUE     | 0.0005061 | 0.5705974 | large          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                                | TRUE     | 0.0248109 | 0.0000715 | large          | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                             | TRUE     | 0.1771355 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0                    | TRUE     | 0.1068606 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5                 | TRUE     | 0.0462447 | 0.0000001 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1                    | TRUE     | 0.5199156 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5          | TRUE     | 0.1509119 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                                       | TRUE     | 0.0033441 | 0.1453171 | liminal        | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                                | TRUE     | 0.0004722 | 0.5842251 | liminal        | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                                      | TRUE     | 0.0000064 | 0.9490227 | liminal        | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                                | TRUE     | 0.4322663 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                             | TRUE     | 0.1761566 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0                    | TRUE     | 0.3129235 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5                 | TRUE     | 0.0055153 | 0.0614540 | liminal        | FALSE      |                 0 | xc       | lev   | FALSE       |
| xc\_lev\_x\_level\_1                    | TRUE     | 0.1690724 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5          | TRUE     | 0.0270612 | 0.0000343 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                                       | TRUE     | 0.0004019 | 0.6121350 | small          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                                | TRUE     | 0.0025264 | 0.2036305 | small          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                                      | TRUE     | 0.0003938 | 0.6157476 | small          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_catP                                | TRUE     | 0.2758163 | 0.0000000 | small          | TRUE       |                 4 | xc       | catP  | TRUE        |
| xc\_lev\_NA                             | TRUE     | 0.5306350 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0                    | TRUE     | 0.1086074 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5                 | TRUE     | 0.1296983 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1                    | TRUE     | 0.1728541 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5          | TRUE     | 0.0215315 | 0.0002062 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_x                                | TRUE     | 0.0013948 | 0.3464151 | large          | FALSE      |                 0 | x        | clean | FALSE       |
| large\_x\_isBAD                         | TRUE     | 0.0089647 | 0.0169860 | large          | FALSE      |                 0 | x        | isBAD | TRUE        |
| large\_xc\_catP                         | TRUE     | 0.0281796 | 0.0000232 | large          | TRUE       |                 4 | xc       | catP  | TRUE        |
| large\_xc\_catB                         | TRUE     | 0.8060001 | 0.0000000 | large          | TRUE       |                 4 | xc       | catB  | TRUE        |
| large\_x2                               | TRUE     | 0.0005061 | 0.5705974 | large          | FALSE      |                 0 | x2       | clean | FALSE       |
| large\_xc\_lev\_NA                      | TRUE     | 0.1771355 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_lev\_x\_level\_minus\_0\_5   | TRUE     | 0.1509119 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_lev\_x\_level\_0             | TRUE     | 0.1068606 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_lev\_x\_level\_0\_5          | TRUE     | 0.0462447 | 0.0000001 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_lev\_x\_level\_1             | TRUE     | 0.5199156 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| liminal\_x                              | TRUE     | 0.0033441 | 0.1453171 | liminal        | FALSE      |                 0 | x        | clean | FALSE       |
| liminal\_x\_isBAD                       | TRUE     | 0.0004722 | 0.5842251 | liminal        | FALSE      |                 0 | x        | isBAD | FALSE       |
| liminal\_xc\_catP                       | TRUE     | 0.3846100 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catP  | TRUE        |
| liminal\_xc\_catB                       | TRUE     | 0.5772563 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catB  | TRUE        |
| liminal\_x2                             | TRUE     | 0.0000064 | 0.9490227 | liminal        | FALSE      |                 0 | x2       | clean | FALSE       |
| liminal\_xc\_lev\_NA                    | TRUE     | 0.1761566 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| liminal\_xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0270612 | 0.0000343 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| liminal\_xc\_lev\_x\_level\_0           | TRUE     | 0.3129235 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| liminal\_xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0055153 | 0.0614540 | liminal        | FALSE      |                 0 | xc       | lev   | FALSE       |
| liminal\_xc\_lev\_x\_level\_1           | TRUE     | 0.1690724 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| small\_x                                | TRUE     | 0.0004019 | 0.6121350 | small          | FALSE      |                 0 | x        | clean | FALSE       |
| small\_x\_isBAD                         | TRUE     | 0.0025264 | 0.2036305 | small          | FALSE      |                 0 | x        | isBAD | FALSE       |
| small\_xc\_catP                         | TRUE     | 0.2181352 | 0.0000000 | small          | TRUE       |                 4 | xc       | catP  | TRUE        |
| small\_xc\_catB                         | TRUE     | 0.7748964 | 0.0000000 | small          | TRUE       |                 4 | xc       | catB  | TRUE        |
| small\_x2                               | TRUE     | 0.0003938 | 0.6157476 | small          | FALSE      |                 0 | x2       | clean | FALSE       |
| small\_xc\_lev\_NA                      | TRUE     | 0.5306350 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| small\_xc\_lev\_x\_level\_minus\_0\_5   | TRUE     | 0.0215315 | 0.0002062 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| small\_xc\_lev\_x\_level\_0             | TRUE     | 0.1086074 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| small\_xc\_lev\_x\_level\_0\_5          | TRUE     | 0.1296983 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| small\_xc\_lev\_x\_level\_1             | TRUE     | 0.1728541 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |

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

    ##  [1] "x_isBAD"                          "xc_catP"                         
    ##  [3] "xc_lev_NA"                        "xc_lev_x_level_0"                
    ##  [5] "xc_lev_x_level_0_5"               "xc_lev_x_level_1"                
    ##  [7] "xc_lev_x_level_minus_0_5"         "large_x_isBAD"                   
    ##  [9] "large_xc_catP"                    "large_xc_catB"                   
    ## [11] "large_xc_lev_NA"                  "large_xc_lev_x_level_minus_0_5"  
    ## [13] "large_xc_lev_x_level_0"           "large_xc_lev_x_level_0_5"        
    ## [15] "large_xc_lev_x_level_1"           "liminal_xc_catP"                 
    ## [17] "liminal_xc_catB"                  "liminal_xc_lev_NA"               
    ## [19] "liminal_xc_lev_x_level_minus_0_5" "liminal_xc_lev_x_level_0"        
    ## [21] "liminal_xc_lev_x_level_1"         "small_xc_catP"                   
    ## [23] "small_xc_catB"                    "small_xc_lev_NA"                 
    ## [25] "small_xc_lev_x_level_minus_0_5"   "small_xc_lev_x_level_0"          
    ## [27] "small_xc_lev_x_level_0_5"         "small_xc_lev_x_level_1"

Or in terms of original variables as
follows.

``` r
good_original_variables = unique(score_frame[score_frame[['recommended']], 'origName', drop = TRUE])
good_original_variables
```

    ## [1] "x"  "xc"

Notice, in each case we must call `unique()` as each variable (derived
or original) is potentially qualified against each possible outcome.

Notice that `d_prepared` only includes derived variables and the outcome
`yc`:

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD | xc\_catP |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 |    large\_x | large\_x\_isBAD | large\_xc\_catP | large\_xc\_catB |   large\_x2 | large\_xc\_lev\_NA | large\_xc\_lev\_x\_level\_minus\_0\_5 | large\_xc\_lev\_x\_level\_0 | large\_xc\_lev\_x\_level\_0\_5 | large\_xc\_lev\_x\_level\_1 |  liminal\_x | liminal\_x\_isBAD | liminal\_xc\_catP | liminal\_xc\_catB | liminal\_x2 | liminal\_xc\_lev\_NA | liminal\_xc\_lev\_x\_level\_minus\_0\_5 | liminal\_xc\_lev\_x\_level\_0 | liminal\_xc\_lev\_x\_level\_0\_5 | liminal\_xc\_lev\_x\_level\_1 |    small\_x | small\_x\_isBAD | small\_xc\_catP | small\_xc\_catB |   small\_x2 | small\_xc\_lev\_NA | small\_xc\_lev\_x\_level\_minus\_0\_5 | small\_xc\_lev\_x\_level\_0 | small\_xc\_lev\_x\_level\_0\_5 | small\_xc\_lev\_x\_level\_1 | yc      |
| ----------: | -------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | ----------: | --------------: | --------------: | --------------: | ----------: | -----------------: | ------------------------------------: | --------------------------: | -----------------------------: | --------------------------: | ----------: | ----------------: | ----------------: | ----------------: | ----------: | -------------------: | --------------------------------------: | ----------------------------: | -------------------------------: | ----------------------------: | ----------: | --------------: | --------------: | --------------: | ----------: | -----------------: | ------------------------------------: | --------------------------: | -----------------------------: | --------------------------: | :------ |
| \-0.7840876 |        0 |    0.206 | \-0.0227714 |           0 |                              1 |                    0 |                       0 |                    0 | \-0.7840876 |               0 |       0.2065868 |    \-12.7602703 | \-0.0227714 |                  0 |                                     1 |                           0 |                              0 |                           0 | \-0.7840876 |                 0 |         0.2155689 |         0.6420714 | \-0.0227714 |                    0 |                                       1 |                             0 |                                0 |                             0 | \-0.7840876 |               0 |       0.2132132 |       0.9345549 | \-0.0227714 |                  0 |                                     1 |                           0 |                              0 |                           0 | small   |
| \-2.5296060 |        0 |    0.206 |   0.4932385 |           0 |                              1 |                    0 |                       0 |                    0 | \-2.5296060 |               0 |       0.1891892 |    \-12.6467857 |   0.4932385 |                  0 |                                     1 |                           0 |                              0 |                           0 | \-2.5296060 |                 0 |         0.1951952 |         0.6759194 |   0.4932385 |                    0 |                                       1 |                             0 |                                0 |                             0 | \-2.5296060 |               0 |       0.2035928 |       0.4938443 |   0.4932385 |                  0 |                                     1 |                           0 |                              0 |                           0 | liminal |
| \-5.5373208 |        0 |    0.178 |   0.5064529 |           0 |                              0 |                    0 |                       1 |                    0 | \-5.5373208 |               0 |       0.1771772 |       0.9451012 |   0.5064529 |                  0 |                                     0 |                           0 |                              1 |                           0 | \-5.5373208 |                 0 |         0.1951952 |         0.3785495 |   0.5064529 |                    0 |                                       0 |                             0 |                                1 |                             0 | \-5.5373208 |               0 |       0.1766467 |    \-12.6171051 |   0.5064529 |                  0 |                                     0 |                           0 |                              1 |                           0 | large   |
|   0.0833948 |        1 |    0.206 | \-0.4408826 |           0 |                              1 |                    0 |                       0 |                    0 |   0.2966566 |               1 |       0.1891892 |    \-12.6467857 | \-0.4408826 |                  0 |                                     1 |                           0 |                              0 |                           0 |   0.1688803 |                 1 |         0.2072072 |         0.8654793 | \-0.4408826 |                    0 |                                       1 |                             0 |                                0 |                             0 |   0.2120892 |               1 |       0.2132132 |       0.9345549 | \-0.4408826 |                  0 |                                     1 |                           0 |                              0 |                           0 | liminal |
|   0.0833948 |        1 |    0.236 | \-0.2036141 |           1 |                              0 |                    0 |                       0 |                    0 |   0.0669934 |               1 |       0.2252252 |    \-12.8211389 | \-0.2036141 |                  1 |                                     0 |                           0 |                              0 |                           0 |   0.1321172 |                 1 |         0.2222222 |      \-12.8077159 | \-0.2036141 |                    1 |                                       0 |                             0 |                                0 |                             0 |   0.2120892 |               1 |       0.2162162 |      14.1666713 | \-0.2036141 |                  1 |                                     0 |                           0 |                              0 |                           0 | small   |
|   0.0833948 |        1 |    0.206 |   0.6698015 |           0 |                              1 |                    0 |                       0 |                    0 |   0.0669934 |               1 |       0.2222222 |    \-12.8077159 |   0.6698015 |                  0 |                                     1 |                           0 |                              0 |                           0 |   0.1688803 |                 1 |         0.2072072 |         0.8654793 |   0.6698015 |                    0 |                                       1 |                             0 |                                0 |                             0 | \-0.1233577 |               1 |       0.2012012 |       0.5167080 |   0.6698015 |                  0 |                                     1 |                           0 |                              0 |                           0 | small   |

## Using the Prepared Data in a Model

Of course, what we really want to do with the prepared training data is
to fit a model jointly with all the (recommended) variables. Let’s try
fitting a logistic regression model to `d_prepared`.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 3.0-1

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
| small   | liminal | 0.0000911 | 0.6329128 | 0.3669961 |                  0.6329128 |                0.3669961 |
| liminal | liminal | 0.0002278 | 0.5064534 | 0.4933187 |                  0.5064534 |                0.5064534 |
| large   | large   | 0.6120100 | 0.3879026 | 0.0000875 |                  0.6120100 |                0.6120100 |
| liminal | liminal | 0.0000005 | 0.6441980 | 0.3558014 |                  0.6441980 |                0.6441980 |
| small   | small   | 0.0000000 | 0.0000097 | 0.9999902 |                  0.9999902 |                0.9999902 |
| small   | small   | 0.0000001 | 0.3571343 | 0.6428657 |                  0.6428657 |                0.6428657 |

``` r
table(truth = d_prepared$yc, prediction = d_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     156      10     0
    ##   liminal    29     109    27
    ##   small       0      15   154

In the above confusion matrix, the entry `[row, column]` gives the
number of true items of `class[row]` that also have prediction of
`class[column]`. In other words, the entry `['large', 'liminal']` gives
the number of ‘large’ items predicted to be ‘liminal’.

Now apply the model to new data.

``` r
# create the new data
dtest <- make_data(450)

# prepare the new data with vtreat
dtest_prepared = transform_design$transform(dtest)

dtest_prepared <- add_predictions(dtest_prepared, model_vars, model)
dtest_prepared <- add_value_by_column(dtest_prepared, 'yc', 'prob_on_correct_class')

dtest_prepared[, to_print, drop = FALSE] %.>%
  head(.) %.>%
  knitr::kable(.)
```

| yc      | predict |     large |   liminal |     small | prob\_on\_predicted\_class | prob\_on\_correct\_class |
| :------ | :------ | --------: | --------: | --------: | -------------------------: | -----------------------: |
| liminal | large   | 0.5813638 | 0.4185649 | 0.0000712 |                  0.5813638 |                0.4185649 |
| liminal | liminal | 0.0000810 | 0.9998817 | 0.0000373 |                  0.9998817 |                0.9998817 |
| small   | small   | 0.0000084 | 0.0000490 | 0.9999425 |                  0.9999425 |                0.9999425 |
| liminal | liminal | 0.0000002 | 0.9999581 | 0.0000418 |                  0.9999581 |                0.9999581 |
| small   | small   | 0.0000000 | 0.0000438 | 0.9999562 |                  0.9999562 |                0.9999562 |
| liminal | liminal | 0.0000002 | 0.9999581 | 0.0000418 |                  0.9999581 |                0.9999581 |

``` r
table(truth = dtest_prepared$yc, prediction = dtest_prepared$predict)
```

    ##          prediction
    ## truth     large liminal small
    ##   large     141       0     0
    ##   liminal    37     121     0
    ##   small       0      62    89

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
    ##        d,
    ##        vars,
    ##        y_name,
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
    ##        d: data to learn from
    ## 
    ##     vars: character, vector of indpendent variable column names.
    ## 
    ##   y_name: character, name of outcome column.
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
    d = d,                                         # data to learn transform from
    vars = setdiff(colnames(d), c('y', 'yc')),     # columns to transform
    y_name = 'yc',                                 # outcome variable
    codeRestriction = c('lev',                     # transforms we want
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
| \-0.7840876 |        0 | \-0.0227714 |           0 |                              1 |                    0 |                       0 |                    0 |    \-12.8211389 |         0.8088664 |       0.5335737 | small   |
| \-2.5296060 |        0 |   0.4932385 |           0 |                              1 |                    0 |                       0 |                    0 |    \-12.6760783 |         0.6090724 |       0.5335737 | liminal |
| \-5.5373208 |        0 |   0.5064529 |           0 |                              0 |                    0 |                       1 |                    0 |       0.7353813 |         0.5503992 |    \-12.5826189 | large   |
|   0.0833948 |        1 | \-0.4408826 |           0 |                              1 |                    0 |                       0 |                    0 |    \-12.6760783 |         0.6090724 |       0.6323379 | liminal |
|   0.0833948 |        1 | \-0.2036141 |           1 |                              0 |                    0 |                       0 |                    0 |    \-12.9435172 |      \-12.9224912 |      14.1804646 | small   |
|   0.0833948 |        1 |   0.6698015 |           0 |                              1 |                    0 |                       0 |                    0 |    \-12.7173934 |         0.7712293 |       0.5335737 | small   |

``` r
knitr::kable(score_frame_thin)
```

| varName                        | varMoves |       rsq |       sig | outcome\_level | needsSplit | extraModelDegrees | origName | code  | recommended |
| :----------------------------- | :------- | --------: | --------: | :------------- | :--------- | ----------------: | :------- | :---- | :---------- |
| x                              | TRUE     | 0.0013948 | 0.3464151 | large          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0089647 | 0.0169860 | large          | FALSE      |                 0 | x        | isBAD | TRUE        |
| x2                             | TRUE     | 0.0005061 | 0.5705974 | large          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1771355 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1068606 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0462447 | 0.0000001 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.5199156 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.1509119 | 0.0000000 | large          | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                              | TRUE     | 0.0033441 | 0.1453171 | liminal        | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0004722 | 0.5842251 | liminal        | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0000064 | 0.9490227 | liminal        | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.1761566 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.3129235 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.0055153 | 0.0614540 | liminal        | FALSE      |                 0 | xc       | lev   | FALSE       |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1690724 | 0.0000000 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0270612 | 0.0000343 | liminal        | FALSE      |                 0 | xc       | lev   | TRUE        |
| x                              | TRUE     | 0.0004019 | 0.6121350 | small          | FALSE      |                 0 | x        | clean | FALSE       |
| x\_isBAD                       | TRUE     | 0.0025264 | 0.2036305 | small          | FALSE      |                 0 | x        | isBAD | FALSE       |
| x2                             | TRUE     | 0.0003938 | 0.6157476 | small          | FALSE      |                 0 | x2       | clean | FALSE       |
| xc\_lev\_NA                    | TRUE     | 0.5306350 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0           | TRUE     | 0.1086074 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_0\_5        | TRUE     | 0.1296983 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_1           | TRUE     | 0.1728541 | 0.0000000 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     | 0.0215315 | 0.0002062 | small          | FALSE      |                 0 | xc       | lev   | TRUE        |
| large\_xc\_catB                | TRUE     | 0.8087125 | 0.0000000 | large          | TRUE       |                 4 | xc       | catB  | TRUE        |
| liminal\_xc\_catB              | TRUE     | 0.5798021 | 0.0000000 | liminal        | TRUE       |                 4 | xc       | catB  | TRUE        |
| small\_xc\_catB                | TRUE     | 0.7760688 | 0.0000000 | small          | TRUE       |                 4 | xc       | catB  | TRUE        |

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
