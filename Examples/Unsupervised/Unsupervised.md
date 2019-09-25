Unsupervised.Rmd
================

# Using vtreat with Unsupervised Problems and Non-Y-aware data treatment

Nina Zumel and John Mount September 2019

Note this is a description of the [`R` version of
`vtreat`](https://github.com/WinVector/vtreat), the same example for the
[`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat)
can be found
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/Unsupervised/Unsupervised.md).

## Preliminaries

Load modules/packages.

``` r
library(vtreat)
suppressPackageStartupMessages(library(ggplot2))
library(WVPlots)
library(rqdatatable)
```

    ## Loading required package: rquery

Generate example data.

  - `y` is a noisy sinusoidal plus linear function of the variable `x`
  - Input `xc` is a categorical variable that represents a
    discretization of `y`, along with some `NaN`s
  - Input `x2` is a pure noise variable with no relationship to the
    output
  - Input `x3` is a constant variable

<!-- end list -->

``` r
make_data <- function(nrows) {
    d <- data.frame(x = 5*rnorm(nrows))
    d['y'] = sin(d[['x']]) + 0.01*d[['x']] + 0.1*rnorm(n = nrows)
    d[4:10, 'x'] = NA                  # introduce NAs
    d['xc'] = paste0('level_', 5*round(d$y/5, 1))
    d['x2'] = rnorm(n = nrows)
    d['x3'] = 1
    d[d['xc']=='level_-1', 'xc'] = NA  # introduce a NA level
    return(d)
}

d = make_data(500)

d %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x |           y | xc          |          x2 | x3 |
| ----------: | ----------: | :---------- | ----------: | -: |
| \-2.6019523 | \-0.6040076 | level\_-0.5 | \-0.7692852 |  1 |
|   0.3775947 |   0.2689662 | level\_0.5  |   0.2447184 |  1 |
|  12.3090071 | \-0.0795553 | level\_0    |   0.0974461 |  1 |
|          NA |   0.0929637 | level\_0    | \-0.6694792 |  1 |
|          NA |   0.4272080 | level\_0.5  | \-0.6826362 |  1 |
|          NA |   0.7376567 | level\_0.5  | \-0.8274254 |  1 |

### Some quick data exploration

Check how many levels `xc` has, and their disribution (including `NaN`)

``` r
unique(d['xc'])
```

    ##            xc
    ## 1  level_-0.5
    ## 2   level_0.5
    ## 3     level_0
    ## 10    level_1
    ## 12       <NA>

``` r
table(d$xc, useNA = 'always')
```

    ## 
    ## level_-0.5    level_0  level_0.5    level_1       <NA> 
    ##         94         95         98        104        109

## Build a transform appropriate for unsupervised (or non-y-aware) problems.

The `vtreat` package is primarily intended for data treatment prior to
supervised learning, as detailed in the
[Classification](https://github.com/WinVector/pyvtreat/blob/master/Examples/Classification/Classification.ipynb)
and
[Regression](https://github.com/WinVector/pyvtreat/blob/master/Examples/Regression/Regression.ipynb)
examples. In these situations, `vtreat` specifically uses the
relationship between the inputs and the outcomes in the training data to
create certain types of synthetic variables. We call these more complex
synthetic variables *y-aware variables*.

However, you may also want to use `vtreat` for basic data treatment for
unsupervised problems, when there is no outcome variable. Or, you may
not want to create any y-aware variables when preparing the data for
supervised modeling. For these applications, `vtreat` is a convenient
alternative to: `pandas.get_dummies()` or
`sklearn.preprocessing.OneHotEncoder()`.

In any case, we still want training data where all the input variables
are numeric and have no missing values or `NaN`s.

First create the data treatment transform object, in this case a
treatment for an unsupervised problem.

``` r
transform = vtreat::designTreatmentsZ(
    dframe = d,                              # data to learn transform from
    varlist = setdiff(colnames(d), c('y'))   # columns to transform
)
```

    ## [1] "vtreat 1.4.6 inspecting inputs Wed Sep 25 10:01:07 2019"
    ## [1] "designing treatments Wed Sep 25 10:01:07 2019"
    ## [1] " have initial level statistics Wed Sep 25 10:01:07 2019"
    ## [1] " scoring treatments Wed Sep 25 10:01:07 2019"
    ## [1] "have treatment plan Wed Sep 25 10:01:07 2019"

``` r
score_frame = transform$scoreFrame
```

Use the training data `d` to fit the transform and the return a treated
training set: completely numeric, with no missing values.

``` r
d_prepared = prepare(transform, d)
d_prepared$y = d$y
```

Now examine the score frame, which gives information about each new
variable, including its type and which original variable it is derived
from. Some of the columns of the score frame (`y_aware`, `PearsonR`,
`significance` and `recommended`) are not relevant to the unsupervised
case; those columns are used by the Regression and Classification
transforms.

``` r
knitr::kable(score_frame)
```

| varName                        | varMoves | rsq | sig | needsSplit | extraModelDegrees | origName | code  |
| :----------------------------- | :------- | --: | --: | :--------- | ----------------: | :------- | :---- |
| x                              | TRUE     |   0 |   1 | FALSE      |                 0 | x        | clean |
| x\_isBAD                       | TRUE     |   0 |   1 | FALSE      |                 0 | x        | isBAD |
| xc\_catP                       | TRUE     |   0 |   1 | TRUE       |                 4 | xc       | catP  |
| x2                             | TRUE     |   0 |   1 | FALSE      |                 0 | x2       | clean |
| xc\_lev\_NA                    | TRUE     |   0 |   1 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_minus\_0\_5 | TRUE     |   0 |   1 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_0           | TRUE     |   0 |   1 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_0\_5        | TRUE     |   0 |   1 | FALSE      |                 0 | xc       | lev   |
| xc\_lev\_x\_level\_1           | TRUE     |   0 |   1 | FALSE      |                 0 | xc       | lev   |

Notice that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level, including `NA` or
    missing (`xc_lev*`)
  - a variable indicating when `xc` was `NaN` in the original data
    (`xd_isBAD`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_catP`)

Any or all of these new variables are available for downstream modeling.

Also note that the variable `x3` did not show up in the score frame, as
it had no range (didn’t vary), so the unsupervised treatment dropped it.

Let’s look at the top of `d_prepared`, which includes all the new
variables, plus `y` (and excluding `x3`).

``` r
d_prepared %.>%
  head(.) %.>%
  knitr::kable(.)
```

|           x | x\_isBAD | xc\_catP |          x2 | xc\_lev\_NA | xc\_lev\_x\_level\_minus\_0\_5 | xc\_lev\_x\_level\_0 | xc\_lev\_x\_level\_0\_5 | xc\_lev\_x\_level\_1 |           y |
| ----------: | -------: | -------: | ----------: | ----------: | -----------------------------: | -------------------: | ----------------------: | -------------------: | ----------: |
| \-2.6019523 |        0 |    0.188 | \-0.7692852 |           0 |                              1 |                    0 |                       0 |                    0 | \-0.6040076 |
|   0.3775947 |        0 |    0.196 |   0.2447184 |           0 |                              0 |                    0 |                       1 |                    0 |   0.2689662 |
|  12.3090071 |        0 |    0.190 |   0.0974461 |           0 |                              0 |                    1 |                       0 |                    0 | \-0.0795553 |
|   0.1475432 |        1 |    0.190 | \-0.6694792 |           0 |                              0 |                    1 |                       0 |                    0 |   0.0929637 |
|   0.1475432 |        1 |    0.196 | \-0.6826362 |           0 |                              0 |                    0 |                       1 |                    0 |   0.4272080 |
|   0.1475432 |        1 |    0.196 | \-0.8274254 |           0 |                              0 |                    0 |                       1 |                    0 |   0.7376567 |

## Using the Prepared Data to Model

Of course, what we really want to do with the prepared training data is
to model.

### K-means clustering

Let’s start with an unsupervised analysis: clustering.

    # don't use y to cluster
    not_variables = ['y']
    model_vars = [v for v in d_prepared.columns if v not in set(not_variables)]
    
    import sklearn.cluster
    
    d_prepared['clusterID'] = sklearn.cluster.KMeans(n_clusters = 5).fit_predict(d_prepared[model_vars])
    d_prepared.clusterID
    
    # colorbrewer Dark2 palette
    mypalette = ['#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e']
    ax = seaborn.scatterplot(x = "x", y = "y", hue="clusterID", 
                        data = d_prepared, 
                        palette=mypalette, 
                        legend=False)
    ax.set_title("y as a function of x, points colored by (unsupervised) clusterID")
    plt.show()

### Supervised modeling with non-y-aware variables

Since in this case we have an outcome variable, `y`, we can try fitting
a linear regression model to `d_prepared`.

    import sklearn.linear_model
    import seaborn
    import sklearn.metrics
    import matplotlib.pyplot
    
    not_variables = ['y', 'prediction', 'clusterID']
    model_vars = [v for v in d_prepared.columns if v not in set(not_variables)]
    fitter = sklearn.linear_model.LinearRegression()
    fitter.fit(d_prepared[model_vars], d_prepared['y'])
    print(fitter.intercept_)
    {model_vars[i]: fitter.coef_[i] for i in range(len(model_vars))}
    
    # now predict
    d_prepared['prediction'] = fitter.predict(d_prepared[model_vars])
    
    # get R-squared
    r2 = sklearn.metrics.r2_score(y_true=d_prepared.y, y_pred=d_prepared.prediction)
    
    title = 'Prediction vs. outcome (training data); R-sqr = {:04.2f}'.format(r2)
    
    # compare the predictions to the outcome (on the training data)
    ax = seaborn.scatterplot(x='prediction', y='y', data=d_prepared)
    matplotlib.pyplot.plot(d_prepared.prediction, d_prepared.prediction, color="darkgray")
    ax.set_title(title)
    plt.show()

Now apply the model to new data.

    # create the new data
    dtest = make_data(450)
    
    # prepare the new data with vtreat
    dtest_prepared = transform.transform(dtest)
    
    # apply the model to the prepared data
    dtest_prepared['prediction'] = fitter.predict(dtest_prepared[model_vars])
    
    # get R-squared
    r2 = sklearn.metrics.r2_score(y_true=dtest_prepared.y, y_pred=dtest_prepared.prediction)
    
    title = 'Prediction vs. outcome (test data); R-sqr = {:04.2f}'.format(r2)
    
    # compare the predictions to the outcome (on the training data)
    ax = seaborn.scatterplot(x='prediction', y='y', data=dtest_prepared)
    matplotlib.pyplot.plot(dtest_prepared.prediction, dtest_prepared.prediction, color="darkgray")
    ax.set_title(title)
    plt.show()

## Parameters for `UnsupervisedTreatment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications. Notice that the parameter
object for unsupervised treatment defines a different set of parameters
than the parameter object for supervised treatments
(`vtreat.vtreat_parameters`).

    vtreat.unsupervised_parameters()

**coders**: The types of synthetic variables that `vtreat` will
(potentially) produce. See *Types of prepared variables* below.

**indicator\_min\_fraction**: By default, `UnsupervisedTreatment`
creates indicators for all possible levels (`indicator_min_fraction=0`).
If `indicator_min_fraction` \> 0, then indicator variables (type
`indicator_code`) are only produced for levels that are present at least
`indicator_min_fraction` of the time. A consequence of this is that
1/`indicator_min_fraction` is the maximum number of indicators that will
be produced for a given categorical variable. See the Example below.

**user\_transforms**: For passing in user-defined transforms for custom
data preparation. Won’t be needed in most situations, but see
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/UserCoders/UserCoders.ipynb)
for an example of applying a GAM transform to input variables.

**sparse\_indicators**: When True, use a (Pandas) sparse representation
for indicator variables. This representation is compatible with
`sklearn`; however, it may not be compatible with other modeling
packages. When False, use a dense representation.

### Example: Restrict the number of indicator variables

    # calculate the prevalence of each level by hand
    d['xc'].value_counts(dropna=False)/d.shape[0]
    
    transform_common = vtreat.UnsupervisedTreatment(
        cols_to_copy = ['y'],          # columns to "carry along" but not treat as input variables
        params = vtreat.unsupervised_parameters({
            'indicator_min_fraction': 0.2 # only make indicators for levels that show up more than 20% of the time
        })
    )  
    
    transform_common.fit_transform(d) # fit the transform
    transform_common.score_frame_     # examine the score frame

In this case, the unsupervised treatment only created levels for the two
most common levels, which are both present more than 20% of the time.

In unsupervised situations, this may only be desirable when there are an
unworkably large number of possible levels (for example, when using ZIP
code as a variable). It is more useful in conjuction with the y-aware
variables produced by `NumericOutputTreatment`,
`BinomialOutcomeTreatment`, or `MultinomialOutcomeTreatment`.

## Types of prepared variables

**clean\_copy**: Produced from numerical variables: a clean numerical
variable with no `NaNs` or missing values

**indicator\_code**: Produced from categorical variables, one for each
level: for each level of the variable, indicates if that level was “on”

**prevalence\_code**: Produced from categorical variables: indicates how
often each level of the variable was “on”

**missing\_indicator**: Produced for both numerical and categorical
variables: an indicator variable that marks when the original variable
was missing or `NaN`

### Example: Produce only a subset of variable types

In this example, suppose you only want to use indicators and continuous
variables in your model; in other words, you only want to use variables
of types (`clean_copy`, `missing_indicator`, and `indicator_code`), and
no `prevalence_code` variables.

    transform_thin = vtreat.UnsupervisedTreatment(
        cols_to_copy = ['y'],          # columns to "carry along" but not treat as input variables
        params = vtreat.unsupervised_parameters({
             'coders': {'clean_copy',
                        'missing_indicator',
                        'indicator_code',
                       }
        })
    )  
    
    transform_thin.fit_transform(d) # fit the transform
    transform_thin.score_frame_
