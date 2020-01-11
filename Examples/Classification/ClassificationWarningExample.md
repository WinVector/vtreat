Classification Warning Example
================

# [`vtreat`](https://github.com/WinVector/vtreat) Nested Model Bias Warning

For quite a while we have been teaching estimating variable re-encodings
on the exact same data they are later *naively* using to train a model
on leads to an undesirable nested model bias. The `vtreat` package (both
the [`R` version](https://github.com/WinVector/vtreat) and [`Python`
version](https://github.com/WinVector/pyvtreat)) both incorporate a
cross-frame method that allows one to use all the training data both to
build learn variable re-encodings and to correctly train a subsequent
model (for an example please see our recent [PyData LA
talk](http://www.win-vector.com/blog/2019/12/pydata-los-angeles-2019-talk-preparing-messy-real-world-data-for-supervised-machine-learning/)).

The next version of `vtreat` will warn the user if they have improperly
used the same data for both `vtreat` impact code inference and
downstream modeling. So in addition to us warning you not to do this,
the package now also checks and warns against this situation.

## Set up the Example

This example is copied from [some of our classification
documentation](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.md).

Load modules/packages.

``` r
# For this example we want vtreat version 1.5.1 or newer
# remotes::install_github("WinVector/vtreat")
library(vtreat)

packageVersion("vtreat")
```

    ## [1] '1.5.1'

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

training_data = make_data(500)

training_data %.>%
  head(.) %.>%
  knitr::kable(.)
```

|          x |           y | xc          |          x2 | yc    |
| ---------: | ----------: | :---------- | ----------: | :---- |
|  10.088396 | \-0.6397045 | level\_-0.5 | \-0.0360472 | FALSE |
| \-3.786014 |   0.6247358 | level\_0.5  |   0.8821910 | TRUE  |
|   5.653378 | \-0.5693094 | level\_-0.5 |   0.2569794 | FALSE |
|         NA | \-0.8233617 | NA          | \-1.7062826 | FALSE |
|         NA |   0.6630078 | level\_0.5  | \-0.1211011 | TRUE  |
|         NA |   0.7865697 | level\_1    |   1.4946036 | TRUE  |

## Demonstrate the Warning

Now that we have the data, we want to treat it prior to modeling: we
want training data where all the input variables are numeric and have no
missing values or `NA`s.

First create the data treatment transform design object, in this case a
treatment for a binomial classification problem.

We use the training data `training_data` to fit the transform and the
return a treated training set: completely numeric, with no missing
values.

``` r
transform_design = vtreat::mkCrossFrameCExperiment(
    dframe = training_data,                                    # data to learn transform from
    varlist = setdiff(colnames(training_data), c('y', 'yc')),  # columns to transform
    outcomename = 'yc',                            # outcome variable
    outcometarget = TRUE                           # outcome of interest
)
```

    ## [1] "vtreat 1.5.1 start initial treatment design Sat Jan 11 09:03:36 2020"
    ## [1] " start cross frame work Sat Jan 11 09:03:37 2020"
    ## [1] " vtreat::mkCrossFrameCExperiment done Sat Jan 11 09:03:37 2020"

``` r
transform <- transform_design$treatments
train_prepared <- transform_design$crossFrame
```

`d_prepared` is the correct way to use the same training data for
inferring the impact-coded variables.

We prepare new test or application data as follows.

``` r
test_data <- make_data(100)

test_prepared <- prepare(transform, test_data)
```

The issue is: for training data we should not call `prepare()`, but
instead use the cross-frame that is produces during transform design.

The point is we should not do the following:

``` r
train_prepared_wrong <- prepare(transform, training_data)
```

    ## Warning in prepare.treatmentplan(transform, training_data):
    ## possibly called prepare() on same data frame as designTreatments*()/
    ## mkCrossFrame*Experiment(), this can lead to over-fit. To avoid this, please use
    ## mkCrossFrame*Experiment$crossFrame.

Notice we now get a warning that we should not have done this, and in
doing so we may have a nested model bias data leak.

And that is the new nested model bias warning feature.
