Constant Coding Leak
================
John Mount, Win-Vector LLC
2020-01-28

We will show how in some situations using “more data in
cross-validation” can be harmful.

Our example: an outcome (`y`) that is independent of a high-complexity
categorical variable (`x`). We will combine this with a variable that is
a noisy constant and leave-one-out cross-validation (which is a
deterministic procedure) to get a bad result (failing to notice
over-fit).

``` r
library("vtreat")

set.seed(352355)

nrow <- 100
d <- data.frame(x = sample(paste0('lev_', seq_len(nrow)), size = nrow, replace = FALSE),
                y = rnorm(nrow),
                stringsAsFactors = FALSE)
```

Introduce a deliberately bad custom coder.

This coder is bad in several ways:

  - It is essentially returning a constant independent of the variable
    it claims to be encoding.
  - It’s predictions are not consistent, it makes different predictions
    for the same value of the independent variable it claims to encode.
  - It is trying to predict the dependent variable `y`, instead of a
    conditional difference of the dependent variable from the
    cross-validated mean of the dependent variable.

<!-- end list -->

``` r
# @param v character scalar: variable name
# @param vcol character vector, independent or input variable values
# @param y numeric, dependent or outcome variable to predict
# @param weights row/example weights
# @return scored training data column
bad_coder_noisy_constant <- function(
  v, vcol, 
  y, 
  weights) {
  # Notice we are returning a constant, independent of vcol!
  # this should not look informative.
  meanY <- sum(y*weights)/sum(weights)
  meanY + 1.0e-3*runif(length(y)) # noise to sneak past constant detector
}

# @param v character scalar: variable name
# @param vcol character vector, independent or input variable values
# @param y numeric, dependent or outcome variable to predict
# @param weights row/example weights
# @return scored training data column
bad_coder_noisy_conditional <- function(
  v, vcol, 
  y, 
  weights) {
  # Note: ignores weights
  agg <- aggregate(y ~ x, data = data.frame(x = vcol, y = y), FUN = mean)
  map <- agg$y
  names(map) <- agg$x
  map[vcol] + 1.0e-3*runif(length(y)) # noise to sneak past constant detector
}

# @param v character scalar: variable name
# @param vcol character vector, independent or input variable values
# @param y numeric, dependent or outcome variable to predict
# @param weights row/example weights
# @return scored training data column
bad_coder_noisy_delta <- function(
  v, vcol, 
  y, 
  weights) {
  # Note: ignores weights
  agg <- aggregate(y ~ x, data = data.frame(x = vcol, y = y), FUN = mean)
  map <- agg$y - mean(y)
  names(map) <- agg$x
  map[vcol] + 1.0e-3*runif(length(y)) # noise to sneak past constant detector
}

customCoders <- list(
  'n.bad_coder_noisy_constant' = bad_coder_noisy_constant,
  'n.bad_coder_noisy_conditional' = bad_coder_noisy_conditional,
  'n.bad_coder_noisy_delta' = bad_coder_noisy_delta)
```

`vtreat` correctly works on this example in the design/prepare pattern,
and rejects the bad custom variable.

``` r
treatplanN <- designTreatmentsN(d, 
                                varlist = c('x'),
                                outcomename = 'y',
                                customCoders = customCoders, 
                                verbose = FALSE)
knitr::kable(treatplanN$scoreFrame)
```

| varName                           | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code                           |
| :-------------------------------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :----------------------------- |
| x\_bad\_coder\_noisy\_constant    | TRUE     | 0.0020488 | 0.6547491 | TRUE       |                99 | x        | bad\_coder\_noisy\_constant    |
| x\_bad\_coder\_noisy\_conditional | TRUE     | 0.0020489 | 0.6547430 | TRUE       |                99 | x        | bad\_coder\_noisy\_conditional |
| x\_bad\_coder\_noisy\_delta       | TRUE     | 0.0002930 | 0.8657779 | TRUE       |                99 | x        | bad\_coder\_noisy\_delta       |
| x\_catN                           | TRUE     | 0.0000000 | 1.0000000 | TRUE       |                99 | x        | catN                           |

Notice `vtreat` correctly identified none of the variables as being
significant.

``` r
treatedD <- prepare(treatplanN, d)
```

    ## Warning in prepare.treatmentplan(treatplanN, d): possibly called prepare() on
    ## same data frame as designTreatments*()/mkCrossFrame*Experiment(), this can lead
    ## to over-fit. To avoid this, please use mkCrossFrame*Experiment$crossFrame.

``` r
summary(lm(y ~ x_bad_coder_noisy_constant, data= treatedD))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x_bad_coder_noisy_constant, data = treatedD)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.47427 -0.66641 -0.07081  0.58681  2.97163 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                   -9.50      86.60  -0.110    0.913
    ## x_bad_coder_noisy_constant    45.38     404.63   0.112    0.911
    ## 
    ## Residual standard error: 1.066 on 98 degrees of freedom
    ## Multiple R-squared:  0.0001283,  Adjusted R-squared:  -0.01007 
    ## F-statistic: 0.01258 on 1 and 98 DF,  p-value: 0.9109

However, specifying `oneWayHoldout` as the cross-validation technique
introduces sampling variation that is correlated with the outcome. This
causes the value in the synthetic cross-frame (used both for calculating
variable significances and returned to the use for further training) to
have a spurious correlation with the outcome. The completely
deterministic structure of leave-one-out holdout itself represents an
information leak that poisons results.

``` r
cfeBad <- mkCrossFrameNExperiment(d, 
                                  varlist = c('x'),
                                  outcomename = 'y',
                                  customCoders = customCoders,
                                  splitFunction = oneWayHoldout)
```

    ## [1] "vtreat 1.5.2 start initial treatment design Tue Jan 28 05:21:55 2020"
    ## [1] " start cross frame work Tue Jan 28 05:21:57 2020"
    ## [1] " vtreat::mkCrossFrameNExperiment done Tue Jan 28 05:22:00 2020"

``` r
knitr::kable(cfeBad$treatments$scoreFrame)
```

| varName                           | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code                           |
| :-------------------------------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :----------------------------- |
| x\_bad\_coder\_noisy\_constant    | TRUE     | 0.9999926 | 0.0000000 | TRUE       |                99 | x        | bad\_coder\_noisy\_constant    |
| x\_bad\_coder\_noisy\_conditional | TRUE     | 0.9999923 | 0.0000000 | TRUE       |                99 | x        | bad\_coder\_noisy\_conditional |
| x\_bad\_coder\_noisy\_delta       | TRUE     | 0.0049319 | 0.4874913 | TRUE       |                99 | x        | bad\_coder\_noisy\_delta       |

Notice the bad coder was (falsely) and conditional coders are both
reported as usable and (falsely) appear useful on the cross-frame. The
delta-code avoid this issue. Also notice the normal coders such as
impact (which was fully rejected by `vtreat`) and levels codes were
properly rejected.

What happened is:

  - The deterministic structure of leave-one-out cross validation
    introduces an information leak that copies a transform of the value
    of `y` into the bad coder. Essentially the leave-one-out cross
    validation is consuming a number of degrees of freedom equal to the
    number of different data sets its presents (one per data row).
  - The bad coder had a design flow of returning a conditional mean,
    instead of a conditional difference from the overall mean. The
    actual `vtreat` impact/effects coders are careful to return the
    difference from cross-validation segment mean (which would be zero
    for all constant values).
  - The bad coder being a near constant means this leak is nearly the
    entirety of the bad coder signal.
  - On any data set other than the one-way-holdout cross-validation
    frame the bad coder is in fact a noisy constant (and not useful).
    The the bad coder is pure over-fit and any model that uses it is at
    risk of over-fit.

In the failing example the value returned data-row `k` is essentially
the mean of all rows except the `k`-th row due to the leave-one-out
holdout. Call this estimate `e(k)` (the estimate assigned to the `k`-th
row).

The coding-estimate for the `k`-th row is essentially `(1/(n-1)) sum(i
= 1, ...,n; i not k) y(i)` (where `n` is the number of training data
rows, and `y(i)` is the `i`-th dependent value). That is the coder
builds its coding of the `k`-th row by averaging all of the training
dependent values it is allowed to see under the leave-1-out cross
validation procedure. In an isolated sense its calculation of the `k`-th
row is independent of `y(k)` as that value was not shown to the
procedure at that time.

However by algebra we have this estimate `e(k)` is also equal to
`(n/(n-1)) mean(y) - y(k)/(n-1)`. So a step in the procedure that also
knows `mean(y)` (such as say the `lm()` linear regression models shown
above, and the variable significance procedures used to build the
`scoreFrame`s) we know that `y(k) = sum(y) - (n-1) e(k)`. Or in vector
form (`y` and `e` being the vectors, all other terms scalars): `y =
sum(y) - (n-1) e`. Jointly for all rows the dependent variable `y` is a
simple linear function of the estimates `e`, even though each estimate
`e(k)` with no knowledge of the dependent value `y(k)` in the same row.

Or: to an observer that knows `n` and `mean(y)` (and hence `sum(y)`)
`e(k)` completely determines `y(k)` even though it was constructed
without knowledge of `y(k)`.

This failing is because:

<ul>

<li>

The estimator tried to estimate `E[y | x]` (which is records sampling
noise from the cross-validation procedure) instead of `E[y - E[y] | x]`
(which does not record the sampling noise in the cross-validation
procedure). `vtreat` uses the encoding of differences technique to avoid
such difficulties.

</li>

<li>

The common cross validation procedures are not [fully nested
simulation](http://www.win-vector.com/blog/2017/01/a-theory-of-nested-cross-simulation/)
in the sense that rows were not excluded from out final calculation (the
estimation of significance, or final linear model). I did not correctly
make the distinction when laying out the theory of notation in the
previous article, but the idea is to maintain full exchangeability every
step of the simulation must systematically exclude sets of rows:
especially the last step if it is performing join over all rows
calculations.

</li>

</ul>

*Fully* nested cross-simulation (where even the last step is under the
cross-control and enumerating excluded sets of training rows) is likely
too cumbersome (requiring more code coordination) and expensive (upping
the size of the sets of rows we have to exclude) to force on
implementers who are also unlikely to see any benefit in non-degenerate
cases. The partially nested cross-simulation used in `vtreat` is likely
a good practical compromise (though we may explore full-nesting for the
score frame estimates, as that is a step completely under `vtreat`
control).

The current `vtreat` procedures are very strong and fully “up to the
job” of assisting in construction of best possible machine learning
models. However in certain degenerate cases (near-constant encoding
combined completely deterministic cross-validation; neither of which is
a default behavior of `vtreat`) the cross validation system itself can
introduce an information leak that promotes over-fit for some custom
coders. `vtreat`’s built-in coders are estimates of conditional changes
from apparent mean (not estimates of conditional values), so tend to
avoid the above issues.
