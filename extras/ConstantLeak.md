Constant Coding Leak
================
John Mount, Win-Vector LLC
2019-07-06

We will show how in some situations using “more data in
cross-validation” can be harmful.

Our example: an outcome (`y`) that is independent of a low-complexity
categorical variable (`x`). We will combine this with a variable that is
a noisy constant and leave-one-out cross-validation (which is a
deterministic procedure) to get a bad result (failing to notice
over-fit).

``` r
library("vtreat")

set.seed(352355)

nrow <- 100
d <- data.frame(x = sample(c('a', 'b'), 
                           nrow, replace = TRUE),
                y = rnorm(nrow),
                stringsAsFactors = FALSE)
```

Introduce a deliberately bad custom coder.

This coder is bad in several ways:

  - It is essentially returning a constant independent of the variable
    it claims to be encoding.
  - It’s predictions are not consistent, it makes different predictions
    for the same value of the independent variable it claims to encode.

<!-- end list -->

``` r
# @param v character scalar: variable name
# @param vcol character vector, independent or input variable values
# @param y numeric, dependent or outcome variable to predict
# @param weights row/example weights
# @return scored training data column
badCoderN <- function(v, vcol, 
                      y, 
                      weights) {
  # Notice we are returning a constant, independent of vcol!
  # this should not look informative.
  meanY <- sum(y*weights)/sum(weights)
  1.0e-3*runif(length(y)) + meanY # noise to sneak past constant detector
}

customCoders <- list('n.badCoderN' = badCoderN)
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

| varName      | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code      |
| :----------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :-------- |
| x\_badCoderN | TRUE     | 0.0016387 | 0.6892408 | TRUE       |                 1 | x        | badCoderN |
| x\_lev\_x\_a | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |
| x\_lev\_x\_b | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |

``` r
treatedD <- prepare(treatplanN, d)
summary(lm(y ~ x_badCoderN, data= treatedD))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x_badCoderN, data = treatedD)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.00554 -0.74592 -0.01978  0.67265  2.98501 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)    391.5      939.1   0.417    0.678
    ## x_badCoderN  -1999.8     4799.8  -0.417    0.678
    ## 
    ## Residual standard error: 1.12 on 98 degrees of freedom
    ## Multiple R-squared:  0.001768,   Adjusted R-squared:  -0.008418 
    ## F-statistic: 0.1736 on 1 and 98 DF,  p-value: 0.6779

`vtreat` correctly works on this example in the cross-frame pattern, and
rejects the bad custom variable.

``` r
cfe <- mkCrossFrameNExperiment(d, 
                               varlist = c('x'),
                               outcomename = 'y',
                               customCoders = customCoders)
```

    ## [1] "vtreat 1.4.3 start initial treatment design Sat Jul  6 16:28:28 2019"
    ## [1] " start cross frame work Sat Jul  6 16:28:28 2019"
    ## [1] " vtreat::mkCrossFrameNExperiment done Sat Jul  6 16:28:28 2019"

``` r
knitr::kable(cfe$treatments$scoreFrame)
```

| varName      | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code      |
| :----------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :-------- |
| x\_badCoderN | TRUE     | 0.0022312 | 0.6407308 | TRUE       |                 1 | x        | badCoderN |
| x\_lev\_x\_a | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |
| x\_lev\_x\_b | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |

``` r
summary(lm(y ~ x_badCoderN, data= cfe$crossFrame))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x_badCoderN, data = cfe$crossFrame)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.04022 -0.74222  0.05169  0.63005  2.95187 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   0.5794     1.1269   0.514    0.608
    ## x_badCoderN  -1.9688     5.7449  -0.343    0.733
    ## 
    ## Residual standard error: 1.121 on 98 degrees of freedom
    ## Multiple R-squared:  0.001197,   Adjusted R-squared:  -0.008995 
    ## F-statistic: 0.1174 on 1 and 98 DF,  p-value: 0.7326

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

    ## [1] "vtreat 1.4.3 start initial treatment design Sat Jul  6 16:28:28 2019"
    ## [1] " start cross frame work Sat Jul  6 16:28:29 2019"
    ## [1] " vtreat::mkCrossFrameNExperiment done Sat Jul  6 16:28:30 2019"

``` r
knitr::kable(cfeBad$treatments$scoreFrame)
```

| varName      | varMoves |       rsq |       sig | needsSplit | extraModelDegrees | origName | code      |
| :----------- | :------- | --------: | --------: | :--------- | ----------------: | :------- | :-------- |
| x\_badCoderN | TRUE     | 0.9999862 | 0.0000000 | TRUE       |                 1 | x        | badCoderN |
| x\_lev\_x\_a | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |
| x\_lev\_x\_b | TRUE     | 0.0017682 | 0.6778556 | FALSE      |                 0 | x        | lev       |

``` r
summary(lm(y ~ x_badCoderN, data= cfeBad$crossFrame))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x_badCoderN, data = cfeBad$crossFrame)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0135012 -0.0028152 -0.0000517  0.0022441  0.0092467 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.557657   0.007569    2584   <2e-16 ***
    ## x_badCoderN -98.994795   0.038634   -2562   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.004332 on 98 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:      1 
    ## F-statistic: 6.566e+06 on 1 and 98 DF,  p-value: < 2.2e-16

``` r
treatedDbad <- prepare(cfeBad$treatments, d)
summary(lm(y ~ x_badCoderN, data= treatedDbad))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x_badCoderN, data = treatedDbad)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.00554 -0.74592 -0.01978  0.67265  2.98501 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -322040     772954  -0.417    0.678
    ## x_badCoderN  1646604    3952139   0.417    0.678
    ## 
    ## Residual standard error: 1.12 on 98 degrees of freedom
    ## Multiple R-squared:  0.001768,   Adjusted R-squared:  -0.008418 
    ## F-statistic: 0.1736 on 1 and 98 DF,  p-value: 0.6779

Notice the bad coder was (falsely) reported as usable and (falsely)
appears useful on the cross-frame (but not on a simple prepared frame).
Also notice the normal coders such as impact (which was fully rejected
by `vtreat`) and levels codes were properly rejected.

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

This failing is because the common cross validation procedures are not
[fully nested
simulation](http://www.win-vector.com/blog/2017/01/a-theory-of-nested-cross-simulation/)
in the sense that rows were not excluded from out final calculation (the
estimation of significance, or final linear model). I did not correctly
make the distinction when laying out the theory of notation in the
previous article, but the idea is to maintain full exchangeability every
step of the simulation must systematically exclude sets of rows:
especially the last step if it is performing join over all rows
calculations.

*Fully* nested cross-simulation (where even the last step is under the
cross-control and enumerating excluded sets of training rows) is likely
too cumbersome (requiring more code coordination) and expensive (upping
the size of the sets of rows we have to exclude) to force on
implementers who are also unlikely to see any benefit in non-degenerate
cases. The partially nested cross-simulation used in `vtreat` is likely
a good practical compromise (though we may explore full-nesting for the
score frame estimates, as that is a step completely under `vtreat`
control).

The current `vtreat` procedures are very strong and fully up to the case
of assisting in construction of best possible machine learning models.
However in certain degenerate cases (near-constant encoding combined
completely deterministic cross-validation; neither of which is a default
behavior of `vtreat`) the cross validation system itself can introduce
an information leak that promotes over-fit.
