Isotone Coding in vtreat
================
John Mount, Win-Vector LLC
2017-10-08

Isotone regression via the (also give [`scam`](https://CRAN.R-project.org/package=scam) and [`gbm` `var.monotone`](https://CRAN.R-project.org/package=gbm) a look, which should have the advantage of also being low complexity).

``` r
suppressPackageStartupMessages(library("ggplot2"))
source("isotone.R")

# set up example data
set.seed(23525)
d <- data.frame(x = 10*runif(200))
d$yIdeal <- d$x^2
d$yObserved <- d$yIdeal + 25*rnorm(nrow(d))
d$isTrain <- runif(nrow(d))<=0.5

ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain)) +
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "dashed curve: ideal (pre-noise) values")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/regression-1.png)

``` r
customCoders = list('n.NonDecreasingV.num' = solveNonDecreasing,
                    'n.NonIncreasingV.num' = solveNonIncreasing)
treatments <- vtreat::designTreatmentsN(d[d$isTrain, , drop=FALSE], 
                                        'x', 'yObserved', 
                                        customCoders = customCoders,
                                        verbose = FALSE)
print(treatments$scoreFrame[, c('varName', 'rsq', 'sig', 'needsSplit'), drop=FALSE])
```

    ##            varName       rsq          sig needsSplit
    ## 1 x_NonDecreasingV 0.5809673 9.905695e-18       TRUE
    ## 2          x_clean 0.6129088 3.320652e-19      FALSE

``` r
dTreated <- vtreat::prepare(treatments, d)
d$soln <- dTreated$x_NonDecreasingV

dTrain <- d[d$isTrain, , drop=FALSE]

# good inference on train
sum((dTrain$yIdeal - dTrain$soln)^2)
```

    ## [1] 7460.391

``` r
sum((dTrain$yIdeal - dTrain$yObserved)^2)
```

    ## [1] 60855.41

``` r
dTest <- d[!d$isTrain, , drop=FALSE]

# good performance on test
sum((dTest$yIdeal - dTest$soln)^2)
```

    ## [1] 14315.25

``` r
sum((dTest$yIdeal - dTest$yObserved)^2)
```

    ## [1] 75152.66

``` r
ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain)) +
  geom_line(aes(x=x, y=soln), color='darkgreen') +
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "solid path: isotone fit")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/regression-2.png)

The above is kind of exciting. You get one degree of freedom per data-row (a very large number), but a simple constraint system (that the produced predictions must follow the x-order constraints) is enough to produce reasonable fits. This reminiscent of the [maximum entropy formulation of logistic regression](http://www.win-vector.com/dfiles/LogisticRegressionMaxEnt.pdf), and is evidence one is working with a sort of dual-formulation of a smaller primal problem.

Can also easily adapt to classification and to categorical inputs.

``` r
suppressPackageStartupMessages(library("ggplot2"))
source("isotone.R")

# set up example data
set.seed(23525)
d <- data.frame(x = 10*runif(200))
d$yIdeal <- -d$x^2
d$yObserved <- d$yIdeal + 25*rnorm(nrow(d))
d$isTrain <- runif(nrow(d))<=0.5
threshold <- -50
d$yIdeal <- ifelse(d$yIdeal >= threshold, 1.0, 0.0)
d$yObserved <- ifelse(d$yObserved >= threshold, 1.0, 0.0)

ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain), alpha=0.5) + 
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "dashed curve: ideal (pre-noise) values")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/classification-1.png)

``` r
# could also build link-space versions
customCoders = list('c.NonDecreasingV.num' = solveNonDecreasing,
                    'c.NonIncreasingV.num' = solveNonIncreasing)
treatments <- vtreat::designTreatmentsC(d[d$isTrain, , drop=FALSE], 
                                        'x', 'yObserved', 1,
                                        customCoders = customCoders,
                                        verbose = FALSE)
# examining variables
print(treatments$scoreFrame[, c('varName', 'rsq', 'sig', 'needsSplit'), drop=FALSE])
```

    ##            varName       rsq          sig needsSplit
    ## 1 x_NonIncreasingV 0.3370146 2.856172e-10       TRUE
    ## 2          x_clean 0.3522417 1.138713e-10      FALSE

``` r
# copy fit over to original data frame
dTreated <- vtreat::prepare(treatments, d)
d$soln <- dTreated$x_NonIncreasingV

dTrain <- d[d$isTrain, , drop=FALSE]

# good inference on train
sum((dTrain$yIdeal - dTrain$soln)^2)
```

    ## [1] 4.390476

``` r
sum((dTrain$yIdeal - dTrain$yObserved)^2)
```

    ## [1] 16

``` r
dTest <- d[!d$isTrain, , drop=FALSE]

# good performance on test
sum((dTest$yIdeal - dTest$soln)^2)
```

    ## [1] 4.860317

``` r
sum((dTest$yIdeal - dTest$yObserved)^2)
```

    ## [1] 26

``` r
ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain)) +
  geom_line(aes(x=x, y=soln), color='darkgreen') +
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "solid path: isotone fit")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/classification-2.png)

One application we have used the monotone methodology with good success is: calibrating regressions and classifiers. That is we take a model that does well on the `AUC` measure (meaning it is good at ranking or reproducing order relations) and build the best model with the same order structure with respect to a more stringent measure (such as sum of squared errors, or deviance). Often this step is ignored or done by binning or some other method- but for systems that are not natively in probability units (such as margin based systems such as support vector machines) this isotone calibration or polish step can be an improvement (assuming one is careful about nested model bias issues).
