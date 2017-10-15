Isotone Coding in vtreat
================
John Mount, Win-Vector LLC
2017-10-15

Monotone (or isotone) regression via the [`isotone` package](https://CRAN.R-project.org/package=isotone) (also give [`scam`](https://CRAN.R-project.org/package=scam) and [`gbm` `var.monotone`](https://CRAN.R-project.org/package=gbm) a look, which should have the advantage of also being low complexity).

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

The above formulation is kind of exciting. You get one degree of freedom per data-row (a very large number), but a simple constraint system (that the produced predictions must follow the x-order constraints) is enough to produce reasonable fits. This reminiscent of the [maximum entropy formulation of logistic regression](http://www.win-vector.com/dfiles/LogisticRegressionMaxEnt.pdf), and is evidence one is working with a sort of dual-formulation of a smaller primal problem.

Some notes on smoother implementations can be found [here](https://github.com/WinVector/vtreat/blob/master/extras/Monotone2.md).

We can also easily adapt to classification and to categorical inputs.

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
d$yIdeal <- d$yIdeal >= threshold
d$yObserved <- d$yObserved >= threshold


# could also build link-space versions
customCoders = list('c.NonDecreasingV.num' = solveNonDecreasing,
                    'c.NonIncreasingV.num' = solveNonIncreasing)
treatments <- vtreat::designTreatmentsC(d[d$isTrain, , drop=FALSE], 
                                        'x', 'yObserved', TRUE,
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
d$prediction <- d$soln>0.5

dTrain <- d[d$isTrain, , drop=FALSE]

# good inference on train
sigr::wrapChiSqTest(dTrain, 'soln', 'yIdeal')
```

    ## [1] "Chi-Square Test summary: pseudo-R2=0.69 (X2(1,N=87)=77, p<1e-05)."

``` r
table(dTrain$prediction, dTrain$yIdeal)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE    28    0
    ##   TRUE      2   57

``` r
sigr::wrapFisherTest(dTrain, 'prediction', 'yIdeal')
```

    ## [1] "Fisher's Exact Test for Count Data: (odds.ratio=Inf, p<1e-05)."

``` r
table(dTrain$yObserved, dTrain$yIdeal)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE    25   11
    ##   TRUE      5   46

``` r
sigr::wrapFisherTest(dTrain, 'yObserved', 'yIdeal')
```

    ## [1] "Fisher's Exact Test for Count Data: (odds.ratio=20, p<1e-05)."

``` r
dTest <- d[!d$isTrain, , drop=FALSE]

# good performance on test
sigr::wrapChiSqTest(dTest, 'soln', 'yIdeal')
```

    ## [1] "Chi-Square Test summary: pseudo-R2=0.72 (X2(1,N=113)=1.1e+02, p<1e-05)."

``` r
table(dTest$prediction, dTest$yIdeal)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE    38    0
    ##   TRUE      1   74

``` r
sigr::wrapFisherTest(dTest, 'prediction', 'yIdeal')
```

    ## [1] "Fisher's Exact Test for Count Data: (odds.ratio=Inf, p<1e-05)."

``` r
table(dTest$yObserved, dTest$yIdeal)
```

    ##        
    ##         FALSE TRUE
    ##   FALSE    24   11
    ##   TRUE     15   63

``` r
sigr::wrapFisherTest(dTest, 'yObserved', 'yIdeal')
```

    ## [1] "Fisher's Exact Test for Count Data: (odds.ratio=8.9, p<1e-05)."

One application we have used the monotone methodology with good success is: calibrating regressions and classifiers. That is: we take a model that does well on the `AUC` measure (meaning it is good at ranking or reproducing order relations) and build the best model with the same order structure with respect to a more stringent measure (such as sum of squared errors, or deviance). Often this step is ignored or done by binning or some other method- but for systems that are not natively in probability units (such as margin based systems such as support vector machines) this isotone calibration or polish step can be an improvement (assuming one is careful about nested model bias issues).
