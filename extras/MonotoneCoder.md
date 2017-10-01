Isotone Coding in vtreat
================
John Mount, Win-Vector LLC
2017-09-30

``` r
suppressPackageStartupMessages(library("ggplot2"))
source("isotone.R")

# set up example data
set.seed(23525)
d <- data.frame(x = 10*runif(200))
d$yIdeal <- d$x^2
d$yObserved <- d$yIdeal + 10*rnorm(nrow(d))
d$isTrain <- runif(nrow(d))<=0.5

ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain)) +
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "dashed curve: ideal (pre-noise) values")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

``` r
customCoders = list('n.NonDecreasingV.num' = solveNonDecreasing )
treatments <- vtreat::designTreatmentsN( d[d$isTrain, , drop=FALSE], 
                                         'x', 'yObserved', 
                                        customCoders = customCoders,
                                        verbose = FALSE)
print(treatments$scoreFrame[, c('varName', 'rsq', 'sig', 'needsSplit'), drop=FALSE])
```

    ##            varName       rsq          sig needsSplit
    ## 1 x_NonDecreasingV 0.8889790 2.458159e-42       TRUE
    ## 2          x_clean 0.8704983 1.725973e-39      FALSE

``` r
dTreated <- vtreat::prepare(treatments, d)
d$soln <- dTreated$x_NonDecreasingV

dTrain <- d[d$isTrain, , drop=FALSE]

# good inference on train
sum((dTrain$yIdeal - dTrain$soln)^2)
```

    ## [1] 1695.844

``` r
sum((dTrain$yIdeal - dTrain$yObserved)^2)
```

    ## [1] 9736.865

``` r
dTest <- d[!d$isTrain, , drop=FALSE]

# good performance on test
sum((dTest$yIdeal - dTest$soln)^2)
```

    ## [1] 2749.191

``` r
sum((dTest$yIdeal - dTest$yObserved)^2)
```

    ## [1] 12024.43

``` r
ggplot(data=d, aes(x=x)) + 
  geom_line(aes(y=yIdeal), color='blue', linetype=2) + 
  geom_point(aes(y=yObserved, color=isTrain, shape=isTrain)) +
  geom_line(data= dTrain, aes(x=x, y=soln), color='darkgreen') +
  ylab('y') +
  ggtitle("ideal and observed responses as functions of x",
          subtitle = "solid path: monotone fit")
```

![](MonotoneCoder_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-2.png)

Can also easily adapt to classification and to categorical inputs.
