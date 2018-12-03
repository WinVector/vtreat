SegFitter
================

``` r
library("ggplot2")
```

``` r
customCoders = list('c.PiecewiseV.num' = vtreat::solve_piecewise,
                    'n.PiecewiseV.num' = vtreat::solve_piecewise)
codeRestriction = c("PiecewiseV", "clean", "isBAD", "catB", "catP")
```

``` r
d <- data.frame(x = seq(0, 15, by = 0.01))
d$x_cat <- paste0("l_", round(d$x, digits = 1))
d$y_ideal <- sin(d$x)
d$x_noise <- d$x[sample.int(nrow(d), nrow(d), replace = FALSE)]
d$x_cat_noise <- d$x_cat[sample.int(nrow(d), nrow(d), replace = FALSE)]
d$y <- d$y_ideal + 0.5*rnorm(nrow(d))
d$yc <- d$y>0.5
d$is_train <- runif(nrow(d))>=0.2

head(d)
```

    ##      x x_cat     y_ideal x_noise x_cat_noise          y    yc is_train
    ## 1 0.00   l_0 0.000000000    9.52       l_9.6  0.4790223 FALSE     TRUE
    ## 2 0.01   l_0 0.009999833    7.12       l_1.8 -0.1065460 FALSE     TRUE
    ## 3 0.02   l_0 0.019998667   14.00         l_3 -0.0168744 FALSE     TRUE
    ## 4 0.03   l_0 0.029995500    6.54       l_2.9  0.4516119 FALSE    FALSE
    ## 5 0.04   l_0 0.039989334   13.59       l_3.5 -1.2377398 FALSE     TRUE
    ## 6 0.05   l_0 0.049979169    4.15       l_8.2  0.5005280  TRUE     TRUE

``` r
summary(d)
```

    ##        x            x_cat              y_ideal           x_noise     
    ##  Min.   : 0.00   Length:1501        Min.   :-1.0000   Min.   : 0.00  
    ##  1st Qu.: 3.75   Class :character   1st Qu.:-0.5917   1st Qu.: 3.75  
    ##  Median : 7.50   Mode  :character   Median : 0.2412   Median : 7.50  
    ##  Mean   : 7.50                      Mean   : 0.1174   Mean   : 7.50  
    ##  3rd Qu.:11.25                      3rd Qu.: 0.8104   3rd Qu.:11.25  
    ##  Max.   :15.00                      Max.   : 1.0000   Max.   :15.00  
    ##  x_cat_noise              y               yc           is_train      
    ##  Length:1501        Min.   :-2.4096   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5704   FALSE:953       FALSE:313      
    ##  Mode  :character   Median : 0.1699   TRUE :548       TRUE :1188     
    ##                     Mean   : 0.1116                                  
    ##                     3rd Qu.: 0.7943                                  
    ##                     Max.   : 2.1773

``` r
ggplot(data=d) +
  geom_point(aes(x = x, y = y, color = yc), alpha=0.5) + 
  geom_line(aes(x = x, y = y_ideal), color = "lightblue") +
  geom_hline(yintercept = 0.5, color = "red")
```

![](SegFitter_files/figure-markdown_github/example-1.png)

``` r
cfn <- vtreat::mkCrossFrameNExperiment(
  d[d$is_train, , drop=FALSE], 
  c('x', 'x_noise', 'x_cat', 'x_cat_noise'), 'y',
  customCoders = customCoders,
  codeRestriction = codeRestriction,
  verbose = FALSE)

cfn$treatments
```

    ##              varName varMoves          rsq           sig needsSplit
    ## 1       x_PiecewiseV     TRUE 0.6884968644 1.171943e-302       TRUE
    ## 2            x_clean     TRUE 0.0013931344  1.985901e-01      FALSE
    ## 3 x_noise_PiecewiseV     TRUE 0.0006633046  3.751276e-01       TRUE
    ## 4      x_noise_clean     TRUE 0.0003866661  4.983346e-01      FALSE
    ## 5         x_cat_catP     TRUE 0.0026513228  7.605188e-02       TRUE
    ## 6   x_cat_noise_catP     TRUE 0.0001434787  6.800174e-01       TRUE
    ##   extraModelDegrees    origName       code
    ## 1              1188           x PiecewiseV
    ## 2                 0           x      clean
    ## 3              1188     x_noise PiecewiseV
    ## 4                 0     x_noise      clean
    ## 5               150       x_cat       catP
    ## 6               150 x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                      rsq           sig
    ## x           0.6884968644 2.343886e-302
    ## x_cat       0.0026513228  7.605188e-02
    ## x_cat_noise 0.0001434787  6.800174e-01
    ## x_noise     0.0006633046  7.502552e-01

``` r
prepared <- vtreat::prepare(cfn$treatments, d)
d$x_PiecewiseV <- prepared$x_PiecewiseV

ggplot(data=d) +
  # geom_point(aes(x = x, y = y)) + 
  geom_line(aes(x = x, y = y_ideal), color = "lightblue") + 
  geom_line(aes(x = x, y = x_PiecewiseV))
```

![](SegFitter_files/figure-markdown_github/solve_numeric-1.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y",
                     "x_PiecewiseV versus observed y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-2.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y_ideal",
                     "x_PiecewiseV versus ideal y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-3.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y",
                     "x_PiecewiseV versus observed y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-4.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_PiecewiseV", "y_ideal",
                     "x_PiecewiseV versus ideal y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-5.png)

``` r
cfc <- vtreat::mkCrossFrameCExperiment(
  d[d$is_train, , drop=FALSE], 
  c('x', 'x_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE,
  customCoders = customCoders,
  codeRestriction = codeRestriction,
  verbose = FALSE)

cfc$treatments
```

    ##              varName varMoves          rsq           sig needsSplit
    ## 1       x_PiecewiseV     TRUE 4.233905e-01 6.741049e-145       TRUE
    ## 2            x_clean     TRUE 8.243324e-08  9.909762e-01      FALSE
    ## 3 x_noise_PiecewiseV     TRUE 1.012852e-03  2.099631e-01       TRUE
    ## 4      x_noise_clean     TRUE 1.235119e-05  8.898922e-01      FALSE
    ## 5         x_cat_catP     TRUE 1.964521e-04  5.808626e-01       TRUE
    ## 6         x_cat_catB     TRUE 3.074449e-01 9.257645e-106       TRUE
    ## 7   x_cat_noise_catP     TRUE 2.809176e-05  8.346164e-01       TRUE
    ## 8   x_cat_noise_catB     TRUE 4.682810e-04  3.939715e-01       TRUE
    ##   extraModelDegrees    origName       code
    ## 1              1188           x PiecewiseV
    ## 2                 0           x      clean
    ## 3              1188     x_noise PiecewiseV
    ## 4                 0     x_noise      clean
    ## 5               150       x_cat       catP
    ## 6               150       x_cat       catB
    ## 7               150 x_cat_noise       catP
    ## 8               150 x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                     rsq           sig
    ## x           0.423390543 1.348210e-144
    ## x_cat       0.307444869 1.851529e-105
    ## x_cat_noise 0.000468281  7.879430e-01
    ## x_noise     0.001012852  4.199261e-01
