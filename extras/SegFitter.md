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
d <- data.frame(x_numeric = seq(0, 15, by = 0.01))
d$x_cat <- paste0("l_", round(d$x_numeric, digits = 1))
d$y_ideal <- sin(d$x_numeric)
d$x_numeric_noise <- d$x_numeric[sample.int(nrow(d), nrow(d), replace = FALSE)]
d$x_cat_noise <- d$x_cat[sample.int(nrow(d), nrow(d), replace = FALSE)]
d$y <- d$y_ideal + 0.5*rnorm(nrow(d))
d$yc <- d$y>0.5
d$is_train <- runif(nrow(d))>=0.2

head(d)
```

    ##   x_numeric x_cat     y_ideal x_numeric_noise x_cat_noise          y    yc
    ## 1      0.00   l_0 0.000000000            0.03      l_13.5 -1.0324863 FALSE
    ## 2      0.01   l_0 0.009999833           13.40       l_7.9 -0.5923803 FALSE
    ## 3      0.02   l_0 0.019998667            1.08         l_3 -0.1716321 FALSE
    ## 4      0.03   l_0 0.029995500            5.65       l_9.6 -0.1398762 FALSE
    ## 5      0.04   l_0 0.039989334            8.68       l_0.4  0.2956738 FALSE
    ## 6      0.05   l_0 0.049979169           11.44       l_7.6  0.8007378  TRUE
    ##   is_train
    ## 1     TRUE
    ## 2     TRUE
    ## 3     TRUE
    ## 4    FALSE
    ## 5     TRUE
    ## 6     TRUE

``` r
summary(d)
```

    ##    x_numeric        x_cat              y_ideal        x_numeric_noise
    ##  Min.   : 0.00   Length:1501        Min.   :-1.0000   Min.   : 0.00  
    ##  1st Qu.: 3.75   Class :character   1st Qu.:-0.5917   1st Qu.: 3.75  
    ##  Median : 7.50   Mode  :character   Median : 0.2412   Median : 7.50  
    ##  Mean   : 7.50                      Mean   : 0.1174   Mean   : 7.50  
    ##  3rd Qu.:11.25                      3rd Qu.: 0.8104   3rd Qu.:11.25  
    ##  Max.   :15.00                      Max.   : 1.0000   Max.   :15.00  
    ##  x_cat_noise              y               yc           is_train      
    ##  Length:1501        Min.   :-2.1693   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5649   FALSE:943       FALSE:312      
    ##  Mode  :character   Median : 0.1866   TRUE :558       TRUE :1189     
    ##                     Mean   : 0.1164                                  
    ##                     3rd Qu.: 0.7984                                  
    ##                     Max.   : 2.4871

``` r
ggplot(data=d) +
  geom_point(aes(x = x_numeric, y = y, color = yc), alpha=0.5) + 
  geom_line(aes(x = x_numeric, y = y_ideal), color = "lightblue") +
  geom_hline(yintercept = 0.5, color = "red")
```

![](SegFitter_files/figure-markdown_github/example-1.png)

``` r
cfn <- vtreat::mkCrossFrameNExperiment(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y',
  customCoders = customCoders,
  codeRestriction = codeRestriction,
  verbose = FALSE)

cfn$treatments
```

    ##                      varName varMoves          rsq           sig
    ## 1       x_numeric_PiecewiseV     TRUE 6.643856e-01 1.095506e-283
    ## 2            x_numeric_clean     TRUE 1.349497e-04  6.890382e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.021873e-03  2.707245e-01
    ## 4      x_numeric_noise_clean     TRUE 2.946122e-05  8.516873e-01
    ## 5                 x_cat_catP     TRUE 1.260035e-04  6.990015e-01
    ## 6           x_cat_noise_catP     TRUE 8.543700e-05  7.501854e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1189       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1189 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig             var
    ## x_cat           0.0001260035     1  6.990015e-01           x_cat
    ## x_cat_noise     0.0000854370     1  7.501854e-01     x_cat_noise
    ## x_numeric       0.6643856030     2 2.191011e-283       x_numeric
    ## x_numeric_noise 0.0010218729     2  5.414490e-01 x_numeric_noise

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig             var
    ## x_cat           2.565557e-05     1  8.614940e-01           x_cat
    ## x_cat_noise     1.255758e-03     1  2.220778e-01     x_cat_noise
    ## x_numeric       6.677550e-01     2 5.479597e-286       x_numeric
    ## x_numeric_noise 5.605690e-03     2  1.961133e-02 x_numeric_noise

``` r
prepared <- vtreat::prepare(cfn$treatments, d)
d$x_numeric_PiecewiseV <- prepared$x_numeric_PiecewiseV

ggplot(data=d) +
  # geom_point(aes(x = x_numeric, y = y)) + 
  geom_line(aes(x = x_numeric, y = y_ideal), color = "lightblue") + 
  geom_line(aes(x = x_numeric, y = x_numeric_PiecewiseV))
```

![](SegFitter_files/figure-markdown_github/solve_numeric-1.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y",
                     "x_numeric_PiecewiseV versus observed y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-2.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y_ideal",
                     "x_numeric_PiecewiseV versus ideal y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-3.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y",
                     "x_numeric_PiecewiseV versus observed y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-4.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y_ideal",
                     "x_numeric_PiecewiseV versus ideal y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-5.png)

``` r
cfc <- vtreat::mkCrossFrameCExperiment(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE,
  customCoders = customCoders,
  codeRestriction = codeRestriction,
  verbose = FALSE)

cfc$treatments
```

    ##                      varName varMoves          rsq           sig
    ## 1       x_numeric_PiecewiseV     TRUE 4.015507e-01 6.620016e-138
    ## 2            x_numeric_clean     TRUE 1.086919e-07  9.896237e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 5.883605e-04  3.386514e-01
    ## 4      x_numeric_noise_clean     TRUE 6.652784e-05  7.476434e-01
    ## 5                 x_cat_catP     TRUE 4.521774e-04  4.015701e-01
    ## 6                 x_cat_catB     TRUE 2.890474e-01 8.061679e-100
    ## 7           x_cat_noise_catP     TRUE 2.454077e-04  5.366033e-01
    ## 8           x_cat_noise_catB     TRUE 5.308126e-06  9.275851e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1189       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1189 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                          rsq count           sig             var
    ## x_cat           0.2890474403     2  1.612336e-99           x_cat
    ## x_cat_noise     0.0002454077     2  1.000000e+00     x_cat_noise
    ## x_numeric       0.4015507463     2 1.324003e-137       x_numeric
    ## x_numeric_noise 0.0005883605     2  6.773028e-01 x_numeric_noise

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                          rsq count           sig             var
    ## x_cat           0.2942699939     2 2.747278e-101           x_cat
    ## x_cat_noise     0.0021761925     2  1.314800e-01     x_cat_noise
    ## x_numeric       0.4064983758     2 2.801900e-139       x_numeric
    ## x_numeric_noise 0.0009857903     2  4.310386e-01 x_numeric_noise
