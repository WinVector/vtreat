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
    ## 1      0.00   l_0 0.000000000           11.75       l_2.4 -0.1820099 FALSE
    ## 2      0.01   l_0 0.009999833           10.70       l_9.2  0.2692465 FALSE
    ## 3      0.02   l_0 0.019998667           11.14       l_9.4  0.3492104 FALSE
    ## 4      0.03   l_0 0.029995500           12.24       l_3.8  0.3388107 FALSE
    ## 5      0.04   l_0 0.039989334           10.24       l_7.8 -0.3418937 FALSE
    ## 6      0.05   l_0 0.049979169            4.43      l_13.2 -0.6202417 FALSE
    ##   is_train
    ## 1     TRUE
    ## 2    FALSE
    ## 3     TRUE
    ## 4     TRUE
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
    ##  Length:1501        Min.   :-2.6281   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5517   FALSE:951       FALSE:293      
    ##  Mode  :character   Median : 0.1604   TRUE :550       TRUE :1208     
    ##                     Mean   : 0.1223                                  
    ##                     3rd Qu.: 0.8018                                  
    ##                     Max.   : 2.5527

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
    ## 1       x_numeric_PiecewiseV     TRUE 0.6636049621 1.381455e-287
    ## 2            x_numeric_clean     TRUE 0.0008170348  3.208845e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 0.0012957325  2.112238e-01
    ## 4      x_numeric_noise_clean     TRUE 0.0011979595  2.293310e-01
    ## 5                 x_cat_catP     TRUE 0.0007464191  3.427430e-01
    ## 6           x_cat_noise_catP     TRUE 0.0010343720  2.640150e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1208       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1208 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.0007464191     1  3.427430e-01
    ## x_cat_noise     0.0010343720     1  2.640150e-01
    ## x_numeric       0.6636049621     2 2.762911e-287
    ## x_numeric_noise 0.0012957325     2  4.224476e-01

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig
    ## x_cat           1.389883e-03     1  1.953676e-01
    ## x_cat_noise     6.278879e-06     1  9.306705e-01
    ## x_numeric       6.609397e-01     2 3.227821e-285
    ## x_numeric_noise 1.197960e-03     2  4.586619e-01

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
    ## 1       x_numeric_PiecewiseV     TRUE 4.270397e-01 2.854978e-149
    ## 2            x_numeric_clean     TRUE 7.148477e-04  2.870431e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.439352e-03  1.308671e-01
    ## 4      x_numeric_noise_clean     TRUE 1.800195e-03  9.112829e-02
    ## 5                 x_cat_catP     TRUE 1.305534e-06  9.637109e-01
    ## 6                 x_cat_catB     TRUE 3.238224e-01 1.130743e-113
    ## 7           x_cat_noise_catP     TRUE 1.166164e-04  6.671931e-01
    ## 8           x_cat_noise_catB     TRUE 1.045809e-03  1.978466e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1208       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1208 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                         rsq count           sig
    ## x_cat           0.323822356     2 2.261486e-113
    ## x_cat_noise     0.001045809     2  3.956932e-01
    ## x_numeric       0.427039722     2 5.709956e-149
    ## x_numeric_noise 0.001800195     2  1.822566e-01

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                         rsq count           sig
    ## x_cat           0.340567888     2 3.784221e-119
    ## x_cat_noise     0.001149465     2  3.540193e-01
    ## x_numeric       0.405972266     2 1.049881e-141
    ## x_numeric_noise 0.001800195     2  1.822566e-01
