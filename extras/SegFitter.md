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
    ## 1      0.00   l_0 0.000000000            6.96       l_7.9  0.8957308  TRUE
    ## 2      0.01   l_0 0.009999833            4.96         l_6 -0.3282835 FALSE
    ## 3      0.02   l_0 0.019998667            2.97         l_8  0.4468062 FALSE
    ## 4      0.03   l_0 0.029995500           14.34      l_14.9 -0.1824496 FALSE
    ## 5      0.04   l_0 0.039989334            4.82       l_6.9 -0.2497848 FALSE
    ## 6      0.05   l_0 0.049979169           14.43       l_2.5 -0.2053733 FALSE
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
    ##  Length:1501        Min.   :-2.4863   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5646   FALSE:952       FALSE:307      
    ##  Mode  :character   Median : 0.1408   TRUE :549       TRUE :1194     
    ##                     Mean   : 0.1043                                  
    ##                     3rd Qu.: 0.7996                                  
    ##                     Max.   : 2.3299

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
    ## 1       x_numeric_PiecewiseV     TRUE 6.666449e-01 1.271238e-286
    ## 2            x_numeric_clean     TRUE 2.534213e-04  5.826377e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.264888e-03  2.194341e-01
    ## 4      x_numeric_noise_clean     TRUE 1.930142e-03  1.292087e-01
    ## 5                 x_cat_catP     TRUE 1.130080e-03  2.457590e-01
    ## 6           x_cat_noise_catP     TRUE 1.636275e-05  8.889528e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1194       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1194 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           1.130080e-03     1  2.457590e-01
    ## x_cat_noise     1.636275e-05     1  8.889528e-01
    ## x_numeric       6.666449e-01     2 2.542476e-286
    ## x_numeric_noise 1.930142e-03     2  2.584173e-01

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
    ## 1       x_numeric_PiecewiseV     TRUE 4.268786e-01 1.118488e-146
    ## 2            x_numeric_clean     TRUE 2.121749e-03  6.901979e-02
    ## 3 x_numeric_noise_PiecewiseV     TRUE 6.800443e-05  7.447835e-01
    ## 4      x_numeric_noise_clean     TRUE 2.135424e-03  6.813086e-02
    ## 5                 x_cat_catP     TRUE 2.656940e-04  5.199396e-01
    ## 6                 x_cat_catB     TRUE 3.090676e-01 9.587791e-107
    ## 7           x_cat_noise_catP     TRUE 4.758908e-04  3.891652e-01
    ## 8           x_cat_noise_catB     TRUE 8.246459e-05  7.199934e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1194       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1194 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.3090676451     2 1.917558e-106
    ## x_cat_noise     0.0004758908     2  7.783304e-01
    ## x_numeric       0.4268785627     2 2.236976e-146
    ## x_numeric_noise 0.0021354239     2  1.362617e-01
