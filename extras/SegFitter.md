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
    ## 1 0.00   l_0 0.000000000    1.97       l_0.2 -0.1458958 FALSE    FALSE
    ## 2 0.01   l_0 0.009999833   11.71       l_5.1  0.5319236  TRUE     TRUE
    ## 3 0.02   l_0 0.019998667    3.05       l_7.4 -0.4709165 FALSE     TRUE
    ## 4 0.03   l_0 0.029995500    8.72       l_8.4 -0.3062934 FALSE     TRUE
    ## 5 0.04   l_0 0.039989334    0.80       l_0.3 -0.4006318 FALSE     TRUE
    ## 6 0.05   l_0 0.049979169    9.69      l_14.8  0.1850090 FALSE    FALSE

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
    ##  Length:1501        Min.   :-2.5733   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5789   FALSE:966       FALSE:287      
    ##  Mode  :character   Median : 0.1669   TRUE :535       TRUE :1214     
    ##                     Mean   : 0.1044                                  
    ##                     3rd Qu.: 0.7858                                  
    ##                     Max.   : 2.4120

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
    ## 1       x_PiecewiseV     TRUE 6.737987e-01 4.149158e-297       TRUE
    ## 2            x_clean     TRUE 1.382718e-03  1.954138e-01      FALSE
    ## 3 x_noise_PiecewiseV     TRUE 2.127247e-03  1.082282e-01       TRUE
    ## 4      x_noise_clean     TRUE 1.321510e-04  6.890538e-01      FALSE
    ## 5         x_cat_catP     TRUE 3.156838e-03  5.032614e-02       TRUE
    ## 6   x_cat_noise_catP     TRUE 8.842717e-05  7.434306e-01       TRUE
    ##   extraModelDegrees    origName       code
    ## 1              1214           x PiecewiseV
    ## 2                 0           x      clean
    ## 3              1214     x_noise PiecewiseV
    ## 4                 0     x_noise      clean
    ## 5               150       x_cat       catP
    ## 6               150 x_cat_noise       catP

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
    ## 1       x_PiecewiseV     TRUE 3.689978e-01 4.463434e-128       TRUE
    ## 2            x_clean     TRUE 1.423295e-03  1.348442e-01      FALSE
    ## 3 x_noise_PiecewiseV     TRUE 1.947111e-03  8.030668e-02       TRUE
    ## 4      x_noise_clean     TRUE 4.380962e-05  7.930630e-01      FALSE
    ## 5         x_cat_catP     TRUE 4.223036e-03  1.000562e-02       TRUE
    ## 6         x_cat_catB     TRUE 2.785072e-01  3.784975e-97       TRUE
    ## 7   x_cat_noise_catP     TRUE 1.038291e-04  6.863152e-01       TRUE
    ## 8   x_cat_noise_catB     TRUE 1.499055e-04  6.274870e-01       TRUE
    ##   extraModelDegrees    origName       code
    ## 1              1214           x PiecewiseV
    ## 2                 0           x      clean
    ## 3              1214     x_noise PiecewiseV
    ## 4                 0     x_noise      clean
    ## 5               150       x_cat       catP
    ## 6               150       x_cat       catB
    ## 7               150 x_cat_noise       catP
    ## 8               150 x_cat_noise       catB
