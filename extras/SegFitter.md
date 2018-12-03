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

    ##   x_numeric x_cat     y_ideal x_numeric_noise x_cat_noise           y
    ## 1      0.00   l_0 0.000000000            5.14      l_13.8 -0.02595038
    ## 2      0.01   l_0 0.009999833           12.90       l_2.7  0.16933527
    ## 3      0.02   l_0 0.019998667            6.90       l_2.4  0.45290602
    ## 4      0.03   l_0 0.029995500           12.67      l_11.2 -0.38670038
    ## 5      0.04   l_0 0.039989334           14.15       l_5.9  0.07917171
    ## 6      0.05   l_0 0.049979169            1.52      l_11.7  0.26330057
    ##      yc is_train
    ## 1 FALSE    FALSE
    ## 2 FALSE     TRUE
    ## 3 FALSE     TRUE
    ## 4 FALSE    FALSE
    ## 5 FALSE     TRUE
    ## 6 FALSE     TRUE

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
    ##  Length:1501        Min.   :-2.3376   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5661   FALSE:931       FALSE:290      
    ##  Mode  :character   Median : 0.1779   TRUE :570       TRUE :1211     
    ##                     Mean   : 0.1286                                  
    ##                     3rd Qu.: 0.7978                                  
    ##                     Max.   : 2.7752

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
    ## 1       x_numeric_PiecewiseV     TRUE 6.629834e-01 8.221180e-288
    ## 2            x_numeric_clean     TRUE 8.020411e-07  9.751634e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 3.188471e-04  5.347335e-01
    ## 4      x_numeric_noise_clean     TRUE 1.845599e-04  6.367172e-01
    ## 5                 x_cat_catP     TRUE 1.529154e-05  8.918679e-01
    ## 6           x_cat_noise_catP     TRUE 5.605302e-04  4.104176e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1211       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1211 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           1.529154e-05     1  8.918679e-01
    ## x_cat_noise     5.605302e-04     1  4.104176e-01
    ## x_numeric       6.629834e-01     2 1.644236e-287
    ## x_numeric_noise 3.188471e-04     2  1.000000e+00

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig
    ## x_cat           1.639576e-07     1  9.887691e-01
    ## x_cat_noise     2.761274e-04     1  5.634616e-01
    ## x_numeric       6.634883e-01     2 6.639476e-288
    ## x_numeric_noise 5.389716e-04     2  8.391375e-01

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
    ## 1       x_numeric_PiecewiseV     TRUE 4.166340e-01 1.489388e-147
    ## 2            x_numeric_clean     TRUE 1.119922e-04  6.714736e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.776973e-04  5.931708e-01
    ## 4      x_numeric_noise_clean     TRUE 7.590624e-05  7.269597e-01
    ## 5                 x_cat_catP     TRUE 1.377021e-03  1.369599e-01
    ## 6                 x_cat_catB     TRUE 2.803978e-01 5.967140e-100
    ## 7           x_cat_noise_catP     TRUE 1.441894e-04  6.303409e-01
    ## 8           x_cat_noise_catB     TRUE 2.003508e-03  7.282991e-02
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1211       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1211 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.2803978149     2  1.193428e-99
    ## x_cat_noise     0.0020035084     2  1.456598e-01
    ## x_numeric       0.4166340115     2 2.978776e-147
    ## x_numeric_noise 0.0001776973     2  1.000000e+00

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                          rsq count           sig
    ## x_cat           0.3008722730     2 8.324970e-107
    ## x_cat_noise     0.0002863495     2  9.953059e-01
    ## x_numeric       0.4134601521     2 3.825486e-146
    ## x_numeric_noise 0.0004665970     2  7.732984e-01
