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
    ## 1      0.00   l_0 0.000000000           13.20      l_12.7  0.3875937 FALSE
    ## 2      0.01   l_0 0.009999833            2.33      l_14.7  0.2501102 FALSE
    ## 3      0.02   l_0 0.019998667           10.51         l_9  0.4087583 FALSE
    ## 4      0.03   l_0 0.029995500            5.34       l_1.2  0.2435319 FALSE
    ## 5      0.04   l_0 0.039989334            3.44         l_8  0.2906728 FALSE
    ## 6      0.05   l_0 0.049979169            9.13       l_9.4 -0.5400803 FALSE
    ##   is_train
    ## 1     TRUE
    ## 2     TRUE
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
    ##  Length:1501        Min.   :-2.3867   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5669   FALSE:939       FALSE:279      
    ##  Mode  :character   Median : 0.2039   TRUE :562       TRUE :1222     
    ##                     Mean   : 0.1102                                  
    ##                     3rd Qu.: 0.7752                                  
    ##                     Max.   : 2.5791

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
    ## 1       x_numeric_PiecewiseV     TRUE 6.434994e-01 1.625383e-275
    ## 2            x_numeric_clean     TRUE 2.103687e-05  8.727464e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.220090e-03  2.224015e-01
    ## 4      x_numeric_noise_clean     TRUE 1.273140e-03  2.126071e-01
    ## 5                 x_cat_catP     TRUE 5.126008e-04  4.290930e-01
    ## 6           x_cat_noise_catP     TRUE 1.108567e-03  2.448159e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1222       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1222 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.0005126008     1  4.290930e-01
    ## x_cat_noise     0.0011085667     1  2.448159e-01
    ## x_numeric       0.6434994085     2 3.250767e-275
    ## x_numeric_noise 0.0012731402     2  4.252143e-01

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig
    ## x_cat           0.0000490687     1  8.067478e-01
    ## x_cat_noise     0.0022296978     1  9.896446e-02
    ## x_numeric       0.6446512796     2 4.510702e-276
    ## x_numeric_noise 0.0012731402     2  4.252143e-01

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
    ## 1       x_numeric_PiecewiseV     TRUE 0.3611518951 1.702503e-128
    ## 2            x_numeric_clean     TRUE 0.0003297376  4.661912e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 0.0001820760  5.881746e-01
    ## 4      x_numeric_noise_clean     TRUE 0.0020403841  6.988543e-02
    ## 5                 x_cat_catP     TRUE 0.0001989547  5.713767e-01
    ## 6                 x_cat_catB     TRUE 0.2372722837  4.367353e-85
    ## 7           x_cat_noise_catP     TRUE 0.0001238860  6.551261e-01
    ## 8           x_cat_noise_catB     TRUE 0.0010389480  1.958494e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1222       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1222 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                         rsq count           sig
    ## x_cat           0.237272284     2  8.734705e-85
    ## x_cat_noise     0.001038948     2  3.916989e-01
    ## x_numeric       0.361151895     2 3.405006e-128
    ## x_numeric_noise 0.002040384     2  1.397709e-01

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                          rsq count           sig
    ## x_cat           0.2325661836     2  3.901284e-83
    ## x_cat_noise     0.0002888464     2  9.904626e-01
    ## x_numeric       0.3570105959     2 9.610490e-127
    ## x_numeric_noise 0.0020403841     2  1.397709e-01
