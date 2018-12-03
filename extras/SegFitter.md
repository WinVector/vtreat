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
    ## 1      0.00   l_0 0.000000000           12.53       l_6.2 -0.12216836
    ## 2      0.01   l_0 0.009999833            2.27         l_6 -0.10059294
    ## 3      0.02   l_0 0.019998667            5.37       l_7.2 -0.65979645
    ## 4      0.03   l_0 0.029995500           12.64       l_5.5  0.53589010
    ## 5      0.04   l_0 0.039989334            5.95       l_8.2 -0.02865555
    ## 6      0.05   l_0 0.049979169           12.72       l_5.3  0.15017077
    ##      yc is_train
    ## 1 FALSE     TRUE
    ## 2 FALSE     TRUE
    ## 3 FALSE     TRUE
    ## 4  TRUE     TRUE
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
    ##  Length:1501        Min.   :-2.2950   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5285   FALSE:971       FALSE:303      
    ##  Mode  :character   Median : 0.1862   TRUE :530       TRUE :1198     
    ##                     Mean   : 0.1146                                  
    ##                     3rd Qu.: 0.7686                                  
    ##                     Max.   : 2.5606

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
    ## 1       x_numeric_PiecewiseV     TRUE 6.428032e-01 1.251525e-269
    ## 2            x_numeric_clean     TRUE 1.539031e-03  1.747952e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 1.973588e-04  6.271352e-01
    ## 4      x_numeric_noise_clean     TRUE 4.780295e-05  8.110589e-01
    ## 5                 x_cat_catP     TRUE 2.519119e-03  8.247675e-02
    ## 6           x_cat_noise_catP     TRUE 3.334111e-04  5.277823e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1198       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1198 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.0025191191     1  8.247675e-02
    ## x_cat_noise     0.0003334111     1  5.277823e-01
    ## x_numeric       0.6428032338     2 2.503050e-269
    ## x_numeric_noise 0.0001973588     2  1.000000e+00

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig
    ## x_cat           7.151639e-05     1  7.699767e-01
    ## x_cat_noise     1.698720e-04     1  6.522308e-01
    ## x_numeric       6.380760e-01     2 6.524385e-266
    ## x_numeric_noise 8.223152e-04     2  6.426811e-01

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
    ## 1       x_numeric_PiecewiseV     TRUE 3.599038e-01 2.390335e-124
    ## 2            x_numeric_clean     TRUE 6.423886e-04  3.163398e-01
    ## 3 x_numeric_noise_PiecewiseV     TRUE 4.449050e-04  4.043462e-01
    ## 4      x_numeric_noise_clean     TRUE 4.338235e-05  7.945609e-01
    ## 5                 x_cat_catP     TRUE 1.372271e-04  6.432795e-01
    ## 6                 x_cat_catB     TRUE 2.439967e-01  6.312272e-85
    ## 7           x_cat_noise_catP     TRUE 9.218479e-05  7.042582e-01
    ## 8           x_cat_noise_catB     TRUE 8.090736e-04  2.607936e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1198       x_numeric PiecewiseV
    ## 2      FALSE                 0       x_numeric      clean
    ## 3       TRUE              1198 x_numeric_noise PiecewiseV
    ## 4      FALSE                 0 x_numeric_noise      clean
    ## 5       TRUE               150           x_cat       catP
    ## 6       TRUE               150           x_cat       catB
    ## 7       TRUE               150     x_cat_noise       catP
    ## 8       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                          rsq count           sig
    ## x_cat           0.2439966515     2  1.262454e-84
    ## x_cat_noise     0.0008090736     2  5.215871e-01
    ## x_numeric       0.3599037768     2 4.780670e-124
    ## x_numeric_noise 0.0004449050     2  8.086923e-01

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                          rsq count           sig
    ## x_cat           0.2284307597     2  2.502406e-79
    ## x_cat_noise     0.0009746853     2  4.342168e-01
    ## x_numeric       0.3555818674     2 1.409012e-122
    ## x_numeric_noise 0.0004908883     2  7.621490e-01
