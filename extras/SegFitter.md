SegFitter
================

``` r
library("ggplot2")
```

``` r
customCoders = list('c.PiecewiseV.num' = vtreat::solve_piecewise,
                    'n.PiecewiseV.num' = vtreat::solve_piecewise,
                    'c.knearest.num' = vtreat::square_window,
                    'n.knearest.num' = vtreat::square_window)
codeRestriction = c("PiecewiseV", 
                    "knearest",
                    "poolN", "poolC",
                    "NonDecreasingV", "NonIncreasingV",
                    "clean", "isBAD", "catB", "catP")
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

dcheck <- d[1:2, ]
dcheck$x_numeric <- NA_real_
dcheck$x_cat[1] <- "new_level"
dcheck$x_cat[2] <- NA_character_
dcheck
```

    ##   x_numeric     x_cat     y_ideal x_numeric_noise x_cat_noise          y
    ## 1        NA new_level 0.000000000           12.25         l_4  0.2223316
    ## 2        NA      <NA> 0.009999833            2.40       l_1.6 -0.3116115
    ##      yc is_train
    ## 1 FALSE     TRUE
    ## 2 FALSE    FALSE

``` r
head(d)
```

    ##   x_numeric x_cat     y_ideal x_numeric_noise x_cat_noise          y    yc
    ## 1      0.00   l_0 0.000000000           12.25         l_4  0.2223316 FALSE
    ## 2      0.01   l_0 0.009999833            2.40       l_1.6 -0.3116115 FALSE
    ## 3      0.02   l_0 0.019998667           14.23      l_10.1  0.3279821 FALSE
    ## 4      0.03   l_0 0.029995500           12.77       l_7.9 -0.6093696 FALSE
    ## 5      0.04   l_0 0.039989334            4.54       l_9.2  0.2709885 FALSE
    ## 6      0.05   l_0 0.049979169           11.44      l_14.2 -0.1573684 FALSE
    ##   is_train
    ## 1     TRUE
    ## 2    FALSE
    ## 3    FALSE
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
    ##  Length:1501        Min.   :-2.2493   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:-0.5458   FALSE:969       FALSE:281      
    ##  Mode  :character   Median : 0.1433   TRUE :532       TRUE :1220     
    ##                     Mean   : 0.1083                                  
    ##                     3rd Qu.: 0.7442                                  
    ##                     Max.   : 2.8165

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
    ## 1       x_numeric_PiecewiseV     TRUE 0.6514630482 4.802074e-281
    ## 2         x_numeric_knearest     TRUE 0.6428991062 1.271859e-274
    ## 3            x_numeric_clean     TRUE 0.0005673683  4.058357e-01
    ## 4 x_numeric_noise_PiecewiseV     TRUE 0.0017048760  1.494889e-01
    ## 5   x_numeric_noise_knearest     TRUE 0.0026962836  6.982372e-02
    ## 6      x_numeric_noise_clean     TRUE 0.0029901581  5.620499e-02
    ## 7                 x_cat_catP     TRUE 0.0016149531  1.606821e-01
    ## 8           x_cat_noise_catP     TRUE 0.0017530032  1.438621e-01
    ##   needsSplit extraModelDegrees        origName       code
    ## 1       TRUE              1220       x_numeric PiecewiseV
    ## 2       TRUE              1220       x_numeric   knearest
    ## 3      FALSE                 0       x_numeric      clean
    ## 4       TRUE              1220 x_numeric_noise PiecewiseV
    ## 5       TRUE              1220 x_numeric_noise   knearest
    ## 6      FALSE                 0 x_numeric_noise      clean
    ## 7       TRUE               150           x_cat       catP
    ## 8       TRUE               150     x_cat_noise       catP

``` r
vtreat::variable_values(cfn$treatments$scoreFrame)
```

    ##                         rsq count           sig             var
    ## x_cat           0.001614953     1  1.606821e-01           x_cat
    ## x_cat_noise     0.001753003     1  1.438621e-01     x_cat_noise
    ## x_numeric       0.651463048     3 1.440622e-280       x_numeric
    ## x_numeric_noise 0.002990158     3  1.686150e-01 x_numeric_noise

``` r
# or directly
vtreat::value_variables_N(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'y')
```

    ##                          rsq count           sig             var
    ## x_cat           2.110403e-05     1  8.726488e-01           x_cat
    ## x_cat_noise     2.136992e-03     1  1.065559e-01     x_cat_noise
    ## x_numeric       6.487648e-01     3 1.581764e-278       x_numeric
    ## x_numeric_noise 3.900086e-03     3  8.750557e-02 x_numeric_noise

``` r
prepared <- vtreat::prepare(cfn$treatments, d)
d$x_numeric_PiecewiseV <- prepared$x_numeric_PiecewiseV
d$x_numeric_knearest <- prepared$x_numeric_knearest

ggplot(data=d) +
  # geom_point(aes(x = x_numeric, y = y)) + 
  geom_line(aes(x = x_numeric, y = y_ideal), color = "lightblue") + 
  geom_line(aes(x = x_numeric, y = x_numeric_PiecewiseV)) +
  ggtitle("y_ideal as a function of x_numeric_PiecewiseV")
```

![](SegFitter_files/figure-markdown_github/solve_numeric-1.png)

``` r
ggplot(data=d) +
  # geom_point(aes(x = x_numeric, y = y)) + 
  geom_line(aes(x = x_numeric, y = y_ideal), color = "lightblue") + 
  geom_line(aes(x = x_numeric, y = x_numeric_knearest)) +
  ggtitle("y_ideal as a function of x_numeric_knearest")
```

![](SegFitter_files/figure-markdown_github/solve_numeric-2.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y",
                     "x_numeric_PiecewiseV versus observed y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-3.png)

``` r
WVPlots::ScatterHist(d[d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y_ideal",
                     "x_numeric_PiecewiseV versus ideal y on train",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-4.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y",
                     "x_numeric_PiecewiseV versus observed y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-5.png)

``` r
WVPlots::ScatterHist(d[!d$is_train, , drop=FALSE], 
                     "x_numeric_PiecewiseV", "y_ideal",
                     "x_numeric_PiecewiseV versus ideal y on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE)
```

![](SegFitter_files/figure-markdown_github/solve_numeric-6.png)

``` r
vtreat::prepare(cfn$treatments, dcheck)
```

    ##   x_numeric_PiecewiseV x_numeric_knearest x_numeric_clean
    ## 1            0.1196219          0.1196219        7.473811
    ## 2            0.1196219          0.1196219        7.473811
    ##   x_numeric_noise_PiecewiseV x_numeric_noise_knearest
    ## 1                 0.18479101                0.3233652
    ## 2                 0.07641353                0.3416230
    ##   x_numeric_noise_clean   x_cat_catP x_cat_noise_catP          y
    ## 1                 12.25 0.0004098361      0.007377049  0.2223316
    ## 2                  2.40 0.0004098361      0.004098361 -0.3116115

``` r
cfc <- vtreat::mkCrossFrameCExperiment(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE,
  customCoders = customCoders,
  codeRestriction = codeRestriction,
  verbose = FALSE)

cfc$treatments
```

    ##                       varName varMoves          rsq           sig
    ## 1        x_numeric_PiecewiseV     TRUE 3.737677e-01 4.046363e-131
    ## 2          x_numeric_knearest     TRUE 3.564538e-01 3.878636e-125
    ## 3             x_numeric_clean     TRUE 6.647456e-06  9.181601e-01
    ## 4  x_numeric_noise_PiecewiseV     TRUE 1.386865e-03  1.377690e-01
    ## 5    x_numeric_noise_knearest     TRUE 5.152255e-04  3.656741e-01
    ## 6       x_numeric_noise_clean     TRUE 4.507035e-03  7.461603e-03
    ## 7                  x_cat_catP     TRUE 3.368584e-05  8.170781e-01
    ## 8                  x_cat_catB     TRUE 3.043611e-01 3.880712e-107
    ## 9            x_cat_noise_catP     TRUE 6.025697e-05  7.570475e-01
    ## 10           x_cat_noise_catB     TRUE 3.185856e-03  2.448468e-02
    ##    needsSplit extraModelDegrees        origName       code
    ## 1        TRUE              1220       x_numeric PiecewiseV
    ## 2        TRUE              1220       x_numeric   knearest
    ## 3       FALSE                 0       x_numeric      clean
    ## 4        TRUE              1220 x_numeric_noise PiecewiseV
    ## 5        TRUE              1220 x_numeric_noise   knearest
    ## 6       FALSE                 0 x_numeric_noise      clean
    ## 7        TRUE               150           x_cat       catP
    ## 8        TRUE               150           x_cat       catB
    ## 9        TRUE               150     x_cat_noise       catP
    ## 10       TRUE               150     x_cat_noise       catB

``` r
vtreat::variable_values(cfc$treatments$scoreFrame)
```

    ##                         rsq count           sig             var
    ## x_cat           0.304361091     2 7.761425e-107           x_cat
    ## x_cat_noise     0.003185856     2  4.896935e-02     x_cat_noise
    ## x_numeric       0.373767743     3 1.213909e-130       x_numeric
    ## x_numeric_noise 0.004507035     3  2.238481e-02 x_numeric_noise

``` r
# or directly
vtreat::value_variables_C(
  d[d$is_train, , drop=FALSE], 
  c('x_numeric', 'x_numeric_noise', 'x_cat', 'x_cat_noise'), 'yc', TRUE)
```

    ##                          rsq count           sig             var
    ## x_cat           0.2914482794     2 2.253035e-102           x_cat
    ## x_cat_noise     0.0005544533     2  6.960659e-01     x_cat_noise
    ## x_numeric       0.3869969854     3 3.266428e-135       x_numeric
    ## x_numeric_noise 0.0045070351     3  2.238481e-02 x_numeric_noise

``` r
vtreat::prepare(cfc$treatments, dcheck)
```

    ##   x_numeric_PiecewiseV x_numeric_knearest x_numeric_clean
    ## 1            0.3557377          0.3557377        7.473811
    ## 2            0.3557377          0.3557377        7.473811
    ##   x_numeric_noise_PiecewiseV x_numeric_noise_knearest
    ## 1                  0.3957186                      0.4
    ## 2                  0.3129442                      0.5
    ##   x_numeric_noise_clean   x_cat_catP x_cat_catB x_cat_noise_catP
    ## 1                 12.25 0.0004098361          0      0.007377049
    ## 2                  2.40 0.0004098361          0      0.004098361
    ##   x_cat_noise_catB    yc
    ## 1      -0.09921836 FALSE
    ## 2       0.18846371 FALSE
