<!-- README.md is generated from README.Rmd. Please edit that file -->
This package desing variable treatments so variables have fewer exceptional cases and models can be used safely in production. Common problems 'vtreat' defends against include: NA, Nan, Inf, too many categorical levels, rare categorical levels, new categorical levels (levels seen during application, but not during training).

Data treatments are "y-aware" (use distribution relations between independent variables and the dependent variable). For binary classification use designTreatmentsC() and for numeric regression use designTreatmentsN().

After the design step, prepare() should be used as you would use model.matrix. prepare() treated variables are all numeric and never take the value NA or +-Inf (so are very safe to use in modeling).

In application we suggest splitting your data into three sets: one for building vtreat encodings, one for training models using these encodings, and one for test and model evaluation.

Trivial example:

    ## [1] "desigining treatments Sun Sep  6 08:52:10 2015"
    ## [1] "design var x Sun Sep  6 08:52:10 2015"
    ## [1] "design var z Sun Sep  6 08:52:10 2015"
    ## [1] "scoring treatments Sun Sep  6 08:52:10 2015"
    ## [1] "have treatment plan Sun Sep  6 08:52:10 2015"

    ##      x_lev_NA     x_lev_x.a     x_lev_x.b        x_catB       z_clean 
    ## -7.930164e-18  2.379437e-17  2.974296e-18  7.922420e-18 -3.965138e-17 
    ##       z_isBAD 
    ## -7.926292e-18

    ##  x_lev_NA x_lev_x.a x_lev_x.b    x_catB   z_clean   z_isBAD 
    ##         1         1         1         1         1         1

    ##        x_lev_NA     x_lev_x.a     x_lev_x.b        x_catB   z_clean
    ## 1 -1.714286e-01 -2.380952e-01  2.857143e-02 -2.619173e-01 0.4918919
    ## 2 -1.714286e-01  1.785714e-01 -7.142857e-02 -1.479283e-02 0.4918919
    ## 3 -2.775558e-17  2.775558e-17  3.469447e-18  1.387779e-17 0.4918919
    ## 4  4.285714e-01  1.785714e-01  2.857143e-02  4.076688e-01 0.0000000
    ##      z_isBAD
    ## 1 -0.1714286
    ## 2 -0.1714286
    ## 3 -0.1714286
    ## 4  0.4285714

    ## [1] "desigining treatments Sun Sep  6 08:52:10 2015"
    ## [1] "design var x Sun Sep  6 08:52:10 2015"
    ## [1] "design var z Sun Sep  6 08:52:10 2015"
    ## [1] "scoring treatments Sun Sep  6 08:52:10 2015"
    ## [1] "have treatment plan Sun Sep  6 08:52:10 2015"

    ##     x_lev_NA    x_lev_x.a    x_lev_x.b       x_catN      z_clean 
    ## 9.020562e-17 0.000000e+00 0.000000e+00 7.021564e-17 1.526557e-16 
    ##      z_isBAD 
    ## 7.632783e-17

    ##  x_lev_NA x_lev_x.a x_lev_x.b    x_catN   z_clean   z_isBAD 
    ##         1         1        NA         1         1         1

    ##        x_lev_NA x_lev_x.a x_lev_x.b        x_catN      z_clean    z_isBAD
    ## 1 -1.666667e-01     -0.25         0 -2.500000e-01 5.238095e-01 -0.1666667
    ## 2 -1.666667e-01      0.25         0  5.887847e-17 5.238095e-01 -0.1666667
    ## 3  8.326673e-17      0.00         0  5.887847e-17 5.238095e-01 -0.1666667
    ## 4  5.000000e-01      0.25         0  5.000000e-01 1.110223e-16  0.5000000
