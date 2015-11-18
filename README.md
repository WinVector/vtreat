<!-- README.md is generated from README.Rmd. Please edit that file -->
This package ('vtreat' available as [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vtreat)](https://cran.r-project.org/package=vtreat) and [Github 0.5.22](https://github.com/WinVector/vtreat) ) designs variable treatments so variables have fewer exceptional cases and models can be used safely in production. Common problems 'vtreat' defends against include: NA, Nan, Inf, too many categorical levels, rare categorical levels, new categorical levels (levels seen during application, but not during training).

Data treatments are "y-aware" (use distribution relations between independent variables and the dependent variable). For binary classification use 'designTreatmentsC()' and for numeric regression use 'designTreatmentsN()'.

After the design step, 'prepare()' should be used as you would use model.matrix. 'prepare()' treated variables are all numeric and never take the value NA or +-Inf (so are very safe to use in modeling).

In application we suggest splitting your data into three sets: one for building vtreat encodings, one for training models using these encodings, and one for test and model evaluation.

'vtreat' is supplied by [Win-Vector LLC](http://www.win-vector.com) under a GPL-3 license, without warranty.

The purpose of 'vtreat' library is to reliably prepare data for supervised machine learning. We try to leave as much as possible to the machine learning algorithms themselves, but cover most of the truly necessary typically ignored precautions. The library is designed to produce a 'data.frame' that is entirely numeric and takes common precautions to guard against the following real world data issues:

-   Categorical variables with very many levels.

    We re-encode such variables as a family of indicator or dummy variables for common levels plus an additional [impact code](http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/) (also called "effects coded" in Jacob Cohen, Patricia Cohen, *Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences*, 2nd edition, 1983). This allows principled use (including smoothing) of huge categorical variables (like zip-codes) when building models. This is critical for some libraries (such as 'randomForest', which has hard limits on the number of allowed levels).

-   Rare categorical levels.

    Levels that do not occur often during training tend not to have reliable effect estimates and contribute to over-fit. vtreat helps with 2 precautions in this case. First the 'rareLevel' argument suppresses levels with this count our below from modeling, except possibly through a grouped contribution. Also with enough data vtreat attempts to estimate out of sample performance of derived variables. Finally we suggest users reserve a portion of data for vtreat design, separate from any data used in additional training, calibration, or testing.

-   Novel categorical levels.

    A common problem in deploying a classifier to production is: new levels (levels not seen during training) encountered during model application. We deal with this by encoding categorical variables in a possibly redundant manner: reserving a dummy variable for all levels (not the more common all but a reference level scheme). This is in fact the correct representation for regularized modeling techniques and lets us code novel levels as all dummies simultaneously zero (which is a reasonable thing to try). This encoding while limited is cheaper than the fully Bayesian solution of computing a weighted sum over previously seen levels during model application.

-   Missing/invalid values NA, NaN, +-Inf.

    Variables with these issues are re-coded as two columns. The first column is clean copy of the variable (with missing/invalid values replaced with either zero or the grand mean, depending on the user chose of the 'scale' parameter). The second column is a dummy or indicator that marks if the replacement has been performed. This is simpler than imputation of missing values, and allows the downstream model to attempt to use missingness as a useful signal (which it often is in industrial data).

-   Extreme values.

    Variables can be restricted to stay in ranges seen during training. This can defend against some run-away classifier issues during model application.

-   Constant and near-constant variables.

    Variables that "don't vary" or "nearly don't vary" are suppressed.

-   Need for estimated single-variable model effect sizes and significances.

    It is a dirty secret that even popular machine learning techniques need some variable pruning (when exposed to very wide data frames, see [here](http://www.win-vector.com/blog/2014/02/bad-bayes-an-example-of-why-you-need-hold-out-testing/) and [here](https://www.youtube.com/watch?v=X_Rn3EOEjGE)). We make the necessary effect size estimates and significances easily available and supply initial variable pruning.

The above are all awful things that often lurk in real world data. Automating these steps ensures they are easy enough that you actually perform them and leaves the analyst time to look for additional data issues. For example this allowed us to essentially automate a number of the steps taught in chapters 4 and 6 of [*Practical Data Science with R* (Zumel, Mount; Manning 2014)](http://practicaldatascience.com/) into a [very short worksheet](http://winvector.github.io/KDD2009/KDD2009RF.html) (though we think for understanding it is *essential* to work all the steps by hand as we did in the book). The idea is: 'data.frame's prepared with the 'vtreat' library are somewhat safe to train on as some precaution has been taken against all of the above issues. Also of interest are the 'vtreat' variable significances (help in initial variable pruning, a necessity when there are a large number of columns) and 'vtreat::prepare(scale=TRUE)' which re-encodes all variables into effect units making them suitable for y-aware dimension reduction (variable clustering, or principal component analysis) and for geometry sensitive machine learning techniques (k-means, knn, linear SVM, and more). You may want to do more than the 'vtreat' library does (such as Bayesian imputation, variable clustering, and more) but you certainly do not want to do less. The [original announcement](http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/) is getting a bit out of date, so we hope to be able to write a new article on 'vtreat' soon. Until then we suggest running 'vignette('vtreat')' in R to produce a rendered version of the [package vignette](https://cran.r-project.org/web/packages/vtreat/vignettes/vtreat.html). You can also checkout the package manual, now [available online](https://cran.r-project.org/web/packages/vtreat/vtreat.pdf). There have been a number of recent substantial improvements to the library, including:

-   Out of sample scoring.
-   Ability to use 'parallel'.
-   More general calculation of effect sizes and significances.
-   Addition of collaring or [Winsorising](https://en.wikipedia.org/wiki/Winsorising) to defend from outliers.

Some of our related articles (which should make clear some of our motivations, and design decisions):

-   [Modeling trick: impact coding of categorical variables with many levels](http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)
-   [A bit more on impact coding](http://www.win-vector.com/blog/2012/08/a-bit-more-on-impact-coding/)
-   [vtreat: designing a package for variable treatment](http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/)
-   [A comment on preparing data for classifiers](http://www.win-vector.com/blog/2014/12/a-comment-on-preparing-data-for-classifiers/)
-   [Nina Zumel presenting on vtreat](http://www.slideshare.net/ChesterChen/vtreat)
-   [What is new in the vtreat library?](http://www.win-vector.com/blog/2015/05/what-is-new-in-the-vtreat-library/)
-   [How do you know if your data has signal?](http://www.win-vector.com/blog/2015/08/how-do-you-know-if-your-data-has-signal/)

Examples of current best practice using 'vtreat' (variable coding, train, test split) can be found [here](http://winvector.github.io/vtreat/Overfit.html) and [here](http://winvector.github.io/KDD2009/KDD2009RF.html).

Trivial example:

``` r
library("vtreat")

# categorical example
dTrainC <- data.frame(x=c('a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,NA,6,NA),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))

# help("designTreatmentsC")

treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                 verbose=FALSE)
print(treatmentsC$scoreFrame)
#>     varName origName  code needsSplit varMoves PRESSRsquared psig
#> 1  x_lev_NA        x   lev      FALSE     TRUE   -0.09374781    1
#> 2 x_lev_x.a        x   lev      FALSE     TRUE   -0.65277227    1
#> 3 x_lev_x.b        x   lev      FALSE     TRUE   -1.26040281    1
#> 4    x_catP        x  catP       TRUE     TRUE   -0.65261808    1
#> 5    x_catB        x  catB       TRUE     TRUE   -0.25610592    1
#> 6   z_clean        z clean      FALSE     TRUE   -0.13404882    1
#> 7   z_isBAD        z isBAD      FALSE     TRUE   -0.09374781    1
#>          sig catPRSquared       csig
#> 1 0.09248399  0.296065432 0.09248399
#> 2 0.26490379  0.130005705 0.26490379
#> 3 0.80967242  0.006067337 0.80967242
#> 4 0.26490379  0.130005705 0.26490379
#> 5 0.18011273  0.187924640 0.18011273
#> 6 0.13176020  0.237601767 0.13176020
#> 7 0.09248399  0.296065432 0.09248399

# help("prepare")

dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=1.0,scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
#>      x_lev_NA     x_lev_x.a     x_lev_x.b        x_catP        x_catB 
#> -7.930164e-18  2.379437e-17  2.974296e-18 -2.854898e-16  7.922420e-18 
#>       z_clean       z_isBAD 
#> -3.965138e-17 -7.926292e-18
# all slopes should be 1
sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})
#>  x_lev_NA x_lev_x.a x_lev_x.b    x_catP    x_catB   z_clean   z_isBAD 
#>         1         1         1         1         1         1         1
dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE)
print(dTestCTreated)
#>     x_lev_NA  x_lev_x.a   x_lev_x.b     x_catP      x_catB   z_clean
#> 1 -0.1714286 -0.2380952  0.02857143 -0.2380952 -0.26191735 0.4918919
#> 2 -0.1714286  0.1785714 -0.07142857  0.1785714 -0.01479283 0.4918919
#> 3 -0.1714286  0.1785714  0.02857143  1.0119048  0.06658235 0.4918919
#> 4  0.4285714  0.1785714  0.02857143  0.1785714  0.40766885 0.0000000
#>      z_isBAD
#> 1 -0.1714286
#> 2 -0.1714286
#> 3 -0.1714286
#> 4  0.4285714
```

``` r
# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,5,NA,7,NA),y=c(0,0,0,1,0,1,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
# help("designTreatmentsN")
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                verbose=FALSE)
print(treatmentsN$scoreFrame)
#>     varName origName  code needsSplit varMoves PRESSRsquared      psig
#> 1  x_lev_NA        x   lev      FALSE     TRUE    0.04000128 0.6348745
#> 2 x_lev_x.a        x   lev      FALSE     TRUE   -0.33333000 1.0000000
#> 3 x_lev_x.b        x   lev      FALSE     TRUE   -1.07998856 1.0000000
#> 4    x_catP        x  catP       TRUE     TRUE   -0.33330334 1.0000000
#> 5    x_catN        x  catN       TRUE     TRUE   -0.74281970 1.0000000
#> 6    x_catD        x  catD       TRUE     TRUE   -3.26661714 1.0000000
#> 7   z_clean        z clean      FALSE     TRUE   -0.02135479 1.0000000
#> 8   z_isBAD        z isBAD      FALSE     TRUE    0.04000128 0.6348745
#>         sig
#> 1 0.6348745
#> 2 1.0000000
#> 3 1.0000000
#> 4 1.0000000
#> 5 1.0000000
#> 6 1.0000000
#> 7 1.0000000
#> 8 0.6348745
dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=1.0,scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
#>     x_lev_NA    x_lev_x.a    x_lev_x.b       x_catP       x_catN 
#> 9.020562e-17 0.000000e+00 2.500000e-01 8.326673e-17 7.021564e-17 
#>       x_catD      z_clean      z_isBAD 
#> 8.326673e-17 1.526557e-16 7.632783e-17
# all slopes should be 1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
#>     x_lev_NA    x_lev_x.a    x_lev_x.b       x_catP       x_catN 
#> 1.000000e+00 1.000000e+00 9.064933e-17 1.000000e+00 1.000000e+00 
#>       x_catD      z_clean      z_isBAD 
#> 1.000000e+00 1.000000e+00 1.000000e+00
dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=c(),scale=TRUE)
print(dTestNTreated)
#>     x_lev_NA x_lev_x.a x_lev_x.b x_catP        x_catN      x_catD
#> 1 -0.1666667     -0.25         0  -0.25 -2.500000e-01 -0.06743804
#> 2 -0.1666667      0.25         1   0.25  5.887847e-17 -0.25818161
#> 3 -0.1666667      0.25         0   0.75  5.887847e-17 -0.25818161
#> 4  0.5000000      0.25         0   0.25  5.000000e-01  0.39305768
#>        z_clean    z_isBAD
#> 1 5.238095e-01 -0.1666667
#> 2 5.238095e-01 -0.1666667
#> 3 5.238095e-01 -0.1666667
#> 4 1.110223e-16  0.5000000

# for large data sets you can consider designing the treatments on 
# a subset like: d[sample(1:dim(d)[[1]],1000),]
```
