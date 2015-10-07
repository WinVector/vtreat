<!-- README.md is generated from README.Rmd. Please edit that file -->
This package ('vtreat' available as [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vtreat)](https://cran.r-project.org/package=vtreat) and [Github 0.5.17](https://github.com/WinVector/vtreat) ) designs variable treatments so variables have fewer exceptional cases and models can be used safely in production. Common problems 'vtreat' defends against include: NA, Nan, Inf, too many categorical levels, rare categorical levels, new categorical levels (levels seen during application, but not during training).

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
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
#> [1] "desigining treatments Tue Oct  6 17:12:52 2015"
#> [1] "design var x Tue Oct  6 17:12:52 2015"
#> [1] "design var z Tue Oct  6 17:12:52 2015"
#> [1] "scoring treatments Tue Oct  6 17:12:52 2015"
#> [1] "WARNING skipped vars: x"
#> [1] "have treatment plan Tue Oct  6 17:12:52 2015"
dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=1.0,scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
#>       z_clean       z_isBAD 
#> -3.965138e-17 -7.926292e-18
# all slopes should be 1
sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})
#> z_clean z_isBAD 
#>       1       1
dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE)
print(dTestCTreated)
#>     z_clean    z_isBAD
#> 1 0.4918919 -0.1714286
#> 2 0.4918919 -0.1714286
#> 3 0.4918919 -0.1714286
#> 4 0.0000000  0.4285714

# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,5,NA,7,NA),y=c(0,0,0,1,0,1,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
#> [1] "desigining treatments Tue Oct  6 17:12:52 2015"
#> [1] "design var x Tue Oct  6 17:12:52 2015"
#> [1] "design var z Tue Oct  6 17:12:52 2015"
#> [1] "scoring treatments Tue Oct  6 17:12:52 2015"
#> [1] "have treatment plan Tue Oct  6 17:12:52 2015"
dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=1.0,scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
#>   x_lev_rare    x_lev_x.a       x_catN      z_clean      z_isBAD 
#> 1.110223e-16 0.000000e+00 2.775558e-17 1.526557e-16 7.632783e-17
# all slopes should be 1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
#> x_lev_rare  x_lev_x.a     x_catN    z_clean    z_isBAD 
#>          1          1          1          1          1
dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=c(),scale=TRUE)
print(dTestNTreated)
#>   x_lev_rare x_lev_x.a x_catN      z_clean    z_isBAD
#> 1      -0.25     -0.25  -0.25 5.238095e-01 -0.1666667
#> 2       0.25      0.25   0.25 5.238095e-01 -0.1666667
#> 3       0.25      0.25   0.25 5.238095e-01 -0.1666667
#> 4       0.25      0.25   0.25 1.110223e-16  0.5000000

# for large data sets you can consider designing the treatments on 
# a subset like: d[sample(1:dim(d)[[1]],1000),]
```
