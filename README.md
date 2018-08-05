
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00584/status.svg)](https://doi.org/10.21105/joss.00584) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1173314.svg)](https://doi.org/10.5281/zenodo.1173314) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/vtreat)](https://cran.r-project.org/package=vtreat)

[vtreat](http://winvector.github.io/vtreat/) is an [R](https://cran.r-project.org) data.frame processor/conditioner that prepares real-world data for predictive modeling in a statistically sound manner.

For more detail please see here: [arXiv:1611.09477 stat.AP](https://arxiv.org/abs/1611.09477) . There is also a series of articles recording the evolution of `vtreat` including some tutorials [here](http://www.win-vector.com/blog/tag/vtreat/).

![](https://github.com/WinVector/vtreat/raw/master/tools/vtreat.png)

(logo: Julie Mount, source: "The Harvest" by Boris Kustodiev 1914)

Even with modern machine learning techniques (random forests, support vector machines, neural nets, gradient boosted trees, and so on) or standard statistical methods (regression, generalized regression, generalized additive models) there are *common* data issues that can cause modeling to fail. vtreat deals with a number of these in a principled and automated fashion.

In particular vtreat emphasizes a concept called "y-aware pre-processing" and implements:

-   Treatment of missing values through safe replacement plus indicator column (a simple but very powerful method when combined with downstream machine learning algorithms).
-   Treatment of novel levels (new values of categorical variable seen during test or application, but not seen during training) through sub-models (or impact/effects coding of pooled rare events).
-   Explicit coding of categorical variable levels as new indicator variables (with optional suppression of non-significant indicators).
-   Treatment of categorical variables with very large numbers of levels through sub-models (again [impact/effects coding](http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)).
-   (optional) User specified significance pruning on levels coded into effects/impact sub-models.
-   Correct treatment of nested models or sub-models through data split (see [here](https://winvector.github.io/vtreat/articles/vtreatOverfit.html)) or through the generation of "cross validated" data frames (see [here](https://winvector.github.io/vtreat/articles/vtreatCrossFrames.html)); these are issues similar to what is required to build statistically efficient stacked models or super-learners).
-   Safe processing of "wide data" (data with very many variables, often driving common machine learning algorithms to over-fit) through [out of sample per-variable significance estimates and user controllable pruning](https://winvector.github.io/vtreat/articles/vtreatSignificance.html) (something we have lectured on previously [here](https://github.com/WinVector/WinVector.github.io/tree/master/DS) and [here](http://www.win-vector.com/blog/2014/02/bad-bayes-an-example-of-why-you-need-hold-out-testing/)).
-   Collaring/Winsorizing of unexpected out of range numeric inputs.
-   (optional) Conversion of all variables into effects (or "y-scale") units (through the optional `scale` argument to `vtreat::prepare()`, using some of the ideas discussed [here](http://www.win-vector.com/blog/2014/06/skimming-statistics-papers-for-the-ideas-instead-of-the-complete-procedures/)). This allows correct/sensible application of principal component analysis pre-processing in a machine learning context.
-   Joining in additional training distribution data (which can be useful in analysis, called "catP" and "catD").

The idea is: even with a sophisticated machine learning algorithm there are *many* ways messy real world data can defeat the modeling process, and vtreat helps with at least ten of them. We emphasize: these problems are already in your data, you simply build better and more reliable models if you attempt to mitigate them. Automated processing is no substitute for actually looking at the data, but vtreat supplies efficient, reliable, documented, and tested implementations of many of the commonly needed transforms.

To help explain the methods we have prepared some documentation:

-   The [vtreat package overall](https://winvector.github.io/vtreat/index.html).
-   [Preparing data for analysis using R white-paper](http://winvector.github.io/DataPrep/EN-CNTNT-Whitepaper-Data-Prep-Using-R.pdf)
-   The [types of new variables](https://winvector.github.io/vtreat/articles/vtreatVariableTypes.html) introduced by vtreat processing (including how to limit down to domain appropriate variable types).
-   Statistically sound treatment of the nested modeling issue introduced by any sort of pre-processing (such as vtreat itself): [nested over-fit issues](https://winvector.github.io/vtreat/articles/vtreatOverfit.html) and a general [cross-frame solution](https://winvector.github.io/vtreat/articles/vtreatCrossFrames.html).
-   [Principled ways to pick significance based pruning levels](https://winvector.github.io/vtreat/articles/vtreatSignificance.html).

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

The above are all awful things that often lurk in real world data. Automating these steps ensures they are easy enough that you actually perform them and leaves the analyst time to look for additional data issues. For example this allowed us to essentially automate a number of the steps taught in chapters 4 and 6 of [*Practical Data Science with R* (Zumel, Mount; Manning 2014)](http://practicaldatascience.com/) into a [very short worksheet](http://winvector.github.io/KDD2009/KDD2009RF.html) (though we think for understanding it is *essential* to work all the steps by hand as we did in the book). The idea is: 'data.frame's prepared with the 'vtreat' library are somewhat safe to train on as some precaution has been taken against all of the above issues. Also of interest are the 'vtreat' variable significances (help in initial variable pruning, a necessity when there are a large number of columns) and 'vtreat::prepare(scale=TRUE)' which re-encodes all variables into effect units making them suitable for y-aware dimension reduction (variable clustering, or principal component analysis) and for geometry sensitive machine learning techniques (k-means, knn, linear SVM, and more). You may want to do more than the 'vtreat' library does (such as Bayesian imputation, variable clustering, and more) but you certainly do not want to do less.

There have been a number of recent substantial improvements to the library, including:

-   Out of sample scoring.
-   Ability to use 'parallel'.
-   More general calculation of effect sizes and significances.

Some of our related articles (which should make clear some of our motivations, and design decisions):

-   [Modeling trick: impact coding of categorical variables with many levels](http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/)
-   [A bit more on impact coding](http://www.win-vector.com/blog/2012/08/a-bit-more-on-impact-coding/)
-   [vtreat: designing a package for variable treatment](http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/)
-   [A comment on preparing data for classifiers](http://www.win-vector.com/blog/2014/12/a-comment-on-preparing-data-for-classifiers/)
-   [Nina Zumel presenting on vtreat](http://www.slideshare.net/ChesterChen/vtreat)
-   [What is new in the vtreat library?](http://www.win-vector.com/blog/2015/05/what-is-new-in-the-vtreat-library/)
-   [How do you know if your data has signal?](http://www.win-vector.com/blog/2015/08/how-do-you-know-if-your-data-has-signal/)

Examples of current best practice using 'vtreat' (variable coding, train, test split) can be found [here](https://winvector.github.io/vtreat/articles/vtreatOverfit.html) and [here](http://winvector.github.io/KDD2009/KDD2009RF.html).

Trivial example:

``` r
library("vtreat")
packageVersion("vtreat")
 #  [1] '1.3.1'
citation('vtreat')
 #  
 #  To cite package 'vtreat' in publications use:
 #  
 #    John Mount and Nina Zumel (2018). vtreat: A Statistically Sound
 #    'data.frame' Processor/Conditioner.
 #    https://github.com/WinVector/vtreat/,
 #    https://winvector.github.io/vtreat/.
 #  
 #  A BibTeX entry for LaTeX users is
 #  
 #    @Manual{,
 #      title = {vtreat: A Statistically Sound 'data.frame' Processor/Conditioner},
 #      author = {John Mount and Nina Zumel},
 #      year = {2018},
 #      note = {https://github.com/WinVector/vtreat/, https://winvector.github.io/vtreat/},
 #    }

# categorical example
dTrainC <- data.frame(x=c('a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,NA,6,NA),
   y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))

# help("designTreatmentsC")

treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                 verbose=FALSE)
print(treatmentsC$scoreFrame[,c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees')])
 #    origName   varName  code         rsq        sig extraModelDegrees
 #  1        x    x_catP  catP 0.130498074 0.26400089                 2
 #  2        x    x_catB  catB 0.030345745 0.59013918                 2
 #  3        z   z_clean clean 0.237601767 0.13176020                 0
 #  4        z   z_isBAD isBAD 0.296065432 0.09248399                 0
 #  5        x  x_lev_NA   lev 0.296065432 0.09248399                 0
 #  6        x x_lev_x_a   lev 0.130005705 0.26490379                 0
 #  7        x x_lev_x_b   lev 0.006067337 0.80967242                 0

# help("prepare")

dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=1.0,scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
 #         x_catP        x_catB       z_clean       z_isBAD      x_lev_NA 
 #   1.585994e-16  0.000000e+00  7.927952e-18 -7.926292e-18  3.965082e-18 
 #      x_lev_x_a     x_lev_x_b 
 #  -1.982154e-17  9.917546e-19
# all non NA slopes should be 1
sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})
 #     x_catP    x_catB   z_clean   z_isBAD  x_lev_NA x_lev_x_a x_lev_x_b 
 #          1         1         1         1         1         1         1
dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE)
print(dTestCTreated)
 #        x_catP     x_catB  z_clean    z_isBAD   x_lev_NA  x_lev_x_a
 #  1 -0.2380952 -0.1897682 1.194595 -0.1714286 -0.1714286 -0.2380952
 #  2  0.1785714 -0.1489924 2.951351 -0.1714286 -0.1714286  0.1785714
 #  3  0.8035714 -0.1320682 4.708108 -0.1714286 -0.1714286  0.1785714
 #  4  0.1785714  0.4336447 0.000000  0.4285714  0.4285714  0.1785714
 #      x_lev_x_b
 #  1  0.02857143
 #  2 -0.07142857
 #  3  0.02857143
 #  4  0.02857143
```

``` r
# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,5,NA,7,NA),y=c(0,0,0,1,0,1,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
# help("designTreatmentsN")
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y',
                                verbose=FALSE)
print(treatmentsN$scoreFrame[,c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees')])
 #    origName   varName  code          rsq       sig extraModelDegrees
 #  1        x    x_catP  catP 3.558824e-01 0.1184999                 2
 #  2        x    x_catN  catN 2.131202e-02 0.7301398                 2
 #  3        x    x_catD  catD 4.512437e-02 0.6135229                 2
 #  4        z   z_clean clean 2.880952e-01 0.1701892                 0
 #  5        z   z_isBAD isBAD 3.333333e-01 0.1339746                 0
 #  6        x  x_lev_NA   lev 3.333333e-01 0.1339746                 0
 #  7        x x_lev_x_a   lev 2.500000e-01 0.2070312                 0
 #  8        x x_lev_x_b   lev 1.110223e-16 1.0000000                 0
dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=1.0,scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
 #         x_catP        x_catN        x_catD       z_clean       z_isBAD 
 #   2.775558e-17  0.000000e+00 -2.775558e-17  4.857226e-17  6.938894e-18 
 #       x_lev_NA     x_lev_x_a     x_lev_x_b 
 #   6.938894e-18  0.000000e+00  7.703720e-34
# all non NA slopes should be 1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
 #     x_catP    x_catN    x_catD   z_clean   z_isBAD  x_lev_NA x_lev_x_a 
 #          1         1         1         1         1         1         1 
 #  x_lev_x_b 
 #          1
dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=c(),scale=TRUE)
print(dTestNTreated)
 #    x_catP x_catN      x_catD   z_clean    z_isBAD   x_lev_NA x_lev_x_a
 #  1 -0.250  -0.25 -0.06743804 0.9952381 -0.1666667 -0.1666667     -0.25
 #  2  0.250   0.00 -0.25818161 2.5666667 -0.1666667 -0.1666667      0.25
 #  3  0.625   0.00 -0.25818161 4.1380952 -0.1666667 -0.1666667      0.25
 #  4  0.250   0.50  0.39305768 0.0000000  0.5000000  0.5000000      0.25
 #        x_lev_x_b
 #  1 -2.266233e-17
 #  2  6.798700e-17
 #  3 -2.266233e-17
 #  4 -2.266233e-17

# for large data sets you can consider designing the treatments on 
# a subset like: d[sample(1:dim(d)[[1]],1000),]
```

Related work:

-   *Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences*, 2nd edition, 1983, Jacob Cohen, Patricia Cohen (called the concept “effects coded variables”).
-   ["A preprocessing scheme for high-cardinality categorical attributes in classification and prediction problems"](http://dl.acm.org/citation.cfm?id=507538) Daniele Micci-Barreca, ACM SIGKDD Explorations, Volume 3 Issue 1, July 2001 Pages 27-32.
-   ["Modeling Trick: Impact Coding of Categorical Variables with Many Levels"](http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/) Nina Zumel, Win-Vector blog, 2012.
-   ["Big Learning Made Easy – with Counts!"](https://blogs.technet.microsoft.com/machinelearning/2015/02/17/big-learning-made-easy-with-counts/), Misha Bilenko, Cortana Intelligence and Machine Learning Blog, 2015.

Installation
------------

Install with either:

``` r
install.packages("vtreat")
```

or

``` r
# install.packages("devtools")
devtools::install_github("WinVector/vtreat")
```

Note
----

Note: `vtreat` is meant only for "tame names", that is: variables and column names that are also valid *simple* (without quotes) `R` variables names.

Also, `vtreat` now has package options that are not set until the package is attached. These are

-   `vtreat.use_data.table_binding = TRUE`

Test of these options also defaults to the values above, so behavior should be the same regardless if `vteat` is attached or not. These options determine if `vtreat` will delegate row binding to `data.table`.
