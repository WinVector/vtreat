Large Example
================
Win-Vector LLC
2/17/2018

[`vtreat`](https://winvector.github.io/vtreat/) is a an [`R`](https://cran.r-project.org) 'data.frame' processor/conditioner that prepares real-world data for predictive modeling in a statistically sound manner. `vtreat` prepares variables so that data has fewer exceptional cases, making it easier to safely use models in production. Common problems `vtreat` defends against: 'Inf', 'NA', too many categorical levels, rare categorical levels, and new categorical levels (levels seen during application, but not during training). Reference: ["`vtreat`: a data.frame Processor for Predictive Modeling", Zumel, Mount, 2016](https://arxiv.org/abs/1611.09477), [DOI:10.5281/zenodo.1173314](https://doi.org/10.5281/zenodo.1173314).

[`vtreat::mkCrossFrameCExperiment()`](https://winvector.github.io/vtreat/reference/mkCrossFrameCExperiment.html) and [`vtreat::mkCrossFrameCExperiment()`](https://winvector.github.io/vtreat/reference/mkCrossFrameNExperiment.html) are the most statistically efficient methods `vtreat` supplies. So we often advise them, especially for data with few rows. However they have non-trivial computational cost. For data with very many rows we suggest the more computationally efficient [`vtreat`::designTreamentsC()](https://winvector.github.io/vtreat/reference/designTreatmentsC.html) and [`vtreat`::designTreatmensN()](https://winvector.github.io/vtreat/reference/designTreatmentsN.html).

Here is an example (based on [`vtreat issue 12`](https://github.com/WinVector/vtreat/issues/12), [perf tests](https://github.com/WinVector/vtreat/blob/master/extras/PerfTests.md), and [perf tests2](https://github.com/WinVector/vtreat/blob/parallel_ind/extras/PerfTests2.md)).

Example data (in this case no variables are truly related to the outcome to be predicted).

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.0.3'

``` r
useParallel <- TRUE

mkEx <- function(n_rows, 
                 n_cat_columns, n_num_columns, n_irrel_columns,
                 n_cat_levels_a, n_cat_levels_b) {
  n_cols <- n_cat_columns + n_num_columns + n_irrel_columns + 2
  d <- as.data.frame(matrix(data = rnorm(n_rows * n_cols), 
                            nrow = n_rows, ncol = n_cols))
  cat_names <- NULL
  num_names <- NULL
  irrel_names <- NULL
  if(n_cat_columns>0) {
    cat_names <- paste0('var_cat_', seq_len(n_cat_columns))
  }
  if(n_num_columns>0) {
    num_names <- paste0('var_num_', seq_len(n_num_columns))
  }
  if(n_irrel_columns>0) {
    irrel_names <- paste0('irrel_', seq_len(n_irrel_columns))
  }
  y_names <- c("yC", "yN")
  colnames(d) <- c(cat_names, num_names, irrel_names, y_names)
  d$yC <- ifelse(d$yC>=0, "Y", "N")
  levels_a <- paste0("lev_a_", seq_len(n_cat_levels_a))
  levels_b <- NULL
  if(n_cat_levels_b>0) {
    levels_b <- paste0("lev_b_", seq_len(n_cat_levels_b))
  }
  for(ci in cat_names) {
    a_set <- rep(TRUE, n_rows)
    if(n_cat_levels_b>0) {
      a_set <- runif(n_rows)>=0.5
    }
    na <- sum(a_set)
    nb <- n_rows - na
    if(na>0) {
      d[[ci]][a_set] <- sample(levels_a, na, replace = TRUE)
    }
    if(nb>0) {
       d[[ci]][!a_set] <- sample(levels_b, nb, replace = TRUE)
    }
  }
  d
}

parallelCluster <- NULL
if(useParallel) {
  ncores <- parallel::detectCores()
  parallelCluster <- parallel::makeCluster(ncores)
}

n_rows <- 2000000
```

Convert the large cardinality categorical variables into new single column sub-models ready to be used in later modeling.

``` r
d <- mkEx(n_rows = n_rows,
          n_cat_columns = 2,
          n_num_columns = 2,
          n_irrel_columns = 10,
          n_cat_levels_a = 10,
          n_cat_levels_b = 50000)
yName <- "yC"
yTarget <- "Y"
varNames <- colnames(d)[grep("^var", colnames(d))]
codeTargets <- c("catB", "catP", "clean", "isBAD")
system.time(
  {
    splitGroup <- sample.int(2, nrow(d), replace = TRUE)
    tplan <- vtreat::designTreatmentsC(d[splitGroup==1, , drop = FALSE],
                                       varNames,
                                       yName,
                                       yTarget,
                                       codeRestriction = codeTargets,
                                       parallelCluster = parallelCluster)
    scoreFrame <- tplan$scoreFrame
    print(scoreFrame[, c("varName", "rsq", "sig", "extraModelDegrees", "origName")])
    # newVars <- scoreFrame$varName[scoreFrame$sig < 1/nrow(scoreFrame)]
    newVars <- scoreFrame$varName
    trainFrame <- vtreat::prepare(tplan, 
                                  d[splitGroup==2, , drop= FALSE],
                                  varRestriction = newVars,
                                  parallelCluster = parallelCluster)
})
```

    ## [1] "designing treatments Sat Feb 17 06:08:54 2018"
    ## [1] "designing treatments Sat Feb 17 06:08:54 2018"
    ## [1] " have initial level statistics Sat Feb 17 06:08:58 2018"
    ## [1] " scoring treatments Sat Feb 17 06:09:11 2018"
    ## [1] "have treatment plan Sat Feb 17 06:09:29 2018"
    ## [1] "rescoring complex variables Sat Feb 17 06:09:29 2018"
    ## [1] "done rescoring complex variables Sat Feb 17 06:10:18 2018"
    ##           varName          rsq        sig extraModelDegrees  origName
    ## 1  var_cat_1_catP 3.714442e-09 0.94281274             50005 var_cat_1
    ## 2  var_cat_1_catB 4.046618e-06 0.01789792             50005 var_cat_1
    ## 3  var_cat_2_catP 3.124530e-07 0.51058560             50005 var_cat_2
    ## 4  var_cat_2_catB 4.306401e-07 0.43987781             50005 var_cat_2
    ## 5 var_num_1_clean 9.964966e-07 0.24001112                 0 var_num_1
    ## 6 var_num_2_clean 1.724072e-06 0.12223092                 0 var_num_2

    ##    user  system elapsed 
    ##  30.953   2.600  88.468

``` r
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- NULL
}
```
