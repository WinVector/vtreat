perf tests
================
Win-Vector LLC
2/15/2018

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.0.2'

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
n_rows <- 500000
```

Get a base timing of a moderately large task.

``` r
d <- mkEx(n_rows = n_rows,
          n_cat_columns = 2,
          n_num_columns = 2,
          n_irrel_columns = 10,
          n_cat_levels_a = 5,
          n_cat_levels_b = 0)
yName <- "yC"
yTarget <- "Y"
varNames <- colnames(d)[grep("^var", colnames(d))]
system.time(
  tplan <- 
    vtreat::mkCrossFrameCExperiment(
      d, 
      varNames, 
      yName, 
      yTarget,
      parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  12.238   1.637  64.085

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  1.0e-06|  0.4112751| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  1.0e-07|  0.8048765| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  1.0e-07|  0.8061192| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  2.5e-06|  0.1859667| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  4.0e-07|  0.6160177| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  2.0e-07|  0.7315422| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  5.0e-07|  0.5493606| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  0.0e+00|  0.8724265| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  0.0e+00|  0.9556600| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  1.0e-07|  0.7956594| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  3.4e-06|  0.1251896| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  5.2e-06|  0.0582204| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  6.0e-07|  0.5091260| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  0.0e+00|  0.8726200| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  8.0e-07|  0.4661966| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  1.0e-07|  0.8320938| FALSE      |                  0| var\_num\_2 | clean |

Measure the effect of irrelevant columns.

``` r
d <- mkEx(n_rows = n_rows,
          n_cat_columns = 2,
          n_num_columns = 2,
          n_irrel_columns = 300,
          n_cat_levels_a = 5,
          n_cat_levels_b = 0)
yName <- "yC"
yTarget <- "Y"
varNames <- colnames(d)[grep("^var", colnames(d))]
system.time(
  tplan <- 
    vtreat::mkCrossFrameCExperiment(
      d, 
      varNames, 
      yName, 
      yTarget,
      parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  13.430   3.010  68.907

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  1.0e-07|  0.8013785| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  2.2e-06|  0.2190508| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  1.0e-07|  0.8260429| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  0.0e+00|  0.9764722| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  2.2e-06|  0.2189264| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  6.1e-06|  0.0399115| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  0.0e+00|  0.9390311| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  3.3e-06|  0.1320445| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  2.0e-07|  0.7357801| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  2.0e-07|  0.7053845| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  0.0e+00|  0.8904021| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  6.0e-07|  0.5150741| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  0.0e+00|  0.9718014| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  9.0e-07|  0.4359623| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  1.0e-07|  0.8472534| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  5.0e-07|  0.5397267| FALSE      |                  0| var\_num\_2 | clean |

Measure the effect of more levels (both common and uncommon).

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
system.time(
  tplan <- 
    vtreat::mkCrossFrameCExperiment(
      d, 
      varNames, 
      yName, 
      yTarget,
      parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  32.666   2.402 139.161

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                        | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1  | TRUE     |  0.0e+00|  0.9162907| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_10 | TRUE     |  4.0e-07|  0.5798830| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2  | TRUE     |  2.0e-07|  0.7325213| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3  | TRUE     |  3.0e-07|  0.6535017| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4  | TRUE     |  1.0e-07|  0.8433908| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5  | TRUE     |  3.0e-07|  0.6463082| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_6  | TRUE     |  0.0e+00|  0.8529945| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_7  | TRUE     |  3.0e-07|  0.6554978| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_8  | TRUE     |  7.0e-07|  0.4943275| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_9  | TRUE     |  5.0e-07|  0.5647106| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP              | TRUE     |  0.0e+00|  0.9976207| TRUE       |              49676| var\_cat\_1 | catP  |
| var\_cat\_1\_catB              | TRUE     |  5.0e-07|  0.5501446| TRUE       |              49676| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1  | TRUE     |  4.0e-07|  0.5970799| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_10 | TRUE     |  6.3e-06|  0.0370985| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2  | TRUE     |  1.0e-07|  0.7530640| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3  | TRUE     |  0.0e+00|  0.9139013| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4  | TRUE     |  8.0e-07|  0.4447940| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5  | TRUE     |  3.9e-06|  0.1004540| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_6  | TRUE     |  0.0e+00|  0.9355194| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_7  | TRUE     |  1.2e-06|  0.3550048| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_8  | TRUE     |  6.0e-07|  0.5193514| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_9  | TRUE     |  2.7e-06|  0.1738158| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP              | TRUE     |  1.4e-06|  0.3193110| TRUE       |              49660| var\_cat\_2 | catP  |
| var\_cat\_2\_catB              | TRUE     |  0.0e+00|  0.8788629| TRUE       |              49660| var\_cat\_2 | catB  |
| var\_num\_1\_clean             | TRUE     |  2.3e-06|  0.2086929| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean             | TRUE     |  1.0e-07|  0.8351640| FALSE      |                  0| var\_num\_2 | clean |

See if it is the indicators.

Measure the effect of more levels (both common and uncommon).

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
system.time(
  tplan <- 
    vtreat::mkCrossFrameCExperiment(
      d, 
      varNames, 
      yName, 
      yTarget,
      minFraction = 2.0,
      parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  33.751   2.390  80.806

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName            | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP  | TRUE     |  7.8e-06|  0.0201505| TRUE       |              49698| var\_cat\_1 | catP  |
| var\_cat\_1\_catB  | TRUE     |  7.0e-07|  0.4897245| TRUE       |              49698| var\_cat\_1 | catB  |
| var\_cat\_2\_catP  | TRUE     |  1.1e-06|  0.3746776| TRUE       |              49673| var\_cat\_2 | catP  |
| var\_cat\_2\_catB  | TRUE     |  7.5e-06|  0.0226816| TRUE       |              49673| var\_cat\_2 | catB  |
| var\_num\_1\_clean | TRUE     |  9.0e-07|  0.4406701| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean | TRUE     |  1.0e-07|  0.8112024| FALSE      |                  0| var\_num\_2 | clean |

``` r
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- NULL
}
```
