perf tests
================
Win-Vector LLC
2/15/2018

``` r
library("vtreat")

useParallel <- FALSE

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
n_rows <- 100000
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
    ##  18.793   3.004  21.878

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |       rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|---------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  4.00e-07|  0.8186675| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  3.92e-05|  0.0197285| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  1.52e-05|  0.1468370| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  5.30e-06|  0.3928081| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  3.00e-07|  0.8361545| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  0.00e+00|  0.9744813| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  4.00e-07|  0.8169474| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  8.80e-06|  0.2698670| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  1.40e-06|  0.6578840| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  4.00e-07|  0.8130080| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  2.61e-05|  0.0571737| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  1.00e-07|  0.9074268| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  3.80e-06|  0.4679437| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  3.40e-06|  0.4905754| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  1.20e-05|  0.1968953| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  4.70e-06|  0.4212914| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  16.719   3.721  20.511

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |       rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|---------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  1.00e-07|  0.9139941| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  3.00e-07|  0.8264812| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  4.00e-07|  0.8058465| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  2.27e-05|  0.0760933| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  1.96e-05|  0.0991551| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  2.54e-05|  0.0603643| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.17e-05|  0.2030959| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  1.80e-06|  0.6150371| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  2.29e-05|  0.0751039| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  4.00e-06|  0.4576871| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  3.50e-06|  0.4867216| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  2.00e-07|  0.8739281| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  4.00e-06|  0.4541639| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  1.80e-06|  0.6222977| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  1.70e-06|  0.6262061| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  8.10e-06|  0.2895603| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  40.972   5.796  46.929

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                        | varMoves |       rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------------------|:---------|---------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1  | TRUE     |  2.10e-06|  0.5867832| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_10 | TRUE     |  5.40e-06|  0.3883052| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2  | TRUE     |  3.78e-05|  0.0220539| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3  | TRUE     |  1.58e-05|  0.1387308| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4  | TRUE     |  2.03e-05|  0.0935265| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5  | TRUE     |  4.00e-07|  0.8108051| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_6  | TRUE     |  7.70e-06|  0.3009286| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_7  | TRUE     |  2.50e-06|  0.5555287| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_8  | TRUE     |  4.20e-06|  0.4437054| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_9  | TRUE     |  1.80e-06|  0.6160210| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP              | TRUE     |  5.23e-05|  0.0071103| TRUE       |              31600| var\_cat\_1 | catP  |
| var\_cat\_1\_catB              | TRUE     |  0.00e+00|  0.9487024| TRUE       |              31600| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1  | TRUE     |  3.80e-06|  0.4690029| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_10 | TRUE     |  0.00e+00|  0.9970259| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2  | TRUE     |  1.11e-05|  0.2141750| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3  | TRUE     |  2.00e-06|  0.5975780| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4  | TRUE     |  1.80e-05|  0.1138243| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5  | TRUE     |  0.00e+00|  0.9600604| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_6  | TRUE     |  3.00e-06|  0.5213055| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_7  | TRUE     |  1.00e-06|  0.7030782| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_8  | TRUE     |  1.10e-06|  0.6989218| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_9  | TRUE     |  3.63e-05|  0.0249580| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP              | TRUE     |  6.90e-06|  0.3275645| TRUE       |              31682| var\_cat\_2 | catP  |
| var\_cat\_2\_catB              | TRUE     |  4.20e-06|  0.4473055| TRUE       |              31682| var\_cat\_2 | catB  |
| var\_num\_1\_clean             | TRUE     |  1.00e-07|  0.9074727| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean             | TRUE     |  0.00e+00|  0.9479890| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  26.113   2.030  28.248

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName            | varMoves |        rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------|:---------|----------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP  | TRUE     |  0.0000025|  0.5523080| TRUE       |              31587| var\_cat\_1 | catP  |
| var\_cat\_1\_catB  | TRUE     |  0.0001057|  0.0001297| TRUE       |              31587| var\_cat\_1 | catB  |
| var\_cat\_2\_catP  | TRUE     |  0.0000153|  0.1458198| TRUE       |              31582| var\_cat\_2 | catP  |
| var\_cat\_2\_catB  | TRUE     |  0.0000104|  0.2288463| TRUE       |              31582| var\_cat\_2 | catB  |
| var\_num\_1\_clean | TRUE     |  0.0000009|  0.7226074| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean | TRUE     |  0.0000054|  0.3850018| FALSE      |                  0| var\_num\_2 | clean |

``` r
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- NULL
}
```
