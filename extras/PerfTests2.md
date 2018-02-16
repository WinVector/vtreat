perf tests
================
Win-Vector LLC
2/15/2018

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
    ##  19.499   2.802  72.815

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP             | TRUE     |  2.1e-06|  0.2271728| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.0e-07|  0.8040180| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_catP             | TRUE     |  0.0e+00|  0.9598669| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  2.0e-06|  0.2338736| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  6.0e-07|  0.5322812| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  4.4e-06|  0.0808018| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  4.0e-07|  0.5872282| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  2.1e-06|  0.2264627| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  2.9e-06|  0.1557756| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  0.0e+00|  0.9102993| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  1.0e-07|  0.8274104| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  1.8e-06|  0.2582663| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  2.0e-07|  0.7436009| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  8.0e-07|  0.4586753| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  2.1e-06|  0.2273941| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  2.0e-07|  0.7351147| FALSE      |                  0| var\_cat\_2 | lev   |

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
    ##  20.400   4.222  76.606

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP             | TRUE     |  2.0e-07|  0.6898053| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.0e-07|  0.7916080| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_catP             | TRUE     |  3.5e-06|  0.1207318| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  1.0e-07|  0.7539347| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  3.0e-07|  0.6400703| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  0.0e+00|  0.8570026| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  1.3e-06|  0.3448711| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  0.0e+00|  0.9460512| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  7.0e-07|  0.4804173| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  3.0e-07|  0.6315115| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  7.0e-06|  0.0280928| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  1.2e-06|  0.3519908| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  1.5e-06|  0.3143043| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  2.8e-06|  0.1649835| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  5.0e-07|  0.5540914| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  5.3e-06|  0.0563713| FALSE      |                  0| var\_cat\_2 | lev   |

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
    ##  43.383   4.160 133.414

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                        | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP              | TRUE     |  4.0e-07|  0.5810126| TRUE       |              49664| var\_cat\_1 | catP  |
| var\_cat\_1\_catB              | TRUE     |  5.4e-06|  0.0532892| TRUE       |              49664| var\_cat\_1 | catB  |
| var\_cat\_2\_catP              | TRUE     |  6.0e-07|  0.5275024| TRUE       |              49672| var\_cat\_2 | catP  |
| var\_cat\_2\_catB              | TRUE     |  8.0e-07|  0.4446452| TRUE       |              49672| var\_cat\_2 | catB  |
| var\_num\_1\_clean             | TRUE     |  3.7e-06|  0.1081629| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean             | TRUE     |  0.0e+00|  0.9939817| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1  | TRUE     |  5.0e-07|  0.5708932| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_10 | TRUE     |  1.0e-06|  0.4142195| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2  | TRUE     |  3.0e-07|  0.6240567| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3  | TRUE     |  6.0e-07|  0.5193472| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4  | TRUE     |  9.0e-07|  0.4254773| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5  | TRUE     |  1.2e-06|  0.3602458| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_6  | TRUE     |  9.0e-07|  0.4210809| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_7  | TRUE     |  6.0e-07|  0.5067995| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_8  | TRUE     |  7.0e-07|  0.5000717| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_9  | TRUE     |  2.7e-06|  0.1692593| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1  | TRUE     |  3.9e-06|  0.1015060| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_10 | TRUE     |  1.0e-07|  0.7844567| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2  | TRUE     |  0.0e+00|  0.8742633| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3  | TRUE     |  4.2e-06|  0.0890741| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4  | TRUE     |  8.0e-07|  0.4533200| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5  | TRUE     |  6.0e-07|  0.5357301| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_6  | TRUE     |  1.0e-07|  0.8311470| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_7  | TRUE     |  1.6e-06|  0.2954955| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_8  | TRUE     |  6.0e-07|  0.5309137| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_9  | TRUE     |  1.0e-07|  0.8192675| FALSE      |                  0| var\_cat\_2 | lev   |

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
    ##  30.714   2.187  71.061

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName            | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP  | TRUE     |  2.6e-06|  0.1832067| TRUE       |              49690| var\_cat\_1 | catP  |
| var\_cat\_1\_catB  | TRUE     |  2.0e-07|  0.6997332| TRUE       |              49690| var\_cat\_1 | catB  |
| var\_cat\_2\_catP  | TRUE     |  8.0e-07|  0.4491281| TRUE       |              49684| var\_cat\_2 | catP  |
| var\_cat\_2\_catB  | TRUE     |  5.0e-07|  0.5717610| TRUE       |              49684| var\_cat\_2 | catB  |
| var\_num\_1\_clean | TRUE     |  3.0e-07|  0.6253902| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean | TRUE     |  3.0e-07|  0.6437738| FALSE      |                  0| var\_num\_2 | clean |

``` r
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- NULL
}
```
