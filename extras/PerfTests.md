perf tests
================
Win-Vector LLC
2/15/2018

``` r
library("vtreat")

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

ncores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(ncores)
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
    ##  12.646   1.693  66.073

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  3.5e-06|  0.1184941| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  1.3e-06|  0.3489834| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  4.0e-07|  0.6002284| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  1.0e-07|  0.7721816| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  7.4e-06|  0.0235845| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  8.0e-07|  0.4496472| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.4e-06|  0.3181307| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  5.0e-07|  0.5401118| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  5.0e-07|  0.5543446| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  1.5e-06|  0.3035759| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  2.0e-06|  0.2371430| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  2.7e-06|  0.1744633| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  1.0e-07|  0.8296674| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  6.0e-07|  0.5060664| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  5.0e-07|  0.5480537| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  2.0e-07|  0.6857887| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  13.268   3.024  66.795

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  3.1e-06|  0.1422848| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  0.0e+00|  0.9134043| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  2.5e-06|  0.1903770| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  7.0e-07|  0.5014171| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  6.0e-07|  0.5314905| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP             | TRUE     |  2.4e-06|  0.2009637| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  4.0e-07|  0.5792881| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  6.0e-07|  0.5062554| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  0.0e+00|  0.8594026| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  8.6e-06|  0.0145220| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  8.4e-06|  0.0160443| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  4.0e-07|  0.6015339| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP             | TRUE     |  1.3e-06|  0.3499355| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  1.5e-06|  0.3052102| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  7.1e-06|  0.0269835| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  5.0e-07|  0.5686294| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  31.897   2.322 129.107

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                        | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_lev\_x.lev\_a\_1  | TRUE     |  1.1e-06|  0.3827034| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_10 | TRUE     |  9.0e-07|  0.4191885| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2  | TRUE     |  2.7e-06|  0.1724857| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3  | TRUE     |  1.3e-06|  0.3442932| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4  | TRUE     |  1.0e-07|  0.7618650| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5  | TRUE     |  9.0e-07|  0.4218736| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_6  | TRUE     |  3.0e-07|  0.6333071| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_7  | TRUE     |  9.0e-07|  0.4251998| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_8  | TRUE     |  8.0e-07|  0.4479263| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_9  | TRUE     |  0.0e+00|  0.8671992| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_catP              | TRUE     |  0.0e+00|  0.9142925| TRUE       |              49677| var\_cat\_1 | catP  |
| var\_cat\_1\_catB              | TRUE     |  6.0e-07|  0.5067359| TRUE       |              49677| var\_cat\_1 | catB  |
| var\_cat\_2\_lev\_x.lev\_a\_1  | TRUE     |  2.3e-06|  0.2051185| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_10 | TRUE     |  5.7e-06|  0.0467089| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2  | TRUE     |  1.9e-06|  0.2461854| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3  | TRUE     |  3.0e-07|  0.6231381| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4  | TRUE     |  0.0e+00|  0.8879168| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5  | TRUE     |  1.0e-06|  0.3945299| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_6  | TRUE     |  0.0e+00|  0.9361832| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_7  | TRUE     |  1.0e-07|  0.7960926| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_8  | TRUE     |  3.5e-06|  0.1213795| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_9  | TRUE     |  1.8e-06|  0.2632273| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_catP              | TRUE     |  1.9e-06|  0.2497871| TRUE       |              49692| var\_cat\_2 | catP  |
| var\_cat\_2\_catB              | TRUE     |  2.5e-06|  0.1882758| TRUE       |              49692| var\_cat\_2 | catB  |
| var\_num\_1\_clean             | TRUE     |  1.0e-06|  0.4101674| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean             | TRUE     |  9.0e-07|  0.4172523| FALSE      |                  0| var\_num\_2 | clean |

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
    ##  30.756   2.182  76.184

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName            | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP  | TRUE     |  1.0e-07|  0.7974485| TRUE       |              49672| var\_cat\_1 | catP  |
| var\_cat\_1\_catB  | TRUE     |  2.0e-07|  0.6923148| TRUE       |              49672| var\_cat\_1 | catB  |
| var\_cat\_2\_catP  | TRUE     |  4.0e-07|  0.6127074| TRUE       |              49659| var\_cat\_2 | catP  |
| var\_cat\_2\_catB  | TRUE     |  1.0e-06|  0.4031716| TRUE       |              49659| var\_cat\_2 | catB  |
| var\_num\_1\_clean | TRUE     |  1.5e-06|  0.3116733| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean | TRUE     |  1.2e-06|  0.3649808| FALSE      |                  0| var\_num\_2 | clean |

``` r
parallel::stopCluster(parallelCluster)
```
