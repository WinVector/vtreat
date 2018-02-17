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
n_rows <- 2000000
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
    ## 168.960  18.400 349.123

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP             | TRUE     |  0.0e+00|  0.9009002| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.9e-06|  0.0229055| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_catP             | TRUE     |  0.0e+00|  0.7288237| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  1.4e-06|  0.0509270| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  2.8e-06|  0.0053804| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  0.0e+00|  0.8547693| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  0.0e+00|  0.8974030| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  0.0e+00|  0.7900162| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  3.0e-07|  0.3603173| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  3.5e-06|  0.0018322| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  1.2e-06|  0.0707593| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  0.0e+00|  0.9510616| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  8.0e-07|  0.1346614| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  5.0e-07|  0.2197869| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  1.3e-06|  0.0571191| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  1.8e-06|  0.0256345| FALSE      |                  0| var\_cat\_2 | lev   |

Measure the effect of irrelevant columns.

``` r
d <- mkEx(n_rows = n_rows,
          n_cat_columns = 2,
          n_num_columns = 2,
          n_irrel_columns = 100,
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
    ## 170.460  19.340 339.995

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                       | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP             | TRUE     |  0.0e+00|  0.9667085| TRUE       |                  4| var\_cat\_1 | catP  |
| var\_cat\_1\_catB             | TRUE     |  1.7e-06|  0.0302954| TRUE       |                  4| var\_cat\_1 | catB  |
| var\_cat\_2\_catP             | TRUE     |  1.0e-07|  0.6423609| TRUE       |                  4| var\_cat\_2 | catP  |
| var\_cat\_2\_catB             | TRUE     |  0.0e+00|  0.8919384| TRUE       |                  4| var\_cat\_2 | catB  |
| var\_num\_1\_clean            | TRUE     |  1.0e-07|  0.5618168| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean            | TRUE     |  0.0e+00|  0.7254961| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1 | TRUE     |  0.0e+00|  0.7633458| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2 | TRUE     |  1.0e-07|  0.7063396| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3 | TRUE     |  0.0e+00|  0.9856348| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4 | TRUE     |  1.4e-06|  0.0516303| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5 | TRUE     |  2.5e-06|  0.0082229| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1 | TRUE     |  3.0e-07|  0.3690146| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2 | TRUE     |  1.0e-07|  0.6876347| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3 | TRUE     |  1.0e-07|  0.7067748| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4 | TRUE     |  1.0e-07|  0.6948953| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5 | TRUE     |  6.0e-07|  0.1878351| FALSE      |                  0| var\_cat\_2 | lev   |

``` r
d <- NULL
tplan <- NULL
gc()
```

    ##           used (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  446145 23.9    1508222   80.6   2945748  157.4
    ## Vcells 2844488 21.8  573857663 4378.2 716997619 5470.3

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
    ## 228.568  25.516 445.929

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName                        | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP              | TRUE     |  1.0e-07|  0.5365186| TRUE       |              50009| var\_cat\_1 | catP  |
| var\_cat\_1\_catB              | TRUE     |  1.0e-07|  0.5412263| TRUE       |              50009| var\_cat\_1 | catB  |
| var\_cat\_2\_catP              | TRUE     |  9.0e-07|  0.1183884| TRUE       |              50009| var\_cat\_2 | catP  |
| var\_cat\_2\_catB              | TRUE     |  1.6e-06|  0.0360525| TRUE       |              50009| var\_cat\_2 | catB  |
| var\_num\_1\_clean             | TRUE     |  3.0e-07|  0.3435448| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean             | TRUE     |  2.0e-07|  0.4481928| FALSE      |                  0| var\_num\_2 | clean |
| var\_cat\_1\_lev\_x.lev\_a\_1  | TRUE     |  2.0e-07|  0.4506758| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_10 | TRUE     |  3.0e-07|  0.3302589| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_2  | TRUE     |  6.0e-07|  0.1804519| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_3  | TRUE     |  1.0e-07|  0.6931947| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_4  | TRUE     |  1.8e-06|  0.0264180| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_5  | TRUE     |  7.0e-07|  0.1785925| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_6  | TRUE     |  1.0e-07|  0.5360518| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_7  | TRUE     |  0.0e+00|  0.7648720| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_8  | TRUE     |  4.0e-07|  0.2906581| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_1\_lev\_x.lev\_a\_9  | TRUE     |  3.0e-07|  0.3594626| FALSE      |                  0| var\_cat\_1 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_1  | TRUE     |  0.0e+00|  0.7183708| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_10 | TRUE     |  2.0e-07|  0.4245337| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_2  | TRUE     |  1.0e-07|  0.6788411| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_3  | TRUE     |  0.0e+00|  0.8151242| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_4  | TRUE     |  1.0e-07|  0.6457424| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_5  | TRUE     |  2.0e-07|  0.5014217| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_6  | TRUE     |  1.0e-07|  0.5968353| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_7  | TRUE     |  1.4e-06|  0.0525395| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_8  | TRUE     |  1.0e-07|  0.5537246| FALSE      |                  0| var\_cat\_2 | lev   |
| var\_cat\_2\_lev\_x.lev\_a\_9  | TRUE     |  0.0e+00|  0.9167634| FALSE      |                  0| var\_cat\_2 | lev   |

``` r
d <- NULL
tplan <- NULL
gc()
```

    ##           used (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  446191 23.9    1761155   94.1   2945748  157.4
    ## Vcells 2845031 21.8  208192011 1588.4 716997619 5470.3

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
    ## 105.892   6.592 187.766

``` r
knitr::kable(tplan$treatments$scoreFrame)
```

| varName            | varMoves |      rsq|        sig| needsSplit |  extraModelDegrees| origName    | code  |
|:-------------------|:---------|--------:|----------:|:-----------|------------------:|:------------|:------|
| var\_cat\_1\_catP  | TRUE     |  1.0e-07|  0.5950522| TRUE       |              50009| var\_cat\_1 | catP  |
| var\_cat\_1\_catB  | TRUE     |  1.0e-06|  0.0888238| TRUE       |              50009| var\_cat\_1 | catB  |
| var\_cat\_2\_catP  | TRUE     |  0.0e+00|  0.8322227| TRUE       |              50009| var\_cat\_2 | catP  |
| var\_cat\_2\_catB  | TRUE     |  0.0e+00|  0.9061606| TRUE       |              50009| var\_cat\_2 | catB  |
| var\_num\_1\_clean | TRUE     |  0.0e+00|  0.8226784| FALSE      |                  0| var\_num\_1 | clean |
| var\_num\_2\_clean | TRUE     |  1.1e-06|  0.0757819| FALSE      |                  0| var\_num\_2 | clean |

``` r
d <- NULL
tplan <- NULL
gc()
```

    ##           used (Mb) gc trigger  (Mb)  max used   (Mb)
    ## Ncells  446193 23.9    1637824  87.5   2945748  157.4
    ## Vcells 2845234 21.8  122937441 938.0 716997619 5470.3

``` r
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- NULL
}
```
