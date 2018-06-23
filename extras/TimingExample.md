TimingExample
================

Set up.

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.2.1'

``` r
packageVersion("data.table") # data.table needed for fast row binding in vtreat 1.2.0 and newer.
```

    ## [1] '1.11.4'

``` r
options('vtreat.use_data.table_binding' = TRUE) # vtreat 1.2.0 fails to set this in some cicrumstances

getOption('vtreat.use_data.table_binding', FALSE)
```

    ## [1] TRUE

``` r
ncores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(ncores)
parallelCluster
```

    ## socket cluster with 4 nodes on host 'localhost'

Make example.

``` r
n_row <- 500000
n_cat_var <- 15
n_cat_var_levels <- c(10, 100, 50000) # mix of cat sizes, smaller ones more likely to create indicators
cat_effect_strength <- 0.1
n_num_var <- 15 
num_effect_strength <- 0.1
na_rate <- 0.01

set.seed(3252)
d <- data.frame(id = seq_len(n_row),
                group = seq_len(n_row) %% 10,
                yN = rnorm(n_row))
for(i in seq_len(n_num_var)) {
  vi <- paste0("nv", i)
  d[[vi]] <- rnorm(n_row)
  d[[vi]][runif(n_row)<=na_rate] <- NA_real_
  d$yN <- d$yN + num_effect_strength*ifelse(is.na(d[[vi]]), 0.5, d[[vi]])
}
for(i in seq_len(n_cat_var)) {
  vi <- paste0("cv", i)
  veci <- sample.int(n_cat_var_levels[1 + (i %% length(n_cat_var_levels))], 
                     n_row, 
                     replace = TRUE)
  d[[vi]] <- sample(paste0("lev_", veci))
  d[[vi]][runif(n_row)<=na_rate] <- NA_character_
  d$yN <- d$yN + cat_effect_strength*ifelse(is.na(d[[vi]]), 0.5, ifelse((veci %% 2) == 1, 1, -1))
}
d$yC <- ifelse(d$yN>0, "YES", "NO")
vars <- setdiff(colnames(d), c("id", "yN", "yC"))
```

Do the work (and time it).

``` r
base::date()
```

    ## [1] "Sat Jun 23 09:20:40 2018"

``` r
system.time(
  ctpc <- mkCrossFrameCExperiment(d, vars, "yC", "YES",
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Sat Jun 23 09:20:40 2018"
    ## [1] " start cross frame work Sat Jun 23 09:25:55 2018"
    ## [1] " vtreat::mkCrossFrameCExperiment done Sat Jun 23 09:28:17 2018"

    ##    user  system elapsed 
    ## 169.798  18.749 456.343

``` r
base::date()
```

    ## [1] "Sat Jun 23 09:28:17 2018"

``` r
base::date()
```

    ## [1] "Sat Jun 23 09:28:17 2018"

``` r
system.time(
  ctpn <- mkCrossFrameNExperiment(d, vars, "yN", 
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Sat Jun 23 09:28:17 2018"
    ## [1] " start cross frame work Sat Jun 23 09:32:18 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Sat Jun 23 09:34:57 2018"

    ##    user  system elapsed 
    ## 163.248  17.687 399.940

``` r
base::date()
```

    ## [1] "Sat Jun 23 09:34:57 2018"

Note a major cost is production of indicator columns (which leads to a large result). Setting `minFraction` to something larger (like `0.1` or `0.2`) can help there.

------------------------------------------------------------------------

Some timings of `prepare()`. Note: one uses `prepare()` on new data, for the variable design data you use `ctpc$crossFrame` to reduce nested model bias.

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id"))
```

    ##    user  system elapsed 
    ##   9.461   2.465  12.143

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id", 
                         parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##   5.859   1.141  11.979

``` r
rqplan <- as_rquery_plan(list(ctpc$treatments))

system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id"))
```

    ##    user  system elapsed 
    ##  38.265   8.545  32.069

``` r
system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id",
                                     partition_column = "group",
                                     parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  10.144   1.637  30.776

------------------------------------------------------------------------

Clean up.

``` r
parallel::stopCluster(parallelCluster)
rm(list = "parallelCluster")
```
