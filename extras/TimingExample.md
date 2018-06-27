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

    ## [1] "Tue Jun 26 18:52:12 2018"

``` r
system.time(
  ctpc <- mkCrossFrameCExperiment(d, vars, "yC", "YES",
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Tue Jun 26 18:52:12 2018"
    ## [1] " start cross frame work Tue Jun 26 18:56:55 2018"
    ## [1] " vtreat::mkCrossFrameCExperiment done Tue Jun 26 18:59:04 2018"

    ##    user  system elapsed 
    ## 160.561  17.151 411.563

``` r
base::date()
```

    ## [1] "Tue Jun 26 18:59:04 2018"

``` r
system.time(
  tpc <- designTreatmentsC(d, vars, "yC", "YES",
                           parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Tue Jun 26 18:59:04 2018"
    ## [1] "designing treatments Tue Jun 26 18:59:04 2018"
    ## [1] " have initial level statistics Tue Jun 26 18:59:15 2018"
    ## [1] " scoring treatments Tue Jun 26 18:59:55 2018"
    ## [1] "have treatment plan Tue Jun 26 19:01:41 2018"
    ## [1] "rescoring complex variables Tue Jun 26 19:01:41 2018"
    ## [1] "done rescoring complex variables Tue Jun 26 19:04:02 2018"

    ##    user  system elapsed 
    ##  98.654  11.852 297.673

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:04:02 2018"

``` r
system.time(
  tpc <- designTreatmentsC(d, vars, "yC", "YES")
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Tue Jun 26 19:04:02 2018"
    ## [1] "designing treatments Tue Jun 26 19:04:02 2018"
    ## [1] " have initial level statistics Tue Jun 26 19:04:14 2018"
    ## [1] " scoring treatments Tue Jun 26 19:05:08 2018"
    ## [1] "have treatment plan Tue Jun 26 19:08:06 2018"
    ## [1] "rescoring complex variables Tue Jun 26 19:08:06 2018"
    ## [1] "done rescoring complex variables Tue Jun 26 19:11:02 2018"

    ##    user  system elapsed 
    ## 315.208  99.395 419.641

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:11:02 2018"

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:11:02 2018"

``` r
system.time(
  ctpn <- mkCrossFrameNExperiment(d, vars, "yN", 
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Tue Jun 26 19:11:02 2018"
    ## [1] " start cross frame work Tue Jun 26 19:15:33 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Tue Jun 26 19:18:00 2018"

    ##    user  system elapsed 
    ## 161.481  20.691 417.946

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:18:00 2018"

``` r
system.time(
  tpn <- designTreatmentsN(d, vars, "yN",
                           parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Tue Jun 26 19:18:01 2018"
    ## [1] "designing treatments Tue Jun 26 19:18:01 2018"
    ## [1] " have initial level statistics Tue Jun 26 19:18:10 2018"
    ## [1] " scoring treatments Tue Jun 26 19:18:56 2018"
    ## [1] "have treatment plan Tue Jun 26 19:19:25 2018"
    ## [1] "rescoring complex variables Tue Jun 26 19:19:25 2018"
    ## [1] "done rescoring complex variables Tue Jun 26 19:21:48 2018"

    ##    user  system elapsed 
    ##  91.284  10.143 227.717

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:21:48 2018"

``` r
system.time(
  tpn <- designTreatmentsN(d, vars, "yN")
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Tue Jun 26 19:21:49 2018"
    ## [1] "designing treatments Tue Jun 26 19:21:49 2018"
    ## [1] " have initial level statistics Tue Jun 26 19:21:59 2018"
    ## [1] " scoring treatments Tue Jun 26 19:23:08 2018"
    ## [1] "have treatment plan Tue Jun 26 19:23:50 2018"
    ## [1] "rescoring complex variables Tue Jun 26 19:23:50 2018"
    ## [1] "done rescoring complex variables Tue Jun 26 19:26:50 2018"

    ##    user  system elapsed 
    ## 241.325  56.694 301.229

``` r
base::date()
```

    ## [1] "Tue Jun 26 19:26:50 2018"

Note a major cost is production of indicator columns (which leads to a large result). Setting `minFraction` to something larger (like `0.1` or `0.2`) can help there.

------------------------------------------------------------------------

Some timings of `prepare()`. Note: one uses `prepare()` on new data, for the variable design data you use `ctpc$crossFrame` to reduce nested model bias.

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id"))
```

    ##    user  system elapsed 
    ##   8.558   2.269  10.911

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id", 
                         parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##   5.957   1.140  11.477

``` r
rqplan <- as_rquery_plan(list(ctpc$treatments))

system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id"))
```

    ##    user  system elapsed 
    ##  37.740   7.852  30.413

``` r
system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id",
                                     partition_column = "group",
                                     parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##   9.878   1.507  29.063

``` r
system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id",
                                     non_join_mapping = TRUE))
```

    ##    user  system elapsed 
    ## 124.044   6.087 130.932

------------------------------------------------------------------------

Clean up.

``` r
parallel::stopCluster(parallelCluster)
rm(list = "parallelCluster")
```
