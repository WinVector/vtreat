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

    ## [1] "Sun Jun 24 17:12:27 2018"

``` r
system.time(
  ctpc <- mkCrossFrameCExperiment(d, vars, "yC", "YES",
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Sun Jun 24 17:12:28 2018"
    ## [1] " start cross frame work Sun Jun 24 17:17:23 2018"
    ## [1] " vtreat::mkCrossFrameCExperiment done Sun Jun 24 17:19:32 2018"

    ##    user  system elapsed 
    ## 163.805  17.895 424.134

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:19:32 2018"

``` r
system.time(
  tpc <- designTreatmentsC(d, vars, "yC", "YES",
                           parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Sun Jun 24 17:19:32 2018"
    ## [1] "designing treatments Sun Jun 24 17:19:32 2018"
    ## [1] " have initial level statistics Sun Jun 24 17:19:43 2018"
    ## [1] " scoring treatments Sun Jun 24 17:20:23 2018"
    ## [1] "have treatment plan Sun Jun 24 17:22:00 2018"
    ## [1] "rescoring complex variables Sun Jun 24 17:22:00 2018"
    ## [1] "done rescoring complex variables Sun Jun 24 17:24:20 2018"

    ##    user  system elapsed 
    ##  98.206  11.258 288.445

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:24:20 2018"

``` r
system.time(
  tpc <- designTreatmentsC(d, vars, "yC", "YES")
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Sun Jun 24 17:24:21 2018"
    ## [1] "designing treatments Sun Jun 24 17:24:21 2018"
    ## [1] " have initial level statistics Sun Jun 24 17:24:33 2018"
    ## [1] "design var group Sun Jun 24 17:24:33 2018"
    ## [1] "design var nv1 Sun Jun 24 17:24:33 2018"
    ## [1] "design var nv2 Sun Jun 24 17:24:34 2018"
    ## [1] "design var nv3 Sun Jun 24 17:24:35 2018"
    ## [1] "design var nv4 Sun Jun 24 17:24:35 2018"
    ## [1] "design var nv5 Sun Jun 24 17:24:36 2018"
    ## [1] "design var nv6 Sun Jun 24 17:24:36 2018"
    ## [1] "design var nv7 Sun Jun 24 17:24:37 2018"
    ## [1] "design var nv8 Sun Jun 24 17:24:38 2018"
    ## [1] "design var nv9 Sun Jun 24 17:24:38 2018"
    ## [1] "design var nv10 Sun Jun 24 17:24:39 2018"
    ## [1] "design var nv11 Sun Jun 24 17:24:39 2018"
    ## [1] "design var nv12 Sun Jun 24 17:24:40 2018"
    ## [1] "design var nv13 Sun Jun 24 17:24:41 2018"
    ## [1] "design var nv14 Sun Jun 24 17:24:41 2018"
    ## [1] "design var nv15 Sun Jun 24 17:24:42 2018"
    ## [1] "design var cv1 Sun Jun 24 17:24:42 2018"
    ## [1] "design var cv2 Sun Jun 24 17:24:43 2018"
    ## [1] "design var cv3 Sun Jun 24 17:24:47 2018"
    ## [1] "design var cv4 Sun Jun 24 17:24:49 2018"
    ## [1] "design var cv5 Sun Jun 24 17:24:50 2018"
    ## [1] "design var cv6 Sun Jun 24 17:24:54 2018"
    ## [1] "design var cv7 Sun Jun 24 17:24:56 2018"
    ## [1] "design var cv8 Sun Jun 24 17:24:57 2018"
    ## [1] "design var cv9 Sun Jun 24 17:25:01 2018"
    ## [1] "design var cv10 Sun Jun 24 17:25:03 2018"
    ## [1] "design var cv11 Sun Jun 24 17:25:04 2018"
    ## [1] "design var cv12 Sun Jun 24 17:25:08 2018"
    ## [1] "design var cv13 Sun Jun 24 17:25:10 2018"
    ## [1] "design var cv14 Sun Jun 24 17:25:11 2018"
    ## [1] "design var cv15 Sun Jun 24 17:25:15 2018"
    ## [1] " scoring treatments Sun Jun 24 17:25:28 2018"
    ## [1] "have treatment plan Sun Jun 24 17:28:27 2018"
    ## [1] "rescoring complex variables Sun Jun 24 17:28:27 2018"
    ## [1] "done rescoring complex variables Sun Jun 24 17:31:27 2018"

    ##    user  system elapsed 
    ## 320.875  99.621 425.773

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:31:27 2018"

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:31:27 2018"

``` r
system.time(
  ctpn <- mkCrossFrameNExperiment(d, vars, "yN", 
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Sun Jun 24 17:31:27 2018"
    ## [1] " start cross frame work Sun Jun 24 17:35:27 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Sun Jun 24 17:38:37 2018"

    ##    user  system elapsed 
    ## 169.418  21.497 430.112

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:38:37 2018"

``` r
system.time(
  tpn <- designTreatmentsN(d, vars, "yN",
                           parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Sun Jun 24 17:38:38 2018"
    ## [1] "designing treatments Sun Jun 24 17:38:38 2018"
    ## [1] " have initial level statistics Sun Jun 24 17:38:48 2018"
    ## [1] " scoring treatments Sun Jun 24 17:39:34 2018"
    ## [1] "have treatment plan Sun Jun 24 17:40:03 2018"
    ## [1] "rescoring complex variables Sun Jun 24 17:40:03 2018"
    ## [1] "done rescoring complex variables Sun Jun 24 17:42:24 2018"

    ##    user  system elapsed 
    ##  89.613  10.397 225.921

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:42:24 2018"

``` r
system.time(
  tpn <- designTreatmentsN(d, vars, "yN")
)
```

    ## [1] "vtreat 1.2.1 inspecting inputs Sun Jun 24 17:42:24 2018"
    ## [1] "designing treatments Sun Jun 24 17:42:24 2018"
    ## [1] " have initial level statistics Sun Jun 24 17:42:34 2018"
    ## [1] "design var group Sun Jun 24 17:42:34 2018"
    ## [1] "design var nv1 Sun Jun 24 17:42:34 2018"
    ## [1] "design var nv2 Sun Jun 24 17:42:35 2018"
    ## [1] "design var nv3 Sun Jun 24 17:42:35 2018"
    ## [1] "design var nv4 Sun Jun 24 17:42:36 2018"
    ## [1] "design var nv5 Sun Jun 24 17:42:37 2018"
    ## [1] "design var nv6 Sun Jun 24 17:42:37 2018"
    ## [1] "design var nv7 Sun Jun 24 17:42:38 2018"
    ## [1] "design var nv8 Sun Jun 24 17:42:39 2018"
    ## [1] "design var nv9 Sun Jun 24 17:42:39 2018"
    ## [1] "design var nv10 Sun Jun 24 17:42:40 2018"
    ## [1] "design var nv11 Sun Jun 24 17:42:40 2018"
    ## [1] "design var nv12 Sun Jun 24 17:42:41 2018"
    ## [1] "design var nv13 Sun Jun 24 17:42:42 2018"
    ## [1] "design var nv14 Sun Jun 24 17:42:42 2018"
    ## [1] "design var nv15 Sun Jun 24 17:42:43 2018"
    ## [1] "design var cv1 Sun Jun 24 17:42:43 2018"
    ## [1] "design var cv2 Sun Jun 24 17:42:45 2018"
    ## [1] "design var cv3 Sun Jun 24 17:42:51 2018"
    ## [1] "design var cv4 Sun Jun 24 17:42:54 2018"
    ## [1] "design var cv5 Sun Jun 24 17:42:56 2018"
    ## [1] "design var cv6 Sun Jun 24 17:43:02 2018"
    ## [1] "design var cv7 Sun Jun 24 17:43:04 2018"
    ## [1] "design var cv8 Sun Jun 24 17:43:06 2018"
    ## [1] "design var cv9 Sun Jun 24 17:43:12 2018"
    ## [1] "design var cv10 Sun Jun 24 17:43:14 2018"
    ## [1] "design var cv11 Sun Jun 24 17:43:16 2018"
    ## [1] "design var cv12 Sun Jun 24 17:43:22 2018"
    ## [1] "design var cv13 Sun Jun 24 17:43:25 2018"
    ## [1] "design var cv14 Sun Jun 24 17:43:26 2018"
    ## [1] "design var cv15 Sun Jun 24 17:43:32 2018"
    ## [1] " scoring treatments Sun Jun 24 17:43:45 2018"
    ## [1] "have treatment plan Sun Jun 24 17:44:27 2018"
    ## [1] "rescoring complex variables Sun Jun 24 17:44:27 2018"
    ## [1] "done rescoring complex variables Sun Jun 24 17:47:30 2018"

    ##    user  system elapsed 
    ## 245.773  57.249 306.375

``` r
base::date()
```

    ## [1] "Sun Jun 24 17:47:30 2018"

Note a major cost is production of indicator columns (which leads to a large result). Setting `minFraction` to something larger (like `0.1` or `0.2`) can help there.

------------------------------------------------------------------------

Some timings of `prepare()`. Note: one uses `prepare()` on new data, for the variable design data you use `ctpc$crossFrame` to reduce nested model bias.

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id"))
```

    ##    user  system elapsed 
    ##   9.147   2.372  11.857

``` r
system.time(r <- prepare(ctpc$treatments, d, 
                         extracols = "id", 
                         parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##   6.590   1.355  14.662

``` r
rqplan <- as_rquery_plan(list(ctpc$treatments))

system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id"))
```

    ##    user  system elapsed 
    ##  39.094  11.131  40.838

``` r
system.time(r <- rqdatatable_prepare(rqplan, d, 
                                     extracols = "id",
                                     partition_column = "group",
                                     parallelCluster = parallelCluster))
```

    ##    user  system elapsed 
    ##  10.162   1.663  30.858

------------------------------------------------------------------------

Clean up.

``` r
parallel::stopCluster(parallelCluster)
rm(list = "parallelCluster")
```
