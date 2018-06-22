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

    ## [1] "Fri Jun 22 10:59:41 2018"

``` r
system.time(
  ctpc <- mkCrossFrameCExperiment(d, vars, "yC", "YES",
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Fri Jun 22 10:59:41 2018"
    ## [1] " start cross frame work Fri Jun 22 11:05:25 2018"
    ## [1] " vtreat::mkCrossFrameCExperiment done Fri Jun 22 11:07:48 2018"

    ##    user  system elapsed 
    ## 175.220  19.602 487.105

``` r
base::date()
```

    ## [1] "Fri Jun 22 11:07:48 2018"

``` r
base::date()
```

    ## [1] "Fri Jun 22 11:07:48 2018"

``` r
system.time(
  ctpn <- mkCrossFrameNExperiment(d, vars, "yN", 
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Fri Jun 22 11:07:48 2018"
    ## [1] " start cross frame work Fri Jun 22 11:12:10 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Fri Jun 22 11:15:02 2018"

    ##    user  system elapsed 
    ## 172.080  21.138 433.745

``` r
base::date()
```

    ## [1] "Fri Jun 22 11:15:02 2018"

Note a major cost is production of indicator columns (which leads to a large result). Setting `minFraction` to something larger (like `0.1` or `0.2`) can help there.

Clean up.

``` r
parallel::stopCluster(parallelCluster)
rm(list = "parallelCluster")
```
