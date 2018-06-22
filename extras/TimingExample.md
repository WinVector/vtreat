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

set.seed(3252)
d <- data.frame(id = seq_len(n_row),
                yN = rnorm(n_row))
for(i in seq_len(n_num_var)) {
  vi <- paste0("nv", i)
  d[[vi]] <- rnorm(n_row)
  d$yN <- d$yN + num_effect_strength*d[[vi]]
}
for(i in seq_len(n_cat_var)) {
  vi <- paste0("cv", i)
  veci <- sample.int(n_cat_var_levels[1 + (i %% length(n_cat_var_levels))], 
                     n_row, 
                     replace = TRUE)
  d[[vi]] <- sample(paste0("lev_", veci))
  d$yN <- d$yN + cat_effect_strength*ifelse((veci %% 2) == 1, 1, -1)
}
d$yC <- ifelse(d$yN>0, "YES", "NO")
vars <- setdiff(colnames(d), c("id", "yN", "yC"))
```

Do the work (and time it).

``` r
base::date()
```

    ## [1] "Fri Jun 22 09:56:07 2018"

``` r
system.time(
  ctpc <- mkCrossFrameCExperiment(d, vars, "yC", "YES",
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Fri Jun 22 09:56:07 2018"
    ## [1] " start cross frame work Fri Jun 22 10:00:53 2018"
    ## [1] " vtreat::mkCrossFrameCExperiment done Fri Jun 22 10:03:23 2018"

    ##    user  system elapsed 
    ## 169.861  19.807 435.689

``` r
base::date()
```

    ## [1] "Fri Jun 22 10:03:23 2018"

``` r
base::date()
```

    ## [1] "Fri Jun 22 10:03:23 2018"

``` r
system.time(
  ctpn <- mkCrossFrameNExperiment(d, vars, "yN", 
                                  parallelCluster = parallelCluster)
)
```

    ## [1] "vtreat 1.2.1 start initial treatment design Fri Jun 22 10:03:24 2018"
    ## [1] " start cross frame work Fri Jun 22 10:07:46 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Fri Jun 22 10:10:31 2018"

    ##    user  system elapsed 
    ## 170.683  20.870 427.205

``` r
base::date()
```

    ## [1] "Fri Jun 22 10:10:31 2018"

Note a major cost is production of indicator columns (which leads to a large result). Setting `minFraction` to something larger (like `0.1` or `0.2`) can help there.

Clean up.

``` r
parallel::stopCluster(parallelCluster)
rm(list = "parallelCluster")
```
