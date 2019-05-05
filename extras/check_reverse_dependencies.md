check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "vtreat"
packageVersion(package)
```

    ## [1] '1.4.0'

``` r
date()
```

    ## [1] "Sun May  5 07:58:56 2019"

``` r
parallelCluster <- NULL
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
#ncores <- parallel::detectCores()
#parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/vtreat/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpjerfLy"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)
```

    ## Error: No dependencies for vtreat

``` r
mk_fn <- function(package, directory) {
  force(package)
  force(directory)
  function(i) {
    library("prrd")
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of vtreat had 0 successes, 0 failures, and 0 skipped packages.

    ## Warning in min(starttime): no non-missing arguments to min; returning Inf

    ## Warning in max(endtime): no non-missing arguments to max; returning -Inf

    ## Error in as.POSIXct.numeric(time1): 'origin' must be supplied

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
