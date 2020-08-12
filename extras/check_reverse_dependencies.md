check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "vtreat"
packageVersion(package)
```

    ## [1] '1.6.0'

``` r
date()
```

    ## [1] "Wed Aug 12 09:42:49 2020"

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

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//Rtmp2KiO48"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)

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

    ## ## Reverse depends check of vtreat 1.6.0 
    ## crispRdesignR_1.1.5 started at 2020-08-12 09:42:51 failure at 2020-08-12 09:42:54 (0/0/1)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of vtreat 1.6.0 had 0 successes, 1 failures, and 0 skipped packages. 
    ## Ran from 2020-08-12 09:42:51 to 2020-08-12 09:42:54 for 3 secs 
    ## Average of 3 secs relative to 2.886 secs using 1 runners
    ## 
    ## Failed packages:  crispRdesignR 
    ## 
    ## Skipped packages:   
    ## 
    ## None still working
    ## 
    ## None still scheduled

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
