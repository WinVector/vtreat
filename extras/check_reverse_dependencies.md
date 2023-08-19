check_reverse_dependencies
================

``` r
repos <- c(CRAN="https://cloud.r-project.org")
library("prrd")
orig_dir <- getwd()
# td <- tempdir()
td <- paste0(orig_dir, '/', 'revdep_tests')
package = "vtreat"
packageVersion(package)
```

    ## [1] '1.6.4'

``` r
date()
```

    ## [1] "Sat Aug 19 11:49:58 2023"

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
#if(ncores > 1) {
#  parallelCluster <- parallel::makeCluster(ncores)
#}


print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/vtreat/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/Users/johnmount/Documents/work/vtreat/extras/revdep_tests"

``` r
options(repos = repos)
jobsdfe <- enqueueJobs(package=package, directory=td)

print("checking:")
```

    ## [1] "checking:"

``` r
print(jobsdfe)
```

    ##   id         title status
    ## 1  1 crispRdesignR  READY

``` r
mk_fn <- function(package, directory, repos) {
  force(package)
  force(directory)
  force(repos)
  function(i) {
    library("prrd")
    options(repos = repos)
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td, repos=repos)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## ## Reverse depends check of vtreat 1.6.4 
    ## crispRdesignR_1.1.7 started at 2023-08-19 11:49:59 failure at 2023-08-19 11:50:01 (0/0/1)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of vtreat 1.6.4 had 0 successes, 1 failures, and 0 skipped packages. 
    ## Ran from 2023-08-19 11:49:59 to 2023-08-19 11:50:01 for 2 secs 
    ## Average of 2 secs relative to 1.824 secs using 1 runners
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
