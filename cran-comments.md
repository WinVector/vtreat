
## Test environments
* local OS X install, 3.3.0
* win-builder (devel and release)

## R CMD check --as-cran vtreat_0.5.26.tar.gz
* using R version 3.3.0 (2016-05-03) 
* using platform: x86_64-apple-darwin14.5.0 (64-bit)

There were no ERRORs, or WARNINGSs.

There was one NOTE:

  * checking for unstated dependencies in vignettes ... NOTE
  '::' or ':::' import not declared from: ‘caret’
  'library' or 'require' calls not declared from:
    ‘caret’ ‘ggplot2’


Both of the above calls are guarded by requireNamespace calls are are there to
illustrate how a user can use the additional caret or ggplot2 packages. 
caret and ggplot2 are not use (even optionally) in the vtreat package, other
than being also present in some unit tests (again optional and to test
things for users using caret or data.table).  These packages are not in
suggests as the vtreat package does not use them or even optionally alter
its behavior if they are available.



Note_to_CRAN_maintainers
Maintainer: ‘John Mount <jmount@win-vector.com>’

## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
