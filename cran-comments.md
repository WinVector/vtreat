
## Test environments
* local OS X install x86_64-apple-darwin13.4.0 (64-bit)
* R version 3.3.1 
* win-builder (devel and release)

## R CMD check --as-cran vtreat_0.5.28.tar.gz
* using R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
* using platform: x86_64-apple-darwin14.5.0 (64-bit)

R CMD check results
0 errors | 0 warnings | 1 notes

 * checking CRAN incoming feasibility ... NOTE
 Maintainer: ‘John Mount <jmount@win-vector.com>’

 The Title field should be in title case, current version then in title    case:
 ‘A Statistically Sound data.frame Processor/Conditioner’
 ‘A Statistically Sound Data.frame Processor/Conditioner’
 
Deviation from title case is for "data.frame" (to match R class name).

## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
