

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.6.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.6’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    rhub::check_for_cran()
    623#> * using R Under development (unstable) (2019-09-18 r77193)
    624#> * using platform: x86_64-w64-mingw32 (64-bit)
    625#> * using session charset: ISO8859-1
    626#> * using option '--as-cran'
    627#> * checking for file 'vtreat/DESCRIPTION' ... OK
    628#> * checking extension type ... Package
    629#> * this is package 'vtreat' version '1.4.6'
    630#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    631#> Maintainer: 'John Mount '
    673#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    674#> Unable to find GhostScript executable to run checks on size reduction
    689#> Status: 1 NOTE
    GhostScript NOTE is a property of the test environment, not the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

