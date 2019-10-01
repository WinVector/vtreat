
## Test Results


### OSX

    R CMD check --as-cran vtreat_1.4.7.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.7’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    744#> * using R Under development (unstable) (2019-09-18 r77193)
    745#> * using platform: x86_64-w64-mingw32 (64-bit)
    746#> * using session charset: ISO8859-1
    747#> * using option '--as-cran'
    748#> * checking for file 'vtreat/DESCRIPTION' ... OK
    749#> * checking extension type ... Package
    750#> * this is package 'vtreat' version '1.4.7'
    751#> Maintainer: 'John Mount '
    794#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    795#> Unable to find GhostScript executable to run checks on size reduction
    810#> Status: 1 NOTE
    GhostScript NOTE is a property of the test environment, not of the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

