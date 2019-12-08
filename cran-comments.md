
## Test Results


### OSX

    R CMD check --as-cran vtreat_1.4.8.tar.gz 
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.8’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
     978#> * using R Under development (unstable) (2019-11-08 r77393)
     979#> * using platform: x86_64-w64-mingw32 (64-bit)
     980#> * using session charset: ISO8859-1
     981#> * using option '--as-cran'
     982#> * checking for file 'vtreat/DESCRIPTION' ... OK
     983#> * checking extension type ... Package
     984#> * this is package 'vtreat' version '1.4.8'
     985#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     986#> Maintainer: 'John Mount '
    1028#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    1029#> Unable to find GhostScript executable to run checks on size reduction
    1044#> Status: 1 NOTE
    GhostScript NOTE is a property of the test environment, not of the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

