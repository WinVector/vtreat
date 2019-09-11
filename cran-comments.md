

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.5.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.5’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    595#> * using R Under development (unstable) (2019-08-30 r77101)
    596#> * using platform: x86_64-w64-mingw32 (64-bit)
    597#> * using session charset: ISO8859-1
    598#> * using option '--as-cran'
    599#> * checking for file 'vtreat/DESCRIPTION' ... OK
    600#> * checking extension type ... Package
    601#> * this is package 'vtreat' version '1.4.5'
    602#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    603#> Maintainer: 'John Mount '
    645#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    646#> Unable to find GhostScript executable to run checks on size reduction
    660#> * DONE
    661#> Status: 1 NOTE
    GhostScript NOTE is a property of the test environment, not the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

