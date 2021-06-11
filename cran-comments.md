
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.6.3.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.6.3’
    * checking CRAN incoming feasibility ...^[ Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Linux

    rhub::check_for_cran()
    6009#> * using R Under development (unstable) (2021-06-10 r80480)
    6010#> * using platform: x86_64-pc-linux-gnu (64-bit)
    6011#> * using session charset: UTF-8
    6012#> * using option ‘--as-cran’
    6013#> * checking for file ‘vtreat/DESCRIPTION’ ... OK
    6014#> * checking extension type ... Package
    6015#> * this is package ‘vtreat’ version ‘1.6.3’
    6016#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    6017#> Maintainer: ‘John Mount ’
    6075#> Status: OK

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2021-06-07 r80458)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'vtreat/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'vtreat' version '1.6.3'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK

## Downstream dependencies

Reverse dependency checks all successful:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
     
Mount and Zumel are not mis-spellings.
