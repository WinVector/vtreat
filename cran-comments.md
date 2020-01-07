
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.5.0.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.5.0’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

     rhub::check_for_cran()
     595#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     596#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     597#> setting R_REMOTES_STANDALONE to true
     598#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     599#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     600#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     601#> * using log directory 'C:/Users/USERHcCdOFfKjh/vtreat.Rcheck'
     602#> * using R Under development (unstable) (2019-11-08 r77393)
     603#> * using platform: x86_64-w64-mingw32 (64-bit)
     604#> * using session charset: ISO8859-1
     605#> * using option '--as-cran'
     606#> * checking for file 'vtreat/DESCRIPTION' ... OK
     607#> * checking extension type ... Package
     608#> * this is package 'vtreat' version '1.5.0'
     609#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     610#> Maintainer: 'John Mount '
     652#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     653#> Unable to find GhostScript executable to run checks on size reduction
     668#> Status: 1 NOTE
     GhostScript NOTE is a property of the test environment, not of the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

