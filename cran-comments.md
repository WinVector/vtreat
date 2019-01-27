

## Test Results

### OSX

    R CMD check --as-cran vtreat_1.3.5.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.5’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    WARNING is spurious, link is good.


### Windows

    devtools::build_win()
    
    rhub::check_for_cran()
    640#> * using R Under development (unstable) (2018-12-26 r75909)
    641#> * using platform: x86_64-w64-mingw32 (64-bit)
    642#> * using session charset: ISO8859-1
    643#> * using option '--as-cran'
    644#> * checking for file 'vtreat/DESCRIPTION' ... OK
    645#> * checking extension type ... Package
    646#> * this is package 'vtreat' version '1.3.5'
    647#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    648#> Maintainer: 'John Mount '
    690#> Unable to find GhostScript executable to run checks on size reduction
    693#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    704#> Status: 1 NOTE
    Note is spurious.
 
## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)

     
Zumel is not a mis-spelling.

