


## Test Results



### OSX

    R CMD check --as-cran vtreat_1.3.4.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    Warning is spurious, link exists.

 
### Windows

    rhub::check_for_cran()
    635#> * using log directory 'C:/Users/USERPLHyKxUmFj/vtreat.Rcheck'
    636#> * using R Under development (unstable) (2018-12-26 r75909)
    637#> * using platform: x86_64-w64-mingw32 (64-bit)
    638#> * using session charset: ISO8859-1
    639#> * using option '--as-cran'
    640#> * checking for file 'vtreat/DESCRIPTION' ... OK
    641#> * checking extension type ... Package
    642#> * this is package 'vtreat' version '1.3.4'
    643#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    644#> Maintainer: 'John Mount '
    686#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    687#> Unable to find GhostScript executable to run checks on size reduction
    700#> Status: 1 NOTE
 
## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
     
Zumel is not a mis-spelling.

