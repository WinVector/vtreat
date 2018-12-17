


## Test Results

### OSX

    R CMD check --as-cran vtreat_1.3.3.tar.gz 
 
### Windows

    devtools::build_win()
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    Warning is spurious, link exists.

    rhub::check_for_cran()

## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
     
Zumel is not a mis-spelling.

