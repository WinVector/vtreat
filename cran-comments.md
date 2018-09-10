

## Test environments

    * OSX
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows

## Test Results

    R CMD check --as-cran vtreat_1.3.1.tar.gz 
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.r-pkg.org" 443

    Status: 1 WARNING

    Warning is an artifact of local badge processing, and not a true dead URL.  Does not occur on Win-Builder.

 
## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)

Zumel is not a mis-spelling.

