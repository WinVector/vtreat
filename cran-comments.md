


## Test environments

    * Windows
    * using R Under development (unstable) (2018-07-01 r74950)
    * using platform: x86_64-w64-mingw32 (64-bit)

    * OSX
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

## R CMD check --as-cran vtreat_1.2.2.tar.gz

    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.2.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking package namespace information ... OK
    Status: 1 WARNING

    Warning is an artifact of pandoc conversion of README.md interfering with CRAN badge.
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    It does not occur on Win-check, and I am assume is just an artifact of my checking environment.

## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
     
Zumel is not a mis-spelling.

