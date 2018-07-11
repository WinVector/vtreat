
Fix failing CRAN checks.

## Test environments

    * OSX
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows
    * using R Under development (unstable) (2018-07-01 r74950)
    * using platform: x86_64-w64-mingw32 (64-bit)


## R CMD check --as-cran vtreat_1.2.3.tar.gz

    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.2.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    
    Warning is an artifact of local badge processing, and not a true dead URL.  Does not occur on Win-Builder.

 
## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)
     
Zumel is not a mis-spelling.

