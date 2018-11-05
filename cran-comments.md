


## Test Results

### OSX

    R CMD check --as-cran vtreat_1.3.2.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING
    Warning is an artifact of http redirects in badge processing, and is a spurious warning.  Does not occur on Win-Builder.

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-11-02 r75540)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'vtreat/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'vtreat' version '1.3.2'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    rhub::check_for_cran()
    601#> * using R Under development (unstable) (2018-09-27 r75377)
    602#> * using platform: x86_64-w64-mingw32 (64-bit)
    603#> * using session charset: ISO8859-1
    604#> * using option '--as-cran'
    605#> * checking for file 'vtreat/DESCRIPTION' ... OK
    606#> * checking extension type ... Package
    607#> * this is package 'vtreat' version '1.3.2'
    608#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    609#> Maintainer: 'John Mount '
    652#> Unable to find GhostScript executable to run checks on size reduction
    665#> Status: 1 NOTE
    NOTE is spurious warning about "Unable to find GhostScript executable to run checks on size reduction"
 
## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('vtreat')
     character(0)

Zumel is not a mis-spelling.

