

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.1.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.1’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Found the following (possibly) invalid URLs:
      URL: https://www.jstor.org/stable/2683780
        From: inst/doc/vtreat.html
        Status: 403
        Message: Forbidden
    ...
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    ...
    Status: 1 WARNING, 1 NOTE
    URLs https://www.jstor.org/stable/2683780 and https://www.r-pkg.org/badges/version/vtreat are correct and working.



### Windows

    rhub::check_for_cran()
    565#> * using R Under development (unstable) (2019-06-21 r76731)
    566#> * using platform: x86_64-w64-mingw32 (64-bit)
    567#> * using session charset: ISO8859-1
    568#> * using option '--as-cran'
    615#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    616#> Unable to find GhostScript executable to run checks on size reduction
    630#> Status: 1 NOTE
  
## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

