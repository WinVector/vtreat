

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.0.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.0’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Number of updates in past 6 months: 7
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING, 1 NOTE
    WARNING is spurious, link is good.


### Windows

    devtools::build_win()

## Downstream dependencies

No declared reverse dependencies 2019-05-01 ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Zumel is not a mis-spelling.

