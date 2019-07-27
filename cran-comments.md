

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.4.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.4.4’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Found the following (possibly) invalid URLs:
      URL: https://www.jstor.org/stable/2683780
        From: inst/doc/vtreat.html
        Status: 403
        Message: Forbidden
    * checking top-level files ... WARNING
    Conversion of ‘README.md’ failed:
    pandoc: Could not fetch https://www.r-pkg.org/badges/version/vtreat
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Status: 1 WARNING, 1 NOTE
    WARNING and NOTE are spurious, URLs https://www.jstor.org/stable/2683780 and https://www.jstor.org/stable/2683780 are correct and working.

### Windows

    rhub::check_for_cran()
    Build ID:	vtreat_1.4.4.tar.gz-d878c7bb020e4c0abb135d9e32ceee14
    Platform:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    NOTES:
    * checking sizes of PDF files under 'inst/doc' ... NOTE
    Unable to find GhostScript executable to run checks on size reduction
    GhostScript NOTE is a property of the test environment, not the package.
 

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

