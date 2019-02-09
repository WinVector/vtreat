

## Test Results

### OSX

    R CMD check --as-cran vtreat_1.3.6.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.3.6’
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
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'vtreat/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'vtreat' version '1.3.6'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
    
    rhub::check_for_cran()
    663#> * using R Under development (unstable) (2019-01-26 r76018)
    664#> * using platform: x86_64-w64-mingw32 (64-bit)
    665#> * using session charset: ISO8859-1
    666#> * using option '--as-cran'
    667#> * checking for file 'vtreat/DESCRIPTION' ... OK
    668#> * checking extension type ... Package
    669#> * this is package 'vtreat' version '1.3.6'
    670#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    671#> Maintainer: 'John Mount '
    713#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    714#> Unable to find GhostScript executable to run checks on size reduction  
    727#> Status: 1 NOTE
    Note is spurious.
    
### Linux

    R CMD check --as-cran vtreat_1.3.6.tar.gz 

    WARNING is spurious, link is good.

 
## Downstream dependencies

No declared reverse dependencies 2019/02/09.


     
Zumel is not a mis-spelling.

