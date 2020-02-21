
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.6.0.tar.gz
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.6.0’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Number of updates in past 6 months: 7
    Status: 1 NOTE

### Windows

    rhub::check_for_cran()
 715#> setting _R_CHECK_FORCE_SUGGESTS_ to false
 716#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
 717#> setting R_REMOTES_STANDALONE to true
 718#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
 719#> setting _R_CHECK_FORCE_SUGGESTS_ to true
 720#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
 721#> * using log directory 'C:/Users/USERtgqFjUHpvO/vtreat.Rcheck'
 722#> * using R Under development (unstable) (2020-01-22 r77697)
 723#> * using platform: x86_64-w64-mingw32 (64-bit)
 724#> * using session charset: ISO8859-1
 725#> * using option '--as-cran'
 726#> * checking for file 'vtreat/DESCRIPTION' ... OK
 727#> * checking extension type ... Package
 728#> * this is package 'vtreat' version '1.6.0'
 729#> * checking CRAN incoming feasibility ... NOTE
 730#> Maintainer: 'John Mount '
 731#> Number of updates in past 6 months: 7
 743#> * checking for future file timestamps ... NOTE
 744#> unable to verify current time
 774#> * checking sizes of PDF files under 'inst/doc' ... NOTE
 775#> Unable to find GhostScript executable to run checks on size reduction
 790#> Status: 3 NOTEs
 Ghostscript and time notes are a property of the testing facility, not of the package.

### Linux

    rhub::check_for_cran()


## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).
     
Mount and Zumel are not mis-spellings.

