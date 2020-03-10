
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
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’

### Windows

    rhub::check_for_cran()
    582#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    583#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    584#> setting R_REMOTES_STANDALONE to true
    585#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    586#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    587#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    588#> * using log directory 'C:/Users/USERGKUapBOMCz/vtreat.Rcheck'
    589#> * using R Under development (unstable) (2020-03-08 r77917)
    590#> * using platform: x86_64-w64-mingw32 (64-bit)
    591#> * using session charset: ISO8859-1
    592#> * using option '--as-cran'
    593#> * checking for file 'vtreat/DESCRIPTION' ... OK
    594#> * checking extension type ... Package
    595#> * this is package 'vtreat' version '1.6.0'
    596#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    597#> Maintainer: 'John Mount '
    639#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    640#> Unable to find GhostScript executable to run checks on size reduction
    655#> Status: 1 NOTE
    Ghostscript note is a proprety of the check infrastructure, not the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).
     
Mount and Zumel are not mis-spellings.
