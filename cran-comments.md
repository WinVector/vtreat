
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.6.1.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.6.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    rhub::check_for_cran()
    785#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    786#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    787#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    788#> setting R_REMOTES_STANDALONE to true
    789#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    790#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    791#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    792#> * using log directory 'C:/Users/USERtCDFqWifop/vtreat.Rcheck'
    793#> * using R Under development (unstable) (2020-07-05 r78784)
    794#> * using platform: x86_64-w64-mingw32 (64-bit)
    795#> * using session charset: ISO8859-1
    796#> * using option '--as-cran'
    797#> * checking for file 'vtreat/DESCRIPTION' ... OK
    798#> * checking extension type ... Package
    799#> * this is package 'vtreat' version '1.6.1'
    800#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    801#> Maintainer: 'John Mount '
    843#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    844#> Unable to find GhostScript executable to run checks on size reduction
    859#> Status: 1 NOTE
    Ghostscript note is a proprety of the check infrastructure, not the package.

    devtools::check_win_devel()

## Downstream dependencies

Reverse dependency checks all successful:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
     
Mount and Zumel are not mis-spellings.
