
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.6.2.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/vtreat.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.6.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    rhub::check_for_cran()
    917#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    918#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    919#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    920#> setting R_REMOTES_STANDALONE to true
    921#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    922#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    923#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    924#> * using log directory 'C:/Users/USERrkFXHUvNPm/vtreat.Rcheck'
    925#> * using R Under development (unstable) (2020-10-09 r79317)
    926#> * using platform: x86_64-w64-mingw32 (64-bit)
    927#> * using session charset: ISO8859-1
    928#> * using option '--as-cran'
    929#> * checking for file 'vtreat/DESCRIPTION' ... OK
    930#> * checking extension type ... Package
    931#> * this is package 'vtreat' version '1.6.2'
    932#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    933#> Maintainer: 'John Mount '
    975#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    976#> Unable to find GhostScript executable to run checks on size reduction
    991#> Status: 1 NOTE
    Ghostscript note is a proprety of the check infrastructure, not the package.


    devtools::check_win_devel()
    * using R Under development (unstable) (2020-10-15 r79342)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'vtreat/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'vtreat' version '1.6.2'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK


## Downstream dependencies

Reverse dependency checks all successful:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
     
Mount and Zumel are not mis-spellings.
