
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
    Ghostscript note is a proprety of the check infrastructure, not the package.

    devtools::check_win_devel()

## Downstream dependencies

Reverse dependency check:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
     
Mount and Zumel are not mis-spellings.
