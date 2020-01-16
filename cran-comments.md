
## Test Results

### OSX

    R CMD check --as-cran vtreat_1.5.1.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘vtreat/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘vtreat’ version ‘1.5.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

     rhub::check_for_cran()
     677#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     678#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     679#> setting R_REMOTES_STANDALONE to true
     680#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     681#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     682#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     683#> * using log directory 'C:/Users/USERDzGwBgVLWk/vtreat.Rcheck'
     684#> * using R Under development (unstable) (2020-01-07 r77637)
     685#> * using platform: x86_64-w64-mingw32 (64-bit)
     686#> * using session charset: ISO8859-1
     687#> * using option '--as-cran'
     688#> * checking for file 'vtreat/DESCRIPTION' ... OK
     689#> * checking extension type ... Package
     690#> * this is package 'vtreat' version '1.5.1'
     691#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     692#> Maintainer: 'John Mount '
     734#> * checking sizes of PDF files under 'inst/doc' ... NOTE
     735#> Unable to find GhostScript executable to run checks on size reduction
     750#> Status: 1 NOTE
     GhostScript NOTE is a property of the test environment, not of the package.

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).
     
Mount and Zumel are not mis-spellings.

