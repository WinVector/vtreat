

## Test Results



### OSX

    R CMD check --as-cran vtreat_1.4.4.tar.gz
    WARNING and NOTE are spurious, URLs https://www.jstor.org/stable/2683780 and https://www.jstor.org/stable/2683780 are correct and working.

### Windows

    rhub::check_for_cran()
    682#> * using R Under development (unstable) (2019-07-04 r76780)
    683#> * using platform: x86_64-w64-mingw32 (64-bit)
    684#> * using session charset: ISO8859-1
    685#> * using option '--as-cran'
    686#> * checking for file 'vtreat/DESCRIPTION' ... OK
    687#> * checking extension type ... Package
    688#> * this is package 'vtreat' version '1.4.3'
    689#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    690#> Maintainer: 'John Mount '
    732#> * checking sizes of PDF files under 'inst/doc' ... NOTE
    733#> Unable to find GhostScript executable to run checks on size reduction
    747#> Status: 1 NOTE
    NOTE is a property of the test environment, not the package.
 

## Downstream dependencies

No declared reverse dependencies ( https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md ).

     
Mount and Zumel are not mis-spellings.

