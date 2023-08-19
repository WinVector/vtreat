
## Test Results


    R CMD check --as-cran vtreat_1.6.4.tar.gz
    * using R version 4.3.0 (2023-04-21)
    * using platform: x86_64-apple-darwin20 (64-bit)

    devtools::check_win_devel()
    * using R Under development (unstable) (2023-08-18 r84986 ucrt)
    * using platform: x86_64-w64-mingw32

    rhub::check_for_cran()
    skipped

## Downstream dependencies

Reverse checks no worse:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
    crispRdesignR uncheckable as it depends on many non-CRAN packages.
     
Mount and Zumel are not mis-spellings.
