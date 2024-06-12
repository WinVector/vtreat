
## Test Results

tinytest::test_package('vtreat')
All ok, 327 results (26.5s)


R CMD check --as-cran vtreat_1.6.5.tar.gz
* using R version 4.3.0 (2023-04-21)
* using platform: x86_64-apple-darwin20 (64-bit)
1 Note (HTML notes)

    
devtools::check_win_devel()


## Downstream dependencies

Reverse checks no worse:
    https://github.com/WinVector/vtreat/blob/master/extras/check_reverse_dependencies.md
    crispRdesignR uncheckable as it depends on many non-CRAN packages such as 'Biostrings', 'GenomicRanges', 'BiocGenerics', 'IRanges', 'GenomeInfoDb', 'S4Vectors', 'rtracklayer'.
     
Mount and Zumel are not mis-spellings.
