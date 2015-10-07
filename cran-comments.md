## Test environments
* local OS X install, 3.2.2
* win-builder (devel and release)

## R CMD check --as-cran vtreat_0.5.19.tar.gz results
* using R version 3.2.2 (2015-08-14)
* using platform: x86_64-apple-darwin11.4.2 (64-bit)
There were no ERRORs or WARNINGs. 

There were three NOTEs and one WARNING:

* NOTE Maintainer: ‘John Mount <jmount@win-vector.com>’
* NOTE No repository set, so cyclic dependency check skipped
* WARNING ‘qpdf’ is needed for checks on size reduction of PDFs
* NOTE (possibly) invalid URLs: http://practicaldatascience.com/ URL: http://www.win-vector.com .  These URLs are valid (double checked) and the canonical forms for the sites in question.


## Downstream dependencies
I have also run R CMD check on downstream dependencies of 'vtreat'
