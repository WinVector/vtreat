---
title: "The vtreat R package: a statistically sound data processor for predictive modeling"
tags:
  - R
  - data science
  - predictive modeling
  - classification
  - regression
  - data preparation
  - significance
  - dimensionality reduction
  - reproducible research
  - cross-validation
authors:
  - name: John Mount
    orcid: 0000-0002-3696-2012
    email: jmount@win-vector.com
    affiliation: 1
  - name: Nina Zumel
    orcid: 0000-0001-8831-0190
    email: nzumel@win-vector.com
    affiliation: 1
affiliations:
  - name: Win-Vector, LLC
    index: 1
date: 9 February 2018
bibliography: paper.bib
---

# Summary

When applying statistical methods or applying machine learning
techniques to real world data, there are common data issues that can cause modeling to
fail. The [`vtreat`](https://github.com/WinVector/vtreat/) package
(@vtreat) is an R data frame processor that prepares messy real world data
for predictive modeling in a reproducible and statistically sound manner.

The package's objective is to produce clean data frames that preserve
the original information, and are safe for model training and model
application.  [`Vtreat`](https://github.com/WinVector/vtreat/) does
this by collecting statistics from training data in order to produce a
*treatment plan*.  [`Vtreat`](https://github.com/WinVector/vtreat/)
then uses this treatment plan to process subsequent data frames prior
to both model training and model application.  The processed data
frame is guaranteed to be purely numeric, with no missing or `NaN`
values, and no string or categorical values.
[`Vtreat`](https://github.com/WinVector/vtreat/) serves as a powerful
alternative to R's native `model.matrix` construct. The goals of the
package differ from those of training harness systems such as `caret`
(@caret) and unsupervised ad-hoc processing systems such as `recipes`
(@recipes).


In particular vtreat emphasizes *safe but y-aware (supervised) pre-processing of
data* for predictive modeling tasks. It automates:

* Treatment of missing values through safe replacement plus indicator column.
* Explicit coding of categorical variable levels as indicator variables.
* Robust handling of novel categorical levels (values seen during test or application, but not seen during training). 
* Supervised re-coding of categorical variables with very large numbers of levels, using an approach similar to that described by @appliedmr.
* Cross validation to mitigate overfit and undesirable supervision bias.
* Optional significance-based and cross-validated variable selection.

[`Vtreat`](https://github.com/WinVector/vtreat/) is careful to
automate only domain-agnostic data cleaning steps that are to common
to many applications. This intentionally leaves domain-specific
processing to the researcher and their own appropriate tools.

The use of [`vtreat`](https://github.com/WinVector/vtreat/) avoids the
perils of ad-hoc data treatment, and provides a reproducible,
documented, and citable data treatment procedure.


For more details and further discussion, please see our
[expository article](https://arxiv.org/abs/1611.09477) @vtreatX and
the package [online documentation](https://winvector.github.io/vtreat/index.html).


# References
