
Starting with version `1.6.0` [the `R` version of `vtreat`](https://github.com/WinVector/vtreat/) exposes an additional *fit_prepare* interface, based on the API of  the [`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat).

The idea is from [`sklearn`'s pipeline](https://scikit-learn.org/stable/modules/generated/sklearn.pipeline.Pipeline.html).  It works as follows.

For each of the common modeling tasks, the user constructs the appropriate, uninitialized data treatment object (a "spec"):

* `BinomialOutcomeTreatment` (binary classification)
* `NumericOutcomeTreatment` (regression)
* `UnsupervisedTreatment` (unsupervised problems)
* `MultinomialOutcomeTreatment` (multiclass classification)

Each object defines three primary methods:

* `fit()`
* `prepare()`
* `fit_prepare()`

They work as follows. 

  * `fit()`: Takes a spec and training data and returns the correct data preparation plan ("treatment plan") from the data. 
  * `prepare()`: Takes a treatment plan and new data and returns new treated data. This is notationally identical to `vtreat's` existing `prepare()` function.
  * `fit_prepare()`: Takes a spec and training data and returns both a treatment plan and a treated data set suitable for fitting a downstream model.
  
`fit_prepare()` performs the cross-validated work required to avoid nested-model bias.  The nested model bias we are working to avoid is an over fit due to using data for data transform design, and then naively treating the same data using the transform for down-stream modeling. Note that (except in the unsupervised case) `fit_prepare(spec, d)` is **not** a shorthand for `fit(spec, d) %.>% prepare(., d)`, but in fact a different method hat takes extra steps to make sure the fit and treatment plan are jointly correct.
  
This corresponds to the classic `R` `vtreat` notations as follows:

  * `plan <- fit(*Treatment(), d)` ~ `plan <- designTreatments*(d)`
  * `prepare(plan, d)` ~ `prepare(plan, d)`
  * `fit_prepare(*Treatment(), d)` ~ `mkCrossFrame*Experiment(d)`
  
We introduced this notation into the `R` version of `vtreat` for consistency of notation, to take advantage of the excellent Scikit-learn paradigm, and to compensate for some unfortunate name choices during the early development of `vtreat` in `R`. Both notations have the same underlying implementation, and we expect to teach and maintain both paradigms.

Examples of the modeling typical tasks in both notations can be found here:

 * Regression: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression_FP.md).
 * Binary Classification: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FP.md).
 * Unsupervised Coding: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised_FP.md).
 * Multinomial Classification: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample_FP.md).

