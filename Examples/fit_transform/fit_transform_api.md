
Starting with version `1.4.8` [the `R` version of `vtreat`](https://github.com/WinVector/vtreat/) includes an additional `.fit_transform()` interface.  This is a back-port from the [`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat).

The idea is from [`sklearn`'s pipeline](https://scikit-learn.org/stable/modules/generated/sklearn.pipeline.Pipeline.html).  It works as follows.

We define mutable objects to manage the variable preparation.  This is standard for `Python`, but a bit unusual for `R`.  However, it has some notation advantages.

These objects define three primary methods:

  * `$fit()`
  * `$transform()`
  * `$fit_transform()`

They work as follows. 

  * `$fit()`: Takes training data as an argument and learns the correct data preparation plan from the data.  The plan is kept inside the object as a side-effect.  The object itself is returned, allowing [method chaining](https://en.wikipedia.org/wiki/Method_chaining).
  * `$transform()`: Uses the in-object stored treatment plan to treat new data (given as an argument).
  * `$fit_transform()`: Performs the cross-validated work required to avoid nested-model bias.  The nested model bias we are working to avoid is an over fit due to using data for data transform design, and then naively treating the same data using the transform for down-stream modeling. `$fit_transform()` in this case is not in fact a shorthand for `$fit()$transform()`, but in fact a different method that takes extra steps to make sure the fit and transform are jointly correct.

This corresponds to the classic `R` `vtreat` notations as follows:

  * `plan$fit(d)` ~ `plan <- designTreatments*(d)`
  * `plan$transform(d)` ~ `prepare(plan, d)`
  * `plan$fit_transform(d)` ~ `mkCrossFrame*Experiment(d)$crossFrame`

Both notation systems are good, the `R` one being more "R-like" (using the usual immutable objects) and the `.fit_transform()` one being more `Pythonic`.  We expect to teach and maintain both paradigms.

Examples of the modeling typical tasks in both notations can be found here:

 * Regression: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression_FP.md).
 * Binary Classification: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Classification/Classification_FP.md).
 * Unsupervised Coding: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Unsupervised/Unsupervised_FP.md).
 * Multinomial Classification: [`R` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample.md), [`fit_prepare()` notation](https://github.com/WinVector/vtreat/blob/master/Examples/Multinomial/MultinomialExample_FP.md).

