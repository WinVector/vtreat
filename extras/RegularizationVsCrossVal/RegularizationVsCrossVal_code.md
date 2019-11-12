Code for 'When Cross-Validation is More Powerful than Regularization'
================
Nina Zumel
10/31/2019

Code to run examples for the Win-Vector blog article "When Cross-Validation is More Powerful than Regularization".

Convenience functions.

``` r
# function to calculate the rmse
rmse = function(ypred, y) {
  resid = y - ypred
  sqrt(mean(resid^2))
}

# function to calculate R-squared
rsquared = function(ypred, y) {
  null_variance = sum((y - mean(y))^2)
  resid_variance = sum((y - ypred)^2)
  1 - (resid_variance/null_variance)
}

compare_models = function(predframe) {
  predictions = setdiff(colnames(predframe), "y")
  data.frame(# pred_type = predictions,
             rmse = vapply(predframe[,predictions, drop=FALSE], 
                           FUN = function(p) rmse(p, predframe$y),
                           numeric(1)),
             rsquared = vapply(predframe[,predictions,drop=FALSE],
                               FUN = function(p) rsquared(p, predframe$y),
                               numeric(1))
  )
}
```

A simple example
----------------

Set up the data and split into training and test sets.

``` r
set.seed(3453421)

Ndata = 500

nnoise = 100
nsig = 10

noise_levels = paste0("n_", sprintf('%02d', 1:nnoise))
signal_levels = paste0("s_", sprintf('%02d', 1:nsig))

sig_amps = 2*runif(1:nsig, min=-1, max=1)
names(sig_amps) = signal_levels
sig_amps = sig_amps - mean(sig_amps) # mean zero

x_s = sample(signal_levels, Ndata, replace=TRUE)
x_n = sample(noise_levels, Ndata, replace=TRUE)

y  = sig_amps[x_s]  + rnorm(Ndata) # a function of x_s but not x_n 
df = data.frame(x_s=x_s, x_n=x_n, y=y, stringsAsFactors=FALSE)

library(zeallot)
c(dtest, dtrain) %<-%  split(df, runif(Ndata) < 0.5)  # false comes first


head(dtrain)
```

    ##     x_s  x_n           y
    ## 2  s_10 n_72  0.34228110
    ## 3  s_01 n_09 -0.03805102
    ## 4  s_03 n_18 -0.92145960
    ## 9  s_08 n_43  1.77069352
    ## 10 s_08 n_17  0.51992928
    ## 11 s_01 n_78  1.04714355

``` r
# for later - a frame to hold the test set predictions
pframe = data.frame(y = dtest$y)
```

The naive way
-------------

For this simple example, you might try representing each variable as the expected value of `y - mean(y)` in the training data, conditioned on the variable's level. So the ith "coefficient" of the one-variable model would be given by:

*v*<sub>*i*</sub> = *E*\[*y*|*x* = *s*<sub>*i*</sub>\]−*E*\[*y*\]

Where *s*<sub>*i*</sub> is the *i*th level.

"Fit" the one-variable model for `x_s`.

``` r
# build the maps of expected values
library(rqdatatable)  # yes, you can use dplyr or base instead...
library(wrapr)

xs_means = dtrain %.>% 
  extend(., delta := y - mean(y)) %.>%
  project(.,
          meany := mean(y),
          coeff := mean(delta),
          groupby = 'x_s') %.>% 
  order_rows(.,
             'x_s') %.>%
  as.data.frame(.) 
 
xs_means
```

    ##     x_s      meany      coeff
    ## 1  s_01  0.7998263  0.8503282
    ## 2  s_02 -1.3815640 -1.3310621
    ## 3  s_03 -0.7928449 -0.7423430
    ## 4  s_04 -0.8245088 -0.7740069
    ## 5  s_05  0.7547054  0.8052073
    ## 6  s_06  0.1564710  0.2069728
    ## 7  s_07 -1.1747557 -1.1242539
    ## 8  s_08  1.3520153  1.4025171
    ## 9  s_09  1.5789785  1.6294804
    ## 10 s_10 -0.7313895 -0.6808876

"Fit" the one-variable model for `x_n` and treat or "prepare" the data (we are using terminology that is consistent with `vtreat`).

``` r
xn_means = dtrain %.>% 
  extend(., delta := y - mean(y)) %.>%
  project(.,
          meany := mean(delta),
          groupby = 'x_n') %.>% 
  order_rows(.,
             'x_n') %.>%
  as.data.frame(.) 

# the maps that convert categorical levels to numerical values
xs_map = with(xs_means, x_s := coeff)
xn_map = with(xn_means, x_n := meany)

prepare_manually = function(coefmap, xcol) {
  treated = coefmap[xcol]
  ifelse(is.na(treated), 0, treated)
}


# "prepare" the data
dtrain_treated = dtrain
dtrain_treated$vs = prepare_manually(xs_map, dtrain$x_s)
dtrain_treated$vn = prepare_manually(xn_map, dtrain$x_n)

head(dtrain_treated)
```

    ##     x_s  x_n           y         vs         vn
    ## 2  s_10 n_72  0.34228110 -0.6808876 0.64754957
    ## 3  s_01 n_09 -0.03805102  0.8503282 0.54991135
    ## 4  s_03 n_18 -0.92145960 -0.7423430 0.01923877
    ## 9  s_08 n_43  1.77069352  1.4025171 1.90394159
    ## 10 s_08 n_17  0.51992928  1.4025171 0.26448341
    ## 11 s_01 n_78  1.04714355  0.8503282 0.70342961

Now fit a linear model for `y` as a function of `vs` and `vn`.

``` r
model_raw = lm(y ~ vs + vn,
               data=dtrain_treated)
summary(model_raw)
```

    ## 
    ## Call:
    ## lm(formula = y ~ vs + vn, data = dtrain_treated)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.33068 -0.57106  0.00342  0.52488  2.25472 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.05050    0.05597  -0.902    0.368    
    ## vs           0.77259    0.05940  13.006   <2e-16 ***
    ## vn           0.61201    0.06906   8.862   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8761 on 242 degrees of freedom
    ## Multiple R-squared:  0.6382, Adjusted R-squared:  0.6352 
    ## F-statistic: 213.5 on 2 and 242 DF,  p-value: < 2.2e-16

Apply the naive model to the test data.

``` r
# apply to test data
dtest_treated = dtest  
dtest_treated$vs = prepare_manually(xs_map, dtest$x_s)
dtest_treated$vn = prepare_manually(xn_map, dtest$x_n)

pframe$ypred_naive = predict(model_raw, newdata=dtest_treated)

# look at the predictions on holdout data
compare_models(pframe) %.>% knitr::kable(.)
```

|              |      rmse|   rsquared|
|--------------|---------:|----------:|
| ypred\_naive |  1.303778|  0.2311538|

The right way: cross-validation
-------------------------------

Let's fit the correct nested model, using `vtreat`.

``` r
library(vtreat)
library(wrapr)
xframeResults = mkCrossFrameNExperiment(dtrain, qc(x_s, x_n), "y",
                                         codeRestriction = qc(catN), 
                                         verbose = FALSE)
# the plan uses the one-variable models to treat data
treatmentPlan = xframeResults$treatments
# the cross-frame
dtrain_treated = xframeResults$crossFrame

head(dtrain_treated)
```

    ##     x_s_catN   x_n_catN           y
    ## 1 -0.6337889 0.91241547  0.34228110
    ## 2  0.8342227 0.82874089 -0.03805102
    ## 3 -0.7020597 0.18198634 -0.92145960
    ## 4  1.3983175 1.99197404  1.77069352
    ## 5  1.3983175 0.11679580  0.51992928
    ## 6  0.8342227 0.06421659  1.04714355

``` r
variables = setdiff(colnames(dtrain_treated), "y")

model_X = lm(mk_formula("y", variables), 
             data=dtrain_treated)
summary(model_X)
```

    ## 
    ## Call:
    ## lm(formula = mk_formula("y", variables), data = dtrain_treated)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2157 -0.7343  0.0225  0.7483  2.9639 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.04169    0.06745  -0.618    0.537    
    ## x_s_catN     0.92968    0.06344  14.656   <2e-16 ***
    ## x_n_catN     0.10204    0.06654   1.533    0.126    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.055 on 242 degrees of freedom
    ## Multiple R-squared:  0.4753, Adjusted R-squared:  0.471 
    ## F-statistic: 109.6 on 2 and 242 DF,  p-value: < 2.2e-16

We can compare the performance of this model to the naive model on holdout data.

``` r
dtest_treated = prepare(treatmentPlan, dtest)
pframe$ypred_crossval = predict(model_X, newdata=dtest_treated)

compare_models(pframe)  %.>% knitr::kable(.)
```

|                 |      rmse|   rsquared|
|-----------------|---------:|----------:|
| ypred\_naive    |  1.303778|  0.2311538|
| ypred\_crossval |  1.093955|  0.4587089|

The correct model has a much smaller root-mean-squared error and a much larger R-squared than the naive model when applied to new data.

An attempted alternative: regularized models.
---------------------------------------------

For a one-variable model, L2-regularization is simply Laplace smoothing. Again, we'll represent each "coefficient" of the one-variable model as the Laplace smoothed value minus the grand mean.

*v*<sub>*i*</sub> = ∑<sub>*x*<sub>*j*</sub> = *s*<sub>*i*</sub></sub>*y*<sub>*i*</sub>/(count<sub>*i*</sub> + *λ*)−*E*\[*y*<sub>*i*</sub>\]

Where count<sub>*i*</sub> is the frequency of *s*<sub>*i*</sub> in the training data, and *λ* is the smoothing parameter (usually 1). If *λ* = 1 then the first term on the right is just adding one to the frequency of the level and then taking the "adjusted conditional mean" of `y`.

"Fit" a regularized model to `x_s`.

``` r
# build the coefficients
lambda = 1
 
xs_regmap = dtrain %.>% 
  extend(., grandmean = mean(y)) %.>%
  project(.,
          sum_y := sum(y),
          count_y := n(),
          grandmean := mean(grandmean), # pseudo-aggregator
          groupby = 'x_s') %.>% 
  extend(.,
          vs := (sum_y/(count_y  + lambda)) - grandmean
         ) %.>%
  order_rows(.,
             'x_s') %.>%
  as.data.frame(.) 

xs_regmap
```

    ##     x_s      sum_y count_y   grandmean         vs
    ## 1  s_01  20.795484      26 -0.05050187  0.8207050
    ## 2  s_02 -37.302227      27 -0.05050187 -1.2817205
    ## 3  s_03 -22.199656      28 -0.05050187 -0.7150035
    ## 4  s_04 -14.016649      17 -0.05050187 -0.7282009
    ## 5  s_05  19.622340      26 -0.05050187  0.7772552
    ## 6  s_06   3.129419      20 -0.05050187  0.1995218
    ## 7  s_07 -35.242672      30 -0.05050187 -1.0863585
    ## 8  s_08  36.504412      27 -0.05050187  1.3542309
    ## 9  s_09  33.158549      21 -0.05050187  1.5577086
    ## 10 s_10 -16.821957      23 -0.05050187 -0.6504130

"Fit" a regularized model to `x_m`. Apply the one variable models for `x_s` and `x_n` to the data.

``` r
xn_regmap = dtrain %.>% 
  extend(., grandmean = mean(y)) %.>%
  project(.,
          sum_y := sum(y),
          count_y := n(),
          grandmean := mean(grandmean), # pseudo-aggregator
          groupby = 'x_n') %.>% 
  extend(.,
          vn := (sum_y/(count_y  + lambda)) - grandmean
         ) %.>%
  order_rows(.,
             'x_n') %.>%
  as.data.frame(.) 

# the maps that convert categorical levels to numerical values
vs_map = xs_regmap$x_s :=  xs_regmap$vs
vn_map = xn_regmap$x_n :=  xn_regmap$vn

# "prepare" the data
dtrain_treated = dtrain
dtrain_treated$vs = prepare_manually(vs_map, dtrain$x_s)
dtrain_treated$vn = prepare_manually(vn_map, dtrain$x_n)

head(dtrain_treated)
```

    ##     x_s  x_n           y         vs         vn
    ## 2  s_10 n_72  0.34228110 -0.6504130 0.44853367
    ## 3  s_01 n_09 -0.03805102  0.8207050 0.42505898
    ## 4  s_03 n_18 -0.92145960 -0.7150035 0.02370493
    ## 9  s_08 n_43  1.77069352  1.3542309 1.28612835
    ## 10 s_08 n_17  0.51992928  1.3542309 0.21098803
    ## 11 s_01 n_78  1.04714355  0.8207050 0.61015422

Now fit the overall model:

``` r
model_reg = lm(y ~ vs + vn, data=dtrain_treated)
summary(model_reg)
```

    ## 
    ## Call:
    ## lm(formula = y ~ vs + vn, data = dtrain_treated)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.30354 -0.57688 -0.02224  0.56799  2.25723 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.06665    0.05637  -1.182    0.238    
    ## vs           0.81142    0.06203  13.082  < 2e-16 ***
    ## vn           0.85393    0.09905   8.621  8.8e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8819 on 242 degrees of freedom
    ## Multiple R-squared:  0.6334, Adjusted R-squared:  0.6304 
    ## F-statistic: 209.1 on 2 and 242 DF,  p-value: < 2.2e-16

Comparing the performance of the three models on holdout data.

``` r
# apply to test data
dtest_treated = dtest 
dtest_treated$vs = prepare_manually(vs_map, dtest$x_s)
dtest_treated$vn = prepare_manually(vn_map, dtest$x_n)

pframe$ypred_reg = predict(model_reg, newdata=dtest_treated)

# compare the predictions of each model
compare_models(pframe) %.>% knitr::kable(.)
```

|                 |      rmse|   rsquared|
|-----------------|---------:|----------:|
| ypred\_naive    |  1.303778|  0.2311538|
| ypred\_crossval |  1.093955|  0.4587089|
| ypred\_reg      |  1.267648|  0.2731756|
