Imputation (using fit\_transform notation)
================

Changing the missing value imputation in vtreat
-----------------------------------------------

Check all notations

### A simple data example

Here we create a simple data set where the inputs have missing values.

``` r
library(vtreat)
```

    ## Loading required package: wrapr

``` r
d = data.frame(
    "x" = c(0, 1, 1000, NA),
    "w" = c(3, 6, NA, 100),
    "y" = c(0, 0, 1, 1)
)

knitr::kable(d)
```

|     x|    w|    y|
|-----:|----:|----:|
|     0|    3|    0|
|     1|    6|    0|
|  1000|   NA|    1|
|    NA|  100|    1|

Some of the summary statistics of `d`. We're primarily interested in the inputs `x` and `w`.

``` r
summary(d)
```

    ##        x                w                y      
    ##  Min.   :   0.0   Min.   :  3.00   Min.   :0.0  
    ##  1st Qu.:   0.5   1st Qu.:  4.50   1st Qu.:0.0  
    ##  Median :   1.0   Median :  6.00   Median :0.5  
    ##  Mean   : 333.7   Mean   : 36.33   Mean   :0.5  
    ##  3rd Qu.: 500.5   3rd Qu.: 53.00   3rd Qu.:1.0  
    ##  Max.   :1000.0   Max.   :100.00   Max.   :1.0  
    ##  NA's   :1        NA's   :1

Convenience function

``` r
# NULL for global_val or map means "use default"

check_unsupervised = function(d, global_val, map) {
  newparams = unsupervised_parameters(list(missingness_imputation=global_val))
  
  transform = UnsupervisedTreatment(
    var_list = c('x','w'),
    cols_to_copy = 'y',
    params = newparams,
    imputation_map = map
  )
  
  # use the fit().prepare() path
  d_treated = fit(transform, d) %.>%
    prepare(., d)
  
  d_treated
  
}

check_classification = function(d, global_val, map, useFT=TRUE) {
  newparams = classification_parameters(
    list(
      missingness_imputation=global_val,
      check_for_duplicate_frames = FALSE # shut the warning up
    )
  )
  
  transform = BinomialOutcomeTreatment(
    var_list = c('x','w'),
    outcome_name = 'y',
    outcome_target = 1,
    params = newparams,
    imputation_map = map
  )
  
  if(useFT) {
    unpack[treatments = treatments] <-
      fit_prepare(transform, d)
    d_treated = prepare(treatments,d)
  } else {
    # use the fit().prepare() path
    d_treated = fit(transform, d) %.>%
      prepare(., d)
  }
  
  d_treated
  
}

check_regression = function(d, global_val, map, useFT=TRUE) {
  newparams = regression_parameters(
    list(
      missingness_imputation=global_val,
      check_for_duplicate_frames = FALSE # shut the warning up
    )
  )
  
  transform = NumericOutcomeTreatment(
    var_list = c('x','w'),
    outcome_name = 'y',
    params = newparams,
    imputation_map = map
  )
  
  if(useFT) {
    unpack[treatments = treatments] <-
      fit_prepare(transform, d)
    d_treated = prepare(treatments,d)
  } else {
    # use the fit().prepare() path
    d_treated = fit(transform, d) %.>%
      prepare(., d)
  }
  d_treated
}

equal_df = function(a, b) {
  isTRUE(all.equal(a, b))
}

check_all = function(d, global_val, map) {
  # unsupervised is the gold standard
  gold_standard = check_unsupervised(d, global_val, map)
  
  # classification
  c1 = check_classification(d, global_val, map)
  stopifnot(equal_df(c1, gold_standard))
  
  c2 = check_classification(d, global_val, map, useFT=FALSE)
  stopifnot(equal_df(c2, gold_standard))
  
  # regression
  r1 = check_regression(d, global_val, map)
  stopifnot(equal_df(r1, gold_standard))
  
  r2 = check_regression(d, global_val, map, useFT=FALSE)
  stopifnot(equal_df(r2, gold_standard))
 
  TRUE
}
```

### The default missing value imputation

``` r
global_imp = NULL
imp_map = NULL

check_all(d, global_imp, imp_map)
```

    ## [1] TRUE

### Change global to median

``` r
median2 <- function(x, wts) {
  median(x)
}

global_imp = median2
imp_map = NULL
check_all(d, global_imp, imp_map)
```

    ## [1] TRUE

### Change global to -1

``` r
global_imp = -1
imp_map = NULL
check_all(d, global_imp, imp_map)
```

    ## [1] TRUE

### Changing the imputation strategy per column

Here we use the maximum value to fill in the missing values for `x` and the value `0` to fill in the missing values for `w`.

``` r
max2 <- function(x, wts) {
  max(x)
}

global_imp = NULL
imp_map = list(
  x = max2,
  w = 0
)

check_all(d, global_imp, imp_map)
```

    ## [1] TRUE

### Partial mapping with non-default global

If we don't specify a column, `vtreat` looks at`missingness_imputation` (in this case, `-1`).

``` r
global_imp = -1
imp_map = list(
  x = max2
)

check_all(d, global_imp, imp_map)
```

    ## [1] TRUE

### Partial mapping with default global

``` r
global_imp = NULL

imp_map = list(
  x = max2
)

check_all(d, global_imp, imp_map)
```

    ## [1] TRUE
