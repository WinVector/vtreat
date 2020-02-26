Imputation
================

Changing the missing value imputation in vtreat
-----------------------------------------------

For this example, we will use the UnsupervisedTreatment, but the same parameters can be used with the other treatment plans as well.

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

### The default missing value imputation

By default, vtreat fills in missing values with the mean value of the column, and adds an advisory \*\_is\_bad column to mark the location of the original missing values.

``` r
treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE)
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|          x|  x\_isBAD|          w|  w\_isBAD|    y|
|----------:|---------:|----------:|---------:|----:|
|     0.0000|         0|    3.00000|         0|    0|
|     1.0000|         0|    6.00000|         0|    0|
|  1000.0000|         0|   36.33333|         1|    1|
|   333.6667|         1|  100.00000|         0|    1|

### Changing the imputation strategy

If you do not want to use the mean to fill in missing values, you can change the imputation function using the parameter missingness\_imputation. Here, we fill in missing values with the median.

``` r
median2 <- function(x, wts) {
  median(x)
}

treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE,
                                missingness_imputation = median2)
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|     x|  x\_isBAD|    w|  w\_isBAD|    y|
|-----:|---------:|----:|---------:|----:|
|     0|         0|    3|         0|    0|
|     1|         0|    6|         0|    0|
|  1000|         0|    6|         1|    1|
|     1|         1|  100|         0|    1|

You can also use a constant value instead of a function. Here we replace missing values with the value -1.

``` r
treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE,
                                missingness_imputation = -1)
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|     x|  x\_isBAD|    w|  w\_isBAD|    y|
|-----:|---------:|----:|---------:|----:|
|     0|         0|    3|         0|    0|
|     1|         0|    6|         0|    0|
|  1000|         0|   -1|         1|    1|
|    -1|         1|  100|         0|    1|

### Changing the imputation strategy per column

You can control the imputation strategy per column via the map imputation\_map. Any column not named in the imputation map will use the imputation strategy specified by the missingness\_imputation parameter (which is the mean by default).

Here we use the maximum value to fill in the missing values for `x` and the value `0` to fill in the missing values for `w`.

``` r
max2 <- function(x, wts) {
  max(x)
}

treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE,
                                imputation_map = list(
                                  x = max2,
                                  w = 0
                                ))
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|     x|  x\_isBAD|    w|  w\_isBAD|    y|
|-----:|---------:|----:|---------:|----:|
|     0|         0|    3|         0|    0|
|     1|         0|    6|         0|    0|
|  1000|         0|    0|         1|    1|
|  1000|         1|  100|         0|    1|

If we don't specify a column, `vtreat` looks at`missingness_imputation` (in this case, `-1`).

``` r
treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE,
                                missingness_imputation = -1,
                                imputation_map = list(
                                  x = max2
                                ))
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|     x|  x\_isBAD|    w|  w\_isBAD|    y|
|-----:|---------:|----:|---------:|----:|
|     0|         0|    3|         0|    0|
|     1|         0|    6|         0|    0|
|  1000|         0|   -1|         1|    1|
|  1000|         1|  100|         0|    1|

If `missingness_imputation` is not specified, vtreat uses a weighted mean.

``` r
treatments <- designTreatmentsZ(d, 
                                varlist = c('x', 'w'), 
                                verbose = FALSE,
                                imputation_map = list(
                                  x = max2
                                ))
d_treated <- prepare(treatments, 
                     d)
d_treated$y <- d$y
knitr::kable(d_treated)
```

|     x|  x\_isBAD|          w|  w\_isBAD|    y|
|-----:|---------:|----------:|---------:|----:|
|     0|         0|    3.00000|         0|    0|
|     1|         0|    6.00000|         0|    0|
|  1000|         0|   36.33333|         1|    1|
|  1000|         1|  100.00000|         0|    1|
