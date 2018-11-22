Modeling Pipelines
================

Reusable modeling pipelines are a practical idea that gets re-developed
many times in many contexts.
[`wrapr`](https://github.com/WinVector/wrapr) supplies a particularly
powerful pipeline notation and as of version `1.7.1` pipeline re-use
system (notes
[here](https://winvector.github.io/wrapr/articles/Function_Objects.html)).
We will demonstrate this with the
[`vtreat`](https://github.com/WinVector/vtreat) data preparation system.

``` r
library("wrapr")
library("vtreat")
library("glmnet")
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-16

``` r
library("WVPlots")

# function to make practice data
mk_data <- function(nrows, n_var_cols, n_noise_cols) {
  d <- data.frame(y = rnorm(nrows))
  for(i in seq_len(n_var_cols)) {
    vari = paste0("var_", sprintf("%03g", i))
    d[[vari]] <- rnorm(nrows)
    d$y <- d$y + (2/n_var_cols)*d[[vari]]
    d[[vari]][d[[vari]]>abs(2*rnorm(nrows))] <- NA
    d[[vari]] <- rlnorm(1, meanlog=10, sdlog = 10)*d[[vari]]
  }
   for(i in seq_len(n_noise_cols)) {
    vari = paste0("noise_", sprintf("%03g", i))
    d[[vari]] <- rnorm(nrows)
    d[[vari]][d[[vari]]>abs(2*rnorm(nrows))] <- NA
    d[[vari]] <- rlnorm(1, meanlog=10, sdlog = 10)*d[[vari]]
   }
  d
}

set.seed(2018)
d <- mk_data(10000, 10, 200)
is_train <- runif(nrow(d))<=0.5
dTrain <- d[is_train, , drop = FALSE]
dTest <- d[!is_train, , drop = FALSE]
outcome_name <- "y"
vars <- setdiff(colnames(dTrain), outcome_name)
```

Suppose our analysis plan is the following:

  - Fix missing values with `vtreat`.
  - Scale and center the data.
  - Model `y` as a function of the other columns using `glmnet`.

Now both `vtreat` and `glmnet` can scale, but we are going to keep the
scaling as a separate step to show how composite data preparation
pipelines work.

This can be done as follows.

``` r
# TODO: parallle on mkCrossFrameNExperiment?
# TODO: search for alpha?

# design a treatment plan using cross-validation methods
cp <- vtreat::mkCrossFrameNExperiment(dTrain, vars, outcome_name)
```

    ## [1] "vtreat 1.3.3 start initial treatment design Thu Nov 22 13:39:32 2018"
    ## [1] " start cross frame work Thu Nov 22 13:39:38 2018"
    ## [1] " vtreat::mkCrossFrameNExperiment done Thu Nov 22 13:39:45 2018"

``` r
# get the list of new variables
sf <- cp$treatments$scoreFrame
newvars <- sf$varName[sf$sig <= 1/nrow(sf)]
print(newvars)
```

    ##  [1] "var_001_clean"   "var_001_isBAD"   "var_002_clean"  
    ##  [4] "var_002_isBAD"   "var_003_clean"   "var_003_isBAD"  
    ##  [7] "var_004_clean"   "var_004_isBAD"   "var_005_clean"  
    ## [10] "var_005_isBAD"   "var_006_clean"   "var_006_isBAD"  
    ## [13] "var_007_clean"   "var_007_isBAD"   "var_008_clean"  
    ## [16] "var_008_isBAD"   "var_009_clean"   "var_009_isBAD"  
    ## [19] "var_010_clean"   "var_010_isBAD"   "noise_156_isBAD"

``` r
# learn a centering and scaling of the cross-validated 
# training frame
tfs <- scale(cp$crossFrame[, newvars, drop = FALSE], 
             center = TRUE, scale = TRUE)
centering <- attr(tfs, "scaled:center")
scaling <- attr(tfs, "scaled:scale")

# apply the centering and scaling to the cross-validated 
# training frame
tfs <- scale(cp$crossFrame[, newvars, drop = FALSE],
             center = centering,
             scale = scaling)

model <- cv.glmnet(as.matrix(tfs), 
                   cp$crossFrame[[outcome_name]],
                   alpha = 0.5,
                   family = "gaussian", 
                   standardize = FALSE)

# need this function for later
get_column <- function(d, cname) {
  as.numeric(d[, cname, drop=TRUE])
}

pipeline <-
  new("PartialNamedFn",
      fn_name = 'prepare',
      fn_package = "vtreat",
      arg_name = "dframe", 
      args = list(treatmentplan = cp$treatments,
                  varRestriction = newvars)) %.>%
  new("PartialNamedFn",
      fn_name ='subset',
      fn_package = "base",
      arg_name = "x",
      args = list(select = newvars))  %.>%
  new("PartialNamedFn",
      fn_name ='scale',
      fn_package = "base",
      arg_name = "x",
      args = list(center = centering,
                  scale = scaling))  %.>%
  new("PartialNamedFn",
      fn_name ="predict.cv.glmnet",
      fn_package = "glmnet",
      arg_name = "newx",
      args = list(object = model,
                  s = "lambda.1se"))  %.>%
  new("PartialFunction",
      fn = get_column,
      arg_name = "d",
      args = list(cname = "1"))

cat(format(pipeline))
```

    ## UnaryFnList(
    ##    vtreat::prepare(dframe=., treatmentplan, varRestriction),
    ##    base::subset(x=., select),
    ##    base::scale(x=., center, scale),
    ##    glmnet::predict.cv.glmnet(newx=., object, s),
    ##    PartialFunction(d=., cname))

``` r
dTrain$prediction <- dTrain %.>% pipeline

WVPlots::ScatterHist(dTrain, "prediction", "y", "fit on training data",
                     smoothmethod = "identity",
                     estimate_sig = TRUE,
                     point_alpha = 0.1,
                     contour = TRUE)
```

![](ModelingPipelines_files/figure-gfm/model1-1.png)<!-- -->

``` r
dTest$prediction <- dTest %.>% pipeline

WVPlots::ScatterHist(dTest, "prediction", "y", "fit on test",
                     smoothmethod = "identity",
                     estimate_sig = TRUE,
                     point_alpha = 0.1,
                     contour = TRUE)
```

![](ModelingPipelines_files/figure-gfm/model1-2.png)<!-- -->

Of course, using `wrapr`’s sequencing controls on a non-trivial data
processing pipeline starts duplicating functionality already present in
[`rquery`](https://github.com/WinVector/rquery)/[`rqdatatable`](https://github.com/WinVector/rqdatatable/).
Let’s work with `rqdatatable` instead.

What we want is to wrap partially evaluated functions as `rquery`
pipeline nodes. While they are more opaque than the `pipe_list()`
wrapping they include a number of powerful operators.

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
ops <- mk_td("d", colnames(dTrain)) %.>%
  rq_partial(., 'prepare',
             fn_package = "vtreat",
             arg_name = "dframe", 
             args = list(treatmentplan = cp$treatments,
                         varRestriction = newvars),
             columns_produced = newvars)  %.>%
  select_columns(., newvars) %.>%
  rq_partial(., 'scale',
             arg_name = "x", 
             args = list(center = centering,
                         scale = scaling),
             check_result_details = FALSE) %.>%
  rq_partial(.,
             "predict.cv.glmnet",
             fn_package = "glmnet",
             arg_name = "newx",
             args = list(object = model,
                         s = "lambda.1se"),
             check_result_details = FALSE)  %.>%
  rq_partialf(.,
              get_column,
              arg_name = "d",
              args = list(cname = "1"),
              check_result_details = FALSE) 

cat(format(ops))
```

    ## table(d; 
    ##   y,
    ##   var_001,
    ##   var_002,
    ##   var_003,
    ##   var_004,
    ##   var_005,
    ##   var_006,
    ##   var_007,
    ##   var_008,
    ##   var_009,
    ##   var_010,
    ##   noise_001,
    ##   noise_002,
    ##   noise_003,
    ##   noise_004,
    ##   noise_005,
    ##   noise_006,
    ##   noise_007,
    ##   noise_008,
    ##   noise_009,
    ##   ...) %.>%
    ##  non_sql_node(., vtreat::prepare) %.>%
    ##  select_columns(.,
    ##    var_001_clean, var_001_isBAD, var_002_clean, var_002_isBAD, var_003_clean, var_003_isBAD, var_004_clean, var_004_isBAD, var_005_clean, var_005_isBAD, var_006_clean, var_006_isBAD, var_007_clean, var_007_isBAD, var_008_clean, var_008_isBAD, var_009_clean, var_009_isBAD, var_010_clean, var_010_isBAD, noise_156_isBAD) %.>%
    ##  non_sql_node(., base::scale) %.>%
    ##  non_sql_node(., glmnet::predict.cv.glmnet) %.>%
    ##  non_sql_node(., function)

``` r
dTest %.>% ops %.>% head
```

    ## [1]  0.328805547  0.017182136 -1.023561866 -0.005864751 -0.856758474
    ## [6]  0.493458036

``` r
head(dTest$prediction)
```

    ## [1]  0.328805547  0.017182136 -1.023561866 -0.005864751 -0.856758474
    ## [6]  0.493458036

In the above example we are somewhat fighting `rquery` as `rquery` is
intended to work only on `data.frame`s and `data.table`s, and in this
example we are pushing around matrices and vectors (the
`check_result_details = FALSE` settings are turning off checks that
usually enforce types and columns.

But we now have two examples of a non-trivial modeling workflow saved as
a serialiazble object (alternately `pipeline` and `ops`).
