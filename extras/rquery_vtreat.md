rquery vtreat
================
John Mount, Win-Vector LLC
2018-08-01

[`vtreat`](https://github.com/WinVector/vtreat) transforms can be hosted on [`rquery`](https://github.com/WinVector/rquery). This allows transforms at scale.

``` r
library("vtreat")

eval_examples <- requireNamespace("rquery", quietly = TRUE)
eval_rqdt <- eval_examples && requireNamespace("rqdatatable", quietly = TRUE)
eval_db <- eval_examples &&
  requireNamespace("DBI", quietly = TRUE) &&
  requireNamespace("RSQLite", quietly = TRUE)
db <- NULL
if(eval_db) {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}
```

Classification example.

``` r
dTrainC <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                      z= c(1, 2, NA, 4, 5, 6),
                      y= c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
                      stringsAsFactors = FALSE)
dTrainC$id <- seq_len(nrow(dTrainC))
treatmentsC <- designTreatmentsC(dTrainC, c("x", "z"), 'y', TRUE)
```

    ## [1] "vtreat 1.3.1 inspecting inputs Wed Aug  1 18:44:44 2018"
    ## [1] "designing treatments Wed Aug  1 18:44:44 2018"
    ## [1] " have initial level statistics Wed Aug  1 18:44:44 2018"
    ## [1] " scoring treatments Wed Aug  1 18:44:44 2018"
    ## [1] "have treatment plan Wed Aug  1 18:44:44 2018"
    ## [1] "rescoring complex variables Wed Aug  1 18:44:44 2018"
    ## [1] "done rescoring complex variables Wed Aug  1 18:44:44 2018"

``` r
prepare(treatmentsC, dTrainC) %.>%
  knitr::kable(.)
```

|    x\_catP|     x\_catB|  z\_clean|  z\_isBAD|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b| y     |
|----------:|-----------:|---------:|---------:|-----------:|-------------:|-------------:|:------|
|  0.5000000|  -0.6930972|       1.0|         0|           0|             1|             0| FALSE |
|  0.5000000|  -0.6930972|       2.0|         0|           0|             1|             0| FALSE |
|  0.5000000|  -0.6930972|       3.6|         1|           0|             1|             0| TRUE  |
|  0.3333333|   0.0000000|       4.0|         0|           0|             0|             1| FALSE |
|  0.1666667|   9.2104404|       5.0|         0|           1|             0|             0| TRUE  |
|  0.3333333|   0.0000000|       6.0|         0|           0|             0|             1| TRUE  |

``` r
rqplan <- as_rquery_plan(list(treatmentsC))
```

``` r
source_data <- rquery::rq_copy_to(db, "dTrainC", dTrainC,
                                  overwrite = TRUE, temporary = TRUE)

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedC",
                       extracols = "id")
resd <- DBI::dbReadTable(db, rest$table_name)
resd  %.>%
  knitr::kable(.)
```

|   id|     x\_catB|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|    y|  z\_clean|  z\_isBAD|
|----:|-----------:|----------:|-----------:|-------------:|-------------:|----:|---------:|---------:|
|    1|  -0.6930972|  0.5000000|           0|             1|             0|    0|       1.0|         0|
|    2|  -0.6930972|  0.5000000|           0|             1|             0|    0|       2.0|         0|
|    3|  -0.6930972|  0.5000000|           0|             1|             0|    1|       3.6|         1|
|    4|   0.0000000|  0.3333333|           0|             0|             1|    0|       4.0|         0|
|    5|   9.2104404|  0.1666667|           1|             0|             0|    1|       5.0|         0|
|    6|   0.0000000|  0.3333333|           0|             0|             1|    1|       6.0|         0|

``` r
rquery::rq_remove_table(db, source_data$table_name)
```

    ## [1] TRUE

``` r
rquery::rq_remove_table(db, rest$table_name)
```

    ## [1] TRUE

Regression example.

``` r
dTrainR <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                      z= c(1, 2, NA, 4, 5, 6),
                      y= as.numeric(c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)),
                      stringsAsFactors = FALSE)
dTrainR$id <- seq_len(nrow(dTrainR))
treatmentsN <- designTreatmentsN(dTrainR, c("x", "z"), 'y')
```

    ## [1] "vtreat 1.3.1 inspecting inputs Wed Aug  1 18:44:45 2018"
    ## [1] "designing treatments Wed Aug  1 18:44:45 2018"
    ## [1] " have initial level statistics Wed Aug  1 18:44:45 2018"
    ## [1] " scoring treatments Wed Aug  1 18:44:45 2018"
    ## [1] "have treatment plan Wed Aug  1 18:44:45 2018"
    ## [1] "rescoring complex variables Wed Aug  1 18:44:45 2018"
    ## [1] "done rescoring complex variables Wed Aug  1 18:44:45 2018"

``` r
prepare(treatmentsN, dTrainR)  %.>%
  knitr::kable(.)
```

|    x\_catP|     x\_catN|    x\_catD|  z\_clean|  z\_isBAD|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|    y|
|----------:|-----------:|----------:|---------:|---------:|-----------:|-------------:|-------------:|----:|
|  0.5000000|  -0.1666667|  0.5773503|       1.0|         0|           0|             1|             0|    0|
|  0.5000000|  -0.1666667|  0.5773503|       2.0|         0|           0|             1|             0|    0|
|  0.5000000|  -0.1666667|  0.5773503|       3.6|         1|           0|             1|             0|    1|
|  0.3333333|   0.0000000|  0.7071068|       4.0|         0|           0|             0|             1|    0|
|  0.1666667|   0.5000000|  0.7071068|       5.0|         0|           1|             0|             0|    1|
|  0.3333333|   0.0000000|  0.7071068|       6.0|         0|           0|             0|             1|    1|

``` r
rqplan <- as_rquery_plan(list(treatmentsN))
```

``` r
source_data <- rquery::rq_copy_to(db, "dTrainR", dTrainR,
                                  overwrite = TRUE, temporary = TRUE)

if(FALSE) {
  ops <- rquery_prepare(db, rqplan, source_data, "dTreatedN",
                       extracols = "id", return_ops = TRUE)
  cat(format(ops))
  ops %.>%
    rquery::op_diagram(.) %.>%
    DiagrammeR::grViz(.)
  # sql <- rquery::to_sql(ops, db)
  # cat(sql)
}

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedN",
                       extracols = "id")
resd <- DBI::dbReadTable(db, rest$table_name)
resd %.>%
  knitr::kable(.)
```

|   id|    x\_catD|     x\_catN|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|    y|  z\_clean|  z\_isBAD|
|----:|----------:|-----------:|----------:|-----------:|-------------:|-------------:|----:|---------:|---------:|
|    1|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    0|       1.0|         0|
|    2|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    0|       2.0|         0|
|    3|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    1|       3.6|         1|
|    4|  0.7071068|   0.0000000|  0.3333333|           0|             0|             1|    0|       4.0|         0|
|    5|  0.0000000|   0.5000000|  0.1666667|           1|             0|             0|    1|       5.0|         0|
|    6|  0.7071068|   0.0000000|  0.3333333|           0|             0|             1|    1|       6.0|         0|

``` r
rquery::rq_remove_table(db, source_data$table_name)
```

    ## [1] TRUE

``` r
rquery::rq_remove_table(db, rest$table_name)
```

    ## [1] TRUE

y-free example.

``` r
dTrainZ <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                      z= c(1, 2, NA, 4, 5, 6),
                      stringsAsFactors = FALSE)
dTrainZ$id <- seq_len(nrow(dTrainZ))
treatmentsZ <- designTreatmentsZ(dTrainZ, c("x", "z"))
```

    ## [1] "vtreat 1.3.1 inspecting inputs Wed Aug  1 18:44:45 2018"
    ## [1] "designing treatments Wed Aug  1 18:44:45 2018"
    ## [1] " have initial level statistics Wed Aug  1 18:44:45 2018"
    ## [1] " scoring treatments Wed Aug  1 18:44:45 2018"
    ## [1] "have treatment plan Wed Aug  1 18:44:45 2018"

``` r
prepare(treatmentsZ, dTrainZ)  %.>%
  knitr::kable(.)
```

|    x\_catP|  z\_clean|  z\_isBAD|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|
|----------:|---------:|---------:|-----------:|-------------:|-------------:|
|  0.5000000|       1.0|         0|           0|             1|             0|
|  0.5000000|       2.0|         0|           0|             1|             0|
|  0.5000000|       3.6|         1|           0|             1|             0|
|  0.3333333|       4.0|         0|           0|             0|             1|
|  0.1666667|       5.0|         0|           1|             0|             0|
|  0.3333333|       6.0|         0|           0|             0|             1|

``` r
rqplan <- as_rquery_plan(list(treatmentsZ))
```

``` r
source_data <- rquery::rq_copy_to(db, "dTrainZ", dTrainZ,
                                  overwrite = TRUE, temporary = TRUE)

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedZ",
                       extracols = "id")
resd <- DBI::dbReadTable(db, rest$table_name)
resd  %.>%
  knitr::kable(.)
```

|   id|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|  z\_clean|  z\_isBAD|
|----:|----------:|-----------:|-------------:|-------------:|---------:|---------:|
|    1|  0.5000000|           0|             1|             0|       1.0|         0|
|    2|  0.5000000|           0|             1|             0|       2.0|         0|
|    3|  0.5000000|           0|             1|             0|       3.6|         1|
|    4|  0.3333333|           0|             0|             1|       4.0|         0|
|    5|  0.1666667|           1|             0|             0|       5.0|         0|
|    6|  0.3333333|           0|             0|             1|       6.0|         0|

``` r
rquery::rq_remove_table(db, source_data$table_name)
```

    ## [1] TRUE

``` r
rquery::rq_remove_table(db, rest$table_name)
```

    ## [1] TRUE

------------------------------------------------------------------------

``` r
if(!is.null(db)) {
  DBI::dbDisconnect(db)
}
```
