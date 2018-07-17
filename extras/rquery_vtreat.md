rquery vtreat
================
John Mount, Win-Vector LLC
2018-07-17

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

    ## [1] "vtreat 1.3.0 inspecting inputs Tue Jul 17 08:54:52 2018"
    ## [1] "designing treatments Tue Jul 17 08:54:52 2018"
    ## [1] " have initial level statistics Tue Jul 17 08:54:52 2018"
    ## [1] " scoring treatments Tue Jul 17 08:54:52 2018"
    ## [1] "have treatment plan Tue Jul 17 08:54:53 2018"
    ## [1] "rescoring complex variables Tue Jul 17 08:54:53 2018"
    ## [1] "done rescoring complex variables Tue Jul 17 08:54:53 2018"

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
treated <- vtreat::rqdatatable_prepare(rqplan, dTrainC, 
                                       extracols = "id", 
                                       print_rquery = TRUE)
```

    ## table('dTrainC'; 
    ##   x,
    ##   z,
    ##   y,
    ##   id) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_92220745595499351705_0000000000'; 
    ##     x,
    ##     x_catP),
    ##   j= LEFT, by= x) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_97498437125668391485_0000000000'; 
    ##     x,
    ##     x_catB),
    ##   j= LEFT, by= x) %.>%
    ##  extend(.,
    ##   x_lev_NA := ifelse(is.na(x), 1, 0),
    ##   x_lev_x_a := ifelse(is.na(x), 0, ifelse(x == "a", 1, 0)),
    ##   x_lev_x_b := ifelse(is.na(x), 0, ifelse(x == "b", 1, 0)),
    ##   x_catP := ifelse(is.na(x), 0.166666666666667, ifelse(is.na(x_catP), 0.0833333333333333, x_catP)),
    ##   x_catB := ifelse(is.na(x), 9.21044036697607, ifelse(is.na(x_catB), 0, x_catB)),
    ##   z_clean := ifelse(is.na(z), 3.6, z),
    ##   z_isBAD := ifelse(is.na(z), 1, 0)) %.>%
    ##  select_columns(.,
    ##    id, x_catB, x_catP, x_lev_NA, x_lev_x_a, x_lev_x_b, y, z_clean, z_isBAD)

``` r
treated[]  %.>%
  knitr::kable(.)
```

|   id|     x\_catB|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b| y     |  z\_clean|  z\_isBAD|
|----:|-----------:|----------:|-----------:|-------------:|-------------:|:------|---------:|---------:|
|    5|   9.2104404|  0.1666667|           1|             0|             0| TRUE  |       5.0|         0|
|    1|  -0.6930972|  0.5000000|           0|             1|             0| FALSE |       1.0|         0|
|    2|  -0.6930972|  0.5000000|           0|             1|             0| FALSE |       2.0|         0|
|    3|  -0.6930972|  0.5000000|           0|             1|             0| TRUE  |       3.6|         1|
|    4|   0.0000000|  0.3333333|           0|             0|             1| FALSE |       4.0|         0|
|    6|   0.0000000|  0.3333333|           0|             0|             1| TRUE  |       6.0|         0|

``` r
treatednj <- vtreat::rqdatatable_prepare(rqplan, dTrainC, 
                                       extracols = "id", 
                                       non_join_mapping = TRUE)
treatednj[]  %.>%
  knitr::kable(.)
```

|   id|     x\_catB|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b| y     |  z\_clean|  z\_isBAD|
|----:|-----------:|----------:|-----------:|-------------:|-------------:|:------|---------:|---------:|
|    1|  -0.6930972|  0.5000000|           0|             1|             0| FALSE |       1.0|         0|
|    2|  -0.6930972|  0.5000000|           0|             1|             0| FALSE |       2.0|         0|
|    3|  -0.6930972|  0.5000000|           0|             1|             0| TRUE  |       3.6|         1|
|    4|   0.0000000|  0.3333333|           0|             0|             1| FALSE |       4.0|         0|
|    5|   9.2104404|  0.1666667|           1|             0|             0| TRUE  |       5.0|         0|
|    6|   0.0000000|  0.3333333|           0|             0|             1| TRUE  |       6.0|         0|

``` r
source_data <- rquery::rq_copy_to(db, "dTrainC", dTrainC,
                                  overwrite = TRUE, temporary = TRUE)

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedC",
                       extracols = "id",
                       print_sql = FALSE)
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

    ## [1] "vtreat 1.3.0 inspecting inputs Tue Jul 17 08:54:53 2018"
    ## [1] "designing treatments Tue Jul 17 08:54:53 2018"
    ## [1] " have initial level statistics Tue Jul 17 08:54:53 2018"
    ## [1] " scoring treatments Tue Jul 17 08:54:53 2018"
    ## [1] "have treatment plan Tue Jul 17 08:54:53 2018"
    ## [1] "rescoring complex variables Tue Jul 17 08:54:53 2018"
    ## [1] "done rescoring complex variables Tue Jul 17 08:54:53 2018"

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
treated <- vtreat::rqdatatable_prepare(rqplan, dTrainR, 
                                       extracols = "id", 
                                       print_rquery = TRUE)
```

    ## table('dTrainR'; 
    ##   x,
    ##   z,
    ##   y,
    ##   id) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_27803335877859574721_0000000000'; 
    ##     x,
    ##     x_catP),
    ##   j= LEFT, by= x) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_08795734462865638973_0000000000'; 
    ##     x,
    ##     x_catN),
    ##   j= LEFT, by= x) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_42131733337549383200_0000000000'; 
    ##     x,
    ##     x_catD),
    ##   j= LEFT, by= x) %.>%
    ##  extend(.,
    ##   x_lev_NA := ifelse(is.na(x), 1, 0),
    ##   x_lev_x_a := ifelse(is.na(x), 0, ifelse(x == "a", 1, 0)),
    ##   x_lev_x_b := ifelse(is.na(x), 0, ifelse(x == "b", 1, 0)),
    ##   x_catP := ifelse(is.na(x), 0.166666666666667, ifelse(is.na(x_catP), 0.0833333333333333, x_catP)),
    ##   x_catN := ifelse(is.na(x), 0.5, ifelse(is.na(x_catN), 0, x_catN)),
    ##   x_catD := ifelse(is.na(x), 0, ifelse(is.na(x_catD), 0, x_catD)),
    ##   z_clean := ifelse(is.na(z), 3.6, z),
    ##   z_isBAD := ifelse(is.na(z), 1, 0)) %.>%
    ##  select_columns(.,
    ##    id, x_catD, x_catN, x_catP, x_lev_NA, x_lev_x_a, x_lev_x_b, y, z_clean, z_isBAD)

``` r
treated[]  %.>%
  knitr::kable(.)
```

|   id|    x\_catD|     x\_catN|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|    y|  z\_clean|  z\_isBAD|
|----:|----------:|-----------:|----------:|-----------:|-------------:|-------------:|----:|---------:|---------:|
|    5|  0.0000000|   0.5000000|  0.1666667|           1|             0|             0|    1|       5.0|         0|
|    1|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    0|       1.0|         0|
|    2|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    0|       2.0|         0|
|    3|  0.5773503|  -0.1666667|  0.5000000|           0|             1|             0|    1|       3.6|         1|
|    4|  0.7071068|   0.0000000|  0.3333333|           0|             0|             1|    0|       4.0|         0|
|    6|  0.7071068|   0.0000000|  0.3333333|           0|             0|             1|    1|       6.0|         0|

``` r
source_data <- rquery::rq_copy_to(db, "dTrainR", dTrainR,
                                  overwrite = TRUE, temporary = TRUE)

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedN",
                       extracols = "id",
                       print_sql = FALSE)
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

    ## [1] "vtreat 1.3.0 inspecting inputs Tue Jul 17 08:54:54 2018"
    ## [1] "designing treatments Tue Jul 17 08:54:54 2018"
    ## [1] " have initial level statistics Tue Jul 17 08:54:54 2018"
    ## [1] " scoring treatments Tue Jul 17 08:54:54 2018"
    ## [1] "have treatment plan Tue Jul 17 08:54:54 2018"

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
treated <- vtreat::rqdatatable_prepare(rqplan, dTrainZ, 
                                       extracols = "id", 
                                       print_rquery = TRUE)
```

    ## table('dTrainZ'; 
    ##   x,
    ##   z,
    ##   id) %.>%
    ##  natural_join(.,
    ##   table('vtreat_tmp_88096207945359266236_0000000000'; 
    ##     x,
    ##     x_catP),
    ##   j= LEFT, by= x) %.>%
    ##  extend(.,
    ##   x_lev_NA := ifelse(is.na(x), 1, 0),
    ##   x_lev_x_a := ifelse(is.na(x), 0, ifelse(x == "a", 1, 0)),
    ##   x_lev_x_b := ifelse(is.na(x), 0, ifelse(x == "b", 1, 0)),
    ##   x_catP := ifelse(is.na(x), 0.166666666666667, ifelse(is.na(x_catP), 0.0833333333333333, x_catP)),
    ##   z_clean := ifelse(is.na(z), 3.6, z),
    ##   z_isBAD := ifelse(is.na(z), 1, 0)) %.>%
    ##  select_columns(.,
    ##    id, x_catP, x_lev_NA, x_lev_x_a, x_lev_x_b, z_clean, z_isBAD)

``` r
treated[]  %.>%
  knitr::kable(.)
```

|   id|    x\_catP|  x\_lev\_NA|  x\_lev\_x\_a|  x\_lev\_x\_b|  z\_clean|  z\_isBAD|
|----:|----------:|-----------:|-------------:|-------------:|---------:|---------:|
|    5|  0.1666667|           1|             0|             0|       5.0|         0|
|    1|  0.5000000|           0|             1|             0|       1.0|         0|
|    2|  0.5000000|           0|             1|             0|       2.0|         0|
|    3|  0.5000000|           0|             1|             0|       3.6|         1|
|    4|  0.3333333|           0|             0|             1|       4.0|         0|
|    6|  0.3333333|           0|             0|             1|       6.0|         0|

``` r
source_data <- rquery::rq_copy_to(db, "dTrainZ", dTrainZ,
                                  overwrite = TRUE, temporary = TRUE)

rest <- rquery_prepare(db, rqplan, source_data, "dTreatedZ",
                       extracols = "id",
                       print_sql = FALSE)
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
