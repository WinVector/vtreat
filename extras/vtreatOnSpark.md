vtreat on Spark
================

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.2.0'

``` r
library("rquery")
packageVersion("rquery")
```

    ## [1] '0.5.0'

``` r
packageVersion("sparklyr")
```

    ## [1] '0.8.4'

``` r
db <- sparklyr::spark_connect(version='2.2.0', 
                              master = "local")
db_opts <- rq_connection_tests(db)
options(db_opts)
```

``` r
tmp_name_gen <- wrapr::mk_tmp_name_source("kddvtreat")

d <- db_td(db, "kdd2009") %.>%
  extend_nse(., sample_col = rand()) %.>%
  materialize(db, ., table_name = tmp_name_gen())

y_name <- "churn"
vars <- setdiff(column_names(d), c(y_name, "sample_col"))

d_train <- d %.>%
  select_rows_nse(., sample_col <= 0.8) %.>%
  materialize(db, ., table_name = tmp_name_gen()) # should not need to materialize this, doign this to debug
d_train <- d %.>%
  select_rows_nse(., sample_col > 0.9)
d_treat <- d %.>%
  select_rows_nse(., (sample_col > 0.8) & (sample_col <= 0.9)) %.>%
  execute(db, .)
```

``` r
cl = parallel::makeCluster(4)
vars <- "Var205"
tp <- vtreat::designTreatmentsC(d_treat, vars, y_name, 1, parallelCluster = cl)
```

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 07:31:28 2018"
    ## [1] "designing treatments Sat Jun 16 07:31:28 2018"
    ## [1] " have initial level statistics Sat Jun 16 07:31:28 2018"
    ## [1] " scoring treatments Sat Jun 16 07:31:28 2018"
    ## [1] "have treatment plan Sat Jun 16 07:31:28 2018"
    ## [1] "rescoring complex variables Sat Jun 16 07:31:28 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 07:31:29 2018"

``` r
newvars <- tp$scoreFrame$varName[tp$scoreFrame$sig < 1/nrow(tp$scoreFrame)]
rqplan <- as_rquery(tp)
```

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var205'(character,character)->character-
    ## >'Var205_catP') , class vtreatment

``` r
d_train <- materialize_treated(db, rqplan, d_train, 
                                tmp_name_gen(), temporary = TRUE, overwrite = TRUE)
column_names(d_train)
```

    ## [1] "churn"                "Var205_catB"          "Var205_lev_x_09_Q"   
    ## [4] "Var205_lev_x_NA"      "Var205_lev_x_sJzTlal" "Var205_lev_x_VpdQ"

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_80652969817413310662_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 5000 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  6 variables:
    ##  $ churn               : int  -1 -1 -1 -1 -1 -1 1 -1 -1 1
    ##  $ Var205_catB         : num  -0.131 -0.131 -0.131 -0.131 -0.131 ...
    ##  $ Var205_lev_x_09_Q   : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_NA     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_sJzTlal: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_VpdQ   : int  1 1 1 1 1 1 1 1 1 1

``` r
# ops <- flatten_fn_list(d_train, rqplan$optree_generators) 
# nc <- column_names(ops)
# ops <- select_columns(ops, c(y_name, intersect(nc, newvars)))
# cat(format(ops))
# q <- to_sql(ops, db)
# res <- DBI::dbGetQuery(db, q)
```
