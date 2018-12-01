vtreat on Spark
================

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:10:05 2018"

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.3.3'

``` r
library("rquery")
packageVersion("rquery")
```

    ## [1] '1.2.1'

``` r
packageVersion("cdata")
```

    ## [1] '1.0.4'

``` r
packageVersion("sparklyr")
```

    ## [1] '0.9.1'

``` r
conf <- sparklyr::spark_config()
conf$sparklyr.log.console <- FALSE
conf$spark.yarn.am.cores <- 2
conf$spark.executor.cores <- 2
conf$spark.executor.memory <- "4G"
conf$spark.yarn.am.memory <- "4G" 
conf$`sparklyr.shell.driver-memory` <- "4G"
conf$`sparklyr.shell.executor-memory` <- "4G"
conf$`spark.yarn.executor.memoryOverhead` <- "4G"

raw_connection <- sparklyr::spark_connect(version='2.2.0', 
                              master = "local",
                              config = conf)
db_opts <- rq_connection_tests(raw_connection)
db <- rquery_db_info(connection = raw_connection,
                     is_dbi = TRUE,
                     connection_options = db_opts)
base::date()
```

    ## [1] "Sat Dec  1 15:10:26 2018"

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:10:40 2018"

``` r
tmp_name_gen <- wrapr::mk_tmp_name_source("kddvtreat")

d <- db_td(db, "kdd2009") %.>%
  extend_nse(., sample_col = random())

cat(format(d))
```

    ## table(`kdd2009`; 
    ##   Var1,
    ##   Var2,
    ##   Var3,
    ##   Var4,
    ##   Var5,
    ##   Var6,
    ##   Var7,
    ##   Var8,
    ##   Var9,
    ##   Var10,
    ##   Var11,
    ##   Var12,
    ##   Var13,
    ##   Var14,
    ##   Var15,
    ##   Var16,
    ##   Var17,
    ##   Var18,
    ##   Var19,
    ##   Var20,
    ##   ...) %.>%
    ##  extend(.,
    ##   sample_col := random())

``` r
#cat(to_sql(d, db))

d <- materialize(db, d, table_name = tmp_name_gen())

y_name <- "churn"
vars <- setdiff(column_names(d), c(y_name, "sample_col"))

d_train <- d %.>%
  select_rows_nse(., sample_col <= 0.5) %.>%
  materialize(db, ., table_name = tmp_name_gen())

d_test <- d %.>%
  select_rows_nse(., sample_col > 0.9) %.>%
  materialize(db, ., table_name = tmp_name_gen())

d_variable_design <- d %.>%
  select_rows_nse(., (sample_col > 0.5) & (sample_col <= 0.9)) %.>%
  materialize(db, ., table_name = tmp_name_gen())
base::date()
```

    ## [1] "Sat Dec  1 15:11:44 2018"

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:11:44 2018"

``` r
cl = parallel::makeCluster(4)
print(length(vars))
```

    ## [1] 230

``` r
# design treatments in small groups to manage memory
vgroups <- split(vars, ceiling(seq_len(length(vars))/10))
treatment_plans <- lapply(vgroups,
                          function(vi) {
                            di <- d_variable_design %.>%
                              select_columns(., c(y_name, vi)) %.>%
                              execute(db, .)
                            vtreat::designTreatmentsC(di, vi, y_name, 1, 
                                                      parallelCluster = cl,
                                                      verbose = FALSE)
                          })
base::date()
```

    ## [1] "Sat Dec  1 15:12:48 2018"

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:12:48 2018"

``` r
# get unified scoreFrame
scoreFrame <- lapply(treatment_plans,
                     function(tpi) {
                       tpi$scoreFrame
                     })
scoreFrame <- do.call(rbind, scoreFrame)
base::date()
```

    ## [1] "Sat Dec  1 15:12:48 2018"

``` r
# try to get Bonferroni- corrected valid derived variables.
approximate_df <- length(vars) + nrow(scoreFrame)
theshold <- 1/(1 + approximate_df)
newvars <- scoreFrame$varName[ (scoreFrame$varMoves) &
                                 (scoreFrame$sig < theshold) & 
                                 (scoreFrame$rsq >= 1.0e-3) ]
newvars <- unique(newvars)
print(length(newvars))
```

    ## [1] 122

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:12:48 2018"

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:12:48 2018"

``` r
col_sample <- execute(db, d_train, limit = 1L)
rqplan <- as_rquery_plan(treatment_plans, var_restriction = newvars)

if(FALSE) {
  ops <- rquery_prepare(db, rqplan, d_train, 
                        "dtrain_prepped", 
                        col_sample = col_sample,
                        return_ops = TRUE)
  cat(format(ops))
  ops %.>%
    rquery::op_diagram(.) %.>%
    DiagrammeR::grViz(.)
  # sql <- rquery::to_sql(ops, db)
  # cat(sql)
}

base::date()
```

    ## [1] "Sat Dec  1 15:12:49 2018"

``` r
d_train <- rquery_prepare(db, rqplan, d_train, 
                          tmp_name_gen(), 
                          col_sample = col_sample,
                          temporary = TRUE, 
                          overwrite = TRUE)
length(column_names(d_train))
```

    ## [1] 123

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:13:28 2018"

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  123 variables:
    ##  $ churn                                 : num  -1 -1 -1 -1 -1 -1 -1 -1 1 -1
    ##  $ Var192_catP                           : num  0.004061 0.006318 0.000251 0.006318 0.005917 ...
    ##  $ Var193_catP                           : num  0.146 0.146 0.146 0.146 0.146 ...
    ##  $ Var193_catB                           : num  -0.546 -0.546 -0.546 -0.546 -0.546 ...
    ##  $ Var198_catP                           : num  1.30e-03 6.52e-04 1.20e-03 4.01e-04 2.51e-05 ...
    ##  $ Var199_catP                           : num  0.011984 0.012234 0.000251 0.005766 0.016095 ...
    ##  $ Var200_catP                           : num  2.51e-05 5.01e-05 2.51e-05 2.51e-05 2.51e-05 ...
    ##  $ Var202_catP                           : num  2.01e-04 3.01e-04 1.15e-03 2.51e-05 1.50e-04 ...
    ##  $ Var205_catP                           : num  0.639 0.639 0.231 0.231 0.639 ...
    ##  $ Var205_catB                           : num  -0.0705 -0.0705 -0.1865 -0.1865 -0.0705 ...
    ##  $ Var206_catP                           : num  0.0144 0.0219 0.0588 0.0588 0.035 ...
    ##  $ Var206_catB                           : num  -0.0674 -0.6848 -0.6859 -0.6859 -0.1927 ...
    ##  $ Var207_catP                           : num  0.699 0.699 0.699 0.699 0.699 ...
    ##  $ Var207_catB                           : num  0.126 0.126 0.126 0.126 0.126 ...
    ##  $ Var210_catP                           : num  0.952 0.952 0.952 0.952 0.952 ...
    ##  $ Var210_catB                           : num  -0.0305 -0.0305 -0.0305 -0.0305 -0.0305 ...
    ##  $ Var212_catP                           : num  0.5841 0.1281 0.1281 0.1281 0.0137 ...
    ##  $ Var212_catB                           : num  0.238 -0.557 -0.557 -0.557 -0.796 ...
    ##  $ Var214_catP                           : num  2.51e-05 5.01e-05 2.51e-05 2.51e-05 2.51e-05 ...
    ##  $ Var216_catB                           : num  -0.233 -0.862 -0.313 -0.313 -0.667 ...
    ##  $ Var217_catP                           : num  2.01e-04 5.01e-05 1.15e-03 2.51e-05 5.01e-05 ...
    ##  $ Var218_catP                           : num  0.48 0.505 0.48 0.505 0.505 ...
    ##  $ Var218_catB                           : num  0.146 -0.254 0.146 -0.254 -0.254 ...
    ##  $ Var220_catP                           : num  1.30e-03 6.52e-04 1.20e-03 4.01e-04 2.51e-05 ...
    ##  $ Var221_catP                           : num  0.737 0.737 0.737 0.737 0.737 ...
    ##  $ Var221_catB                           : num  0.103 0.103 0.103 0.103 0.103 ...
    ##  $ Var222_catP                           : num  1.30e-03 6.52e-04 1.20e-03 4.01e-04 2.51e-05 ...
    ##  $ Var225_catP                           : num  0.207 0.219 0.207 0.219 0.219 ...
    ##  $ Var225_catB                           : num  -0.0792 -0.4271 -0.0792 -0.4271 -0.4271 ...
    ##  $ Var226_catP                           : num  0.162 0.0276 0.06 0.0223 0.0938 ...
    ##  $ Var226_catB                           : num  0.2437 0.0901 -0.2908 -0.0442 -0.1246 ...
    ##  $ Var227_catP                           : num  0.701 0.701 0.701 0.701 0.701 ...
    ##  $ Var227_catB                           : num  0.125 0.125 0.125 0.125 0.125 ...
    ##  $ Var228_catP                           : num  0.0874 0.0874 0.0874 0.0874 0.0874 ...
    ##  $ Var228_catB                           : num  -0.388 -0.388 -0.388 -0.388 -0.388 ...
    ##  $ Var229_catP                           : num  0.000652 0.000652 0.000652 0.000652 0.000652 ...
    ##  $ Var229_catB                           : num  0.0981 0.0981 0.0981 0.0981 0.0981 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var7_clean                            : num  7 7 7 7 0 7 7 7 7 0
    ##  $ Var13_clean                           : num  2968 632 528 400 348 ...
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var65_clean                           : num  9 9 9 9 9 9 9 9 9 9
    ##  $ Var72_clean                           : num  3 3 4.21 3 4.21 ...
    ##  $ Var73_clean                           : num  126 118 126 106 132 90 100 100 80 82
    ##  $ Var74_clean                           : num  490 7 28 35 0 224 140 168 189 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  115048 36041 1076211 146004 1692 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 -709072 44967 126870 -2135 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -18 -28 4 -0.409 ...
    ##  $ Var126_isBAD                          : int  0 0 0 0 1 0 1 0 1 1
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                          : num  11845 205 1000 110 0 ...
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 18 18 9 9 ...
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                          : num  354 222 366 324 240 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 0 1 1 1 1 1
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 1 0 0 1 0
    ##  $ Var205_lev_x_09_Q                     : int  0 0 1 1 0 0 1 0 1 0
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 1 0 1 0 1
    ##  $ Var205_lev_x_VpdQ                     : int  1 1 0 0 1 0 0 0 0 0
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 1 1 1 1 0
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var211_lev_x_L84s                     : int  1 0 1 1 1 0 1 1 1 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 1 0 0 0 1 0 0 0 0
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 0 1 1 1 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 1 1 1 0 0 0 0 0 0
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 1 0 0 1 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var218_lev_x_cJvF                     : int  0 1 0 1 1 0 0 0 1 1
    ##  $ Var218_lev_x_UYBR                     : int  1 0 1 0 0 1 1 1 0 0
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 1 1 1 1 0
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_lev_x_ELof                     : int  0 1 0 1 1 0 0 0 0 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 1 0 0 1 0
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 1 1 1 1 0
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 1 1 1 1 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0

``` r
base::date()
```

    ## [1] "Sat Dec  1 15:13:29 2018"
