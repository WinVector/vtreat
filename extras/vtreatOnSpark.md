vtreat on Spark
================

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.3.1'

``` r
library("rquery")
packageVersion("rquery")
```

    ## [1] '0.6.0'

``` r
packageVersion("cdata")
```

    ## [1] '0.7.3'

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
```

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

# get unified scoreFrame
scoreFrame <- lapply(treatment_plans,
                     function(tpi) {
                       tpi$scoreFrame
                     })
scoreFrame <- do.call(rbind, scoreFrame)

# try to get Bonferroni- corrected valid derived variables.
approximate_df <- length(vars) + nrow(scoreFrame)
theshold <- 1/(1 + approximate_df)
newvars <- scoreFrame$varName[ (scoreFrame$varMoves) &
                                 (scoreFrame$sig < theshold) & 
                                 (scoreFrame$rsq >= 1.0e-3) ]
print(length(newvars))
```

    ## [1] 135

``` r
rqplan <- as_rquery_plan(treatment_plans, var_restriction = newvars)
# ops <- flatten_fn_list(d_train, rqplan$optree_generators)
# cat(format(ops))
# ops %.>%
#   op_diagram(.) %.>% 
#   DiagrammeR::grViz(.)
# sql <- to_sql(ops, db)
# cat(sql)
d_train <- rquery_prepare(db, rqplan, d_train, 
                          tmp_name_gen(), 
                          temporary = TRUE, overwrite = TRUE)
length(column_names(d_train))
```

    ## [1] 136

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  136 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 44967 126870 -2135 90994 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -28 4 -0.46 -0.46 ...
    ##  $ Var126_isBAD                          : int  0 0 0 1 1 0 0 0 0 1
    ##  $ Var13_clean                           : num  2968 528 400 348 648 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                          : num  11845 1000 110 0 1350 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 18 9 9 9 54 0 0 36 9
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                          : num  354 366 324 240 269 ...
    ##  $ Var192_catP                           : num  0.004105 0.000198 0.00643 0.006183 0.004748 ...
    ##  $ Var193_catB                           : num  -0.467 -0.467 -0.467 -0.467 0.133 ...
    ##  $ Var193_catP                           : num  0.143 0.143 0.143 0.143 0.724 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 0 0 0 0 1 1
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 1 1 1 1 0 0
    ##  $ Var194_catP                           : num  0.25 0.25 0.745 0.745 0.745 ...
    ##  $ Var194_lev_x_NA                       : int  0 0 1 1 1 0 0 1 1 1
    ##  $ Var197_catP                           : num  0.09066 0.00465 0.00524 0.09066 0.00341 ...
    ##  $ Var198_catP                           : num  1.04e-03 9.40e-04 2.47e-04 4.95e-05 2.47e-05 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.011376 0.000297 0.005045 0.014146 0.002275 ...
    ##  $ Var200_catP                           : num  9.89e-05 2.47e-05 2.47e-05 2.47e-05 4.95e-05 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var201_catP                           : num  0.255 0.255 0.745 0.745 0.745 ...
    ##  $ Var201_lev_x_NA                       : int  0 0 1 1 1 0 0 1 1 1
    ##  $ Var201_lev_x_smXZ                     : int  1 1 0 0 0 1 1 0 0 0
    ##  $ Var202_catP                           : num  2.97e-04 9.40e-04 2.47e-05 1.48e-04 3.46e-04 ...
    ##  $ Var205_catB                           : num  -0.072 -0.12 -0.12 -0.072 -0.12 ...
    ##  $ Var205_catP                           : num  0.639 0.228 0.228 0.639 0.228 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 0 0 1 0 0 1 1 0 1
    ##  $ Var206_catB                           : num  0.2235 -0.7198 -0.7198 -0.0962 0.1078 ...
    ##  $ Var206_catP                           : num  0.0145 0.0559 0.0559 0.0346 0.0793 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 1 1 0 0 0 0 0 1 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 1 1 1 0 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.126 0.126 0.126 0.126 -0.459 ...
    ##  $ Var207_catP                           : num  0.703 0.703 0.703 0.703 0.071 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 0 1 0 0 0 0
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0299 -0.0299 -0.0299 -0.0299 -0.0299 ...
    ##  $ Var210_catP                           : num  0.95 0.95 0.95 0.95 0.95 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 1 1 1 1 0 0 1 1 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var212_catB                           : num  0.218 -0.473 -0.473 -0.314 -0.52 ...
    ##  $ Var212_catP                           : num  0.5885 0.1284 0.1284 0.0133 0.0612 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 1 1 0 0 1 0 0 0 0
    ##  $ Var214_catP                           : num  9.89e-05 2.47e-05 2.47e-05 2.47e-05 4.95e-05 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_catB                           : num  0.017 -0.23 -0.23 -0.109 -0.398 ...
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catP                           : num  2.97e-04 8.90e-04 2.47e-05 2.47e-05 9.89e-05 ...
    ##  $ Var218_catB                           : num  0.132 0.132 -0.238 -0.238 -0.238 ...
    ##  $ Var218_catP                           : num  0.48 0.48 0.506 0.506 0.506 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 0 1 1 1 0 1 0 1 1
    ##  $ Var218_lev_x_UYBR                     : int  1 1 0 0 0 1 0 1 0 0
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  1.04e-03 9.40e-04 2.47e-04 4.95e-05 2.47e-05 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0999 0.0999 0.0999 0.0999 -0.526 ...
    ##  $ Var221_catP                           : num  0.7414 0.7414 0.7414 0.7414 0.0627 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 0 1 0 0 0 0
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catP                           : num  1.04e-03 9.40e-04 2.47e-04 4.95e-05 2.47e-05 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.151 -0.151 -0.343 -0.343 -0.343 ...
    ##  $ Var225_catP                           : num  0.207 0.207 0.218 0.218 0.218 ...
    ##  $ Var225_lev_x_ELof                     : int  0 0 1 1 1 0 0 1 1 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_catB                           : num  0.30269 -0.41359 -0.0784 -0.07951 0.00433 ...
    ##  $ Var226_catP                           : num  0.1624 0.0602 0.0241 0.0943 0.042 ...
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 1 0 0 0 1 0 0 0 0
    ##  $ Var227_catB                           : num  0.125 0.125 0.125 0.125 -0.335 ...
    ##  $ Var227_catP                           : num  0.7049 0.7049 0.7049 0.7049 0.0459 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 0 1 0 0 0 0
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var228_catB                           : num  -0.398 -0.398 -0.398 -0.398 0.165 ...
    ##  $ Var228_catP                           : num  0.0851 0.0851 0.0851 0.0851 0.6599 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 1 1 0 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  -0.322 -0.322 -0.322 -0.322 -0.322 ...
    ##  $ Var229_catP                           : num  0.00089 0.00089 0.00089 0.00089 0.00089 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var65_clean                           : num  9 9 9 9 18 9 9 9 36 36
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                            : num  7 7 7 0 14 7 0 7 28 14
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                           : num  3 4.17 3 4.17 3 ...
    ##  $ Var73_clean                           : num  126 126 106 132 84 86 140 112 132 146
    ##  $ Var74_clean                           : num  490 28 35 0 35 35 0 7 70 322
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  115048 1076211 146004 1692 181203 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
