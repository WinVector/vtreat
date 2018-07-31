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

    ## [1] 151

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

    ## [1] 152

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  152 variables:
    ##  $ churn                           : int  -1 -1 -1 -1 -1 -1 -1 1 -1 -1
    ##  $ Var106_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                    : num  -38397 44967 126870 -2135 168554 ...
    ##  $ Var117_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_isBAD                    : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var123_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                    : num  -26 -28 4 -0.773 -22 ...
    ##  $ Var126_isBAD                    : int  0 0 0 1 0 0 0 1 1 0
    ##  $ Var13_clean                     : num  2968 528 400 348 720 ...
    ##  $ Var13_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                    : num  11845 1000 110 0 805 ...
    ##  $ Var140_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                    : num  36 18 9 9 9 ...
    ##  $ Var144_isBAD                    : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var145_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var150_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var155_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_isBAD                    : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                    : num  354 366 324 240 269 ...
    ##  $ Var19_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var192_catP                     : num  0.003855 0.000247 0.006079 0.005832 0.007315 ...
    ##  $ Var193_catB                     : num  -0.487 -0.487 -0.487 -0.487 0.15 ...
    ##  $ Var193_catP                     : num  0.147 0.147 0.147 0.147 0.718 ...
    ##  $ Var193_lev_x_2Knk1KF            : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var193_lev_x_AERks4l            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12               : int  0 0 0 0 1 1 1 1 1 0
    ##  $ Var194_catP                     : num  0.254 0.254 0.741 0.741 0.741 ...
    ##  $ Var194_lev_x_NA                 : int  0 0 1 1 1 0 0 1 1 0
    ##  $ Var194_lev_x_SEuy               : int  1 1 0 0 0 1 1 0 0 1
    ##  $ Var198_catP                     : num  9.39e-04 1.04e-03 2.47e-04 2.47e-05 1.04e-03 ...
    ##  $ Var198_lev_x_fhk21Ss            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                     : num  -1.287 -7.406 -0.478 -0.224 0.195 ...
    ##  $ Var199_catP                     : num  1.11e-02 9.88e-05 5.09e-03 1.60e-02 1.09e-03 ...
    ##  $ Var200_catP                     : num  2.47e-05 2.47e-05 2.47e-05 9.88e-05 5.04e-01 ...
    ##  $ Var200_lev_x_NA                 : int  0 0 0 0 1 0 0 1 0 0
    ##  $ Var201_catP                     : num  0.258 0.258 0.741 0.741 0.741 ...
    ##  $ Var201_lev_x_NA                 : int  0 0 1 1 1 0 0 1 1 0
    ##  $ Var201_lev_x_smXZ               : int  1 1 0 0 0 1 1 0 0 1
    ##  $ Var202_catP                     : num  2.97e-04 1.09e-03 2.47e-05 4.94e-04 5.93e-04 ...
    ##  $ Var205_catB                     : num  -0.0805 -0.1117 -0.1117 -0.0805 0.6331 ...
    ##  $ Var205_catP                     : num  0.6378 0.2314 0.2314 0.6378 0.0909 ...
    ##  $ Var205_lev_x_sJzTlal            : int  0 0 0 0 1 0 1 0 1 0
    ##  $ Var205_lev_x_VpdQ               : int  1 0 0 1 0 1 0 0 0 1
    ##  $ Var206_catB                     : num  0.1362 -0.5647 -0.5647 -0.1608 0.0473 ...
    ##  $ Var206_catP                     : num  0.0143 0.0575 0.0575 0.037 0.0796 ...
    ##  $ Var206_lev_x_haYg               : int  0 1 1 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP               : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var206_lev_x_NA                 : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var206_lev_x_y6dw               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                     : num  0.127 0.127 0.127 0.127 0.127 ...
    ##  $ Var207_catP                     : num  0.703 0.703 0.703 0.703 0.703 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ         : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var21_isBAD                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var210_catP                     : num  0.952 0.952 0.952 0.952 0.952 ...
    ##  $ Var210_lev_x_g5HH               : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var210_lev_x_uKAI               : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var211_lev_x_L84s               : int  1 1 1 1 0 1 1 1 1 0
    ##  $ Var211_lev_x_Mtgm               : int  0 0 0 0 1 0 0 0 0 1
    ##  $ Var212_catB                     : num  0.22 -0.436 -0.436 -0.972 0.22 ...
    ##  $ Var212_catP                     : num  0.5852 0.131 0.131 0.0147 0.5852 ...
    ##  $ Var212_lev_x_NhsEn4L            : int  1 0 0 0 1 0 1 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_      : int  0 1 1 0 0 1 0 0 0 0
    ##  $ Var214_catP                     : num  2.47e-05 2.47e-05 2.47e-05 9.88e-05 5.04e-01 ...
    ##  $ Var214_lev_x_NA                 : int  0 0 0 0 1 0 0 1 0 0
    ##  $ Var216_catB                     : num  0.251 -0.309 -0.309 -0.765 0.551 ...
    ##  $ Var216_lev_x_kZJyVg2            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catP                     : num  2.97e-04 1.09e-03 2.47e-05 9.88e-05 5.93e-04 ...
    ##  $ Var218_catB                     : num  0.132 0.132 -0.218 -0.218 0.132 ...
    ##  $ Var218_catP                     : num  0.481 0.481 0.505 0.505 0.481 ...
    ##  $ Var218_lev_x_cJvF               : int  0 0 1 1 0 0 0 1 1 0
    ##  $ Var218_lev_x_UYBR               : int  1 1 0 0 1 1 1 0 0 1
    ##  $ Var219_catB                     : num  0.0191 0.0191 0.0191 0.0191 0.0191 ...
    ##  $ Var22_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                     : num  9.39e-04 1.04e-03 2.47e-04 2.47e-05 1.04e-03 ...
    ##  $ Var220_lev_x_4UxGlow            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                     : num  0.1 0.1 0.1 0.1 0.1 ...
    ##  $ Var221_catP                     : num  0.743 0.743 0.743 0.743 0.743 ...
    ##  $ Var221_lev_x_oslk               : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var221_lev_x_zCkv               : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var222_catP                     : num  9.39e-04 1.04e-03 2.47e-04 2.47e-05 1.04e-03 ...
    ##  $ Var222_lev_x_catzS2D            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                     : num  -0.143 -0.143 -0.325 -0.325 0.197 ...
    ##  $ Var225_catP                     : num  0.21 0.21 0.222 0.222 0.519 ...
    ##  $ Var225_lev_x_ELof               : int  0 0 1 1 0 0 0 0 1 0
    ##  $ Var225_lev_x_NA                 : int  0 0 0 0 1 0 0 1 0 0
    ##  $ Var225_lev_x_xG3x               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_catB                     : num  0.172 -0.166 -0.341 -0.195 -0.13 ...
    ##  $ Var227_catB                     : num  0.125 0.125 0.125 0.125 0.125 ...
    ##  $ Var227_catP                     : num  0.705 0.705 0.705 0.705 0.705 ...
    ##  $ Var227_lev_x_RAYp               : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var227_lev_x_ZI9m               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                     : num  -0.47 -0.47 -0.47 -0.47 0.183 ...
    ##  $ Var228_catP                     : num  0.0869 0.0869 0.0869 0.0869 0.6553 ...
    ##  $ Var228_lev_x_55YFVY9            : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I      : int  0 0 0 0 1 1 1 1 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV   : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var229_catB                     : num  -0.142 -0.142 -0.142 -0.142 -0.142 ...
    ##  $ Var229_catP                     : num  0.000741 0.000741 0.000741 0.000741 0.000741 ...
    ##  $ Var229_lev_x_am7c               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                 : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var37_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_isBAD                      : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_isBAD                      : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var65_clean                     : num  9 9 9 9 9 9 9 9 9 9
    ##  $ Var65_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                      : num  7 7 7 0 7 7 7 7 0 7
    ##  $ Var7_isBAD                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                     : num  3 4.18 3 4.18 3 ...
    ##  $ Var73_clean                     : num  126 126 106 132 90 92 100 80 82 108
    ##  $ Var74_clean                     : num  490 28 35 0 224 0 168 189 0 0
    ##  $ Var74_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                     : num  115048 1076211 146004 1692 94249 ...
    ##  $ Var81_isBAD                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var82_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var94_clean                     : num  242262 99559 43389 99559 10032 ...
    ##  $ Var99_isBAD                     : int  1 1 1 1 1 1 1 1 1 1
