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

    ## [1] '0.6.1'

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

    ## [1] 137

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

    ## [1] 138

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  138 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 44967 168554 42538 166512 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -28 -22 -0.595 4 ...
    ##  $ Var126_isBAD                          : int  0 0 0 1 0 0 1 0 0 0
    ##  $ Var13_clean                           : num  2968 528 720 3316 1612 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                          : num  11845 1000 805 7765 90 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 18 9 27 18 54 0 0 18 0
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                          : num  354 366 271 271 438 ...
    ##  $ Var192_catP                           : num  0.004275 0.000199 0.007307 0.003778 0.00686 ...
    ##  $ Var193_catB                           : num  -0.515 -0.515 0.139 0.139 0.139 ...
    ##  $ Var193_catP                           : num  0.147 0.147 0.716 0.716 0.716 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 1 1 1 1 1 1 1 1
    ##  $ Var194_catP                           : num  0.253 0.253 0.743 0.743 0.253 ...
    ##  $ Var194_lev_x_NA                       : int  0 0 1 1 0 0 1 0 1 1
    ##  $ Var195_catP                           : num  0.959 0.959 0.959 0.959 0.959 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var198_catP                           : num  0.000795 0.000795 0.000944 0.00179 0.001491 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.010588 0.000497 0.000845 0.010439 0.002287 ...
    ##  $ Var200_catB                           : num  -6.675 0 0.197 0.197 -6.675 ...
    ##  $ Var200_catP                           : num  4.97e-05 2.49e-05 5.05e-01 5.05e-01 4.97e-05 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var201_catP                           : num  0.256 0.256 0.743 0.743 0.256 ...
    ##  $ Var201_lev_x_NA                       : int  0 0 1 1 0 0 1 0 1 1
    ##  $ Var201_lev_x_smXZ                     : int  1 1 0 0 1 1 0 1 0 0
    ##  $ Var202_catP                           : num  0.000398 0.000994 0.000398 0.000249 0.001193 ...
    ##  $ Var205_catB                           : num  -0.1205 -0.0664 0.6959 -0.1205 -0.1205 ...
    ##  $ Var205_catP                           : num  0.6385 0.2326 0.0892 0.6385 0.6385 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 1 0 0 1 1 0 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 0 0 1 1 0 0 1 1 1
    ##  $ Var206_catB                           : num  -0.09178 -0.52236 0.00558 0.00558 -0.41591 ...
    ##  $ Var206_catP                           : num  0.0133 0.0598 0.0808 0.0808 0.031 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 1 1 1 0 1
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.122 0.122 0.122 0.122 0.122 ...
    ##  $ Var207_catP                           : num  0.696 0.696 0.696 0.696 0.696 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 1 0 0 0 0
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0368 -0.0368 -0.0368 -0.0368 -0.0368 ...
    ##  $ Var210_catP                           : num  0.952 0.952 0.952 0.952 0.952 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 1 0 1 1 0 1 0 1 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 1 0 0 1 0 1 0 0
    ##  $ Var212_catB                           : num  0.21 -0.397 0.21 0.21 -0.397 ...
    ##  $ Var212_catP                           : num  0.582 0.128 0.582 0.582 0.128 ...
    ##  $ Var212_lev_x_4kVnq_T26xq1p            : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 1 1 0 0 0 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 1 0 0 1 1 0 0 0 0
    ##  $ Var214_catP                           : num  4.97e-05 2.49e-05 5.05e-01 5.05e-01 4.97e-05 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var216_catB                           : num  -0.2055 -0.2298 -8.9776 -0.0583 -0.2298 ...
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catP                           : num  0.000398 0.000944 0.000398 0.000249 0.004126 ...
    ##  $ Var218_catB                           : num  0.121 0.121 0.121 0.121 0.121 ...
    ##  $ Var218_catP                           : num  0.478 0.478 0.478 0.478 0.478 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 0 0 0 0 0 1 1 1 0
    ##  $ Var218_lev_x_UYBR                     : int  1 1 1 1 1 1 0 0 0 1
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  0.000795 0.000795 0.000944 0.00179 0.001491 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0972 0.0972 0.0972 0.0972 0.0972 ...
    ##  $ Var221_catP                           : num  0.735 0.735 0.735 0.735 0.735 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 1 0 0 0 0
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 1 1 1 1
    ##  $ Var222_catP                           : num  0.000795 0.000795 0.000944 0.00179 0.001491 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.104 -0.104 0.191 0.191 -0.104 ...
    ##  $ Var225_catP                           : num  0.21 0.21 0.52 0.52 0.21 ...
    ##  $ Var225_lev_x_ELof                     : int  0 0 0 0 0 0 1 0 1 1
    ##  $ Var225_lev_x_NA                       : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var225_lev_x_xG3x                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var226_catB                           : num  0.21016 -0.32988 -0.00538 0.21016 -0.32988 ...
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 1 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  0.12 0.12 0.12 0.12 0.12 ...
    ##  $ Var227_catP                           : num  0.697 0.697 0.697 0.697 0.697 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 1 0 0 0 0
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var228_catB                           : num  -0.509 -0.509 0.174 0.174 0.174 ...
    ##  $ Var228_catP                           : num  0.0886 0.0886 0.6512 0.6512 0.6512 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 1 1 1 1 0 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var229_catB                           : num  0.589 0.589 0.589 0.589 0.589 ...
    ##  $ Var229_catP                           : num  0.000795 0.000795 0.000795 0.000795 0.000795 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_clean                            : num  1589 56 1162 693 658 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var65_clean                           : num  9 9 9 27 9 9 9 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                            : num  7 7 7 7 7 7 0 0 7 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                           : num  3 4.21 3 3 4.21 ...
    ##  $ Var73_clean                           : num  126 126 90 90 92 86 82 140 104 112
    ##  $ Var74_clean                           : num  490 28 224 175 0 35 0 0 14 7
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  115048 1076211 94249 213526 202337 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
