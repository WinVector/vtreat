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
newvars <- unique(newvars)
print(length(newvars))
```

    ## [1] 125

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

d_train <- rquery_prepare(db, rqplan, d_train, 
                          tmp_name_gen(), 
                          col_sample = col_sample,
                          temporary = TRUE, 
                          overwrite = TRUE)
length(column_names(d_train))
```

    ## [1] 126

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  126 variables:
    ##  $ churn                                 : num  -1 -1 -1 -1 -1 -1 1 -1 -1 -1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 -2135 90994 166512 21927 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -0.621 -0.621 4 -0.621 ...
    ##  $ Var126_isBAD                          : int  0 1 1 0 1 0 1 0 0 0
    ##  $ Var13_clean                           : num  2968 348 648 1612 2112 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                          : num  11845 0 1350 90 185 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 9 9 18 0 ...
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                          : num  354 240 270 438 246 ...
    ##  $ Var192_catP                           : num  0.00379 0.00661 0.00424 0.00606 0.00722 ...
    ##  $ Var193_catB                           : num  -0.475 -0.475 0.132 0.132 0.132 ...
    ##  $ Var193_catP                           : num  0.142 0.142 0.724 0.724 0.724 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 1 1 1 1 1 1 1 1
    ##  $ Var195_catB                           : num  0.0159 0.0159 0.0159 0.0159 0.0159 ...
    ##  $ Var198_catP                           : num  7.07e-04 2.52e-05 5.05e-05 1.41e-03 1.16e-03 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.01115 0.01428 0.00182 0.00167 0.00419 ...
    ##  $ Var200_catP                           : num  5.05e-05 5.05e-05 2.52e-05 5.05e-05 5.05e-05 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var202_catP                           : num  0.000202 0.000303 0.000505 0.001716 0.001161 ...
    ##  $ Var205_catB                           : num  -0.1082 -0.1082 -0.0899 -0.1082 -0.0899 ...
    ##  $ Var205_catP                           : num  0.634 0.634 0.237 0.634 0.237 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 1 0 1 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 1 0 1 0 0 0 0 1 1
    ##  $ Var206_catB                           : num  -0.0836 0.0795 0.0221 -0.5036 0.0221 ...
    ##  $ Var206_catP                           : num  0.0136 0.0337 0.0814 0.0303 0.0814 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.111 0.111 -0.412 0.111 0.111 ...
    ##  $ Var207_catP                           : num  0.7028 0.7028 0.0699 0.7028 0.7028 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 0 1 1 1 1 1 0 0
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var210_catB                           : num  -0.033 -0.033 -0.033 -0.033 -0.033 ...
    ##  $ Var210_catP                           : num  0.951 0.951 0.951 0.951 0.951 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 1 1 1 1 1 1 0 0 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 0 0 0 0 0 1 1 0
    ##  $ Var212_catB                           : num  0.209 -0.653 -0.479 -0.405 0.209 ...
    ##  $ Var212_catP                           : num  0.5868 0.0142 0.0592 0.1287 0.5868 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 1 1 0 1 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var214_catP                           : num  5.05e-05 5.05e-05 2.52e-05 5.05e-05 5.05e-05 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var216_catB                           : num  0.0358 -0.4351 0.3108 -0.315 0.0862 ...
    ##  $ Var216_lev_x_kZJtVhC                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  -8.0452 -6.6589 0 -0.406 0.0948 ...
    ##  $ Var217_catP                           : num  2.02e-04 5.05e-05 2.52e-05 4.09e-03 1.92e-03 ...
    ##  $ Var218_catB                           : num  0.125 -0.216 -0.216 0.125 0.125 ...
    ##  $ Var218_catP                           : num  0.48 0.506 0.506 0.48 0.48 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 1 1 0 0 0 1 0 1 1
    ##  $ Var218_lev_x_UYBR                     : int  1 0 0 1 1 1 0 1 0 0
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  7.07e-04 2.52e-05 5.05e-05 1.41e-03 1.16e-03 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0957 0.0957 -0.4654 0.0957 0.0957 ...
    ##  $ Var221_catP                           : num  0.741 0.741 0.0606 0.741 0.741 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 0 1 1 1 1 1 0 0
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var222_catP                           : num  7.07e-04 2.52e-05 5.05e-05 1.41e-03 1.16e-03 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.111 -0.299 -0.299 -0.111 -0.111 ...
    ##  $ Var225_catP                           : num  0.207 0.224 0.224 0.207 0.207 ...
    ##  $ Var225_lev_x_ELof                     : int  0 1 1 0 0 0 0 0 0 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var227_catB                           : num  0.111 0.111 -0.317 0.111 0.111 ...
    ##  $ Var227_catP                           : num  0.7046 0.7046 0.0451 0.7046 0.7046 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 0 1 1 1 1 1 0 0
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var228_catB                           : num  -0.459 -0.459 0.152 0.152 0.152 ...
    ##  $ Var228_catP                           : num  0.0849 0.0849 0.6578 0.6578 0.6578 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 1 1 1 1 1 1 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  0.606 0.606 0.606 0.606 0.606 ...
    ##  $ Var229_catP                           : num  0.000808 0.000808 0.000808 0.000808 0.000808 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_clean                           : num  315 167 899 684 167 ...
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var65_clean                           : num  9 9 18 9 9 9 9 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                            : num  7 0 14 7 7 7 7 7 0 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                           : num  3 4.17 3 4.17 3 ...
    ##  $ Var73_clean                           : num  126 132 84 92 100 100 80 90 140 88
    ##  $ Var74_clean                           : num  490 0 35 0 140 168 189 224 0 91
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  115048 1692 181203 202337 4808 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
