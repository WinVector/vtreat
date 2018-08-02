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

    ## [1] 182

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

    ## [1] 183

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  183 variables:
    ##  $ churn                                 : num  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 -709072 44967 126870 90994 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -18 -28 4 -0.848 ...
    ##  $ Var126_isBAD                          : int  0 0 0 0 1 0 1 1 0 0
    ##  $ Var13_clean                           : num  2968 632 528 400 648 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  11845 205 1000 110 1350 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 18 18 9 9 9 0 27 18 54
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var157_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  354 222 366 324 270 ...
    ##  $ Var192_catB                           : num  -0.454 0.182 -7.78 -0.354 -0.649 ...
    ##  $ Var192_catP                           : num  0.00415 0.00629 0.00015 0.00659 0.00375 ...
    ##  $ Var193_catB                           : num  -0.506 -0.506 -0.506 -0.506 0.154 ...
    ##  $ Var193_catP                           : num  0.148 0.148 0.148 0.148 0.718 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 1 1 1 1 1 1
    ##  $ Var198_catP                           : num  1.45e-03 5.49e-04 9.49e-04 3.50e-04 9.99e-05 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.01119 0.01059 0.00025 0.00614 0.00215 ...
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catP                           : num  9.99e-05 2.50e-05 2.50e-05 2.50e-05 4.99e-05 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 1 0 1 0 0
    ##  $ Var201_catB                           : num  -0.1286 0.0406 -0.1286 0.0406 0.0406 ...
    ##  $ Var202_catP                           : num  0.00025 0.00035 0.000949 0.000025 0.000499 ...
    ##  $ Var205_catB                           : num  -0.097 -0.097 -0.067 -0.067 -0.067 ...
    ##  $ Var205_catP                           : num  0.639 0.639 0.233 0.233 0.233 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 1 0 0 0 1
    ##  $ Var205_lev_x_VpdQ                     : int  1 1 0 0 0 0 0 1 1 0
    ##  $ Var206_catB                           : num  0.267846 -0.053333 -0.653549 -0.653549 -0.000953 ...
    ##  $ Var206_catP                           : num  0.0143 0.022 0.0602 0.0602 0.0799 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.129 0.129 0.129 0.129 -0.494 ...
    ##  $ Var207_catP                           : num  0.703 0.703 0.703 0.703 0.07 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 0 1 1 1 1 1
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0432 -0.0432 -0.0432 -0.0432 -0.0432 ...
    ##  $ Var210_catP                           : num  0.954 0.954 0.954 0.954 0.954 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 1 1 0
    ##  $ Var212_catB                           : num  0.223 -0.406 -0.406 -0.406 -0.581 ...
    ##  $ Var212_catP                           : num  0.5844 0.1306 0.1306 0.1306 0.0596 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 0 1 1 1 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 1 1 1 0 0 0 0 1 1
    ##  $ Var213_lev_x_KdSa                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var213_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var214_catP                           : num  9.99e-05 2.50e-05 2.50e-05 2.50e-05 4.99e-05 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 1 0 1 0 0
    ##  $ Var216_catB                           : num  0.151 -0.512 -0.164 -0.164 -0.285 ...
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  -8.29 0 -9.57 0 -7.37 ...
    ##  $ Var217_catP                           : num  2.50e-04 2.50e-05 8.99e-04 2.50e-05 9.99e-05 ...
    ##  $ Var218_catB                           : num  0.0989 -0.1939 0.0989 -0.1939 -0.1939 ...
    ##  $ Var218_catP                           : num  0.481 0.506 0.481 0.506 0.506 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 1 0 1 1 0 0 0 0 0
    ##  $ Var218_lev_x_UYBR                     : int  1 0 1 0 0 1 1 1 1 1
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  1.45e-03 5.49e-04 9.49e-04 3.50e-04 9.99e-05 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0978 0.0978 0.0978 0.0978 -0.6032 ...
    ##  $ Var221_catP                           : num  0.7408 0.7408 0.7408 0.7408 0.0609 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 0 1 1 1 1 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catP                           : num  1.45e-03 5.49e-04 9.49e-04 3.50e-04 9.99e-05 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.123 -0.385 -0.123 -0.385 -0.385 ...
    ##  $ Var225_catP                           : num  0.211 0.221 0.211 0.221 0.221 ...
    ##  $ Var225_lev_x_ELof                     : int  0 1 0 1 1 0 0 0 0 0
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 1 0 1 0 0
    ##  $ Var226_catB                           : num  0.2331 0.264 -0.1866 -0.0405 -0.158 ...
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 0 0 0 0 1 0 0
    ##  $ Var227_catB                           : num  0.127 0.127 0.127 0.127 -0.49 ...
    ##  $ Var227_catP                           : num  0.7046 0.7046 0.7046 0.7046 0.0461 ...
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 0 1 1 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.443 -0.443 -0.443 -0.443 0.179 ...
    ##  $ Var228_catP                           : num  0.0902 0.0902 0.0902 0.0902 0.6512 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 1 1 1 1 1 1
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  -9.45 -9.45 -9.45 -9.45 -9.45 ...
    ##  $ Var229_catP                           : num  0.000799 0.000799 0.000799 0.000799 0.000799 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 9 9 9 18 9 9 27 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  7 7 7 7 14 7 7 7 7 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var71_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  3 3 4.18 3 3 ...
    ##  $ Var73_clean                           : num  126 118 126 106 84 90 100 90 92 86
    ##  $ Var74_clean                           : num  490 7 28 35 35 224 140 175 0 35
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  115048 36041 1076211 146004 181203 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var91_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
