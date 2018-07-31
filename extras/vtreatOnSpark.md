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

    ## [1] 178

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

    ## [1] 179

``` r
# cdata::qlook(db, d_train$table_name)
# rquery::rlook(db, d_train$table_name)
d_train %.>% 
  orderby(., limit = 10) %.>% 
  execute(db, .) %.>% 
  str(., list.len = 10000)
```

    ## 'data.frame':    10 obs. of  179 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 1 -1 -1
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 -87345 44967 126870 -2135 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -30 -28 4 -0.567 ...
    ##  $ Var126_isBAD                          : int  0 0 0 0 1 1 0 1 1 0
    ##  $ Var13_clean                           : num  2968 52 528 400 348 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  11845 990 1000 110 0 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 9 18 9 9 9 54 9 0 0
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var151_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  354 228 366 324 240 ...
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var192_catB                           : num  0.366 0.254 -8.636 -0.127 -0.204 ...
    ##  $ Var192_catP                           : num  0.003886 0.007519 0.000353 0.006106 0.00656 ...
    ##  $ Var193_catB                           : num  -0.458 -0.458 -0.458 -0.458 -0.458 ...
    ##  $ Var193_catP                           : num  0.147 0.147 0.147 0.147 0.147 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 0 1 1 1 1 0
    ##  $ Var195_catP                           : num  0.958 0.958 0.958 0.958 0.958 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var198_catP                           : num  1.06e-03 7.57e-04 1.01e-03 4.04e-04 2.52e-05 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                           : num  -0.649 0.429 -8.076 -0.397 -0.521 ...
    ##  $ Var199_catP                           : num  0.012515 0.004592 0.000202 0.005904 0.015492 ...
    ##  $ Var200_catB                           : num  0 0 0 0 0 ...
    ##  $ Var200_catP                           : num  2.52e-05 2.52e-05 2.52e-05 2.52e-05 2.52e-05 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var202_catB                           : num  -8.076 1.134 -0.475 0 -8.636 ...
    ##  $ Var202_catP                           : num  2.02e-04 2.52e-04 1.06e-03 2.52e-05 3.53e-04 ...
    ##  $ Var204_lev_x_RcM7                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_catB                           : num  -0.0713 -0.1355 -0.1355 -0.1355 -0.0713 ...
    ##  $ Var205_catP                           : num  0.64 0.231 0.231 0.231 0.64 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 0 1 0 1 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 0 0 0 1 0 0 0 0 1
    ##  $ Var206_catB                           : num  0.115 -0.213 -0.579 -0.579 -0.213 ...
    ##  $ Var206_catP                           : num  0.014 0.0364 0.0573 0.0573 0.0364 ...
    ##  $ Var206_lev_x_haYg                     : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 1 0 1 1
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.121 0.121 0.121 0.121 0.121 ...
    ##  $ Var207_catP                           : num  0.7 0.7 0.7 0.7 0.7 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 0 1 1 0 1
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0332 -0.0332 -0.0332 -0.0332 -0.0332 ...
    ##  $ Var210_catP                           : num  0.952 0.952 0.952 0.952 0.952 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 0 0 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 1 1 1 1 1 0 1 1 0
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 0 0 0 0 1 0 0 1
    ##  $ Var212_catB                           : num  0.205 -0.428 -0.393 -0.393 -0.428 ...
    ##  $ Var212_catP                           : num  0.5875 0.0142 0.1255 0.1255 0.0142 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 0 1 1 0 0 1 0 0 0
    ##  $ Var214_catP                           : num  2.52e-05 2.52e-05 2.52e-05 2.52e-05 2.52e-05 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  -8.076 1.134 -0.475 0 -6.69 ...
    ##  $ Var217_catP                           : num  2.02e-04 2.52e-04 1.06e-03 2.52e-05 5.05e-05 ...
    ##  $ Var218_catB                           : num  0.114 0.114 0.114 -0.218 -0.218 ...
    ##  $ Var218_catP                           : num  0.477 0.477 0.477 0.509 0.509 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 0 0 1 1 1 0 1 1 0
    ##  $ Var218_lev_x_UYBR                     : int  1 1 1 0 0 0 1 0 0 1
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  1.06e-03 7.57e-04 1.01e-03 4.04e-04 2.52e-05 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0925 0.0925 0.0925 0.0925 0.0925 ...
    ##  $ Var221_catP                           : num  0.737 0.737 0.737 0.737 0.737 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 0 1 1 0 1
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var222_catP                           : num  1.06e-03 7.57e-04 1.01e-03 4.04e-04 2.52e-05 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.097 -0.307 -0.097 -0.307 -0.307 ...
    ##  $ Var225_catP                           : num  0.207 0.224 0.207 0.224 0.224 ...
    ##  $ Var225_lev_x_ELof                     : int  0 1 0 1 1 1 0 0 1 0
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var226_catB                           : num  0.2919 -0.085 -0.5141 -0.0238 -0.1297 ...
    ##  $ Var226_catP                           : num  0.1589 0.0286 0.0583 0.0236 0.0971 ...
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 0 1 0 0 0 1 0 0 0
    ##  $ Var227_catB                           : num  0.119 0.119 0.119 0.119 0.119 ...
    ##  $ Var227_catP                           : num  0.701 0.701 0.701 0.701 0.701 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 0 1 1 0 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.409 -0.409 -0.409 -0.409 -0.409 ...
    ##  $ Var228_catP                           : num  0.0886 0.0886 0.0886 0.0886 0.0886 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 1 1 1 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  0.649 0.649 0.649 0.649 0.649 ...
    ##  $ Var229_catP                           : num  0.000757 0.000757 0.000757 0.000757 0.000757 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var33_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_clean                            : num  1589 98 56 770 259 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var61_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 9 9 9 9 18 9 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  7 7 7 7 0 14 7 7 0 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  3 3 4.22 3 4.22 ...
    ##  $ Var73_clean                           : num  126 104 126 106 132 84 86 80 82 108
    ##  $ Var74_clean                           : num  490 63 28 35 0 35 35 189 0 0
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  115048 288493 1076211 146004 1692 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
