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

    ## [1] '0.5.0'

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

    ## table('kdd2009'; 
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

    ## [1] 125

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
    ##  $ churn                           : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var112_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                    : num  44967 -87345 -709072 166512 -1885436 ...
    ##  $ Var119_isBAD                    : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var123_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                    : num  -28 -30 -18 4 -20 -30 -22 -22 4 -18
    ##  $ Var126_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var13_clean                     : num  528 52 632 1612 4724 ...
    ##  $ Var13_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                    : num  1000 990 205 90 2745 ...
    ##  $ Var140_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                    : num  18 9 18 18 54 ...
    ##  $ Var144_isBAD                    : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var153_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                    : num  366 228 222 438 240 ...
    ##  $ Var192_catP                     : num  0.000199 0.008229 0.006334 0.006732 0.007032 ...
    ##  $ Var193_catB                     : num  -0.477 -0.477 -0.477 0.125 0.125 ...
    ##  $ Var193_catP                     : num  0.147 0.147 0.147 0.713 0.713 ...
    ##  $ Var193_lev_x_2Knk1KF            : int  1 1 1 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12               : int  0 0 0 1 1 1 1 1 0 0
    ##  $ Var195_catB                     : num  0.0185 0.0185 0.0185 0.0185 0.0185 ...
    ##  $ Var195_catP                     : num  0.959 0.959 0.959 0.959 0.959 ...
    ##  $ Var195_lev_x_taul               : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var197_catP                     : num  0.00419 0.00618 0.03835 0.08797 0.00663 ...
    ##  $ Var198_catP                     : num  0.001097 0.000848 0.000549 0.001247 0.000997 ...
    ##  $ Var198_lev_x_fhk21Ss            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                     : num  -8.464 1.014 -0.274 -10.585 -0.295 ...
    ##  $ Var199_catP                     : num  0.000299 0.004738 0.009675 0.002494 0.000898 ...
    ##  $ Var200_catP                     : num  2.49e-05 2.49e-05 4.99e-05 4.99e-05 2.49e-05 ...
    ##  $ Var200_lev_x_NA                 : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var202_catP                     : num  0.000997 0.00015 0.000549 0.001346 0.001845 ...
    ##  $ Var205_catB                     : num  -0.054 -0.054 -0.104 -0.104 0.641 ...
    ##  $ Var205_catP                     : num  0.2298 0.2298 0.641 0.641 0.0906 ...
    ##  $ Var205_lev_x_sJzTlal            : int  0 0 0 0 1 1 1 0 0 0
    ##  $ Var205_lev_x_VpdQ               : int  0 0 1 1 0 0 0 1 1 1
    ##  $ Var206_catB                     : num  -0.437 -0.175 -0.407 -0.292 0.234 ...
    ##  $ Var206_catP                     : num  0.0595 0.0361 0.0219 0.0295 0.3427 ...
    ##  $ Var206_lev_x_haYg               : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP               : int  0 0 0 0 1 0 0 1 1 1
    ##  $ Var206_lev_x_NA                 : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var206_lev_x_y6dw               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                     : num  0.0999 0.0999 0.0999 0.0999 0.0999 ...
    ##  $ Var207_catP                     : num  0.696 0.696 0.696 0.696 0.696 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ         : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var21_isBAD                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var210_catB                     : num  -0.0313 -0.0313 -0.0313 -0.0313 -0.3149 ...
    ##  $ Var210_catP                     : num  0.95168 0.95168 0.95168 0.95168 0.00274 ...
    ##  $ Var210_lev_x_g5HH               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI               : int  1 1 1 1 0 1 1 1 1 1
    ##  $ Var211_lev_x_L84s               : int  1 1 0 1 0 1 0 0 1 0
    ##  $ Var211_lev_x_Mtgm               : int  0 0 1 0 1 0 1 1 0 1
    ##  $ Var212_catB                     : num  -0.361 -0.724 -0.361 -0.361 -0.361 ...
    ##  $ Var212_catP                     : num  0.129 0.0149 0.129 0.129 0.129 ...
    ##  $ Var212_lev_x_CrNX               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L            : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_      : int  1 0 1 1 1 0 0 0 0 0
    ##  $ Var214_catP                     : num  2.49e-05 2.49e-05 4.99e-05 4.99e-05 2.49e-05 ...
    ##  $ Var214_lev_x_NA                 : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var216_catB                     : num  -0.121 -0.562 -0.44 -0.121 -7.366 ...
    ##  $ Var216_lev_x_XTbPUYD            : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var217_catB                     : num  -0.40668 1.84457 11.74811 -0.73314 -0.00784 ...
    ##  $ Var217_catP                     : num  9.97e-04 1.50e-04 4.99e-05 4.09e-03 2.74e-03 ...
    ##  $ Var218_catB                     : num  0.097 0.097 -0.188 0.097 0.097 ...
    ##  $ Var218_catP                     : num  0.476 0.476 0.509 0.476 0.476 ...
    ##  $ Var218_lev_x_cJvF               : int  0 0 1 0 0 0 0 1 0 0
    ##  $ Var218_lev_x_UYBR               : int  1 1 0 1 1 1 1 0 1 1
    ##  $ Var22_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                     : num  0.001097 0.000848 0.000549 0.001247 0.000997 ...
    ##  $ Var220_lev_x_4UxGlow            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                     : num  0.0715 0.0715 0.0715 0.0715 0.0715 ...
    ##  $ Var221_catP                     : num  0.735 0.735 0.735 0.735 0.735 ...
    ##  $ Var221_lev_x_d0EEeJi            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk               : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var222_catP                     : num  0.001097 0.000848 0.000549 0.001247 0.000997 ...
    ##  $ Var222_lev_x_catzS2D            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                     : num  -0.0856 -0.2903 -0.2903 -0.0856 -0.0856 ...
    ##  $ Var225_catP                     : num  0.212 0.222 0.222 0.212 0.212 ...
    ##  $ Var225_lev_x_ELof               : int  0 1 1 0 0 0 0 0 0 0
    ##  $ Var225_lev_x_NA                 : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var226_lev_x_7P5s               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                     : num  0.0999 0.0999 0.0999 0.0999 0.0999 ...
    ##  $ Var227_catP                     : num  0.698 0.698 0.698 0.698 0.698 ...
    ##  $ Var227_lev_x_RAYp               : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var227_lev_x_ZI9m               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                     : num  -0.362 -0.362 -0.362 0.144 0.144 ...
    ##  $ Var228_catP                     : num  0.087 0.087 0.087 0.648 0.648 ...
    ##  $ Var228_lev_x_55YFVY9            : int  1 1 1 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I      : int  0 0 0 1 1 1 1 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                     : num  0.14 0.14 0.14 0.14 0.14 ...
    ##  $ Var229_catP                     : num  0.000598 0.000598 0.000598 0.000598 0.000598 ...
    ##  $ Var229_lev_x_am7c               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                 : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_isBAD                      : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var65_clean                     : num  9 9 9 9 9 9 9 9 9 9
    ##  $ Var65_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                      : num  7 7 7 7 7 7 7 0 7 7
    ##  $ Var7_isBAD                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var73_clean                     : num  126 104 118 92 86 100 90 140 104 108
    ##  $ Var74_clean                     : num  28 63 7 0 35 168 224 0 252 0
    ##  $ Var74_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                     : num  1076211 288493 36041 202337 10652 ...
    ##  $ Var81_isBAD                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var83_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
