vtreat on Spark
================

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.3.0'

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

    ## table('`kdd2009`'; 
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
rquery::rlook(db, d_train$table_name)
```

    ## table `kddvtreat_45169246770176629050_0000000004` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 24979 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  138 variables:
    ##  $ churn                           : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var112_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                    : num  -38397 44967 126870 42538 166512 ...
    ##  $ Var119_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var123_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_clean                    : num  189765 4302 48645 470214 36117 ...
    ##  $ Var125_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                    : num  -26 -28 4 -0.496 4 ...
    ##  $ Var126_isBAD                    : int  0 0 0 1 0 0 0 0 1 0
    ##  $ Var13_clean                     : num  2968 528 400 3316 1612 ...
    ##  $ Var13_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                    : num  11845 1000 110 7765 90 ...
    ##  $ Var140_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                    : num  36 18 9 27 18 0 0 36 18 9
    ##  $ Var144_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var153_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                    : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                    : num  354 366 324 270 438 ...
    ##  $ Var192_catP                     : num  3.99e-03 9.99e-05 6.49e-03 3.79e-03 6.34e-03 ...
    ##  $ Var193_catB                     : num  -0.695 -0.695 -0.695 0.151 0.151 ...
    ##  $ Var193_catP                     : num  0.145 0.145 0.145 0.724 0.724 ...
    ##  $ Var193_lev_x_2Knk1KF            : int  1 1 1 0 0 0 0 1 1 1
    ##  $ Var193_lev_x_RO12               : int  0 0 0 1 1 1 1 0 0 0
    ##  $ Var194_catP                     : num  0.251 0.251 0.745 0.745 0.251 ...
    ##  $ Var194_lev_x_NA                 : int  0 0 1 1 0 0 1 1 0 0
    ##  $ Var194_lev_x_SEuy               : int  1 1 0 0 1 1 0 0 1 1
    ##  $ Var197_catP                     : num  0.08968 0.00514 0.00619 0.0391 0.08703 ...
    ##  $ Var198_catP                     : num  0.001049 0.000849 0.0003 0.001348 0.001248 ...
    ##  $ Var198_lev_x_fhk21Ss            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                     : num  0.01168 0.0003 0.00589 0.01054 0.00225 ...
    ##  $ Var200_catB                     : num  0 0 0 0.218 -6.62 ...
    ##  $ Var200_catP                     : num  2.50e-05 2.50e-05 2.50e-05 5.07e-01 4.99e-05 ...
    ##  $ Var200_lev_x_NA                 : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var201_catB                     : num  -0.188 -0.188 0.058 0.058 -0.188 ...
    ##  $ Var201_catP                     : num  0.255 0.255 0.745 0.745 0.255 ...
    ##  $ Var201_lev_x_NA                 : int  0 0 1 1 0 0 1 1 0 0
    ##  $ Var201_lev_x_smXZ               : int  1 1 0 0 1 1 0 0 1 1
    ##  $ Var202_catP                     : num  0.00035 0.000999 0.000025 0.0002 0.001548 ...
    ##  $ Var204_lev_x_RcM7               : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var205_catB                     : num  -0.1078 -0.0103 -0.0103 -0.1078 -0.1078 ...
    ##  $ Var205_catP                     : num  0.639 0.231 0.231 0.639 0.639 ...
    ##  $ Var205_lev_x_sJzTlal            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_VpdQ               : int  1 0 0 1 1 1 1 0 0 1
    ##  $ Var206_catB                     : num  0.0859 -0.7054 -0.7054 -0.2303 -0.5616 ...
    ##  $ Var206_catP                     : num  0.0139 0.0559 0.0559 0.0799 0.0317 ...
    ##  $ Var206_lev_x_haYg               : int  0 1 1 0 0 0 0 1 0 0
    ##  $ Var206_lev_x_IYzP               : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var206_lev_x_NA                 : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                     : num  0.122 0.122 0.122 0.122 0.122 ...
    ##  $ Var207_catP                     : num  0.706 0.706 0.706 0.706 0.706 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy: int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ         : int  1 1 1 1 1 0 0 0 1 1
    ##  $ Var21_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                     : num  -0.0362 -0.0362 -0.0362 -0.0362 -0.0362 ...
    ##  $ Var210_catP                     : num  0.951 0.951 0.951 0.951 0.951 ...
    ##  $ Var210_lev_x_g5HH               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI               : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var211_lev_x_L84s               : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var211_lev_x_Mtgm               : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var212_catB                     : num  0.219 -0.432 -0.432 0.219 -0.432 ...
    ##  $ Var212_catP                     : num  0.588 0.13 0.13 0.588 0.13 ...
    ##  $ Var212_lev_x_CrNX               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L            : int  1 0 0 1 0 0 0 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_      : int  0 1 1 0 1 0 0 0 1 0
    ##  $ Var214_catB                     : num  0 0 0 0.218 -6.62 ...
    ##  $ Var214_catP                     : num  2.50e-05 2.50e-05 2.50e-05 5.07e-01 4.99e-05 ...
    ##  $ Var214_lev_x_NA                 : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var216_catB                     : num  -0.444 -0.138 -0.138 -0.193 -0.138 ...
    ##  $ Var216_lev_x_XTbPUYD            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catP                     : num  0.00035 0.000949 0.000025 0.00015 0.004144 ...
    ##  $ Var218_catB                     : num  0.102 0.102 -0.178 0.102 0.102 ...
    ##  $ Var218_catP                     : num  0.477 0.477 0.51 0.477 0.477 ...
    ##  $ Var218_lev_x_cJvF               : int  0 0 1 0 0 1 0 1 1 1
    ##  $ Var218_lev_x_UYBR               : int  1 1 0 1 1 0 1 0 0 0
    ##  $ Var22_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                     : num  0.001049 0.000849 0.0003 0.001348 0.001248 ...
    ##  $ Var220_lev_x_4UxGlow            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                     : num  0.0952 0.0952 0.0952 0.0952 0.0952 ...
    ##  $ Var221_catP                     : num  0.742 0.742 0.742 0.742 0.742 ...
    ##  $ Var221_lev_x_d0EEeJi            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk               : int  1 1 1 1 1 0 0 0 1 1
    ##  $ Var221_lev_x_zCkv               : int  0 0 0 0 0 1 1 1 0 0
    ##  $ Var222_catP                     : num  0.001049 0.000849 0.0003 0.001348 0.001248 ...
    ##  $ Var222_lev_x_catzS2D            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                     : num  -0.193 -0.193 -0.377 0.204 -0.193 ...
    ##  $ Var225_catP                     : num  0.207 0.207 0.224 0.521 0.207 ...
    ##  $ Var225_lev_x_ELof               : int  0 0 1 0 0 0 1 1 0 0
    ##  $ Var225_lev_x_kG3k               : int  1 1 0 0 1 0 0 0 1 1
    ##  $ Var225_lev_x_NA                 : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var226_catB                     : num  0.197 -0.594 -0.15 0.197 -0.594 ...
    ##  $ Var226_lev_x_7P5s               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2               : int  1 0 0 1 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ               : int  0 1 0 0 1 0 0 0 0 0
    ##  $ Var227_catB                     : num  0.12 0.12 0.12 0.12 0.12 ...
    ##  $ Var227_catP                     : num  0.707 0.707 0.707 0.707 0.707 ...
    ##  $ Var227_lev_x_RAYp               : int  1 1 1 1 1 0 0 0 1 1
    ##  $ Var227_lev_x_ZI9m               : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var228_catB                     : num  -0.705 -0.705 -0.705 0.179 0.179 ...
    ##  $ Var228_catP                     : num  0.088 0.088 0.088 0.659 0.659 ...
    ##  $ Var228_lev_x_55YFVY9            : int  1 1 1 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I      : int  0 0 0 1 1 0 0 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                     : num  -0.243 -0.243 -0.243 -0.243 -0.243 ...
    ##  $ Var229_catP                     : num  0.000899 0.000899 0.000899 0.000899 0.000899 ...
    ##  $ Var229_lev_x_am7c               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                 : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_clean                      : num  1589 56 770 693 658 ...
    ##  $ Var6_isBAD                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var65_clean                     : num  9 9 9 27 9 9 9 36 27 27
    ##  $ Var65_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                      : num  7 7 7 7 7 0 7 28 14 21
    ##  $ Var7_isBAD                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                     : num  3 4.2 3 3 4.2 ...
    ##  $ Var73_clean                     : num  126 126 106 90 92 140 112 132 160 156
    ##  $ Var74_clean                     : num  490 28 35 175 0 0 7 70 21 56
    ##  $ Var74_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                     : num  115048 1076211 146004 213526 202337 ...
    ##  $ Var81_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var83_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                     : int  0 0 0 0 0 0 0 0 0 0
