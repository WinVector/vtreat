vtreat on Spark
================

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.2.1'

``` r
library("rquery")
packageVersion("rquery")
```

    ## [1] '0.5.1'

``` r
packageVersion("cdata")
```

    ## [1] '0.7.1'

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
cat(to_sql(d, db))
```

    ## SELECT
    ##  `Var1`,
    ##  `Var2`,
    ##  `Var3`,
    ##  `Var4`,
    ##  `Var5`,
    ##  `Var6`,
    ##  `Var7`,
    ##  `Var8`,
    ##  `Var9`,
    ##  `Var10`,
    ##  `Var11`,
    ##  `Var12`,
    ##  `Var13`,
    ##  `Var14`,
    ##  `Var15`,
    ##  `Var16`,
    ##  `Var17`,
    ##  `Var18`,
    ##  `Var19`,
    ##  `Var20`,
    ##  `Var21`,
    ##  `Var22`,
    ##  `Var23`,
    ##  `Var24`,
    ##  `Var25`,
    ##  `Var26`,
    ##  `Var27`,
    ##  `Var28`,
    ##  `Var29`,
    ##  `Var30`,
    ##  `Var31`,
    ##  `Var32`,
    ##  `Var33`,
    ##  `Var34`,
    ##  `Var35`,
    ##  `Var36`,
    ##  `Var37`,
    ##  `Var38`,
    ##  `Var39`,
    ##  `Var40`,
    ##  `Var41`,
    ##  `Var42`,
    ##  `Var43`,
    ##  `Var44`,
    ##  `Var45`,
    ##  `Var46`,
    ##  `Var47`,
    ##  `Var48`,
    ##  `Var49`,
    ##  `Var50`,
    ##  `Var51`,
    ##  `Var52`,
    ##  `Var53`,
    ##  `Var54`,
    ##  `Var55`,
    ##  `Var56`,
    ##  `Var57`,
    ##  `Var58`,
    ##  `Var59`,
    ##  `Var60`,
    ##  `Var61`,
    ##  `Var62`,
    ##  `Var63`,
    ##  `Var64`,
    ##  `Var65`,
    ##  `Var66`,
    ##  `Var67`,
    ##  `Var68`,
    ##  `Var69`,
    ##  `Var70`,
    ##  `Var71`,
    ##  `Var72`,
    ##  `Var73`,
    ##  `Var74`,
    ##  `Var75`,
    ##  `Var76`,
    ##  `Var77`,
    ##  `Var78`,
    ##  `Var79`,
    ##  `Var80`,
    ##  `Var81`,
    ##  `Var82`,
    ##  `Var83`,
    ##  `Var84`,
    ##  `Var85`,
    ##  `Var86`,
    ##  `Var87`,
    ##  `Var88`,
    ##  `Var89`,
    ##  `Var90`,
    ##  `Var91`,
    ##  `Var92`,
    ##  `Var93`,
    ##  `Var94`,
    ##  `Var95`,
    ##  `Var96`,
    ##  `Var97`,
    ##  `Var98`,
    ##  `Var99`,
    ##  `Var100`,
    ##  `Var101`,
    ##  `Var102`,
    ##  `Var103`,
    ##  `Var104`,
    ##  `Var105`,
    ##  `Var106`,
    ##  `Var107`,
    ##  `Var108`,
    ##  `Var109`,
    ##  `Var110`,
    ##  `Var111`,
    ##  `Var112`,
    ##  `Var113`,
    ##  `Var114`,
    ##  `Var115`,
    ##  `Var116`,
    ##  `Var117`,
    ##  `Var118`,
    ##  `Var119`,
    ##  `Var120`,
    ##  `Var121`,
    ##  `Var122`,
    ##  `Var123`,
    ##  `Var124`,
    ##  `Var125`,
    ##  `Var126`,
    ##  `Var127`,
    ##  `Var128`,
    ##  `Var129`,
    ##  `Var130`,
    ##  `Var131`,
    ##  `Var132`,
    ##  `Var133`,
    ##  `Var134`,
    ##  `Var135`,
    ##  `Var136`,
    ##  `Var137`,
    ##  `Var138`,
    ##  `Var139`,
    ##  `Var140`,
    ##  `Var141`,
    ##  `Var142`,
    ##  `Var143`,
    ##  `Var144`,
    ##  `Var145`,
    ##  `Var146`,
    ##  `Var147`,
    ##  `Var148`,
    ##  `Var149`,
    ##  `Var150`,
    ##  `Var151`,
    ##  `Var152`,
    ##  `Var153`,
    ##  `Var154`,
    ##  `Var155`,
    ##  `Var156`,
    ##  `Var157`,
    ##  `Var158`,
    ##  `Var159`,
    ##  `Var160`,
    ##  `Var161`,
    ##  `Var162`,
    ##  `Var163`,
    ##  `Var164`,
    ##  `Var165`,
    ##  `Var166`,
    ##  `Var167`,
    ##  `Var168`,
    ##  `Var169`,
    ##  `Var170`,
    ##  `Var171`,
    ##  `Var172`,
    ##  `Var173`,
    ##  `Var174`,
    ##  `Var175`,
    ##  `Var176`,
    ##  `Var177`,
    ##  `Var178`,
    ##  `Var179`,
    ##  `Var180`,
    ##  `Var181`,
    ##  `Var182`,
    ##  `Var183`,
    ##  `Var184`,
    ##  `Var185`,
    ##  `Var186`,
    ##  `Var187`,
    ##  `Var188`,
    ##  `Var189`,
    ##  `Var190`,
    ##  `Var191`,
    ##  `Var192`,
    ##  `Var193`,
    ##  `Var194`,
    ##  `Var195`,
    ##  `Var196`,
    ##  `Var197`,
    ##  `Var198`,
    ##  `Var199`,
    ##  `Var200`,
    ##  `Var201`,
    ##  `Var202`,
    ##  `Var203`,
    ##  `Var204`,
    ##  `Var205`,
    ##  `Var206`,
    ##  `Var207`,
    ##  `Var208`,
    ##  `Var209`,
    ##  `Var210`,
    ##  `Var211`,
    ##  `Var212`,
    ##  `Var213`,
    ##  `Var214`,
    ##  `Var215`,
    ##  `Var216`,
    ##  `Var217`,
    ##  `Var218`,
    ##  `Var219`,
    ##  `Var220`,
    ##  `Var221`,
    ##  `Var222`,
    ##  `Var223`,
    ##  `Var224`,
    ##  `Var225`,
    ##  `Var226`,
    ##  `Var227`,
    ##  `Var228`,
    ##  `Var229`,
    ##  `Var230`,
    ##  `churn`,
    ##  rand ( )  AS `sample_col`
    ## FROM (
    ##  SELECT
    ##   `Var1`,
    ##   `Var2`,
    ##   `Var3`,
    ##   `Var4`,
    ##   `Var5`,
    ##   `Var6`,
    ##   `Var7`,
    ##   `Var8`,
    ##   `Var9`,
    ##   `Var10`,
    ##   `Var11`,
    ##   `Var12`,
    ##   `Var13`,
    ##   `Var14`,
    ##   `Var15`,
    ##   `Var16`,
    ##   `Var17`,
    ##   `Var18`,
    ##   `Var19`,
    ##   `Var20`,
    ##   `Var21`,
    ##   `Var22`,
    ##   `Var23`,
    ##   `Var24`,
    ##   `Var25`,
    ##   `Var26`,
    ##   `Var27`,
    ##   `Var28`,
    ##   `Var29`,
    ##   `Var30`,
    ##   `Var31`,
    ##   `Var32`,
    ##   `Var33`,
    ##   `Var34`,
    ##   `Var35`,
    ##   `Var36`,
    ##   `Var37`,
    ##   `Var38`,
    ##   `Var39`,
    ##   `Var40`,
    ##   `Var41`,
    ##   `Var42`,
    ##   `Var43`,
    ##   `Var44`,
    ##   `Var45`,
    ##   `Var46`,
    ##   `Var47`,
    ##   `Var48`,
    ##   `Var49`,
    ##   `Var50`,
    ##   `Var51`,
    ##   `Var52`,
    ##   `Var53`,
    ##   `Var54`,
    ##   `Var55`,
    ##   `Var56`,
    ##   `Var57`,
    ##   `Var58`,
    ##   `Var59`,
    ##   `Var60`,
    ##   `Var61`,
    ##   `Var62`,
    ##   `Var63`,
    ##   `Var64`,
    ##   `Var65`,
    ##   `Var66`,
    ##   `Var67`,
    ##   `Var68`,
    ##   `Var69`,
    ##   `Var70`,
    ##   `Var71`,
    ##   `Var72`,
    ##   `Var73`,
    ##   `Var74`,
    ##   `Var75`,
    ##   `Var76`,
    ##   `Var77`,
    ##   `Var78`,
    ##   `Var79`,
    ##   `Var80`,
    ##   `Var81`,
    ##   `Var82`,
    ##   `Var83`,
    ##   `Var84`,
    ##   `Var85`,
    ##   `Var86`,
    ##   `Var87`,
    ##   `Var88`,
    ##   `Var89`,
    ##   `Var90`,
    ##   `Var91`,
    ##   `Var92`,
    ##   `Var93`,
    ##   `Var94`,
    ##   `Var95`,
    ##   `Var96`,
    ##   `Var97`,
    ##   `Var98`,
    ##   `Var99`,
    ##   `Var100`,
    ##   `Var101`,
    ##   `Var102`,
    ##   `Var103`,
    ##   `Var104`,
    ##   `Var105`,
    ##   `Var106`,
    ##   `Var107`,
    ##   `Var108`,
    ##   `Var109`,
    ##   `Var110`,
    ##   `Var111`,
    ##   `Var112`,
    ##   `Var113`,
    ##   `Var114`,
    ##   `Var115`,
    ##   `Var116`,
    ##   `Var117`,
    ##   `Var118`,
    ##   `Var119`,
    ##   `Var120`,
    ##   `Var121`,
    ##   `Var122`,
    ##   `Var123`,
    ##   `Var124`,
    ##   `Var125`,
    ##   `Var126`,
    ##   `Var127`,
    ##   `Var128`,
    ##   `Var129`,
    ##   `Var130`,
    ##   `Var131`,
    ##   `Var132`,
    ##   `Var133`,
    ##   `Var134`,
    ##   `Var135`,
    ##   `Var136`,
    ##   `Var137`,
    ##   `Var138`,
    ##   `Var139`,
    ##   `Var140`,
    ##   `Var141`,
    ##   `Var142`,
    ##   `Var143`,
    ##   `Var144`,
    ##   `Var145`,
    ##   `Var146`,
    ##   `Var147`,
    ##   `Var148`,
    ##   `Var149`,
    ##   `Var150`,
    ##   `Var151`,
    ##   `Var152`,
    ##   `Var153`,
    ##   `Var154`,
    ##   `Var155`,
    ##   `Var156`,
    ##   `Var157`,
    ##   `Var158`,
    ##   `Var159`,
    ##   `Var160`,
    ##   `Var161`,
    ##   `Var162`,
    ##   `Var163`,
    ##   `Var164`,
    ##   `Var165`,
    ##   `Var166`,
    ##   `Var167`,
    ##   `Var168`,
    ##   `Var169`,
    ##   `Var170`,
    ##   `Var171`,
    ##   `Var172`,
    ##   `Var173`,
    ##   `Var174`,
    ##   `Var175`,
    ##   `Var176`,
    ##   `Var177`,
    ##   `Var178`,
    ##   `Var179`,
    ##   `Var180`,
    ##   `Var181`,
    ##   `Var182`,
    ##   `Var183`,
    ##   `Var184`,
    ##   `Var185`,
    ##   `Var186`,
    ##   `Var187`,
    ##   `Var188`,
    ##   `Var189`,
    ##   `Var190`,
    ##   `Var191`,
    ##   `Var192`,
    ##   `Var193`,
    ##   `Var194`,
    ##   `Var195`,
    ##   `Var196`,
    ##   `Var197`,
    ##   `Var198`,
    ##   `Var199`,
    ##   `Var200`,
    ##   `Var201`,
    ##   `Var202`,
    ##   `Var203`,
    ##   `Var204`,
    ##   `Var205`,
    ##   `Var206`,
    ##   `Var207`,
    ##   `Var208`,
    ##   `Var209`,
    ##   `Var210`,
    ##   `Var211`,
    ##   `Var212`,
    ##   `Var213`,
    ##   `Var214`,
    ##   `Var215`,
    ##   `Var216`,
    ##   `Var217`,
    ##   `Var218`,
    ##   `Var219`,
    ##   `Var220`,
    ##   `Var221`,
    ##   `Var222`,
    ##   `Var223`,
    ##   `Var224`,
    ##   `Var225`,
    ##   `Var226`,
    ##   `Var227`,
    ##   `Var228`,
    ##   `Var229`,
    ##   `Var230`,
    ##   `churn`
    ##  FROM
    ##   `kdd2009`
    ##  ) tsql_59895061063051043977_0000000000

``` r
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
# treat variables in small groups to manage memory
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
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_22701108987347141580_0000000004` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 25001 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  138 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var112_clean                          : num  88 0 64 16 56 32 0 0 0 16
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -709072 -87345 126870 -2135 168554 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -18 -30 4 -0.727 -22 ...
    ##  $ Var126_isBAD                          : int  0 0 0 1 0 1 0 0 0 0
    ##  $ Var13_clean                           : num  632 52 400 348 720 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var140_clean                          : num  205 990 110 0 805 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  18 9 9 9 9 ...
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var160_clean                          : num  76 2 30 0 4 32 12 0 8 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var189_clean                          : num  222 228 324 240 270 ...
    ##  $ Var192_catB                           : num  0.336 0.242 -0.472 -0.293 -1.206 ...
    ##  $ Var192_catP                           : num  0.00545 0.00754 0.00734 0.0062 0.0064 ...
    ##  $ Var193_catB                           : num  -0.49 -0.49 -0.49 -0.49 0.138 ...
    ##  $ Var193_catP                           : num  0.145 0.145 0.145 0.145 0.719 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 1 1 1 1 1 0
    ##  $ Var195_catP                           : num  0.959 0.959 0.959 0.959 0.959 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var198_catP                           : num  0.0006 0.0005 0.00025 0.00005 0.000849 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.01049 0.0048 0.00505 0.01449 0.00065 ...
    ##  $ Var200_catP                           : num  0.00005 0 0 0.00005 0.50797 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var202_catB                           : num  -6.69 2.52 0 -8.07 -8.77 ...
    ##  $ Var202_catP                           : num  5.00e-05 9.99e-05 0.00 2.00e-04 4.00e-04 ...
    ##  $ Var204_catB                           : num  0.1803 0.0477 0.2065 0.0317 -0.3352 ...
    ##  $ Var205_catB                           : num  -0.1462 -0.0213 -0.0213 -0.1462 0.6958 ...
    ##  $ Var205_catP                           : num  0.6369 0.2327 0.2327 0.6369 0.0927 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 1 0 1 1 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 0 0 1 0 0 0 0 1 1
    ##  $ Var206_catB                           : num  -0.51216 -0.00624 -0.71673 -0.00624 0.03559 ...
    ##  $ Var206_catP                           : num  0.0229 0.0359 0.0597 0.0359 0.0788 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 1 0 1 1
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.114 0.114 0.114 0.114 0.114 ...
    ##  $ Var207_catP                           : num  0.7 0.7 0.7 0.7 0.7 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var210_catB                           : num  -0.035 -0.035 -0.035 -0.035 -0.035 ...
    ##  $ Var210_catP                           : num  0.95 0.95 0.95 0.95 0.95 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  0 1 1 1 0 1 0 1 0 0
    ##  $ Var211_lev_x_Mtgm                     : int  1 0 0 0 1 0 1 0 1 1
    ##  $ Var212_catB                           : num  -0.473 -0.572 -0.473 -0.572 0.212 ...
    ##  $ Var212_catP                           : num  0.1302 0.0138 0.1302 0.0138 0.5846 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  0 0 0 0 1 1 0 1 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  1 0 1 0 0 0 1 0 0 0
    ##  $ Var214_catP                           : num  0.00005 0 0 0.00005 0.50797 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var216_catB                           : num  -0.53 -0.421 -0.378 -0.421 -8.767 ...
    ##  $ Var216_lev_x_XTbPUYD                  : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  0 2.52 0 -6.69 -8.77 ...
    ##  $ Var217_catP                           : num  0.00 9.99e-05 0.00 5.00e-05 4.00e-04 ...
    ##  $ Var218_catB                           : num  -0.243 0.129 -0.243 -0.243 0.129 ...
    ##  $ Var218_catP                           : num  0.512 0.474 0.512 0.512 0.474 ...
    ##  $ Var218_lev_x_cJvF                     : int  1 0 1 1 0 0 0 0 1 0
    ##  $ Var218_lev_x_UYBR                     : int  0 1 0 0 1 1 1 1 0 1
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  0.0006 0.0005 0.00025 0.00005 0.000849 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0934 0.0934 0.0934 0.0934 0.0934 ...
    ##  $ Var221_catP                           : num  0.739 0.739 0.739 0.739 0.739 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var222_catP                           : num  0.0006 0.0005 0.00025 0.00005 0.000849 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.39 -0.39 -0.39 -0.39 0.193 ...
    ##  $ Var225_catP                           : num  0.221 0.221 0.221 0.221 0.523 ...
    ##  $ Var225_lev_x_ELof                     : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var226_catB                           : num  0.137 -0.129 -0.221 -0.12 -0.128 ...
    ##  $ Var226_catP                           : num  0.0273 0.0288 0.024 0.0971 0.0516 ...
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  0.111 0.111 0.111 0.111 0.111 ...
    ##  $ Var227_catP                           : num  0.702 0.702 0.702 0.702 0.702 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.418 -0.418 -0.418 -0.418 0.167 ...
    ##  $ Var228_catP                           : num  0.0877 0.0877 0.0877 0.0877 0.6543 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 1 1 1 1 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  0.577 0.577 0.577 0.577 0.577 ...
    ##  $ Var229_catP                           : num  0.000799 0.000799 0.000799 0.000799 0.000799 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_clean                           : num  288 0 0 0 24 120 96 0 40 24
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var6_clean                            : num  2240 98 770 259 1162 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var65_clean                           : num  9 9 9 9 9 9 9 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var7_clean                            : num  7 7 7 0 7 7 7 7 0 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var72_clean                           : num  3 3 3 4.18 3 ...
    ##  $ Var73_clean                           : num  118 104 106 132 90 100 86 100 140 108
    ##  $ Var74_clean                           : num  7 63 35 0 224 140 35 168 0 0
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  36041 288493 146004 1692 94249 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
