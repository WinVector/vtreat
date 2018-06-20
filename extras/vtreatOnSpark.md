vtreat on Spark
================

``` r
library("vtreat")
packageVersion("vtreat")
```

    ## [1] '1.2.0'

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
    ##  ) tsql_80286711703950139957_0000000000

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

    ## [1] 173

``` r
rqplan <- as_rquery_plan(treatment_plans, var_restriction = newvars)
# ops <- flatten_fn_list(d_train, rqplan$optree_generators)
# cat(format(ops))
# ops %.>%
#   op_diagram(.) %.>% 
#   DiagrammeR::grViz(.)
# sql <- to_sql(ops, db)
# cat(sql)
d_train <- materialize_treated(db, rqplan, d_train, 
                               tmp_name_gen(), 
                               temporary = TRUE, overwrite = TRUE)
length(column_names(d_train))
```

    ## [1] 174

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_17188333575575532049_0000000004` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 25028 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  174 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 1
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -709072 -87345 126870 90994 168554 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -18 -30 4 -0.723 -22 ...
    ##  $ Var126_isBAD                          : int  0 0 0 1 0 1 1 0 0 1
    ##  $ Var13_clean                           : num  632 52 400 648 720 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  205 990 110 1350 805 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  18 9 9 9 9 0 27 18 54 9
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  222 228 324 269 269 ...
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var192_catB                           : num  0.182 0.182 -0.02 0.261 -0.153 ...
    ##  $ Var192_catP                           : num  0.00634 0.00749 0.00694 0.00322 0.00629 ...
    ##  $ Var193_catP                           : num  0.149 0.149 0.149 0.717 0.717 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 1 1 1 1 1 1 1
    ##  $ Var198_catP                           : num  0.000603 0.001006 0.000251 0 0.001006 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catP                           : num  0.01066 0.004224 0.005179 0.002263 0.000352 ...
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catP                           : num  1.01e-04 0.00 0.00 5.03e-05 5.07e-01 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 1 0 1 0 0 1
    ##  $ Var202_catP                           : num  0.000302 0.000151 0 0.000251 0.000402 ...
    ##  $ Var205_catB                           : num  -0.1272 -0.0681 -0.0681 -0.0681 0.6684 ...
    ##  $ Var205_catP                           : num  0.6403 0.2286 0.2286 0.2286 0.0919 ...
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 1 0 0 0 1 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 0 0 0 0 0 1 1 0 0
    ##  $ Var206_catB                           : num  -0.6106 -0.0971 -0.5743 -0.0821 -0.0821 ...
    ##  $ Var206_catP                           : num  0.023 0.0343 0.0621 0.0794 0.0794 ...
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catP                           : num  0.709 0.709 0.709 0.067 0.709 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 0 1 1 1 1 1 1
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0383 -0.0383 -0.0383 -0.0383 -0.0383 ...
    ##  $ Var210_catP                           : num  0.95 0.95 0.95 0.95 0.95 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 1 0 0
    ##  $ Var211_lev_x_L84s                     : int  0 1 1 1 0 1 1 1 0 1
    ##  $ Var211_lev_x_Mtgm                     : int  1 0 0 0 1 0 0 0 1 0
    ##  $ Var212_catP                           : num  0.1339 0.0146 0.1339 0.0572 0.5886 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  0 0 0 0 1 1 1 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  1 0 1 0 0 0 0 1 1 0
    ##  $ Var214_catB                           : num  -7.374 0 0 -6.681 0.143 ...
    ##  $ Var214_catP                           : num  1.01e-04 0.00 0.00 5.03e-05 5.07e-01 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 1 0 1 0 0 1
    ##  $ Var216_catB                           : num  -0.693 -0.528 -0.128 -1.159 0.227 ...
    ##  $ Var216_lev_x_XTbPUYD                  : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catP                           : num  5.03e-05 1.51e-04 0.00 5.03e-05 4.02e-04 ...
    ##  $ Var218_catB                           : num  -0.232 0.136 -0.232 -0.232 0.136 ...
    ##  $ Var218_catP                           : num  0.508 0.477 0.508 0.508 0.477 ...
    ##  $ Var218_lev_x_cJvF                     : int  1 0 1 1 0 0 0 0 0 1
    ##  $ Var218_lev_x_UYBR                     : int  0 1 0 0 1 1 1 1 1 0
    ##  $ Var219_catP                           : num  0.806 0.806 0.806 0.806 0.806 ...
    ##  $ Var219_lev_x_FzaX                     : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catP                           : num  0.000603 0.001006 0.000251 0 0.001006 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0818 0.0818 0.0818 -0.4963 0.0818 ...
    ##  $ Var221_catP                           : num  0.7485 0.7485 0.7485 0.0587 0.7485 ...
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 0 1 1 1 1 1 1
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catP                           : num  0.000603 0.001006 0.000251 0 0.001006 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var225_catB                           : num  -0.309 -0.309 -0.309 -0.309 0.139 ...
    ##  $ Var225_catP                           : num  0.22 0.22 0.22 0.22 0.523 ...
    ##  $ Var225_lev_x_ELof                     : int  1 1 1 1 0 0 0 0 0 0
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 1 0 1 0 0 1
    ##  $ Var226_catB                           : num  0.253669 -0.156707 -0.164676 -0.000375 0.043548 ...
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var227_catB                           : num  0.109 0.109 0.109 -0.322 0.109 ...
    ##  $ Var227_catP                           : num  0.7102 0.7102 0.7102 0.0433 0.7102 ...
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 0 1 1 1 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.335 -0.335 -0.335 0.148 0.148 ...
    ##  $ Var228_catP                           : num  0.0895 0.0895 0.0895 0.6534 0.6534 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 1 1 1 1 1 1 1
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  -9.08 -9.08 -9.08 -9.08 -9.08 ...
    ##  $ Var229_catP                           : num  0.000553 0.000553 0.000553 0.000553 0.000553 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var65_clean                           : num  9 9 9 18 9 9 27 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  7 7 7 14 7 7 7 7 7 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var73_clean                           : num  118 104 106 84 90 100 90 92 86 80
    ##  $ Var74_clean                           : num  7 63 35 35 224 140 175 0 35 189
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var81_clean                           : num  36041 288493 146004 181203 94249 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
