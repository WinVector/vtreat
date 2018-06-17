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

    ## [1] '0.5.0'

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
    ##   `kdd2009`.`Var1`,
    ##   `kdd2009`.`Var2`,
    ##   `kdd2009`.`Var3`,
    ##   `kdd2009`.`Var4`,
    ##   `kdd2009`.`Var5`,
    ##   `kdd2009`.`Var6`,
    ##   `kdd2009`.`Var7`,
    ##   `kdd2009`.`Var8`,
    ##   `kdd2009`.`Var9`,
    ##   `kdd2009`.`Var10`,
    ##   `kdd2009`.`Var11`,
    ##   `kdd2009`.`Var12`,
    ##   `kdd2009`.`Var13`,
    ##   `kdd2009`.`Var14`,
    ##   `kdd2009`.`Var15`,
    ##   `kdd2009`.`Var16`,
    ##   `kdd2009`.`Var17`,
    ##   `kdd2009`.`Var18`,
    ##   `kdd2009`.`Var19`,
    ##   `kdd2009`.`Var20`,
    ##   `kdd2009`.`Var21`,
    ##   `kdd2009`.`Var22`,
    ##   `kdd2009`.`Var23`,
    ##   `kdd2009`.`Var24`,
    ##   `kdd2009`.`Var25`,
    ##   `kdd2009`.`Var26`,
    ##   `kdd2009`.`Var27`,
    ##   `kdd2009`.`Var28`,
    ##   `kdd2009`.`Var29`,
    ##   `kdd2009`.`Var30`,
    ##   `kdd2009`.`Var31`,
    ##   `kdd2009`.`Var32`,
    ##   `kdd2009`.`Var33`,
    ##   `kdd2009`.`Var34`,
    ##   `kdd2009`.`Var35`,
    ##   `kdd2009`.`Var36`,
    ##   `kdd2009`.`Var37`,
    ##   `kdd2009`.`Var38`,
    ##   `kdd2009`.`Var39`,
    ##   `kdd2009`.`Var40`,
    ##   `kdd2009`.`Var41`,
    ##   `kdd2009`.`Var42`,
    ##   `kdd2009`.`Var43`,
    ##   `kdd2009`.`Var44`,
    ##   `kdd2009`.`Var45`,
    ##   `kdd2009`.`Var46`,
    ##   `kdd2009`.`Var47`,
    ##   `kdd2009`.`Var48`,
    ##   `kdd2009`.`Var49`,
    ##   `kdd2009`.`Var50`,
    ##   `kdd2009`.`Var51`,
    ##   `kdd2009`.`Var52`,
    ##   `kdd2009`.`Var53`,
    ##   `kdd2009`.`Var54`,
    ##   `kdd2009`.`Var55`,
    ##   `kdd2009`.`Var56`,
    ##   `kdd2009`.`Var57`,
    ##   `kdd2009`.`Var58`,
    ##   `kdd2009`.`Var59`,
    ##   `kdd2009`.`Var60`,
    ##   `kdd2009`.`Var61`,
    ##   `kdd2009`.`Var62`,
    ##   `kdd2009`.`Var63`,
    ##   `kdd2009`.`Var64`,
    ##   `kdd2009`.`Var65`,
    ##   `kdd2009`.`Var66`,
    ##   `kdd2009`.`Var67`,
    ##   `kdd2009`.`Var68`,
    ##   `kdd2009`.`Var69`,
    ##   `kdd2009`.`Var70`,
    ##   `kdd2009`.`Var71`,
    ##   `kdd2009`.`Var72`,
    ##   `kdd2009`.`Var73`,
    ##   `kdd2009`.`Var74`,
    ##   `kdd2009`.`Var75`,
    ##   `kdd2009`.`Var76`,
    ##   `kdd2009`.`Var77`,
    ##   `kdd2009`.`Var78`,
    ##   `kdd2009`.`Var79`,
    ##   `kdd2009`.`Var80`,
    ##   `kdd2009`.`Var81`,
    ##   `kdd2009`.`Var82`,
    ##   `kdd2009`.`Var83`,
    ##   `kdd2009`.`Var84`,
    ##   `kdd2009`.`Var85`,
    ##   `kdd2009`.`Var86`,
    ##   `kdd2009`.`Var87`,
    ##   `kdd2009`.`Var88`,
    ##   `kdd2009`.`Var89`,
    ##   `kdd2009`.`Var90`,
    ##   `kdd2009`.`Var91`,
    ##   `kdd2009`.`Var92`,
    ##   `kdd2009`.`Var93`,
    ##   `kdd2009`.`Var94`,
    ##   `kdd2009`.`Var95`,
    ##   `kdd2009`.`Var96`,
    ##   `kdd2009`.`Var97`,
    ##   `kdd2009`.`Var98`,
    ##   `kdd2009`.`Var99`,
    ##   `kdd2009`.`Var100`,
    ##   `kdd2009`.`Var101`,
    ##   `kdd2009`.`Var102`,
    ##   `kdd2009`.`Var103`,
    ##   `kdd2009`.`Var104`,
    ##   `kdd2009`.`Var105`,
    ##   `kdd2009`.`Var106`,
    ##   `kdd2009`.`Var107`,
    ##   `kdd2009`.`Var108`,
    ##   `kdd2009`.`Var109`,
    ##   `kdd2009`.`Var110`,
    ##   `kdd2009`.`Var111`,
    ##   `kdd2009`.`Var112`,
    ##   `kdd2009`.`Var113`,
    ##   `kdd2009`.`Var114`,
    ##   `kdd2009`.`Var115`,
    ##   `kdd2009`.`Var116`,
    ##   `kdd2009`.`Var117`,
    ##   `kdd2009`.`Var118`,
    ##   `kdd2009`.`Var119`,
    ##   `kdd2009`.`Var120`,
    ##   `kdd2009`.`Var121`,
    ##   `kdd2009`.`Var122`,
    ##   `kdd2009`.`Var123`,
    ##   `kdd2009`.`Var124`,
    ##   `kdd2009`.`Var125`,
    ##   `kdd2009`.`Var126`,
    ##   `kdd2009`.`Var127`,
    ##   `kdd2009`.`Var128`,
    ##   `kdd2009`.`Var129`,
    ##   `kdd2009`.`Var130`,
    ##   `kdd2009`.`Var131`,
    ##   `kdd2009`.`Var132`,
    ##   `kdd2009`.`Var133`,
    ##   `kdd2009`.`Var134`,
    ##   `kdd2009`.`Var135`,
    ##   `kdd2009`.`Var136`,
    ##   `kdd2009`.`Var137`,
    ##   `kdd2009`.`Var138`,
    ##   `kdd2009`.`Var139`,
    ##   `kdd2009`.`Var140`,
    ##   `kdd2009`.`Var141`,
    ##   `kdd2009`.`Var142`,
    ##   `kdd2009`.`Var143`,
    ##   `kdd2009`.`Var144`,
    ##   `kdd2009`.`Var145`,
    ##   `kdd2009`.`Var146`,
    ##   `kdd2009`.`Var147`,
    ##   `kdd2009`.`Var148`,
    ##   `kdd2009`.`Var149`,
    ##   `kdd2009`.`Var150`,
    ##   `kdd2009`.`Var151`,
    ##   `kdd2009`.`Var152`,
    ##   `kdd2009`.`Var153`,
    ##   `kdd2009`.`Var154`,
    ##   `kdd2009`.`Var155`,
    ##   `kdd2009`.`Var156`,
    ##   `kdd2009`.`Var157`,
    ##   `kdd2009`.`Var158`,
    ##   `kdd2009`.`Var159`,
    ##   `kdd2009`.`Var160`,
    ##   `kdd2009`.`Var161`,
    ##   `kdd2009`.`Var162`,
    ##   `kdd2009`.`Var163`,
    ##   `kdd2009`.`Var164`,
    ##   `kdd2009`.`Var165`,
    ##   `kdd2009`.`Var166`,
    ##   `kdd2009`.`Var167`,
    ##   `kdd2009`.`Var168`,
    ##   `kdd2009`.`Var169`,
    ##   `kdd2009`.`Var170`,
    ##   `kdd2009`.`Var171`,
    ##   `kdd2009`.`Var172`,
    ##   `kdd2009`.`Var173`,
    ##   `kdd2009`.`Var174`,
    ##   `kdd2009`.`Var175`,
    ##   `kdd2009`.`Var176`,
    ##   `kdd2009`.`Var177`,
    ##   `kdd2009`.`Var178`,
    ##   `kdd2009`.`Var179`,
    ##   `kdd2009`.`Var180`,
    ##   `kdd2009`.`Var181`,
    ##   `kdd2009`.`Var182`,
    ##   `kdd2009`.`Var183`,
    ##   `kdd2009`.`Var184`,
    ##   `kdd2009`.`Var185`,
    ##   `kdd2009`.`Var186`,
    ##   `kdd2009`.`Var187`,
    ##   `kdd2009`.`Var188`,
    ##   `kdd2009`.`Var189`,
    ##   `kdd2009`.`Var190`,
    ##   `kdd2009`.`Var191`,
    ##   `kdd2009`.`Var192`,
    ##   `kdd2009`.`Var193`,
    ##   `kdd2009`.`Var194`,
    ##   `kdd2009`.`Var195`,
    ##   `kdd2009`.`Var196`,
    ##   `kdd2009`.`Var197`,
    ##   `kdd2009`.`Var198`,
    ##   `kdd2009`.`Var199`,
    ##   `kdd2009`.`Var200`,
    ##   `kdd2009`.`Var201`,
    ##   `kdd2009`.`Var202`,
    ##   `kdd2009`.`Var203`,
    ##   `kdd2009`.`Var204`,
    ##   `kdd2009`.`Var205`,
    ##   `kdd2009`.`Var206`,
    ##   `kdd2009`.`Var207`,
    ##   `kdd2009`.`Var208`,
    ##   `kdd2009`.`Var209`,
    ##   `kdd2009`.`Var210`,
    ##   `kdd2009`.`Var211`,
    ##   `kdd2009`.`Var212`,
    ##   `kdd2009`.`Var213`,
    ##   `kdd2009`.`Var214`,
    ##   `kdd2009`.`Var215`,
    ##   `kdd2009`.`Var216`,
    ##   `kdd2009`.`Var217`,
    ##   `kdd2009`.`Var218`,
    ##   `kdd2009`.`Var219`,
    ##   `kdd2009`.`Var220`,
    ##   `kdd2009`.`Var221`,
    ##   `kdd2009`.`Var222`,
    ##   `kdd2009`.`Var223`,
    ##   `kdd2009`.`Var224`,
    ##   `kdd2009`.`Var225`,
    ##   `kdd2009`.`Var226`,
    ##   `kdd2009`.`Var227`,
    ##   `kdd2009`.`Var228`,
    ##   `kdd2009`.`Var229`,
    ##   `kdd2009`.`Var230`,
    ##   `kdd2009`.`churn`
    ##  FROM
    ##   `kdd2009`
    ##  ) tsql_22856112863489964124_0000000000

``` r
d <- materialize(db, d, table_name = tmp_name_gen())

y_name <- "churn"
vars <- setdiff(column_names(d), c(y_name, "sample_col"))

d_train <- d %.>%
  select_rows_nse(., sample_col <= 0.8) %.>%
  materialize(db, ., table_name = tmp_name_gen()) # should not need to materialize this, doign this to debug

d_test <- d %.>%
  select_rows_nse(., sample_col > 0.9)

d_treat <- d %.>%
  select_rows_nse(., (sample_col > 0.8) & (sample_col <= 0.9)) %.>%
  execute(db, .)
```

``` r
cl = parallel::makeCluster(4)
tp <- vtreat::designTreatmentsC(d_treat, vars, y_name, 1, parallelCluster = cl)
```

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 17:26:33 2018"
    ## [1] "designing treatments Sat Jun 16 17:26:33 2018"
    ## [1] " have initial level statistics Sat Jun 16 17:26:33 2018"
    ## [1] " scoring treatments Sat Jun 16 17:26:35 2018"
    ## [1] "have treatment plan Sat Jun 16 17:26:41 2018"
    ## [1] "rescoring complex variables Sat Jun 16 17:26:41 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 17:26:45 2018"

``` r
# newvars <- tp$scoreFrame$varName[tp$scoreFrame$sig < 1/nrow(tp$scoreFrame)]
rqplan <- as_rquery(tp)
```

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var192'(character,character)->character-
    ## >'Var192_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var193'(character,character)->character-
    ## >'Var193_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var194'(character,character)->character-
    ## >'Var194_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var195'(character,character)->character-
    ## >'Var195_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var196'(character,character)->character-
    ## >'Var196_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var197'(character,character)->character-
    ## >'Var197_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var198'(character,character)->character-
    ## >'Var198_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var199'(character,character)->character-
    ## >'Var199_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var200'(character,character)->character-
    ## >'Var200_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var202'(character,character)->character-
    ## >'Var202_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var203'(character,character)->character-
    ## >'Var203_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var204'(character,character)->character-
    ## >'Var204_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var205'(character,character)->character-
    ## >'Var205_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var206'(character,character)->character-
    ## >'Var206_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var207'(character,character)->character-
    ## >'Var207_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var208'(character,character)->character-
    ## >'Var208_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var210'(character,character)->character-
    ## >'Var210_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var212'(character,character)->character-
    ## >'Var212_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var214'(character,character)->character-
    ## >'Var214_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var216'(character,character)->character-
    ## >'Var216_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var217'(character,character)->character-
    ## >'Var217_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var218'(character,character)->character-
    ## >'Var218_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var219'(character,character)->character-
    ## >'Var219_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var220'(character,character)->character-
    ## >'Var220_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var221'(character,character)->character-
    ## >'Var221_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var222'(character,character)->character-
    ## >'Var222_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var223'(character,character)->character-
    ## >'Var223_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var225'(character,character)->character-
    ## >'Var225_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var226'(character,character)->character-
    ## >'Var226_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var227'(character,character)->character-
    ## >'Var227_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var228'(character,character)->character-
    ## >'Var228_catP') , class vtreatment

    ## Warning in as_rquery.vtreatment(ti): vtreat::as_rquery not yet implemented
    ## for vtreat 'Prevalence Code'('Var229'(character,character)->character-
    ## >'Var229_catP') , class vtreatment

``` r
# ops <- flatten_fn_list(d_train, rqplan$optree_generators)
# cat(format(ops))
# sql <- to_sql(ops, db)
d_train <- materialize_treated(db, rqplan, d_train, 
                               tmp_name_gen(), 
                               temporary = TRUE, overwrite = TRUE)
length(column_names(d_train))
```

    ## [1] 505

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_69937689504551982431_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 40086 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  505 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 1 -1 -1
    ##  $ Var1_clean                            : num  15.9 15.9 15.9 15.9 15.9 ...
    ##  $ Var1_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var10_clean                           : num  344938 344938 344938 344938 344938 ...
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var100_clean                          : num  0.563 0.563 0.563 0.563 0.563 ...
    ##  $ Var100_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var101_clean                          : num  20.9 20.9 20.9 20.9 20.9 ...
    ##  $ Var101_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var102_clean                          : num  30655 30655 30655 30655 30655 ...
    ##  $ Var102_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_clean                          : num  8.9 8.9 8.9 8.9 8.9 ...
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var104_clean                          : num  122 122 122 122 122 ...
    ##  $ Var104_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var105_clean                          : num  81 81 81 81 81 ...
    ##  $ Var105_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_clean                          : num  80887 80887 80887 80887 80887 ...
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_clean                          : num  2.7 2.7 2.7 2.7 2.7 ...
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var108_clean                          : num  170353 170353 170353 170353 170353 ...
    ##  $ Var108_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var109_clean                          : num  40 16 32 80 8 0 32 24 32 8
    ##  $ Var109_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var11_clean                           : num  8.67 8.67 8.67 8.67 8.67 ...
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var110_clean                          : num  6.76 6.76 6.76 6.76 6.76 ...
    ##  $ Var110_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_clean                          : num  203540 203540 203540 203540 203540 ...
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_clean                          : num  64 16 16 88 0 8 32 40 0 0
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  126870 -2135 -38397 -709072 -87345 ...
    ##  $ Var114_clean                          : num  599388 599388 599388 599388 599388 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var115_clean                          : num  41.2 41.2 41.2 41.2 41.2 ...
    ##  $ Var115_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var116_clean                          : num  0.138 0.138 0.138 0.138 0.138 ...
    ##  $ Var116_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_clean                          : num  106183 106183 106183 106183 106183 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var118_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_clean                          : num  580 20 610 1480 250 90 575 445 515 315
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var12_clean                           : num  27.7 27.7 27.7 27.7 27.7 ...
    ##  $ Var12_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var120_clean                          : num  14.4 14.4 14.4 14.4 14.4 ...
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var121_clean                          : num  6.9 6.9 6.9 6.9 6.9 ...
    ##  $ Var121_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_clean                          : num  0.0916 0.0916 0.0916 0.0916 0.0916 ...
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_clean                          : num  42 0 108 90 12 12 66 30 0 12
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_clean                          : num  265303 265303 265303 265303 265303 ...
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_clean                          : num  48645 2574 189765 333 41094 ...
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  4 -0.939 -26 -18 -30 ...
    ##  $ Var126_isBAD                          : int  0 1 0 0 0 0 1 1 0 0
    ##  $ Var127_clean                          : num  24.1 24.1 24.1 24.1 24.1 ...
    ##  $ Var127_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var128_clean                          : num  73.9 73.9 73.9 73.9 73.9 ...
    ##  $ Var128_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var129_clean                          : num  14.4 14.4 14.4 14.4 14.4 ...
    ##  $ Var129_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var13_clean                           : num  400 348 2968 632 52 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_clean                          : num  0.481 0.481 0.481 0.481 0.481 ...
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var131_clean                          : num  2198418 2198418 2198418 2198418 2198418 ...
    ##  $ Var131_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_clean                          : num  6480000 0 2417555 2881310 1762675 ...
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_clean                          : num  0 0 286546 512588 0 ...
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_clean                          : num  215 215 215 215 215 ...
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var136_clean                          : num  126098 126098 126098 126098 126098 ...
    ##  $ Var136_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var137_clean                          : num  3.95 3.95 3.95 3.95 3.95 ...
    ##  $ Var137_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_clean                          : num  117428 117428 117428 117428 117428 ...
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_clean                           : num  0.901 0.901 0.901 0.901 0.901 ...
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  110 0 11845 205 990 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var142_clean                          : num  0.92 0.92 0.92 0.92 0.92 ...
    ##  $ Var142_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var143_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  9 9 36 18 9 18 9 9 18 54
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var145_clean                          : num  47.3 47.3 47.3 47.3 47.3 ...
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var146_clean                          : num  2.48 2.48 2.48 2.48 2.48 ...
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_clean                          : num  1.92 1.92 1.92 1.92 1.92 ...
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_clean                          : num  65.1 65.1 65.1 65.1 65.1 ...
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var149_clean                          : num  98707 0 985208 213220 0 ...
    ##  $ Var149_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var150_clean                          : num  183274 183274 183274 183274 183274 ...
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var151_clean                          : num  8.69 8.69 8.69 8.69 8.69 ...
    ##  $ Var151_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_clean                          : num  8.07 8.07 8.07 8.07 8.07 ...
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_clean                          : num  10641120 17016 10522600 7305080 6920680 ...
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var154_clean                          : num  1781848 1781848 1781848 1781848 1781848 ...
    ##  $ Var154_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var155_clean                          : num  0.819 0.819 0.819 0.819 0.819 ...
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var156_clean                          : num  234 234 234 234 234 ...
    ##  $ Var156_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var157_clean                          : num  14.6 14.6 14.6 14.6 14.6 ...
    ##  $ Var157_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var158_clean                          : num  2.27 2.27 2.27 2.27 2.27 ...
    ##  $ Var158_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_clean                          : num  4.95 4.95 4.95 4.95 4.95 ...
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_clean                           : num  117 117 117 117 117 ...
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_clean                          : num  30 0 16 76 2 2 32 38 2 12
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_clean                          : num  3.16 3.16 3.16 3.16 3.16 ...
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var162_clean                          : num  378328 378328 378328 378328 378328 ...
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_clean                          : num  0 0 326064 767406 1008234 ...
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_clean                          : num  2.6 2.6 2.6 2.6 2.6 ...
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var165_clean                          : num  34455 34455 34455 34455 34455 ...
    ##  $ Var165_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var166_clean                          : num  10.3 10.3 10.3 10.3 10.3 ...
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var168_clean                          : num  355 355 355 355 355 ...
    ##  $ Var168_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_clean                           : num  9.18 9.18 9.18 9.18 9.18 ...
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_clean                          : num  2.27 2.27 2.27 2.27 2.27 ...
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var171_clean                          : num  495023 495023 495023 495023 495023 ...
    ##  $ Var171_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_clean                          : num  9.93 9.93 9.93 9.93 9.93 ...
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_clean                          : num  4.02 4.02 4.02 4.02 4.02 ...
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var176_clean                          : num  4.46 4.46 4.46 4.46 4.46 ...
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_clean                          : num  595112 595112 595112 595112 595112 ...
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var178_clean                          : num  18.4 18.4 18.4 18.4 18.4 ...
    ##  $ Var178_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_clean                          : num  1.73 1.73 1.73 1.73 1.73 ...
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_clean                           : num  5.89 5.89 5.89 5.89 5.89 ...
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var180_clean                          : num  4620355 4620355 4620355 4620355 4620355 ...
    ##  $ Var180_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_clean                          : num  1409470 1409470 1409470 1409470 1409470 ...
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var183_clean                          : num  82040 82040 82040 82040 82040 ...
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_clean                          : num  7.39 7.39 7.39 7.39 7.39 ...
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var186_clean                          : num  3.45 3.45 3.45 3.45 3.45 ...
    ##  $ Var186_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var187_clean                          : num  26 26 26 26 26 ...
    ##  $ Var187_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_clean                          : num  164 164 164 164 164 ...
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  324 240 354 222 228 ...
    ##  $ Var189_isBAD                          : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var19_clean                           : num  0.211 0.211 0.211 0.211 0.211 ...
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var190_clean                          : num  16847 16847 16847 16847 16847 ...
    ##  $ Var190_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_r_I                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var192_catB                           : num  -0.746 -9.9564 -0.0201 -0.6793 -0.1453 ...
    ##  $ Var193_catB                           : num  -0.305 -0.305 -0.305 -0.305 -0.305 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 1 1 0 0 0 0
    ##  $ Var193_lev_x_AERks4l                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 0 0 1 1 1 1
    ##  $ Var194_catB                           : num  -0.0489 -0.0489 0.1551 -0.0489 -0.0489 ...
    ##  $ Var194_lev_x_NA                       : int  1 1 0 1 1 0 1 1 0 0
    ##  $ Var194_lev_x_SEuy                     : int  0 0 1 0 0 1 0 0 1 1
    ##  $ Var195_catB                           : num  0.00457 0.00457 0.00457 0.00457 0.00457 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var196_catB                           : num  -0.000897 -0.000897 -0.000897 -0.000897 -0.000897 ...
    ##  $ Var196_lev_x_1K8T                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var197_catB                           : num  -0.2024 0.1505 0.1505 0.0767 -0.2024 ...
    ##  $ Var197_lev_x_0Xwj                     : int  0 1 1 0 0 0 0 1 0 0
    ##  $ Var197_lev_x_487l                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_JLbT                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_lK27                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var197_lev_x_ssAy                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var197_lev_x_TyGl                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_catB                           : num  0 0 1.99 -7.22 -7.22 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                           : num  -0.0201 -1.1728 -0.433 -0.3566 -9.567 ...
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catB                           : num  0 0 -6.52 0 0 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var201_lev_x_NA                       : int  1 1 0 1 1 0 1 1 0 0
    ##  $ Var201_lev_x_smXZ                     : int  0 0 1 0 0 1 0 0 1 1
    ##  $ Var202_catB                           : num  0 -7.62 -7.22 -7.22 0 ...
    ##  $ Var203_catB                           : num  -0.0128 0 0.1229 -0.0128 -0.0128 ...
    ##  $ Var203_lev_x_9_Y1                     : int  1 0 0 1 1 1 1 0 1 1
    ##  $ Var203_lev_x_F3hy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var203_lev_x_HLqf                     : int  0 0 1 0 0 0 0 1 0 0
    ##  $ Var204_catB                           : num  0.29 -0.23 -0.501 -0.291 -9.89 ...
    ##  $ Var204_lev_x_15m3                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_7WNq                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_k13i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_m_h1                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_MBhA                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_RcM7                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var204_lev_x_RVjC                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_SkZj                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_Y9Bl                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_z5Ry                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_catB                           : num  -0.1286 -0.0751 -0.0751 -0.0751 -0.1286 ...
    ##  $ Var205_lev_x_09_Q                     : int  1 0 0 0 1 1 1 1 0 0
    ##  $ Var205_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var205_lev_x_VpdQ                     : int  0 1 1 1 0 0 0 0 1 0
    ##  $ Var206_catB                           : num  -0.232 -0.626 -1.643 -2.003 -0.626 ...
    ##  $ Var206_lev_x_43pnToF                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_6JmL                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  1 0 0 0 0 1 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_kxE9                     : int  0 0 0 0 0 0 0 1 1 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_sYC_                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var206_lev_x_wMei                     : int  0 1 0 0 1 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.11 0.11 0.11 0.11 0.11 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var207_lev_x_Kxdu                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var207_lev_x_NKv3VA1BpP               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var208_catB                           : num  -0.00796 0.13122 0.13122 -0.00796 -0.00796 ...
    ##  $ Var208_lev_x_kIsH                     : int  1 0 0 1 1 1 1 0 1 1
    ##  $ Var208_lev_x_sBgB                     : int  0 1 1 0 0 0 0 1 0 0
    ##  $ Var21_clean                           : num  128 8 152 472 20 20 140 156 124 92
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0258 -0.0258 -0.0258 -0.0258 -0.0258 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 0 1 0
    ##  $ Var211_lev_x_L84s                     : int  1 1 1 0 1 1 1 1 1 0
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 0 1 0 0 0 0 0 1
    ##  $ Var212_catB                           : num  -0.271 -1.561 0.195 -0.271 -1.561 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var212_lev_x_FMSzZ91zL2               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_Ie_5MZs                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  1 0 0 1 0 1 0 0 1 1
    ##  $ Var213_lev_x_KdSa                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var213_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var214_catB                           : num  0 0 -6.52 0 0 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var215_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var216_catB                           : num  -0.324 -1.39 0.608 -0.166 -1.39 ...
    ##  $ Var216_lev_x_11p4mKe                  : int  1 0 0 0 0 1 0 0 1 0
    ##  $ Var216_lev_x_beK4AFX                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_kZJtVhC                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_kZJyVg2                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAja5EA                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAjbk_S                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_NGZxnJM                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  0 0 -7.22 0 0 ...
    ##  $ Var218_catB                           : num  -0.1768 -0.1768 0.0927 -0.1768 0.0927 ...
    ##  $ Var218_lev_x_cJvF                     : int  1 1 0 1 0 0 1 1 0 0
    ##  $ Var218_lev_x_UYBR                     : int  0 0 1 0 1 1 0 0 1 1
    ##  $ Var219_catB                           : num  0.0369 0.0369 0.0369 0.0369 0.0369 ...
    ##  $ Var219_lev_x_AU8pNoi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_FzaX                     : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var219_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_qxDb                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var22_clean                           : num  160 10 190 590 25 25 175 195 155 115
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catB                           : num  0 0 1.99 -7.22 -7.22 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0885 0.0885 0.0885 0.0885 0.0885 ...
    ##  $ Var221_lev_x_Al6ZaUT                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catB                           : num  0 0 1.99 -7.22 -7.22 ...
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_catB                           : num  -0.021 -0.021 -0.021 -0.021 -0.021 ...
    ##  $ Var223_lev_x_jySVZNlOJy               : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var223_lev_x_LM8l689qOp               : int  1 1 1 1 1 1 1 1 0 0
    ##  $ Var223_lev_x_M_8D                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var224_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var225_catB                           : num  -0.305 -0.305 0.0969 -0.305 -0.305 ...
    ##  $ Var225_lev_x_ELof                     : int  1 1 0 1 1 0 1 0 0 0
    ##  $ Var225_lev_x_kG3k                     : int  0 0 1 0 0 1 0 0 1 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var225_lev_x_xG3x                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_catB                           : num  0.04 -0.294 0.172 -0.16 -0.241 ...
    ##  $ Var226_lev_x_3Cy4                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_453m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_5Acm                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7aLG                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Aoh3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_fKCe                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_kwS7                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_me1d                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var226_lev_x_PM2D                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qcbd                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var226_lev_x_Qu4f                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_rgKb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 0 0 0 0 1 0 0 1 1
    ##  $ Var226_lev_x_uWr3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_WqMG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Xa3G                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var226_lev_x_xb3V                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  0.108 0.108 0.108 0.108 0.108 ...
    ##  $ Var227_lev_x_02N6s8f                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_6fzt                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.207 -0.207 -0.207 -0.207 -0.207 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 1 1 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 0 1 1 1 1
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_xwM2aC7IdeMC0            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  -7.91 -7.91 -7.91 -7.91 -7.91 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_clean                           : num  3.37 3.37 3.37 3.37 3.37 ...
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var24_clean                           : num  2 0 4 6 0 0 2 4 0 0
    ##  $ Var24_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_clean                           : num  0 0 32 288 0 8 8 40 176 96
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_clean                           : num  0.0698 0.0698 0.0698 0.0698 0.0698 ...
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_clean                           : num  0.031 0.031 0.031 0.031 0.031 ...
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_clean                           : num  220 167 315 200 187 ...
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var29_clean                           : num  0.023 0.023 0.023 0.023 0.023 ...
    ##  $ Var29_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var3_clean                            : num  225 225 225 225 225 ...
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var30_clean                           : num  7.47 7.47 7.47 7.47 7.47 ...
    ##  $ Var30_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var33_clean                           : num  95612 95612 95612 95612 95612 ...
    ##  $ Var33_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_clean                           : num  1.59 1.59 1.59 1.59 1.59 ...
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_clean                           : num  142457 142457 142457 142457 142457 ...
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var37_clean                           : int  572252 572252 572252 572252 572252 572252 572252 572252 572252 572252
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_clean                           : num  3628806 0 8068320 2726190 7257600 ...
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_clean                            : num  0.105 0.105 0.105 0.105 0.105 ...
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var40_clean                           : num  12.9 12.9 12.9 12.9 12.9 ...
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var41_clean                           : num  36.1 36.1 36.1 36.1 36.1 ...
    ##  $ Var41_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_clean                           : num  6.64 6.64 6.64 6.64 6.64 ...
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var45_clean                           : num  6835 6835 6835 6835 6835 ...
    ##  $ Var45_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var46_clean                           : num  19 19 19 19 19 ...
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var47_clean                           : num  1.93 1.93 1.93 1.93 1.93 ...
    ##  $ Var47_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_clean                           : num  0.183 0.183 0.183 0.183 0.183 ...
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var5_clean                            : num  150135 150135 150135 150135 150135 ...
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var50_clean                           : num  36.6 36.6 36.6 36.6 36.6 ...
    ##  $ Var50_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var51_clean                           : num  40070 40070 40070 40070 40070 ...
    ##  $ Var51_isBAD                           : int  1 1 1 1 1 1 1 1 1 0
    ##  $ Var53_clean                           : num  948491 948491 948491 948491 948491 ...
    ##  $ Var53_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_clean                           : num  5.44 5.44 5.44 5.44 5.44 ...
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var56_clean                           : num  93451 93451 93451 93451 93451 ...
    ##  $ Var56_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var57_clean                           : num  3.43 1.22 6.71 5.52 1.8 ...
    ##  $ Var58_clean                           : num  208770 208770 208770 208770 208770 ...
    ##  $ Var58_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var59_clean                           : num  407848 407848 407848 407848 407848 ...
    ##  $ Var59_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_clean                            : num  770 259 1589 2240 98 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var60_clean                           : num  4.28 4.28 4.28 4.28 4.28 ...
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var61_clean                           : num  23.5 23.5 23.5 23.5 23.5 ...
    ##  $ Var61_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var62_clean                           : num  8.51 8.51 8.51 8.51 8.51 ...
    ##  $ Var62_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var63_clean                           : num  48.6 48.6 48.6 48.6 48.6 ...
    ##  $ Var63_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var64_clean                           : num  23855 23855 23855 23855 23855 ...
    ##  $ Var64_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 9 9 9 9 9 18 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var66_clean                           : num  134 134 134 134 134 ...
    ##  $ Var66_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var68_clean                           : num  89.6 89.6 89.6 89.6 89.6 ...
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_clean                           : num  2445617 2445617 2445617 2445617 2445617 ...
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  7 0 7 7 7 7 14 7 7 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_clean                           : num  268465 268465 268465 268465 268465 ...
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var71_clean                           : num  60.3 60.3 60.3 60.3 60.3 ...
    ##  $ Var71_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  3 4.2 3 3 3 ...
    ##  $ Var72_isBAD                           : int  0 1 0 0 0 1 0 1 1 0
    ##  $ Var73_clean                           : num  106 132 126 118 104 126 84 80 92 86
    ##  $ Var74_clean                           : num  35 0 490 7 63 28 35 189 0 35
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_clean                           : num  7.44 7.44 7.44 7.44 7.44 ...
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_clean                           : num  4951208 34032 1382400 1911368 0 ...
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var77_clean                           : int  16 16 16 16 16 16 16 16 16 16
    ##  $ Var77_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var78_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_clean                           : num  36512 36512 36512 36512 36512 ...
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  146004 1692 115048 36041 288493 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var82_clean                           : num  2.39 2.39 2.39 2.39 2.39 ...
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_clean                           : num  10 0 25 30 0 0 15 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_clean                           : num  38.2 38.2 38.2 38.2 38.2 ...
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_clean                           : num  0 0 2 24 4 0 0 10 16 6
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var86_clean                           : num  380664 380664 380664 380664 380664 ...
    ##  $ Var86_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var87_clean                           : num  5.39 5.39 5.39 5.39 5.39 ...
    ##  $ Var87_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var88_clean                           : num  52.8 52.8 52.8 52.8 52.8 ...
    ##  $ Var88_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var89_clean                           : num  6.68 6.68 6.68 6.68 6.68 ...
    ##  $ Var89_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var9_clean                            : num  66.9 66.9 66.9 66.9 66.9 ...
    ##  $ Var9_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var90_clean                           : num  0.0805 0.0805 0.0805 0.0805 0.0805 ...
    ##  $ Var90_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var91_clean                           : num  40.2 40.2 40.2 40.2 40.2 ...
    ##  $ Var91_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var92_clean                           : num  289504 289504 289504 289504 289504 ...
    ##  $ Var92_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var93_clean                           : num  2.2 2.2 2.2 2.2 2.2 ...
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var94_clean                           : num  43389 99772 242262 4083 12372 ...
    ##  $ Var94_isBAD                           : int  0 1 0 0 0 1 0 1 1 0
    ##  $ Var95_clean                           : num  157576 157576 157576 157576 157576 ...
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_clean                           : num  4.99 4.99 4.99 4.99 4.99 ...
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_clean                           : num  0.651 0.651 0.651 0.651 0.651 ...
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var98_clean                           : num  16186 16186 16186 16186 16186 ...
    ##  $ Var98_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_clean                           : num  22.8 22.8 22.8 22.8 22.8 ...
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
