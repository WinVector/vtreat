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
  extend_nse(., sample_col = random()) %.>%
  materialize(db, ., table_name = tmp_name_gen())

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
# vars <- vars[seq_len(floor(length(vars)/2))]
tp <- vtreat::designTreatmentsC(d_treat, vars, y_name, 1, parallelCluster = cl)
```

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 16:22:19 2018"
    ## [1] "designing treatments Sat Jun 16 16:22:19 2018"
    ## [1] " have initial level statistics Sat Jun 16 16:22:19 2018"
    ## [1] " scoring treatments Sat Jun 16 16:22:22 2018"
    ## [1] "have treatment plan Sat Jun 16 16:22:28 2018"
    ## [1] "rescoring complex variables Sat Jun 16 16:22:28 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 16:22:33 2018"

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
column_names(d_train)
```

    ##   [1] "churn"                                 
    ##   [2] "Var1_clean"                            
    ##   [3] "Var1_isBAD"                            
    ##   [4] "Var10_clean"                           
    ##   [5] "Var10_isBAD"                           
    ##   [6] "Var100_clean"                          
    ##   [7] "Var100_isBAD"                          
    ##   [8] "Var101_clean"                          
    ##   [9] "Var101_isBAD"                          
    ##  [10] "Var102_clean"                          
    ##  [11] "Var102_isBAD"                          
    ##  [12] "Var103_clean"                          
    ##  [13] "Var103_isBAD"                          
    ##  [14] "Var104_clean"                          
    ##  [15] "Var104_isBAD"                          
    ##  [16] "Var105_clean"                          
    ##  [17] "Var105_isBAD"                          
    ##  [18] "Var106_clean"                          
    ##  [19] "Var106_isBAD"                          
    ##  [20] "Var107_clean"                          
    ##  [21] "Var107_isBAD"                          
    ##  [22] "Var108_clean"                          
    ##  [23] "Var108_isBAD"                          
    ##  [24] "Var109_clean"                          
    ##  [25] "Var109_isBAD"                          
    ##  [26] "Var11_clean"                           
    ##  [27] "Var11_isBAD"                           
    ##  [28] "Var110_clean"                          
    ##  [29] "Var110_isBAD"                          
    ##  [30] "Var111_clean"                          
    ##  [31] "Var111_isBAD"                          
    ##  [32] "Var112_clean"                          
    ##  [33] "Var112_isBAD"                          
    ##  [34] "Var113_clean"                          
    ##  [35] "Var114_clean"                          
    ##  [36] "Var114_isBAD"                          
    ##  [37] "Var115_clean"                          
    ##  [38] "Var115_isBAD"                          
    ##  [39] "Var116_clean"                          
    ##  [40] "Var116_isBAD"                          
    ##  [41] "Var117_clean"                          
    ##  [42] "Var117_isBAD"                          
    ##  [43] "Var118_isBAD"                          
    ##  [44] "Var119_clean"                          
    ##  [45] "Var119_isBAD"                          
    ##  [46] "Var12_clean"                           
    ##  [47] "Var12_isBAD"                           
    ##  [48] "Var120_clean"                          
    ##  [49] "Var120_isBAD"                          
    ##  [50] "Var121_clean"                          
    ##  [51] "Var121_isBAD"                          
    ##  [52] "Var122_clean"                          
    ##  [53] "Var122_isBAD"                          
    ##  [54] "Var123_clean"                          
    ##  [55] "Var123_isBAD"                          
    ##  [56] "Var124_clean"                          
    ##  [57] "Var124_isBAD"                          
    ##  [58] "Var125_clean"                          
    ##  [59] "Var125_isBAD"                          
    ##  [60] "Var126_clean"                          
    ##  [61] "Var126_isBAD"                          
    ##  [62] "Var127_clean"                          
    ##  [63] "Var127_isBAD"                          
    ##  [64] "Var128_clean"                          
    ##  [65] "Var128_isBAD"                          
    ##  [66] "Var129_clean"                          
    ##  [67] "Var129_isBAD"                          
    ##  [68] "Var13_clean"                           
    ##  [69] "Var13_isBAD"                           
    ##  [70] "Var130_clean"                          
    ##  [71] "Var130_isBAD"                          
    ##  [72] "Var131_clean"                          
    ##  [73] "Var131_isBAD"                          
    ##  [74] "Var132_clean"                          
    ##  [75] "Var132_isBAD"                          
    ##  [76] "Var133_clean"                          
    ##  [77] "Var133_isBAD"                          
    ##  [78] "Var134_clean"                          
    ##  [79] "Var134_isBAD"                          
    ##  [80] "Var135_clean"                          
    ##  [81] "Var135_isBAD"                          
    ##  [82] "Var136_clean"                          
    ##  [83] "Var136_isBAD"                          
    ##  [84] "Var137_clean"                          
    ##  [85] "Var137_isBAD"                          
    ##  [86] "Var138_clean"                          
    ##  [87] "Var138_isBAD"                          
    ##  [88] "Var139_clean"                          
    ##  [89] "Var139_isBAD"                          
    ##  [90] "Var14_clean"                           
    ##  [91] "Var14_isBAD"                           
    ##  [92] "Var140_clean"                          
    ##  [93] "Var140_isBAD"                          
    ##  [94] "Var142_clean"                          
    ##  [95] "Var142_isBAD"                          
    ##  [96] "Var143_clean"                          
    ##  [97] "Var143_isBAD"                          
    ##  [98] "Var144_clean"                          
    ##  [99] "Var144_isBAD"                          
    ## [100] "Var145_clean"                          
    ## [101] "Var145_isBAD"                          
    ## [102] "Var146_clean"                          
    ## [103] "Var146_isBAD"                          
    ## [104] "Var147_clean"                          
    ## [105] "Var147_isBAD"                          
    ## [106] "Var148_clean"                          
    ## [107] "Var148_isBAD"                          
    ## [108] "Var149_clean"                          
    ## [109] "Var149_isBAD"                          
    ## [110] "Var150_clean"                          
    ## [111] "Var150_isBAD"                          
    ## [112] "Var151_clean"                          
    ## [113] "Var151_isBAD"                          
    ## [114] "Var152_clean"                          
    ## [115] "Var152_isBAD"                          
    ## [116] "Var153_clean"                          
    ## [117] "Var153_isBAD"                          
    ## [118] "Var154_clean"                          
    ## [119] "Var154_isBAD"                          
    ## [120] "Var155_clean"                          
    ## [121] "Var155_isBAD"                          
    ## [122] "Var156_clean"                          
    ## [123] "Var156_isBAD"                          
    ## [124] "Var157_clean"                          
    ## [125] "Var157_isBAD"                          
    ## [126] "Var158_clean"                          
    ## [127] "Var158_isBAD"                          
    ## [128] "Var159_clean"                          
    ## [129] "Var159_isBAD"                          
    ## [130] "Var16_clean"                           
    ## [131] "Var16_isBAD"                           
    ## [132] "Var160_clean"                          
    ## [133] "Var160_isBAD"                          
    ## [134] "Var161_clean"                          
    ## [135] "Var161_isBAD"                          
    ## [136] "Var162_clean"                          
    ## [137] "Var162_isBAD"                          
    ## [138] "Var163_clean"                          
    ## [139] "Var163_isBAD"                          
    ## [140] "Var164_clean"                          
    ## [141] "Var164_isBAD"                          
    ## [142] "Var165_clean"                          
    ## [143] "Var165_isBAD"                          
    ## [144] "Var166_clean"                          
    ## [145] "Var166_isBAD"                          
    ## [146] "Var168_clean"                          
    ## [147] "Var168_isBAD"                          
    ## [148] "Var17_clean"                           
    ## [149] "Var17_isBAD"                           
    ## [150] "Var170_clean"                          
    ## [151] "Var170_isBAD"                          
    ## [152] "Var171_clean"                          
    ## [153] "Var171_isBAD"                          
    ## [154] "Var172_clean"                          
    ## [155] "Var172_isBAD"                          
    ## [156] "Var173_clean"                          
    ## [157] "Var173_isBAD"                          
    ## [158] "Var174_clean"                          
    ## [159] "Var174_isBAD"                          
    ## [160] "Var176_clean"                          
    ## [161] "Var176_isBAD"                          
    ## [162] "Var177_clean"                          
    ## [163] "Var177_isBAD"                          
    ## [164] "Var178_clean"                          
    ## [165] "Var178_isBAD"                          
    ## [166] "Var179_clean"                          
    ## [167] "Var179_isBAD"                          
    ## [168] "Var18_clean"                           
    ## [169] "Var18_isBAD"                           
    ## [170] "Var180_clean"                          
    ## [171] "Var180_isBAD"                          
    ## [172] "Var181_clean"                          
    ## [173] "Var181_isBAD"                          
    ## [174] "Var182_clean"                          
    ## [175] "Var182_isBAD"                          
    ## [176] "Var183_clean"                          
    ## [177] "Var183_isBAD"                          
    ## [178] "Var184_clean"                          
    ## [179] "Var184_isBAD"                          
    ## [180] "Var186_clean"                          
    ## [181] "Var186_isBAD"                          
    ## [182] "Var187_clean"                          
    ## [183] "Var187_isBAD"                          
    ## [184] "Var188_clean"                          
    ## [185] "Var188_isBAD"                          
    ## [186] "Var189_clean"                          
    ## [187] "Var189_isBAD"                          
    ## [188] "Var19_isBAD"                           
    ## [189] "Var190_clean"                          
    ## [190] "Var190_isBAD"                          
    ## [191] "Var191_lev_x_NA"                       
    ## [192] "Var191_lev_x_r_I"                      
    ## [193] "Var192_catB"                           
    ## [194] "Var193_catB"                           
    ## [195] "Var193_lev_x_2Knk1KF"                  
    ## [196] "Var193_lev_x_AERks4l"                  
    ## [197] "Var193_lev_x_RO12"                     
    ## [198] "Var194_catB"                           
    ## [199] "Var194_lev_x_NA"                       
    ## [200] "Var194_lev_x_SEuy"                     
    ## [201] "Var195_catB"                           
    ## [202] "Var195_lev_x_taul"                     
    ## [203] "Var196_catB"                           
    ## [204] "Var196_lev_x_1K8T"                     
    ## [205] "Var197_catB"                           
    ## [206] "Var197_lev_x_0Xwj"                     
    ## [207] "Var197_lev_x_487l"                     
    ## [208] "Var197_lev_x_JLbT"                     
    ## [209] "Var197_lev_x_lK27"                     
    ## [210] "Var197_lev_x_ssAy"                     
    ## [211] "Var197_lev_x_TyGl"                     
    ## [212] "Var197_lev_x_z32l"                     
    ## [213] "Var198_catB"                           
    ## [214] "Var198_lev_x_fhk21Ss"                  
    ## [215] "Var198_lev_x_PHNvXy8"                  
    ## [216] "Var199_catB"                           
    ## [217] "Var199_lev_x_jTP8ioIlJ"                
    ## [218] "Var2_isBAD"                            
    ## [219] "Var200_catB"                           
    ## [220] "Var200_lev_x_NA"                       
    ## [221] "Var201_lev_x_NA"                       
    ## [222] "Var201_lev_x_smXZ"                     
    ## [223] "Var202_catB"                           
    ## [224] "Var203_catB"                           
    ## [225] "Var203_lev_x_9_Y1"                     
    ## [226] "Var203_lev_x_F3hy"                     
    ## [227] "Var203_lev_x_HLqf"                     
    ## [228] "Var204_catB"                           
    ## [229] "Var204_lev_x_7WNq"                     
    ## [230] "Var204_lev_x_k13i"                     
    ## [231] "Var204_lev_x_m_h1"                     
    ## [232] "Var204_lev_x_MBhA"                     
    ## [233] "Var204_lev_x_RcM7"                     
    ## [234] "Var204_lev_x_rGJy"                     
    ## [235] "Var204_lev_x_RVjC"                     
    ## [236] "Var204_lev_x_SkZj"                     
    ## [237] "Var204_lev_x_vzJD"                     
    ## [238] "Var204_lev_x_z5Ry"                     
    ## [239] "Var205_catB"                           
    ## [240] "Var205_lev_x_09_Q"                     
    ## [241] "Var205_lev_x_NA"                       
    ## [242] "Var205_lev_x_sJzTlal"                  
    ## [243] "Var205_lev_x_VpdQ"                     
    ## [244] "Var206_catB"                           
    ## [245] "Var206_lev_x_43pnToF"                  
    ## [246] "Var206_lev_x_6JmL"                     
    ## [247] "Var206_lev_x_hAFG"                     
    ## [248] "Var206_lev_x_haYg"                     
    ## [249] "Var206_lev_x_IYzP"                     
    ## [250] "Var206_lev_x_kxE9"                     
    ## [251] "Var206_lev_x_NA"                       
    ## [252] "Var206_lev_x_sYC_"                     
    ## [253] "Var206_lev_x_wMei"                     
    ## [254] "Var206_lev_x_y6dw"                     
    ## [255] "Var206_lev_x_zm5i"                     
    ## [256] "Var207_catB"                           
    ## [257] "Var207_lev_x_7M47J5GA0pTYIFxg5uy"      
    ## [258] "Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_"
    ## [259] "Var207_lev_x_Kxdu"                     
    ## [260] "Var207_lev_x_me75fM6ugJ"               
    ## [261] "Var207_lev_x_NKv3VA1BpP"               
    ## [262] "Var208_catB"                           
    ## [263] "Var208_lev_x_kIsH"                     
    ## [264] "Var208_lev_x_sBgB"                     
    ## [265] "Var21_clean"                           
    ## [266] "Var21_isBAD"                           
    ## [267] "Var210_catB"                           
    ## [268] "Var210_lev_x_g5HH"                     
    ## [269] "Var210_lev_x_uKAI"                     
    ## [270] "Var211_lev_x_L84s"                     
    ## [271] "Var211_lev_x_Mtgm"                     
    ## [272] "Var212_catB"                           
    ## [273] "Var212_lev_x_CrNX"                     
    ## [274] "Var212_lev_x_FMSzZ91zL2"               
    ## [275] "Var212_lev_x_Ie_5MZs"                  
    ## [276] "Var212_lev_x_NhsEn4L"                  
    ## [277] "Var212_lev_x_XfqtO3UdzaXh_"            
    ## [278] "Var213_lev_x_KdSa"                     
    ## [279] "Var213_lev_x_NA"                       
    ## [280] "Var214_catB"                           
    ## [281] "Var214_lev_x_NA"                       
    ## [282] "Var215_lev_x_NA"                       
    ## [283] "Var216_catB"                           
    ## [284] "Var216_lev_x_11p4mKe"                  
    ## [285] "Var216_lev_x_beK4AFX"                  
    ## [286] "Var216_lev_x_kq0n8Bj"                  
    ## [287] "Var216_lev_x_kZJtVhC"                  
    ## [288] "Var216_lev_x_mAja5EA"                  
    ## [289] "Var216_lev_x_mAjbk_S"                  
    ## [290] "Var216_lev_x_NGZxnJM"                  
    ## [291] "Var216_lev_x_XTbPUYD"                  
    ## [292] "Var217_catB"                           
    ## [293] "Var218_catB"                           
    ## [294] "Var218_lev_x_cJvF"                     
    ## [295] "Var218_lev_x_UYBR"                     
    ## [296] "Var219_catB"                           
    ## [297] "Var219_lev_x_AU8pNoi"                  
    ## [298] "Var219_lev_x_FzaX"                     
    ## [299] "Var219_lev_x_NA"                       
    ## [300] "Var219_lev_x_qxDb"                     
    ## [301] "Var22_clean"                           
    ## [302] "Var22_isBAD"                           
    ## [303] "Var220_catB"                           
    ## [304] "Var220_lev_x_4UxGlow"                  
    ## [305] "Var220_lev_x_UF16siJ"                  
    ## [306] "Var221_catB"                           
    ## [307] "Var221_lev_x_Al6ZaUT"                  
    ## [308] "Var221_lev_x_d0EEeJi"                  
    ## [309] "Var221_lev_x_oslk"                     
    ## [310] "Var221_lev_x_QKW8DRm"                  
    ## [311] "Var221_lev_x_zCkv"                     
    ## [312] "Var222_catB"                           
    ## [313] "Var222_lev_x_APgdzOv"                  
    ## [314] "Var222_lev_x_catzS2D"                  
    ## [315] "Var223_catB"                           
    ## [316] "Var223_lev_x_jySVZNlOJy"               
    ## [317] "Var223_lev_x_LM8l689qOp"               
    ## [318] "Var223_lev_x_M_8D"                     
    ## [319] "Var223_lev_x_NA"                       
    ## [320] "Var224_lev_x_NA"                       
    ## [321] "Var225_catB"                           
    ## [322] "Var225_lev_x_ELof"                     
    ## [323] "Var225_lev_x_kG3k"                     
    ## [324] "Var225_lev_x_NA"                       
    ## [325] "Var225_lev_x_xG3x"                     
    ## [326] "Var226_catB"                           
    ## [327] "Var226_lev_x_3Cy4"                     
    ## [328] "Var226_lev_x_453m"                     
    ## [329] "Var226_lev_x_5Acm"                     
    ## [330] "Var226_lev_x_7aLG"                     
    ## [331] "Var226_lev_x_7P5s"                     
    ## [332] "Var226_lev_x_Aoh3"                     
    ## [333] "Var226_lev_x_fKCe"                     
    ## [334] "Var226_lev_x_FSa2"                     
    ## [335] "Var226_lev_x_kwS7"                     
    ## [336] "Var226_lev_x_me1d"                     
    ## [337] "Var226_lev_x_PM2D"                     
    ## [338] "Var226_lev_x_Qcbd"                     
    ## [339] "Var226_lev_x_Qu4f"                     
    ## [340] "Var226_lev_x_rgKb"                     
    ## [341] "Var226_lev_x_szEZ"                     
    ## [342] "Var226_lev_x_TNEC"                     
    ## [343] "Var226_lev_x_uWr3"                     
    ## [344] "Var226_lev_x_WqMG"                     
    ## [345] "Var226_lev_x_wX53"                     
    ## [346] "Var226_lev_x_Xa3G"                     
    ## [347] "Var226_lev_x_xb3V"                     
    ## [348] "Var227_catB"                           
    ## [349] "Var227_lev_x_02N6s8f"                  
    ## [350] "Var227_lev_x_6fzt"                     
    ## [351] "Var227_lev_x_nIGXDli"                  
    ## [352] "Var227_lev_x_RAYp"                     
    ## [353] "Var227_lev_x_ZI9m"                     
    ## [354] "Var228_catB"                           
    ## [355] "Var228_lev_x_55YFVY9"                  
    ## [356] "Var228_lev_x_F2FyR07IdsN7I"            
    ## [357] "Var228_lev_x_ib5G6X1eUxUn6"            
    ## [358] "Var228_lev_x_iyHGyLCEkQ"               
    ## [359] "Var228_lev_x_R4y5gQQWY8OodqDV"         
    ## [360] "Var228_lev_x_TCU50_Yjmm6GIBZ0lL_"      
    ## [361] "Var228_lev_x_xwM2aC7IdeMC0"            
    ## [362] "Var229_catB"                           
    ## [363] "Var229_lev_x_am7c"                     
    ## [364] "Var229_lev_x_mj86"                     
    ## [365] "Var229_lev_x_NA"                       
    ## [366] "Var23_clean"                           
    ## [367] "Var23_isBAD"                           
    ## [368] "Var24_clean"                           
    ## [369] "Var24_isBAD"                           
    ## [370] "Var25_clean"                           
    ## [371] "Var25_isBAD"                           
    ## [372] "Var26_clean"                           
    ## [373] "Var26_isBAD"                           
    ## [374] "Var27_clean"                           
    ## [375] "Var27_isBAD"                           
    ## [376] "Var28_clean"                           
    ## [377] "Var28_isBAD"                           
    ## [378] "Var29_clean"                           
    ## [379] "Var29_isBAD"                           
    ## [380] "Var3_clean"                            
    ## [381] "Var3_isBAD"                            
    ## [382] "Var30_clean"                           
    ## [383] "Var30_isBAD"                           
    ## [384] "Var33_clean"                           
    ## [385] "Var33_isBAD"                           
    ## [386] "Var34_clean"                           
    ## [387] "Var34_isBAD"                           
    ## [388] "Var35_clean"                           
    ## [389] "Var35_isBAD"                           
    ## [390] "Var36_clean"                           
    ## [391] "Var36_isBAD"                           
    ## [392] "Var37_clean"                           
    ## [393] "Var37_isBAD"                           
    ## [394] "Var38_clean"                           
    ## [395] "Var38_isBAD"                           
    ## [396] "Var4_isBAD"                            
    ## [397] "Var40_clean"                           
    ## [398] "Var40_isBAD"                           
    ## [399] "Var41_clean"                           
    ## [400] "Var41_isBAD"                           
    ## [401] "Var43_clean"                           
    ## [402] "Var43_isBAD"                           
    ## [403] "Var44_clean"                           
    ## [404] "Var44_isBAD"                           
    ## [405] "Var45_clean"                           
    ## [406] "Var45_isBAD"                           
    ## [407] "Var46_clean"                           
    ## [408] "Var46_isBAD"                           
    ## [409] "Var47_clean"                           
    ## [410] "Var47_isBAD"                           
    ## [411] "Var49_clean"                           
    ## [412] "Var49_isBAD"                           
    ## [413] "Var5_clean"                            
    ## [414] "Var5_isBAD"                            
    ## [415] "Var50_clean"                           
    ## [416] "Var50_isBAD"                           
    ## [417] "Var51_clean"                           
    ## [418] "Var51_isBAD"                           
    ## [419] "Var53_clean"                           
    ## [420] "Var53_isBAD"                           
    ## [421] "Var54_clean"                           
    ## [422] "Var54_isBAD"                           
    ## [423] "Var56_clean"                           
    ## [424] "Var56_isBAD"                           
    ## [425] "Var57_clean"                           
    ## [426] "Var58_clean"                           
    ## [427] "Var58_isBAD"                           
    ## [428] "Var59_clean"                           
    ## [429] "Var59_isBAD"                           
    ## [430] "Var6_clean"                            
    ## [431] "Var6_isBAD"                            
    ## [432] "Var60_clean"                           
    ## [433] "Var60_isBAD"                           
    ## [434] "Var61_clean"                           
    ## [435] "Var61_isBAD"                           
    ## [436] "Var62_clean"                           
    ## [437] "Var62_isBAD"                           
    ## [438] "Var63_clean"                           
    ## [439] "Var63_isBAD"                           
    ## [440] "Var64_clean"                           
    ## [441] "Var64_isBAD"                           
    ## [442] "Var65_clean"                           
    ## [443] "Var65_isBAD"                           
    ## [444] "Var66_clean"                           
    ## [445] "Var66_isBAD"                           
    ## [446] "Var67_isBAD"                           
    ## [447] "Var68_clean"                           
    ## [448] "Var68_isBAD"                           
    ## [449] "Var69_clean"                           
    ## [450] "Var69_isBAD"                           
    ## [451] "Var7_clean"                            
    ## [452] "Var7_isBAD"                            
    ## [453] "Var70_clean"                           
    ## [454] "Var70_isBAD"                           
    ## [455] "Var71_clean"                           
    ## [456] "Var71_isBAD"                           
    ## [457] "Var72_clean"                           
    ## [458] "Var72_isBAD"                           
    ## [459] "Var73_clean"                           
    ## [460] "Var74_clean"                           
    ## [461] "Var74_isBAD"                           
    ## [462] "Var75_clean"                           
    ## [463] "Var75_isBAD"                           
    ## [464] "Var76_clean"                           
    ## [465] "Var76_isBAD"                           
    ## [466] "Var77_clean"                           
    ## [467] "Var77_isBAD"                           
    ## [468] "Var78_clean"                           
    ## [469] "Var78_isBAD"                           
    ## [470] "Var80_clean"                           
    ## [471] "Var80_isBAD"                           
    ## [472] "Var81_clean"                           
    ## [473] "Var81_isBAD"                           
    ## [474] "Var82_clean"                           
    ## [475] "Var82_isBAD"                           
    ## [476] "Var83_clean"                           
    ## [477] "Var83_isBAD"                           
    ## [478] "Var84_clean"                           
    ## [479] "Var84_isBAD"                           
    ## [480] "Var85_clean"                           
    ## [481] "Var85_isBAD"                           
    ## [482] "Var86_clean"                           
    ## [483] "Var86_isBAD"                           
    ## [484] "Var87_clean"                           
    ## [485] "Var87_isBAD"                           
    ## [486] "Var88_clean"                           
    ## [487] "Var88_isBAD"                           
    ## [488] "Var89_clean"                           
    ## [489] "Var89_isBAD"                           
    ## [490] "Var9_clean"                            
    ## [491] "Var9_isBAD"                            
    ## [492] "Var90_clean"                           
    ## [493] "Var90_isBAD"                           
    ## [494] "Var91_clean"                           
    ## [495] "Var91_isBAD"                           
    ## [496] "Var92_clean"                           
    ## [497] "Var92_isBAD"                           
    ## [498] "Var93_clean"                           
    ## [499] "Var93_isBAD"                           
    ## [500] "Var94_clean"                           
    ## [501] "Var94_isBAD"                           
    ## [502] "Var95_clean"                           
    ## [503] "Var95_isBAD"                           
    ## [504] "Var96_clean"                           
    ## [505] "Var96_isBAD"                           
    ## [506] "Var97_clean"                           
    ## [507] "Var97_isBAD"                           
    ## [508] "Var98_clean"                           
    ## [509] "Var98_isBAD"                           
    ## [510] "Var99_clean"                           
    ## [511] "Var99_isBAD"

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_82882728393669768585_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 39979 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  511 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 1 -1 -1 -1
    ##  $ Var1_clean                            : num  9.42 9.42 9.42 9.42 9.42 ...
    ##  $ Var1_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var10_clean                           : num  400882 400882 400882 400882 400882 ...
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var100_clean                          : num  0.565 0.565 0.565 0.565 0.565 ...
    ##  $ Var100_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var101_clean                          : num  15.6 15.6 15.6 15.6 15.6 15.6 15.6 15.6 15.6 15.6
    ##  $ Var101_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var102_clean                          : num  30549 30549 30549 30549 30549 ...
    ##  $ Var102_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_clean                          : num  18.6 18.6 18.6 18.6 18.6 ...
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var104_clean                          : num  104 104 104 104 104 ...
    ##  $ Var104_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var105_clean                          : int  69 69 69 69 69 69 69 69 69 69
    ##  $ Var105_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_clean                          : num  49490 49490 49490 49490 49490 ...
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_clean                          : num  3.74 3.74 3.74 3.74 3.74 ...
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var108_clean                          : num  137901 137901 137901 137901 137901 ...
    ##  $ Var108_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var109_clean                          : num  16 32 8 0 40 ...
    ##  $ Var109_isBAD                          : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var11_clean                           : num  8.25 8.25 8.25 8.25 8.25 8.25 8.25 8.25 8.25 8.25
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var110_clean                          : num  6.58 6.58 6.58 6.58 6.58 ...
    ##  $ Var110_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_clean                          : num  294428 294428 294428 294428 294428 ...
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_clean                          : num  16 16 0 8 64 32 40 0 0 0
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -2135 -38397 -87345 44967 126870 ...
    ##  $ Var114_clean                          : num  645579 645579 645579 645579 645579 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var115_clean                          : num  35.6 35.6 35.6 35.6 35.6 ...
    ##  $ Var115_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var116_clean                          : num  0.0968 0.0968 0.0968 0.0968 0.0968 ...
    ##  $ Var116_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_clean                          : num  143062 143062 143062 143062 143062 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var118_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_clean                          : num  20 610 250 90 580 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var12_clean                           : num  11.3 11.3 11.3 11.3 11.3 ...
    ##  $ Var12_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var120_clean                          : num  30.1 30.1 30.1 30.1 30.1 ...
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var121_clean                          : num  3.35 3.35 3.35 3.35 3.35 ...
    ##  $ Var121_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_clean                          : num  0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_clean                          : num  0 108 12 12 42 66 30 0 12 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_clean                          : num  172527 172527 172527 172527 172527 ...
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_clean                          : num  2574 189765 41094 4302 48645 ...
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -0.251 -26 -30 -28 4 ...
    ##  $ Var126_isBAD                          : int  1 0 0 0 0 1 1 0 0 0
    ##  $ Var127_clean                          : num  19 19 19 19 19 ...
    ##  $ Var127_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var128_clean                          : num  73.7 73.7 73.7 73.7 73.7 ...
    ##  $ Var128_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var129_clean                          : num  7.29 7.29 7.29 7.29 7.29 ...
    ##  $ Var129_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var13_clean                           : num  348 2968 52 528 400 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_clean                          : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var131_clean                          : num  1535791 1535791 1535791 1535791 1535791 ...
    ##  $ Var131_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_clean                          : num  0 2417555 1762675 2285440 6480000 ...
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_clean                          : num  0 286546 0 1209600 0 ...
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_clean                          : num  195 195 195 195 195 ...
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var136_clean                          : num  147345 147345 147345 147345 147345 ...
    ##  $ Var136_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var137_clean                          : num  1.42 1.42 1.42 1.42 1.42 ...
    ##  $ Var137_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_clean                          : num  0.012 0.012 0.012 0.012 0.012 ...
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_clean                          : num  176686 176686 176686 176686 176686 ...
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_clean                           : num  0.917 0.917 0.917 0.917 0.917 ...
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  0 11845 990 1000 110 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var142_clean                          : num  0.645 0.645 0.645 0.645 0.645 ...
    ##  $ Var142_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var143_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  9 36 9 18 9 ...
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var145_clean                          : num  45.1 45.1 45.1 45.1 45.1 ...
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var146_clean                          : num  3.57 3.57 3.57 3.57 3.57 ...
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_clean                          : int  2 2 2 2 2 2 2 2 2 2
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_clean                          : num  133 133 133 133 133 ...
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var149_clean                          : num  0 985208 0 0 98707 ...
    ##  $ Var149_isBAD                          : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var150_clean                          : num  212303 212303 212303 212303 212303 ...
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var151_clean                          : num  6.6 6.6 6.6 6.6 6.6 6.6 6.6 6.6 6.6 6.6
    ##  $ Var151_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_clean                          : num  8.26 8.26 8.26 8.26 8.26 ...
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_clean                          : num  17016 10522600 6920680 8569360 10641120 ...
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var154_clean                          : num  1847248 1847248 1847248 1847248 1847248 ...
    ##  $ Var154_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var155_clean                          : num  0.958 0.958 0.958 0.958 0.958 ...
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var156_clean                          : num  115 115 115 115 115 ...
    ##  $ Var156_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var157_clean                          : num  26.4 26.4 26.4 26.4 26.4 ...
    ##  $ Var157_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var158_clean                          : num  2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1
    ##  $ Var158_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_clean                          : num  4.22 4.22 4.22 4.22 4.22 ...
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_clean                           : num  129 129 129 129 129 ...
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_clean                          : num  0 16 2 2 30 32 38 2 12 0
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_clean                          : num  3.61 3.61 3.61 3.61 3.61 ...
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var162_clean                          : num  380396 380396 380396 380396 380396 ...
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_clean                          : num  0 326064 1008234 0 0 ...
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_clean                          : num  1.46 1.46 1.46 1.46 1.46 ...
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var165_clean                          : num  37246 37246 37246 37246 37246 ...
    ##  $ Var165_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var166_clean                          : num  26.3 26.3 26.3 26.3 26.3 ...
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var168_clean                          : num  313 313 313 313 313 ...
    ##  $ Var168_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_clean                           : num  8.53 8.53 8.53 8.53 8.53 ...
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_clean                          : num  3.91 3.91 3.91 3.91 3.91 ...
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var171_clean                          : num  399247 399247 399247 399247 399247 ...
    ##  $ Var171_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_clean                          : num  11.6 11.6 11.6 11.6 11.6 ...
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_clean                          : num  5.17 5.17 5.17 5.17 5.17 ...
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var176_clean                          : num  4.42 4.42 4.42 4.42 4.42 ...
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_clean                          : num  709748 709748 709748 709748 709748 ...
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var178_clean                          : num  19.4 19.4 19.4 19.4 19.4 ...
    ##  $ Var178_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_clean                          : num  1.86 1.86 1.86 1.86 1.86 ...
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_clean                           : num  4.74 4.74 4.74 4.74 4.74 ...
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var180_clean                          : num  3664861 3664861 3664861 3664861 3664861 ...
    ##  $ Var180_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_clean                          : num  1548174 1548174 1548174 1548174 1548174 ...
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var183_clean                          : num  56523 56523 56523 56523 56523 ...
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_clean                          : num  17.5 17.5 17.5 17.5 17.5 ...
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var186_clean                          : num  4.06 4.06 4.06 4.06 4.06 ...
    ##  $ Var186_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var187_clean                          : num  10.1 10.1 10.1 10.1 10.1 ...
    ##  $ Var187_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_clean                          : num  158 158 158 158 158 ...
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  240 354 228 366 324 ...
    ##  $ Var189_isBAD                          : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var190_clean                          : num  23767 23767 23767 23767 23767 ...
    ##  $ Var190_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_r_I                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var192_catB                           : num  0.0971 -9.5613 0.8972 -6.671 0.1723 ...
    ##  $ Var193_catB                           : num  -0.507 -0.507 -0.507 -0.507 -0.507 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var193_lev_x_AERks4l                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 0 1 1 1 1 1
    ##  $ Var194_catB                           : num  0.0909 -0.3121 0.0909 -0.3121 0.0909 ...
    ##  $ Var194_lev_x_NA                       : int  1 0 1 0 1 1 1 0 0 0
    ##  $ Var194_lev_x_SEuy                     : int  0 1 0 1 0 0 0 1 1 1
    ##  $ Var195_catB                           : num  0.014 0.014 0.014 0.014 0.014 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var196_catB                           : num  -0.00397 -0.00397 -0.00397 -0.00397 -0.00397 ...
    ##  $ Var196_lev_x_1K8T                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var197_catB                           : num  -0.0549 -0.0549 -9.929 -0.0996 -0.6794 ...
    ##  $ Var197_lev_x_0Xwj                     : int  1 1 0 0 0 0 1 0 0 0
    ##  $ Var197_lev_x_487l                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_JLbT                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_lK27                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var197_lev_x_ssAy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_TyGl                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_z32l                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_catB                           : num  0 0.93 1.44 1.44 2.54 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_lev_x_PHNvXy8                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                           : num  -0.911 -0.135 1.376 -7.364 0.306 ...
    ##  $ Var199_lev_x_jTP8ioIlJ                : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catB                           : num  -6.67 0 0 0 0 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var201_lev_x_NA                       : int  1 0 1 0 1 1 1 0 0 0
    ##  $ Var201_lev_x_smXZ                     : int  0 1 0 1 0 0 0 1 1 1
    ##  $ Var202_catB                           : num  -7.36 0 0 -7.36 0 ...
    ##  $ Var203_catB                           : num  -6.67104 0.01542 0.00599 0.00599 0.00599 ...
    ##  $ Var203_lev_x_9_Y1                     : int  0 0 1 1 1 1 0 1 1 1
    ##  $ Var203_lev_x_F3hy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var203_lev_x_HLqf                     : int  0 1 0 0 0 0 1 0 0 0
    ##  $ Var204_catB                           : num  0.3847 0.0786 0.0971 0.0971 -0.2014 ...
    ##  $ Var204_lev_x_7WNq                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_k13i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_m_h1                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_MBhA                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_RcM7                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var204_lev_x_rGJy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_RVjC                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_SkZj                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_vzJD                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_z5Ry                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_catB                           : num  -0.1398 -0.1398 -0.0451 -0.0451 -0.0451 ...
    ##  $ Var205_lev_x_09_Q                     : int  0 0 1 1 1 1 1 0 0 0
    ##  $ Var205_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var205_lev_x_VpdQ                     : int  1 1 0 0 0 0 0 1 0 0
    ##  $ Var206_catB                           : num  0.0971 -0.1852 0.0971 -0.8946 -0.8946 ...
    ##  $ Var206_lev_x_43pnToF                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_6JmL                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 0 1 1 0 0 0 0 0
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var206_lev_x_kxE9                     : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_sYC_                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var206_lev_x_wMei                     : int  1 0 1 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.131 0.131 0.131 0.131 0.131 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var207_lev_x_Kxdu                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var207_lev_x_NKv3VA1BpP               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var208_catB                           : num  0.07482 0.07482 -0.00515 -0.00515 -0.00515 ...
    ##  $ Var208_lev_x_kIsH                     : int  0 0 1 1 1 1 0 1 1 1
    ##  $ Var208_lev_x_sBgB                     : int  1 1 0 0 0 0 1 0 0 0
    ##  $ Var21_clean                           : num  8 152 20 20 128 ...
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var210_catB                           : num  -0.0504 -0.0504 -0.0504 -0.0504 -0.0504 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 0 1 0 1
    ##  $ Var211_lev_x_L84s                     : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var212_catB                           : num  -1.071 0.222 -1.071 -0.516 -0.516 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var212_lev_x_FMSzZ91zL2               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_Ie_5MZs                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  0 1 0 0 0 0 0 0 0 1
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 0 0 1 1 0 0 1 1 0
    ##  $ Var213_lev_x_KdSa                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var213_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var214_catB                           : num  -6.67 0 0 0 0 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var215_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var216_catB                           : num  -0.878 0.237 -0.878 -0.369 -0.369 ...
    ##  $ Var216_lev_x_11p4mKe                  : int  0 0 0 1 1 0 0 1 0 0
    ##  $ Var216_lev_x_beK4AFX                  : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var216_lev_x_kq0n8Bj                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_kZJtVhC                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAja5EA                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAjbk_S                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_NGZxnJM                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var217_catB                           : num  -6.67 0 0 -7.36 0 ...
    ##  $ Var218_catB                           : num  -0.167 0.103 0.103 0.103 -0.167 ...
    ##  $ Var218_lev_x_cJvF                     : int  1 0 0 0 1 1 1 0 0 0
    ##  $ Var218_lev_x_UYBR                     : int  0 1 1 1 0 0 0 1 1 1
    ##  $ Var219_catB                           : num  0.0179 0.0179 0.0179 0.0179 0.0179 ...
    ##  $ Var219_lev_x_AU8pNoi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_FzaX                     : int  1 1 1 1 1 1 1 0 1 1
    ##  $ Var219_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_qxDb                     : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var22_clean                           : num  10 190 25 25 160 175 195 155 115 0
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catB                           : num  0 0.93 1.44 1.44 2.54 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_lev_x_UF16siJ                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.0913 0.0913 0.0913 0.0913 0.0913 ...
    ##  $ Var221_lev_x_Al6ZaUT                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catB                           : num  0 0.93 1.44 1.44 2.54 ...
    ##  $ Var222_lev_x_APgdzOv                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_catB                           : num  -0.0288 -0.0288 -0.0288 -0.0288 -0.0288 ...
    ##  $ Var223_lev_x_jySVZNlOJy               : int  0 0 0 0 0 0 0 1 1 0
    ##  $ Var223_lev_x_LM8l689qOp               : int  1 1 1 1 1 1 1 0 0 1
    ##  $ Var223_lev_x_M_8D                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var224_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var225_catB                           : num  -0.19 -0.245 -0.19 -0.245 -0.19 ...
    ##  $ Var225_lev_x_ELof                     : int  1 0 1 0 1 1 0 0 0 0
    ##  $ Var225_lev_x_kG3k                     : int  0 1 0 1 0 0 0 1 1 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var225_lev_x_xG3x                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_catB                           : num  -0.0577 0.3188 0.46 -0.2478 -0.0816 ...
    ##  $ Var226_lev_x_3Cy4                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_453m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_5Acm                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7aLG                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Aoh3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_fKCe                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_kwS7                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_me1d                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var226_lev_x_PM2D                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qcbd                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var226_lev_x_Qu4f                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_rgKb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 0 0 1 0 0 0 1 1 1
    ##  $ Var226_lev_x_TNEC                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_uWr3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_WqMG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_wX53                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Xa3G                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_xb3V                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  0.124 0.124 0.124 0.124 0.124 ...
    ##  $ Var227_lev_x_02N6s8f                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_6fzt                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 1 1 0 1 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.459 -0.459 -0.459 -0.459 -0.459 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 1 0 0 0 0 0
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 1 1 1 1 1
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_xwM2aC7IdeMC0            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  1.44 1.44 1.44 1.44 1.44 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_clean                           : num  6.19 6.19 6.19 6.19 6.19 ...
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var24_clean                           : num  0 4 0 0 2 ...
    ##  $ Var24_isBAD                           : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var25_clean                           : num  0 32 0 8 0 8 40 176 96 0
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_clean                           : num  0.0462 0.0462 0.0462 0.0462 0.0462 ...
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_clean                           : num  0.0462 0.0462 0.0462 0.0462 0.0462 ...
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_clean                           : num  167 315 187 167 220 ...
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var29_clean                           : num  0.0323 0.0323 0.0323 0.0323 0.0323 ...
    ##  $ Var29_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var3_clean                            : num  525 525 525 525 525 ...
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var30_clean                           : num  8.47 8.47 8.47 8.47 8.47 ...
    ##  $ Var30_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var33_clean                           : num  84185 84185 84185 84185 84185 ...
    ##  $ Var33_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_clean                           : num  1.92 1.92 1.92 1.92 1.92 ...
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_clean                           : num  175854 175854 175854 175854 175854 ...
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var37_clean                           : num  667220 667220 667220 667220 667220 ...
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_clean                           : num  0 8068320 7257600 5589792 3628806 ...
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var40_clean                           : num  11.2 11.2 11.2 11.2 11.2 ...
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var41_clean                           : num  26.8 26.8 26.8 26.8 26.8 ...
    ##  $ Var41_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_clean                           : num  4.27 4.27 4.27 4.27 4.27 ...
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var45_clean                           : num  8430 8430 8430 8430 8430 ...
    ##  $ Var45_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var46_clean                           : num  12.4 12.4 12.4 12.4 12.4 ...
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var47_clean                           : num  0.645 0.645 0.645 0.645 0.645 ...
    ##  $ Var47_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_clean                           : num  0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var5_clean                            : int  251382 251382 251382 251382 251382 251382 251382 251382 251382 251382
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var50_clean                           : num  18.9 18.9 18.9 18.9 18.9 ...
    ##  $ Var50_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var51_clean                           : num  45134 45134 45134 45134 45134 ...
    ##  $ Var51_isBAD                           : int  1 1 1 1 1 1 1 1 0 1
    ##  $ Var53_clean                           : num  343170 343170 343170 343170 343170 ...
    ##  $ Var53_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_clean                           : num  5.42 5.42 5.42 5.42 5.42 ...
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var56_clean                           : num  59967 59967 59967 59967 59967 ...
    ##  $ Var56_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var57_clean                           : num  1.22 6.71 1.8 5.27 3.43 ...
    ##  $ Var58_clean                           : num  67074 67074 67074 67074 67074 ...
    ##  $ Var58_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var59_clean                           : num  319212 319212 319212 319212 319212 ...
    ##  $ Var59_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_clean                            : num  259 1589 98 56 770 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var60_clean                           : num  8.28 8.28 8.28 8.28 8.28 ...
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var61_clean                           : int  30 30 30 30 30 30 30 30 30 30
    ##  $ Var61_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var62_clean                           : num  3.52 3.52 3.52 3.52 3.52 ...
    ##  $ Var62_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var63_clean                           : num  23.1 23.1 23.1 23.1 23.1 23.1 23.1 23.1 23.1 23.1
    ##  $ Var63_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var64_clean                           : num  24187 24187 24187 24187 24187 ...
    ##  $ Var64_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 9 9 9 9 18 9 9 9 9
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var66_clean                           : num  65.7 65.7 65.7 65.7 65.7 ...
    ##  $ Var66_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var68_clean                           : num  96.7 96.7 96.7 96.7 96.7 ...
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_clean                           : num  3958795 3958795 3958795 3958795 3958795 ...
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  0 7 7 7 7 14 7 7 7 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_clean                           : num  543781 543781 543781 543781 543781 ...
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var71_clean                           : num  124 124 124 124 124 ...
    ##  $ Var71_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  4.14 3 3 4.14 3 ...
    ##  $ Var72_isBAD                           : int  1 0 0 1 0 0 1 1 0 0
    ##  $ Var73_clean                           : num  132 126 104 126 106 84 80 92 86 100
    ##  $ Var74_clean                           : num  0 490 63 28 35 35 189 0 35 168
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_clean                           : num  7.24 7.24 7.24 7.24 7.24 ...
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_clean                           : num  34032 1382400 0 1190536 4951208 ...
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var77_clean                           : num  5.32 5.32 5.32 5.32 5.32 ...
    ##  $ Var77_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var78_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_clean                           : num  50450 50450 50450 50450 50450 ...
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  1692 115048 288493 1076211 146004 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var82_clean                           : num  2.34 2.34 2.34 2.34 2.34 ...
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_clean                           : num  0 25 0 0 10 15 0 0 0 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_clean                           : num  43 43 43 43 43 ...
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_clean                           : num  0 2 4 0 0 0 10 16 6 0
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var86_clean                           : num  268601 268601 268601 268601 268601 ...
    ##  $ Var86_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var87_clean                           : num  6.1 6.1 6.1 6.1 6.1 ...
    ##  $ Var87_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var88_clean                           : num  52.6 52.6 52.6 52.6 52.6 ...
    ##  $ Var88_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var89_clean                           : num  5.65 5.65 5.65 5.65 5.65 ...
    ##  $ Var89_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var9_clean                            : num  32.5 32.5 32.5 32.5 32.5 ...
    ##  $ Var9_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var90_clean                           : num  0.113 0.113 0.113 0.113 0.113 ...
    ##  $ Var90_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var91_clean                           : num  82.4 82.4 82.4 82.4 82.4 ...
    ##  $ Var91_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var92_clean                           : num  110146 110146 110146 110146 110146 ...
    ##  $ Var92_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var93_clean                           : num  2.09 2.09 2.09 2.09 2.09 ...
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var94_clean                           : num  98416 242262 12372 98416 43389 ...
    ##  $ Var94_isBAD                           : int  1 0 0 1 0 0 1 1 0 0
    ##  $ Var95_clean                           : num  95451 95451 95451 95451 95451 ...
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_clean                           : num  5.54 5.54 5.54 5.54 5.54 ...
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_clean                           : num  1.06 1.06 1.06 1.06 1.06 ...
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var98_clean                           : num  5321 5321 5321 5321 5321 ...
    ##  $ Var98_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_clean                           : num  23 23 23 23 23 ...
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
