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
  extend_nse(., sample_col = rand()) %.>%
  materialize(db, ., table_name = tmp_name_gen())

y_name <- "churn"
vars <- setdiff(column_names(d), c(y_name, "sample_col"))

d_train <- d %.>%
  select_rows_nse(., sample_col <= 0.8) %.>%
  materialize(db, ., table_name = tmp_name_gen()) # should not need to materialize this, doign this to debug
d_train <- d %.>%
  select_rows_nse(., sample_col > 0.9)
d_treat <- d %.>%
  select_rows_nse(., (sample_col > 0.8) & (sample_col <= 0.9)) %.>%
  execute(db, .)
```

``` r
cl = parallel::makeCluster(4)
# vars <- c("Var228", "Var229")
tp <- vtreat::designTreatmentsC(d_treat, vars, y_name, 1, parallelCluster = cl)
```

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 08:37:06 2018"
    ## [1] "designing treatments Sat Jun 16 08:37:06 2018"
    ## [1] " have initial level statistics Sat Jun 16 08:37:06 2018"
    ## [1] " scoring treatments Sat Jun 16 08:37:08 2018"
    ## [1] "have treatment plan Sat Jun 16 08:37:14 2018"
    ## [1] "rescoring complex variables Sat Jun 16 08:37:14 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 08:37:19 2018"

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
    ##  [86] "Var138_isBAD"                          
    ##  [87] "Var139_clean"                          
    ##  [88] "Var139_isBAD"                          
    ##  [89] "Var14_clean"                           
    ##  [90] "Var14_isBAD"                           
    ##  [91] "Var140_clean"                          
    ##  [92] "Var140_isBAD"                          
    ##  [93] "Var142_clean"                          
    ##  [94] "Var142_isBAD"                          
    ##  [95] "Var143_clean"                          
    ##  [96] "Var143_isBAD"                          
    ##  [97] "Var144_clean"                          
    ##  [98] "Var144_isBAD"                          
    ##  [99] "Var145_clean"                          
    ## [100] "Var145_isBAD"                          
    ## [101] "Var146_clean"                          
    ## [102] "Var146_isBAD"                          
    ## [103] "Var147_clean"                          
    ## [104] "Var147_isBAD"                          
    ## [105] "Var148_clean"                          
    ## [106] "Var148_isBAD"                          
    ## [107] "Var149_clean"                          
    ## [108] "Var149_isBAD"                          
    ## [109] "Var150_clean"                          
    ## [110] "Var150_isBAD"                          
    ## [111] "Var151_clean"                          
    ## [112] "Var151_isBAD"                          
    ## [113] "Var152_clean"                          
    ## [114] "Var152_isBAD"                          
    ## [115] "Var153_clean"                          
    ## [116] "Var153_isBAD"                          
    ## [117] "Var154_clean"                          
    ## [118] "Var154_isBAD"                          
    ## [119] "Var155_clean"                          
    ## [120] "Var155_isBAD"                          
    ## [121] "Var156_clean"                          
    ## [122] "Var156_isBAD"                          
    ## [123] "Var157_clean"                          
    ## [124] "Var157_isBAD"                          
    ## [125] "Var158_clean"                          
    ## [126] "Var158_isBAD"                          
    ## [127] "Var159_clean"                          
    ## [128] "Var159_isBAD"                          
    ## [129] "Var16_clean"                           
    ## [130] "Var16_isBAD"                           
    ## [131] "Var160_clean"                          
    ## [132] "Var160_isBAD"                          
    ## [133] "Var161_clean"                          
    ## [134] "Var161_isBAD"                          
    ## [135] "Var162_clean"                          
    ## [136] "Var162_isBAD"                          
    ## [137] "Var163_clean"                          
    ## [138] "Var163_isBAD"                          
    ## [139] "Var164_clean"                          
    ## [140] "Var164_isBAD"                          
    ## [141] "Var165_clean"                          
    ## [142] "Var165_isBAD"                          
    ## [143] "Var166_clean"                          
    ## [144] "Var166_isBAD"                          
    ## [145] "Var168_clean"                          
    ## [146] "Var168_isBAD"                          
    ## [147] "Var17_clean"                           
    ## [148] "Var17_isBAD"                           
    ## [149] "Var170_clean"                          
    ## [150] "Var170_isBAD"                          
    ## [151] "Var171_clean"                          
    ## [152] "Var171_isBAD"                          
    ## [153] "Var172_clean"                          
    ## [154] "Var172_isBAD"                          
    ## [155] "Var173_clean"                          
    ## [156] "Var173_isBAD"                          
    ## [157] "Var174_clean"                          
    ## [158] "Var174_isBAD"                          
    ## [159] "Var176_clean"                          
    ## [160] "Var176_isBAD"                          
    ## [161] "Var177_clean"                          
    ## [162] "Var177_isBAD"                          
    ## [163] "Var178_clean"                          
    ## [164] "Var178_isBAD"                          
    ## [165] "Var179_clean"                          
    ## [166] "Var179_isBAD"                          
    ## [167] "Var18_clean"                           
    ## [168] "Var18_isBAD"                           
    ## [169] "Var180_clean"                          
    ## [170] "Var180_isBAD"                          
    ## [171] "Var181_clean"                          
    ## [172] "Var181_isBAD"                          
    ## [173] "Var182_clean"                          
    ## [174] "Var182_isBAD"                          
    ## [175] "Var183_clean"                          
    ## [176] "Var183_isBAD"                          
    ## [177] "Var184_clean"                          
    ## [178] "Var184_isBAD"                          
    ## [179] "Var186_clean"                          
    ## [180] "Var186_isBAD"                          
    ## [181] "Var187_clean"                          
    ## [182] "Var187_isBAD"                          
    ## [183] "Var188_clean"                          
    ## [184] "Var188_isBAD"                          
    ## [185] "Var189_clean"                          
    ## [186] "Var189_isBAD"                          
    ## [187] "Var19_clean"                           
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
    ## [212] "Var198_catB"                           
    ## [213] "Var198_lev_x_fhk21Ss"                  
    ## [214] "Var198_lev_x_PHNvXy8"                  
    ## [215] "Var199_catB"                           
    ## [216] "Var199_lev_x_r83_sZi"                  
    ## [217] "Var2_isBAD"                            
    ## [218] "Var200_catB"                           
    ## [219] "Var200_lev_x_NA"                       
    ## [220] "Var201_lev_x_NA"                       
    ## [221] "Var201_lev_x_smXZ"                     
    ## [222] "Var202_catB"                           
    ## [223] "Var203_catB"                           
    ## [224] "Var203_lev_x_9_Y1"                     
    ## [225] "Var203_lev_x_F3hy"                     
    ## [226] "Var203_lev_x_HLqf"                     
    ## [227] "Var204_catB"                           
    ## [228] "Var204_lev_x_7WNq"                     
    ## [229] "Var204_lev_x_DtNL"                     
    ## [230] "Var204_lev_x_e7QV"                     
    ## [231] "Var204_lev_x_k13i"                     
    ## [232] "Var204_lev_x_m_h1"                     
    ## [233] "Var204_lev_x_MBhA"                     
    ## [234] "Var204_lev_x_RcM7"                     
    ## [235] "Var204_lev_x_rGJy"                     
    ## [236] "Var204_lev_x_RVjC"                     
    ## [237] "Var204_lev_x_SkZj"                     
    ## [238] "Var204_lev_x_vzJD"                     
    ## [239] "Var204_lev_x_YULl"                     
    ## [240] "Var205_catB"                           
    ## [241] "Var205_lev_x_09_Q"                     
    ## [242] "Var205_lev_x_NA"                       
    ## [243] "Var205_lev_x_sJzTlal"                  
    ## [244] "Var205_lev_x_VpdQ"                     
    ## [245] "Var206_catB"                           
    ## [246] "Var206_lev_x_43pnToF"                  
    ## [247] "Var206_lev_x_6JmL"                     
    ## [248] "Var206_lev_x_hAFG"                     
    ## [249] "Var206_lev_x_haYg"                     
    ## [250] "Var206_lev_x_IYzP"                     
    ## [251] "Var206_lev_x_kxE9"                     
    ## [252] "Var206_lev_x_NA"                       
    ## [253] "Var206_lev_x_sYC_"                     
    ## [254] "Var206_lev_x_wMei"                     
    ## [255] "Var206_lev_x_y6dw"                     
    ## [256] "Var206_lev_x_zm5i"                     
    ## [257] "Var207_catB"                           
    ## [258] "Var207_lev_x_7M47J5GA0pTYIFxg5uy"      
    ## [259] "Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_"
    ## [260] "Var207_lev_x_GjJ35utlTa_GNSvxxpb9ju"   
    ## [261] "Var207_lev_x_Kxdu"                     
    ## [262] "Var207_lev_x_me75fM6ugJ"               
    ## [263] "Var207_lev_x_NKv3VA1BpP"               
    ## [264] "Var208_catB"                           
    ## [265] "Var208_lev_x_kIsH"                     
    ## [266] "Var208_lev_x_sBgB"                     
    ## [267] "Var21_clean"                           
    ## [268] "Var21_isBAD"                           
    ## [269] "Var210_catB"                           
    ## [270] "Var210_lev_x_g5HH"                     
    ## [271] "Var210_lev_x_uKAI"                     
    ## [272] "Var211_lev_x_L84s"                     
    ## [273] "Var211_lev_x_Mtgm"                     
    ## [274] "Var212_catB"                           
    ## [275] "Var212_lev_x_CrNX"                     
    ## [276] "Var212_lev_x_FMSzZ91zL2"               
    ## [277] "Var212_lev_x_Ie_5MZs"                  
    ## [278] "Var212_lev_x_NhsEn4L"                  
    ## [279] "Var212_lev_x_XfqtO3UdzaXh_"            
    ## [280] "Var213_lev_x_KdSa"                     
    ## [281] "Var213_lev_x_NA"                       
    ## [282] "Var214_catB"                           
    ## [283] "Var214_lev_x_NA"                       
    ## [284] "Var215_lev_x_NA"                       
    ## [285] "Var216_catB"                           
    ## [286] "Var216_lev_x_11p4mKe"                  
    ## [287] "Var216_lev_x_beK4AFX"                  
    ## [288] "Var216_lev_x_kZJtVhC"                  
    ## [289] "Var216_lev_x_kZJyVg2"                  
    ## [290] "Var216_lev_x_mAja5EA"                  
    ## [291] "Var216_lev_x_mAjbk_S"                  
    ## [292] "Var216_lev_x_NGZxnJM"                  
    ## [293] "Var216_lev_x_XTbPUYD"                  
    ## [294] "Var217_catB"                           
    ## [295] "Var218_catB"                           
    ## [296] "Var218_lev_x_cJvF"                     
    ## [297] "Var218_lev_x_UYBR"                     
    ## [298] "Var219_catB"                           
    ## [299] "Var219_lev_x_AU8pNoi"                  
    ## [300] "Var219_lev_x_FzaX"                     
    ## [301] "Var219_lev_x_NA"                       
    ## [302] "Var219_lev_x_qxDb"                     
    ## [303] "Var22_clean"                           
    ## [304] "Var22_isBAD"                           
    ## [305] "Var220_catB"                           
    ## [306] "Var220_lev_x_4UxGlow"                  
    ## [307] "Var220_lev_x_UF16siJ"                  
    ## [308] "Var221_catB"                           
    ## [309] "Var221_lev_x_Al6ZaUT"                  
    ## [310] "Var221_lev_x_d0EEeJi"                  
    ## [311] "Var221_lev_x_oslk"                     
    ## [312] "Var221_lev_x_QKW8DRm"                  
    ## [313] "Var221_lev_x_zCkv"                     
    ## [314] "Var222_catB"                           
    ## [315] "Var222_lev_x_APgdzOv"                  
    ## [316] "Var222_lev_x_catzS2D"                  
    ## [317] "Var223_catB"                           
    ## [318] "Var223_lev_x_jySVZNlOJy"               
    ## [319] "Var223_lev_x_LM8l689qOp"               
    ## [320] "Var223_lev_x_M_8D"                     
    ## [321] "Var223_lev_x_NA"                       
    ## [322] "Var224_lev_x_NA"                       
    ## [323] "Var225_catB"                           
    ## [324] "Var225_lev_x_ELof"                     
    ## [325] "Var225_lev_x_kG3k"                     
    ## [326] "Var225_lev_x_NA"                       
    ## [327] "Var225_lev_x_xG3x"                     
    ## [328] "Var226_catB"                           
    ## [329] "Var226_lev_x_3Cy4"                     
    ## [330] "Var226_lev_x_453m"                     
    ## [331] "Var226_lev_x_5Acm"                     
    ## [332] "Var226_lev_x_7aLG"                     
    ## [333] "Var226_lev_x_7P5s"                     
    ## [334] "Var226_lev_x_Aoh3"                     
    ## [335] "Var226_lev_x_fKCe"                     
    ## [336] "Var226_lev_x_FSa2"                     
    ## [337] "Var226_lev_x_kwS7"                     
    ## [338] "Var226_lev_x_me1d"                     
    ## [339] "Var226_lev_x_PM2D"                     
    ## [340] "Var226_lev_x_Qcbd"                     
    ## [341] "Var226_lev_x_Qu4f"                     
    ## [342] "Var226_lev_x_rgKb"                     
    ## [343] "Var226_lev_x_szEZ"                     
    ## [344] "Var226_lev_x_TNEC"                     
    ## [345] "Var226_lev_x_uWr3"                     
    ## [346] "Var226_lev_x_WqMG"                     
    ## [347] "Var226_lev_x_Xa3G"                     
    ## [348] "Var226_lev_x_xb3V"                     
    ## [349] "Var227_catB"                           
    ## [350] "Var227_lev_x_02N6s8f"                  
    ## [351] "Var227_lev_x_6fzt"                     
    ## [352] "Var227_lev_x_nIGXDli"                  
    ## [353] "Var227_lev_x_RAYp"                     
    ## [354] "Var227_lev_x_ZI9m"                     
    ## [355] "Var228_catB"                           
    ## [356] "Var228_lev_x_55YFVY9"                  
    ## [357] "Var228_lev_x_F2FyR07IdsN7I"            
    ## [358] "Var228_lev_x_ib5G6X1eUxUn6"            
    ## [359] "Var228_lev_x_iyHGyLCEkQ"               
    ## [360] "Var228_lev_x_R4y5gQQWY8OodqDV"         
    ## [361] "Var228_lev_x_TCU50_Yjmm6GIBZ0lL_"      
    ## [362] "Var228_lev_x_xwM2aC7IdeMC0"            
    ## [363] "Var229_catB"                           
    ## [364] "Var229_lev_x_am7c"                     
    ## [365] "Var229_lev_x_mj86"                     
    ## [366] "Var229_lev_x_NA"                       
    ## [367] "Var23_clean"                           
    ## [368] "Var23_isBAD"                           
    ## [369] "Var24_clean"                           
    ## [370] "Var24_isBAD"                           
    ## [371] "Var25_clean"                           
    ## [372] "Var25_isBAD"                           
    ## [373] "Var26_clean"                           
    ## [374] "Var26_isBAD"                           
    ## [375] "Var27_clean"                           
    ## [376] "Var27_isBAD"                           
    ## [377] "Var28_clean"                           
    ## [378] "Var28_isBAD"                           
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
    ## [396] "Var4_clean"                            
    ## [397] "Var4_isBAD"                            
    ## [398] "Var40_clean"                           
    ## [399] "Var40_isBAD"                           
    ## [400] "Var41_clean"                           
    ## [401] "Var41_isBAD"                           
    ## [402] "Var43_clean"                           
    ## [403] "Var43_isBAD"                           
    ## [404] "Var44_clean"                           
    ## [405] "Var44_isBAD"                           
    ## [406] "Var45_clean"                           
    ## [407] "Var45_isBAD"                           
    ## [408] "Var46_clean"                           
    ## [409] "Var46_isBAD"                           
    ## [410] "Var47_clean"                           
    ## [411] "Var47_isBAD"                           
    ## [412] "Var49_clean"                           
    ## [413] "Var49_isBAD"                           
    ## [414] "Var5_clean"                            
    ## [415] "Var5_isBAD"                            
    ## [416] "Var50_clean"                           
    ## [417] "Var50_isBAD"                           
    ## [418] "Var51_clean"                           
    ## [419] "Var51_isBAD"                           
    ## [420] "Var53_clean"                           
    ## [421] "Var53_isBAD"                           
    ## [422] "Var54_clean"                           
    ## [423] "Var54_isBAD"                           
    ## [424] "Var56_clean"                           
    ## [425] "Var56_isBAD"                           
    ## [426] "Var57_clean"                           
    ## [427] "Var58_clean"                           
    ## [428] "Var58_isBAD"                           
    ## [429] "Var59_clean"                           
    ## [430] "Var59_isBAD"                           
    ## [431] "Var6_clean"                            
    ## [432] "Var6_isBAD"                            
    ## [433] "Var60_clean"                           
    ## [434] "Var60_isBAD"                           
    ## [435] "Var61_clean"                           
    ## [436] "Var61_isBAD"                           
    ## [437] "Var62_clean"                           
    ## [438] "Var62_isBAD"                           
    ## [439] "Var63_clean"                           
    ## [440] "Var63_isBAD"                           
    ## [441] "Var64_clean"                           
    ## [442] "Var64_isBAD"                           
    ## [443] "Var65_clean"                           
    ## [444] "Var65_isBAD"                           
    ## [445] "Var66_clean"                           
    ## [446] "Var66_isBAD"                           
    ## [447] "Var67_clean"                           
    ## [448] "Var67_isBAD"                           
    ## [449] "Var68_clean"                           
    ## [450] "Var68_isBAD"                           
    ## [451] "Var69_clean"                           
    ## [452] "Var69_isBAD"                           
    ## [453] "Var7_clean"                            
    ## [454] "Var7_isBAD"                            
    ## [455] "Var70_clean"                           
    ## [456] "Var70_isBAD"                           
    ## [457] "Var71_clean"                           
    ## [458] "Var71_isBAD"                           
    ## [459] "Var72_clean"                           
    ## [460] "Var72_isBAD"                           
    ## [461] "Var73_clean"                           
    ## [462] "Var74_clean"                           
    ## [463] "Var74_isBAD"                           
    ## [464] "Var75_clean"                           
    ## [465] "Var75_isBAD"                           
    ## [466] "Var76_clean"                           
    ## [467] "Var76_isBAD"                           
    ## [468] "Var77_clean"                           
    ## [469] "Var77_isBAD"                           
    ## [470] "Var78_clean"                           
    ## [471] "Var78_isBAD"                           
    ## [472] "Var80_clean"                           
    ## [473] "Var80_isBAD"                           
    ## [474] "Var81_clean"                           
    ## [475] "Var81_isBAD"                           
    ## [476] "Var82_clean"                           
    ## [477] "Var82_isBAD"                           
    ## [478] "Var83_clean"                           
    ## [479] "Var83_isBAD"                           
    ## [480] "Var84_clean"                           
    ## [481] "Var84_isBAD"                           
    ## [482] "Var85_clean"                           
    ## [483] "Var85_isBAD"                           
    ## [484] "Var86_clean"                           
    ## [485] "Var86_isBAD"                           
    ## [486] "Var87_clean"                           
    ## [487] "Var87_isBAD"                           
    ## [488] "Var88_clean"                           
    ## [489] "Var88_isBAD"                           
    ## [490] "Var89_clean"                           
    ## [491] "Var89_isBAD"                           
    ## [492] "Var9_clean"                            
    ## [493] "Var9_isBAD"                            
    ## [494] "Var90_isBAD"                           
    ## [495] "Var91_clean"                           
    ## [496] "Var91_isBAD"                           
    ## [497] "Var92_clean"                           
    ## [498] "Var92_isBAD"                           
    ## [499] "Var93_clean"                           
    ## [500] "Var93_isBAD"                           
    ## [501] "Var94_clean"                           
    ## [502] "Var94_isBAD"                           
    ## [503] "Var95_clean"                           
    ## [504] "Var95_isBAD"                           
    ## [505] "Var96_clean"                           
    ## [506] "Var96_isBAD"                           
    ## [507] "Var97_clean"                           
    ## [508] "Var97_isBAD"                           
    ## [509] "Var98_clean"                           
    ## [510] "Var98_isBAD"                           
    ## [511] "Var99_clean"                           
    ## [512] "Var99_isBAD"

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_34616963887310456356_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 4968 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  512 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 1 -1 1 -1
    ##  $ Var1_clean                            : num  5.68 5.68 5.68 5.68 5.68 ...
    ##  $ Var1_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var10_clean                           : num  545656 545656 545656 545656 545656 ...
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var100_clean                          : num  0.677 0.677 0.677 0.677 0.677 ...
    ##  $ Var100_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var101_clean                          : num  31.9 31.9 31.9 31.9 31.9 ...
    ##  $ Var101_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var102_clean                          : num  31377 31377 31377 31377 31377 ...
    ##  $ Var102_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_clean                          : num  28.5 28.5 28.5 28.5 28.5 ...
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var104_clean                          : num  101 101 101 101 101 ...
    ##  $ Var104_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var105_clean                          : num  67.3 67.3 67.3 67.3 67.3 ...
    ##  $ Var105_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_clean                          : num  55035 55035 55035 55035 55035 ...
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_clean                          : num  3.5 3.5 3.5 3.5 3.5 3.5 3.5 3.5 3.5 3.5
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var108_clean                          : num  124506 124506 124506 124506 124506 ...
    ##  $ Var108_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var109_clean                          : num  16 16 24 128 58.5 ...
    ##  $ Var109_isBAD                          : int  0 0 0 0 1 1 0 0 0 0
    ##  $ Var11_clean                           : num  8.63 8.63 8.63 8.63 8.63 ...
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var110_clean                          : num  6.48 6.48 6.48 6.48 6.48 ...
    ##  $ Var110_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_clean                          : num  372467 372467 372467 372467 372467 ...
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_clean                          : num  16 0 16 64 0 0 16 64 32 48
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -2135 -1259516 -1798800 201331 -449256 ...
    ##  $ Var114_clean                          : num  746483 746483 746483 746483 746483 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var115_clean                          : num  40.4 40.4 40.4 40.4 40.4 ...
    ##  $ Var115_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var116_clean                          : num  0.145 0.145 0.145 0.145 0.145 ...
    ##  $ Var116_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_clean                          : num  132534 132534 132534 132534 132534 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var118_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_clean                          : num  20 85 75 8580 25 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var12_clean                           : num  10.4 10.4 10.4 10.4 10.4 ...
    ##  $ Var12_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var120_clean                          : num  29 29 29 29 29 ...
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var121_clean                          : num  4.1 4.1 4.1 4.1 4.1 ...
    ##  $ Var121_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_clean                          : num  0.0236 0.0236 0.0236 0.0236 0.0236 ...
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_clean                          : num  0 0 0 270 0 0 120 24 12 102
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_clean                          : num  271331 271331 271331 271331 271331 ...
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_clean                          : num  2574 252 30294 7119 52380 ...
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  0.0764 -22 -18 4 2 ...
    ##  $ Var126_isBAD                          : int  1 0 0 0 0 0 0 0 1 1
    ##  $ Var127_clean                          : num  39.4 39.4 39.4 39.4 39.4 ...
    ##  $ Var127_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var128_clean                          : num  123 123 123 123 123 ...
    ##  $ Var128_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var129_clean                          : num  6.68 6.68 6.68 6.68 6.68 ...
    ##  $ Var129_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var13_clean                           : num  348 0 512 2104 3832 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_clean                          : num  0.661 0.661 0.661 0.661 0.661 ...
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var131_clean                          : num  11311943 11311943 11311943 11311943 11311943 ...
    ##  $ Var131_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_clean                          : num  0 0 0 0 0 0 0 0 0 8
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_clean                          : num  0 17915 0 8168400 0 ...
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_clean                          : num  0 13300 19136 220560 0 ...
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_clean                          : num  198 198 198 198 198 ...
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var136_clean                          : num  1e+05 1e+05 1e+05 1e+05 1e+05 ...
    ##  $ Var136_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var137_clean                          : num  4.97 4.97 4.97 4.97 4.97 ...
    ##  $ Var137_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_clean                          : num  190192 190192 190192 190192 190192 ...
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_clean                           : num  1.04 1.04 1.04 1.04 1.04 ...
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  0 0 10 180 13975 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var142_clean                          : num  1.29 1.29 1.29 1.29 1.29 ...
    ##  $ Var142_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var143_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  9 0 0 18 9 ...
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var145_clean                          : num  76 76 76 76 76 ...
    ##   [list output truncated]
