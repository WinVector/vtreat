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

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 08:57:36 2018"
    ## [1] "designing treatments Sat Jun 16 08:57:36 2018"
    ## [1] " have initial level statistics Sat Jun 16 08:57:36 2018"
    ## [1] " scoring treatments Sat Jun 16 08:57:39 2018"
    ## [1] "have treatment plan Sat Jun 16 08:57:44 2018"
    ## [1] "rescoring complex variables Sat Jun 16 08:57:44 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 08:57:49 2018"

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
    ##  [39] "Var116_isBAD"                          
    ##  [40] "Var117_clean"                          
    ##  [41] "Var117_isBAD"                          
    ##  [42] "Var118_isBAD"                          
    ##  [43] "Var119_clean"                          
    ##  [44] "Var119_isBAD"                          
    ##  [45] "Var12_clean"                           
    ##  [46] "Var12_isBAD"                           
    ##  [47] "Var120_clean"                          
    ##  [48] "Var120_isBAD"                          
    ##  [49] "Var121_clean"                          
    ##  [50] "Var121_isBAD"                          
    ##  [51] "Var122_clean"                          
    ##  [52] "Var122_isBAD"                          
    ##  [53] "Var123_clean"                          
    ##  [54] "Var123_isBAD"                          
    ##  [55] "Var124_clean"                          
    ##  [56] "Var124_isBAD"                          
    ##  [57] "Var125_clean"                          
    ##  [58] "Var125_isBAD"                          
    ##  [59] "Var126_clean"                          
    ##  [60] "Var126_isBAD"                          
    ##  [61] "Var127_clean"                          
    ##  [62] "Var127_isBAD"                          
    ##  [63] "Var128_clean"                          
    ##  [64] "Var128_isBAD"                          
    ##  [65] "Var129_clean"                          
    ##  [66] "Var129_isBAD"                          
    ##  [67] "Var13_clean"                           
    ##  [68] "Var13_isBAD"                           
    ##  [69] "Var130_clean"                          
    ##  [70] "Var130_isBAD"                          
    ##  [71] "Var131_clean"                          
    ##  [72] "Var131_isBAD"                          
    ##  [73] "Var132_clean"                          
    ##  [74] "Var132_isBAD"                          
    ##  [75] "Var133_clean"                          
    ##  [76] "Var133_isBAD"                          
    ##  [77] "Var134_clean"                          
    ##  [78] "Var134_isBAD"                          
    ##  [79] "Var135_clean"                          
    ##  [80] "Var135_isBAD"                          
    ##  [81] "Var136_clean"                          
    ##  [82] "Var136_isBAD"                          
    ##  [83] "Var137_clean"                          
    ##  [84] "Var137_isBAD"                          
    ##  [85] "Var138_isBAD"                          
    ##  [86] "Var139_clean"                          
    ##  [87] "Var139_isBAD"                          
    ##  [88] "Var14_clean"                           
    ##  [89] "Var14_isBAD"                           
    ##  [90] "Var140_clean"                          
    ##  [91] "Var140_isBAD"                          
    ##  [92] "Var142_clean"                          
    ##  [93] "Var142_isBAD"                          
    ##  [94] "Var143_clean"                          
    ##  [95] "Var143_isBAD"                          
    ##  [96] "Var144_clean"                          
    ##  [97] "Var144_isBAD"                          
    ##  [98] "Var145_clean"                          
    ##  [99] "Var145_isBAD"                          
    ## [100] "Var146_clean"                          
    ## [101] "Var146_isBAD"                          
    ## [102] "Var147_clean"                          
    ## [103] "Var147_isBAD"                          
    ## [104] "Var148_clean"                          
    ## [105] "Var148_isBAD"                          
    ## [106] "Var149_clean"                          
    ## [107] "Var149_isBAD"                          
    ## [108] "Var150_clean"                          
    ## [109] "Var150_isBAD"                          
    ## [110] "Var151_clean"                          
    ## [111] "Var151_isBAD"                          
    ## [112] "Var152_clean"                          
    ## [113] "Var152_isBAD"                          
    ## [114] "Var153_clean"                          
    ## [115] "Var153_isBAD"                          
    ## [116] "Var154_clean"                          
    ## [117] "Var154_isBAD"                          
    ## [118] "Var155_clean"                          
    ## [119] "Var155_isBAD"                          
    ## [120] "Var156_clean"                          
    ## [121] "Var156_isBAD"                          
    ## [122] "Var157_clean"                          
    ## [123] "Var157_isBAD"                          
    ## [124] "Var158_clean"                          
    ## [125] "Var158_isBAD"                          
    ## [126] "Var159_clean"                          
    ## [127] "Var159_isBAD"                          
    ## [128] "Var16_clean"                           
    ## [129] "Var16_isBAD"                           
    ## [130] "Var160_clean"                          
    ## [131] "Var160_isBAD"                          
    ## [132] "Var161_clean"                          
    ## [133] "Var161_isBAD"                          
    ## [134] "Var162_clean"                          
    ## [135] "Var162_isBAD"                          
    ## [136] "Var163_clean"                          
    ## [137] "Var163_isBAD"                          
    ## [138] "Var164_clean"                          
    ## [139] "Var164_isBAD"                          
    ## [140] "Var165_clean"                          
    ## [141] "Var165_isBAD"                          
    ## [142] "Var166_clean"                          
    ## [143] "Var166_isBAD"                          
    ## [144] "Var168_clean"                          
    ## [145] "Var168_isBAD"                          
    ## [146] "Var17_clean"                           
    ## [147] "Var17_isBAD"                           
    ## [148] "Var170_clean"                          
    ## [149] "Var170_isBAD"                          
    ## [150] "Var171_clean"                          
    ## [151] "Var171_isBAD"                          
    ## [152] "Var172_clean"                          
    ## [153] "Var172_isBAD"                          
    ## [154] "Var173_clean"                          
    ## [155] "Var173_isBAD"                          
    ## [156] "Var174_clean"                          
    ## [157] "Var174_isBAD"                          
    ## [158] "Var176_clean"                          
    ## [159] "Var176_isBAD"                          
    ## [160] "Var177_clean"                          
    ## [161] "Var177_isBAD"                          
    ## [162] "Var178_clean"                          
    ## [163] "Var178_isBAD"                          
    ## [164] "Var179_clean"                          
    ## [165] "Var179_isBAD"                          
    ## [166] "Var18_clean"                           
    ## [167] "Var18_isBAD"                           
    ## [168] "Var180_clean"                          
    ## [169] "Var180_isBAD"                          
    ## [170] "Var181_clean"                          
    ## [171] "Var181_isBAD"                          
    ## [172] "Var182_clean"                          
    ## [173] "Var182_isBAD"                          
    ## [174] "Var183_clean"                          
    ## [175] "Var183_isBAD"                          
    ## [176] "Var184_clean"                          
    ## [177] "Var184_isBAD"                          
    ## [178] "Var186_clean"                          
    ## [179] "Var186_isBAD"                          
    ## [180] "Var187_clean"                          
    ## [181] "Var187_isBAD"                          
    ## [182] "Var188_clean"                          
    ## [183] "Var188_isBAD"                          
    ## [184] "Var189_clean"                          
    ## [185] "Var189_isBAD"                          
    ## [186] "Var19_clean"                           
    ## [187] "Var19_isBAD"                           
    ## [188] "Var190_clean"                          
    ## [189] "Var190_isBAD"                          
    ## [190] "Var191_lev_x_NA"                       
    ## [191] "Var191_lev_x_r_I"                      
    ## [192] "Var192_catB"                           
    ## [193] "Var193_catB"                           
    ## [194] "Var193_lev_x_2Knk1KF"                  
    ## [195] "Var193_lev_x_AERks4l"                  
    ## [196] "Var193_lev_x_RO12"                     
    ## [197] "Var194_catB"                           
    ## [198] "Var194_lev_x_NA"                       
    ## [199] "Var194_lev_x_SEuy"                     
    ## [200] "Var195_catB"                           
    ## [201] "Var195_lev_x_taul"                     
    ## [202] "Var196_lev_x_1K8T"                     
    ## [203] "Var197_catB"                           
    ## [204] "Var197_lev_x_0Xwj"                     
    ## [205] "Var197_lev_x_487l"                     
    ## [206] "Var197_lev_x_JLbT"                     
    ## [207] "Var197_lev_x_lK27"                     
    ## [208] "Var197_lev_x_ssAy"                     
    ## [209] "Var197_lev_x_TyGl"                     
    ## [210] "Var197_lev_x_z32l"                     
    ## [211] "Var198_catB"                           
    ## [212] "Var198_lev_x_fhk21Ss"                  
    ## [213] "Var198_lev_x_PHNvXy8"                  
    ## [214] "Var199_catB"                           
    ## [215] "Var199_lev_x_r83_sZi"                  
    ## [216] "Var2_isBAD"                            
    ## [217] "Var200_catB"                           
    ## [218] "Var200_lev_x_NA"                       
    ## [219] "Var201_lev_x_NA"                       
    ## [220] "Var201_lev_x_smXZ"                     
    ## [221] "Var202_catB"                           
    ## [222] "Var203_catB"                           
    ## [223] "Var203_lev_x_9_Y1"                     
    ## [224] "Var203_lev_x_F3hy"                     
    ## [225] "Var203_lev_x_HLqf"                     
    ## [226] "Var204_catB"                           
    ## [227] "Var204_lev_x_DtNL"                     
    ## [228] "Var204_lev_x_k13i"                     
    ## [229] "Var204_lev_x_m_h1"                     
    ## [230] "Var204_lev_x_rGJy"                     
    ## [231] "Var204_lev_x_RVjC"                     
    ## [232] "Var204_lev_x_SkZj"                     
    ## [233] "Var204_lev_x_z5Ry"                     
    ## [234] "Var205_catB"                           
    ## [235] "Var205_lev_x_09_Q"                     
    ## [236] "Var205_lev_x_NA"                       
    ## [237] "Var205_lev_x_sJzTlal"                  
    ## [238] "Var205_lev_x_VpdQ"                     
    ## [239] "Var206_catB"                           
    ## [240] "Var206_lev_x_43pnToF"                  
    ## [241] "Var206_lev_x_6JmL"                     
    ## [242] "Var206_lev_x_hAFG"                     
    ## [243] "Var206_lev_x_haYg"                     
    ## [244] "Var206_lev_x_IYzP"                     
    ## [245] "Var206_lev_x_kxE9"                     
    ## [246] "Var206_lev_x_NA"                       
    ## [247] "Var206_lev_x_sYC_"                     
    ## [248] "Var206_lev_x_wMei"                     
    ## [249] "Var206_lev_x_y6dw"                     
    ## [250] "Var206_lev_x_zm5i"                     
    ## [251] "Var207_catB"                           
    ## [252] "Var207_lev_x_7M47J5GA0pTYIFxg5uy"      
    ## [253] "Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_"
    ## [254] "Var207_lev_x_Kxdu"                     
    ## [255] "Var207_lev_x_me75fM6ugJ"               
    ## [256] "Var207_lev_x_NKv3VA1BpP"               
    ## [257] "Var208_catB"                           
    ## [258] "Var208_lev_x_kIsH"                     
    ## [259] "Var208_lev_x_sBgB"                     
    ## [260] "Var21_clean"                           
    ## [261] "Var21_isBAD"                           
    ## [262] "Var210_catB"                           
    ## [263] "Var210_lev_x_g5HH"                     
    ## [264] "Var210_lev_x_uKAI"                     
    ## [265] "Var211_lev_x_L84s"                     
    ## [266] "Var211_lev_x_Mtgm"                     
    ## [267] "Var212_catB"                           
    ## [268] "Var212_lev_x_CrNX"                     
    ## [269] "Var212_lev_x_FMSzZ91zL2"               
    ## [270] "Var212_lev_x_Ie_5MZs"                  
    ## [271] "Var212_lev_x_NhsEn4L"                  
    ## [272] "Var212_lev_x_XfqtO3UdzaXh_"            
    ## [273] "Var213_lev_x_KdSa"                     
    ## [274] "Var213_lev_x_NA"                       
    ## [275] "Var214_catB"                           
    ## [276] "Var214_lev_x_NA"                       
    ## [277] "Var215_lev_x_NA"                       
    ## [278] "Var216_catB"                           
    ## [279] "Var216_lev_x_11p4mKe"                  
    ## [280] "Var216_lev_x_beK4AFX"                  
    ## [281] "Var216_lev_x_kZJtVhC"                  
    ## [282] "Var216_lev_x_kZJyVg2"                  
    ## [283] "Var216_lev_x_mAja5EA"                  
    ## [284] "Var216_lev_x_mAjbk_S"                  
    ## [285] "Var216_lev_x_NGZxnJM"                  
    ## [286] "Var216_lev_x_XTbPUYD"                  
    ## [287] "Var217_catB"                           
    ## [288] "Var218_catB"                           
    ## [289] "Var218_lev_x_cJvF"                     
    ## [290] "Var218_lev_x_UYBR"                     
    ## [291] "Var219_catB"                           
    ## [292] "Var219_lev_x_AU8pNoi"                  
    ## [293] "Var219_lev_x_FzaX"                     
    ## [294] "Var219_lev_x_NA"                       
    ## [295] "Var219_lev_x_qxDb"                     
    ## [296] "Var22_clean"                           
    ## [297] "Var22_isBAD"                           
    ## [298] "Var220_catB"                           
    ## [299] "Var220_lev_x_4UxGlow"                  
    ## [300] "Var220_lev_x_UF16siJ"                  
    ## [301] "Var221_catB"                           
    ## [302] "Var221_lev_x_Al6ZaUT"                  
    ## [303] "Var221_lev_x_d0EEeJi"                  
    ## [304] "Var221_lev_x_oslk"                     
    ## [305] "Var221_lev_x_QKW8DRm"                  
    ## [306] "Var221_lev_x_zCkv"                     
    ## [307] "Var222_catB"                           
    ## [308] "Var222_lev_x_APgdzOv"                  
    ## [309] "Var222_lev_x_catzS2D"                  
    ## [310] "Var223_catB"                           
    ## [311] "Var223_lev_x_jySVZNlOJy"               
    ## [312] "Var223_lev_x_LM8l689qOp"               
    ## [313] "Var223_lev_x_M_8D"                     
    ## [314] "Var223_lev_x_NA"                       
    ## [315] "Var224_lev_x_NA"                       
    ## [316] "Var225_catB"                           
    ## [317] "Var225_lev_x_ELof"                     
    ## [318] "Var225_lev_x_kG3k"                     
    ## [319] "Var225_lev_x_NA"                       
    ## [320] "Var225_lev_x_xG3x"                     
    ## [321] "Var226_catB"                           
    ## [322] "Var226_lev_x_3Cy4"                     
    ## [323] "Var226_lev_x_453m"                     
    ## [324] "Var226_lev_x_5Acm"                     
    ## [325] "Var226_lev_x_7aLG"                     
    ## [326] "Var226_lev_x_7P5s"                     
    ## [327] "Var226_lev_x_Aoh3"                     
    ## [328] "Var226_lev_x_fKCe"                     
    ## [329] "Var226_lev_x_FSa2"                     
    ## [330] "Var226_lev_x_kwS7"                     
    ## [331] "Var226_lev_x_me1d"                     
    ## [332] "Var226_lev_x_PM2D"                     
    ## [333] "Var226_lev_x_Qcbd"                     
    ## [334] "Var226_lev_x_Qu4f"                     
    ## [335] "Var226_lev_x_rgKb"                     
    ## [336] "Var226_lev_x_szEZ"                     
    ## [337] "Var226_lev_x_TNEC"                     
    ## [338] "Var226_lev_x_uWr3"                     
    ## [339] "Var226_lev_x_WqMG"                     
    ## [340] "Var226_lev_x_Xa3G"                     
    ## [341] "Var226_lev_x_xb3V"                     
    ## [342] "Var227_catB"                           
    ## [343] "Var227_lev_x_02N6s8f"                  
    ## [344] "Var227_lev_x_6fzt"                     
    ## [345] "Var227_lev_x_nIGXDli"                  
    ## [346] "Var227_lev_x_RAYp"                     
    ## [347] "Var227_lev_x_ZI9m"                     
    ## [348] "Var228_catB"                           
    ## [349] "Var228_lev_x_55YFVY9"                  
    ## [350] "Var228_lev_x_F2FyR07IdsN7I"            
    ## [351] "Var228_lev_x_ib5G6X1eUxUn6"            
    ## [352] "Var228_lev_x_iyHGyLCEkQ"               
    ## [353] "Var228_lev_x_R4y5gQQWY8OodqDV"         
    ## [354] "Var228_lev_x_TCU50_Yjmm6GIBZ0lL_"      
    ## [355] "Var228_lev_x_xwM2aC7IdeMC0"            
    ## [356] "Var229_catB"                           
    ## [357] "Var229_lev_x_am7c"                     
    ## [358] "Var229_lev_x_mj86"                     
    ## [359] "Var229_lev_x_NA"                       
    ## [360] "Var23_clean"                           
    ## [361] "Var23_isBAD"                           
    ## [362] "Var24_clean"                           
    ## [363] "Var24_isBAD"                           
    ## [364] "Var25_clean"                           
    ## [365] "Var25_isBAD"                           
    ## [366] "Var26_clean"                           
    ## [367] "Var26_isBAD"                           
    ## [368] "Var27_clean"                           
    ## [369] "Var27_isBAD"                           
    ## [370] "Var28_clean"                           
    ## [371] "Var28_isBAD"                           
    ## [372] "Var29_clean"                           
    ## [373] "Var29_isBAD"                           
    ## [374] "Var3_clean"                            
    ## [375] "Var3_isBAD"                            
    ## [376] "Var30_clean"                           
    ## [377] "Var30_isBAD"                           
    ## [378] "Var33_clean"                           
    ## [379] "Var33_isBAD"                           
    ## [380] "Var34_clean"                           
    ## [381] "Var34_isBAD"                           
    ## [382] "Var35_clean"                           
    ## [383] "Var35_isBAD"                           
    ## [384] "Var36_clean"                           
    ## [385] "Var36_isBAD"                           
    ## [386] "Var37_clean"                           
    ## [387] "Var37_isBAD"                           
    ## [388] "Var38_clean"                           
    ## [389] "Var38_isBAD"                           
    ## [390] "Var4_clean"                            
    ## [391] "Var4_isBAD"                            
    ## [392] "Var40_clean"                           
    ## [393] "Var40_isBAD"                           
    ## [394] "Var41_clean"                           
    ## [395] "Var41_isBAD"                           
    ## [396] "Var43_clean"                           
    ## [397] "Var43_isBAD"                           
    ## [398] "Var44_clean"                           
    ## [399] "Var44_isBAD"                           
    ## [400] "Var45_clean"                           
    ## [401] "Var45_isBAD"                           
    ## [402] "Var46_clean"                           
    ## [403] "Var46_isBAD"                           
    ## [404] "Var47_clean"                           
    ## [405] "Var47_isBAD"                           
    ## [406] "Var49_clean"                           
    ## [407] "Var49_isBAD"                           
    ## [408] "Var5_clean"                            
    ## [409] "Var5_isBAD"                            
    ## [410] "Var50_clean"                           
    ## [411] "Var50_isBAD"                           
    ## [412] "Var51_clean"                           
    ## [413] "Var51_isBAD"                           
    ## [414] "Var53_clean"                           
    ## [415] "Var53_isBAD"                           
    ## [416] "Var54_clean"                           
    ## [417] "Var54_isBAD"                           
    ## [418] "Var56_clean"                           
    ## [419] "Var56_isBAD"                           
    ## [420] "Var57_clean"                           
    ## [421] "Var58_clean"                           
    ## [422] "Var58_isBAD"                           
    ## [423] "Var59_clean"                           
    ## [424] "Var59_isBAD"                           
    ## [425] "Var6_clean"                            
    ## [426] "Var6_isBAD"                            
    ## [427] "Var60_clean"                           
    ## [428] "Var60_isBAD"                           
    ## [429] "Var61_clean"                           
    ## [430] "Var61_isBAD"                           
    ## [431] "Var62_clean"                           
    ## [432] "Var62_isBAD"                           
    ## [433] "Var63_clean"                           
    ## [434] "Var63_isBAD"                           
    ## [435] "Var64_clean"                           
    ## [436] "Var64_isBAD"                           
    ## [437] "Var65_clean"                           
    ## [438] "Var65_isBAD"                           
    ## [439] "Var66_clean"                           
    ## [440] "Var66_isBAD"                           
    ## [441] "Var67_clean"                           
    ## [442] "Var67_isBAD"                           
    ## [443] "Var68_clean"                           
    ## [444] "Var68_isBAD"                           
    ## [445] "Var69_clean"                           
    ## [446] "Var69_isBAD"                           
    ## [447] "Var7_clean"                            
    ## [448] "Var7_isBAD"                            
    ## [449] "Var70_clean"                           
    ## [450] "Var70_isBAD"                           
    ## [451] "Var71_clean"                           
    ## [452] "Var71_isBAD"                           
    ## [453] "Var72_clean"                           
    ## [454] "Var72_isBAD"                           
    ## [455] "Var73_clean"                           
    ## [456] "Var74_clean"                           
    ## [457] "Var74_isBAD"                           
    ## [458] "Var75_clean"                           
    ## [459] "Var75_isBAD"                           
    ## [460] "Var76_clean"                           
    ## [461] "Var76_isBAD"                           
    ## [462] "Var77_clean"                           
    ## [463] "Var77_isBAD"                           
    ## [464] "Var78_clean"                           
    ## [465] "Var78_isBAD"                           
    ## [466] "Var80_clean"                           
    ## [467] "Var80_isBAD"                           
    ## [468] "Var81_clean"                           
    ## [469] "Var81_isBAD"                           
    ## [470] "Var82_clean"                           
    ## [471] "Var82_isBAD"                           
    ## [472] "Var83_clean"                           
    ## [473] "Var83_isBAD"                           
    ## [474] "Var84_clean"                           
    ## [475] "Var84_isBAD"                           
    ## [476] "Var85_clean"                           
    ## [477] "Var85_isBAD"                           
    ## [478] "Var86_clean"                           
    ## [479] "Var86_isBAD"                           
    ## [480] "Var87_clean"                           
    ## [481] "Var87_isBAD"                           
    ## [482] "Var88_clean"                           
    ## [483] "Var88_isBAD"                           
    ## [484] "Var89_clean"                           
    ## [485] "Var89_isBAD"                           
    ## [486] "Var9_clean"                            
    ## [487] "Var9_isBAD"                            
    ## [488] "Var90_isBAD"                           
    ## [489] "Var91_clean"                           
    ## [490] "Var91_isBAD"                           
    ## [491] "Var92_clean"                           
    ## [492] "Var92_isBAD"                           
    ## [493] "Var93_clean"                           
    ## [494] "Var93_isBAD"                           
    ## [495] "Var94_clean"                           
    ## [496] "Var94_isBAD"                           
    ## [497] "Var95_clean"                           
    ## [498] "Var95_isBAD"                           
    ## [499] "Var96_clean"                           
    ## [500] "Var96_isBAD"                           
    ## [501] "Var97_clean"                           
    ## [502] "Var97_isBAD"                           
    ## [503] "Var98_clean"                           
    ## [504] "Var98_isBAD"                           
    ## [505] "Var99_clean"                           
    ## [506] "Var99_isBAD"

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_21800584103307670504_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 4982 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  506 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var1_clean                            : num  10.1 10.1 10.1 10.1 10.1 ...
    ##  $ Var1_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var10_clean                           : num  369787 369787 369787 369787 369787 ...
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var100_clean                          : num  1.38 1.38 1.38 1.38 1.38 ...
    ##  $ Var100_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var101_clean                          : num  13.5 13.5 13.5 13.5 13.5 13.5 13.5 13.5 13.5 13.5
    ##  $ Var101_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var102_clean                          : num  23599 23599 23599 23599 23599 ...
    ##  $ Var102_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_clean                          : num  18.3 18.3 18.3 18.3 18.3 ...
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var104_clean                          : num  95.7 95.7 95.7 95.7 95.7 ...
    ##  $ Var104_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var105_clean                          : num  63.8 63.8 63.8 63.8 63.8 ...
    ##  $ Var105_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_clean                          : num  42006 42006 42006 42006 42006 ...
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_clean                          : num  4.4 4.4 4.4 4.4 4.4 ...
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var108_clean                          : num  228463 228463 228463 228463 228463 ...
    ##  $ Var108_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var109_clean                          : num  32 80 8 16 32 32 56 96 40 32
    ##  $ Var109_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var11_clean                           : num  8.81 8.81 8.81 8.81 8.81 ...
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var110_clean                          : num  7.01 7.01 7.01 7.01 7.01 ...
    ##  $ Var110_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_clean                          : num  340260 340260 340260 340260 340260 ...
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_clean                          : num  16 88 0 8 32 32 48 88 120 72
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -38397 -709072 -87345 9859 157083 ...
    ##  $ Var114_clean                          : num  643220 643220 643220 643220 643220 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var115_clean                          : num  38.9 38.9 38.9 38.9 38.9 ...
    ##  $ Var115_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var116_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_clean                          : num  137248 137248 137248 137248 137248 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var118_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_clean                          : num  610 1480 250 355 630 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var12_clean                           : num  17.7 17.7 17.7 17.7 17.7 ...
    ##  $ Var12_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var120_clean                          : num  29.9 29.9 29.9 29.9 29.9 ...
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var121_clean                          : num  5.15 5.15 5.15 5.15 5.15 ...
    ##  $ Var121_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_clean                          : num  0.0756 0.0756 0.0756 0.0756 0.0756 ...
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_clean                          : num  108 90 12 18 42 66 102 138 12 0
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_clean                          : num  342234 342234 342234 342234 342234 ...
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_clean                          : num  189765 333 41094 3393 33165 ...
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -26 -18 -30 26 -0.676 ...
    ##  $ Var126_isBAD                          : int  0 0 0 0 1 1 1 0 0 0
    ##  $ Var127_clean                          : num  18.1 18.1 18.1 18.1 18.1 ...
    ##  $ Var127_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var128_clean                          : num  69.6 69.6 69.6 69.6 69.6 ...
    ##  $ Var128_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var129_clean                          : num  7.01 7.01 7.01 7.01 7.01 ...
    ##  $ Var129_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var13_clean                           : num  2968 632 52 1920 7072 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_clean                          : num  0.58 0.58 0.58 0.58 0.58 ...
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var131_clean                          : num  1099756 1099756 1099756 1099756 1099756 ...
    ##  $ Var131_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_clean                          : num  0 0 0 0 0 0 8 0 0 40
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_clean                          : num  2417555 2881310 1762675 31425 4002710 ...
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_clean                          : num  286546 512588 0 67728 1005576 ...
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_clean                          : num  212 212 212 212 212 ...
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var136_clean                          : num  95263 95263 95263 95263 95263 ...
    ##  $ Var136_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var137_clean                          : num  3.32 3.32 3.32 3.32 3.32 ...
    ##  $ Var137_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_clean                          : num  205332 205332 205332 205332 205332 ...
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_clean                           : num  0.941 0.941 0.941 0.941 0.941 ...
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  11845 205 990 130 16390 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var142_clean                          : num  0.901 0.901 0.901 0.901 0.901 ...
    ##  $ Var142_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var143_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  36 18 9 9 36 0 9 18 9 18
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var145_clean                          : num  43.5 43.5 43.5 43.5 43.5 ...
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var146_clean                          : num  3.72 3.72 3.72 3.72 3.72 ...
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_clean                          : num  1.57 1.57 1.57 1.57 1.57 ...
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_clean                          : num  135 135 135 135 135 ...
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var149_clean                          : num  985208 213220 0 55636 554414 ...
    ##  $ Var149_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var150_clean                          : num  147957 147957 147957 147957 147957 ...
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var151_clean                          : num  9.79 9.79 9.79 9.79 9.79 ...
    ##  $ Var151_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_clean                          : num  7.81 7.81 7.81 7.81 7.81 ...
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_clean                          : num  10522600 7305080 6920680 284796 9855080 ...
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var154_clean                          : num  1644718 1644718 1644718 1644718 1644718 ...
    ##  $ Var154_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var155_clean                          : num  0.926 0.926 0.926 0.926 0.926 ...
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var156_clean                          : num  126 126 126 126 126 ...
    ##  $ Var156_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var157_clean                          : num  28.7 28.7 28.7 28.7 28.7 ...
    ##  $ Var157_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var158_clean                          : num  1.72 1.72 1.72 1.72 1.72 ...
    ##  $ Var158_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_clean                          : num  3.93 3.93 3.93 3.93 3.93 ...
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_clean                           : num  124 124 124 124 124 ...
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_clean                          : num  16 76 2 6 24 30 46 70 52 54
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_clean                          : num  2.83 2.83 2.83 2.83 2.83 ...
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var162_clean                          : num  314216 314216 314216 314216 314216 ...
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_clean                          : num  326064 767406 1008234 97170 1425636 ...
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_clean                          : num  1.67 1.67 1.67 1.67 1.67 ...
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var165_clean                          : num  28577 28577 28577 28577 28577 ...
    ##  $ Var165_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var166_clean                          : num  25.8 25.8 25.8 25.8 25.8 ...
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var168_clean                          : num  308 308 308 308 308 ...
    ##  $ Var168_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_clean                           : num  8.33 8.33 8.33 8.33 8.33 ...
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_clean                          : num  2.57 2.57 2.57 2.57 2.57 ...
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var171_clean                          : num  501912 501912 501912 501912 501912 ...
    ##  $ Var171_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_clean                          : num  10.4 10.4 10.4 10.4 10.4 ...
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_clean                          : num  5.53 5.53 5.53 5.53 5.53 ...
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var176_clean                          : num  3.87 3.87 3.87 3.87 3.87 ...
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_clean                          : num  776288 776288 776288 776288 776288 ...
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var178_clean                          : num  19.3 19.3 19.3 19.3 19.3 ...
    ##  $ Var178_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_clean                          : num  1.67 1.67 1.67 1.67 1.67 ...
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_clean                           : num  4.04 4.04 4.04 4.04 4.04 ...
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var180_clean                          : num  4010570 4010570 4010570 4010570 4010570 ...
    ##  $ Var180_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_clean                          : num  0 0 0 0 0 0 0 0 0 14
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_clean                          : num  1524710 1524710 1524710 1524710 1524710 ...
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var183_clean                          : num  70536 70536 70536 70536 70536 ...
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_clean                          : num  8.13 8.13 8.13 8.13 8.13 ...
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var186_clean                          : num  2.96 2.96 2.96 2.96 2.96 ...
    ##  $ Var186_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var187_clean                          : num  11.1 11.1 11.1 11.1 11.1 ...
    ##  $ Var187_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_clean                          : num  182 182 182 182 182 ...
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  354 222 228 272 282 ...
    ##  $ Var189_isBAD                          : int  0 0 0 1 0 0 1 1 0 0
    ##  $ Var19_clean                           : num  0.278 0.278 0.278 0.278 0.278 ...
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var190_clean                          : num  20360 20360 20360 20360 20360 ...
    ##  $ Var190_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_r_I                      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var192_catB                           : num  -9.3784 0.1196 -0.0746 1.3724 -0.5735 ...
    ##  $ Var193_catB                           : num  -0.586 -0.586 -0.586 -0.586 -0.586 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var193_lev_x_AERks4l                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var194_catB                           : num  -0.125 0.042 0.042 -0.125 0.042 ...
    ##  $ Var194_lev_x_NA                       : int  0 1 1 0 1 0 1 0 1 0
    ##  $ Var194_lev_x_SEuy                     : int  1 0 0 1 0 1 0 1 0 1
    ##  $ Var195_catB                           : num  0.00948 0.00948 0.00948 0.00948 0.00948 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var196_lev_x_1K8T                     : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var197_catB                           : num  0.3178 0.0451 0.0287 -0.9134 0.2031 ...
    ##  $ Var197_lev_x_0Xwj                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_487l                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_JLbT                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_lK27                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var197_lev_x_ssAy                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_TyGl                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_z32l                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_catB                           : num  2.47 -7.84 -7.43 0 0 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_lev_x_PHNvXy8                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                           : num  -0.541 -0.186 0.574 -0.499 -0.787 ...
    ##  $ Var199_lev_x_r83_sZi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catB                           : num  0 0 0 0 0 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var201_lev_x_NA                       : int  0 1 1 0 1 0 1 0 1 0
    ##  $ Var201_lev_x_smXZ                     : int  1 0 0 1 0 1 0 1 0 1
    ##  $ Var202_catB                           : num  -7.4326 -7.4326 -6.7395 -0.0139 -6.7395 ...
    ##  $ Var203_catB                           : num  0.3461 -0.0384 -0.0384 -0.0384 -0.0384 ...
    ##  $ Var203_lev_x_9_Y1                     : int  0 1 1 1 1 1 1 1 1 1
    ##  $ Var203_lev_x_F3hy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var203_lev_x_HLqf                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var204_catB                           : num  -0.2591 -0.8432 0.0731 0.3051 0.2738 ...
    ##  $ Var204_lev_x_DtNL                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_k13i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_m_h1                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_rGJy                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var204_lev_x_RVjC                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var204_lev_x_SkZj                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_z5Ry                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_catB                           : num  -0.056 -0.056 -0.115 -0.115 -0.115 ...
    ##  $ Var205_lev_x_09_Q                     : int  0 0 1 1 1 0 0 0 0 0
    ##  $ Var205_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_sJzTlal                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  1 1 0 0 0 1 1 1 1 1
    ##  $ Var206_catB                           : num  -0.1243 -0.7578 0.1038 -0.2408 -0.0614 ...
    ##  $ Var206_lev_x_43pnToF                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_6JmL                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 1 0 0 0 0 1 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_IYzP                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var206_lev_x_kxE9                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_sYC_                     : int  0 0 0 0 0 0 1 1 0 0
    ##  $ Var206_lev_x_wMei                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  0.0821 0.0821 0.0821 -0.4617 -0.4119 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 0 1 0 0 1 0 0 0
    ##  $ Var207_lev_x_Kxdu                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  1 1 1 0 0 0 0 1 1 1
    ##  $ Var207_lev_x_NKv3VA1BpP               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var208_catB                           : num  0.3516 -0.0361 -0.0361 -0.0361 -0.0361 ...
    ##  $ Var208_lev_x_kIsH                     : int  0 1 1 1 1 1 1 1 1 1
    ##  $ Var208_lev_x_sBgB                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var21_clean                           : num  152 472 20 108 144 156 256 396 324 176
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0343 -0.0343 -0.0343 -0.0343 -0.0343 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 0 1 1 1 1 1 1 1 0
    ##  $ Var211_lev_x_Mtgm                     : int  0 1 0 0 0 0 0 0 0 1
    ##  $ Var212_catB                           : num  0.13 -0.278 -0.132 -0.661 -8.126 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 0 1 0 1 1 0 0 0
    ##  $ Var212_lev_x_FMSzZ91zL2               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_Ie_5MZs                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  1 0 0 0 0 0 0 1 0 0
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 1 0 0 0 0 0 0 1 1
    ##  $ Var213_lev_x_KdSa                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var213_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var214_catB                           : num  0 0 0 0 0 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var215_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var216_catB                           : num  -10.072 -0.563 0.144 -8.685 0 ...
    ##  $ Var216_lev_x_11p4mKe                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_beK4AFX                  : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var216_lev_x_kZJtVhC                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_kZJyVg2                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAja5EA                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAjbk_S                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_NGZxnJM                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 1 0 0 0 0 0 0 1 1
    ##  $ Var217_catB                           : num  -7.4326 0 -6.7395 -0.0139 0 ...
    ##  $ Var218_catB                           : num  0.13 -0.205 0.13 0.13 -0.205 ...
    ##  $ Var218_lev_x_cJvF                     : int  0 1 0 0 1 0 0 1 1 0
    ##  $ Var218_lev_x_UYBR                     : int  1 0 1 1 0 1 1 0 0 1
    ##  $ Var219_catB                           : num  -0.000844 -0.000844 -0.000844 -0.000844 -0.000844 ...
    ##  $ Var219_lev_x_AU8pNoi                  : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var219_lev_x_FzaX                     : int  1 1 1 1 1 0 0 1 1 1
    ##  $ Var219_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_qxDb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var22_clean                           : num  190 590 25 135 180 195 320 495 405 220
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catB                           : num  2.47 -7.84 -7.43 0 0 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_lev_x_UF16siJ                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  0.055 0.055 0.055 -0.62 -9.378 ...
    ##  $ Var221_lev_x_Al6ZaUT                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 0 1 0 1 1 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  1 1 1 0 0 0 0 1 1 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_catB                           : num  2.47 -7.84 -7.43 0 0 ...
    ##  $ Var222_lev_x_APgdzOv                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_catB                           : num  -0.0233 -0.0233 -0.0233 -0.0233 -0.0233 ...
    ##  $ Var223_lev_x_jySVZNlOJy               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_lev_x_LM8l689qOp               : int  1 1 1 1 1 0 0 1 1 1
    ##  $ Var223_lev_x_M_8D                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var223_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var224_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var225_catB                           : num  -0.0738 -0.2399 -0.2399 -0.0738 -0.2399 ...
    ##  $ Var225_lev_x_ELof                     : int  0 1 1 0 1 0 0 0 1 0
    ##  $ Var225_lev_x_kG3k                     : int  1 0 0 1 0 0 0 0 0 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var225_lev_x_xG3x                     : int  0 0 0 0 0 1 0 1 0 0
    ##  $ Var226_catB                           : num  0.2305 -0.1601 -0.2304 0.2305 0.0335 ...
    ##  $ Var226_lev_x_3Cy4                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_453m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_5Acm                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var226_lev_x_7aLG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var226_lev_x_Aoh3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_fKCe                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  1 0 0 1 0 0 0 0 0 0
    ##  $ Var226_lev_x_kwS7                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_me1d                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_PM2D                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qcbd                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qu4f                     : int  0 0 0 0 0 1 0 0 0 0
    ##  $ Var226_lev_x_rgKb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_TNEC                     : int  0 0 0 0 0 0 1 0 0 0
    ##  $ Var226_lev_x_uWr3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_WqMG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Xa3G                     : int  0 0 1 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_xb3V                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  0.0836 0.0836 0.0836 -0.094 0.1684 ...
    ##  $ Var227_lev_x_02N6s8f                  : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var227_lev_x_6fzt                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  1 1 1 0 0 0 0 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  -0.562 -0.562 -0.562 -0.562 -0.562 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_xwM2aC7IdeMC0            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  -8.126 -8.126 -8.126 -0.206 -0.206 ...
    ##  $ Var229_lev_x_am7c                     : int  0 0 0 1 1 1 1 1 1 1
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_clean                           : num  7.77 7.77 7.77 7.77 7.77 ...
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var24_clean                           : num  4 6 0 2 2 2 4 4 2 6
    ##  $ Var24_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var25_clean                           : num  32 288 0 120 56 32 72 120 256 56
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_clean                           : num  0.151 0.151 0.151 0.151 0.151 ...
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_clean                           : num  0.0629 0.0629 0.0629 0.0629 0.0629 ...
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_clean                           : num  315.3 200 186.6 99.7 351.5 ...
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var29_clean                           : num  0.0563 0.0563 0.0563 0.0563 0.0563 ...
    ##  $ Var29_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var3_clean                            : num  530 530 530 530 530 ...
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var30_clean                           : num  7.54 7.54 7.54 7.54 7.54 ...
    ##  $ Var30_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var33_clean                           : num  133254 133254 133254 133254 133254 ...
    ##  $ Var33_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_clean                           : num  1.01 1.01 1.01 1.01 1.01 ...
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_clean                           : num  155026 155026 155026 155026 155026 ...
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var37_clean                           : num  730613 730613 730613 730613 730613 ...
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_clean                           : num  8068320 2726190 7257600 62232 3326490 ...
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_clean                            : num  0.167 0.167 0.167 0.167 0.167 ...
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var40_clean                           : num  19.5 19.5 19.5 19.5 19.5 ...
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var41_clean                           : num  23.8 23.8 23.8 23.8 23.8 ...
    ##  $ Var41_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_clean                           : num  4.5 4.5 4.5 4.5 4.5 ...
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var45_clean                           : num  6094 6094 6094 6094 6094 ...
    ##  $ Var45_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var46_clean                           : num  15.6 15.6 15.6 15.6 15.6 ...
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var47_clean                           : num  1.13 1.13 1.13 1.13 1.13 ...
    ##  $ Var47_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_clean                           : num  0.151 0.151 0.151 0.151 0.151 ...
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var5_clean                            : num  306891 306891 306891 306891 306891 ...
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var50_clean                           : num  22.2 22.2 22.2 22.2 22.2 ...
    ##  $ Var50_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var51_clean                           : num  40261 40261 40261 9380 40261 ...
    ##  $ Var51_isBAD                           : int  1 1 1 0 1 1 1 1 1 0
    ##  $ Var53_clean                           : num  628687 628687 628687 628687 628687 ...
    ##  $ Var53_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_clean                           : num  4.57 4.57 4.57 4.57 4.57 ...
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var56_clean                           : num  35699 35699 35699 35699 35699 ...
    ##  $ Var56_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var57_clean                           : num  6.71 5.52 1.8 3.51 3.16 ...
    ##  $ Var58_clean                           : num  104195 104195 104195 104195 104195 ...
    ##  $ Var58_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var59_clean                           : num  321120 321120 321120 321120 321120 ...
    ##  $ Var59_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_clean                            : num  1589 2240 98 658 952 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var60_clean                           : num  7.72 7.72 7.72 7.72 7.72 ...
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var61_clean                           : num  31.9 31.9 31.9 31.9 31.9 ...
    ##  $ Var61_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var62_clean                           : num  4.75 4.75 4.75 4.75 4.75 ...
    ##  $ Var62_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var63_clean                           : num  32.1 32.1 32.1 32.1 32.1 ...
    ##  $ Var63_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var64_clean                           : num  31645 31645 31645 31645 31645 ...
    ##  $ Var64_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 9 9 9 18 9 18 36 18 27
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var66_clean                           : num  72.1 72.1 72.1 72.1 72.1 ...
    ##  $ Var66_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var67_clean                           : num  0.0314 0.0314 0.0314 0.0314 0.0314 ...
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var68_clean                           : num  85.2 85.2 85.2 85.2 85.2 ...
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_clean                           : num  4110196 4110196 4110196 4110196 4110196 ...
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  7 7 7 7 14 7 7 21 7 21
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_clean                           : num  503994 503994 503994 503994 503994 ...
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var71_clean                           : num  130 130 130 130 130 ...
    ##  $ Var71_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  3 3 3 4.22 6 ...
    ##  $ Var72_isBAD                           : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var73_clean                           : num  126 118 104 104 132 106 118 128 152 124
    ##  $ Var74_clean                           : num  490 7 63 350 154 231 35 28 28 14
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_clean                           : num  5.92 5.92 5.92 5.92 5.92 ...
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_clean                           : num  1382400 1911368 0 35864 1227792 ...
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var77_clean                           : num  8.11 8.11 8.11 8.11 8.11 ...
    ##  $ Var77_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var78_clean                           : num  0 0 0 0 0 0 3 0 0 9
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_clean                           : num  61834 61834 61834 61834 61834 ...
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  115048 36041 288493 6480 138539 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var82_clean                           : num  2.26 2.26 2.26 2.26 2.26 ...
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_clean                           : num  25 30 0 0 15 20 15 45 10 0
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_clean                           : num  55.2 55.2 55.2 55.2 55.2 ...
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_clean                           : num  2 24 4 10 6 2 12 12 8 2
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var86_clean                           : num  325174 325174 325174 325174 325174 ...
    ##  $ Var86_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var87_clean                           : num  5.52 5.52 5.52 5.52 5.52 ...
    ##  $ Var87_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var88_clean                           : num  49.7 49.7 49.7 49.7 49.7 ...
    ##  $ Var88_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var89_clean                           : num  6.53 6.53 6.53 6.53 6.53 ...
    ##  $ Var89_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var9_clean                            : num  35.3 35.3 35.3 35.3 35.3 ...
    ##  $ Var9_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var90_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var91_clean                           : num  86.8 86.8 86.8 86.8 86.8 ...
    ##  $ Var91_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var92_clean                           : num  226258 226258 226258 226258 226258 ...
    ##  $ Var92_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var93_clean                           : num  2.21 2.21 2.21 2.21 2.21 ...
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var94_clean                           : num  242262 4083 12372 94029 364086 ...
    ##  $ Var94_isBAD                           : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var95_clean                           : num  127710 127710 127710 127710 127710 ...
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_clean                           : num  4.12 4.12 4.12 4.12 4.12 ...
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_clean                           : num  0.868 0.868 0.868 0.868 0.868 ...
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var98_clean                           : num  16715 16715 16715 16715 16715 ...
    ##  $ Var98_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_clean                           : num  21.1 21.1 21.1 21.1 21.1 ...
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
