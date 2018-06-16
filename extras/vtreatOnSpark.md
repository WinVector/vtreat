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

    ## [1] "vtreat 1.2.0 inspecting inputs Sat Jun 16 10:23:10 2018"
    ## [1] "designing treatments Sat Jun 16 10:23:10 2018"
    ## [1] " have initial level statistics Sat Jun 16 10:23:10 2018"
    ## [1] " scoring treatments Sat Jun 16 10:23:12 2018"
    ## [1] "have treatment plan Sat Jun 16 10:23:18 2018"
    ## [1] "rescoring complex variables Sat Jun 16 10:23:18 2018"
    ## [1] "done rescoring complex variables Sat Jun 16 10:23:22 2018"

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
    ## [187] "Var19_isBAD"                           
    ## [188] "Var190_clean"                          
    ## [189] "Var190_isBAD"                          
    ## [190] "Var191_lev_x_NA"                       
    ## [191] "Var192_catB"                           
    ## [192] "Var193_catB"                           
    ## [193] "Var193_lev_x_2Knk1KF"                  
    ## [194] "Var193_lev_x_AERks4l"                  
    ## [195] "Var193_lev_x_RO12"                     
    ## [196] "Var194_catB"                           
    ## [197] "Var194_lev_x_NA"                       
    ## [198] "Var194_lev_x_SEuy"                     
    ## [199] "Var195_catB"                           
    ## [200] "Var195_lev_x_taul"                     
    ## [201] "Var196_lev_x_1K8T"                     
    ## [202] "Var197_catB"                           
    ## [203] "Var197_lev_x_0Xwj"                     
    ## [204] "Var197_lev_x_487l"                     
    ## [205] "Var197_lev_x_JLbT"                     
    ## [206] "Var197_lev_x_lK27"                     
    ## [207] "Var197_lev_x_ssAy"                     
    ## [208] "Var197_lev_x_TyGl"                     
    ## [209] "Var198_catB"                           
    ## [210] "Var198_lev_x_fhk21Ss"                  
    ## [211] "Var198_lev_x_PHNvXy8"                  
    ## [212] "Var199_catB"                           
    ## [213] "Var199_lev_x_jTP8ioIlJ"                
    ## [214] "Var2_isBAD"                            
    ## [215] "Var200_catB"                           
    ## [216] "Var200_lev_x_NA"                       
    ## [217] "Var201_lev_x_NA"                       
    ## [218] "Var201_lev_x_smXZ"                     
    ## [219] "Var202_catB"                           
    ## [220] "Var203_catB"                           
    ## [221] "Var203_lev_x_9_Y1"                     
    ## [222] "Var203_lev_x_F3hy"                     
    ## [223] "Var203_lev_x_HLqf"                     
    ## [224] "Var204_catB"                           
    ## [225] "Var204_lev_x_15m3"                     
    ## [226] "Var204_lev_x_7WNq"                     
    ## [227] "Var204_lev_x_e7QV"                     
    ## [228] "Var204_lev_x_k13i"                     
    ## [229] "Var204_lev_x_m_h1"                     
    ## [230] "Var204_lev_x_Px52"                     
    ## [231] "Var204_lev_x_RVjC"                     
    ## [232] "Var204_lev_x_Y9Bl"                     
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
    ## [356] "Var228_lev_x_Zy3gnGM"                  
    ## [357] "Var229_catB"                           
    ## [358] "Var229_lev_x_am7c"                     
    ## [359] "Var229_lev_x_mj86"                     
    ## [360] "Var229_lev_x_NA"                       
    ## [361] "Var23_clean"                           
    ## [362] "Var23_isBAD"                           
    ## [363] "Var24_clean"                           
    ## [364] "Var24_isBAD"                           
    ## [365] "Var25_clean"                           
    ## [366] "Var25_isBAD"                           
    ## [367] "Var26_clean"                           
    ## [368] "Var26_isBAD"                           
    ## [369] "Var27_clean"                           
    ## [370] "Var27_isBAD"                           
    ## [371] "Var28_clean"                           
    ## [372] "Var28_isBAD"                           
    ## [373] "Var29_clean"                           
    ## [374] "Var29_isBAD"                           
    ## [375] "Var3_clean"                            
    ## [376] "Var3_isBAD"                            
    ## [377] "Var30_clean"                           
    ## [378] "Var30_isBAD"                           
    ## [379] "Var33_clean"                           
    ## [380] "Var33_isBAD"                           
    ## [381] "Var34_clean"                           
    ## [382] "Var34_isBAD"                           
    ## [383] "Var35_clean"                           
    ## [384] "Var35_isBAD"                           
    ## [385] "Var36_clean"                           
    ## [386] "Var36_isBAD"                           
    ## [387] "Var37_clean"                           
    ## [388] "Var37_isBAD"                           
    ## [389] "Var38_clean"                           
    ## [390] "Var38_isBAD"                           
    ## [391] "Var4_clean"                            
    ## [392] "Var4_isBAD"                            
    ## [393] "Var40_clean"                           
    ## [394] "Var40_isBAD"                           
    ## [395] "Var41_clean"                           
    ## [396] "Var41_isBAD"                           
    ## [397] "Var43_clean"                           
    ## [398] "Var43_isBAD"                           
    ## [399] "Var44_clean"                           
    ## [400] "Var44_isBAD"                           
    ## [401] "Var45_clean"                           
    ## [402] "Var45_isBAD"                           
    ## [403] "Var46_clean"                           
    ## [404] "Var46_isBAD"                           
    ## [405] "Var47_clean"                           
    ## [406] "Var47_isBAD"                           
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
    ## [488] "Var90_clean"                           
    ## [489] "Var90_isBAD"                           
    ## [490] "Var91_clean"                           
    ## [491] "Var91_isBAD"                           
    ## [492] "Var92_clean"                           
    ## [493] "Var92_isBAD"                           
    ## [494] "Var93_clean"                           
    ## [495] "Var93_isBAD"                           
    ## [496] "Var94_clean"                           
    ## [497] "Var94_isBAD"                           
    ## [498] "Var95_clean"                           
    ## [499] "Var95_isBAD"                           
    ## [500] "Var96_clean"                           
    ## [501] "Var96_isBAD"                           
    ## [502] "Var97_clean"                           
    ## [503] "Var97_isBAD"                           
    ## [504] "Var98_clean"                           
    ## [505] "Var98_isBAD"                           
    ## [506] "Var99_clean"                           
    ## [507] "Var99_isBAD"

``` r
cdata::qlook(db, d_train$table_name)
```

    ## table `kddvtreat_20713012849288074061_0000000002` spark_connection spark_shell_connection DBIConnection 
    ##  nrow: 4961 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  507 variables:
    ##  $ churn                                 : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    ##  $ Var1_clean                            : num  8.26 8.26 8.26 8.26 8.26 ...
    ##  $ Var1_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var10_clean                           : num  362357 362357 362357 362357 362357 ...
    ##  $ Var10_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var100_clean                          : num  0.677 0.677 0.677 0.677 0.677 ...
    ##  $ Var100_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var101_clean                          : num  16.3 16.3 16.3 16.3 16.3 ...
    ##  $ Var101_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var102_clean                          : num  29349 29349 29349 29349 29349 ...
    ##  $ Var102_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var103_clean                          : num  16.9 16.9 16.9 16.9 16.9 ...
    ##  $ Var103_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var104_clean                          : num  96.9 96.9 96.9 96.9 96.9 ...
    ##  $ Var104_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var105_clean                          : num  64.6 64.6 64.6 64.6 64.6 ...
    ##  $ Var105_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var106_clean                          : num  39118 39118 39118 39118 39118 ...
    ##  $ Var106_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var107_clean                          : num  2.85 2.85 2.85 2.85 2.85 ...
    ##  $ Var107_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var108_clean                          : num  369476 369476 369476 369476 369476 ...
    ##  $ Var108_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var109_clean                          : num  64.5 32 32 40 32 ...
    ##  $ Var109_isBAD                          : int  1 0 0 0 0 0 0 0 1 0
    ##  $ Var11_clean                           : num  8.73 8.73 8.73 8.73 8.73 ...
    ##  $ Var11_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var110_clean                          : num  6.48 6.48 6.48 6.48 6.48 ...
    ##  $ Var110_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var111_clean                          : num  287416 287416 287416 287416 287416 ...
    ##  $ Var111_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var112_clean                          : num  0 48 48 64 24 256 8 0 0 280
    ##  $ Var112_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var113_clean                          : num  -4058 64894 -808516 172182 -139304 ...
    ##  $ Var114_clean                          : num  679861 679861 679861 679861 679861 ...
    ##  $ Var114_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var115_clean                          : num  33.2 33.2 33.2 33.2 33.2 ...
    ##  $ Var115_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var116_clean                          : num  0.0968 0.0968 0.0968 0.0968 0.0968 ...
    ##  $ Var116_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var117_clean                          : num  1e+05 1e+05 1e+05 1e+05 1e+05 ...
    ##  $ Var117_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var118_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var119_clean                          : num  50 495 455 675 450 ...
    ##  $ Var119_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var12_clean                           : num  11.3 11.3 11.3 11.3 11.3 ...
    ##  $ Var12_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var120_clean                          : num  22.8 22.8 22.8 22.8 22.8 ...
    ##  $ Var120_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var121_clean                          : num  4.94 4.94 4.94 4.94 4.94 ...
    ##  $ Var121_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var122_clean                          : num  0.0606 0.0606 0.0606 0.0606 0.0606 ...
    ##  $ Var122_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var123_clean                          : num  0 42 6 42 66 0 36 0 0 138
    ##  $ Var123_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var124_clean                          : num  117520 117520 117520 117520 117520 ...
    ##  $ Var124_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var125_clean                          : num  21717 5121 19863 45144 2691 ...
    ##  $ Var125_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var126_clean                          : num  -0.415 -24 -0.415 -0.415 54 ...
    ##  $ Var126_isBAD                          : int  1 0 1 1 0 0 0 0 1 1
    ##  $ Var127_clean                          : num  22.1 22.1 22.1 22.1 22.1 ...
    ##  $ Var127_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var128_clean                          : num  85.7 85.7 85.7 85.7 85.7 ...
    ##  $ Var128_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var129_clean                          : num  7.68 7.68 7.68 7.68 7.68 ...
    ##  $ Var129_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var13_clean                           : num  24 352 2732 508 516 ...
    ##  $ Var13_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var130_clean                          : num  0.485 0.485 0.485 0.485 0.485 ...
    ##  $ Var130_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var131_clean                          : num  70567 70567 70567 70567 70567 ...
    ##  $ Var131_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var132_clean                          : num  0 8 0 0 0 0 40 0 0 0
    ##  $ Var132_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var133_clean                          : num  0 5273150 2657530 8123050 2224990 ...
    ##  $ Var133_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var134_clean                          : num  0 172800 554494 0 10614 ...
    ##  $ Var134_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var135_clean                          : num  176 176 176 176 176 ...
    ##  $ Var135_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var136_clean                          : num  127487 127487 127487 127487 127487 ...
    ##  $ Var136_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var137_clean                          : num  2.06 2.06 2.06 2.06 2.06 ...
    ##  $ Var137_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var138_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var139_clean                          : num  207546 207546 207546 207546 207546 ...
    ##  $ Var139_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var14_clean                           : num  0.566 0.566 0.566 0.566 0.566 ...
    ##  $ Var14_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var140_clean                          : num  0 35 2210 725 405 ...
    ##  $ Var140_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var142_clean                          : num  0.839 0.839 0.839 0.839 0.839 ...
    ##  $ Var142_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var143_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var143_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var144_clean                          : num  0 27 0 9 18 9 9 18 9 9
    ##  $ Var144_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var145_clean                          : num  47 47 47 47 47 ...
    ##  $ Var145_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var146_clean                          : num  3.8 3.8 3.8 3.8 3.8 ...
    ##  $ Var146_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var147_clean                          : num  1.76 1.76 1.76 1.76 1.76 ...
    ##  $ Var147_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var148_clean                          : num  99.1 99.1 99.1 99.1 99.1 ...
    ##  $ Var148_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var149_clean                          : num  290615 1209600 1256129 604800 0 ...
    ##  $ Var149_isBAD                          : int  1 0 0 0 0 0 0 0 1 0
    ##  $ Var150_clean                          : num  157352 157352 157352 157352 157352 ...
    ##  $ Var150_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var151_clean                          : int  6 6 6 6 6 6 6 6 6 6
    ##  $ Var151_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var152_clean                          : num  7.71 7.71 7.71 7.71 7.71 ...
    ##  $ Var152_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var153_clean                          : num  0 10093760 4673560 10486680 7826320 ...
    ##  $ Var153_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var154_clean                          : num  1398533 1398533 1398533 1398533 1398533 ...
    ##  $ Var154_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var155_clean                          : num  0.854 0.854 0.854 0.854 0.854 ...
    ##  $ Var155_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var156_clean                          : num  122 122 122 122 122 ...
    ##  $ Var156_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var157_clean                          : num  20.5 20.5 20.5 20.5 20.5 ...
    ##  $ Var157_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var158_clean                          : num  1.95 1.95 1.95 1.95 1.95 ...
    ##  $ Var158_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var159_clean                          : int  4 4 4 4 4 4 4 4 4 4
    ##  $ Var159_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var16_clean                           : num  121 121 121 121 121 ...
    ##  $ Var16_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var160_clean                          : num  0 28 36 38 12 38 50 0 0 54
    ##  $ Var160_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var161_clean                          : num  2.22 2.22 2.22 2.22 2.22 ...
    ##  $ Var161_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var162_clean                          : num  446689 446689 446689 446689 446689 ...
    ##  $ Var162_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var163_clean                          : num  0 0 390450 0 77730 ...
    ##  $ Var163_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var164_clean                          : num  1.63 1.63 1.63 1.63 1.63 ...
    ##  $ Var164_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var165_clean                          : num  12484 12484 12484 12484 12484 ...
    ##  $ Var165_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var166_clean                          : num  15.7 15.7 15.7 15.7 15.7 ...
    ##  $ Var166_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var168_clean                          : num  305 305 305 305 305 ...
    ##  $ Var168_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var17_clean                           : num  10.9 10.9 10.9 10.9 10.9 ...
    ##  $ Var17_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var170_clean                          : num  2.15 2.15 2.15 2.15 2.15 ...
    ##  $ Var170_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var171_clean                          : num  323401 323401 323401 323401 323401 ...
    ##  $ Var171_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var172_clean                          : num  10.8 10.8 10.8 10.8 10.8 ...
    ##  $ Var172_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var173_clean                          : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var173_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var174_clean                          : num  5.11 5.11 5.11 5.11 5.11 ...
    ##  $ Var174_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var176_clean                          : num  5.09 5.09 5.09 5.09 5.09 ...
    ##  $ Var176_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var177_clean                          : num  648623 648623 648623 648623 648623 ...
    ##  $ Var177_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var178_clean                          : num  18 18 18 18 18 ...
    ##  $ Var178_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var179_clean                          : num  1.99 1.99 1.99 1.99 1.99 ...
    ##  $ Var179_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var18_clean                           : num  4.67 4.67 4.67 4.67 4.67 ...
    ##  $ Var18_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var180_clean                          : num  3522070 3522070 3522070 3522070 3522070 ...
    ##  $ Var180_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var181_clean                          : num  0 0 0 0 0 0 14 0 0 0
    ##  $ Var181_isBAD                          : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var182_clean                          : num  1094094 1094094 1094094 1094094 1094094 ...
    ##  $ Var182_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var183_clean                          : num  90180 90180 90180 90180 90180 ...
    ##  $ Var183_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var184_clean                          : num  7.72 7.72 7.72 7.72 7.72 ...
    ##  $ Var184_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var186_clean                          : num  2.9 2.9 2.9 2.9 2.9 ...
    ##  $ Var186_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var187_clean                          : num  10.7 10.7 10.7 10.7 10.7 ...
    ##  $ Var187_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var188_clean                          : num  166 166 166 166 166 ...
    ##  $ Var188_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var189_clean                          : num  156 267 390 270 240 ...
    ##  $ Var189_isBAD                          : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var19_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var190_clean                          : num  17495 17495 17495 17495 17495 ...
    ##  $ Var190_isBAD                          : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var191_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var192_catB                           : num  -0.853 -0.128 -0.332 0.415 -10.287 ...
    ##  $ Var193_catB                           : num  0.183 -0.639 -0.639 -0.639 -0.639 ...
    ##  $ Var193_lev_x_2Knk1KF                  : int  0 1 1 1 1 1 1 1 1 1
    ##  $ Var193_lev_x_AERks4l                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var193_lev_x_RO12                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var194_catB                           : num  0.0633 -0.2015 -0.2015 -0.2015 -0.2015 ...
    ##  $ Var194_lev_x_NA                       : int  1 0 0 0 0 0 1 1 0 0
    ##  $ Var194_lev_x_SEuy                     : int  0 1 1 1 1 1 0 0 1 1
    ##  $ Var195_catB                           : num  0.03 0.03 0.03 0.03 0 ...
    ##  $ Var195_lev_x_taul                     : int  1 1 1 1 0 1 0 1 1 1
    ##  $ Var196_lev_x_1K8T                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var197_catB                           : num  -0.2207 -0.0605 -0.0103 -0.152 -1.1252 ...
    ##  $ Var197_lev_x_0Xwj                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_487l                     : int  0 0 1 0 0 0 0 0 0 1
    ##  $ Var197_lev_x_JLbT                     : int  0 0 0 0 0 0 0 0 1 0
    ##  $ Var197_lev_x_lK27                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_ssAy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var197_lev_x_TyGl                     : int  0 0 0 1 0 0 1 0 0 0
    ##  $ Var198_catB                           : num  0.128 0 0 0 -6.598 ...
    ##  $ Var198_lev_x_fhk21Ss                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var198_lev_x_PHNvXy8                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var199_catB                           : num  -6.598 -0.789 -8.39 0 -9.306 ...
    ##  $ Var199_lev_x_jTP8ioIlJ                : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var2_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var200_catB                           : num  0 0 0 0 0 ...
    ##  $ Var200_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var201_lev_x_NA                       : int  1 0 0 0 0 0 1 1 0 0
    ##  $ Var201_lev_x_smXZ                     : int  0 1 1 1 1 1 0 0 1 1
    ##  $ Var202_catB                           : num  -6.6 -8.68 0 0 -7.29 ...
    ##  $ Var203_catB                           : num  -0.0515 -0.0515 -0.0515 -0.0515 -0.0515 ...
    ##  $ Var203_lev_x_9_Y1                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var203_lev_x_F3hy                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var203_lev_x_HLqf                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_catB                           : num  -0.646 -0.152 -0.606 -0.128 -0.884 ...
    ##  $ Var204_lev_x_15m3                     : int  0 0 0 1 0 0 0 0 0 0
    ##  $ Var204_lev_x_7WNq                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_e7QV                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_k13i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_m_h1                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_Px52                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_RVjC                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_Y9Bl                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var204_lev_x_z5Ry                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_catB                           : num  0.6147 -0.1124 -0.1124 -0.0748 -0.1124 ...
    ##  $ Var205_lev_x_09_Q                     : int  0 0 0 1 0 1 0 1 0 0
    ##  $ Var205_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_sJzTlal                  : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var205_lev_x_VpdQ                     : int  0 1 1 0 1 0 1 0 1 1
    ##  $ Var206_catB                           : num  0.212 -0.361 -0.361 -0.552 -0.665 ...
    ##  $ Var206_lev_x_43pnToF                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_6JmL                     : int  0 0 0 0 1 0 0 0 0 0
    ##  $ Var206_lev_x_hAFG                     : int  0 0 0 0 0 1 1 0 0 0
    ##  $ Var206_lev_x_haYg                     : int  0 0 0 0 0 0 0 1 1 0
    ##  $ Var206_lev_x_IYzP                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_kxE9                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_sYC_                     : int  0 1 1 0 0 0 0 0 0 1
    ##  $ Var206_lev_x_wMei                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_y6dw                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var206_lev_x_zm5i                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_catB                           : num  -0.205 -0.205 -0.238 -0.238 0.162 ...
    ##  $ Var207_lev_x_7M47J5GA0pTYIFxg5uy      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_DHn_WUyBhW_whjA88g9bvA64_: int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var207_lev_x_Kxdu                     : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var207_lev_x_me75fM6ugJ               : int  0 0 0 0 1 1 1 1 1 1
    ##  $ Var207_lev_x_NKv3VA1BpP               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var208_catB                           : num  -0.041 -0.041 -0.041 -0.041 -0.041 ...
    ##  $ Var208_lev_x_kIsH                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var208_lev_x_sBgB                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var21_clean                           : num  0 124 148 156 116 472 196 120 0 604
    ##  $ Var21_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_catB                           : num  -0.0461 -0.0461 -0.0461 -0.0461 -0.0461 ...
    ##  $ Var210_lev_x_g5HH                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var210_lev_x_uKAI                     : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var211_lev_x_L84s                     : int  1 0 1 1 1 1 1 1 1 1
    ##  $ Var211_lev_x_Mtgm                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var212_catB                           : num  0.128 -7.291 -0.113 -0.113 -0.456 ...
    ##  $ Var212_lev_x_CrNX                     : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var212_lev_x_FMSzZ91zL2               : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_Ie_5MZs                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var212_lev_x_NhsEn4L                  : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var212_lev_x_XfqtO3UdzaXh_            : int  0 0 0 0 1 1 1 1 1 0
    ##  $ Var213_lev_x_KdSa                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var213_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var214_catB                           : num  0 0 0 0 0 ...
    ##  $ Var214_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var215_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var216_catB                           : num  0 -7.291 -0.16 -0.16 -0.485 ...
    ##  $ Var216_lev_x_11p4mKe                  : int  0 0 0 0 1 1 0 0 0 0
    ##  $ Var216_lev_x_beK4AFX                  : int  0 0 0 0 0 0 0 0 0 1
    ##  $ Var216_lev_x_kZJtVhC                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_kZJyVg2                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAja5EA                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_mAjbk_S                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_NGZxnJM                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var216_lev_x_XTbPUYD                  : int  0 0 0 0 0 0 0 1 1 0
    ##  $ Var217_catB                           : num  -6.6 -8.68 0 0 -7.29 ...
    ##  $ Var218_catB                           : num  -0.1669 0.0911 0.0911 0.0911 -0.1669 ...
    ##  $ Var218_lev_x_cJvF                     : int  1 0 0 0 1 1 1 1 0 0
    ##  $ Var218_lev_x_UYBR                     : int  0 1 1 1 0 0 0 0 1 1
    ##  $ Var219_catB                           : num  0.0502 0.0502 -1.0764 0.0502 0.0502 ...
    ##  $ Var219_lev_x_AU8pNoi                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_FzaX                     : int  1 1 0 1 1 1 0 1 1 1
    ##  $ Var219_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var219_lev_x_qxDb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var22_clean                           : num  0 155 185 195 145 590 245 150 0 755
    ##  $ Var22_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_catB                           : num  0.128 0 0 0 -6.598 ...
    ##  $ Var220_lev_x_4UxGlow                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var220_lev_x_UF16siJ                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_catB                           : num  -0.429 -0.429 -0.13 -0.13 0.122 ...
    ##  $ Var221_lev_x_Al6ZaUT                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_d0EEeJi                  : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var221_lev_x_oslk                     : int  0 0 0 0 1 1 1 1 1 1
    ##  $ Var221_lev_x_QKW8DRm                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var221_lev_x_zCkv                     : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var222_catB                           : num  0.128 0 0 0 -6.598 ...
    ##  $ Var222_lev_x_APgdzOv                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var222_lev_x_catzS2D                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_catB                           : num  -0.00234 -0.00234 -0.00234 -0.00234 0.15995 ...
    ##  $ Var223_lev_x_jySVZNlOJy               : int  0 0 0 0 1 0 0 1 1 0
    ##  $ Var223_lev_x_LM8l689qOp               : int  1 1 1 1 0 1 0 0 0 1
    ##  $ Var223_lev_x_M_8D                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var223_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var224_lev_x_NA                       : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var225_catB                           : num  -0.38 -0.126 -0.126 -0.126 -0.582 ...
    ##  $ Var225_lev_x_ELof                     : int  1 0 0 0 0 0 1 0 0 0
    ##  $ Var225_lev_x_kG3k                     : int  0 1 1 1 0 0 0 0 1 1
    ##  $ Var225_lev_x_NA                       : int  0 0 0 0 0 0 0 1 0 0
    ##  $ Var225_lev_x_xG3x                     : int  0 0 0 0 1 1 0 0 0 0
    ##  $ Var226_catB                           : num  0.23875 -0.00861 0.14053 0.14053 -0.63794 ...
    ##  $ Var226_lev_x_3Cy4                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_453m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_5Acm                     : int  1 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7aLG                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_7P5s                     : int  0 0 0 0 1 1 1 1 0 0
    ##  $ Var226_lev_x_Aoh3                     : int  0 0 0 0 0 0 0 0 1 1
    ##  $ Var226_lev_x_fKCe                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_FSa2                     : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var226_lev_x_kwS7                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_me1d                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_PM2D                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qcbd                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Qu4f                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_rgKb                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_szEZ                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_TNEC                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_uWr3                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_WqMG                     : int  0 1 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_Xa3G                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var226_lev_x_xb3V                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_catB                           : num  -0.1255 -0.1255 -0.0731 -0.0731 0.1638 ...
    ##  $ Var227_lev_x_02N6s8f                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_6fzt                     : int  1 1 0 0 0 0 0 0 0 0
    ##  $ Var227_lev_x_nIGXDli                  : int  0 0 1 1 0 0 0 0 0 0
    ##  $ Var227_lev_x_RAYp                     : int  0 0 0 0 1 1 1 1 1 1
    ##  $ Var227_lev_x_ZI9m                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_catB                           : num  0.429 -0.596 -0.596 -0.596 -0.596 ...
    ##  $ Var228_lev_x_55YFVY9                  : int  0 1 1 1 1 1 1 1 1 1
    ##  $ Var228_lev_x_F2FyR07IdsN7I            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_ib5G6X1eUxUn6            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_iyHGyLCEkQ               : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_R4y5gQQWY8OodqDV         : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_TCU50_Yjmm6GIBZ0lL_      : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_xwM2aC7IdeMC0            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var228_lev_x_Zy3gnGM                  : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_catB                           : num  1.003 -0.157 -0.157 -0.157 -0.157 ...
    ##  $ Var229_lev_x_am7c                     : int  0 1 1 1 1 1 1 1 1 1
    ##  $ Var229_lev_x_mj86                     : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var229_lev_x_NA                       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var23_clean                           : num  4.04 4.04 4.04 4.04 4.04 ...
    ##  $ Var23_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var24_clean                           : num  4.71 4 8 2 0 ...
    ##  $ Var24_isBAD                           : int  1 0 0 0 0 0 0 0 1 0
    ##  $ Var25_clean                           : num  0 8 80 0 32 368 96 176 0 288
    ##  $ Var25_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var26_clean                           : num  0.0851 0.0851 0.0851 0.0851 0.0851 ...
    ##  $ Var26_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var27_clean                           : num  0.0426 0.0426 0.0426 0.0426 0.0426 ...
    ##  $ Var27_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var28_clean                           : num  0 187 180 220 287 ...
    ##  $ Var28_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var29_clean                           : num  0.0323 0.0323 0.0323 0.0323 0.0323 ...
    ##  $ Var29_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var3_clean                            : num  228 228 228 228 228 ...
    ##  $ Var3_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var30_clean                           : num  7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5
    ##  $ Var30_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var33_clean                           : num  110007 110007 110007 110007 110007 ...
    ##  $ Var33_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var34_clean                           : num  0.97 0.97 0.97 0.97 0.97 ...
    ##  $ Var34_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var35_clean                           : num  0 5 0 0 0 0 0 0 0 0
    ##  $ Var35_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var36_clean                           : num  151045 151045 151045 151045 151045 ...
    ##  $ Var36_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var37_clean                           : num  442947 442947 442947 442947 442947 ...
    ##  $ Var37_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var38_clean                           : num  0 3628842 9564 1597638 5331096 ...
    ##  $ Var38_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var4_clean                            : num  0.114 0.114 0.114 0.114 0.114 ...
    ##  $ Var4_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var40_clean                           : num  13.1 13.1 13.1 13.1 13.1 ...
    ##  $ Var40_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var41_clean                           : num  23.6 23.6 23.6 23.6 23.6 ...
    ##  $ Var41_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var43_clean                           : num  4.7 4.7 4.7 4.7 4.7 ...
    ##  $ Var43_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var44_clean                           : num  0 0 0 0 0 0 0 0 0 0
    ##  $ Var44_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var45_clean                           : num  7621 7621 7621 7621 7621 ...
    ##  $ Var45_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var46_clean                           : num  17.5 17.5 17.5 17.5 17.5 ...
    ##  $ Var46_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var47_clean                           : num  2.71 2.71 2.71 2.71 2.71 ...
    ##  $ Var47_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var49_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var5_clean                            : num  230948 230948 230948 230948 230948 ...
    ##  $ Var5_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var50_clean                           : num  11.4 11.4 11.4 11.4 11.4 ...
    ##  $ Var50_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var51_clean                           : num  46039 46039 46039 46039 46039 ...
    ##  $ Var51_isBAD                           : int  1 1 1 1 1 1 0 1 1 1
    ##  $ Var53_clean                           : num  579676 579676 579676 579676 579676 ...
    ##  $ Var53_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var54_clean                           : num  5.17 5.17 5.17 5.17 5.17 ...
    ##  $ Var54_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var56_clean                           : num  40819 40819 40819 40819 40819 ...
    ##  $ Var56_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var57_clean                           : num  2.75 3.07 2.71 1.82 5.11 ...
    ##  $ Var58_clean                           : num  222111 222111 222111 222111 222111 ...
    ##  $ Var58_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var59_clean                           : num  449032 449032 449032 449032 449032 ...
    ##  $ Var59_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var6_clean                            : num  0 854 882 2359 784 ...
    ##  $ Var6_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var60_clean                           : num  6.53 6.53 6.53 6.53 6.53 ...
    ##  $ Var60_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var61_clean                           : num  24.4 24.4 24.4 24.4 24.4 ...
    ##  $ Var61_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var62_clean                           : num  3.49 3.49 3.49 3.49 3.49 ...
    ##  $ Var62_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var63_clean                           : num  27.7 27.7 27.7 27.7 27.7 ...
    ##  $ Var63_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var64_clean                           : num  21981 21981 21981 21981 21981 ...
    ##  $ Var64_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var65_clean                           : num  9 18 9 18 45 18 9 9 9 27
    ##  $ Var65_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var66_clean                           : num  69.5 69.5 69.5 69.5 69.5 ...
    ##  $ Var66_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var67_clean                           : num  0.0355 0.0355 0.0355 0.0355 0.0355 ...
    ##  $ Var67_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var68_clean                           : num  83.1 83.1 83.1 83.1 83.1 ...
    ##  $ Var68_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var69_clean                           : num  3651789 3651789 3651789 3651789 3651789 ...
    ##  $ Var69_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var7_clean                            : num  0 7 7 14 35 14 7 7 0 7
    ##  $ Var7_isBAD                            : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var70_clean                           : num  358653 358653 358653 358653 358653 ...
    ##  $ Var70_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var71_clean                           : num  98.6 98.6 98.6 98.6 98.6 ...
    ##  $ Var71_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var72_clean                           : num  4.23 3 4.23 6 4.23 ...
    ##  $ Var72_isBAD                           : int  1 0 1 0 1 0 0 0 1 0
    ##  $ Var73_clean                           : num  82 104 110 106 126 112 110 138 132 132
    ##  $ Var74_clean                           : num  0 0 308 0 7 105 7 0 0 168
    ##  $ Var74_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var75_clean                           : num  6.16 6.16 6.16 6.16 6.16 ...
    ##  $ Var75_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var76_clean                           : num  0 4147200 2343760 5294160 2073600 ...
    ##  $ Var76_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var77_clean                           : num  6.29 6.29 6.29 6.29 6.29 ...
    ##  $ Var77_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var78_clean                           : num  0 0 0 0 0 0 9 0 0 0
    ##  $ Var78_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var80_clean                           : num  60968 60968 60968 60968 60968 ...
    ##  $ Var80_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var81_clean                           : num  536 193596 79899 40030 149444 ...
    ##  $ Var81_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var82_clean                           : num  2.56 2.56 2.56 2.56 2.56 ...
    ##  $ Var82_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var83_clean                           : num  0 15 0 25 20 0 5 0 0 20
    ##  $ Var83_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var84_clean                           : int  52 52 52 52 52 52 52 52 52 52
    ##  $ Var84_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var85_clean                           : num  0 0 4 0 2 42 8 16 0 52
    ##  $ Var85_isBAD                           : int  0 0 0 0 0 0 0 0 0 0
    ##  $ Var86_clean                           : num  176948 176948 176948 176948 176948 ...
    ##  $ Var86_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var87_clean                           : num  5.98 5.98 5.98 5.98 5.98 ...
    ##  $ Var87_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var88_clean                           : num  61.2 61.2 61.2 61.2 61.2 ...
    ##  $ Var88_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var89_clean                           : num  5.39 5.39 5.39 5.39 5.39 ...
    ##  $ Var89_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var9_clean                            : num  34.2 34.2 34.2 34.2 34.2 ...
    ##  $ Var9_isBAD                            : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var90_clean                           : num  0.113 0.113 0.113 0.113 0.113 ...
    ##  $ Var90_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var91_clean                           : num  65.7 65.7 65.7 65.7 65.7 ...
    ##  $ Var91_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var92_clean                           : num  200626 200626 200626 200626 200626 ...
    ##  $ Var92_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var93_clean                           : num  2.16 2.16 2.16 2.16 2.16 ...
    ##  $ Var93_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var94_clean                           : num  97838 41997 97838 182220 97838 ...
    ##  $ Var94_isBAD                           : int  1 0 1 0 1 0 0 0 1 0
    ##  $ Var95_clean                           : num  160987 160987 160987 160987 160987 ...
    ##  $ Var95_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var96_clean                           : num  4.57 4.57 4.57 4.57 4.57 ...
    ##  $ Var96_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var97_clean                           : num  0.809 0.809 0.809 0.809 0.809 ...
    ##  $ Var97_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var98_clean                           : num  43661 43661 43661 43661 43661 ...
    ##  $ Var98_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
    ##  $ Var99_clean                           : num  21.2 21.2 21.2 21.2 21.2 ...
    ##  $ Var99_isBAD                           : int  1 1 1 1 1 1 1 1 1 1
