


```r
# To make the html: echo "library(knitr); knit('KDD2009example.Rmd')" | R --vanilla ; pandoc KDD2009example.md -o KDD2009example.html
# Example of working with KDD2009 data (just to show library at work).
# For vtreat details see: http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/
# and Chapter 6 of Practical Data Science with R: http://www.amazon.com/Practical-Data-Science/dp/1617291560
# For details on data see: https://github.com/WinVector/zmPDSwR/tree/master/KDD2009

# load the data as in the book
dir <- '~/Documents/work/DataScienceBook/zmPDSwR/KDD2009/' # change this path to match your directory structure
d <- read.table(paste(dir,'orange_small_train.data.gz',sep=''),
                header=T,sep='\t',na.strings=c('NA',''))
churn <- read.table(paste(dir,'orange_small_train_churn.labels.txt',sep=''),
                    header=F,sep='\t')
d$churn <- churn$V1
appetency <- read.table(paste(dir,'orange_small_train_appetency.labels.txt',sep=''),
                        header=F,sep='\t')
d$appetency <- appetency$V1
upselling <- read.table(paste(dir,'orange_small_train_upselling.labels.txt',sep=''),
                        header=F,sep='\t')
d$upselling <- upselling$V1
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainPri <- subset(d,rgroup<=0.8)
dTrainCal <- subset(d,rgroup>0.8 & rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
rm(list=c('d','churn','appetency','upselling','dir'))
outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainPri),
                c(outcomes,'rgroup'))
yName <- 'churn'
yTarget <- 1
yCond <- paste(yName,yTarget,sep='==')
```




```r
#load some libraries
library('vtreat')  # This library isn't public yet, intall instructions: http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/

# try the automatic variable treatment
set.seed(239525)
treatments <- designTreatmentsC(dTrainPri,
    vars,yName,yTarget,smFactor=1.0,minFraction=2.0,maxMissing=-1.0)
save(file='kdd2009ex.Rdata',list=ls())
```


```r
#load('kdd2009ex.Rdata') # how we would load the data by hand
#load/re-load some libraries
library('ggplot2')
library('ROCR')
```

```
## Loading required package: gplots
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
library('vtreat')
library('randomForest')
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library('class')



# select variables that look good in calibration
pruneLevel <- 0.99999 # noisy weak variables, so not leaning on PRESS statistic
treatedTrain <- prepare(treatments,dTrainPri,
                        pruneLevel=pruneLevel,scale=TRUE)
treatedCal <- prepare(treatments,dTrainCal,
                        pruneLevel=pruneLevel,scale=TRUE)
treatedTest <- prepare(treatments,dTest,
                       pruneLevel=pruneLevel,scale=TRUE)
scoreVarCal <- function(vname,trainFrame,calFrame) {
  mv <- glm(paste(yCond,vname,sep=' ~ '),data=trainFrame,family=binomial(link='logit'))
  sv <- predict(mv,newdata=calFrame,type='response')
  ncal <- length(calFrame[,yName])
  sc <- sum(calFrame[,yName]==yTarget)/ncal
  (sum(ifelse(calFrame[,yName]==yTarget,log(sv),log(1-sv))) - sum(ifelse(calFrame[,yName]==yTarget,log(sc),log(1-sc))))/ncal
}
m1vars <- intersect(treatments$vars,colnames(treatedTrain))
varValues <- sapply(m1vars,function(v) scoreVarCal(v,treatedTrain,treatedCal)) # weak noisy variables, so score on calibration set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
print(table(PRESSgood=treatments$varScores[m1vars]<1,CALgood=varValues>0))
```

```
##          CALgood
## PRESSgood FALSE TRUE
##      TRUE    39  119
```

```r
mvars <- names(varValues)[varValues>1.0e-5]


# Add some principal components as new synthetic variables (better would be something like partial least squares 
# as we are working towards inverse-regressio style effects).  
# See http://www.win-vector.com/blog/2014/06/skimming-statistics-papers-for-the-ideas-instead-of-the-complete-procedures/ )
pcomp <- prcomp(treatedTrain[,mvars])
goodP <- pcomp$sdev>=1.0e-3
projection <- pcomp$rotation[,goodP]
treatedTrainP <- as.data.frame(as.matrix(treatedTrain[,mvars]) %*% projection)
pvars1 <- colnames(treatedTrainP)
treatedTrainP <- cbind(treatedTrainP,treatedTrain)
treatedCalP <- as.data.frame(as.matrix(treatedCal[,mvars]) %*% projection)
treatedCalP <- cbind(treatedCalP,treatedCal)
treatedTestP <- as.data.frame(as.matrix(treatedTest[,mvars]) %*% projection)
treatedTestP <- cbind(treatedTestP,treatedTest)
pvarValues <- sapply(pvars1,function(v) scoreVarCal(v,treatedTrainP,treatedCalP)) # weak noisy variables, so score on calibration set
pvars <- names(pvarValues)[pvarValues>1.0e-5]


uvars <- c(mvars,pvars)


for (varset in list(mvars,pvars,uvars)) {
   print('----------')
   print(varset)
   
   print('')
   print('glm')
   # simple glm model (just to show things work)
   formulaP <- paste(yCond,paste(varset,collapse=' + '),sep=' ~ ')
   modelP <- glm(formulaP,data=treatedTrainP,family=binomial(link='logit'))
   treatedTestP$glmPred <- predict(modelP,newdata=treatedTestP,type='response')
   print(ggplot(data=treatedTestP) +
      geom_density(aes_string(x='glmPred',color=paste('as.factor(',yName,')'))))
   # compute AUC aas in chapter 5 of Practical Data Science with R
   ROCR_glmPred <- prediction(treatedTestP$glmPred,treatedTest[,yName]==yTarget)
   #plot(performance(ROCR_glmPred,"tpr","fpr"))
   glmPredAUC <- attributes(performance(ROCR_glmPred,'auc'))$y.values[[1]]
   print(glmPredAUC)
   print('')

   print('')
   print('randomForest')
   # random forest model, see chapter 9 of Practical Data Science with R
   modelF <- randomForest(x=treatedTrainP[,varset],
      y=as.factor(treatedTrainP[,yName]==yTarget),
      ntree=300)
   treatedTestP$rfPred <- predict(modelF,newdata=treatedTestP[,varset],type='prob')[,'TRUE']
   print(ggplot(data=treatedTestP) +
      geom_density(aes_string(x='rfPred',color=paste('as.factor(',yName,')'))))
   ROCR_rfPred <- prediction(treatedTestP$rfPred,treatedTest[,yName]==yTarget)
   #plot(performance(ROCR_rfPred,"tpr","fpr"))
   rfPredAUC <- attributes(performance(ROCR_rfPred,'auc'))$y.values[[1]]
   print(rfPredAUC)
   print('')

   print('')
   print('knn')
   # knn model, see chapter 6 of Practical Data Science with R
   knnPred <- function(trainF,trainY,nK,testF) {
       knnDecision <- knn(trainF,testF,trainY,k=nK,prob=T)
       ifelse(knnDecision==TRUE,
          attributes(knnDecision)$prob,
          1-(attributes(knnDecision)$prob))
   }
   treatedTestP$knnPred <- knnPred(treatedTrainP[,varset],treatedTrainP[,yName],
                                   200,
                                   treatedTestP[,varset])
   print(ggplot(data=treatedTestP) +
      geom_density(aes_string(x='knnPred',color=paste('as.factor(',yName,')'))))
   ROCR_knnPred <- prediction(treatedTestP$knnPred,treatedTest[,yName]==yTarget)
   #plot(performance(ROCR_knnPred,"tpr","fpr"))
   knnPredAUC <- attributes(performance(ROCR_knnPred,'auc'))$y.values[[1]]
   print(knnPredAUC)
   print('')
}
```

```
## [1] "----------"
##   [1] "Var2_isBAD"   "Var3_isBAD"   "Var4_isBAD"   "Var6_clean"  
##   [5] "Var6_isBAD"   "Var7_clean"   "Var7_isBAD"   "Var11_isBAD" 
##   [9] "Var13_clean"  "Var13_isBAD"  "Var14_isBAD"  "Var17_isBAD" 
##  [13] "Var18_isBAD"  "Var19_isBAD"  "Var21_isBAD"  "Var22_isBAD" 
##  [17] "Var25_isBAD"  "Var28_clean"  "Var28_isBAD"  "Var34_isBAD" 
##  [21] "Var35_isBAD"  "Var36_isBAD"  "Var37_isBAD"  "Var38_isBAD" 
##  [25] "Var40_isBAD"  "Var43_isBAD"  "Var44_isBAD"  "Var46_isBAD" 
##  [29] "Var49_isBAD"  "Var51_isBAD"  "Var54_isBAD"  "Var65_clean" 
##  [33] "Var65_isBAD"  "Var68_isBAD"  "Var72_clean"  "Var73_clean" 
##  [37] "Var74_clean"  "Var74_isBAD"  "Var75_isBAD"  "Var76_isBAD" 
##  [41] "Var78_isBAD"  "Var81_clean"  "Var81_isBAD"  "Var82_isBAD" 
##  [45] "Var83_isBAD"  "Var84_isBAD"  "Var85_isBAD"  "Var88_isBAD" 
##  [49] "Var95_isBAD"  "Var96_isBAD"  "Var99_isBAD"  "Var101_isBAD"
##  [53] "Var106_isBAD" "Var112_isBAD" "Var113_clean" "Var114_isBAD"
##  [57] "Var117_isBAD" "Var119_isBAD" "Var122_isBAD" "Var123_isBAD"
##  [61] "Var124_isBAD" "Var125_clean" "Var125_isBAD" "Var126_clean"
##  [65] "Var126_isBAD" "Var127_isBAD" "Var128_isBAD" "Var130_isBAD"
##  [69] "Var132_isBAD" "Var133_isBAD" "Var134_isBAD" "Var135_isBAD"
##  [73] "Var138_isBAD" "Var140_isBAD" "Var143_isBAD" "Var144_clean"
##  [77] "Var144_isBAD" "Var145_isBAD" "Var149_clean" "Var150_isBAD"
##  [81] "Var152_isBAD" "Var153_isBAD" "Var155_isBAD" "Var158_isBAD"
##  [85] "Var159_isBAD" "Var160_isBAD" "Var161_isBAD" "Var162_isBAD"
##  [89] "Var163_isBAD" "Var164_isBAD" "Var165_isBAD" "Var170_isBAD"
##  [93] "Var171_isBAD" "Var173_isBAD" "Var174_isBAD" "Var176_isBAD"
##  [97] "Var177_isBAD" "Var179_isBAD" "Var181_isBAD" "Var182_isBAD"
## [101] "Var183_isBAD" "Var184_isBAD" "Var188_isBAD" "Var189_clean"
## [105] "Var191_catN"  "Var193_catN"  "Var205_catN"  "Var206_catN" 
## [109] "Var207_catN"  "Var210_catN"  "Var211_catN"  "Var218_catN" 
## [113] "Var219_catN"  "Var221_catN"  "Var225_catN"  "Var226_catN" 
## [117] "Var227_catN"  "Var228_catN"  "Var229_catN" 
## [1] ""
## [1] "glm"
```

```
## Warning: prediction from a rank-deficient fit may be misleading
```

![plot of chunk kddexanalyze](figure/kddexanalyze1.png) 

```
## [1] 0.706
## [1] ""
## [1] ""
## [1] "randomForest"
```

![plot of chunk kddexanalyze](figure/kddexanalyze2.png) 

```
## [1] 0.6867
## [1] ""
## [1] ""
## [1] "knn"
```

![plot of chunk kddexanalyze](figure/kddexanalyze3.png) 

```
## [1] 0.6846
## [1] ""
## [1] "----------"
##  [1] "PC1"  "PC2"  "PC3"  "PC4"  "PC5"  "PC6"  "PC7"  "PC10" "PC12" "PC14"
## [11] "PC15" "PC16" "PC18" "PC20" "PC21" "PC22" "PC23" "PC25" "PC28" "PC29"
## [21] "PC30" "PC32" "PC35"
## [1] ""
## [1] "glm"
```

![plot of chunk kddexanalyze](figure/kddexanalyze4.png) 

```
## [1] 0.6984
## [1] ""
## [1] ""
## [1] "randomForest"
```

![plot of chunk kddexanalyze](figure/kddexanalyze5.png) 

```
## [1] 0.6773
## [1] ""
## [1] ""
## [1] "knn"
```

```
## [1] 0.6892
## [1] ""
## [1] "----------"
##   [1] "Var2_isBAD"   "Var3_isBAD"   "Var4_isBAD"   "Var6_clean"  
##   [5] "Var6_isBAD"   "Var7_clean"   "Var7_isBAD"   "Var11_isBAD" 
##   [9] "Var13_clean"  "Var13_isBAD"  "Var14_isBAD"  "Var17_isBAD" 
##  [13] "Var18_isBAD"  "Var19_isBAD"  "Var21_isBAD"  "Var22_isBAD" 
##  [17] "Var25_isBAD"  "Var28_clean"  "Var28_isBAD"  "Var34_isBAD" 
##  [21] "Var35_isBAD"  "Var36_isBAD"  "Var37_isBAD"  "Var38_isBAD" 
##  [25] "Var40_isBAD"  "Var43_isBAD"  "Var44_isBAD"  "Var46_isBAD" 
##  [29] "Var49_isBAD"  "Var51_isBAD"  "Var54_isBAD"  "Var65_clean" 
##  [33] "Var65_isBAD"  "Var68_isBAD"  "Var72_clean"  "Var73_clean" 
##  [37] "Var74_clean"  "Var74_isBAD"  "Var75_isBAD"  "Var76_isBAD" 
##  [41] "Var78_isBAD"  "Var81_clean"  "Var81_isBAD"  "Var82_isBAD" 
##  [45] "Var83_isBAD"  "Var84_isBAD"  "Var85_isBAD"  "Var88_isBAD" 
##  [49] "Var95_isBAD"  "Var96_isBAD"  "Var99_isBAD"  "Var101_isBAD"
##  [53] "Var106_isBAD" "Var112_isBAD" "Var113_clean" "Var114_isBAD"
##  [57] "Var117_isBAD" "Var119_isBAD" "Var122_isBAD" "Var123_isBAD"
##  [61] "Var124_isBAD" "Var125_clean" "Var125_isBAD" "Var126_clean"
##  [65] "Var126_isBAD" "Var127_isBAD" "Var128_isBAD" "Var130_isBAD"
##  [69] "Var132_isBAD" "Var133_isBAD" "Var134_isBAD" "Var135_isBAD"
##  [73] "Var138_isBAD" "Var140_isBAD" "Var143_isBAD" "Var144_clean"
##  [77] "Var144_isBAD" "Var145_isBAD" "Var149_clean" "Var150_isBAD"
##  [81] "Var152_isBAD" "Var153_isBAD" "Var155_isBAD" "Var158_isBAD"
##  [85] "Var159_isBAD" "Var160_isBAD" "Var161_isBAD" "Var162_isBAD"
##  [89] "Var163_isBAD" "Var164_isBAD" "Var165_isBAD" "Var170_isBAD"
##  [93] "Var171_isBAD" "Var173_isBAD" "Var174_isBAD" "Var176_isBAD"
##  [97] "Var177_isBAD" "Var179_isBAD" "Var181_isBAD" "Var182_isBAD"
## [101] "Var183_isBAD" "Var184_isBAD" "Var188_isBAD" "Var189_clean"
## [105] "Var191_catN"  "Var193_catN"  "Var205_catN"  "Var206_catN" 
## [109] "Var207_catN"  "Var210_catN"  "Var211_catN"  "Var218_catN" 
## [113] "Var219_catN"  "Var221_catN"  "Var225_catN"  "Var226_catN" 
## [117] "Var227_catN"  "Var228_catN"  "Var229_catN"  "PC1"         
## [121] "PC2"          "PC3"          "PC4"          "PC5"         
## [125] "PC6"          "PC7"          "PC10"         "PC12"        
## [129] "PC14"         "PC15"         "PC16"         "PC18"        
## [133] "PC20"         "PC21"         "PC22"         "PC23"        
## [137] "PC25"         "PC28"         "PC29"         "PC30"        
## [141] "PC32"         "PC35"        
## [1] ""
## [1] "glm"
```

```
## Warning: prediction from a rank-deficient fit may be misleading
```

![plot of chunk kddexanalyze](figure/kddexanalyze6.png) ![plot of chunk kddexanalyze](figure/kddexanalyze7.png) 

```
## [1] 0.706
## [1] ""
## [1] ""
## [1] "randomForest"
```

![plot of chunk kddexanalyze](figure/kddexanalyze8.png) 

```
## [1] 0.6981
## [1] ""
## [1] ""
## [1] "knn"
```

![plot of chunk kddexanalyze](figure/kddexanalyze9.png) 

```
## [1] 0.6846
## [1] ""
```
