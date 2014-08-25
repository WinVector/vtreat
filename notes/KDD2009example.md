


```r
# To make the html: echo "library(knitr); knit('KDD2009example.Rmd')" | R --vanilla ; pandoc KDD2009example.md -o KDD2009example.html
# Example of working with KDD2009 data (just to show library at work).
# For data and details see: https://github.com/WinVector/zmPDSwR/tree/master/KDD2009
# and Chapter 6 of Practical Data Science with R: http://www.amazon.com/Practical-Data-Science/dp/1617291560
# load the data as in the book
dir <- '~/Documents/work/DataScienceBook/zmPDSwR/KDD2009/'
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
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
rm(list=c('d','churn','appetency','upselling','dir'))
outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll),
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
treatments <- designTreatmentsC(dTrainAll[sample.int(n=dim(dTrainAll)[[1]],size=2000),],vars,yName,1)
pruneLevel <- 0.99
treatedTrain <- prepare(treatments,dTrainAll,
                        pruneLevel=pruneLevel,scale=TRUE)
treatedTest <- prepare(treatments,dTest,
                       pruneLevel=pruneLevel,scale=TRUE)
mvars <- intersect(treatments$vars,colnames(treatedTrain))
```


```r
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
library('vtreat')  # This library isn't public yet, intall instructions: http://www.win-vector.com/blog/2014/08/vtreat-designin# try a principal components version of the analysis
pcomp <- prcomp(treatedTrain[,mvars])
goodP <- pcomp$sdev>1.0e-2
projection <- pcomp$rotation[,goodP]
treatedTrainP <- as.data.frame(as.matrix(treatedTrain[,mvars]) %*% projection)
pvars <- colnames(treatedTrainP)
treatedTrainP[,yName] = treatedTrain[,yName]
treatedTestP <- as.data.frame(as.matrix(treatedTest[,mvars]) %*% projection)
treatedTestP[,yName] = treatedTest[,yName]
formulaP <- paste(yCond,paste(pvars,collapse=' + '),sep=' ~ ')

# simple glm model
modelP <- glm(formulaP,data=treatedTrainP,family=binomial(link='logit'))
treatedTestP$glmPred <- predict(modelP,newdata=treatedTestP,type='link')
ggplot(data=treatedTestP) + 
   geom_density(aes_string(x='glmPred',color=paste('as.factor(',yName,')')))
```

![plot of chunk kddexanalyze](figure/kddexanalyze.png) 

```r
# compute AUC aas in chapter 5 of Practical Data Science with R
ROCRpred <- prediction(treatedTest$glmPred,treatedTest[,yName]==yTarget)
```

```
## Error: Format of predictions is invalid.
```

```r
plot(performance(ROCRpred,"tpr","fpr"))
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'plot': Error in performance(ROCRpred, "tpr", "fpr") : 
##   object 'ROCRpred' not found
```

```r
auc <- attributes(performance(ROCRpred,'auc'))$y.values[[1]]
```

```
## Error: object 'ROCRpred' not found
```

```r
print(auc)
```

```
## Error: object 'auc' not found
```
