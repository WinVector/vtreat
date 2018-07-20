## ------------------------------------------------------------------------
set.seed(22626)

mkData <- function(n) {
  d <- data.frame(xBad1=sample(paste('level',1:1000,sep=''),n,replace=TRUE),
                  xBad2=sample(paste('level',1:1000,sep=''),n,replace=TRUE),
                  xBad3=sample(paste('level',1:1000,sep=''),n,replace=TRUE),
                  xGood1=rnorm(n),
                  xGood2=rnorm(n))
  
  # outcome only depends on "good" variables
  d$y <- rnorm(nrow(d))+0.2*d$xGood1 + 0.3*d$xGood2>0.5
  # the random group used for splitting the data set, not a variable.
  d$rgroup <- sample(c("cal","train","test"),nrow(d),replace=TRUE)  
  d
}

d <- mkData(2000)

# devtools::install_github("WinVector/WVPlots")
# library('WVPlots')
plotRes <- function(d,predName,yName,title) {
  print(title)
  tab <- table(truth=d[[yName]],pred=d[[predName]]>0.5)
  print(tab)
  diag <- sum(vapply(seq_len(min(dim(tab))),
                     function(i) tab[i,i],numeric(1)))
  acc <- diag/sum(tab)
#  if(requireNamespace("WVPlots",quietly=TRUE)) {
#     print(WVPlots::ROCPlot(d,predName,yName,title))
#  }
  print(paste('accuracy',acc))
}

## ----badmixcalandtrain---------------------------------------------------
dTrain <- d[d$rgroup!='test',,drop=FALSE]
dTest <- d[d$rgroup=='test',,drop=FALSE]
treatments <- vtreat::designTreatmentsC(dTrain,c('xBad1','xBad2','xBad3','xGood1','xGood2'),
                                        'y',TRUE,
  rareCount=0 # Note: usually want rareCount>0, setting to zero to illustrate problem
)
dTrainTreated <- vtreat::prepare(treatments,dTrain,
  pruneSig=c() # Note: usually want pruneSig to be a small fraction, setting to null to illustrate problems
)
m1 <- glm(y~xBad1_catB + xBad2_catB + xBad3_catB + xGood1_clean + xGood2_clean,
          data=dTrainTreated,family=binomial(link='logit'))
print(summary(m1))  # notice low residual deviance

dTrain$predM1 <- predict(m1,newdata=dTrainTreated,type='response')
plotRes(dTrain,'predM1','y','model1 on train')
dTestTreated <- vtreat::prepare(treatments,dTest,pruneSig=c())
dTest$predM1 <- predict(m1,newdata=dTestTreated,type='response')
plotRes(dTest,'predM1','y','model1 on test')

## ----separatecalandtrain-------------------------------------------------
dCal <- d[d$rgroup=='cal',,drop=FALSE]
dTrain <- d[d$rgroup=='train',,drop=FALSE]
dTest <- d[d$rgroup=='test',,drop=FALSE]

# a nice heuristic, 
# expect only a constant number of noise variables to sneak past
pruneSig <- 1/ncol(dTrain) 
treatments <- vtreat::designTreatmentsC(dCal,
                                        c('xBad1','xBad2','xBad3','xGood1','xGood2'),
                                        'y',TRUE,
  rareCount=0 # Note: usually want rareCount>0, setting to zero to illustrate problem
)
dTrainTreated <- vtreat::prepare(treatments,dTrain,
  pruneSig=pruneSig)
newvars <- setdiff(colnames(dTrainTreated),'y')
m1 <- glm(paste('y',paste(newvars,collapse=' + '),sep=' ~ '),
          data=dTrainTreated,family=binomial(link='logit'))
print(summary(m1))  

dTrain$predM1 <- predict(m1,newdata=dTrainTreated,type='response')
plotRes(dTrain,'predM1','y','model1 on train')
dTestTreated <- vtreat::prepare(treatments,dTest,
                                pruneSig=pruneSig)
dTest$predM1 <- predict(m1,newdata=dTestTreated,type='response')
plotRes(dTest,'predM1','y','model1 on test')

## ----crossframes---------------------------------------------------------
dTrain <- d[d$rgroup!='test',,drop=FALSE]
dTest <- d[d$rgroup=='test',,drop=FALSE]
prep <- vtreat::mkCrossFrameCExperiment(dTrain,
           c('xBad1','xBad2','xBad3','xGood1','xGood2'),
           'y',TRUE,
           rareCount=0 # Note: usually want rareCount>0, setting to zero to illustrate problems
)
treatments <- prep$treatments

knitr::kable(treatments$scoreFrame[,c('varName','sig')])
colnames(prep$crossFrame)

# vtreat::mkCrossFrameCExperiment doesn't take a pruneSig argument, but we can
# prune on our own.
print(pruneSig)
newvars <- treatments$scoreFrame$varName[treatments$scoreFrame$sig<=pruneSig]
# force in bad variables, to show we "belt and suspenders" deal with them
# in that things go well in the cross-frame even if they sneak past pruning
newvars <- sort(union(newvars,c("xBad1_catB","xBad2_catB","xBad3_catB")))
print(newvars)
dTrainTreated <- prep$crossFrame

## ----xframemodel---------------------------------------------------------
m1 <- glm(paste('y',paste(newvars,collapse=' + '),sep=' ~ '),
          data=dTrainTreated,family=binomial(link='logit'))
print(summary(m1))  

dTrain$predM1 <- predict(m1,newdata=dTrainTreated,type='response')
plotRes(dTrain,'predM1','y','model1 on train')
dTestTreated <- vtreat::prepare(treatments,dTest,
                                pruneSig=c(),varRestriction=newvars)
knitr::kable(head(dTestTreated))
dTest$predM1 <- predict(m1,newdata=dTestTreated,type='response')
plotRes(dTest,'predM1','y','model1 on test')

