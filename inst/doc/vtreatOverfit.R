## ------------------------------------------------------------------------
set.seed(22626)
d <- data.frame(x=sample(paste('level',1:1000,sep=''),2000,replace=TRUE)) # independent variable.
d$y <- runif(nrow(d))>0.5  # the quantity to be predicted, notice: independent of variables.
d$rgroup <- round(100*runif(nrow(d)))  # the random group used for splitting the data set, not a variable.

## ------------------------------------------------------------------------
dTrain <- d[d$rgroup<=80,,drop=FALSE]
dTest <- d[d$rgroup>80,,drop=FALSE]
library('vtreat')
treatments <- vtreat::designTreatmentsC(dTrain,'x','y',TRUE,
  rareCount=0 # Note: usually want rareCount>0, setting to zero to illustrate problem
)
dTrainTreated <- vtreat::prepare(treatments,dTrain,
  pruneSig=c() # Note: usually want pruneSig to be a small fraction, setting to null to illustrate problem
)
m1 <- glm(y~x_catB,data=dTrainTreated,family=binomial(link='logit'))
print(summary(m1))  # notice low residual deviance
dTrain$predM1 <- predict(m1,newdata=dTrainTreated,type='response')

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

# evaluate model on training
plotRes(dTrain,'predM1','y','model1 on train')

# evaluate model on test
dTestTreated <- vtreat::prepare(treatments,dTest,pruneSig=c())
dTest$predM1 <- predict(m1,newdata=dTestTreated,type='response')
plotRes(dTest,'predM1','y','model1 on test')

## ------------------------------------------------------------------------
print(treatments$scoreFrame)

## ------------------------------------------------------------------------
dCode <- d[d$rgroup<=20,,drop=FALSE]
dTrain <- d[(d$rgroup>20) & (d$rgroup<=80),,drop=FALSE]
treatments <- vtreat::designTreatmentsC(dCode,'x','y',TRUE,
                                        rareCount=0,  # Note set this to something larger, like 5
                                        rareSig=c() # Note set this to something like 0.3
)
dTrainTreated <- vtreat::prepare(treatments,dTrain,
                                 pruneSig=c() # Note: set this to filter, like 0.05 or 1/nvars
)
m2 <- glm(y~x_catB,data=dTrainTreated,family=binomial(link='logit'))
print(summary(m2)) # notice high residual deviance
dTrain$predM2 <- predict(m2,newdata=dTrainTreated,type='response')
plotRes(dTrain,'predM2','y','model2 on train')
# We do not advise creating dCodeTreated for any purpose other than
# diagnostic plotting.  You should not use the treated coding data
# for anything (as that would undo the benefit of having a separate
# coding data subset).
dCodeTreated <- vtreat::prepare(treatments,dCode,pruneSig=c())
dCode$predM2 <- predict(m2,newdata=dCodeTreated,type='response')
plotRes(dCode,'predM2','y','model2 on coding set')
dTestTreated <- vtreat::prepare(treatments,dTest,pruneSig=c())
dTest$predM2 <- predict(m2,newdata=dTestTreated,type='response')
plotRes(dTest,'predM2','y','model2 on test set')

