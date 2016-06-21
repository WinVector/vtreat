## ---- tidy=FALSE---------------------------------------------------------
library(vtreat)
dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
head(dTrainC)

dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
head(dTestC)

treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
print(treatmentsC)
print(treatmentsC$treatments[[1]])

## ---- tidy=FALSE---------------------------------------------------------

dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=TRUE)
head(dTrainCTreated)

varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
# all slopes should be 1 for variables with dTrainCTreated$scoreFrame$sig<1
sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})

dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE)
head(dTestCTreated)

## ---- tidy=FALSE---------------------------------------------------------

# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA),
   z=c(1,2,3,4,5,NA,7),y=c(0,0,0,1,0,1,1))
head(dTrainN)

dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
head(dTestN)

treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
print(treatmentsN)

dTrainNTreated <- prepare(treatmentsN,dTrainN,
                          pruneSig=c(),scale=TRUE)
head(dTrainNTreated)

varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
# all slopes should be 1 for variables with treatmentsN$scoreFrame$sig<1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 


# prepared frame
dTestNTreated <- prepare(treatmentsN,dTestN,
                         pruneSig=c())
head(dTestNTreated)

# scaled prepared frame
dTestNTreatedS <- prepare(treatmentsN,dTestN,
                         pruneSig=c(),scale=TRUE)
head(dTestNTreatedS)

