## ------------------------------------------------------------------------
library('vtreat')
library('ggplot2')

set.seed(2325)
populationFrame <- data.frame(
   popsize = round(rlnorm(100,meanlog=log(4000),sdlog=1)),
   stringsAsFactors = FALSE)
populationFrame$code <- paste0('z',formatC(sample.int(100000,
                                              size=nrow(populationFrame),
                                              replace=FALSE),width=5,flag='0'))
rareCodes <- populationFrame$code[populationFrame$popsize<1000]

# Draw individuals from code-regions proportional to size of code region
# (or uniformly over all individuals labeled by code region).
# Also add the outcome which has elevated probability for rareCodes.
drawIndividualsAndReturnCodes <- function(n) {
  ords <- sort(sample.int(sum(populationFrame$popsize),size=n,replace=TRUE))
  cs <- cumsum(populationFrame$popsize)
  indexes <- findInterval(ords,cs,left.open=TRUE)+1
  indexes <- indexes[sample.int(n,size=n,replace=FALSE)]
  samp <- data.frame(code=populationFrame$code[indexes],
                     stringsAsFactors = FALSE)
  samp$inClass <- runif(n) < ifelse(samp$code %in% rareCodes,0.3,0.01)
  samp
}

## ------------------------------------------------------------------------
testSet <- drawIndividualsAndReturnCodes(2000)
table(generatedAsRare=testSet$code %in% rareCodes,inClass=testSet$inClass)

## ------------------------------------------------------------------------
designSet <- drawIndividualsAndReturnCodes(2000)
treatments <- vtreat::designTreatmentsC(designSet,'code','inClass',TRUE,
                                        rareCount=5,rareSig=NULL,
                                        verbose=FALSE)
treatments$scoreFrame[,c('varName','sig'),drop=FALSE]

## ------------------------------------------------------------------------
designSetTreated <-  vtreat::prepare(treatments,designSet,pruneSig=0.5)
designSetTreated$code <- designSet$code
summary(as.numeric(table(designSetTreated$code[designSetTreated$code_lev_rare==1])))
summary(as.numeric(table(designSetTreated$code[designSetTreated$code_lev_rare!=1])))

## ---- fig.width=6--------------------------------------------------------
testSetTreated <- vtreat::prepare(treatments,testSet,pruneSig=0.5)
testSetTreated$code <- testSet$code
testSetTreated$newCode <- !(testSetTreated$code %in% unique(designSet$code))
testSetTreated$generatedAsRareCode <- testSetTreated$code %in% rareCodes

# Show code_lev_rare==1 corresponds to a subset of rows with elevated inClass==TRUE rate.
table(code_lev_rare=testSetTreated$code_lev_rare,
      inClass=testSetTreated$inClass)

# Show newCodes get coded with code_level_rare==1.

table(newCode=testSetTreated$newCode,code_lev_rare=testSetTreated$code_lev_rare)

# Show newCodes tend to come from defined rareCodes.
table(newCode=testSetTreated$newCode,
      generatedAsRare=testSetTreated$generatedAsRareCode)

## ---- fig.width=6--------------------------------------------------------
# Show code_catP's behavior on rare and novel levels.
summary(testSetTreated$code_catP)
ggplot(data=testSetTreated,aes(x=code_catP)) + geom_density()

summary(testSetTreated$code_catP[testSetTreated$code_lev_rare==1])
ggplot(data=testSetTreated,aes(x=code_catP,color=as.factor(testSetTreated$code_lev_rare))) + 
  geom_density() + theme(legend.position="bottom")

summary(testSetTreated$code_catP[testSetTreated$newCode])

summary(testSetTreated$code_catP[testSetTreated$generatedAsRareCode])
ggplot(data=testSetTreated,aes(x=code_catP,color=as.factor(testSetTreated$generatedAsRareCode))) + 
  geom_density() + theme(legend.position="bottom")

