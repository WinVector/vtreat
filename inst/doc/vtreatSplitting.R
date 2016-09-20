## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 7)

## ------------------------------------------------------------------------
vtreat::oneWayHoldout(3,NULL,NULL,NULL)

## ------------------------------------------------------------------------
# This method is not a great idea as the data could have structure that strides
# in the same pattern as this split.
# Such technically is possible for any split, but we typically use
# pseudo-random structure (that is not the same across many potential
# split calls) to try and make it unlikely such structures
# match often.
modularSplit <- function(nRows,nSplits,dframe,y) {
  group <- seq_len(nRows) %% nSplits
  lapply(unique(group),
         function(gi) {
           list(train=which(group!=gi),
                app=which(group==gi))
         })
}

## ------------------------------------------------------------------------
vtreat::buildEvalSets(nRows=25,nSplits=3,splitFunction=modularSplit)

## ------------------------------------------------------------------------
badSplit <- function(nRows,nSplits,dframe,y) {
  list(list(train=seq_len(nRows),app=seq_len(nRows)))
}
vtreat::buildEvalSets(nRows=5,nSplits=3,splitFunction=badSplit)

## ----warning=FALSE-------------------------------------------------------
library('vtreat')
haveGGPlot2 <- requireNamespace("ggplot2",quietly=TRUE)
if(haveGGPlot2) {
  library('ggplot2')
}

## -----------------------------------------------------------------------------------------
set.seed(23255)
d <- data.frame(y=sin(1:100))

# stratified 5-fold cross validation
pStrat <- kWayStratifiedY(nrow(d),5,d,d$y)
# check if the split is a good partition
check = vtreat::problemAppPlan(nrow(d),5,pStrat,TRUE)
if(is.null(check)) {
  print("Plan is good")
} else {
  print(paste0("Problem with plan: ", check))
}
d$stratGroup <- vtreat::getSplitPlanAppLabels(nrow(d),pStrat)

# unstratified 5-fold cross validation
pSimple <- kWayCrossValidation(nrow(d),5,d,d$y)
# check if the split is a good partition; return null if so
check = vtreat::problemAppPlan(nrow(d),5,pSimple,TRUE)
if(is.null(check)) {
  print("Plan is good")
} else {
  print(paste0("Problem with plan: ", check))
}
d$simpleGroup <- vtreat::getSplitPlanAppLabels(nrow(d),pSimple)

# mean(y) for each fold, unstratified
tapply(d$y,d$simpleGroup,mean)
# standard error of mean(y)
sd(tapply(d$y,d$simpleGroup,mean))
if(haveGGPlot2) {
  # plot the distribution of y in each fold
  ggplot(data=d,aes(x=y,color=as.factor(simpleGroup))) + 
    geom_density() + ggtitle('simple (unstratified) grouping')
}
# mean(y) for each fold, unstratified
tapply(d$y,d$stratGroup,mean)
# standard error of mean(y)
sd(tapply(d$y,d$stratGroup,mean))
if(haveGGPlot2) {
  # plot the distribution of y in each fold
  ggplot(data=d,aes(x=y,color=as.factor(stratGroup))) + 
    geom_density() + ggtitle('y-stratified grouping')
}

