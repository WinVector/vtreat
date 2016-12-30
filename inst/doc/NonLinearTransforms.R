## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(vtreat)
set.seed(23255)

have_ggplot = requireNamespace("ggplot2", quietly=TRUE)
if(have_ggplot) {
  library(ggplot2)
}

## ----monotoneXformD------------------------------------------------------
set.seed(36346)
d <- data.frame(x= 10*runif(20), y= rnorm(20))
d$what <- 'y'
dR <- d
dR$what <- 'linearPred'
mR <- lm(y~x,data=d)
dR$y <- predict(mR, newdata= dR)
dL <- d
dL$what <- 'isotonicPred'
m1 <- monotoneXformD(d$x,d$y)
dL$y <- m1(dL$x)
dM <- rbind(dR,dL)
dT <- d
dT$what <- 'theoretical expectation'
dT$y <- 0
dM <- rbind(dR,dL,dT)
if(have_ggplot) {
  ggplot(mapping=aes(x=x,y=y,color=what,linetype=what)) + 
    geom_point(data=d) + 
    geom_line(data=dM) +
    ggtitle("data where there is no relation between x and y")
}

## ----monotoneXformDs-----------------------------------------------------
set.seed(36346)
d <- data.frame(x= 10*runif(20), y= rnorm(20))
reln <- 0.01*d$x^3
d$y <- d$y + reln
d$what <- 'y'
dR <- d
dR$what <- 'linearPred'
mR <- lm(y~x,data=d)
dR$y <- predict(mR, newdata= dR)
dL <- d
dL$what <- 'isotonicPred'
m1 <- monotoneXformD(d$x,d$y)
dL$y <- m1(dL$x)
dT <- d
dT$what <- 'theoretical expectation'
dT$y <- reln
dM <- rbind(dR,dL,dT)
if(have_ggplot) {
  ggplot(mapping=aes(x=x,y=y,color=what,linetype=what)) + 
    geom_point(data=d) + 
    geom_line(data=dM) +
    ggtitle("data where there is a relation between x and y")
}

## ----monotone------------------------------------------------------------
d <- data.frame(x=c(15,1,-12,NA,5,NaN))
d$y <- -log(d$x)
m <- monotoneXform(d$x,d$y)
d2 <- data.frame(x=-20:20)
d2$pred <- m(d2$x)
if(have_ggplot) {
  ggplot() + geom_point(data=d,mapping=aes(x=x,y=y)) + 
    geom_line(data=d2,mapping=aes(x=x,y=pred),color='blue')
}
print(d)

## ----gam-----------------------------------------------------------------
d <- data.frame(x=0.2*(1:30))
d$y <- sin(d$x) + 0.1*rnorm(nrow(d))
m <- gamXform(d$x,d$y)
d$pred <- m(d$x)
if(have_ggplot) {
  ggplot(data=d,mapping=aes(x=x)) + geom_point(aes(y=y)) +
    geom_line(aes(y=pred),color='blue')
}

## ----gamc----------------------------------------------------------------
d$yC <- (sin(d$x) + 0.5*rnorm(nrow(d))) >0.0
mC <- gamXform(d$x, d$yC, family= stats::binomial(link='logit'))
d$predC <- mC(d$x)
if(have_ggplot) {
  ggplot(data=d,mapping=aes(x=predC,color=yC)) + geom_density()
}
print(table(truth=d$yC,pred=d$predC>0.5))

## ----eval=FALSE----------------------------------------------------------
#  # design non-linear transforms
#  # assumes colNamesToGam disjoint from colNamesToMonotone
#  gamTreatments <- lapply(colNamesToGam,
#                          function(vname) {
#                            gamXform(dCal[[vname]], dCal[[yname]])
#                          })
#  names(gamTreatments) <- colNamesToGam
#  monotoneTreatments <- lapply(colNamesToMonotone,
#                          function(vname) {
#                            monotoneXform(dCal[[vname]], dCal[[yname]])
#                          })
#  names(monotoneTreatments) <- colNamesToMonotone
#  preTreatments <- c(gamTreatments,monotoneTreatments)
#  
#  # apply the pre treatments
#  for(vname in names(preTreatments)) {
#    dTrain[[vname]] <- preTreatments[[vname]](dTrain[[vname]])
#    dTest[[vname]] <- preTreatments[[vname]](dTest[[vname]])
#  }
#  
#  # use vtreat on the transformed data
#  ce <- mkCrossFrameNExperiment(dTrain,varlist,yname)
#  sf <- ce$treatments$scoreFrame
#  modelingVars <- sf$varName[sf$sig<1/nrow(sf)]
#  dTrainPrepared <- ce$crossFrame[ , c(yname,modelingVars), drop= FALSE]
#  dTestPrepared <- prepare(ce$treatments, dTest,
#                           pruneSig= NULL, varRestriction= modelingVars)

