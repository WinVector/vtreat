## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library("vtreat")
set.seed(23255)

have_ggplot = requireNamespace("ggplot2", quietly=TRUE)
if(have_ggplot) {
  library("ggplot2")
}

## ----gam-----------------------------------------------------------------
set.seed(23255)
d <- data.frame(x=0.2*(1:30))
d$y <- sin(d$x) + 0.1*rnorm(nrow(d))
m <- gamXform(d$x,d$y)
d$pred <- m(d$x)
if(have_ggplot) {
  print(ggplot(data=d,mapping=aes(x=x)) + 
          geom_point(aes(y=y)) +
          geom_line(aes(y=pred),color='blue') +
          ggtitle("gamXform regression example",
                  subtitle = "estimated expected value as a function of x"))
  print(ggplot(data=d,mapping=aes(x=pred,y=y)) + 
          geom_point() +
          geom_abline(color='blue') +
          ggtitle("gamXform regression example",
                  subtitle = "y as a function of prediction"))
}

## ----gamc----------------------------------------------------------------
set.seed(23255)
d$yC <- (sin(d$x) + rnorm(nrow(d))) >0.0
mC <- gamXform(d$x, d$yC, family= stats::binomial(link='logit'))
d$predC <- mC(d$x)
if(have_ggplot) {
  print(ggplot(data=d,mapping=aes(x=x,y=predC)) +
          geom_line() + 
          ggtitle("predicted probability as a function of x"))
  print(ggplot(data=d,mapping=aes(x=predC,color=yC)) + 
          geom_density() +
          facet_wrap(~yC, scales = 'free_y', ncol=1) +
          ggtitle("gamXform classification example"))
}
print(table(truth=d$yC,pred=d$predC>0.5))

## ----gamck---------------------------------------------------------------
mC <- gamXform(d$x, d$yC, k=3, family= stats::binomial(link='logit'))
d$predC <- mC(d$x)
if(have_ggplot) {
  print(ggplot(data=d,mapping=aes(x=x,y=predC)) +
          geom_line() + 
          ggtitle("predicted probability as a function of x, k=3"))
  print(ggplot(data=d,mapping=aes(x=predC,color=yC)) + 
          geom_density() +
          facet_wrap(~yC, scales = 'free_y', ncol=1) +
          ggtitle("gamXform classification example, k=3"))
}
print(table(truth=d$yC,pred=d$predC>0.5))

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
d$pred <- m(d$x)

d2 <- data.frame(x=-20:20)
d2$pred <- m(d2$x)
if(have_ggplot) {
  ggplot() + 
    geom_point(data=d,mapping=aes(x=x,y=pred),color='blue',
               shape=0) + 
    geom_point(data=d,mapping=aes(x=x,y=y)) + 
    geom_line(data=d2,mapping=aes(x=x,y=pred),color='blue') +
    geom_point(data=d2,mapping=aes(x=x,y=pred),color='blue',
               shape=3,alpha=0.5) +
    ggtitle("recovered monotone (increasing or decreasing) relation")
}
print(d)

## ----vdata, echo=FALSE, message=FALSE, warning=FALSE---------------------
set.seed(33463)
dTrain <- data.frame(x1=rnorm(100),x2=rnorm(100))
dTrain$y <- sin(dTrain$x1) + dTrain$x2^3 + rnorm(100)
dCal <- dTrain
dTest <- dTrain
yname <- 'y'
colNamesToGam <- 'x1'
colNamesToMonotone <- 'x2'
varlist <- c(colNamesToGam,colNamesToMonotone)

## ----vtreatsteps---------------------------------------------------------
# design the single variable non-linear transforms
# assumes colNamesToGam disjoint from colNamesToMonotone
gamTreatments <- lapply(colNamesToGam,
                        function(vname) {
                          gamXform(dCal[[vname]], dCal[[yname]])
                        })
names(gamTreatments) <- colNamesToGam
monotoneTreatments <- lapply(colNamesToMonotone,
                        function(vname) {
                          monotoneXform(dCal[[vname]], dCal[[yname]])
                        })
names(monotoneTreatments) <- colNamesToMonotone
preTreatments <- c(gamTreatments,monotoneTreatments)

# apply the single variable pre treatments
for(vname in names(preTreatments)) {
  dTrain[[vname]] <- preTreatments[[vname]](dTrain[[vname]])
  dTest[[vname]] <- preTreatments[[vname]](dTest[[vname]])
}

# use vtreat on the transformed data
ce <- mkCrossFrameNExperiment(dTrain,varlist,yname)
sf <- ce$treatments$scoreFrame
modelingVars <- sf$varName[sf$sig<1/nrow(sf)]
dTrainPrepared <- ce$crossFrame[ , c(yname,modelingVars), drop= FALSE]
dTestPrepared <- prepare(ce$treatments, dTest,
                         pruneSig= NULL, varRestriction= modelingVars)

