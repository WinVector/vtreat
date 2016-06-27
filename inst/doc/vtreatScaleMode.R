## ----exampledata---------------------------------------------------------
library('vtreat')
dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
                      y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                 catScaling=FALSE,
                                 verbose=FALSE)
dTrainCTreatedUnscaled <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=FALSE)
dTrainCTreatedScaled <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=TRUE)

## ----printorig-----------------------------------------------------------
print(dTrainC)

## ----printunscaled-------------------------------------------------------
print(dTrainCTreatedUnscaled)

## ----printscaled---------------------------------------------------------
print(dTrainCTreatedScaled)

## ----check---------------------------------------------------------------
slopeFrame <- data.frame(varName = treatmentsC$scoreFrame$varName,
                         stringsAsFactors = FALSE)
slopeFrame$mean <-
  vapply(dTrainCTreatedScaled[, slopeFrame$varName, drop = FALSE], mean,
         numeric(1))
slopeFrame$slope <- vapply(slopeFrame$varName,
                           function(c) {
                             lm(paste('y', c, sep = '~'),
                                data = dTrainCTreatedScaled)$coefficients[[2]]
                           },
                           numeric(1))
slopeFrame$sig <- vapply(slopeFrame$varName,
                         function(c) {
                           treatmentsC$scoreFrame[treatmentsC$scoreFrame$varName == c, 'sig']
                         },
                         numeric(1))
slopeFrame$badSlope <-
  ifelse(is.na(slopeFrame$slope), TRUE, abs(slopeFrame$slope - 1) > 1.e-8)
print(slopeFrame)

## ----catscale------------------------------------------------------------
treatmentsC2 <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                  catScaling=TRUE,
                                  verbose=FALSE)
dTrainCTreatedScaled2 <- prepare(treatmentsC2,dTrainC,pruneSig=c(),scale=TRUE)
print(dTrainCTreatedScaled2)

## ----checks--------------------------------------------------------------
colMeans(dTrainCTreatedScaled2)
lm(y~x_lev_NA,data=dTrainCTreatedScaled)
lm(y~x_lev_NA,data=dTrainCTreatedScaled2)

## ------------------------------------------------------------------------
 vapply(slopeFrame$varName,
                           function(c) {
                             glm(paste('y', c, sep = '~'),family=binomial,
                                data = dTrainCTreatedScaled2)$coefficients[[2]]
                           },
                           numeric(1))

## ------------------------------------------------------------------------
set.seed(235235)
dTrainN <- data.frame(x1=rnorm(100),
                      x2=rnorm(100),
                      x3=rnorm(100),
                      stringsAsFactors=FALSE)
dTrainN$y <- 1000*(dTrainN$x1 + dTrainN$x2)
cEraw <- vtreat::mkCrossFrameNExperiment(dTrainN,
                                         c('x1','x2','x3'),'y',
                                         scale=TRUE)
dM1 <- as.matrix(cEraw$crossFrame[,c('x1_clean','x2_clean','x3_clean')])
pCraw <- stats::prcomp(dM1,
                       scale.=FALSE,center=TRUE)
print(pCraw)
dTrainN$yScaled <- scale(dTrainN$y,center=TRUE,scale=TRUE)
cEscaled <- vtreat::mkCrossFrameNExperiment(dTrainN,
                                            c('x1','x2','x3'),'yScaled',
                                            scale=TRUE)
dM2 <- as.matrix(cEscaled$crossFrame[,c('x1_clean','x2_clean','x3_clean')])
pCscaled <- stats::prcomp(dM2,
                          scale.=FALSE,center=TRUE)
print(pCscaled)

