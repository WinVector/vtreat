library('vtreat')

context("Test Scale")

test_that("testScale: check corner case on scale", {
  library('vtreat')
  dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
                        z=c(1,2,3,4,NA,6),
                        y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
  treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE,
                                   verbose=FALSE)
  dTrainCTreatedUnscaled <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=FALSE)
  dTrainCTreatedScaled <- prepare(treatmentsC,dTrainC,pruneSig=c(),scale=TRUE)
  slopeFrame <- data.frame(varName=treatmentsC$scoreFrame$varName,
                           stringsAsFactors = FALSE)
  slopeFrame$mean <- vapply(dTrainCTreatedScaled[,slopeFrame$varName,drop=FALSE],mean,
                            numeric(1))
  slopeFrame$slope <- vapply(slopeFrame$varName,
                             function(c) { 
                               lm(paste('y',c,sep='~'),
                                  data=dTrainCTreatedScaled)$coefficients[[2]]
                             },
                             numeric(1))
  slopeFrame$sig <- vapply(slopeFrame$varName,
                           function(c) { 
                             treatmentsC$scoreFrame[treatmentsC$scoreFrame$varName==c,'sig']
                           },
                           numeric(1))
  slopeFrame$badSlope <- ifelse(is.na(slopeFrame$slope),TRUE,abs(slopeFrame$slope-1)>1.e-8)
  expect_false(any(is.na(dTrainCTreatedUnscaled)))
  expect_false(any(is.na(dTrainCTreatedScaled)))
  expect_false(any(is.na(slopeFrame$mean)))
  expect_false(any(is.infinite(slopeFrame$mean)))
  expect_true(max(abs(slopeFrame$mean))<=1.0e-8)
  expect_false(any(slopeFrame$badSlope & (slopeFrame$sig<1)))
})