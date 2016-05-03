library('vtreat')

context("Significance")

test_that("testSig: test basic significance calculations", {
  d <- data.frame(x=c(1,3,2,1),
                  y=c(1,2,2,0),
                  yC=c(TRUE,TRUE,FALSE,FALSE))
  # summary(lm(y~x,data=d))
  # sL <- vtreat:::linScore('x',d$x,d$y,NULL)
  # expect_true(abs(sL$sig-0.1818)<=1.0e-3)
  tL <- designTreatmentsN(d,'x','y',verbose=FALSE)
  expect_true(abs(tL$scoreFrame[1,'sig']-0.1818)<=1.0e-3)
  
  # model <- glm(yC~x,data=d,family=binomial)
  # delta_deviance <- model$null.deviance - model$deviance
  # delta_df <- model$df.null - model$df.residual
  # pRsq <- 1.0 - model$deviance/model$null.deviance
  # sig <- stats::pchisq(delta_deviance, delta_df,lower.tail=FALSE)
  # sC <- vtreat:::catScore('x',d$x,d$yC,TRUE,NULL)
  # expect_true(abs(sC$sig-0.5412708)<=1.0e-3)
  tC <- designTreatmentsC(d,'x','yC',TRUE,verbose=FALSE)
  expect_true(abs(tC$scoreFrame[1,'sig']-0.5412708)<=1.0e-3)
})

  