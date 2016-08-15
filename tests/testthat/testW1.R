library('vtreat')

context("Variable Scoring")


test_that("testW1: test out of sample scoring defeats nested model bias", {
  # build data
  set.seed(235)
  zip <- paste('z',1:100)
  N = 1000
  d <- data.frame(zip=sample(zip,N,replace=TRUE),
                  zip2=sample(zip,N,replace=TRUE),
                  y=runif(N))
  del <- runif(length(zip))
  names(del) <- zip
  d$y <- d$y + del[d$zip2]
  d$yc <- d$y>=mean(d$y)

  # show good variable control on numeric/regression
  tN <- designTreatmentsN(d,c('zip','zip2'),'y',
                          verbose=FALSE,
                          rareCount=2,rareSig=0.5)
  dTN <- prepare(tN,d,pruneSig=0.01)
  expect_false('zip_catN' %in% colnames(dTN))
  expect_true('zip2_catN' %in% colnames(dTN))

  # show good variable control on categorization
  tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE,
                          verbose=FALSE,
                          rareCount=2,rareSig=0.5)
  dTC <- prepare(tC,d,pruneSig=0.01)
  expect_false('zip_catB' %in% colnames(dTC))
  expect_true('zip2_catB' %in% colnames(dTC))

  
  # show naive method has high correlations
  dTN <- prepare(tN,d,pruneSig=c())
  expect_true(cor(dTN$zip_catN,dTN$y)>0.2)
  
  dTC <- prepare(tC,d,pruneSig=c())
  expect_true(cor(as.numeric(dTC$yc),dTC$zip_catB)>0.2)
  
  # show cross table helps lower this
  cC <- mkCrossFrameCExperiment(d,c('zip','zip2'),'yc',TRUE,
                          rareCount=2,rareSig=0.5)
  expect_true(cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip_catB)<0.1)
  expect_true(cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip2_catB)>0.4)
                          
  # show cross table helps lower this
  cN <- mkCrossFrameNExperiment(d,c('zip','zip2'),'y',
                                rareCount=2,rareSig=0.5)
  expect_true(cor(cN$crossFrame$y,cN$crossFrame$zip_catN)<0.1)
  expect_true(cor(cN$crossFrame$y,cN$crossFrame$zip2_catN)>0.5)
})

