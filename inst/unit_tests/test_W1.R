
test_W1 <- function() {
  # build data
  set.seed(235)
  zip <- paste('z',1:100)
  N = 1000
  d <- data.frame(zip=sample(zip,N,replace=TRUE),  # no signal
                  zip2=sample(zip,N,replace=TRUE), # has signal
                  y=runif(N))
  del <- runif(length(zip))
  names(del) <- zip
  d$y <- d$y + del[d$zip2]
  d$yc <- d$y>=mean(d$y)

  # show good variable control on numeric/regression
  tN <- designTreatmentsN(d,c('zip','zip2'),'y',
                           rareCount=2,rareSig=0.5,
                          verbose=FALSE)
  dTN <- prepare(tN,d,pruneSig=0.01)
  RUnit::checkTrue(!('zip_catN' %in% colnames(dTN)))
  RUnit::checkTrue('zip2_catN' %in% colnames(dTN))

  # show good variable control on categorization
  tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE,
                          rareCount=2,rareSig=0.5,
                          verbose=FALSE)
  dTC <- prepare(tC,d,pruneSig=0.01)
  RUnit::checkTrue(!('zip_catB' %in% colnames(dTC)))
  RUnit::checkTrue('zip2_catB' %in% colnames(dTC))

  
  tC# show naive method has high correlations
  dTN <- prepare(tN,d,pruneSig=c())
  RUnit::checkTrue(cor(dTN$zip_catN,dTN$y)>0.1)
  
  dTC <- prepare(tC,d,pruneSig=c())
  RUnit::checkTrue(cor(as.numeric(dTC$yc),dTC$zip_catB)>0.1)
  
  # show cross table helps lower this
  cC <- mkCrossFrameCExperiment(d,c('zip','zip2'),'yc',TRUE,
                          rareCount=2,rareSig=0.5,
                          verbose = FALSE)
  RUnit::checkTrue(cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip_catB)<0.1)
  RUnit::checkTrue(cor(as.numeric(cC$crossFrame$yc),cC$crossFrame$zip2_catB)>0.1)
                          
  # show cross table helps lower this
  cN <- mkCrossFrameNExperiment(d,c('zip','zip2'),'y',
                                rareCount=2,rareSig=0.5,
                                verbose = FALSE)
  RUnit::checkTrue(cor(cN$crossFrame$y,cN$crossFrame$zip_catN)<0.1)
  RUnit::checkTrue(cor(cN$crossFrame$y,cN$crossFrame$zip2_catN)>0.1)
  
  invisible(NULL)
}

