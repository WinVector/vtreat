library('vtreat')

context("Defensive Coding")

test_that("testWeirdTypes: Protect from odd columns types (and warn)", {
  suppressWarnings({
  d <- data.frame(xInteger=1:4,
                  xNumeric=0,
                  xCharacter='a',
                  xFactor=as.factor('b'),
                  xPOSIXct=Sys.time(),
                  xRaw=raw(4),
                  xLogical=TRUE,
                  xArrayNull=as.array(list(NULL,NULL,NULL,NULL)),
                  stringsAsFactors=FALSE)
  d$xPOSIXlt <- as.POSIXlt(c(Sys.time(),Sys.time()+100,Sys.time()+200,Sys.time()+300))
  d$xArray <- as.array(c(17,18,19,20))
  d$xMatrix1 <- matrix(data=c(88,89,90,91),nrow=4,ncol=1)
  d$xMatrix2 <- matrix(data=c(1,2,3,4,5,6,7,8),nrow=4,ncol=2)
  d$xMatrixC1 <- matrix(data=c('88','89','90','91'),nrow=4,ncol=1)
  d$xMatrixC2 <- matrix(data=c('1','2','3','4','5','6','7','8'),nrow=4,ncol=2)
  d$xListH <- list(10,20,'thirty','forty')
  d$xListR <- list(list(),list('a'),list('a','b'),list('a','b','c'))
  d$xData.Frame <- data.frame(xData.FrameA=6:9,xData.FrameB=11:14)
  d$xListR2 <- I(list(NULL,'a',c('a','b'),1))
  d$xFunctions=I(c(function(){},function(){},function(){},function(){}))
  d$y <- c(1,1,0,0)
  yVar <- 'y'
  yTarget <- 1
  xVars <- setdiff(colnames(d),yVar)
  treatmentsC <- designTreatmentsC(d,xVars,yVar,yTarget,verbose=FALSE)
  treatmentsN <- designTreatmentsN(d,xVars,yVar,verbose=FALSE)
  })
})
