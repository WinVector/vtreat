library('vtreat')

context("Defensive Coding")

test_that("Protect from odd columns types (and warn)", {
  op <- options(warn = (-1)) # suppress warnings 
  d <- data.frame(xInteger=1:3,
                  xNumeric=0,
                  xCharacter='a',
                  xFactor=as.factor('b'),
                  xPOSIXct=Sys.time(),
                  xRaw=raw(3),
                  xLogical=TRUE,
                  xArrayNull=as.array(list(NULL,NULL,NULL)),
                  stringsAsFactors=FALSE)
  d$xPOSIXlt <- as.POSIXlt(c(Sys.time(),Sys.time()+100,Sys.time()+200))
  d$xArray <- as.array(c(17,18,19))
  d$xMatrix1 <- matrix(data=c(88,89,90),nrow=3,ncol=1)
  d$xMatrix2 <- matrix(data=c(1,2,3,4,5,6),nrow=3,ncol=2)
  d$xMatrixC1 <- matrix(data=c('88','89','90'),nrow=3,ncol=1)
  d$xMatrixC2 <- matrix(data=c('1','2','3','4','5','6'),nrow=3,ncol=2)
  d$xListH <- list(10,20,'thirty')
  d$xListR <- list(list(),list('a'),list('a','b'))
  d$xData.Frame <- data.frame(xData.FrameA=6:8,xData.FrameB=11:13)
  d$xListR2 <- I(list(NULL,'a',c('a','b')))
  d$xFunctions=I(c(function(){},function(){},function(){}))
  d$y <- c(1,1,0)
  yVar <- 'y'
  yTarget <- 1
  xVars <- setdiff(colnames(d),yVar)
  treatmentsC <- designTreatmentsC(d,xVars,yVar,yTarget,verbose=FALSE)
  treatmentsN <- designTreatmentsN(d,xVars,yVar,verbose=FALSE)

  options(op) # restore settings
})
