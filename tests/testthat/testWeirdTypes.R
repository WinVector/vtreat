library('vtreat')

context("Defensive Coding")

test_that("Protect from odd columns types (and warn)", {
  d <- data.frame(xInteger=1:3,
                  xNumeric=0,
                  xCharacter='a',
                  xFactor=as.factor('b'),
                  xPOSIXct=Sys.time(),
                  xRaw=raw(3),
                  xLogical=TRUE,
                  xArrayNull=as.array(list(NULL,NULL,NULL)),
                  stringsAsFactors=FALSE)
  d$xPOSIXlt <- as.POSIXlt(Sys.time())
  d$xArray <- as.array(c(7,7,7))
  d$xMatrix <- matrix(data=-1,nrow=3,ncol=2)
  d$xListH <- list(10,20,'thirty')
  d$xListR <- list(list(),list('a'),list('a','b'))
  d$xData.Frame <- data.frame(xData.FrameA=6:8,xData.FrameB=11:13)
  d$xListR2 <- I(list(NULL,'a',c('a','b')))
  d$xFunctions=I(c(function(){},function(){},function(){}))
  d$y <- c(1,1,0)
  yVar <- 'y'
  yTarget <- 1
  xVars <- setdiff(colnames(d),yVar)
  treatmentsC <- designTreatmentsC(d,xVars,yVar,yTarget)
  dCTreated <- prepare(treatmentsC,d,pruneLevel=NULL)
  treatmentsN <- designTreatmentsN(d,xVars,yVar)
  dNTreated <- prepare(treatmentsN,d,pruneLevel=NULL)
})
