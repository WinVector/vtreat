## ------------------------------------------------------------------------
signk <- function(n,k) {
  sigTab <- data.frame(y=c(rep(TRUE,n/2),rep(FALSE,n/2)),v=FALSE)
  sigTab[seq_len(k),'v'] <- TRUE
  vtreat::designTreatmentsC(sigTab,'v','y',TRUE,verbose=FALSE)$scoreFrame[1,'sig']
}
sigTab <- data.frame(k=c(1,2,3,4,5,10,20,50,100))
# If you want to see a rare but perfect indicator of positive class
# that's only on k times out of 1000, this is the lower bound on pruneSig
sigTab$sigEst = vapply(sigTab$k,function(k) signk(1000,k),numeric(1)) 
sigTab$minusLogSig = -log(sigTab$sigEst) # we expect this to be approximately k
print(sigTab)

## ------------------------------------------------------------------------
set.seed(3346)
n <- 1000
k <- 4
d <- data.frame(y=rbinom(n,size=1,prob=0.5)>0)
d$catVarNoise <- rep(paste0('lev',sprintf("%03d",1:floor(n/k))),(k+1))[1:n]
d$catVarPerfect <- paste0(d$catVar,substr(as.character(d$y),1,1))
d <- d[order(d$catVarPerfect),]
head(d)

treatmentsC <- vtreat::designTreatmentsC(d,c('catVarNoise','catVarPerfect'),'y',TRUE)


# Estimate effect significance (not coeficient significance).
estSigGLM <- function(xVar,yVar,numberOfHiddenDegrees=0) {
  d <- data.frame(x=xVar,y=yVar,stringsAsFactors = FALSE)
  model <- stats::glm(stats::as.formula('y~x'),
                      data=d,
                      family=stats::binomial(link='logit'))
  delta_deviance <- model$null.deviance - model$deviance
  delta_df <- model$df.null - model$df.residual + numberOfHiddenDegrees
  pRsq <- 1.0 - model$deviance/model$null.deviance
  sig <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
  sig
}

prepD <- vtreat::prepare(treatmentsC,d,pruneSig=c())

## ----scoreframe----------------------------------------------------------
print(treatmentsC$scoreFrame[c('varName','origName','sig')])

## ----scoresignal---------------------------------------------------------
summary(glm(y~d$catVarPerfect=='lev001T',data=d,family=binomial))
estSigGLM(prepD$catVarPerfect_catB,prepD$y,0) # wrong est
estSigGLM(prepD$catVarPerfect_catB,prepD$y,
          numberOfHiddenDegrees=length(unique(d$catVarPerfect))-1)

## ----scorenoise----------------------------------------------------------
summary(glm(y~d$catVarNoise=='lev001',data=d,family=binomial))
estSigGLM(prepD$catVarNoise_catB,prepD$y,0) # wrong est
estSigGLM(prepD$catVarNoise_catB,prepD$y,
          numberOfHiddenDegrees=length(unique(d$catVarNoise))-1)

