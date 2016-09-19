## ------------------------------------------------------------------------
signk <- function(n,k) {
  d <- data.frame(y=c(rep(TRUE,n/2),rep(FALSE,n/2)),v=FALSE)
  d[seq_len(k),'v'] <- TRUE
  vtreat::designTreatmentsC(d,'v','y',TRUE,verbose=FALSE)$scoreFrame[1,'sig']
}
d <- data.frame(k=c(1,2,3,4,5,10,20,50,100))
# If you want to see a rare but perfect indicator of positive class
# that's only on k times out of 1000, this is the lower bound on pruneSig
d$sigEst = vapply(d$k,function(k) signk(1000,k),numeric(1)) 
d$minusLogSig = -log(d$sigEst) # we expect this to be approximately k
print(d)

