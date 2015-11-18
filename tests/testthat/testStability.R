library('vtreat')

context("Test Score Stability")

test_that("testStability: Stability of estimates", {
  expandTab <- function(tab) {
    # expand out into data
    d <- c()
    for(vLevelI in seq_len(nrow(tab))) {
      for(yLevelI in seq_len(ncol(tab))) {
        count <- tab[vLevelI,yLevelI]
        if(count>0) {
          di <- data.frame(x=character(count),
                           y=logical(count))
          di$x <- rownames(tab)[vLevelI]
          di$y <- as.logical(colnames(tab)[yLevelI])
          d <- rbind(d,di)
        }
      }
    }
    d
  }
  
#   # table describing data
#   tab <- matrix(data=c(1131,583,6538,2969,136,78),
#                 byrow=TRUE,ncol=2)
#   rownames(tab) <- c('1','2','unknown')
#   colnames(tab) <- c(FALSE,TRUE)
#   #print(tab)
#   d <- expandTab(tab)
#   #print(table(d)) # should match tab
#   tP <- vtreat::designTreatmentsC(d,'x','y',TRUE,rareSig=1,verbose=FALSE)
  
  # print(tp$scoreFrame) # why did "unknown" not show up?
  tab <- matrix(
    data =  c(
      202,89,913,419,498,214,8,0,3,0,
      1260,651,70,31,24,4,225,107,1900,
      921,1810,853,10,1,778,282,104,58
    ),
    byrow = TRUE,ncol = 2
  )
  rownames(tab) <-
    c(
      'Beige', 'Blau', 'Braun', 'Gelb', 'Gold', 'Grau', 'Grün', 'Orange',
      'Rot', 'Schwarz', 'Silber', 'Violett', 'Weiß', 'unknown'
    )
  colnames(tab) <- c(FALSE,TRUE)
  d <- expandTab(tab)
  d$x[d$x!='Weiß'] <- 'unknown'
  nRun <- 5
  set.seed(235235)
  # vtreat run: max arount 0.5 min ~ 5e-5
  csig <- numeric(nRun)
  for(i in seq_len(nRun)) {
    tP <- vtreat::designTreatmentsC(d,'x','y',TRUE,rareSig=1,verbose=FALSE)
    # looking at instability in csig of WeiB level
    csig[[i]] <- tP$scoreFrame$csig[tP$scoreFrame$varName=='x_lev_x.Weiß']
  }
  expect_true((max(csig)-min(csig))<1.0e-5)
#   # direct run same instability max ~ 0.5, min ~ 0.007
#   dsig <- numeric(nRun)
#   for(i in seq_len(nRun)) {
#     dsub <- d[sample(nrow(d),2859),]
#     model <- stats::glm(stats::as.formula('y~x=="Weiß"'),
#                         data=dsub,
#                         family=stats::binomial(link='logit'))
#     if(model$converged) {
#       delta_deviance = model$null.deviance - model$deviance
#       delta_df = model$df.null - model$df.residual
#       sig <- 1.0
#       pRsq <- 1.0 - model$deviance/model$null.deviance
#       if(pRsq>0) {
#         dsig[[i]] <- stats::pchisq(delta_deviance, delta_df, lower.tail=FALSE)
#       }
#     }
#   }
 })