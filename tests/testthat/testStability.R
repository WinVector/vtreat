library('vtreat')

context("Test Score Stability")

test_that("testStability: Stability of estimates", {
  set.seed(235235)
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
      'Beige', 'Blau', 'Braun', 'Gelb', 'Gold', 'Grau', 'Grun', 'Orange',
      'Rot', 'Schwarz', 'Silber', 'Violett', 'Weiss', 'unknown'
    )
  colnames(tab) <- c(FALSE,TRUE)
  d <- expandTab(tab)
  d$x[d$x!='Weiss'] <- 'unknown'
  nRun <- 5
  set.seed(235235)
  # vtreat run: max arount 0.5 min ~ 5e-5
  csig <- numeric(nRun)
  for(i in seq_len(nRun)) {
    tP <- vtreat::designTreatmentsC(d,'x','y',TRUE,rareSig=1,verbose=FALSE)
    # looking at instability in csig of Weiss level
    csig[[i]] <- tP$scoreFrame$csig[tP$scoreFrame$varName=='x_lev_x.Weiss']
  }
  expect_true((max(csig)-min(csig))<1.0e-5)
 })
