set.seed(23525)
zip <- paste('z',1:400)
N = 1000
d <- data.frame(zip=sample(zip,N,replace=TRUE),
   zip2=sample(zip,N,replace=TRUE),
   y=runif(N))
del <- runif(length(zip))
names(del) <- zip
d$y <- d$y + del[d$zip2]
d$yc <- d$y>=mean(d$y)
library(vtreat)
tN <- designTreatmentsN(d,c('zip','zip2'),'y')
print(tN$varScores)
tC <- designTreatmentsC(d,c('zip','zip2'),'yc',TRUE)
print(tC$varScores)

