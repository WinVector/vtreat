\name{vtreat-package}
\alias{vtreat-package}
\alias{vtreat}
\docType{package}
\title{
vtreat: a package for simple variable treatment
}
\description{
vtreat is a package that treats variables so that models can be used
in production. Common problems vtreat defends against: NA, too many categorical
values, rare categorical values, new categorical values (levels seen during application,
but not during training).   \code{\link{prepare}} should be used as you would
use model.matrix.  \code{\link{prepare}} treated variables are all numeric and never
take the value NA (so are very safe to use in modeling).
}
\details{
\tabular{ll}{
Package: \tab vtreat\cr
Type: \tab Package\cr
Version: \tab 0.2.09.0007\cr
Date: \tab 2015-01-31\cr
License: \tab GNU General Public License version 3\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
John Mount and Nina Zumel

Maintainer: <contact@win-vector.com>
}
\references{
See: 
http://www.win-vector.com/blog/2014/06/r-minitip-dont-use-data-matrix-when-you-mean-model-matrix/
http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/
http://practicaldatascience.com/
}

\keyword{ package }
\seealso{
First build a list of variable treatments from your training data using
\code{\link{designTreatmentsC}} (for models predicting binary categorical outcomes)
or \code{\link{designTreatmentsN}} (for models predicting numeric outcomes).
Then apply a list of treatments to a data frame to get a treated data frame using
\code{\link{prepare}}.  All the code is assuming we are working only with rows
where the outcome or y-value is not NA, finite and not nan.  Categoric examples are
in "probability space" (computing expectations of a 0/1 outcome).  For modeling some work you may
want to first transform columns into logistic space with the by setting the \code{logitTransform=TRUE} 
option in \code{\link{prepare}}.
}
\examples{
# categorical example
dTrainC <- data.frame(x=c('a','a','a','b','b',NA),
   z=c(1,2,3,4,NA,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneLevel=c(),scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
# all slopes should be 1
sapply(varsC,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})
dTestCTreated <- prepare(treatmentsC,dTestC,pruneLevel=c(),scale=TRUE)
# logit categorical example
dTrainLTreated <- prepare(treatmentsC,dTrainC,pruneLevel=c(),scale=TRUE,logitTransform=TRUE)
dTestLTreated <- prepare(treatmentsC,dTestC,pruneLevel=c(),scale=TRUE,logitTransform=TRUE)

# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA),
   z=c(1,2,3,4,5,NA,7),y=c(0,0,0,1,0,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneLevel=c(),scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
# all slopes should be 1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
dTestNTreated <- prepare(treatmentsN,dTestN,pruneLevel=c(),scale=TRUE)

# for large data sets you can consider designing the treatments on 
# a subset like: d[sample(1:dim(d)[[1]],1000),]
}
