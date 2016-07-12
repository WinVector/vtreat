\name{vtreat-package}
\alias{vtreat-package}
\alias{vtreat}
\docType{package}
\title{
vtreat: a package for simple variable treatment
}
\description{
vtreat is a data.frame processor/conditioner that prepares real-world data for predictive modeling in a statistically sound manner.
    Prepares variables so that data has fewer exceptional cases, making
    it easier to safely use models in production.

Common problems vtreat defends against: NA, too many categorical
values, rare categorical values, new categorical values (levels seen during application,
but not during training).   \code{\link{prepare}} should be used as you would
use model.matrix.  \code{\link{prepare}} treated variables are all numeric and never
take the value NA (so are very safe to use in modeling).

To use: first build a list of variable treatments from your training data using
\code{\link{designTreatmentsC}} (for models predicting binary categorical outcomes)
or \code{\link{designTreatmentsN}} (for models predicting numeric outcomes).
If you have enough data we suggested running the design step on a subset of data disjoint from training and test (this avoids many issues including mis-counting degrees of freedom on effect or impact codes arising from categorical variables with a large number of levels).
Then apply a list of treatments to a data frame to get a treated data frame using
\code{\link{prepare}}.  All the code is assuming we are working only with rows
where the outcome or y-value is not NA, finite and not nan.
}
\details{
\tabular{ll}{
Package: \tab vtreat\cr
Type: \tab Package\cr
Version: \tab 0.5.26\cr
Date: \tab 2016-07-10\cr
License: \tab GNU General Public License version 3\cr
}
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
  "Effect codes" in Cohen et. al. Applied multiple regression/correlation for the behavioral sciences.
}

\keyword{ package }
\seealso{
For more information: 
\itemize{
  \item \code{vignette(package='vtreat')}
  \item \code{RShowDoc('vtreat',package='vtreat')}
  \item \code{RShowDoc('vtreatVariableTypes',package='vtreat')}
  \item \code{RShowDoc('vtreatOverfit',package='vtreat')}
  \item Website: \url{https://github.com/WinVector/vtreat}
  }
}
\examples{
# categorical example
dTrainC <- data.frame(x=c('a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,NA,6,NA),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
dTrainCTreated <- prepare(treatmentsC,dTrainC,pruneSig=1.0,scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated),'y')
# all input variables should be mean 0 for significant variables
sapply(dTrainCTreated[,varsC,drop=FALSE],mean)
# all non NA slopes should be 1
sapply(varsC,function(c) { glm(paste('y',c,sep='~'),family=binomial,
   data=dTrainCTreated)$coefficients[[2]]})
dTestCTreated <- prepare(treatmentsC,dTestC,pruneSig=c(),scale=TRUE)

# numeric example
dTrainN <- data.frame(x=c('a','a','a','a','b','b',NA,NA),
   z=c(1,2,3,4,5,NA,7,NA),y=c(0,0,0,1,0,1,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
dTrainNTreated <- prepare(treatmentsN,dTrainN,pruneSig=1.0,scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated),'y')
# all input variables should be mean 0
sapply(dTrainNTreated[,varsN,drop=FALSE],mean) 
# all non NA slopes should be 1
sapply(varsN,function(c) { lm(paste('y',c,sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
dTestNTreated <- prepare(treatmentsN,dTestN,pruneSig=c(),scale=TRUE)

# for large data sets you can consider designing the treatments on 
# a subset like so: d[sample(1:dim(d)[[1]],1000),,drop=FALSE]
}
