\name{designTreatmentsN}
\alias{designTreatmentsN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{designTreatmentsN
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
designTreatmentsN(dframe, varlist, outcomename,
   weights=c(),
   minFraction=0.02, smFactor=0.0, maxMissing=0.04)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dframe}{
%%     ~~Describe \code{dframe} here~~
}
  \item{varlist}{
%%     ~~Describe \code{varlist} here~~
}
  \item{outcomename}{
%%     ~~Describe \code{outcomename} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dTrainN <- data.frame(x=c('a','a','a','a','b','b','b'),z=c(1,2,3,4,5,6,7),y=c(0,0,0,1,0,1,1))
dTestN <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),'y')
dTrainNTreated <- prepare(treatmentsN,dTrainN)
dTestNTreated <- prepare(treatmentsN,dTestN)
}
