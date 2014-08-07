\name{designTreatmentsC}
\alias{designTreatmentsC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{designTreatmentsC
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
designTreatmentsC(dframe, varlist, outcomename, outcometarget,
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
  \item{outcometarget}{
%%     ~~Describe \code{outcometarget} here~~
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
dTrainC <- data.frame(x=c('a','a','a','b','b','b'),z=c(1,2,3,4,5,6),y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),z=c(10,20,30,NA))
treatmentsC <- designTreatmentsC(dTrainC,colnames(dTrainC),'y',TRUE)
dTrainCTreated <- prepare(treatmentsC,dTrainC)
dTestCTreated <- prepare(treatmentsC,dTestC)
}