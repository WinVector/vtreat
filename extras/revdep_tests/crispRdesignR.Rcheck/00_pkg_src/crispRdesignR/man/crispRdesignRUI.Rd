\name{crispRdesignRUI}
\alias{crispRdesignRUI}
\title{UI caller for crispRdesignR}
\description{
Activates the shiny UI for the crispRdesignR package
}
\usage{
crispRdesignRUI(max_gtf_size = 150)
}
\arguments{
  \item{max_gtf_size}{
    The maximum size (in MB) of the geneome annotation file (.gtf) that can be used with the shiny App. By default this is set to 150.
  }
}
\value{
No return value, called to initiate user interface.
}
\author{
Dylan Beeber
}
\examples{
requireNamespace("gbm", quietly = TRUE)
requireNamespace("Biostrings", quietly = TRUE)
if (interactive()) {
  crispRdesignRUI()
  }
}
