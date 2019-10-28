\name{SIC-codes}
\alias{SIC.codes-package}
\alias{SIC.codes}
\alias{SIC}
\alias{sic_description}
\alias{description_sic}
\title{
  Look Up and Search for SIC Codes
}
\description{
  Look up and search for SIC codes.
}
\usage{
description_sic(pattern, ..., ignore.case = TRUE)

sic_description(s, ...)
}
\arguments{
  \item{s}{
    character
}
  \item{\dots}{
    other arguments
}
}
\details{
  Experimental
}
\value{
  character
}
\references{
  SEC site
}
\author{
  Enrico Schumann
}
\seealso{
  add other packages
}
\examples{
sic_description(c("9198", "9199", "9999"))
sic_description(c("9198", "9199", "9999"), type = "division")
}
