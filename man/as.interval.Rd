% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmi_smallfunctions.R
\name{as.interval}
\alias{as.interval}
\title{Function to transform objects into an interval object}
\usage{
as.interval(x, sna = FALSE)
}
\arguments{
\item{x}{An object to transform.
Currently the function can transform numeric vectors and characters}

\item{sna}{Boolean: if \code{TRUE}, \code{NA}s are kept as standard \code{NA}s.
Otherwise they are turned into \code{"-Inf;Inf"}.}
}
\value{
A vector of class \code{interval}.
}
\description{
Function to transform numeric (or character) vectors or n times 2 matrices into an interval object
}
\examples{
as.interval(c("1000;2000", "700;700", NA))
}
\seealso{
\link[hmi]{generate_interval}
}
