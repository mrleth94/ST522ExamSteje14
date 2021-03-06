% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/densityEstimate.R
\name{densityEstimate}
\alias{densityEstimate}
\title{Estimated density}
\usage{
densityEstimate(x,d,h,method = "naive")
}
\arguments{
\item{x}{A numeric variable}

\item{d}{The point we want to evaluate the density at}

\item{h}{The bandwith}

\item{method}{Can be either naive or kernel}
}
\value{
Return the evalutated density at the point d
}
\description{
Calculate the density at the point d.
}
\examples{
densityEstimate(cars$dist,method = "naive")
}
\author{
Steffan Leth Jensen \cr
Department of mathematics and computer science (IMADA) \cr
University of southern Denmark, SDU, Odense \cr
\email{steje14@student.sdu.dk} \cr
}
\keyword{density}
\keyword{estimate}

