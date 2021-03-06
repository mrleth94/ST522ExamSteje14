% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi.probability.R
\name{chi.probability}
\alias{chi.probability}
\title{Chi probability}
\usage{
chi.probability(x,df=1,n=10)
}
\arguments{
\item{x}{The value that we will estimate about}

\item{df}{Degrees of freedom}

\item{n}{Number of generated random variables}
}
\value{
Return the probability that the chi-squared distribution is greater than the value x.
}
\description{
Calcutate the probability that the chisquared distribution is larger than a number x.
}
\examples{
chi.probability(4,19,1000)
}
\author{
Steffan Leth Jensen \cr
Department of mathematics and computer science (IMADA) \cr
University of southern Denmark, SDU, Odense \cr
\email{steje14@student.sdu.dk} \cr
}
\keyword{chi}
\keyword{chisquare}
\keyword{probability}

