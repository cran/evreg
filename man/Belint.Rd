% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Belint.R
\name{Belint}
\alias{Belint}
\title{Finds a belief interval centered on mu for a Gaussian random fuzzy number}
\usage{
Belint(level = 0.9, GRFN)
}
\arguments{
\item{level}{The specified degree of belief (between 0 and 1).}

\item{GRFN}{A Gaussian random fuzzy number, encoded as a list with components mu, sig
and h.}
}
\value{
A vector containing the lower and upper bounds of the interval.
}
\description{
\code{Belint} find an interval of the form [mu-r,mu+r] with specified degree of belief
for a Gaussian random fuzzy number.
}
\examples{
int<-Belint(0.9,list(mu=2,sig=1,h=2))
print(int)
}
\references{
Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
Pages 1-36, 2023.
}
\seealso{
\code{\link{Bel}}, \code{\link{Pl}}, \code{\link{pl_contour}}
}
