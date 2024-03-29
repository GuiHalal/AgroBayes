% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_bn_test.R
\name{testCreateNetworks}
\alias{testCreateNetworks}
\title{TEST FUNCTION -  Creates Bayesian networks for performance evaluation}
\usage{
testCreateNetworks(areaphase, areatype)
}
\arguments{
\item{areaphase}{the dataframe to be used.}

\item{areatype}{integer to identify which type
of area the dataframe belongs to.}
}
\value{
Network evaluation metrics,
as calculated in the \code{\link{testValidateNetwork}} function.
}
\description{
Using harvest data from a phenological phase of the cultivar,
from a specific area of the plantation, Bayesian network are generated
(using the \code{bnlearn::hc} and \code{bnlearn::mmhc} functions),
trained (using the \code{bnlearn::bn.fit} function)
and evaluated for performance (using \code{\link{testRunNetworks}}).
Four networks are created, two from the pre-established topology and
two learned only from the presented data.
}
\examples{
\donttest{
areaphase <- data.frame(area1_phase_1)
metricsArea1Phase1 <- testCreateNetworks (areaphase, 1)}
}
\keyword{internal}
