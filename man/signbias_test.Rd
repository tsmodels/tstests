% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signbias.R
\name{signbias_test}
\alias{signbias_test}
\title{Sign Bias Test}
\usage{
signbias_test(x, sigma = 1, ...)
}
\arguments{
\item{x}{a series representing the residuals of some estimated model.}

\item{sigma}{either a scalar representing the residuals standard deviation else
a vector of the same length as x representing the conditional standard deviation
of the residuals.}

\item{...}{additional arguments passed to \code{\link[car]{linearHypothesis}},
except the \dQuote{test} which is fixed to use the Chisq test.}
}
\value{
An object of class \dQuote{tstest.signbias} which has a print and
as_flextable method.
}
\description{
The sign bias test of Engle and Ng (1993).
}
\examples{
library(tsgarch)
library(tsdistributions)
library(xts)
data("spy")
spyr <- na.omit(diff(log(spy)))
spec <- garch_modelspec(spyr, model = "garch", order = c(1,1),
constant = TRUE, distribution = "jsu")
mod <- estimate(spec)
print(signbias_test(residuals(mod), sigma(mod)))

}
\references{
\insertRef{Engleng1993}{tstests}
}
