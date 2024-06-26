% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dac.R
\name{dac_test}
\alias{dac_test}
\title{Directional Accuracy Tests}
\usage{
dac_test(actual, forecast, ...)
}
\arguments{
\item{actual}{a series representing the actual value of the series in the
out of sample period.}

\item{forecast}{the forecast values of the series in the out of sample period.}

\item{...}{not currently used.}
}
\value{
An object of class \dQuote{tstest.dac} which has a print and
as_flextable method.
}
\description{
The directional accuracy test of Pesaran and Timmermann (1992),
and excess profitability test of Anatolyev and Gerko (2005).
}
\details{
The null hypothesis for the test of Pesaran and Timmermann (1992) is
that the actual and predicted are independent (no sign predictability), whereas
the test of Anatolyev and Gerko (2005) measures the significance of the excess
profitability under the null hypothesis of no excess excess profitability. Both
are Hausman type tests asymptotically distributed as standard Normal.
}
\note{
The test will not work with constant forecasts.
}
\examples{
data(arma_forecast)
print(dac_test(arma_forecast$actual, arma_forecast$forecast))

}
\references{
\insertRef{Pesaran1992}{tstests}

\insertRef{Anatolyev2005}{tstests}
}
