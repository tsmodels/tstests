#' Sample GARCH Forecast Data
#'
#' A pre-computed backtest of the SPY log returns data
#' using a GARCH(1,1)-JSU model (see details for replication code).
#' @details
#' The replication code for the backtest based 1-step ahead forecast distribution
#' is as follows:
#'
#' ```{r,eval=FALSE,echo=TRUE}
#' library(xts)
#' library(tsgarch)
#' data("spy", package = "tstests")
#' spyr <- na.omit(diff(log(spy)))
#' n <- NROW(spyr)
#' spec <- garch_modelspec(spyr, model = "garch", constant = T,
#' distribution = "jsu")
#' b <- tsbacktest(spec, start = (n - 250), end = n, h = 1, estimate_every = 30,
#' rolling = T, trace = T)
#' garch_forecast <- data.table(date = b$table$forecast_date,
#' actual = b$table$actual, forecast = b$table$mu, sigma = b$table$sigma,
#' skew = b$table$skew, shape = b$table$shape)
#' ```
#' @format ## `garch_forecast`
#' A data.table with 250 rows and 5 columns:
#' \describe{
#'   \item{date}{the forecast date}
#'   \item{actual}{the realized values}
#'   \item{forecast}{the forecast mu}
#'   \item{sigma}{the forecast sigma}
#'   \item{skew}{the estimated skew of the jsu distribution}
#'   \item{shape}{the estimated shape of the jsu distribution}
#' }
"garch_forecast"

#' Sample ARMA Forecast Data
#'
#' A pre-computed backtest of the SPY log returns data
#' using an ARMA(1,1)-JSU model (see details for replication code).
#' @details
#' The replication code for the backtest based 1-step ahead forecast distribution
#' is as follows:
#'
#' ```{r,eval=FALSE,echo=TRUE}
#' library(xts)
#' library(tsarma)
#' # from the tsmodels github repo
#' data("spy", package = "tstests")
#' spyr <- na.omit(diff(log(spy)))
#' n <- NROW(spyr)
#' spec <- arma_modelspec(spyr, order c(1,1), distribution = "jsu")
#' b <- tsbacktest(spec, start = (n - 250), end = n, h = 1, estimate_every = 30,
#' rolling = T, trace = T)
#' arma_forecast <- data.table(date = b$table$forecast_date,
#' actual = b$table$actual, forecast = b$table$mu, sigma = b$table$sigma,
#' skew = b$table$skew, shape = b$table$shape)
#' ```
#' @format ## `arma_forecast`
#' A data.table with 250 rows and 5 columns:
#' \describe{
#'   \item{date}{the forecast date}
#'   \item{actual}{the realized values}
#'   \item{forecast}{the forecast mu}
#'   \item{sigma}{the estimated sigma}
#'   \item{skew}{the estimated skew of the jsu distribution}
#'   \item{shape}{the estimated shape of the jsu distribution}
#' }
"arma_forecast"


#' SPY ETF Adjusted Close
#'
#' The adjusted closing price of the SPY ETF.
#'
#' @format ## `spy`
#' An xts vector with 7597 observations spanning the period 1993-01-29 / 2023-03-30
#' from Yahoo Finance.
"spy"
