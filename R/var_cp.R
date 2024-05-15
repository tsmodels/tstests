#' Value at Risk CP Test
#'
#' @description The value at risk coverage and duration tests of
#' Kupiec (1995) and Christoffersen and Pelletier (1998,2004).
#' @param actual a series representing the actual value of the series in the
#' out of sample period.
#' @param forecast the forecast values of the series at the quantile
#' given by alpha (the forecast value at risk).
#' @param alpha the quantile level used to calculate the forecast value at risk.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.var_cp} which has a print and
#' as_flextable method.
#' @details
#' The unconditional (Kupiec 1995) and conditional (Christoffersen and
#' Pelletier 1998) coverage tests evaluate the correctness and independence of
#' value at risk violations (failures), individually and jointly. Correctness
#' is measured in terms of the expected and actual violations of value at risk
#' for a given quantile and data size, whilst independence checks the clustering
#' of violations with past violations, which is key in determining whether a model
#' can accurately capture the higher order dynamics of a series.
#' The duration of time between value ar risk violations (no-hits) should ideally
#' be independent and not cluster. Under the null hypothesis of a correctly
#' specified risk model, the no-hit duration should have no memory. Since the
#' only continuous distribution which is memory free is the exponential, the
#' test can conducted on any distribution which embeds the exponential as a
#' restricted case, and a likelihood ratio test then conducted to see whether
#' the restriction holds. Following Christoffersen and Pelletier (2004), the
#' Weibull distribution is used with parameter \sQuote{b=1} representing the case of
#' the exponential.
#' @aliases var_cp_test
#' @references
#' \insertRef{Kupiec1995}{tstests}
#'
#' \insertRef{Christoffersen1998}{tstests}
#'
#' \insertRef{Christoffersen2004}{tstests}
#' @aliases var_cp_test
#' @examples
#' library(tsdistributions)
#' data("garch_forecast")
#' q <- qdist("jsu", p = 0.05, mu = garch_forecast$forecast, sigma = garch_forecast$sigma,
#' skew = garch_forecast$skew, shape = garch_forecast$shape)
#' var_cp_test(actual = garch_forecast$actual, forecast = q, alpha = 0.05)
#'
#' @rdname var_cp_test
#' @export
#'
#'
#'
var_cp_test <- function(actual, forecast, alpha, ...)
{
    if (missing(actual)) stop("\nactual is missing.")
    if (missing(forecast)) stop("\nforecast is missing.")
    if (missing(alpha)) stop("\nalpha is missing.")
    actual <- as.numeric(actual)
    forecast <- as.numeric(forecast)
    n_actual <- length(actual)
    n_forecast <- length(forecast)
    if (n_actual != n_forecast) stop("\nactual and forecast lengths must be equal.")

    coverage_test <- .coverage_test(actual = actual, q = forecast, alpha = alpha)
    duration_test <- .duration_test(actual, q = forecast, alpha)

    p_values <- c(coverage_test$p_value, duration_test$p_value)
    var_tab <- data.table("Test" = c("Kupiec (UC)", "CP (CCI)", "CP (CC)", "CP (D)"), "DoF" = c(1,1,2,1),
                      "Statistic" = c(coverage_test$uc_lr_stat, coverage_test$cci_lr_stat, coverage_test$cc_lr_stat,
                                  duration_test$lr_stat),
                      "Pr(>Chisq)" = p_values,
                      signif = pvalue_format(p_values))
    decision <- rep("Fail to Reject H0", 4)
    if (p_values[1] < 0.05) decision[1] <- "Reject H0"
    if (p_values[2] < 0.05) decision[2] <- "Reject H0"
    if (p_values[3] < 0.05) decision[3] <- "Reject H0"
    if (p_values[4] < 0.05) decision[4] <- "Reject H0"
    H0 <- "Unconditional(UC), Independent(CCI), Joint Coverage(CC) and Duration(D)"
    failures <- c(failures = coverage_test$N, expected = floor(alpha*coverage_test$TN))
    var_tab[,'Decision(5%)' := decision]
    references <- c(
        "Kupic, P. (1995), Techniques for verifying accuracy of risk measurement models, Journal of Derivatives, 3, 73--84.",
        "Christoffersen, P. (1998), Evaluating Interval Forecasts, International Economic Review, 39, 841--862.",
        "Christoffersen, P. and Pelletier, D. (2004), Backtesting value-at-risk: A duration-based approach, Journal of Financial Econometrics, 2(1), 84--108.")
    out <- list(table = var_tab,
                failures = failures,
                alpha = alpha,
                weibull_b = duration_test$b,
                nobs = n_actual,
                hypothesis = H0,  test_type = "Likelihood Ratio",
                p_value = p_values,
                distribution = "Chi-squared", symbols = NULL, test_class = "var_cp",
                test_name = "Value at Risk Tests (Christoffersen and Pelletier)",
                reference = references)
    class(out) <- c("tstest.var_cp","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.var_cp
#' @rdname print
#' @export
#'
#'
print.tstest.var_cp <- function(x, digits = max(3L, getOption("digits") - 3L),
                                   signif.stars = getOption("show.signif.stars"),
                                   include.decision = FALSE, ...)
{
    `Decision(5%)` <- signif <- NULL
    cat(x$test_name)
    cat("\nHypothesis(H0) :", x$hypothesis,"\n\n")
    tab <- copy(x$table)
    if (!include.decision) {
        tab[,`Decision(5%)` := NULL]
    }
    if (!signif.stars) {
        tab[,signif := NULL]
    } else {
        setnames(tab, "signif"," ")
    }
    tab <- as.data.frame(tab)
    rownames(tab) <- tab[,1]
    tab <- tab[,-1]
    print(tab, digits = digits)
    cat("\n---")
    if (signif.stars) {
        cat("\n")
        cat(signif_codes())
    }
    cat("\n\nCoverage\t:", x$alpha)
    cat("\nObs.\t\t:", x$nobs,"\n")
    cat("Failures\t:", x$failures[1],"\n")
    cat("E[Failures]\t:", x$failures[2],"\n")
    return(invisible(x))
}


#' @aliases as_flextable.tstest
#' @method as_flextable tstest.var_cp
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.var_cp <- function(x, digits = max(3L, getOption("digits") - 3L),
                                          signif.stars = getOption("show.signif.stars"),
                                          include.decision = FALSE,
                                          table.caption = x$test_name,
                                          footnote.reference = FALSE, ...)
{
    `Decision(5%)` <- NULL
    if (is.null(table.caption)) table.caption <- x$test_name
    tab <- copy(x$table)
    cnames <- colnames(tab)
    if (!include.decision) {
        tab[,`Decision(5%)` := NULL]
        cnames <- colnames(tab)
    }
    if (!signif.stars) {
        tab[,signif := NULL]
        cnames <- colnames(tab)
    }
    tab <- as.data.frame(tab)
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "Test", align = "left")
    if (signif.stars) {
        out <- out |> align(j = "signif", align = "left") |>
            bold(j = "signif", bold = TRUE) |>
            padding(padding.left = 0, j = "signif", part  = "all") |>
            set_header_labels(signif = "")
        out <- out |> add_footer_lines(values = signif_codes())
    }
    out <- out |> colformat_int(j = "DoF", prefix = "# ")
    .text <- paste0("Coverage: ", x$alpha, ", Obs: ", x$nobs, ", Failures: ", x$failures[1], ", E[Failures]: ", x$failures[2])
    out <- out |> add_footer_lines(top = FALSE, values = .text)
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = c("References: ", x$reference))
    }
    out <- colformat_double(out, j = c("Statistic", "Pr(>Chisq)"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}



.coverage_test <- function(actual, q, alpha)
{
    failures <- as.numeric(ifelse(actual < q, 1, 0))
    # number of exceedances/failures
    N <- sum(failures)
    # length of series
    TN <- length(failures)
    # Thanks to Kurt Hornik for the simplifications and fix
    tab <- table(head(failures, -1), tail(failures, -1))
    N00 <- tab[1,1]
    N11 <- tab[2,2]
    N01 <- tab[1,2]
    N10 <- tab[2,1]
    # p01: Probability of having a failure at time t | no failure at t-1
    p01 <- N01/(N00 + N01)
    # p11: Probability of having a failure at time t | failure at t-1
    p11 <- N11/(N10 + N11)
    # pn : unconditional probability of a failure
    pn <- (N01 + N11)/sum(tab)
    res <- log(1 - pn) * (N00 + N10) + log(pn) * (N01 + N11)
    unr <- log(1 - p01) * N00  + log(p01) * (N01) + log(1 - p11) * (N10) + log(p11) * (N11)
    cci_lr_stat <- -2 * (res - unr)
    uc_lr_stat <- .lr_unc_coverage(TN, N, alpha)
    if (is.nan(cci_lr_stat)) cci_lr_stat <- 1e10
    cc_lr_stat <- cci_lr_stat + uc_lr_stat
    p_value <- c(1 - pchisq(uc_lr_stat,  df = 1),
                  1 - pchisq(cci_lr_stat,  df = 1),
                  1 - pchisq(cc_lr_stat,  df = 2))
    return(list(cc_lr_stat = cc_lr_stat, cci_lr_stat = cci_lr_stat, uc_lr_stat = uc_lr_stat,
                p_value = p_value, N = N, TN = TN))
}

.lr_unc_coverage <- function(TN, N, alpha)
{
    res <- log(1 - alpha) * (TN - N) + log(alpha) * N
    unr <- log(1 - N/TN) * (TN - N) + log(N/TN) * N
    stat <- -2 * (res - unr)
    if (is.nan(stat)) stat <- 1e10
    return(stat)
}

.duration_test <- function(actual, q, alpha)
{
    failures <- ifelse(actual < q, 1, 0)
    N <- sum(failures)
    TN <- length(failures)
    D <- diff(which(failures == 1))
    C <- rep(0, length(D))
    # left-censored
    if (failures[1] == 0) {
        C = c(1, C)
        # the number of days until we get the first hit
        D <- c(which(failures == 1)[1], D)
    }
    # right-censored
    if (failures[TN] == 0) {
        C <- c(C, 1)
        # the number of days after the last one in the hit sequence
        D <- c(D, TN - tail(which(failures == 1), 1))
    }
    N <- length(D)
    sol <- try(optim(par = 2, fn = .lik_duration_weibull, gr = NULL, D = D, C = C, N = N,
                     method = "L-BFGS-B", lower = 0.001, upper = 10, control = list(trace = 0)), silent = TRUE)
    b <- sol$par
    unr_loglik <- -sol$value
    res_loglik <- -.lik_duration_weibull(1, D, C, N)
    lr_stat <- -2 * (res_loglik - unr_loglik)
    p_value <- 1 - pchisq(lr_stat, 1)
    H0 <- "Duration Between Exceedances have no memory (Weibull b=1 = Exponential)"
    return(list(b = b, unr_loglik = unr_loglik, res_loglik = res_loglik, lr_stat = lr_stat, p_value = p_value, H0 = H0))
}


.lik_duration_weibull <- function(pars, D, C, N){
    b <- pars[1]
    a <- ( (N - C[1] - C[N])/(sum(D^b)) )^(1/b)
    lik <- C[1] * log(.pweibull(D[1],a,b,survival = TRUE)) + (1 - C[1]) * .dweibull(D[1], a, b, log = TRUE) +
        sum(.dweibull(D[2:(N - 1)], a, b, log = TRUE)) + C[N] * log(.pweibull(D[N], a, b, survival = TRUE))  +
        (1 - C[N]) * .dweibull(D[N], a, b, log = TRUE)
    if (!is.finite(lik) || is.nan(lik)) lik <- 1e10 else lik <- -lik
    return(lik)
}

# When b=1 we get the exponential
.dweibull <- function(D, a, b, log = FALSE) {
    # density of Weibull
    pdf <- b * log(a) + log(b) + (b - 1) * log(D) - (a * D)^b
    if (!log) pdf <- exp(pdf)
    return(pdf)
}

.pweibull <- function(D, a, b, survival = FALSE) {
    # distribution of Weibull
    cdf <- 1 - exp(-(a*D)^b)
    if (survival) cdf <- 1 - cdf
    return(cdf)
}
.hweibull <- function(D, a, b) {
    # hazard of Weibull
    h <- (a^b) * b * (D^(b - 1))
    return(h)
}

