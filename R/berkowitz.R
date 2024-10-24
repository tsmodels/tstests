#' Berkowitz Forecast Density Test
#'
#' @description The forecast density test of Berkowitz (2001).
#' @param x a series representing the PIT transformed actuals given the forecast
#' values.
#' @param lags the number of autoregressive lags (positive and greater than 0).
#' @param ... additional arguments passed to the arima function which estimates
#' the unrestricted model.
#' @returns An object of class \dQuote{tstest.berkowitz} which has a print and
#' as_flextable method.
#' @aliases berkowitz_test
#' @references
#' \insertRef{Berkowitz2001}{tstests}
#'
#' \insertRef{Jarque1987}{tstests}
#' @examples
#' library(tsdistributions)
#' data(garch_forecast)
#' x <- pdist('jsu', q = garch_forecast$actual, mu = garch_forecast$forecast,
#' sigma = garch_forecast$sigma, skew = garch_forecast$skew,
#' shape = garch_forecast$shape)
#' print(berkowitz_test(x))
#'
#' @rdname berkowitz_test
#' @export
#'
berkowitz_test <- function(x, lags = 1, ...)
{
    if (missing(x)) stop("\nx is missing.")
    x <- as.numeric(x)
    x <- validate_uniform(x)
    x <- qnorm(x)
    n <- length(x)
    lags <- pmax(0, as.integer(lags))
    mod <- arima(x, order = c(lags, 0, 0), ...)
    unrestricted_likelihood <- as.numeric(logLik(mod))
    restricted_likelihood <- sum(dnorm(x, mean = 0, sd = 1, log = TRUE))
    likelihood_ratio_stat <- -2 * (restricted_likelihood - unrestricted_likelihood)
    prob <- 1 - pchisq(likelihood_ratio_stat, 2 + lags)
    m1 <- sum(x)/n
    xm <- (x - m1)
    m2 <- sum(xm^2)/n
    m3 <- sum(xm^3)/n
    m4 <- sum(xm^4)/n
    k1 <- (m3/m2^(3/2))^2
    k2 <- (m4/m2^2)
    jb_stat <- n * k1/6 + n * (k2 - 3)^2/24
    jb_prob <- 1 - pchisq(jb_stat, df = 2)
    signif <- pvalue_format(c(prob,jb_prob))
    decision <- c("Fail to Reject H0", "Fail to Reject Normal Assumption")
    if (prob < 0.05) decision[1] <- "Reject H0"
    if (jb_prob < 0.05) decision[2] <- "Reject Normal Assumption"
    H0 <- paste("Normal(0,1) with no autocorrelation")
    berk_tab <- data.table(Test = c("Berkowitz","Jarque-Bera"), DoF = c(2 + lags, 2), "Statistic" = c(likelihood_ratio_stat, jb_stat), "Pr(>Chisq)" = c(prob, jb_prob),
                           signif = signif)
    berk_tab[,'Decision(5%)' := decision]
    out <- list(table = berk_tab, hypothesis = H0, test_type = "Likelihood Ratio",
                p_value = prob,
                distribution = "Chi-squared", symbols = NULL,
                test_name = "Berkowitz Density Forecast Test", test_class = "berkowitz",
                reference = c("Berkowitz, J. (2001), Testing density forecasts, with applications to risk management, Journal of Business and Economic Statistics, 19(4), 465--474.",
                              "Jarque, C.M. and Bera, A.K. 1987, A test for normality of observations and regression residuals, International Statistical Review, 55(2), 163--172."))
    class(out) <- c("tstest.berkowitz","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.berkowitz
#' @rdname print
#' @export
#'
#'
print.tstest.berkowitz <- function(x, digits = max(3L, getOption("digits") - 3L),
                                signif.stars = getOption("show.signif.stars"),
                                include.decision = FALSE, ...)
{
    `Decision(5%)` <- signif <- NULL
    cat(x$test_name)
    cat("\nHypothesis(H0) : ", x$hypothesis,"\n\n")
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
    tab[,"Statistic"] <- as.character(signif(tab[,"Statistic"], digits = digits))
    tab[,"Pr(>Chisq)"] <- as.character(signif(tab[,"Pr(>Chisq)"], digits = digits))
    print(tab)
    cat("\n---")
    if (signif.stars) {
        cat("\n")
        cat(signif_codes())
    }
    return(invisible(x))
}

#' @aliases as_flextable.tstest
#' @method as_flextable tstest.berkowitz
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.berkowitz <- function(x, digits = max(3L, getOption("digits") - 3L),
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
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    out <- out |> hline(i = 2)
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = c("References: ", x$reference))
    }
    out <- colformat_double(out, j = c("Statistic", "Pr(>Chisq)"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}
