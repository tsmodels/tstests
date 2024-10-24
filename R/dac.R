#' Directional Accuracy Tests
#'
#' @description The directional accuracy test of Pesaran and Timmermann (1992),
#' and excess profitability test of Anatolyev and Gerko (2005).
#' @param actual a series representing the actual value of the series in the
#' out of sample period.
#' @param forecast the forecast values of the series in the out of sample period.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.dac} which has a print and
#' as_flextable method.
#' @details The null hypothesis for the test of Pesaran and Timmermann (1992) is
#' that the actual and predicted are independent (no sign predictability), whereas
#' the test of Anatolyev and Gerko (2005) measures the significance of the excess
#' profitability under the null hypothesis of no excess excess profitability. Both
#' are Hausman type tests asymptotically distributed as standard Normal.
#' @note The test will not work with constant forecasts.
#' @aliases dac_test
#' @references
#' \insertRef{Pesaran1992}{tstests}
#'
#' \insertRef{Anatolyev2005}{tstests}
#' @aliases dac_test
#' @examples
#' data(arma_forecast)
#' print(dac_test(arma_forecast$actual, arma_forecast$forecast))
#'
#' @rdname dac_test
#' @export
#'
#'
dac_test <- function(actual, forecast, ...)
{
    if (missing(actual)) stop("\nactual is missing.")
    if (missing(forecast)) stop("\nforecast is missing.")
    `Pr(>|t|)` <- NULL
    actual <- as.numeric(actual)
    forecast_var <- as.numeric(forecast)
    n_actual <- length(actual)
    n_forecast <- length(forecast)
    if (n_actual != n_forecast) stop("\nactual and forecast lengths must be equal.")
    if (norm(matrix(diff(forecast), ncol = 1)) <= 1e-12) {
        stop("\nforecast is constant to at least 12 decimal places...test cannot be calculated.")
    }
    prevalence <- mean(actual >= 0)
    test_1 <- .pt_test(actual, forecast)
    test_2 <- .ag_test(actual, forecast)
    dac_tab <- data.table("Test" = c(test_1$Test, test_2$Test),
                         "Statistic" = c(test_1$Stat, test_2$Stat), "Pr(>|t|)" = c(test_1$p_value, test_2$p_value))
    dac_tab[,signif := pvalue_format(`Pr(>|t|)`)]
    decision <- rep("Fail to Reject H0 ", 2)
    if (dac_tab$`Pr(>|t|)`[1] <= 0.05) decision[1] <- "Reject H0"
    if (dac_tab$`Pr(>|t|)`[2] <= 0.05) decision[2] <- "Reject H0"
    dac_tab[,'Decision(5%)' := decision]
    H0 <- c(test_1$H0, test_2$H0)
    references <- c("Pesaran, M.H. and Timmermann, A. (1992), A simple nonparametric test of predictive performance, Journal of Business and Economic Statistics, 10(4), 461--465.",
                    "Anatolyev, S. and Gerko, A. (2005), A trading approach to testing for predictability, Journal of Business and Economic Statistics, 23(4), 455--461.")
    out <- list(table = dac_tab,
                hypothesis = H0,  test_type = c("Hausman","Hausman"),
                accuracy = test_1$Accuracy, prevalence = prevalence,
                nobs = n_actual,
                p_value = c(test_1$p_value, test_2$p_value),
                distribution = c("Normal","Normal"), test_class = "dac",
                test_name = "Directional Accuracy Tests",
                reference = references)
    class(out) <- c("tstest.dac","tstest")
    return(out)
}

#' @aliases print.tstest
#' @method print tstest.dac
#' @rdname print
#' @export
#'
#'
print.tstest.dac <- function(x, digits = max(3L, getOption("digits") - 3L),
                                      signif.stars = getOption("show.signif.stars"),
                                      include.decision = FALSE, ...)
{
    signif <- `Decision(5%)` <- NULL
    cat(x$test_name)
    cat("\nHypothesis(H0) : No Predictability\n")
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
    cat("\nObs.\t\t:", x$nobs)
    cat("\nAccuracy\t:", x$accuracy)
    cat("\nPrevalence\t:", x$prevalence)
    return(invisible(x))
}


#' @aliases as_flextable.tstest
#' @method as_flextable tstest.dac
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.dac <- function(x, digits = max(3L, getOption("digits") - 3L),
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
    .text <- paste0("Obs: ", x$nobs, ", Accuracy: ", x$accuracy, ", Prevalence: ", x$prevalence)
    out <- out |> add_footer_lines(top = FALSE, values = .text)
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = c("References: ", x$reference))
    }
    out <- colformat_double(out, j = c("Statistic", "Pr(>|t|)"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}


.pt_test <- function(actual, forecast)
{
    n <- length(actual)
    x_t <- z_t <- y_t <- rep(0, n)
    x_t[which(actual > 0)] <- 1
    y_t[which(forecast > 0)] <- 1
    p_y <- mean(y_t)
    p_x <- mean(x_t)
    z_t[which((forecast * actual) > 0)] <- 1
    p_hat <- mean(z_t)
    p_star <- p_y * p_x + (1 - p_y) * (1 - p_x)
    p_hat_var <- (p_star * (1 - p_star))/n
    p_star_var <- ((2*p_y - 1)^2 * (p_x * (1 - p_x)))/n + ((2 * p_x - 1)^2 * (p_y * (1 - p_y)))/n + (4 * p_x * p_y * (1 - p_x) * (1 - p_y))/n^2
    s_n <- (p_hat - p_star)/sqrt(p_hat_var - p_star_var)
    ans <- list(Test = "PT: (Sign)", Accuracy = p_hat, Stat = s_n, p_value = 1 - pnorm(s_n),
               H0 = "PT: No Predictability (Sign)")
    return(ans)
}

.ag_test <- function(actual, forecast)
{
    n <- length(actual)
    r_t <- sign(forecast) * actual
    A_t <- mean(r_t)
    B_t <- mean(sign(forecast)) * mean(actual)
    p_y <- 0.5 * (1 + mean(sign(forecast)))
    V_EP <- (4/(n^2)) * p_y * (1 - p_y) * sum((actual - mean(actual))^2)
    EP <- (A_t - B_t)/sqrt(V_EP)
    ans <- list(Test = "AG: (Mean)", Accuracy = sum(r_t > 0)/n, Stat = EP,
                p_value = 1 - pnorm(EP), H0 = "AG: No Predictability (Mean)")
    return(ans)
}
