#' Value at Risk and Expected Shortfall Tests
#'
#' @description The value at risk coverage and duration tests of
#' Kupiec (1995) and Christoffersen and Pelletier (1998,2004), and
#' expected shortfall test of Du and Escanciano (2017).
#' @param actual a series representing the actual value of the series in the
#' out of sample period.
#' @param forecast the forecast values of the series at the quantile
#' given by alpha (the forecast value at risk).
#' @param x the probability integral transformed series (pit).
#' @param alpha the quantile level used to calculate the forecast value at risk.
#' @param lags the numbers of lags to use for the conditional shortfall test.
#' @param boot whether to use bootstrap simulation for estimating the p-values of
#' the conditional shortfall test.
#' @param n_boot the bootstrap replications used to calculate the p-value.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.vares} which has a print and
#' as_flextable method.
#' @details
#' This is a condensed table of both the \code{\link{var_cp_test}} and
#' \code{\link{shortfall_de_test}}.
#' @aliases var_test
#' @references
#' \insertRef{Kupiec1995}{tstests}
#'
#' \insertRef{Christoffersen1998}{tstests}
#'
#' \insertRef{Christoffersen2004}{tstests}
#'
#' \insertRef{Du2017}{tstests}
#'
#' @aliases var_test
#' @rdname var_test
#' @export
#'
#'
var_test <- function(actual, forecast, x, alpha, lags = 1, boot = FALSE, n_boot = 2000, ...)
{
    if (missing(actual)) stop("\nactual is missing.")
    if (missing(forecast)) stop("\nforecast is missing.")
    if (missing(x)) stop("\nx is missing.")
    if (missing(alpha)) stop("\nalpha is missing.")
    value_at_risk <- var_cp_test(actual = actual, forecast = forecast, alpha = alpha)
    expected_shortfall <- shortfall_de_test(x = x, alpha = alpha, lags = lags, boot = boot, n_boot = n_boot)
    tab_var <- value_at_risk$table
    tab_shortfall <- expected_shortfall$table
    tab <- data.table(Test = c(tab_var$Test, tab_shortfall$Test), "Measure" = c(rep("VaR", 4),rep("ES",2)),
                      "Statistic" = c(tab_var$Chisq, tab_shortfall$Statistic),
                      "Pr(>.)" = c(tab_var$`Pr(>Chisq)`,tab_shortfall$`Pr(>|t|)`),
                      "signif" = c(tab_var$signif,tab_shortfall$signif),
                      "Decision(5%)" = c(tab_var$`Decision(5%)`,tab_shortfall$`Decision(5%)`))
    out <- list(table = tab, alpha = alpha, nobs = length(actual), failures = value_at_risk$failures,
                hypothesis = list(value_at_risk$hypothesis, expected_shortfall$hypothesis),
                test_type = list(value_at_risk$test_type, expected_shortfall$test_type),
                reference = list(value_at_risk$reference, expected_shortfall$reference),
                test_name = "Value at Risk and Expected Shortfall Tests")
    class(out) <- c("tstest.vares","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.vares
#' @rdname print
#' @export
#'
#'
print.tstest.vares <- function(x, digits = max(3L, getOption("digits") - 3L),
                                      signif.stars = getOption("show.signif.stars"),
                                      include.decision = FALSE, ...)
{
    `Decision(5%)` <- signif <- NULL
    cat(x$test_name)
    cat("\n")
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
#' @method as_flextable tstest.vares
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.vares <- function(x, digits = max(3L, getOption("digits") - 3L),
                                             signif.stars = getOption("show.signif.stars"),
                                             include.decision = FALSE,
                                             table.caption = x$test_name,
                                             footnote.reference = FALSE, ...)
{
    signif <- `Decision(5%)` <- NULL
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
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "Test", align = "left") |> align(j = "Measure", align = "left")
    if (signif.stars) {
        out <- out |> align(j = "signif", align = "left") |>
            bold(j = "signif", bold = TRUE) |>
            padding(padding.left = 0, j = "signif", part  = "all") |>
            set_header_labels(signif = "")
        out <- out |> add_footer_lines(values = signif_codes())
    }
    .text <- paste0("Coverage: ", x$alpha, ", Obs: ", x$nobs, ", Failures: ", x$failures[1], ", E[Failures]: ", x$failures[2])
    out <- out |> add_footer_lines(top = FALSE, values = .text)
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = c("References: ", unlist(x$reference)))
    }
    out <- colformat_double(out, j = c("Statistic", "Pr(>.)"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}

