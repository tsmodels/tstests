#' Mincer-Zarnowitz Test
#'
#' @description The forecast unbiasedness test of Mincer and Zarnowitz (1969).
#' @param actual a vector representing the actual values of a series.
#' @param forecast a vector representing the forecasted values of the series.
#' @param ... additional arguments passed to \code{\link[car]{linearHypothesis}},
#' except the \dQuote{test} argument which is fixed to Chisq.
#' @returns An object of class \dQuote{tstest.minzar} which has a print and
#' as_flextable method.
#' @aliases minzar_test
#' @references
#' \insertRef{Mincer1969}{tstests}
#' @aliases minzar_test
#' @examples
#' data(arma_forecast)
#' test <- minzar_test(arma_forecast$actual, arma_forecast$forecast)
#' test
#'
#' @rdname minzar_test
#' @export
#'
#'
minzar_test <- function(actual, forecast, ...)
{
    parameter <- `Pr(>|t|)` <- NULL
    if (missing(actual)) stop("\nactual is missing.")
    if (missing(forecast)) stop("\nforecast is missing.")
    n_actual <- length(actual)
    n_forecast <- length(forecast)
    if (n_actual != n_forecast) stop("\nlength of actual must be equal to length of forecast")
    z <- data.frame(actual = as.numeric(actual), forecast = as.numeric(forecast))
    fit <- lm(actual ~ forecast, data = z)
    jeffect <- linearHypothesis(fit, c("(Intercept) = 0", "forecast = 1"), test = "Chisq", ...)
    minzar_table <- as.data.table(summary(fit)$coefficients, keep.rownames = TRUE)
    setnames(minzar_table,"rn","parameter")
    minzar_table[,parameter := c("constant","forecast")]
    # remove constant from tables
    joint_effect_table <- data.table("parameter" = "J", "Estimate" = NA, "Std. Error" = NA, "t value" = jeffect$Chisq[2], "Pr(>|t|)" = jeffect$`Pr(>Chisq)`[2])
    minzar_table <- rbind(minzar_table, joint_effect_table)
    minzar_table[,signif := pvalue_format(`Pr(>|t|)`)]
    decision <- rep('Fail to Reject H0', nrow(minzar_table))
    if (any(minzar_table$`Pr(>|t|)` <= 0.05)) {
        decision[which(minzar_table$`Pr(>|t|)` <= 0.05)] <- "Reject H0"
    }
    minzar_table[,'Decision(5%)' := decision]
    out <- list(table = minzar_table,
                joint_hypothesis = c("constant = 0","forecast = 0"),
                hypothesis = "Unbiased Forecast",  test_type = "Wald",
                p_value = jeffect$`Pr(>Chisq)`[2],
                distribution = "Chi-squared", symbols = NULL, test_class = "minzar",
                test_name = "Mincer-Zarnowitz Test",
                reference = "Mincer JA. and Zarnowitz V. (1969), 'The evaluation of economic forecasts.' In Economic forecasts and expectations: Analysis of forecasting behavior and performance, NBER, 3--46")
    class(out) <- c("tstest.minzar","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.minzar
#' @rdname print
#' @export
#'
#'
print.tstest.minzar <- function(x, digits = max(3L, getOption("digits") - 3L),
                                signif.stars = getOption("show.signif.stars"),
                                include.decision = FALSE, ...)
{
    signif <- parameter <- `Decision(5%)` <- NULL
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
    cnames <- tab$parameter
    tab[,parameter := NULL]
    tab <- as.data.frame(tab)
    rownames(tab) <- cnames
    print(tab, digits = digits)
    cat("\n---")
    if (signif.stars) {
        cat("\n")
        cat(signif_codes())
    }
    return(invisible(x))
}

#' @aliases as_flextable.tstest
#' @method as_flextable tstest.minzar
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.minzar <- function(x, digits = max(3L, getOption("digits") - 3L),
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
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "parameter", align = "left")
    if (signif.stars) {
        out <- out |> align(j = "signif", align = "left") |>
            bold(j = "signif", bold = TRUE) |>
            padding(padding.left = 0, j = "signif", part  = "all") |>
            set_header_labels(signif = "", parameter = "")
        out <- out |> add_footer_lines(values = signif_codes())
    }
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = paste0("Reference: ", x$reference))
    }
    out <- colformat_double(out, digits = digits) |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}
