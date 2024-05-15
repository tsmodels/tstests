#' Sign Bias Test
#'
#' @description The sign bias test of Engle and Ng (1993).
#' @param x a series representing the residuals of some estimated model.
#' @param sigma either a scalar representing the residuals standard deviation else
#' a vector of the same length as x representing the conditional standard deviation
#' of the residuals.
#' @param ... additional arguments passed to \code{\link[car]{linearHypothesis}},
#' except the \dQuote{test} which is fixed to use the Chisq test.
#' @returns An object of class \dQuote{tstest.signbias} which has a print and
#' as_flextable method.
#' @aliases signbias_test
#' @references
#' \insertRef{Engleng1993}{tstests}
#' @aliases signbias_test
#' @rdname signbias_test
#' @examples
#' library(tsgarch)
#' library(tsdistributions)
#' library(xts)
#' data("spy")
#' spyr <- na.omit(diff(log(spy)))
#' spec <- garch_modelspec(spyr, model = "garch", order = c(1,1),
#' constant = TRUE, distribution = "jsu")
#' mod <- estimate(spec)
#' print(signbias_test(residuals(mod), sigma(mod)))
#'
#' @export
#'
#'
signbias_test <- function(x, sigma = 1, ...)
{
    if (missing(x)) stop("\nx is missing.")
    `Pr(>|t|)` <- parameter <- NULL
    term <- NULL
    res <- as.numeric(x)
    sigma <- as.numeric(sigma)
    if (length(sigma) != 1) {
        if (length(sigma) != length(x)) stop("\nsigma must either be a scalar or a vector of length(x).")
    }
    std_res <- res/sigma
    sqr_std_res <- std_res^2
    n <- length(std_res)
    negative_indicator <- as.integer(res < 0)
    positive_indicator <- 1 - negative_indicator
    res_negative <- negative_indicator * res
    res_positive <- positive_indicator * res
    df <- data.frame(y = sqr_std_res[2:n], constant = rep(1, n - 1), `sign bias` = negative_indicator[1:(n - 1)],
                     `negative sign bias` = res_negative[1:(n - 1)], `positive sign bias` = res_positive[1:(n - 1)], check.names = FALSE)
    fit <- lm(y ~ constant + `sign bias` + `negative sign bias` + `positive sign bias` - 1, data = df)
    fit_residuals <- residuals(fit)
    sign_bias_table <- as.data.table(summary(fit)$coefficients, keep.rownames = TRUE)
    setnames(sign_bias_table,"rn","parameter")
    sign_bias_table[,parameter := c("constant","sign bias","negative sign bias","positive sign bias")]
    # remove constant from tables
    sign_bias_table <- sign_bias_table[-1]
    jeffect <- linearHypothesis(fit, c("`sign bias` = 0", "`negative sign bias` = 0","`positive sign bias`  =  0"), test = "Chisq", ...)
    joint_effect_table <- data.table("parameter" = "J", "Estimate" = NA, "Std. Error" = NA, "t value" = jeffect$Chisq[2], "Pr(>|t|)" = jeffect$`Pr(>Chisq)`[2])
    sign_bias_table <- rbind(sign_bias_table, joint_effect_table)
    sign_bias_table[,signif := pvalue_format(`Pr(>|t|)`)]
    decision <- rep('Fail to Reject H0', nrow(sign_bias_table))
    if (any(sign_bias_table$`Pr(>|t|)` <= 0.05)) {
        decision[which(sign_bias_table$`Pr(>|t|)` <= 0.05)] <- "Reject H0"
    }
    sign_bias_table[,'Decision(5%)' := decision]
    symbols <- c("S^{-}_{t-1}","S^{-}_{t-1}\\varepsilon_{t-1}","S^{+}_{t-1}\\varepsilon_{t-1}","\\mathrm{J}")
    out <- list(table = sign_bias_table,
                joint_hypothesis = c("sign bias = 0","negative sign bias = 0", "positive sign bias = 0"),
                hypothesis = "No sign bias",  test_type = "Wald",
                p_value = jeffect$`Pr(>Chisq)`[2],
                distribution = "Chi-squared", symbols = symbols, test_class = "signbias",
                test_name = "Sign Bias Test",
                reference = "Engle RF, Ng VK (1993). Measuring and testing the impact of news on volatility. The Journal of Finance, 48(5), 1749--1778.")
    class(out) <- c("tstest.signbias","tstest")
    return(out)
}

#' Test Print method
#'
#' @description Print method for objects inheriting class \dQuote{tstest}
#' @param x an object inheriting class \dQuote{tstest.test}.
#' @param digits integer, used for number formatting. Optionally, to avoid
#' scientific notation, set \sQuote{options(scipen=999)}.
#' @param signif.stars logical. If TRUE, \sQuote{significance stars} are printed.
#' @param include.decision prints out whether to reject the NULL at the 5% level
#' of significance.
#' @param ... not currently used.
#' @return Invisibly returns the original object.
#' @aliases print.tstest
#' @method print tstest.signbias
#' @rdname print
#' @export
#'
#'
print.tstest.signbias <- function(x, digits = max(3L, getOption("digits") - 3L),
                                  signif.stars = getOption("show.signif.stars"),
                                  include.decision = FALSE, ...)
{
    signif <- `Decision(5%)` <- parameter <- NULL
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

#' Transform a summary object into flextable
#' @description
#' Transforms a \dQuote{tstest.test} object into a flextable
#' with options on symbolic representation and model equation.
#' @param x an object of which inherits a \dQuote{tstest} class.
#' @param digits integer, used for number formatting. Optionally, to avoid
#' scientific notation, set \sQuote{options(scipen=999)}.
#' @param signif.stars logical. If TRUE, \sQuote{significance stars} are printed.
#' @param include.decision prints out whether to reject the NULL at the 5% level
#' of significance.
#' @param use.symbols for tests which either have parameters for which the
#' latex symbols were included in the calling function or for which the tests
#' generate values which can be represented as latex symbols, then these will
#' be generated.
#' @param table.caption an optional string for the table caption.
#' @param footnote.reference whether to include the reference paper of the test
#' in the footnotes.
#' @param ... not currently used. The returned object can be manipulated further
#' using flextable.
#' @return A flextable object.
#' @aliases as_flextable.tstest
#' @method as_flextable tstest.signbias
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.signbias <- function(x, digits = max(3L, getOption("digits") - 3L),
                                        signif.stars = getOption("show.signif.stars"),
                                        include.decision = FALSE,
                                        use.symbols = TRUE, table.caption = x$test_name,
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
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "parameter", align = "left")
    if (signif.stars) {
        out <- out |> align(j = "signif", align = "left") |>
            bold(j = "signif", bold = TRUE) |>
            padding(padding.left = 0, j = "signif", part  = "all") |>
            set_header_labels(signif = "", parameter = "")
        out <- out |> add_footer_lines(values = signif_codes())
    }
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (use.symbols) {
        sym <- x$symbols
        for (i in 1:nrow(tab)) {
            out <- compose(out, i = i, j = 1, as_paragraph(as_chunk(' '))) |> append_chunks(i = i,j = 1, as_equation(sym[i]))
        }
    }
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = paste0("Reference: ", x$reference))
    }
    out <- colformat_double(out, digits = digits) |> autofit()
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}
