#' Expected Shortfall DE Test
#'
#' @description The expected shortfall test of Du and Escanciano (2017).
#' @param x the probability integral transformed series (pit).
#' @param alpha the quantile level for calculating the forecast value at risk
#' and expected shortfall.
#' @param lags the numbers of lags to use for the conditional test.
#' @param boot whether to use bootstrap simulation for estimating the p-values.
#' @param n_boot the bootstrap replications used to calculate the p-value.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.shortfall_de} which has a print and
#' as_flextable method.
#' @details The test of Du and Escanciano (2017) combines ideas from
#' Berkowitz (2001) and Christoffersen (1998) to create an unconditional
#' and conditional shortfall test based on the probability integral transformed
#' actuals conditioned on the forecast distribution to evaluate the severity
#' and independence of the residuals shortfall (based on violations of VaR).
#' The unconditional test (severity) checks for the mean of cumulative violations using
#' a t-test, whilst the conditional test (independence) is a Portmanteau test applied to
#' estimated cumulative violations. A bootstrap approach to calculating the
#' distribution of the test statistics is available for finite samples, similar
#' to the suggestions of McNeil (2000).
#' @aliases shortfall_test
#' @references
#' \insertRef{Du2017}{tstests}
#'
#' \insertRef{Berkowitz2001}{tstests}
#'
#' \insertRef{Christoffersen1998}{tstests}
#'
#' \insertRef{McNeil2000}{tstests}
#'
#' @aliases shortfall_de_test
#' @examples
#' library(tsdistributions)
#' data("garch_forecast")
#' x <- pdist("jsu", q = garch_forecast$actual, mu = garch_forecast$forecast,
#' sigma = garch_forecast$sigma, skew = garch_forecast$skew,
#' shape = garch_forecast$shape)
#' print(shortfall_de_test(x, alpha = 0.05, lags = 4))
#'
#' @rdname shortfall_de_test
#' @export
#'
#'
shortfall_de_test <- function(x, alpha = 0.05, lags = 1, boot = FALSE, n_boot = 2000, ...)
{
    if (missing(x)) stop("\nx is missing.")
    x <- validate_uniform(x)
    alpha <- validate_alpha(alpha[1])
    lags <- max(1, abs(as.integer(lags[1])))
    n <- length(x)
    unconditional_stat <- unconditional_de_statistic(x, alpha)
    conditional_stat <- conditional_de_statistic(x, lags = lags, alpha = alpha)
    if (boot) {
        unconditional_pvalue <- simulate_unconditional_de_pvalue(x, n = n, nsim = n_boot, alpha = alpha)
        conditional_pvalue <- simulate_conditional_de_pvalue(x, n = n, lags = lags, nsim = n_boot, alpha = alpha)
    } else {
        unconditional_pvalue <- unconditional_de_pvalue(unconditional_stat, n = n, alpha = alpha)
        conditional_pvalue <- conditional_de_pvalue(conditional_stat, lags = lags, alpha = alpha)
    }
    stat <- c(unconditional_stat, conditional_stat)
    p_values <- c(unconditional_pvalue, conditional_pvalue)
    decision <- rep("Fail to Reject H0", 2)
    if (p_values[1] <= 0.05) decision[1] <- "Reject H0"
    if (p_values[2] <= 0.05) decision[2] <- "Reject H0"
    de_tab <- data.table("Test" = c("DE (U)", "DE (C)"),
               "Statistic" = stat, "Pr(>|t|)" = p_values, signif = pvalue_format(p_values))
    de_tab[,'Decision(5%)' := decision]
    H0 <- "Unconditional(U) and Independent(C)"
    out <- list(table = de_tab,
                hypothesis = H0,  test_type = c("t-test","portmanteau"),
                p_value = p_values,
                distribution = c("Normal","Chi-squared"), test_class = "shortfall_de",
                test_name = "Expected Shortfall Test (Du and Escanciano)",
                alpha = alpha, nobs = n, lags = lags,
                reference = "Du Z. and Escanciano J.C. (2017). Backtesting expected shortfall: accounting for tail risk. Management Science, 63(4), 940--958.")
    class(out) <- c("tstest.shortfall_de","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.shortfall_de
#' @rdname print
#' @export
#'
#'
print.tstest.shortfall_de <- function(x, digits = max(3L, getOption("digits") - 3L),
                                signif.stars = getOption("show.signif.stars"),
                                include.decision = FALSE, ...)
{
    signif <- `Decision(5%)` <- NULL
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
    return(invisible(x))
}


#' @aliases as_flextable.tstest
#' @method as_flextable tstest.shortfall_de
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.shortfall_de <- function(x, digits = max(3L, getOption("digits") - 3L),
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
    .text <- paste0("Coverage: ", x$alpha, ", Obs: ", x$nobs)
    out <- out |> add_footer_lines(top = FALSE, values = .text)
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = paste0("Reference: ", x$reference))
    }
    out <- colformat_double(out, j = c("Statistic", "Pr(>|t|)"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}

unconditional_de_statistic <- function(p, alpha = 0.05)
{
    stat <- mean((alpha - p) * (p <= alpha)/alpha)
    return(stat)
}

unconditional_de_pvalue <- function(stat, n, alpha)
{
    mu <- alpha/2
    sig <- sqrt(alpha * (1/3 - alpha/4))
    tvalue <- abs((sqrt(n) * (stat - mu))/sig)
    pvalue <- 2 * min(pnorm(abs(tvalue)), 1 - pnorm(abs(tvalue)))
    return(pvalue)
}

conditional_de_statistic <- function(p, lags = 1, alpha = 0.05)
{
    n <- length(p)
    stat <- (alpha - p) * (p <= alpha)/alpha
    adj_stat <- stat - alpha/2
    v_adj_stat <- mean(adj_stat^2)
    auto_cov <- sapply(1:lags, function(i) (1/(n - i)) * sum((adj_stat[(i + 1):n] * adj_stat[1:(n - i)])))
    auto_cor <- auto_cov/v_adj_stat
    stat <- n * sum(auto_cor^2)
    return(stat)
}

conditional_de_pvalue <- function(stat, lags, alpha)
{
    pvalue <- pchisq(stat, df = lags, lower.tail = FALSE)
    return(pvalue)
}

# bootstrap
simulate_draws_de <- function(n = 1000, nsim = 2000)
{
    # n = size of data
    matrix(runif(n * nsim), nrow = n, ncol = nsim, byrow = T)
}

simulate_unconditional_de_pvalue <- function(p, n = 1000, nsim = 2000, alpha = 0.05){
    sim <- simulate_draws_de(n, nsim)
    stat <- unconditional_de_statistic(p, alpha)
    # simulate the statistic for the same data size over nsim runs
    sim_stat <- apply(sim, 2, function(x) unconditional_de_statistic(x, alpha))
    sim_stat <- sort(sim_stat)
    # use to minimize P(C_lower <= U) = alpha/2 and P(U>= C_upper) = alpha/2
    p_value <- 2 * min(sum(sim_stat <= stat)/nsim, sum(sim_stat >= stat)/nsim)
    return(p_value)
}


simulate_conditional_de_pvalue <- function(p, n = 1000, nsim = 2000, lags = 1, alpha = 0.05){
    sim <- simulate_draws_de(n, nsim)
    stat <- conditional_de_statistic(p, lags = lags, alpha)
    # simulate the statistic for the same data size over nsim runs
    sim_stat <- apply(sim, 2, function(x) conditional_de_statistic(x, lags, alpha))
    sim_stat <- sort(sim_stat)
    # use to minimize P(C_lower <= U) = alpha/2 and P(U>= C_upper) = alpha/2
    p_value <- sum(sim_stat >= stat)/nsim
    return(p_value)
}
