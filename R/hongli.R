#' The Non-Parametric Density Test of Hong and Li
#'
#' @description Implements the Non-Parametric Density Test of Hong and Li (2005).
#' @param x a series representing the PIT transformed actuals given the forecast
#' values.
#' @param lags the number lags to use for testing the joint hypothesis.
#' @param conf_level the confidence level for generating the critical values
#' which serve as thresholds for deciding on the null hypothesis.
#' @param ... none.
#' @returns An object of class \dQuote{tstest.hongli} which has a print and
#' \dQuote{as_flextable} method.
#' @details
#' A novel method to analyze how well a conditional density fits the underlying
#' data is through the probability integral transformation (PIT) discussed in
#' Rosenblatt (1952) and used in the \code{\link{berkowitz_test}}. Hong and Li (2005)
#' introduced a nonparametric portmanteau test, building on the work of
#' Ait-Sahalia (1996), which tests the joint hypothesis of i.i.d and uniformity
#' for a series of PIT transformed data. To achieve this, it tests for
#' misspecification in the conditional moments of the model transformed
#' standardized residuals, and is distributed as N(0, 1) under the null of a
#' correctly specified model. These moment tests are reported as \sQuote{M(1,1)}
#' to \sQuote{M(4,4)} in the output, with \sQuote{M(1,2)} related to
#' ARCH-in-mean effects, and \sQuote{M(2,1)} to leverage, while \sQuote{W} is the
#' Portmanteu type test statistic for general misspecification (using p lags)
#' and also distributed as N(0, 1) under the null of a correctly specified model.
#' Only upper tail critical values are used in this test. The interested reader
#' is referred to the paper for more details.
#' @aliases hongli_test
#' @references
#' \insertRef{Hong2005}{tstests}
#' @examples
#' library(tsdistributions)
#' data(garch_forecast)
#' x <- pdist('jsu', q = garch_forecast$actual, mu = garch_forecast$forecast,
#' sigma = garch_forecast$sigma, skew = garch_forecast$skew,
#' shape = garch_forecast$shape)
#' print(hongli_test(x), include.decision = TRUE)
#'
#' @rdname hongli_test
#' @export
#'
hongli_test <- function(x, lags = 4, conf_level = 0.95, ...)
{
    p <- lags
    V_con <- 2 * (50/49 - 300/294 + 1950/1960 - 900/1568 + 450/2304)^2
    Q_hat <- matrix(0, p, 1)
    res <- rep(0, 7)
    A_con_2 <- integrate(.funb, 0, 1)$value
    n <- length(x)
    hpit = sd(x) * n^(-1/6)
    A_con_11 <- (1/hpit - 2) * (5/7)
    A_con_1 = (A_con_11 + 2 * A_con_2)^2 - 1
    for (i in 1:p) {
        M_hat = gauss_legendre_2d(f = .ghat, a1 = 0, b1 = 1, a2 = 0, b2 = 1, x, i, n, hpit)
        Q_hat[i, 1] = ((n - i) * hpit * M_hat - hpit * A_con_1)/sqrt(V_con)
    }
    res[7] = sum(Q_hat[, 1])/sqrt(p)
    res[1] = .moment_stats(1, 1, p, x, n)
    res[2] = .moment_stats(2, 2, p, x, n)
    res[3] = .moment_stats(3, 3, p, x, n)
    res[4] = .moment_stats(4, 4, p, x, n)
    res[5] = .moment_stats(1, 2, p, x, n)
    res[6] = .moment_stats(2, 1, p, x, n)
    H0 <- "Correctly Specified"
    decision <- rep("Fail to Reject H0", 7)
    critical_value <- qnorm(conf_level)
    decision <- sapply(res, function(x) if (x > critical_value) "Reject H0" else "Fail to Reject H0")
    names(res) = c("M(1,1)", "M(2,2)", "M(3,3)", "M(4,4)", "M(1,2)", "M(2,1)", "J")
    hongli_tab <- data.table(Test = c("M(1,1)", "M(2,2)", "M(3,3)", "M(4,4)", "M(1,2)", "M(2,1)", "J"), "Statistic (z)" = res, "Critical Value" = critical_value)
    hongli_tab[,'Decision' := decision]
    out <- list(table = hongli_tab, hypothesis = H0, test_type = "Portmaneau",
                conf_level = conf_level,
                distribution = "Normal(0,1)", symbols = NULL,
                test_name = "Hong-Li Non-Parametric Density Test", test_class = "hongli",
                reference = c("Hong, Y., and Li, H. (2005), Nonparametric specification testing for continuous-time models with applications to term structure of interest rates, Review of Financial Studies, 18(1), 37--84."))
    class(out) <- c("tstest.hongli","tstest")
    return(out)
}


#' @aliases print.tstest
#' @method print tstest.hongli
#' @rdname print
#' @export
#'
#'
print.tstest.hongli <- function(x, digits = max(3L, getOption("digits") - 3L),
                                   signif.stars = getOption("show.signif.stars"),
                                   include.decision = FALSE, ...)
{
    `Decision` <- signif <- NULL
    cat(x$test_name)
    cat("\nHypothesis(H0) : ", x$hypothesis,"\n\n")
    tab <- copy(x$table)
    if (!include.decision) {
        tab[,`Decision` := NULL]
    }
    tab <- as.data.frame(tab)
    rownames(tab) <- tab[,1]
    tab <- tab[,-1]
    tab[,"Statistic (z)"] <- as.character(signif(tab[,"Statistic (z)"], digits = digits))
    tab[,"Critical Value"] <- as.character(signif(tab[,"Critical Value"], digits = digits))
    print(tab)
    cat("\n---")
    cat("\nConfidence Level (%) :", round(100 * x$conf_level,3))
    return(invisible(x))
}

#' @aliases as_flextable.tstest
#' @method as_flextable tstest.hongli
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.hongli <- function(x, digits = max(3L, getOption("digits") - 3L),
                                          signif.stars = getOption("show.signif.stars"),
                                          include.decision = FALSE,
                                          table.caption = x$test_name,
                                          footnote.reference = FALSE, ...)
{
    `Decision` <- NULL
    if (is.null(table.caption)) table.caption <- x$test_name
    tab <- copy(x$table)
    cnames <- colnames(tab)
    if (!include.decision) {
        tab[,`Decision` := NULL]
        cnames <- colnames(tab)
    }
    tab <- as.data.frame(tab)
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "Test", align = "left")
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Confidence Level (%) : ",round(100 * x$conf_level,3))))
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    out <- out |> hline(i = 2)
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = c("References: ", x$reference))
    }
    out <- colformat_double(out, j = c("Statistic (z)", "Critical Value"), digits = digits) |> autofit()
    out <- out |> autofit(add_w = 0.4)
    return(out)
}

gauss_legendre_2d <- function(f, a1, b1, a2, b2, ...)
{
    nodes <- c(-0.9815606, -0.9041173, -0.7699027, -0.587318,
               -0.3678315, -0.1252334, 0.1252334, 0.3678315, 0.587318,
               0.7699027, 0.9041173, 0.9815606)
    weights <- c(0.04717534, 0.10693933, 0.16007833, 0.20316743,
                 0.23349254, 0.24914705, 0.24914705, 0.23349254, 0.20316743,
                 0.16007833, 0.10693933, 0.04717534)
    C <- (b1 - a1)/2
    D <- (b1 + a1)/2
    S <- 0
    for (i in 1:length(nodes)) {
        tmp_x <- nodes[i] * C + D
        S <- S + weights[i] *
            gauss_legendre_2d_helper(f = f, x = tmp_x, a2 = a2, b2 = b2, nodes = nodes,
                                     weights = weights, ...)
    }
    return(C * S)
}

gauss_legendre_2d_helper <- function(f, x, a2, b2, nodes, weights, ...)
{
    C <- (b2 - a2)/2
    D <- (b2 + a2)/2
    S <- 0
    for (i in 1:length(nodes)) {
        tmp_y <- nodes[i] * C + D
        S <- S + weights[i] * f(x = c(x, tmp_y), ...)
    }
    return(C * S)
}

.funb <- function(b)
{
    tmp1 <- (8/15 + b - (2/3) * (b^3) + (1/5) * (b^5))^(-2)
    tmp2 <- b * ((1 - b^2)^4) + 128/315 + (8/3) * (b^3) - (24/5) *
        (b^5) + (24/7) * (b^7) - (8/9) * (b^9)
    ans <- tmp1 * tmp2
    return(ans)
}

.ghat <- function(x, pit, i, n, hpit)
{
    z1 <- x[1]
    z2 <- x[2]
    np <- length(pit)
    b1 <- .fnbound(z1, pit[-c(1:i)], hpit)
    b2 <- .fnbound(z2, pit[-c((np - i + 1):np)], hpit)
    g <- sum(b1 * b2)/(n - i)
    return((g - 1)^2)
}

.fnbound <- function(x, y, hpit)
{
    K1 <- .quartic_kernel((x - y)/hpit)/hpit
    if ((x >= 0) * (x < hpit)) {
        K2 <- integrate(.quartic_kernel, (-x/hpit), 1)$value
        return(K1/K2)
    } else if ((x >= hpit) * (x <= (1 - hpit))) {
        return(K1)
    } else if ((x > (1 - hpit)) * (x <= 1)) {
        K2 = integrate(.quartic_kernel, -1, (1 - x)/hpit)$value
        return(K1/K2)
    } else {
        return(K1)
    }
}

.quartic_kernel <- function(z)
{
    kern <- rep(0, length(z))
    non_zeros <- which(z <= 1 & z >= -1)
    v <- z[non_zeros]
    v <- 1 - v^2
    kern[non_zeros] <- (15/16) * (v^2)
    return(kern)
}

.moment_stats <- function(m, ll, p, pit, n)
{
    part1 <- 0
    part2 <- 0
    part3 <- 0
    for (j in 1:p) {
        w <- 1 - j/p
        part1 <- part1 + (w^2) * (n - j) *
            ((.rcc(j, m, ll, pit, n)/.rcc(0, m, ll, pit, n))^2)
        part2 <- part2 + w^2
        part3 <- part3 + w^4
    }
    return((part1 - part2)/part3)
}

.rcc <- function(j, m, ll, pit, n)
{
    r1 <- sum((pit[(j + 1):n]^m) * (pit[1:(n - j)]^ll))/n
    r2 <- sum((pit[(j + 1):n]^m))/n
    r3 <- sum((pit[1:(n - j)]^ll))/n
    return(r1 - r2 * r3)
}

