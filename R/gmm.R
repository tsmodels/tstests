#' GMM Orthogonality Test
#'
#' @description The GMM orthogonality test of Hansen (1982).
#' @param x a series representing the standardized residuals of some estimated model.
#' @param lags the lags for the co-moment test.
#' @param skewness the skewness of the estimated model residuals.
#' @param kurtosis the kurtosis of the estimated model residuals.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.gmm} which has a print and
#' as_flextable method.
#' @details For parametric models estimated with a particular distribution, the
#' skewness and kurtosis should flow from the distributional model. See for example
#' \code{\link[tsdistributions]{dskewness}} and
#' \code{\link[tsdistributions]{dkurtosis}}.
#' @aliases gmm_test
#'
#' @references
#' \insertRef{Hansen1982}{tstests}
#'
#' @aliases gmm_test
#' @examples
#' library(tsgarch)
#' library(tsdistributions)
#' library(data.table)
#' library(xts)
#' data("spy")
#' spyr <- na.omit(diff(log(spy)))
#' spec <- garch_modelspec(spyr, model = "egarch", order = c(2,1), constant = TRUE,
#' distribution = "jsu")
#' mod <- estimate(spec)
#' skewness <- dskewness("jsu", skew = coef(mod)["skew"], shape = coef(mod)["shape"])
#' # kurtosis is dkurtosis is the excess over the Normal (3) so we add back 3
#' # since the test takes the actual not excess kurtosis.
#' kurtosis <- dkurtosis("jsu", skew = coef(mod)["skew"], shape = coef(mod)["shape"]) + 3
#' test <- gmm_test(residuals(mod, standardize = TRUE), lags = 2, skewness = skewness,
#' kurtosis = kurtosis)
#' print(test, collapse = TRUE, include.decision = TRUE)
#'
#' @rdname gmm_test
#' @export
#'
#'
gmm_test <- function(x, lags = 1, skewness = 0, kurtosis = 3, ...)
{
    if (missing(x)) stop("\nx is missing.")
    `Pr(>|t|)` <- NULL
    std_residuals <- matrix(as.numeric(x), ncol = 1)
    n <- NROW(std_residuals) - lags
    std_residuals_lagged <- std_residuals[-c(1:lags), , drop = FALSE]
    if (lags == 1) {
        q <- 3
        c_names <- c("M1","M2","M3","M4","Q2[1]","Q3[1]","Q4[1]","J")
        symbols <- c("E\\left[z_t\\right]",
                     "E\\left[z^2_t - 1\\right]",
                     "E\\left[z^3_t\\right]",
                     "E\\left[z^4_t - 3\\right]",
                     "E\\left[(z^2_t - 1)(z^2_{t-1} - 1)\\right]",
                     "E\\left[(z^3_t)(z^3_{t-1})\\right]",
                     "E\\left[(z^4_t - 3)(z^4_{t-1} - 3)\\right]",
                     "J")
        joint_index <- c(1,2,3,4,5,6,7,8)
    } else {
        q <- 3 * (lags + 1)
        c_names <- c("M1","M2","M3","M4",
                     paste0("Q2[",1:lags,"]"), paste0("Q2[J]"),
                     paste0("Q3[",1:lags,"]"), paste0("Q3[J]"),
                     paste0("Q4[",1:lags,"]"), paste0("Q4[J]"),
                     "J")
        symbols <- c("E\\left[z_t\\right]",
                     "E\\left[z^2_t - 1\\right]",
                     "E\\left[z^3_t\\right]",
                     "E\\left[z^4_t - 3\\right]",
                     paste0("E\\left[(z^2_t - 1)(z^2_{t-",1:lags,"} - 1)\\right]"),
                     "E\\left[(z^2_t - 1)(z^2_{t-j} - 1)\\right]",
                     paste0("E\\left[(z^3_t)(z^3_{t-",1:lags,"})\\right]"),
                     "E\\left[(z^3_t)(z^3_{t-j})\\right]",
                     paste0("E\\left[(z^4_t - 3)(z^4_{t-",1:lags,"} - 3)\\right]"),
                     "E\\left[(z^4_t - 3)(z^4_{t-j} - 3)\\right]",
                     "J")
        joint_index <- c(1, 2, 3, 4, 4 + lags + 1, (4 + lags + 1) + lags + 1,
                         (4 + lags + 1) + lags + 1 + lags + 1, length(symbols))
    }
    df_individual <- n - 1
    orthogonal_matrix <- matrix(NA, ncol = 4 + q + 1 , nrow = 4)
    colnames(orthogonal_matrix) <- c_names
    rownames(orthogonal_matrix) <- c("Mean","Std. Error","t value","Pr(>t)")
    f1 <- std_residuals_lagged[,1]
    orthogonal_matrix[1:3, 1] <- c(mean(f1), sqrt(mean(f1^2)/n), mean(f1)/sqrt(mean(f1^2)/n))
    f2 <- (std_residuals_lagged[,1]^2) - 1
    orthogonal_matrix[1:3, 2] <- c(mean(f2), sqrt(mean(f2^2)/n), mean(f2)/sqrt(mean(f2^2)/n))
    f3 <- std_residuals_lagged[,1]^3 - skewness
    orthogonal_matrix[1:3, 3] <- c(mean(f3), sqrt(mean(f3^2)/n), mean(f3)/sqrt(mean(f3^2)/n))
    f4 <- std_residuals_lagged[,1]^4 - kurtosis
    orthogonal_matrix[1:3, 4] <- c(mean(f4), sqrt(mean(f4^2)/n), mean(f4)/sqrt(mean(f4^2)/n))
    M <- rbind(t(f1), t(f2), t(f3), t(f4))
    orthogonal_matrix[4,1:4] <- 2 * (1 - pt(abs(orthogonal_matrix[3,1:4]), df_individual))

    tmp1 <- .wald_comoment_test(std_residuals[,1]^2 - 1, lags, n)
    k <- 5
    if (lags == 1) {
        orthogonal_matrix[1, k] <- tmp1$g
        orthogonal_matrix[2, k] <- tmp1$varg
        orthogonal_matrix[3, k] <- tmp1$tval[2]
        orthogonal_matrix[4, k] <- 1 - pchisq(orthogonal_matrix[3,k], lags)
        k <- k + 1
    } else {
        orthogonal_matrix[1, k:(k + lags - 1)] <- tmp1$g
        orthogonal_matrix[2, k:(k + lags - 1)] <- tmp1$varg
        orthogonal_matrix[3, k:(k + lags)] <- tmp1$tval
        orthogonal_matrix[4, k:(k + lags - 1)] <- 1 - pchisq(orthogonal_matrix[3,k:(k + lags - 1)], 1:lags)
        orthogonal_matrix[4, (k + lags)] <- 1 - pchisq(orthogonal_matrix[3,(k + lags)], lags)
        k <- k + lags + 1
    }
    tmp2 <- .wald_comoment_test(std_residuals[,1]^3 - skewness, lags, n)
    if (lags == 1) {
        orthogonal_matrix[1, k] <- tmp2$g
        orthogonal_matrix[2, k] <- tmp2$varg
        orthogonal_matrix[3, k] <- tmp2$tval[2]
        orthogonal_matrix[4, k] <- 1 - pchisq(orthogonal_matrix[3,k], lags)
        k <- k + 1
    } else {
        orthogonal_matrix[1, k:(k + lags - 1)] <- tmp2$g
        orthogonal_matrix[2, k:(k + lags - 1)] <- tmp2$varg
        orthogonal_matrix[3, k:(k + lags)] <- tmp2$tval
        orthogonal_matrix[4, k:(k + lags - 1)] <- 1 - pchisq(orthogonal_matrix[3,k:(k + lags - 1)], 1:lags)
        orthogonal_matrix[4, (k + lags)] <- 1 - pchisq(orthogonal_matrix[3,(k + lags)], lags)
        k <- k + lags + 1
    }
    tmp3 <- .wald_comoment_test(std_residuals[,1]^4 - kurtosis, lags, n)
    if (lags == 1) {
        orthogonal_matrix[1, k] <- tmp3$g
        orthogonal_matrix[2, k] <- tmp3$varg
        orthogonal_matrix[3, k] <- tmp3$tval[2]
        orthogonal_matrix[4, k] <- 1 - pchisq(orthogonal_matrix[3,k], lags)
        k <- k + 1
    } else {
        orthogonal_matrix[1, k:(k + lags - 1)] <- tmp3$g
        orthogonal_matrix[2, k:(k + lags - 1)] <- tmp3$varg
        orthogonal_matrix[3, k:(k + lags)] <- tmp3$tval
        orthogonal_matrix[4, k:(k + lags - 1)] <- 1 - pchisq(orthogonal_matrix[3,k:(k + lags - 1)], 1:lags)
        orthogonal_matrix[4, (k + lags)] <- 1 - pchisq(orthogonal_matrix[3,(k + lags)], lags)
        k <- k + lags + 1
    }
    M <- rbind(M, tmp1$h, tmp2$h, tmp3$h)
    g <- c(as.numeric(orthogonal_matrix[1,1:4]), tmp1$g, tmp2$g, tmp3$g)
    # all moments
    S <- (M %*% t(M))/n
    orthogonal_matrix[3,k] <- n * t(g) %*% solve(S) %*% g
    orthogonal_matrix[4,k] <- 1 - pchisq(orthogonal_matrix[3,k], 4 + 3*lags)
    gmm_table <- as.data.table(t(orthogonal_matrix), keep.rownames = T)
    colnames(gmm_table) <- c("Moment","Mean","Std. Error","t value","Pr(>|t|)")
    gmm_table[,signif := pvalue_format(`Pr(>|t|)`)]
    hypothesis <- "E[Moment] = 0"
    distribution <- c("T","Chi-squared")
    decision <- rep('Fail to Reject H0', nrow(gmm_table))
    if (any(gmm_table$`Pr(>|t|)` <= 0.05)) {
        decision[which(gmm_table$`Pr(>|t|)` <= 0.05)] <- "Reject H0"
    }
    gmm_table[,'Decision(5%)' := decision]
    out <- list(table = gmm_table, hypothesis = hypothesis,
                p_value = orthogonal_matrix[4,k],
                distribution = distribution,
                symbols = symbols,
                test_type = "t-test/Wald test",
                joint_index  = joint_index,
                test_class = "gmm", test_name = "GMM Orthogonality Test",
                reference = "Hansen,L.P. (1982), Large sample properties of generalized method of moments estimators, Econometrica, 50(4), 1029--1054.")
    class(out) <- c("tstest.gmm", "tstest")
    return(out)
}

#' @param collapse collapses the results for multiple lags to just report the
#' joint test.
#' @aliases print.tstest
#' @method print tstest.gmm
#' @rdname print
#' @export
#'
#'
print.tstest.gmm <- function(x, digits = max(3L, getOption("digits") - 3L),
                             signif.stars = getOption("show.signif.stars"),
                             include.decision = FALSE, collapse = TRUE,
                             ...)
{
    Moment <- signif <- `Decision(5%)` <- NULL
    cat(x$test_name)
    cat("\nHypothesis(H0) : ", x$hypothesis,"\n\n")
    tab <- copy(x$table)
    if (collapse) {
        tab <- tab[x$joint_index]
    }
    if (!include.decision) {
        tab[,`Decision(5%)` := NULL]
    }
    if (!signif.stars) {
        tab[,signif := NULL]
    } else {
        setnames(tab, "signif"," ")
    }
    cnames <- tab$Moment
    tab[,Moment := NULL]
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


#' @param collapse collapses the results for multiple lags to just report the
#' joint test.
#' @aliases as_flextable.tstest
#' @method as_flextable tstest.gmm
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.gmm <- function(x, digits = max(3L, getOption("digits") - 3L),
                             signif.stars = getOption("show.signif.stars"),
                             include.decision = FALSE, collapse = TRUE,
                             use.symbols = TRUE,
                             table.caption = x$test_name,
                             footnote.reference = FALSE, ...)
{
    `Decision(5%)` <- NULL
    if (is.null(table.caption)) table.caption <- x$test_name
    tab <- copy(x$table)
    cnames <- colnames(tab)
    if (collapse) {
        tab <- tab[x$joint_index]
        idx <- x$joint_index
    } else {
        idx <- 1:nrow(x$table)
    }
    if (!include.decision) {
        tab[,`Decision(5%)` := NULL]
        cnames <- colnames(tab)

    }
    if (!signif.stars) {
        tab[,signif := NULL]
        cnames <- colnames(tab)
    }
    tab <- as.data.frame(tab)
    out <- flextable(tab) |> set_caption(caption = table.caption) |> align(j = "Moment", align = "left")
    if (signif.stars) {
        out <- out |> align(j = "signif", align = "left") |>
            bold(j = "signif", bold = TRUE) |>
            padding(padding.left = 0, j = "signif", part  = "all") |>
            set_header_labels(signif = "" )
        out <- out |> add_footer_lines(values = signif_codes())
    }
    out <- out |> add_footer_lines(top = FALSE, values = c(paste0("Hypothesis(H0) : ",x$hypothesis)))
    if (use.symbols) {
        sym <- x$symbols[idx]
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



.wald_comoment_test <- function(std_residuals, lags, n){
    f0 <- std_residuals %*% matrix(1, nrow = 1, ncol = lags)
    fx <- NULL
    for (i in 1:lags) fx <- cbind(fx, .lag_vector(std_residuals, n_lag = i, pad = 0))
    fx <- fx[-c(1:lags), , drop = FALSE]
    f0 <- f0[-c(1:lags), , drop = FALSE]
    fmat <- f0 * fx
    g <- colMeans(fmat)
    varg <- apply(fmat, 2, FUN = function(x) sum(x^2)/n/n)
    tval <- g^2/varg
    h <- t(fmat)
    S <- (h %*% t(h))/n
    joint <- n * t(g) %*% solve(S) %*% g
    tval <- c(tval, joint)
    return(list(tval = tval, h = h, g = g, varg = varg, S = S))
}
