#' Nyblom-Hansen Parameter Constancy Test
#'
#' @description The parameter constancy test of Nyblom (1989).
#' @param x a series representing the standardized residuals of some estimated model.
#' @param scores the log likelihood score matrix. The \code{\link[sandwich]{estfun}} method
#' if exported by a package for a model will return this matrix.
#' @param parameter_names optional character vector of the parameter names. Usually
#' read off the column names of the score matrix.
#' @param parameter_symbols an optional character vector of the latex names of the
#' parameters which can be used when printing using the flextable format.
#' @param ... not currently used.
#' @returns An object of class \dQuote{tstest.nyblom} which has a print and
#' as_flextable method.
#' @details The p-values for the test statistic are based on a pre-computed density,
#' by simulation using equation 3.3 of Nyblom (1989), with up to 40 parameters and
#' saved as an internal data object within the package. A kernel density is used
#' to fit the 10,000 samples of the distribution before extracting the p-values.
#' The original simulation generated more than 100,000 data points but these
#' were compressed to quantiles at intervals of 0.001 in order to keep the package
#' size under 5MB.
#' @aliases nyblom_test
#' @references
#' \insertRef{Nyblom1989}{tstests}
#' @examples
#' library(tsgarch)
#' library(xts)
#' data("spy")
#' spyr <- na.omit(diff(log(spy)))
#' spec <- garch_modelspec(spyr[1:1200], model = "garch", order = c(1,1),
#' constant = TRUE, distribution = "norm")
#' mod <- estimate(spec)
#' test <- nyblom_test(residuals(mod, standardize = TRUE), scores = estfun(mod),
#' parameter_names = names(coef(mod)),
#' parameter_symbols = mod$parmatrix[estimate == 1]$symbol)
#' print(test)
#'
#' @rdname nyblom_test
#' @export
#'
#'
nyblom_test <- function(x, scores = NULL, parameter_names = colnames(scores), parameter_symbols = NULL, ...)
{
    `Decision(5%)` <- `Pr(>t)` <- NULL
    if (missing(x)) stop("\nx (residuals) vector is missing.")
    if (is.null(scores)) stop("\nscores matrix must be provided")
    std_res <- x
    n <- length(std_res)
    H <- t(scores) %*% (scores)
    V <- try(solve(H), silent = TRUE)
    m <- NCOL(scores)
    if (is.null(parameter_names)) parameter_names <- colnames(scores)
    if (is.null(parameter_names)) {
        parameter_names <- paste0("par",1:m)
        parameter_symbols <- NULL
    }
    if (length(parameter_names) != m) stop("\nlength of parameter_names not equal to NCOL(scores)")
    check <- check_symbols(parameter_symbols, m)
    fun_i <- nyblom_f(1)
    fun_j <- nyblom_f(m)
    if (inherits(V, "try-error")) {
        warning("\nproblem inverting OPG estimator. Check scores for problem.")
        individual_stat <- matrix(rep(NA, length(parameter_names)), ncol = 1)
        joint_stat <- NA
        individual_pvalue <- rep(as.numeric(NA), m)
        joint_pvalue <- as.numeric(NA)
        rownames(individual_stat) <- parameter_names
        nyblom_table <- data.table(parameter = c(parameter_names, "Joint"),
                                   "Statistic" = c(individual_stat, joint_stat),
                                   "Pr(>t)" = c(individual_pvalue, joint_pvalue))
        nyblom_table[,signif := rep("", m + 1)]
        decision <- rep('Fail to Reject H0', nrow(nyblom_table))
        nyblom_table[,'Decision(5%)' := decision]
    } else {
        cumulative_scores <- as.matrix(apply(scores, 2, FUN = function(x) cumsum(x)))
        xx <- t(cumulative_scores) %*% cumulative_scores
        nyblomj <- sum(diag(xx %*% V))/n
        nyblomt <- diag(xx)/(diag(H) * n)
        individual_stat <- matrix(nyblomt, ncol = 1)
        joint_stat <- nyblomj
        rownames(individual_stat) <- parameter_names
        individual_pvalue <- 1 - sapply(as.numeric(individual_stat), function(x) pkde(x, fun_i))
        joint_pvalue <- 1 - pkde(joint_stat, fun_j)
        nyblom_table <- data.table(parameter = c(parameter_names, "Joint"),
                                   "Statistic" = c(as.numeric(individual_stat), joint_stat),
                                   "Pr(>t)" = c(individual_pvalue, joint_pvalue))
        nyblom_table[,signif := pvalue_format(`Pr(>t)`)]
        decision <- rep('Fail to Reject H0', nrow(nyblom_table))
        if (any(nyblom_table$`Pr(>t)` <= 0.05)) {
            decision[which(nyblom_table$`Pr(>t)` <= 0.05)] <- "Reject H0"
        }
        nyblom_table[,'Decision(5%)' := decision]
    }
    out <- list(table = nyblom_table, hypothesis = "Constant Parameters", test_type = "Lagrange Multiplier",
                p_value = joint_pvalue,
                distribution = "Cramer-von Mises", symbols = parameter_symbols,
                test_name = "Nyblom-Hansen Parameter Constancy Test", test_class = "nyblom",
                reference = "Nyblom,J. (1989), Testing for the constancy of parameters over time, Journal of the American Statistical Association, 405, 223--230.")
    class(out) <- c("tstest.nyblom","tstest")
    return(out)
}

#' @aliases print.tstest
#' @method print tstest.nyblom
#' @rdname print
#' @export
#'
#'
print.tstest.nyblom <- function(x, digits = max(3L, getOption("digits") - 3L),
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
#' @method as_flextable tstest.nyblom
#' @rdname as_flextable
#' @export
#'
as_flextable.tstest.nyblom <- function(x, digits = max(3L, getOption("digits") - 3L),
                                       signif.stars = getOption("show.signif.stars"),
                                       include.decision = FALSE,
                                       use.symbols = TRUE,
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
    if (use.symbols & !is.null(x$symbols)) {
        sym <- c(x$symbols,"\\mathrm{J}")
        for (i in 1:nrow(tab)) {
            out <- compose(out, i = i, j = 1, as_paragraph(as_chunk(' '))) |> append_chunks(i = i,j = 1, as_equation(sym[i]))
        }
    }
    if (footnote.reference) {
        out <- out |> add_footer_lines(top = FALSE, values = paste0("Reference: ", x$reference))
    }
    out <- colformat_double(out, digits = digits) |> autofit(add_w = 0.2)
    if (signif.stars) out <- out |> hline(i = 1, part = "footer")
    return(out)
}

nyblom_f <- function(k = 1)
{
    if (k > 40) {
        warning("\ndistribution not currently available for more than 40 parameters.")
        f <- as.numeric(NA)
    } else {
        x <- nyblom_distribution[,k]
        f <- kde(sort(x))
    }
    return(f)
}

nyblom_critical <- function(k = 1, alpha = 0.05)
{
    if (k > 40) {
        warning("\ndistribution not currently available for more than 40 parameters.")
        critical_value <- as.numeric(NA)
    } else {
        x <- nyblom_distribution[,k]
        f <- kde(sort(x))
        critical_value <- qkde(1 - alpha, f)
    }
    return(critical_value)
}
