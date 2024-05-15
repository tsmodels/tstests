pvalue_format <- function(x) {
    z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels = c("***", "**", "*", ".", ""))
    as.character(z)
}

signif_codes <- function()
{
    return(c("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"))
}

.lag_vector <- function(x, n_lag = 1, remove_na = FALSE, pad = NA)
{
    # has NAs
    x <- as.matrix(x)
    n <- NROW(x)
    d <- NCOL(x)
    if (d == 1) x <- matrix(x, ncol = 1)
    z <- apply(x, 2, FUN = function(y) .embed_vector(y, n_lag + 1)[,n_lag + 1])
    if (!remove_na) z <- rbind(matrix(pad, ncol = d, nrow = n_lag),z)
    return(z)
}

.embed_vector <- function(x, k, by = 1, ascending = FALSE)
{
    x <- matrix(x, ncol = 1)
    n <- NROW(x)
    s <- seq(1, n - k + 1, by = by)
    lens <- length(s)
    cols <- if (ascending) 1:k else k:1
    return(matrix(x[s + rep(cols, rep(lens,k)) - 1], lens))
}

check_symbols <- function(symbols, m)
{
    if (!is.null(symbols)) {
        if (length(symbols) != m) stop(paste0("\nsymbols length must be equal to ",m))
    }
    return(1)
}

validate_uniform <- function(x)
{
    if (any(x < 0) | any(x > 1)) stop("\n x must be between 0 and 1 (PIT transformed).")
    return(x)
}

validate_alpha <- function(alpha)
{
    if (any(alpha < 0) | any(alpha > 1)) stop("\n alpha must be between 0 and 1 (probability).")
    return(alpha)
}

