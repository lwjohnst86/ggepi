
# From ggplot2's utilities.
"%||%" <- ggplot2:::"%||%"

#' Convert continuous p-values into a factor based on the given split.
#'
#' @param p.value Vector of p-values.
#' @param split_by Where to split the p-values up by. Default is the <0.001,
#'   <0.01, <0.05, and all others are >0.05.
#'
#' @return A factor vector of p-value splits.
#' @export
#'
#' @examples
#'
#' pvals <- c(0.001, 0.0123, 0.123, 0.009, 0.2145, 0.987, 0.05, 0.049, 0.051)
#' discrete_pvalue(pvals)
#' discrete_pvalue(pvals, split_by = c(0.05))
#' discrete_pvalue(pvals, split_by = c(0.01))
#' discrete_pvalue(pvals, split_by = c(0.01, 0.05))
#' discrete_pvalue(pvals, split_by = c(0.05, 0.001, 0.01))
discrete_pvalue <- function(p.value, split_by = c(0.001, 0.01, 0.05)) {
    stopifnot(is.numeric(p.value), is.numeric(split_by))
    split_by <- sort(split_by)
    sig_pval_labels <- paste0("<", split_by)
    nonsig_pval_labels <- paste0(">", split_by[length(split_by)])
    pval_labels <- c(sig_pval_labels, nonsig_pval_labels)
    pval_discrete <- cut(
        p.value,
        breaks = c(-Inf, split_by, Inf),
        labels = pval_labels,
        ordered_result = TRUE
    )

    factor(pval_discrete, levels = rev(levels(pval_discrete)))
}
