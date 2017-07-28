
#' Standard plot for estimates and confidence interval.
#'
#' Typical figure for examining estimates and confidence intervals from
#' regression type analyses. Has standard features for the plot, but if you want
#' more customization, see the \code{\link{geom_estci}} function.
#'
#' @seealso geom_estci
#'
#' @param data Tidied dataframe from a linear regression, as from the function
#'   \link[broom]{tidy} from the broom package.
#' @param xterms Column that contains the predictor variables (ie. "term").
#' @param split_by Column to split the results up by (e.g. different models or
#'   different y terms).
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param legend.title Legend title.
#' @param center.line Location for the center line.
#'
#' @return Plot of estimate and confidence interval.
#' @export
#'
#' @examples
#'
#' library(broom)
#' fit <- lm(Fertility ~ 0 + Catholic + Agriculture + Examination +
#'     Education + Infant.Mortality, data = swiss)
#' fit <- tidy(fit, conf.int = TRUE)
#' fit <- transform(fit, model = "non-log")
#'
#' view_estci(fit, xterms = "term")
#' view_estci(fit, xterms = "term", center.line = 1)
#'
#' fit_log <- lm(log(Fertility) ~ 0 + Catholic + Agriculture + Examination +
#'     Education + Infant.Mortality, data = swiss)
#' fit_log <- tidy(fit_log, conf.int = TRUE)
#' fit_log <- transform(fit_log, model = "log")
#' two_fits <- rbind(fit, fit_log)
#'
#' view_estci(two_fits, xterms = "term", split_by = "model", center.line = 1)
view_estci <-
    function(data,
             xterms = "term",
             split_by = NA,
             ylab = "Predictor terms",
             xlab = "Estimate and CI",
             legend.title = "P-value",
             center.line = 0) {

        needed_columns <-
            setdiff(stats::na.omit(
                c(
                    "estimate",
                    "conf.low",
                    "conf.high",
                    "p.value",
                    xterms,
                    split_by
                )
            ),
            names(data))

        if (length(needed_columns) != 0) {
            stop(
                "The columns (",
                paste(needed_columns, collapse = ", "),
                ") are missing from the dataset.",
                call. = FALSE
            )
        }

        data$p.value <- discrete_pvalue(data$p.value)

        p <-
            ggplot(
                data,
                aes_string(
                    x = "estimate",
                    y = xterms,
                    xmin = "conf.low",
                    xmax = "conf.high",
                    size = "p.value",
                    colour = "p.value"
                )
            ) +
            geom_estci(aes(xintercept = center.line), fatten = 2) +
            scale_color_grey(start = 0.75,
                             end = 0,
                             name = legend.title) +
            scale_size_discrete(name = legend.title, range = c(0.5, 2)) +
            theme_classic() +
            theme(strip.background = element_blank()) +
            labs(y = ylab, x = xlab)

        if (!is.na(split_by)) {
            p <- p + facet_grid(paste0("~", split_by), scales = "free")
        }

        return(p)
    }
