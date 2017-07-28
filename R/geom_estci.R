#' Horizontal point and line range with vertical center line.
#'
#' Useful to visualize results from regression type analyses, as it shows the
#' estimate, confidence interval, and optionally use the value of the p.value
#' to highlight significant associations. A vertical line is included
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_pointrange
#' @param center.linetype The linetype for the center line.
#' @param center.linecolour Line colour for the center line.
#' @param center.linesize Line size for the center line.
#'
#' @return Adds a ggplot2 geom layer.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' library(broom)
#'
#' fit <- lm(Fertility ~ 0 + Catholic + Agriculture + Examination + Education + Infant.Mortality, data = swiss)
#' fit <- tidy(fit, conf.int = TRUE)
#' fit <- transform(fit, model = "non-log")
#'
#' p <- ggplot(fit, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))
#' p
#' p + geom_estci()
#' p + geom_estci(aes(xintercept = 1.1), center.linecolour = "red")
#' p + geom_estci(aes(size = rev(p.value)), linetype = "dotted")
#' p + geom_estci(aes(colour = rev(p.value), size = rev(p.value)), linetype = "dotted")
#' p + geom_estci(aes(size = rev(p.value), alpha = rev(p.value)), linetype = "dotted")
#' p + geom_estci(aes(size = rev(p.value), alpha = rev(p.value), colour = rev(p.value)))
#' p + geom_estci(aes(alpha = rev(p.value)), linetype = "dashed", center.linetype = "solid")
#' p + geom_estci(aes(alpha = rev(p.value), xintercept = 1), colour = "blue", linetype = "dashed", center.linetype = "solid")
#' p + geom_estci(aes(alpha = rev(p.value), xintercept = 1), center.linesize = 1.5)
#' p + geom_estci(center.linesize = 0.25, height = 1, fatten = 2)
#' p + geom_estci(center.linesize = 2, height = 0.5, fatten = 8)
#'
#' fit_log <- lm(log(Fertility) ~ 0 + Catholic + Agriculture + Examination + Education + Infant.Mortality, data = swiss)
#' fit_log <- tidy(fit_log, conf.int = TRUE)
#' fit_log <- transform(fit_log, model = "log")
#' two_fits <- rbind(fit, fit_log)
#'
#' p <- ggplot(two_fits, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))
#' # It might be possible to show groups with 'dodging', but it is currently in development.
#' # p + geom_estci(aes(group = model, colour = model), position = position_dodge(width = 0.3))
#' p + geom_estci()
#'
#' p <- ggplot(two_fits, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high))
#' p + geom_estci() + facet_grid(~ model)
#'
geom_estci <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       height = NA,
                       fatten = 4,
                       na.rm = FALSE,
                       show.legend = NA,
                       center.linetype = "dashed",
                       center.linecolour = "black",
                       center.linesize = 0.5,
                       inherit.aes = TRUE) {

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomEstci,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            fatten = fatten,
            height = height,
            na.rm = na.rm,
            center.linetype = center.linetype,
            center.linecolour = center.linecolour,
            center.linesize = center.linesize,
            ...
        )
    )
}

#' @rdname ggepi-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomEstci <- ggplot2::ggproto(
    "GeomEstci",
    ggplot2::Geom,

    default_aes = ggplot2::aes(
        colour = "black",
        size = 0.5,
        linetype = 1,
        height = NA,
        alpha = NA,
        shape = 19,
        fill = NA,
        stroke = 0.5,
        xintercept = 0
    ),
    required_aes = c("y", "x", "xmin", "xmax", "xintercept"),

    draw_key = ggplot2::draw_key_point,

    setup_data = function(data, params) {
        data$height <- data$height %||%
            params$height %||% (resolution(data$y, FALSE) * 0.9)

        data$xintercept <- data$xintercept %||%
            params$xintercept %||% 0

        data$center.linetype <- data$center.linetype %||%
            params$center.linetype %||% "dotted"

        data$center.linecolour <- data$center.linecolour %||%
            params$center.linecolour %||% "black"

        data$center.linesize <- data$center.linesize %||%
            params$center.linesize %||% 0.5

        transform(
            data,
            ymin = y - height / 2,
            ymax = y + height / 2,
            height = NULL
        )
    },

    draw_group = function(data,
                          panel_params,
                          coord,
                          height = NULL,
                          fatten = NA,
                          center.linetype = NA,
                          center.linecolour = NA,
                          center.linesize = NA) {

        data_centerline <- transform(
            data,
            linetype = data$center.linetype,
            alpha = NA,
            colour = data$center.linecolour,
            size = data$center.linesize
        )

        ggplot2:::ggname("geom_estci", grid::grobTree(
            GeomVline$draw_panel(data_centerline, panel_params, coord),
            GeomErrorbarh$draw_panel(data, panel_params, coord, height = height),
            GeomPoint$draw_panel(transform(data, size = size * fatten), panel_params, coord)
        ))
    }
)
