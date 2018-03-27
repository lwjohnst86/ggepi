#' Correlation circle plot for dimensionality reduction techniques.
#'
#' This creates a ggplot2 layer that plots the correlation values of the
#' scores for components against the real values, as obtained from
#' dimensionality reduction methods. These methods include principal components
#' analysis and partial least squares.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param center.linetype,center.linecolour,center.linesize The line type,
#'   colour, and size for the two center lines.
#' @param inner.linetype,inner.linecolour,inner.linesize The line type,
#'   colour, and size for the inner circle line.
#' @param outer.linetype,outer.linecolour,outer.linesize The line type,
#'   colour, and size for the outer circle line.
#'
#' @return Adds a ggplot2 geom layer.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' library(broom)
#' library(pls)
#' data(yarn)
#'
#' # Set up data to plot.
#' fit <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
#' fit <- cor(model.matrix(fit), scores(fit)[, 1:2, drop = FALSE])
#' fit <- as.data.frame(fit)
#' fit$Variables <- rownames(fit)
#' rownames(fit) <- NULL
#' colnames(fit)[1:2] <- c("Comp1", "Comp2")
#' fit$Groups <- as.factor(sample.int(2, nrow(fit), replace = TRUE))
#'
#' p <- ggplot(fit, aes(x = Comp1, y = Comp2))
#' p + geom_corr_circle()
#' p + geom_corr_circle(outer.linetype = "dotted")
#' p + geom_corr_circle(inner.linecolour = "blue")
#' p + geom_corr_circle(center.linesize = 1)
#' p + geom_corr_circle(center.linecolour = "grey50", size = 3)
#'
#' # If you want to remove the circle or center lines, use 0.
#' p + geom_corr_circle(center.linetype = 0)
#' p + geom_corr_circle(outer.linetype = 0)
#'
#' # With grouping
#' p + geom_corr_circle(aes(colour = Groups))
#' p + geom_corr_circle(aes(colour = Groups), size = 3) +
#' scale_colour_brewer()
#' p + geom_corr_circle(aes(alpha = Groups))
#' p + geom_corr_circle(aes(size = Groups))
#' p + geom_corr_circle(aes(shape = Groups), size = 2)
#'
#' # With facets
#' p + geom_corr_circle() +
#' facet_grid(~ Groups)
#'
geom_corr_circle <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       outer.linetype = "solid",
                       outer.linecolour = "black",
                       outer.linesize = 0.5,
                       inner.linetype = "dotted",
                       inner.linecolour = "black",
                       inner.linesize = 0.5,
                       center.linetype = "solid",
                       center.linecolour = "grey50",
                       center.linesize = 0.3,
                       inherit.aes = TRUE) {

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomCorrcircle,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            outer.linetype = outer.linetype,
            outer.linecolour = outer.linecolour,
            outer.linesize = outer.linesize,
            inner.linetype = inner.linetype,
            inner.linecolour = inner.linecolour,
            inner.linesize = inner.linesize,
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
GeomCorrcircle <- ggproto(
    "GeomCorrcircle",
    Geom,

    default_aes = aes(
        colour = "black",
        size = 0.5,
        linetype = 1,
        alpha = NA,
        fill = NA,
        shape = 19,
        stroke = 0.5
    ),
    required_aes = c("y", "x"),

    draw_key = draw_key_point,

    setup_data = function(data, params) {

        data$outer.linetype <- data$outer.linetype %||%
            params$outer.linetype %||% "solid"
        data$outer.linecolour <- data$outer.linecolour %||%
            params$outer.linecolour %||% "black"
        data$outer.linesize <- data$outer.linesize %||%
            params$outer.linesize %||% 0.5

        data$inner.linetype <- data$inner.linetype %||%
            params$inner.linetype %||% "dashed"
        data$inner.linecolour <- data$inner.linecolour %||%
            params$inner.linecolour %||% "black"
        data$inner.linesize <- data$inner.linesize %||%
            params$inner.linesize %||% 0.5

        data$center.linetype <- data$center.linetype %||%
            params$center.linetype %||% "solid"
        data$center.linecolour <- data$center.linecolour %||%
            params$center.linecolour %||% "black"
        data$center.linesize <- data$center.linesize %||%
            params$center.linesize %||% 0.5

        data
    },

    draw_group = function(data,
                          panel_params,
                          coord,
                          outer.linetype = NA,
                          outer.linecolour = NA,
                          outer.linesize = NA,
                          inner.linetype = NA,
                          inner.linecolour = NA,
                          inner.linesize = NA,
                          center.linetype = NA,
                          center.linecolour = NA,
                          center.linesize = NA
                          ) {

        outer_circle <- circle_data(1, npoints = nrow(data))
        data_outer_circle <- transform(
            data,
            x = outer_circle$x,
            y = outer_circle$y,
            linetype = data$outer.linetype,
            alpha = NA,
            colour = data$outer.linecolour,
            size = data$outer.linesize
        )

        inner_circle <- circle_data(sqrt(1 / 2), npoints = nrow(data))
        data_inner_circle <- transform(
            data,
            x = inner_circle$x,
            y = inner_circle$y,
            linetype = data$inner.linetype,
            alpha = NA,
            colour = data$inner.linecolour,
            size = data$inner.linesize
        )

        data_hline <- transform(
            data,
            x = -1,
            y = 0,
            xend = 1,
            yend = 0,
            alpha = NA,
            linetype = data$center.linetype,
            colour = data$center.linecolour,
            size = data$center.linesize
        )
        data_hline <- unique(data_hline)

        data_vline <- transform(
            data,
            x = 0,
            y = -1,
            xend = 0,
            yend = 1,
            alpha = NA,
            linetype = data$center.linetype,
            colour = data$center.linecolour,
            size = data$center.linesize
        )
        data_vline <- unique(data_vline)

        ggplot2:::ggname("geom_corr_circle", grid::grobTree(
            GeomSegment$draw_panel(data_hline, panel_params, coord),
            GeomSegment$draw_panel(data_vline, panel_params, coord),
            GeomPath$draw_panel(data_outer_circle, panel_params, coord),
            GeomPath$draw_panel(data_inner_circle, panel_params, coord),
            GeomPoint$draw_panel(data, panel_params, coord)
        ))
    }
)

#         ggrepel::geom_text_repel(
#             data = fit,
#             aes(label = xvariables),
#             size = 2.5,
#             box.padding = 0.4,
#             segment.alpha = 0.3
#         )


circle_data <-
    function(radius = 1,
             center = c(0, 0),
             npoints = 100) {
        tt <- seq(0, 2 * pi, length.out = npoints)
        xvalues <- center[1] + radius * cos(tt)
        yvalues <- center[2] + radius * sin(tt)
        return(data.frame(x = xvalues, y = yvalues))
    }
