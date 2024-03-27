#' Horizontal line of the mean of y
#'
#' Draw a horizontal line of the mean of y
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_polygon}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param linetype,linewidth [linetype]
#' @return a ggplot object
#'
#' @export
stat_mean_hline <- function(mapping = NULL, data = NULL, geom = "hline",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, linetype = "dashed",
                            linewidth = .25,  ...) {
  layer(
    stat = StatMeanHline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(linetype = linetype, linewidth = linewidth,
                  na.rm = na.rm, ...)
  )
}

StatMeanHline <- ggproto(
  "StatMeanHline", Stat,
  compute_group = function(data, scales) {
    transform(data, yintercept = mean(y))
  },
  required_aes = c("x", "y")
)

#' Vertical line of the mean of x
#'
#' Draw a Vertical line of the mean of x
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_polygon}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param linetype,linewidth [linetype]
#' @return a ggplot object
#'
#' @export
stat_mean_vline <- function(mapping = NULL, data = NULL,
                            geom = "vline", position = "identity",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, linetype = "dashed",
                            linewidth = .25, ...) {
  layer(
    stat = StatMeanVline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(linetype = linetype, linewidth = linewidth,
                  na.rm = na.rm, ...)
  )
}

StatMeanVline <- ggproto(
  "StatMeanVLine", Stat,
  compute_group = function(data, scales) {
    transform(data, xintercept = mean(x))
  },
  required_aes = c("x", "y")
)

#' Horizontal and vertical line of the mean of x, y
#'
#' Draw a horizontal and vertical line of the mean of x, y
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_polygon}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param linetype,linewidth [linetype]
#' @return a ggplot object
#'
#' @export
stat_mean_line <- function(mapping = NULL, data = NULL,
                           geom = c("hline", "vline"), position = "identity",
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, linetype = "dashed",
                           linewidth = .25, ...) {
  list(
    stat_mean_hline(
      mapping = mapping, data = data, geom = geom[1L], position = position,
      na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    ),
    stat_mean_vline(
      mapping = mapping, data = data, geom = geom[2L], position = position,
      na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    )
  )
}

#' Convex hull
#'
#' Draw a convex hull
#'
#' @inheritParams ggplot2::layer
#' @param alpha colour transparency level in \[0,1]
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_polygon}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @return a ggplot object
#'
#' @export
stat_chull <- function(mapping = NULL, data = NULL,
                       geom = "polygon", position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, alpha = .5, ...) {
  layer(
    stat = StatChull,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(alpha = alpha, na.rm = na.rm, ...)
  )
}

StatChull <- ggproto(
  "StatChull", Stat,
  compute_group = function(data, scales) {
    data[grDevices::chull(data$x, data$y), , drop = FALSE]
  },
  required_aes = c("x", "y")
)

#' Point of the mean of x, y
#'
#' Draw a point of the mean of x, y
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_point}}.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param shape [shape]
#' @return a ggplot object
#'
#' @export
stat_mean_point <- function(mapping = NULL, data = NULL,
                            geom = "point", position = "identity",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, shape = 4, ...) {
  layer(
    stat = StatMeanPoint, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(shape = shape, na.rm = na.rm, ...)
  )
}

StatMeanPoint <- ggproto(
  "StatMean", Stat,
  compute_group = function(data, scales) {
    data.frame(x = mean(data$x, na.rm = TRUE),
               y = mean(data$y, na.rm = TRUE))
  },
  required_aes = c("x", "y")
)
