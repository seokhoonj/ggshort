#' Crosshair lines at mean/median
#'
#' Draw horizontal and vertical lines at the mean or median of `x` and `y`.
#'
#' @name stat_line
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; if `TRUE`, remove missing values silently.
#' @param linetype,linewidth Passed to [ggplot2::geom_hline()] / [ggplot2::geom_vline()].
#' @param ... Additional arguments to the underlying geoms.
#'
#' @return A list of ggplot2 layers.
#'
#' @examples
#' \donttest{
#' # Mean
#' ggpoint(iris, x = Sepal.Length, y = Petal.Length, color = Species) +
#'   stat_mean_line() +
#'   stat_mean_point() +
#'   theme_view()
#'
#' # Median
#' ggpoint(iris, x = Sepal.Length, y = Petal.Length, color = Species) +
#'   stat_median_line() +
#'   stat_median_point() +
#'   theme_view()
#' }
NULL

#' @rdname stat_line
#' @export
stat_mean_hline <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                            show.legend = FALSE, inherit.aes = TRUE,
                            linetype = "dashed", linewidth = .3,  ...) {
  ggplot2::layer(
    stat = StatMeanHline, data = data, mapping = mapping,
    geom = "hline", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linetype = linetype,
      linewidth = linewidth,
      ...
    )
  )
}

StatMeanHline <- ggplot2::ggproto(
  "StatMeanHline",
  ggplot2::Stat,
  required_aes = c("y"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(data, yintercept = mean(y, na.rm = na.rm))
  }
)

#' @rdname stat_line
#' @export
stat_median_hline <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,
                              linetype = "dashed", linewidth = .3,
                              ...) {
  ggplot2::layer(
    stat = StatMedianHline, data = data, mapping = mapping,
    geom = "hline", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linetype = linetype,
      linewidth = linewidth,
      ...
    )
  )
}

StatMedianHline <- ggplot2::ggproto(
  "StatMedianHline",
  ggplot2::Stat,
  required_aes = c("y"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(data, yintercept = stats::median(y, na.rm = na.rm))
  }
)

#' @rdname stat_line
#' @export
stat_mean_vline <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE,
                            linetype = "dashed", linewidth = .3,
                            ...) {
  ggplot2::layer(
    stat = StatMeanVline, data = data, mapping = mapping,
    geom = "vline", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linetype = linetype,
      linewidth = linewidth,
      ...
    )
  )
}

StatMeanVline <- ggplot2::ggproto(
  "StatMeanVline",
  ggplot2::Stat,
  required_aes = c("x"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(data, xintercept = mean(x, na.rm = na.rm))
  }
)

#' @rdname stat_line
#' @export
stat_mean_line <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE,
                           linetype = "dashed", linewidth = .3,
                           ...) {
  list(
    stat_mean_hline(
      mapping = mapping, data = data, na.rm = na.rm,
      show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    ),
    stat_mean_vline(
      mapping = mapping, data = data, na.rm = na.rm,
      show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    )
  )
}

#' @rdname stat_line
#' @export
stat_median_vline <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,
                              linetype = "dashed", linewidth = .3,
                              ...) {
  ggplot2::layer(
    stat = StatMedianVline, data = data, mapping = mapping,
    geom = "vline", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linetype = linetype,
      linewidth = linewidth,
      ...
    )
  )
}

StatMedianVline <- ggplot2::ggproto(
  "StatMedianVline",
  ggplot2::Stat,
  required_aes = c("x"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(data, xintercept = median(x, na.rm = na.rm))
  }
)

#' @rdname stat_line
#' @export
stat_median_line <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE,
                             linetype = "dashed", linewidth = .3,
                             ...) {
  list(
    stat_median_hline(
      mapping = mapping, data = data, na.rm = na.rm,
      show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    ),
    stat_median_vline(
      mapping = mapping, data = data, na.rm = na.rm,
      show.legend = show.legend, inherit.aes = inherit.aes,
      linetype = linetype, linewidth = linewidth, ...
    )
  )
}

# Density -----------------------------------------------------------------

#' Vertical quantile line(s) for density plots
#'
#' Draw vertical line(s) at quantile(s) of `x`. Respects grouping if a
#' grouping/color/fill aesthetic is mapped.
#'
#' @param probs Numeric vector of probabilities in \[0, 1] (e.g., `0.95`).
#' @param na.rm Logical; remove NAs silently if `TRUE`. Default `FALSE`.
#' @param linetype,linewidth Line style/width for the vline(s).
#' @param ... Additional arguments passed to [ggplot2::geom_vline()].
#' @inheritParams ggplot2::layer
#'
#' @return A ggplot layer.
#'
#' @examples
#' \donttest{
#' ggdensity(iris, x = Sepal.Length, fill = Species) +
#'   stat_density_quantile_vline(probs = c(.5, .9))
#' }
#'
#' @export
stat_density_quantile_vline <- function(mapping = NULL, data = NULL,
                                        probs = .95, na.rm = FALSE,
                                        linetype = "dashed", linewidth = .3,
                                        show.legend = FALSE, inherit.aes = TRUE,
                                        ...) {
  ggplot2::layer(
    stat = StatDensityQuantileVline,
    data = data, mapping = mapping,
    geom = "vline", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      probs = probs, na.rm = na.rm,
      linetype = linetype, linewidth = linewidth,
      ...
    )
  )
}

StatDensityQuantileVline <- ggplot2::ggproto(
  "StatDensityQuantileVline",
  ggplot2::Stat,
  required_aes = c("x"),
  compute_group = function(data, scales, probs = .95, na.rm = TRUE) {
    qs  <- stats::quantile(data$x, probs = probs, na.rm = na.rm, names = FALSE)
    data.frame(x = qs, xintercept = qs, prob = probs,
               stringsAsFactors = FALSE)
  }
)

#' Quantile label(s) for density plots
#'
#' Place text labels at quantile location(s) of `x`. Use with
#' `geom_density()` and (optionally) `stat_density_quantile_vline()`.
#'
#' @param probs Numeric vector of probabilities in \[0, 1\].
#' @param y Numeric y position for labels (e.g., `Inf` to place at the top).
#' @param fmt A function `function(p, q)` returning a label for probability `p`
#'   and quantile `q`. Default prints `"q (p%)"`.
#' @param family Font family passed to [ggplot2::geom_text()].
#' @param na.rm Logical; remove NAs silently if `TRUE`. Default `TRUE`.
#' @param ... Additional arguments passed to [ggplot2::geom_text()].
#' @inheritParams ggplot2::layer
#'
#' @return A ggplot layer.
#'
#' @examples
#' \donttest{
#' ggdensity(iris, x = Sepal.Length, fill = Species) +
#'   stat_density_quantile_text(probs = c(.5, .9))
#' }
#'
#' @export
stat_density_quantile_text <- function(mapping = NULL, data = NULL,
                                       probs = .95, na.rm = TRUE,
                                       y = Inf,
                                       fmt = function(p, q)
                                         sprintf("%.2f(%.f%%)", q, p * 100),
                                       family = getOption("ggshort.font"),
                                       show.legend = FALSE, inherit.aes = TRUE,
                                       ...) {
  ggplot2::layer(
    stat = StatDensityQuantileText,
    data = data, mapping = mapping,
    geom = "text", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      probs = probs, na.rm = na.rm,
      y = y, fmt = fmt, family = family,
      ...
    )
  )
}

StatDensityQuantileText <- ggplot2::ggproto(
  "StatDensityQuantileText",
  ggplot2::Stat,
  required_aes = c("x"),
  compute_group = function(data, scales, probs = .95, na.rm = TRUE,
                           y = Inf, fmt, family) {
    qs  <- stats::quantile(data$x, probs = probs, na.rm = na.rm, names = FALSE)
    label <- mapply(fmt, probs, qs)
    data.frame(x = qs, y = y, label = label, prob = probs,
               stringsAsFactors = FALSE)
  }
)


#' Density quantile helper (vline + label)
#'
#' Convenience wrapper that adds both vertical quantile line(s) and text
#' label(s) at the same probabilities.
#'
#' @inheritParams stat_density_quantile_vline
#' @inheritParams stat_density_quantile_text
#' @param fmt Label formatter function (see [stat_density_quantile_text()]).
#' @param family Font family for labels (passed to `stat_density_quantile_text()`).
#' @param ... Additional arguments forwarded to both layers.
#'
#' @return A list of two ggplot layers.
#'
#' @examples
#' \donttest{
#' ggdensity(iris, x = Sepal.Length, fill = Species) +
#'   stat_density_quantile(probs = c(.05, .95))
#' }
#'
#' @export
stat_density_quantile <- function(mapping = NULL, data = NULL,
                                  probs = .95, na.rm = TRUE,
                                  y = Inf,
                                  fmt = function(p, q)
                                    sprintf("%.2f (%.0f%%)", q, p * 100),
                                  family = getOption("ggshort.font"),
                                  show.legend = FALSE, inherit.aes = TRUE, ...) {
  list(
    stat_density_quantile_vline(
      mapping = mapping, data = data,
      probs = probs, na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    ),
    stat_density_quantile_text(
      mapping = mapping, data = data,
      probs = probs, na.rm = na.rm,
      y = y, fmt = fmt, family = family,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  )
}

# Convex hull -------------------------------------------------------------

#' Convex hull polygon
#'
#' Draw the convex hull of points in the current group.
#'
#' @inheritParams ggplot2::layer
#' @param alpha Fill transparency in `[0, 1]`.
#' @param na.rm If `FALSE` (default), missing values are removed with a warning;
#'   if `TRUE`, they are silently removed.
#' @param ... Additional arguments passed to [ggplot2::geom_polygon()].
#'
#' @return A ggplot2 layer.
#'
#' @export
stat_chull <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, alpha = .3,
                       ...) {
  ggplot2::layer(
    stat = StatChull,
    data = data,
    mapping = mapping,
    geom = "polygon",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, alpha = alpha, ...)
  )
}

StatChull <- ggplot2::ggproto(
  "StatChull",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    data[grDevices::chull(data$x, data$y), , drop = FALSE]
  }
)

#' Mean/median point
#'
#' Draw a point at the mean or median of `x` and `y`.
#'
#' @name stat_point
#' @inheritParams ggplot2::layer
#' @param shape Passed to [ggplot2::geom_point()].
#' @param na.rm Logical; if `TRUE`, remove missing values silently.
#' @param ... Additional arguments to [ggplot2::geom_point()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \donttest{
#' # Mean point on Sepal.Length vs Sepal.Width
#' ggpoint(iris, x = Sepal.Length, y = Sepal.Width) +
#'   stat_mean_point(shape = 21, size = 4, fill = "red", color = "black") +
#'   theme_view()
#'
#' # Median point, grouped by Species
#' ggpoint(iris, x = Sepal.Length, y = Sepal.Width, color = Species) +
#'   stat_median_point(size = 4) +
#'   theme_view()
#'
#' # Add both mean (red) and median (blue) points
#' ggpoint(iris, x = Petal.Length, y = Petal.Width, color = Species) +
#'   stat_mean_point(color = "red", size = 3) +
#'   stat_median_point(color = "blue", size = 3) +
#'   theme_view()
#' }
NULL

#' @rdname stat_point
#' @export
stat_mean_point <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, shape = 1,
                            ...) {
  ggplot2::layer(
    stat = StatMeanPoint, data = data, mapping = mapping,
    geom = "point", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, shape = shape, ...)
  )
}

StatMeanPoint <- ggplot2::ggproto(
  "StatMeanPoint",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(
      data,
      x = mean(x, na.rm = na.rm),
      y = mean(y, na.rm = na.rm)
    )
  }
)

#' @rdname stat_point
#' @export
stat_median_point <- function(mapping = NULL, data = NULL, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, shape = 4,
                              ...) {
  ggplot2::layer(
    stat = StatMedianPoint, data = data, mapping = mapping,
    geom = "point", position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, shape = shape, ...)
  )
}

StatMedianPoint <- ggplot2::ggproto(
  "StatMedianPoint",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, na.rm = FALSE) {
    transform(
      data,
      x = median(x, na.rm = na.rm),
      y = median(y, na.rm = na.rm)
    )
  }
)
