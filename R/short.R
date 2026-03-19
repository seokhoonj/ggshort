#' ggplot bar helper (frequently used arguments)
#'
#' Quickly build bar charts with common options. Supply **unquoted**
#' column names.
#'
#' @param data A data.frame.
#' @param x,y Unquoted column names for x and y aesthetics.
#' @param ymin,ymax Optional lower/upper bounds for y (rarely used for bars).
#' @param ymin_err,ymax_err Optional lower/upper bounds for error bars.
#' @param color,fill,group Optional columns mapped to the `color`, `fill` and
#'   `group` aesthetics.
#' @param text Optional column/expression for tooltip text (e.g., ggplotly).
#' @param label Unquoted column/expression to use as text label.
#' @param label_args A named list of text style options for `geom_text()`.
#'   Supported names: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggline()], [ggpoint()], [ggjitter()], [ggscatter()], [ggdensity()],
#'   [gghistogram()], [ggbox()], [ggpie()], [ggmix()], [ggtable()]
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- expand.grid(x = c("A","B","C"), fill = c("X","Y","Z"))
#' n <- nrow(df); df$y <- sample(10:30, n, TRUE)
#' se <- runif(n, 1.5, 4)
#' df$ymin_err <- pmax(df$y - se, 0); df$ymax_err <- df$y + se
#' ggbar(df, x = x, y = y, fill = fill,
#'       ymin_err = ymin_err, ymax_err = ymax_err,
#'       label = y, label_args = list(vjust = -0.25)) +
#'   theme_view()
#' }
#'
#' @export
ggbar <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                  color = NULL, fill = NULL, group = NULL, text = NULL,
                  label,
                  label_args = list(
                    family = getOption("ggshort.font"),
                    size   = 4,
                    angle  = 0,
                    hjust  = 0.5,
                    vjust  = 0.5,
                    color  = "black"
                  )) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y, ymin = ymin, ymax = ymax,
    color = color, fill = fill, group = group,
    text = text
  ))

  position <- ggplot2::position_dodge2(preserve = "single")

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_bar(stat = "identity", position = position)

  if (!(missing(ymin_err) && missing(ymax_err))) {
    quos_err <- .valid_enquos(rlang::enquos(
      x = x, ymin = ymin_err, ymax = ymax_err
    ))
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(!!!quos_err),
      position = position,
      alpha = .5
    )
  }

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_dodge2(width = .9, preserve = "single")
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  p
}

#' ggplot line helper (frequently used arguments)
#'
#' Quickly draw line charts with the most common options. Supply **unquoted**
#' column names for aesthetics. Supports optional error bars and text labels.
#' Companion helpers: `ggbar`, `ggpoint`, `ggjitter`, `ggscatter`, `ggdensity`,
#' `ggpie`, `ggmix`, `ggtable`.
#'
#' @param data A data.frame.
#' @param x,y Unquoted column names mapped to the x and y aesthetics.
#' @param ymin,ymax Optional lower/upper bounds for y (rare in pure line charts).
#' @param ymin_err,ymax_err Optional lower/upper bounds for error bars.
#' @param color,fill,group Optional columns mapped to the `color`, `fill` and
#'   `group` aesthetics.
#' @param linetype Optional column for line type mapping.
#' @param text Optional column/expression for tooltip text (e.g., with plotly).
#' @param label Optional unquoted column/expression used for text labels.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggpoint()], [ggjitter()], [ggscatter()], [ggdensity()],
#'   [gghistogram()], [ggbox()], [ggpie()], [ggmix()], [ggtable()]
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- expand.grid(x = 1:20, color = c("X","Y","Z"))
#' df$y <- sample(1:10, nrow(df), TRUE)
#'
#' ggline(
#'   data = df, x = x, y = y, color = color,
#'   label = y, label_args = list(vjust = -0.4, size = 3)
#' ) + theme_view()
#' }
#'
#' @export
ggline <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                   color = NULL, fill = NULL, group = NULL,
                   linetype = NULL, text = NULL,
                   label,
                   label_args = list(
                     family = getOption("ggshort.font"),
                     size   = 4,
                     angle  = 0,
                     hjust  = 0.5,
                     vjust  = 0.5,
                     color  = "black"
                   )) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y, ymin = ymin, ymax = ymax,
    color = color, fill = fill, group = group,
    linetype = linetype,
    text = text
  ))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_line()

  if (!(missing(ymin_err) && missing(ymax_err))) {
    quos_err <- .valid_enquos(rlang::enquos(
      x = x, ymin = ymin_err, ymax = ymax_err
    ))
    position <- ggplot2::position_dodge2(preserve = "single")
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(!!!quos_err),
      position = position,
      alpha = .5
    )
  }

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_dodge2(width = .9, preserve = "single")
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  p
}

#' ggplot point helpers (frequently used arguments)
#'
#' Build point-based plots (points, jittered points, scatter with extras) with
#' the most common options. Supply **unquoted** column names for aesthetics.
#' These helpers share a consistent interface and `label_args` for text styling.
#'
#' @section Functions:
#' - **ggpoint()**: Basic point plot (`geom_point()`).
#' - **ggjitter()**: Jittered point plot (`geom_jitter()`).
#' - **ggscatter()**: Scatter plot with optional jitter toggle, median/mean
#'   guides, and convex hull.
#'
#' @param data A data.frame.
#' @param x,y Unquoted column names mapped to x and y.
#' @param xmin,xmax Optional lower/upper x bounds (rarelx used for pure points).
#' @param ymin,ymax Optional lower/upper y bounds (rarely used for pure points).
#' @param color,fill,group Optional columns mapped to the `color`, `fill` and
#'   `group` aesthetics.
#' @param alpha Optional column mapped to point transparency.
#' @param shape Optional column mapped to point shape.
#' @param size Optional column mapped to point size.
#' @param text Optional column/expression used for tooltip text (e.g., plotly).
#' @param label Optional unquoted column/expression for text labels.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param show_mean Logical; if `TRUE`, add group/global mean point(s) using
#'   [stat_mean_point()].
#' @param show_median Logical; if `TRUE`, add group/global median point(s) using
#'   [stat_median_point()].
#' @param show_ellipse Logical; if `TRUE`, add confidence ellipse(s) per group
#'   via [ggplot2::stat_ellipse()] (default 90% level).
#' @param show_chull Logical; if `TRUE`, draw convex hull(s) per group using
#'   [stat_chull()].
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggjitter()], [ggscatter()], [ggdensity()],
#'   [gghistogram()], [ggbox()], [ggpie()], [ggmix()], [ggtable()]
#'
#' @examples
#' \donttest{
#' # Basic points: Sepal.Length vs Petal.Width
#' ggpoint(iris, x = Sepal.Length, y = Petal.Width,
#'         color = Species, shape = Species,
#'         label = Species,
#'         label_args = list(size = 3, vjust = -0.5)) +
#'   theme_view()
#'
#' # Jittered points
#' ggjitter(iris, x = Species, y = Petal.Width,
#'          color = Species, shape = Species) +
#'   theme_view()
#'
#' # Scatter with convex hulls per species
#' ggscatter(iris, x = Sepal.Length, y = Petal.Width,
#'           color = Species, fill = Species,
#'           show_median = TRUE, show_chull = TRUE) +
#'   theme_view()
#' }
#'
#' @name ggpoint
#' @export
ggpoint <- function(data,
                    x, y,
                    xmin = NULL, xmax = NULL,
                    ymin = NULL, ymax = NULL,
                    color = NULL, fill = NULL, group = NULL,
                    shape = NULL, size = NULL, alpha = NULL,
                    text = NULL,
                    label,
                    label_args = list(
                      family = getOption("ggshort.font"),
                      size   = 4,
                      angle  = 0,
                      hjust  = 0.5,
                      vjust  = 0.5,
                      color  = "black"
                    ),
                    show_mean = FALSE,
                    show_median = FALSE,
                    show_ellipse = FALSE,
                    show_chull = FALSE) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y,
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    text = text
  ))
  quos_map2 <- .valid_enquos(rlang::enquos(
    color = color, fill = fill, group = group,
    shape = shape, size = size, alpha = alpha
  ))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_point(ggplot2::aes(!!!quos_map2))

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_identity()
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  quos_point <- .valid_enquos(rlang::enquos(group = group))

  if (show_mean)
    p <- p +
      stat_mean_point(stroke = 1.5) +
      stat_mean_point(ggplot2::aes(!!!quos_point),
                      color = "red", stroke = 1)

  if (show_median)
    p <- p +
      stat_median_point(stroke = 1.5) +
      stat_median_point(ggplot2::aes(!!!quos_point),
                        color = "red", stroke = 1)

  if (show_ellipse) {
    quos_ellipse <- .valid_enquos(rlang::enquos(
      color = color, group = group
    ))
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(!!!quos_ellipse), level = .9, alpha = .9
    )
  }

  if (show_chull) {
    quos_chull <- .valid_enquos(rlang::enquos(
      group = group, color = color, fill = fill,
      shape = shape, size = size
    ))
    p <- p + stat_chull(ggplot2::aes(!!!quos_chull))
  }

  p
}

#' @rdname ggpoint
#' @export
ggjitter <- function(data,
                     x, y,
                     xmin = NULL, xmax = NULL,
                     ymin = NULL, ymax = NULL,
                     color = NULL, fill = NULL, group = NULL,
                     shape = NULL, size = NULL, alpha = NULL,
                     text = NULL,
                     label,
                     label_args = list(
                       family = getOption("ggshort.font"),
                       size   = 4,
                       angle  = 0,
                       hjust  = 0.5,
                       vjust  = 0.5,
                       color  = "black"
                     ),
                     show_mean = FALSE,
                     show_median = FALSE,
                     show_ellipse = FALSE,
                     show_chull = FALSE) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y, ymin = ymin, ymax = ymax, text = text
  ))
  quos_map2 <- .valid_enquos(rlang::enquos(
    color = color, fill = fill, group = group,
    shape = shape, size = size, alpha = alpha
  ))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_jitter(ggplot2::aes(!!!quos_map2))

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_jitter()
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  quos_point <- .valid_enquos(rlang::enquos(group = group))

  if (show_mean)
    p <- p +
      stat_mean_point(stroke = 1.5) +
      stat_mean_point(ggplot2::aes(!!!quos_point),
                      color = "red", stroke = 1)

  if (show_median)
    p <- p +
      stat_median_point(stroke = 1.5) +
      stat_median_point(ggplot2::aes(!!!quos_point),
                        color = "red", stroke = 1)

  if (show_ellipse) {
    quos_ellipse <- .valid_enquos(rlang::enquos(
      color = color, group = group
    ))
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(!!!quos_ellipse),
      level = .9, alpha = .9
    )
  }

  if (show_chull) {
    quos_chull <- .valid_enquos(rlang::enquos(
      color = color, fill = fill, group = group,
      shape = shape, size = size
    ))
    p <- p + stat_chull(ggplot2::aes(!!!quos_chull))
  }

  p
}

#' @rdname ggpoint
#' @param jitter For `ggscatter()` only: logical; if `TRUE`, use jittered points.
#' @export
ggscatter <- function(data,
                      x, y,
                      xmin = NULL, xmax = NULL,
                      ymin = NULL, ymax = NULL,
                      color = NULL, fill = NULL, group = NULL,
                      shape = NULL, size = NULL, alpha = NULL, text = NULL,
                      label,
                      label_args = list(
                        family = getOption("ggshort.font"),
                        size   = 4,
                        angle  = 0,
                        hjust  = 0.5,
                        vjust  = 0.5,
                        color  = "black"
                      ),
                      jitter = FALSE,
                      show_mean = FALSE,
                      show_median = FALSE,
                      show_ellipse = FALSE,
                      show_chull = FALSE) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y, ymin = ymin, ymax = ymax, text = text
  ))
  quos_map2 <- .valid_enquos(rlang::enquos(
    color = color, fill = fill, group = group,
    shape = shape, size = size, alpha = alpha
  ))

  geom_fun <- if (jitter) ggplot2::geom_jitter else ggplot2::geom_point

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    geom_fun(ggplot2::aes(!!!quos_map2))

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- if (jitter) {
      ggplot2::position_jitter()
    } else {
      ggplot2::position_identity()
    }
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  quos_point <- .valid_enquos(rlang::enquos(group = group))

  if (show_mean)
    p <- p +
      stat_mean_point(stroke = 1.5) +
      stat_mean_point(ggplot2::aes(!!!quos_point),
                      color = "red", stroke = 1)

  if (show_median)
    p <- p +
      stat_median_point(stroke = 1.5) +
      stat_median_point(ggplot2::aes(!!!quos_point),
                        color = "red", stroke = 1)

  if (show_ellipse) {
    quos_ellipse <- .valid_enquos(rlang::enquos(
      color = color, group = group
    ))
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(!!!quos_ellipse),
      level = .9, alpha = .9
    )
  }

  if (show_chull) {
    quos_chull <- .valid_enquos(rlang::enquos(
      color = color, fill = fill, group = group,
      shape = shape, size = size
    ))
    p <- p + stat_chull(ggplot2::aes(!!!quos_chull))
  }

  p
}

#' ggplot density helper (frequently used arguments)
#'
#' Quickly draw kernel density curves with common options. Supply **unquoted**
#' column names for aesthetics. Supports optional mean/median guides and
#' quantile lines/labels.
#'
#' @param data A data.frame.
#' @param x Unquoted column mapped to the x aesthetic.
#' @param color,fill,group Optional columns mapped to the corresponding
#'   aesthetics.
#' @param probs Numeric vector of probabilities in \[0, 1] for quantile guides
#'   (used when `show_vline`/`show_label` are `TRUE`). Default `0.95`.
#' @param na.rm Logical; remove missing values silently if `TRUE`. Default `TRUE`.
#' @param y Numeric y position for quantile labels (used by `show_label`).
#'   Default `Inf` (top of panel).
#' @param alpha Numeric in \[0,1]; transparency for the box layer
#'   (passed to [ggplot2::geom_boxplot()]). Default `0.6`.
#' @param show_mean,show_median Logical; add mean/median vertical guides
#'   (via `stat_mean_vline()` / `stat_median_vline()`). Defaults `FALSE`.
#' @param show_vline Logical; draw vertical quantile line(s) at `probs`
#'   (via `stat_density_quantile_vline()`). Default `FALSE`.
#' @param show_label Logical; add quantile label(s) at `probs`
#'   (via `stat_density_quantile()`). Default `TRUE`.
#' @param label_digits Integer; number of decimal digits for quantile labels.
#'   Default `1`.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggbox()], [ggpie()], [ggmix()], [ggtable()],
#'   [stat_density_quantile_vline()], [stat_density_quantile()]
#'
#' @examples
#' \donttest{
#' # Basic density by group with 50% quantile line (median) and label
#' ggdensity(iris, x = Sepal.Length, color = Species, fill = Species,
#'           probs = 0.5, show_vline = TRUE, show_label = TRUE) +
#'   theme_view()
#' }
#'
#' @export
ggdensity <- function(data, x, color = NULL, fill = NULL, group = NULL,
                      probs = .95, na.rm = TRUE, y = Inf, alpha = .6,
                      show_mean = FALSE,
                      show_median = FALSE,
                      show_vline = TRUE,
                      show_label = TRUE,
                      label_digits = 1,
                      label_args = list(
                        family = getOption("ggshort.font"),
                        size  = 4,
                        angle = 90,
                        hjust = 2,
                        vjust = 0.5,
                        color = "black"
                      )) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, color = color, fill = fill, group = group
  ))

  p <- ggplot2::ggplot(data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_density(alpha = alpha)

  if (show_mean)
    p <- p + stat_mean_vline(na.rm = na.rm, linetype = "solid")

  if (show_median)
    p <- p + stat_median_vline(na.rm = na.rm, linetype = "dotdash")

  if (show_vline)
    p <- p + stat_density_quantile_vline(probs = probs, na.rm = na.rm)

  if (show_label) {
    args <- .modify_label_args(label_args)
    fmt <- sprintf("%%.%df (%%.1f%%%%)", label_digits)
    p <- p + stat_density_quantile_text(
      probs = probs, na.rm = na.rm, y = y,
      fmt = function(p, q) sprintf(fmt, q, p * 100),
      family = args$family, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust
    )
  }
  p
}

#' ggplot histogram helper (frequently used arguments)
#'
#' Quickly draw histograms with common options. Supply **unquoted**
#' column names for aesthetics. Supports optional mean/median guides and
#' quantile lines/labels.
#'
#' @param data A data.frame.
#' @param x Unquoted column mapped to the x aesthetic.
#' @param color,fill,group Optional columns mapped to the corresponding
#'   aesthetics.
#' @param probs Numeric vector of probabilities in \[0, 1] for quantile guides
#'   (used when `show_vline`/`show_label` are `TRUE`). Default `0.95`.
#' @param na.rm Logical; remove missing values silently if `TRUE`. Default `TRUE`.
#' @param y Numeric y position for quantile labels (used by `show_label`).
#'   Default `Inf` (top of panel).
#' @param alpha Numeric in \[0,1]; transparency for the box layer
#'   (passed to [ggplot2::geom_boxplot()]). Default `0.6`.
#' @param bins Number of bins for the histogram. Ignored if `binwidth` is
#'   specified. Default `30`.
#' @param binwidth Width of each histogram bin. Overrides `bins` if provided.
#'   Should be a single numeric value.
#' @param show_mean,show_median Logical; add mean/median vertical guides
#'   (via `stat_mean_vline()` / `stat_median_vline()`). Defaults `FALSE`.
#' @param show_vline Logical; draw vertical quantile line(s) at `probs`
#'   (via `stat_density_quantile_vline()`). Default `TRUE`.
#' @param show_label Logical; add quantile label(s) at `probs`
#'   (via `stat_density_quantile_text()`). Default `TRUE`.
#' @param label_digits Integer; number of decimal digits for quantile labels.
#'   Default `1`.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggdensity()], [ggbox()], [ggpie()], [ggmix()], [ggtable()],
#'   [stat_density_quantile_vline()], [stat_density_quantile_text()]
#'
#' @examples
#' \donttest{
#' # Basic histogram with 95% quantile line and label
#' gghistogram(iris, x = Sepal.Length, fill = Species,
#'             probs = 0.95, show_vline = TRUE, show_label = TRUE) +
#'   theme_view()
#'
#' # Use custom binwidth instead of fixed bins
#' gghistogram(iris, x = Sepal.Length, binwidth = 0.2) +
#'   theme_view()
#' }
#'
#' @export
gghistogram <- function(data, x, color = NULL, fill = NULL, group = NULL,
                        probs = .95, na.rm = TRUE, y = Inf, alpha = .6,
                        bins = 30, binwidth = NULL,
                        show_mean = FALSE,
                        show_median = FALSE,
                        show_vline = TRUE,
                        show_label = TRUE,
                        label_digits = 1,
                        label_args = list(
                          family = getOption("ggshort.font"),
                          size  = 4,
                          angle = 90,
                          hjust = 2,
                          vjust = 0.5,
                          color = "black"
                        )) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, color = color, fill = fill, group = group
  ))

  p <- ggplot2::ggplot(data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_histogram(alpha = alpha, bins = bins, binwidth = binwidth)

  if (show_mean)
    p <- p + stat_mean_vline(na.rm = na.rm, linetype = "solid")

  if (show_median)
    p <- p + stat_median_vline(na.rm = na.rm, linetype = "dotdash")

  if (show_vline)
    p <- p + stat_density_quantile_vline(probs = probs, na.rm = na.rm)

  if (show_label) {
    args <- .modify_label_args(label_args)
    fmt <- sprintf("%%.%df (%%.1f%%%%)", label_digits)
    p <- p + stat_density_quantile_text(
      probs = probs, na.rm = na.rm, y = y,
      fmt = function(p, q) sprintf(fmt, q, p * 100),
      family = args$family, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust
    )
  }
  p
}

#' ggplot boxplot helper (frequently used arguments)
#'
#' Quickly draw boxplots with common options. Supply **unquoted**
#' column names for aesthetics. Supports optional mean/median markers
#' and overlaid jittered points.
#'
#' @param data A data.frame.
#' @param x,y Unquoted column names mapped to the x and y aesthetics.
#' @param color,fill,group Optional columns mapped to the `color`, `fill` and
#'   `group` aesthetics.
#' @param text Optional column/expression used for tooltip text (e.g., plotly).
#' @param alpha Numeric in \[0,1]; transparency for the box layer
#'   (passed to [ggplot2::geom_boxplot()]). Default `0.6`.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param width Box width passed to [ggplot2::geom_boxplot()] (default `0.6`).
#' @param varwidth Logical; if `TRUE`, boxes are drawn with widths proportional
#'   to the square-roots of the number of observations in the groups.
#' @param show_point Logical; if `TRUE`, overlay jittered points
#'   ([ggplot2::geom_jitter()]) on top of the boxplots.
#' @param show_mean Logical; if `TRUE`, add mean markers
#'   using [stat_mean_point()]
#' @param show_label Logical; if `TRUE`, print the **x group label** at the
#'   box **median** (added via `stat_summary(geom = "text")`). Default `FALSE`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggdensity()], [gghistogram()], [ggpie()], [ggmix()], [ggtable()]
#'
#' @examples
#' \donttest{
#' # Basic grouped boxplot with jittered points
#' ggbox(iris, x = Species, y = Sepal.Length, fill = Species,
#'       show_point = TRUE) +
#'   theme_view()
#'
#' # Add labels at the middle of each box (here, the group name)
#' ggbox(iris, x = Species, y = Sepal.Length, fill = Species,
#'       label_args = list(vjust = -0.4), show_label = TRUE) +
#'   theme_view()
#' }
#'
#' @export
ggbox <- function(data, x, y,
                  color = NULL, fill = NULL, group = NULL,
                  text = NULL, alpha = .6,
                  show_point = TRUE,
                  show_mean = TRUE,
                  show_label = FALSE,
                  label_args = list(
                    family = getOption("ggshort.font"),
                    size   = 4,
                    angle  = 0,
                    hjust  = .5,
                    vjust  = -.25,
                    color  = "black"
                  ),
                  width = .6,
                  varwidth = FALSE) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y,
    color = color, fill = fill, group = group,
    text = text
  ))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_boxplot(width = width, varwidth = varwidth,
                          outlier.shape = if (show_point) NA else 19,
                          alpha = alpha)

  if (show_point) {
    quos_point <- .valid_enquos(rlang::enquos(
      color = color, group = group
    ))
    p <- p + ggplot2::geom_jitter(
      ggplot2::aes(!!!quos_point),
      width = width * .4, height = 0, alpha = .5
    )
  }

  if (show_label) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = x))
    # add labels at median
    p <- p + ggplot2::stat_summary(
      ggplot2::aes(!!!quos_label),
      fun = stats::median, geom = "text",
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  if (show_mean) {
    quos_group <- .valid_enquos(rlang::enquos(group = group))
    p <- p +
      stat_mean_point(stroke = 2) +
      stat_mean_point(ggplot2::aes(!!!quos_group), color = "red", stroke = 1)
  }

  p
}

#' ggplot pie helper (frequently used arguments)
#'
#' Quickly build pie charts with common options. Supply **unquoted** column
#' names for aesthetics. Supports text labels styled via `label_args`.
#'
#' @param data A data.frame.
#' @param group Unquoted column used for grouping (slices of the pie).
#' @param value Unquoted column specifying values (slice sizes).
#' @param text Optional column/expression for tooltip text (e.g., with plotly).
#' @param label Optional unquoted column/expression used as slice labels.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggdensity()], [gghistogram()], [ggbox], [ggmix()], [ggtable()]
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- data.frame(group = c("A", "B", "C"), value = c(60, 30, 10))
#'
#' ggpie(df, group = group, value = value,
#'       label = sprintf("%s%%", value),
#'       label_args = list(size = 5, family = getOption("ggshort.font")))
#' }
#'
#' @export
ggpie <- function(data, group, value, text,
                  label,
                  label_args = list(
                    family = getOption("ggshort.font"),
                    size   = 4,
                    angle  = 0,
                    hjust  = 0.5,
                    vjust  = 0.5,
                    color  = "black"
                  )) {
  quos_map <- .valid_enquos(rlang::enquos(
    y = value, group = group, fill = group, text = text
  ))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = 0, !!!quos_map))+
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::coord_polar("y", start = 0)

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_stack(vjust = .5)
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  base_family <- label_args$family %||% getOption("ggshort.font")
  p + ggplot2::theme_void(base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = .5),
      plot.subtitle = ggplot2::element_text(hjust = .5)
    )
}

#' ggplot mix helper (stacked 100% bars with common options)
#'
#' Build **proportion (100%) stacked bars** quickly with the most-used options.
#' Supply **unquoted** column names for aesthetics. Supports text labels and
#' reversing the stacking order.
#'
#' @param data A data.frame.
#' @param x,y Unquoted columns mapped to x and y (heights). Typically `y` is a value
#'   to be summed per `x` and `fill`, then normalized by `position_fill()`.
#' @param ymin,ymax Optional lower/upper bounds for y (rarely needed here).
#' @param color,fill,group Optional columns for grouping and fill (stacking is based on `fill`).
#' @param text Optional column/expression for tooltip text (e.g., with plotly).
#' @param label Optional unquoted column/expression used for `geom_text()`.
#' @param label_args A named list of `geom_text()` style options. Supported keys:
#'   `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param reverse Logical; if `TRUE`, reverse the stacking order in `position_fill()`.
#' @param flip Logical; if `TRUE` (default), use [ggplot2::coord_flip()] to
#'   flip the axes so categories are on the y-axis and values on the x-axis.
#'
#' @return A ggplot object.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggdensity()], [gghistogram()], [ggpie()], [ggbox], [ggtable()]
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- expand.grid(x = c("A","B","C"), fill = c("X","Y","Z"))
#' df$y <- sample(1:10, nrow(df), TRUE)
#'
#' ggmix(
#'   data = df, x = x, y = y, fill = fill,
#'   label = y,
#'   label_args = list(size = 3),
#'   reverse = TRUE
#' ) + theme_view()
#' }
#'
#' @export
ggmix <- function(data, x, y, ymin = NULL, ymax = NULL,
                  color = NULL, fill = NULL, group = NULL,
                  text = NULL,
                  label,
                  label_args = list(
                    family = getOption("ggshort.font"),
                    size   = 4,
                    angle  = 0,
                    hjust  = 0.5,
                    vjust  = 0.5,
                    color  = "black"
                  ), reverse = TRUE, flip = TRUE) {
  quos_map <- .valid_enquos(rlang::enquos(
    x = x, y = y, ymin = ymin, ymax = ymax,
    color = color, fill = fill, group = group,
    text = text
  ))

  position <- ggplot2::position_fill(vjust = .5, reverse = reverse)

  p <- ggplot2::ggplot(data = data, ggplot2::aes(!!!quos_map)) +
    ggplot2::geom_bar(stat = "identity", position = position)

  if (!missing(label)) {
    args <- .modify_label_args(label_args)
    quos_label <- .valid_enquos(rlang::enquos(label = label))
    position <- ggplot2::position_fill(vjust = .5, reverse = reverse)
    p <- p + ggplot2::geom_text(
      ggplot2::aes(!!!quos_label),
      position = position,
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  if (flip) {
    p <- p + ggplot2::coord_flip()
  }

  p
}

#' ggplot table helper (frequently used arguments)
#' ggplot table helper
#'
#' Create a table-like plot using `ggplot2`. Values mapped to `x` and `y`
#' are coerced to factors when possible, then converted to numeric positions
#' so that each cell can be drawn on a regular grid. Labels are placed inside
#' cells, optional background fill can be applied by a threshold rule, and
#' table borders are drawn with vertical and horizontal grid lines.
#'
#' Unlike many other ggshort helpers, `x`, `y`, `label`, and `fill` are first
#' evaluated and stored as temporary columns. This allows `ggtable()` to accept
#' bare column names, quasiquoted symbols, `.data[[...]]` expressions, and
#' other expressions that evaluate to vectors of the same length as `data`.
#'
#' @param data A data.frame.
#' @param x,y Unquoted columns or expressions mapped to the table axes.
#'   Character, numeric, and Date values are automatically converted to factors.
#' @param linetype Line type for table grid lines. One of `"solid"` (default),
#'   `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, or `"twodash"`.
#' @param text Optional column/expression for tooltip text (for example, when
#'   used with plotly).
#' @param label Unquoted column/expression used as text labels inside cells.
#' @param label_args A named list of `geom_text()` style options.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param fill Optional numeric column/expression used to determine cell
#'   background color. If supplied, each cell is filled according to `fill_args`.
#' @param fill_args A named list controlling conditional cell background fill.
#'   Supported keys:
#'   \describe{
#'     \item{threshold}{Numeric threshold value (or a length-2 vector for
#'       `"inside"` / `"outside"` conditions). Required when `fill` is used.}
#'     \item{when}{Condition to apply. One of `">"`, `">="`, `"<"`, `"<="`,
#'       `"inside"`, or `"outside"`. Default is `">"`.}
#'     \item{high}{Fill color for cells satisfying the condition.
#'       Default is `"mistyrose"`.}
#'     \item{low}{Fill color for cells not satisfying the condition.
#'       Default is `"white"`.}
#'     \item{na}{Fill color for `NA` values. Default is `"white"`.}
#'     \item{border}{Border color for cells passed to `geom_tile()`.
#'       Default is `NA` (no border).}
#'     \item{linewidth}{Border line width for cells. Default is `0.2`.}
#'     \item{alpha}{Alpha transparency for cell fill. Default is `1`.}
#'   }
#' @param xlab_position Position of x-axis labels, one of `"bottom"` (default)
#'   or `"top"`.
#' @param ylab_position Position of y-axis labels, one of `"left"` (default)
#'   or `"right"`.
#'
#' @details
#' `ggtable()` is designed for compact, matrix-like displays such as triangles,
#' scorecards, and labeled heatmap tables.
#'
#' Axis values supplied to `x` and `y` are coerced to factors with sorted levels
#' (if they are character, numeric, or Date), then drawn as equally spaced cells.
#'
#' When `fill` is supplied, background colors are assigned conditionally using
#' `fill_args$threshold` and `fill_args$when`.
#'
#' @return A ggplot object representing a table-like layout.
#'
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggscatter()],
#'   [ggdensity()], [gghistogram()], [ggbox()], [ggpie()], [ggmix()]
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- expand.grid(x = c("A", "B", "C"), y = c("X", "Y", "Z"))
#' df$label <- sample(1:10, size = 9, replace = TRUE)
#'
#' ggtable(
#'   df,
#'   x = x,
#'   y = y,
#'   label = label,
#'   label_args = list(size = 5, family = getOption("ggshort.font")),
#'   xlab_position = "top"
#' ) +
#'   theme_view()
#'
#' ggtable(
#'   df,
#'   x = x,
#'   y = y,
#'   label = label,
#'   fill = label,
#'   fill_args = list(
#'     threshold = 5,
#'     high = "mistyrose"
#'   )
#' ) +
#'   theme_view()
#'
#' x_var <- "x"
#' y_var <- "y"
#' ggtable(
#'   df,
#'   x = .data[[x_var]],
#'   y = .data[[y_var]],
#'   label = label
#' ) +
#'   theme_view()
#' }
#'
#' @export
ggtable <- function(data, x, y, linetype = "solid", text = NULL,
                    label,
                    label_args = list(
                      family = getOption("ggshort.font"),
                      size   = 4,
                      angle  = 0,
                      hjust  = 0.5,
                      vjust  = 0.5,
                      color  = "black"
                    ),
                    fill = NULL,
                    fill_args = list(
                      threshold = NULL,
                      when      = ">",
                      high      = "mistyrose",
                      low       = "white",
                      na        = "white",
                      border    = NA,
                      linewidth = 0.2,
                      alpha     = 1
                    ),
                    xlab_position = c("bottom", "top"),
                    ylab_position = c("left", "right")) {

  data <- data.table::as.data.table(data)

  qx     <- rlang::enquo(x)
  qy     <- rlang::enquo(y)
  qtext  <- rlang::enquo(text)
  qlabel <- rlang::enquo(label)
  qfill  <- rlang::enquo(fill)

  use_text <- !(rlang::quo_is_null(qtext) || rlang::quo_is_missing(qtext))
  use_fill <- !(rlang::quo_is_null(qfill) || rlang::quo_is_missing(qfill))

  # evaluate inputs
  data[[".ggtable_x"]]     <- rlang::eval_tidy(qx, data = data)
  data[[".ggtable_y"]]     <- rlang::eval_tidy(qy, data = data)
  data[[".ggtable_label"]] <- rlang::eval_tidy(qlabel, data = data)

  if (use_text) {
    data[[".ggtable_text"]] <- rlang::eval_tidy(qtext, data = data)
  }

  if (use_fill) {
    data[[".ggtable_fill_value"]] <- rlang::eval_tidy(qfill, data = data)
  }

  # axis labels
  x_lab <- tryCatch(
    instead::capture_names(data, !!qx),
    error = function(e) rlang::as_label(qx)
  )
  y_lab <- tryCatch(
    instead::capture_names(data, !!qy),
    error = function(e) rlang::as_label(qy)
  )

  if (length(x_lab) != 1L) x_lab <- rlang::as_label(qx)
  if (length(y_lab) != 1L) y_lab <- rlang::as_label(qy)

  # coerce axes to factors
  data[[".ggtable_x"]] <- .coerce_to_factor(data[[".ggtable_x"]], "x")
  data[[".ggtable_y"]] <- .coerce_to_factor(data[[".ggtable_y"]], "y")

  x_lvl <- levels(data[[".ggtable_x"]])
  y_lvl <- levels(data[[".ggtable_y"]])
  x_len <- length(x_lvl)
  y_len <- length(y_lvl)

  data[[".ggtable_x"]] <- as.numeric(data[[".ggtable_x"]])
  data[[".ggtable_y"]] <- as.numeric(data[[".ggtable_y"]])

  # fill handling
  fill_defaults <- list(
    threshold = NULL,
    when      = ">",
    high      = "mistyrose",
    low       = "white",
    na        = "white",
    border    = NA,
    linewidth = 0.2,
    alpha     = 1
  )
  fa <- utils::modifyList(fill_defaults, fill_args, keep.null = TRUE)

  if (use_fill) {
    if (is.null(fa$threshold)) {
      stop("fill_args$threshold must be provided when `fill` is used.", call. = FALSE)
    }

    v <- data[[".ggtable_fill_value"]]
    if (!is.numeric(v)) {
      stop("`fill` must evaluate to a numeric vector.", call. = FALSE)
    }

    threshold <- fa$threshold
    when <- match.arg(fa$when, c(">", ">=", "<", "<=", "outside", "inside"))

    flag <- switch(
      when,
      `>`  = v > threshold,
      `>=` = v >= threshold,
      `<`  = v < threshold,
      `<=` = v <= threshold,
      outside = {
        if (length(threshold) != 2L) {
          stop("For `when = 'outside'`, threshold must be c(lo, hi).", call. = FALSE)
        }
        v < min(threshold) | v > max(threshold)
      },
      inside = {
        if (length(threshold) != 2L) {
          stop("For `when = 'inside'`, threshold must be c(lo, hi).", call. = FALSE)
        }
        v >= min(threshold) & v <= max(threshold)
      }
    )

    data[[".ggtable_fill"]] <- ifelse(
      is.na(v), fa$na,
      ifelse(flag, fa$high, fa$low)
    )
  } else {
    data[[".ggtable_fill"]] <- fa$low
  }

  la <- .modify_label_args(label_args)
  xlab_position <- match.arg(xlab_position)
  ylab_position <- match.arg(ylab_position)

  # plot
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[".ggtable_x"]],
      y = .data[[".ggtable_y"]]
    )
  )

  if (use_fill) {
    p <- p +
      ggplot2::geom_tile(
        ggplot2::aes(fill = .data[[".ggtable_fill"]]),
        width = 1,
        height = 1,
        alpha = fa$alpha,
        color = fa$border,
        linewidth = fa$linewidth
      ) +
      ggplot2::scale_fill_identity()
  }

  p <- p +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[".ggtable_label"]]),
      family = la$family,
      size   = la$size,
      angle  = la$angle,
      hjust  = la$hjust,
      vjust  = la$vjust,
      color  = la$color
    ) +
    ggplot2::geom_vline(
      xintercept = seq(1, x_len + 1) - 0.5,
      linetype = linetype
    ) +
    ggplot2::geom_hline(
      yintercept = seq(1, y_len + 1) - 0.5,
      linetype = linetype
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(x_len),
      labels = x_lvl,
      limits = c(0.5, x_len + 0.5),
      expand = c(0, 0),
      position = xlab_position
    ) +
    ggplot2::scale_y_reverse(
      breaks = seq_len(y_len),
      labels = y_lvl,
      limits = c(y_len + 0.5, 0.5),
      expand = c(0, 0),
      position = ylab_position
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab
    )

  p
}



# Internal helper functions -----------------------------------------------

#' Keep only valid quosures (not missing, not NULL)
#'
#' Utility to filter quosures returned by [rlang::enquos()],
#' dropping those that are missing or explicitly NULL.
#'
#' @param quos A list of quosures (from [rlang::enquos()])
#'
#' @return A filtered list of quosures
#'
#' @keywords internal
.valid_enquos <- function(quos) {
  Filter(function(quo) {
    !(rlang::quo_is_missing(quo) || rlang::quo_is_null(quo))
  }, quos)
}

#' Merge label arguments with defaults
#'
#' Internal helper to merge user-supplied `label_args` with a set of defaults.
#' Used by ggshort plotting helpers (e.g., `ggbar`, `ggline`, `ggpoint`).
#'
#' @param label_args A named list of label arguments passed by the user.
#'   Recognized keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @return A named list of label arguments with defaults filled in.
#'
#' @keywords internal
#' @noRd
.modify_label_args <- function(label_args) {
  defaults <- list(
    family = getOption("ggshort.font"),
    size   = 4,
    angle  = 0,
    hjust  = 0.5,
    vjust  = 0.5,
    color  = "black"
  )
  utils::modifyList(defaults, label_args, keep.null = TRUE)
}

#' Coerce input to factor with stable ordering
#'
#' Internal helper to ensure that variables used in table-like plots
#' (e.g., [ggtable()]) are treated as factors.
#'
#' The function converts character, numeric, and Date vectors into factors
#' with levels sorted in ascending order. Existing factors are returned
#' unchanged.
#'
#' @param x A vector to be coerced into a factor.
#' @param var_name Optional string used in error messages to identify
#'   the input variable (e.g., `"x"` or `"y"`). Defaults to `"input"`.
#'
#' @return A factor vector with sorted levels.
#'
#' @details
#' Supported input types:
#' \itemize{
#'   \item factor (returned as-is)
#'   \item character (converted to factor)
#'   \item numeric (converted to factor)
#'   \item Date (converted to factor with chronological ordering)
#' }
#'
#' Unsupported types (e.g., list, POSIXct) will raise an error.
#'
#' @keywords internal
#' @noRd
.coerce_to_factor <- function(x, var_name = NULL) {
  if (is.factor(x))
    return(x)

  if (is.character(x) || is.numeric(x) || inherits(x, "Date"))
    return(factor(x, levels = sort(unique(x))))

  if (is.null(var_name)) var_name <- "input"

  stop(
    sprintf("`%s` must be factor, character, numeric, or Date. Got: %s",
            var_name, class(x)[1L]),
    call. = FALSE
  )
}
