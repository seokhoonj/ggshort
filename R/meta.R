#' Plot for meta objects
#'
#' Visualize summary information from a meta object:
#' a left panel with variable-level metadata (class, distinct, mode),
#' and a right panel with bar charts of `prop`/`nzprop`.
#'
#' @param x A meta object created by [instead::meta()].
#' @param widths Numeric vector of length 2 giving relative widths of the
#'   left (table) and right (bars) panels. Default: `c(5, 5)`.
#' @param text_width Integer, maximum number of display characters. Default: `14`
#' @param title,subtitle,caption Plot title, subtitle, and caption. Passed to
#'   [ggplot2::labs()].
#' @param theme One of `"view"`, `"save"`, or `"shiny"`; passed to [switch_theme()].
#' @param y.size Font sizes for an y-axis. Defaults: `0`.
#' @param ... Additional arguments forwarded to [switch_theme()] for both panels.
#'
#' @return A ggplot object that wraps the combined gtable.
#'
#' @seealso [plot.meta()]
#'
#' @examples
#' \dontrun{
#' mt <- instead::meta(mtcars)
#' plot_meta(mt)  # draws and returns a gtable
#' }
#'
#' @export
plot_meta <- function(x, widths = c(5, 5), text_width = 14,
                      title = NULL, subtitle = NULL, caption = NULL,
                      theme = c("view", "save", "shiny"),
                      y.size = 0, ...) {
  if (!inherits(x, "meta"))
    stop("`x` must be a meta object.")

  theme <- match.arg(theme)

  number <- value <- variable <- NULL

  if (inherits(x, "data.table")) {
    dt <- data.table::copy(x)
  } else {
    dt <- data.table::as.data.table(x)
  }

  data.table::set(dt, j = "number"  , value = seq_along(dt$column))
  data.table::set(dt, j = "distinct", value = instead::as_comma(dt$distinct))
  data.table::set(dt, j = "mode"    , value = as.character(dt$mode))
  data.table::set(dt, j = "column"  , value = factor(dt$column, levels = dt$column))

  # Left panel data
  d1 <- data.table::melt(
    dt,
    id.vars = c("number"),
    measure.vars = c("column", "class", "distinct", "mode")
  )

  data.table::set(
    d1, j = "value",
    value = instead::truncate_text(d1$value, width = text_width)
  )
  data.table::set(
    d1, j = "info" ,
    value = sprintf(
      "NC: %s, NR: %s, NU: %s",
      instead::as_comma(attr(x, "ncol")),
      instead::as_comma(attr(x, "nrow")),
      instead::as_comma(attr(x, "nunique")))
  )

  # Right panel data
  d2 <- data.table::melt(
    dt,
    id.vars = c("number"),
    measure.vars = c("prop", "nzprop")
  )

  # Left panel plot
  p1 <- ggtable(d1, x = variable, y = number, label = value,
                xlab_position = "bottom", linetype = "solid") +
    ggplot2::facet_wrap(~ info) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    switch_theme(theme = theme, y.size = y.size, ...) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) # last for ovelay

  # Right panel plot
  n <- nrow(dt)

  p2 <- ggbar(d2, x = number, y = value, ymax = 1.5,
              label = sprintf("%.1f", value * 100),
              label_args = list(hjust = -.1)) +
    ggplot2::geom_vline(
      xintercept = seq(1, 1 + n) - .5,
      linetype = "solid"
    ) +
    ggplot2::scale_x_reverse(
      breaks = seq_len(n), labels = seq_len(n),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .5, 1, 1.5), labels = c("0", 50, 100, ""),
      limits = c(0, 1.5), expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ variable) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    switch_theme(theme = theme, y.size = y.size, ...) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  # Add title, subtitle, and caption
  if (!is.null(title)) {
    p1 <- p1 + ggplot2::labs(title = title)
    p2 <- p2 + ggplot2::labs(title = "")
  }
  if (!is.null(subtitle)) {
    p1 <- p1 + ggplot2::labs(subtitle = subtitle)
    p2 <- p2 + ggplot2::labs(subtitle = "")
  }
  if (!is.null(caption)) {
    p1 <- p1 + ggplot2::labs(caption = "")
    p2 <- p2 + ggplot2::labs(caption = caption)
  }

  p <- hstack_plots(p1, p2, widths = widths)

  grob_to_ggplot(p)
}

#' Plot method for meta objects
#'
#' S3 method for [plot()] that delegates to [plot_meta()] for objects of class
#' `meta`.
#'
#' @inheritParams plot_meta
#' @param ... Additional arguments forwarded to [plot_meta()].
#'
#' @return A ggplot object created by [plot_meta()].
#'
#' @seealso [plot_meta()]
#'
#' @examples
#' \dontrun{
#' mt <- instead::meta(mtcars)
#' plot(mt)  # equivalent to metaplot(mt)
#' }
#'
#' @method plot meta
#' @export
plot.meta <- function(x, widths = c(5, 5), text_width = 10,
                      theme = c("view", "save", "shiny"),
                      y.size = 0, ...) {
  theme <- match.arg(theme)
  plot_meta(x = x, widths = widths, text_width = text_width,
            theme = theme, y.size = y.size, ...)
}
