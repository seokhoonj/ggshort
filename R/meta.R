#' Plot for meta objects
#'
#' Visualize summary information from a meta object:
#' a left panel with variable-level metadata (class, distinct, mode),
#' and a right panel with bar charts of `prop`/`nzprop`.
#'
#' @param x A meta object created by [jaid::meta()].
#' @param widths Numeric vector of length 2 giving relative widths of the
#'   left (table) and right (bars) panels. Default: `c(5, 5)`.
#' @param title,subtitle,caption Plot title, subtitle, and caption. Passed to
#'   [ggplot2::labs()].
#' @param theme One of `"view"`, `"save"`, or `"shiny"`; passed to [switch_theme()].
#' @param ... Additional arguments forwarded to [switch_theme()] for both panels.
#'
#' @return A ggplot object that wraps the combined gtable.
#'
#' @seealso [plot.meta()]
#'
#' @examples
#' \dontrun{
#' mt <- jaid::meta(mtcars)
#' meta_plot(mt)  # draws and returns a gtable
#' }
#'
#' @export
meta_plot <- function(x, widths = c(5, 5),
                      title = NULL, subtitle = NULL, caption = NULL,
                      theme = c("view", "save", "shiny"),
                      ...) {
  if (!requireNamespace("jaid", quietly = TRUE))
    stop("Package 'jaid' is required for meta_plot(); install it with install.packages('jaid').",
         call. = FALSE)
  if (!inherits(x, "meta"))
    stop("`x` must be a meta object.")

  theme <- match.arg(theme)

  column <- value <- variable <- NULL

  x$column <- sprintf("%d.%s", seq_along(x$column), x$column)
  x$distinct <- as.character(x$distinct)
  x$mode <- as.character(x$mode)

  dt <- data.table::melt(
    data.table::as.data.table(x),
    id.vars = c("column"),
    measure.vars = c("prop", "nzprop")
  )
  dt$column <- factor(dt$column, levels = unique(dt$column))
  xlvl <- levels(dt$column)
  dt$column <- as.numeric(dt$column)

  ds <- data.table::melt(
    data.table::as.data.table(x),
    id.vars = c("column"),
    measure.vars = c("class", "distinct", "mode")
  )
  ds$column <- factor(ds$column, levels = xlvl)
  ds$info <- sprintf("NC: %d, NR, UQ", nrow(x)) #, attr(x, "nrow"))

  # left panel
  p1 <-
    ggtable(
      ds, x = variable, y = column, label = value,
      xlab_position = "bottom", linetype = "solid"
    ) +
    ggplot2::facet_wrap(~ info) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    switch_theme(theme = "view", ...) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) # last for ovelay

  # right panel
  p2 <-
    ggbar(
      dt, x = column, y = value, ymax = 1.5,
      label = sprintf("%.1f", value * 100),
      label_args = list(hjust = -.1)
    ) +
    ggplot2::geom_vline(
      xintercept = seq(1, 1 + length(xlvl)) - .5,
      linetype = "solid"
    ) +
    ggplot2::scale_x_reverse(
      breaks = seq_along(xlvl), labels = xlvl,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, .5, 1, 1.5), labels = c("0", 50, 100, ""),
      limits = c(0, 1.5), expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ variable) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    switch_theme(theme = theme, y.size = 0, ...)

  if (!is.null(title)) {
    p1 <- p1 + labs(title = title)
    p2 <- p2 + labs(title = "")
  }
  if (!is.null(subtitle)) {
    p1 <- p1 + labs(subtitle = subtitle)
    p2 <- p2 + labs(subtitle = "")
  }
  if (!is.null(caption)) {
    p1 <- p1 + labs(caption = "")
    p2 <- p2 + labs(caption = caption)
  }

  p <- hstack_plots(p1, p2, widths = widths)
  grob_to_ggplot(p)
}

#' Plot method for meta objects
#'
#' S3 method for [plot()] that delegates to [meta_plot()] for objects of class
#' `meta`.
#'
#' @inheritParams meta_plot
#' @param ... Additional arguments forwarded to [meta_plot()].
#'
#' @return A ggplot object created by [meta_plot()].
#'
#' @seealso [meta_plot()]
#'
#' @examples
#' \dontrun{
#' mt <- jaid::meta(mtcars)
#' plot(mt)  # equivalent to metaplot(mt)
#' }
#'
#' @method plot meta
#' @export
plot.meta <- function(x, widths = c(5, 5), theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  meta_plot(x = x, widths = widths, theme = theme)
}
