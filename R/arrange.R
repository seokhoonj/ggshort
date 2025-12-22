#' Arrange multiple plots with a shared legend
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Helpers for arranging two ggplot objects together with a legend.
#'
#' - `grid_top_bottom_right()`: place one plot on top, another below,
#'   and the legend to the right.
#' - `grid_left_to_right()`: place two plots side by side with the legend on the right.
#'
#' @param g1,g2 ggplot objects to arrange.
#' @param legend A gtable legend object (as from [get_legend()]). If missing,
#'   the legend is extracted from `g1`.
#' @param heights Numeric vector of relative heights for top/bottom plots (default `c(5, 5)`).
#' @param widths Numeric vector of relative widths for plots vs legend
#'   (default `c(8.5, 1.5)` for `grid_top_bottom_right()`,
#'   `c(4, 4, 2)` for `grid_left_to_right()`).
#'
#' @return A gtable object (arranged grid). The object is also drawn by default.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- expand.grid(x = c("A","B","C"), fill = c("X","Y","Z"))
#' df$y <- sample(1:10, 9, TRUE)
#' g1 <- ggbar(df, x = x, y = y, fill = fill, label = y,
#'             label_args = list(vjust = -0.25)) +
#'   theme_view()
#'
#' df2 <- expand.grid(x = 1:20, color = c("X","Y","Z"))
#' df2$y <- sample(1:10, nrow(df2), TRUE)
#' g2 <- ggline(df2, x = x, y = y, color = color) +
#'   theme_view()
#'
#' grid_top_bottom_right(g1, g2)
#' grid_left_to_right(g1, g2)
#' }
#'
#' @keywords internal
#' @noRd
grid_top_bottom_right <- function(g1, g2, legend, heights = c(5, 5),
                                  widths = c(8.5, 1.5)) {
  lifecycle::deprecate_warn(
    "0.0.0.9001", "grid_top_bottom_right()", "vstack_plots_with_legend()"
  )
  if (missing(legend))
    legend <- get_legend(g1)
  g1 <- g1 + theme(legend.position = "none")
  g2 <- g2 + theme(legend.position = "none")
  g <- gridExtra::arrangeGrob(
    gridExtra::arrangeGrob(
      ggplotGrob(g1),
      ggplotGrob(g2), nrow = 2, heights = heights
    ), legend, ncol = 2, widths = widths
  )
  gridExtra::grid.arrange(g)
  invisible(g)
}

#' @rdname grid_top_bottom_right
#' @keywords internal
#' @noRd
grid_left_to_right <- function (g1, g2, legend, widths = c(4, 4, 2)) {
  lifecycle::deprecate_warn(
    "0.0.0.9001", "grid_left_to_right()", "hstack_plots_with_legend()"
  )
  if (missing(legend))
    legend <- get_legend(g1)
  g1 <- g1 + theme(legend.position = "none")
  g2 <- g2 + theme(legend.position = "none")
  g <- gridExtra::arrangeGrob(
    gridExtra::arrangeGrob(
      ggplotGrob(g1), ggplotGrob(g2), legend, ncol = 3, widths = widths
    )
  )
  gridExtra::grid.arrange(g)
  invisible(g)
}

