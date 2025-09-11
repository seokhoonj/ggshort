#' Stack/arrange two plots (vertical or horizontal)
#'
#' Combine two plotting objects into a single gtable layout:
#' - `vstack_plots()` places one plot **above** the other (rows).
#' - `hstack_plots()` places plots **side by side** (columns).
#'
#' Inputs may be ggplot, gtable, or grid grob objects; they are coerced
#' internally to a gtable. Relative sizes are controlled by `heights` (vertical)
#' or `widths` (horizontal). When `draw = TRUE`, the result is drawn immediately.
#'
#' @name stack_plots
#' @param top,bottom Plots to stack vertically (for `vstack_plots()`).
#' @param left,right Plots to arrange horizontally (for `hstack_plots()`).
#' @param heights Numeric vector of two relative heights (default `c(1, 1)`).
#'   Used by `vstack_plots()`.
#' @param widths Numeric vector of two relative widths (default `c(1, 1)`).
#'   Used by `hstack_plots()`.
#' @param draw Logical; if `TRUE`, draw the combined gtable immediately.
#'   Default `FALSE`.
#'
#' @return A gtable object representing the combined layout (invisible).
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' df <- expand.grid(x = c("A","B","C"), fill = c("X","Y","Z"))
#' df$y <- sample(1:10, 9, TRUE)
#' p1 <- ggbar(df, x = x, y = y, fill = fill) + theme_view()
#'
#' df2 <- expand.grid(x = 1:20, color = c("X","Y","Z"))
#' df2$y <- sample(1:10, nrow(df2), TRUE)
#' p2 <- ggline(df2, x = x, y = y, color = color) + theme_view()
#'
#' # vertical stack
#' vstack_plots(p1, p2, heights = c(1, 1), draw = TRUE)
#'
#' # horizontal stack
#' hstack_plots(p1, p2, widths = c(1, 1), draw = TRUE)
#' }
NULL

#' @rdname stack_plots
#' @export
vstack_plots <- function(top, bottom, heights = c(1, 1), draw = FALSE) {
  top    <- .as_gtable(top)
  bottom <- .as_gtable(bottom)

  combined <- gtable::gtable(
    widths  = grid::unit(1, "null"),
    heights = grid::unit(heights, "null")
  )

  combined <- gtable::gtable_add_grob(
    combined, top,
    t = 1, l = 1, b = 1, r = 1
  )
  combined <- gtable::gtable_add_grob(
    combined, bottom,
    t = 2, l = 1, b = 2, r = 1
  )

  if (isTRUE(draw)) { grid::grid.newpage(); grid::grid.draw(combined) }
  invisible(combined)
}

#' @rdname stack_plots
#' @export
hstack_plots <- function(left, right, widths = c(1, 1), draw = FALSE) {
  left  <- .as_gtable(left)
  right <- .as_gtable(right)

  combined <- gtable::gtable(
    widths  = grid::unit(widths, "null"),
    heights = grid::unit(1, "null")
  )

  combined <- gtable::gtable_add_grob(
    combined, left,
    t = 1, l = 1, b = 1, r = 1
  )
  combined <- gtable::gtable_add_grob(
    combined, right,
    t = 1, l = 2, b = 1, r = 2
  )

  if (isTRUE(draw)) { grid::grid.newpage(); grid::grid.draw(combined) }
  invisible(combined)
}

#' Stack/arrange two plots with a shared legend
#'
#' Combine two ggplot objects into a single layout **including a shared legend**.
#' - `hstack_plots_with_legend()` arranges the plots side by side with the legend
#'   placed to the right.
#' - `vstack_plots_with_legend()` arranges the plots vertically with the legend
#'   also placed to the right.
#'
#' By default, the legend is automatically extracted from the first plot (`plot1`).
#' A custom legend object (gtable) may also be supplied via the `legend` argument.
#' Axis texts/ticks can optionally be hidden to reduce redundancy.
#'
#' @name stack_plots_with_legend
#' @param plot1,plot2 Two ggplot objects to combine. The legend is taken from `plot1`
#'   unless explicitly provided via `legend`.
#' @param legend Optional legend gtable. If missing, it is extracted automatically
#'   from `plot1`.
#' @param widths Numeric vector of relative widths for horizontal layout
#'   (default `c(4.25, 4.25, 1.5)` for `hstack_plots_with_legend()`).
#' @param heights Numeric vector of relative heights for vertical layout
#'   (default `c(5, 5)` for `vstack_plots_with_legend()`).
#' @param x_axis,y_axis Logical; if `FALSE`, hide x- or y-axis text/ticks for
#'   both plots (useful for compact arrangements).
#' @param title,subtitle,caption Optional plot annotations. Applied to `plot1`
#'   (with blank text in `plot2` to avoid duplication).
#'
#' @return A ggplot object containing the combined plots and shared legend.
#'
#' @examples
#' \donttest{
#' p1 <- ggbox(iris, x = Species, y = Sepal.Length, fill = Species) +
#'   theme_view()
#' p2 <- ggdensity(iris, x = Sepal.Length, fill = Species) +
#'   theme_view()
#'
#' # Horizontal arrangement with shared legend
#' hstack_plots_with_legend(p1, p2, widths = c(4.25, 4.25, 1.5))
#'
#' # Vertical arrangement with shared legend
#' vstack_plots_with_legend(p1, p2, heights = c(5, 5), widths = c(8.5, 1.5),
#'                          y_axis = FALSE)
#' }
NULL

#' @rdname stack_plots_with_legend
#' @export
hstack_plots_with_legend <- function(plot1, plot2, legend,
                                     widths = c(4.25, 4.25, 1.5),
                                     x_axis = TRUE, y_axis = TRUE,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL) {
  if (missing(legend))
    legend <- get_legend(plot1)

  p1 <- plot1 + ggplot2::theme(legend.position = "none")
  p2 <- plot2 + ggplot2::theme(legend.position = "none")

  if (!x_axis) {
    p1 <- p1 + ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
    p2 <- p2 + ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }

  if (!y_axis) {
    p1 <- p1 + ggplot2::theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
    p2 <- p2 + ggplot2::theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }

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

  widths1 <- widths[1:2]
  widths2 <- c(sum(widths1), widths[3])
  p <- hstack_plots(p1, p2, widths = widths1)
  p <- hstack_plots(p, legend, widths = widths2)
  grob_to_ggplot(p)
}

#' @rdname stack_plots_with_legend
#' @export
vstack_plots_with_legend <- function(plot1, plot2, legend,
                                     heights = c(5, 5), widths = c(8.5, 1.5),
                                     x_axis = TRUE, y_axis = TRUE,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL) {
  if (missing(legend))
    legend <- get_legend(plot1)

  p1 <- plot1 + ggplot2::theme(legend.position = "none")
  p2 <- plot2 + ggplot2::theme(legend.position = "none")

  if (!x_axis) {
    p1 <- p1 + ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
    p2 <- p2 + ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }

  if (!y_axis) {
    p1 <- p1 + ggplot2::theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
    p2 <- p2 + ggplot2::theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }

  if (!is.null(title)) {
    p1 <- p1 + labs(title = title)
    p2 <- p2 + labs(title = "")
  }

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

  p <- vstack_plots(p1, p2, heights = heights)
  p <- hstack_plots(p, legend, widths = widths)
  grob_to_ggplot(p)
}
