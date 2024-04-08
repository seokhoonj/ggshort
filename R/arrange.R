#' Grid top plot, bottom plot and right legend
#'
#' Grid top plot, bottom plot and right legend
#'
#' @param g1 a ggplot object
#' @param g2 a ggplot object
#' @param legend a gtable object
#' @param heights,widths a unit vector giving the height of each row or the widths of each column
#' @return a gtable object
#'
#' @examples
#' # grid top plot, bottom plot, right legend
#' \donttest{set.seed(123)
#' data <- expand.grid(x = c("A", "B", "C"), fill = c("X", "Y", "Z"))
#' data$y <- sample(x = 1:10, size = 9, replace = TRUE)
#' g1 <- ggbar(data = data, x = x, y = y, fill = fill, label = y, label_vjust = -.25,
#'   label_family = NA) +
#'   theme_view(family = NULL)
#' set.seed(123)
#' data <- expand.grid(x = 1:20, color = c("X", "Y", "Z"))
#' data$y <- sample(1:10, size = nrow(data), replace = TRUE)
#' g2 <- ggline(data = data, x = x, y = y, color = color) +
#'   theme_view(family = NULL)
#' legend <- get_legend(g1)
#' grid_top_bottom_right(g1, g2, legend)}
#'
#' @export
grid_top_bottom_right <- function(g1, g2, legend, heights = c(5, 5),
                                  widths = c(8.5, 1.5)) {
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
#' @export
grid_left_to_right <- function (g1, g2, legend, widths = c(4, 4, 2)) {
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

#' Add top
#'
#' Add top labels
#'
#' @param g a ggplot object
#' @param top a string specifying a top label
#' @param heights a unit vector giving the height of each row
#' @param fontfamily the font family
#' @param fontsize the font size
#' @param hjust A numeric vector specifying horizontal justification. If specified, overrides the just setting.
#' @param vjust A numeric vector specifying vertical justification. If specified, overrides the just setting.
#' @return a gtable object
#'
#' @export
add_top <- function(g, top, heights = c(1, 9), fontfamily = "Comic Sans MS",
                    fontsize = 16, hjust = NULL, vjust = NULL) {
  top <- grid::textGrob(
    top, gp = grid::gpar(fontfamily = fontfamily, fontsize = fontsize
  ), hjust = hjust, vjust = vjust)
  g <- gridExtra::arrangeGrob(top, g, heights = heights)
  gridExtra::grid.arrange(g)
}
