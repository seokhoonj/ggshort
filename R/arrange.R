#' Grid top plot, bottom plot and right legend
#'
#' Grid top plot, bottom plot and right legend
#'
#' @param g1 a ggplot object
#' @param g2 a ggplot object
#' @param legend a gtable object
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
grid_top_bottom_right <- function(g1, g2, legend) {
  legend <- get_legend(g1)
  g1 <- g1 + theme(legend.position = "none")
  g2 <- g2 + theme(legend.position = "none")
  g <- gridExtra::arrangeGrob(
    gridExtra::arrangeGrob(
      ggplotGrob(g1),
      ggplotGrob(g2), nrow = 2, heights = c(5, 5)
    ), legend, ncol = 2, widths = c(8.5, 1.5)
  )
  gridExtra::grid.arrange(g)
  invisible(g)
}
