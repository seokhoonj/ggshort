#' Get a legend
#'
#' Get a legend object from the ggplot object.
#'
#' @param plot ggplot object
#' @return ggplot object
#'
#' @examples
#' # get a legend.
#' \donttest{data <- expand.grid(x = c("A", "B", "C"), fill = c("X", "Y", "Z"))
#' data$y <- sample(x = 1:10, size = 9, replace = TRUE)
#' g <- ggbar(data = data, x = x, y = y, fill = fill, label = y, label_vjust = -.25) +
#'   theme_view(family = NULL)
#' get_legend(g)}
#'
#' @export
get_legend <- function(plot) {
  gtable <- ggplot_gtable(ggplot_build(plot))
  guide <- which(sapply(gtable$grobs, function(x) x$name) == "guide-box")
  return(gtable$grobs[[guide]])
}

match_cols <- function(df, cols) names(df)[match(cols, names(df), 0L)]
