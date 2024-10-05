#' Correlation plot
#'
#' Draw a plot for a correlation matrix.
#'
#' @param x a correlation matrix
#' @param display a matrix display type ("full", "lower", "upper")
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... arguments passed on to (`theme_view`, `theme_save`, `theme_shiny`)
#' @return a ggplot object
#'
#' @examples
#' # correlation plot
#' \dontrun{
#' x <- cor(mtcars)
#' corplot(x)}
#'
#' @export
corplot <- function(x, display = c("full", "lower", "upper"),
                    theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "matrix")
  if (max(abs(range(x))) > 1)
    stop("Not a correlation matrix", call. = FALSE)
  display <- match.arg(display)
  if (display == "lower") {
    x[upper.tri(x)] <- NA
  } else if (display == "upper") {
    x[lower.tri(x)] <- NA
  }
  diag(x) <- NA
  dn <- dimnames(x)
  levels <- dn[[1L]]
  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  data <- data.table(labels, correlation = as.vector(x))
  data <- data[!is.na(correlation)]
  data[, Var1 := factor(Var1, levels = levels)]
  data[, Var2 := factor(Var2, levels = levels)]

  ggplot(data = data, aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile(color = "gray") +
    geom_text(aes(label = sprintf("%.2f", correlation))) +
    scale_fill_gradient2(low = "#80B1D3", mid = "white", high = "#FB8072", midpoint = 0, limit = c(-1, 1)) +
    xlab("") + ylab("") +
    match_theme(theme = theme, x.angle = 90, x.hjust = 1, ...)
}
