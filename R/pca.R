#' PCA plot
#'
#' Draw a PCA plot for a prcomp object.
#'
#' @param object a prcomp object
#' @param x a priciple component rank for a x axis (default: 1)
#' @param y a priciple component rank for a y axis (default: 2)
#' @param color a vector for grouping points color (default: NULL)
#' @param scale a scaling parameter, disabled by 0 (default: 1)
#' @param alpha a point color transparency
#' @param label_family a string specifying label font-family
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... arguments passed on to (`theme_view`, `theme_save`, `theme_shiny`)
#'
#' @return a ggplot object
#'
#' @examples
#' # PCA plot
#' \dontrun{
#' pca <- prcomp(iris[, -5])
#' pcaplot(pca, x = 1, y = 1, color = iris$Species)}
#'
#' @export
pcaplot <- function(object, x = 1, y = 2, color = NULL, scale = 1, alpha = .3,
                    label_family = "Comic Sans MS",
                    theme = c("view", "save", "shiny"), ...) {
  assert_class(object, "prcomp")
  theme <- match.arg(theme)
  data <- as.data.frame(object$x)
  if (!is.null(color)) {
    if (is.numeric(color))
      color <- as.factor(color)
    data <- cbind(data, color = color)
  }
  sdev <- object$sdev
  ve <- sdev^2 / sum(sdev^2)
  ve <- ve[c(x, y)]
  pc <- paste0("PC", c(x, y))
  col_x <- pc[1L]; col_y <- pc[2L]
  labs <- sprintf("%s (%.2f%%)", pc, ve * 100)
  xlab <- labs[1L]; ylab <- labs[2L]
  lam <- sdev[c(x, y)] * sqrt(nrow(data))
  if (scale != 0) {
    lam <- lam^scale
    data[, c(col_x, col_y)] <- t(t(data[, c(col_x, col_y)])/lam)
  }
  rotation <- as.data.frame(object$rotation)
  rotation$variable <- rownames(rotation)
  scaler <- min(
    max(abs(data[, 1L])) / max(abs(rotation[, 1L])),
    max(abs(data[, 2L])) / max(abs(rotation[, 2L]))
  )
  rotation[, 1L:2L] <- rotation[, 1L:2L] * scaler * .8
  ggplot(data = data, aes(x = !!rlang::sym(col_x), y = !!rlang::sym(col_y))) +
    list(
    if (is.null(color)) {
      geom_point(alpha = alpha)
    } else {
      geom_point(aes(color = color), alpha = alpha)
    }) +
    ggshort::stat_mean_line() +
    geom_segment(data = rotation,
                 aes(x = 0, y = 0, xend = !!rlang::sym(col_x),
                     yend = !!rlang::sym(col_y), color = variable),
                 arrow = grid::arrow(length = grid::unit(7, "points"))) +
    geom_text(data = rotation, aes(label = variable), color = "gray7",
              family = label_family) +
    xlab(xlab) +
    ylab(ylab) +
    match_theme(theme = theme, ...)
}
