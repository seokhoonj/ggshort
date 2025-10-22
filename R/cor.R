#' Correlation plot
#'
#' Draw a heatmap-style plot for a correlation matrix.
#'
#' @param x A numeric correlation matrix (square, symmetric, values in `[-1, 1]`).
#' @param display Which triangle to display: `"lower"` or `"upper"`. Diagonal
#'   cells are controlled separately via `show_diag`.
#' @param show_diag Logical; if `TRUE`, keep diagonal cells. Default `FALSE`.
#' @param show_label Logical; if `TRUE`, print correlation values on tiles.
#'   Default `TRUE`.
#' @param label_args A named list of text style options for `geom_text()`.
#'   Supported keys: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param title,subtitle,caption Plot title, subtitle, and caption. Passed to
#'   [ggplot2::labs()].
#' @param theme A ggshort theme key used by `match_theme()`: one of
#'   `"view"`, `"save"`, or `"shiny"`.
#' @param ... Additional arguments forwarded to `match_theme(theme = ...)`
#'   (e.g., `x.angle`, `x.hjust`).
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' x <- cor(mtcars)
#' # lower triangle with labels
#' plot_cor(x, display = "lower")
#'
#' # upper triangle without diagonal or labels
#' plot_cor(x, display = "upper", show_diag = FALSE, show_label = FALSE)
#' }
#'
#' @export
plot_cor <- function(x, display = c("lower", "upper"),
                     show_diag = FALSE,
                     show_label = TRUE,
                     label_args = list(
                       family = getOption("ggshort.font"),
                       size   = 4,
                       angle  = 0,
                       hjust  = 0.5,
                       vjust  = 0.5,
                       color  = "black"
                     ),
                     title = NULL, subtitle = NULL, caption = NULL,
                     theme = c("view", "save", "shiny"), ...) {
  if (!inherits(x, "matrix"))
    stop("`x` must be a character or factor.", call. = FALSE)
  if (nrow(x) != ncol(x))
    stop("`x` must be a square matrix.", call. = FALSE)
  if (max(abs(range(x))) > 1)
    stop("`x` is not a correlation matrix", call. = FALSE)
  if (!isTRUE(all.equal(x, t(x), tolerance = 1e-8, check.attributes = FALSE)))
    stop("`x` must be symmetric.", call. = FALSE)

  display <- match.arg(display)
  if (display == "lower") {
    x[lower.tri(x, diag = FALSE)] <- NA_real_
  } else if (display == "upper") {
    x[upper.tri(x, diag = FALSE)] <- NA_real_
  }

  if (!show_diag) {
    diag(x) <- NA_real_
  }

  dn <- dimnames(x)
  levels <- dn[[1L]]
  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  data.table::setnames(labels, c("var1", "var2"))
  data <- data.frame(labels, correlation = as.vector(x))
  data <- data[!is.na(data$correlation),]
  data$var1 <- factor(data$var1, levels = levels)
  data$var2 <- factor(data$var2, levels = rev(levels))

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[["var1"]], y = .data[["var2"]],
      fill = .data[["correlation"]]
    )
  ) +
    ggplot2::geom_tile(color = "gray") +
    ggplot2::scale_fill_gradient2(
      low = "#80B1D3", mid = "white", high = "#FB8072",
      midpoint = 0, limit = c(-1, 1)
    ) + ggplot2::xlab("") + ggplot2::ylab("")

  if (show_label) {
    args <- .modify_label_args(label_args)
    p <- p + ggplot2::geom_text(
      aes(label = sprintf("%.2f", .data[["correlation"]])),
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    switch_theme(theme = theme, ...)
}
