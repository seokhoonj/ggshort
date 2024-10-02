#' Meta plot
#'
#' Draw a plot for a meta object.
#'
#' @param x a meta object
#' @param widths a unit vector giving the width of each column
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @return a ggplot object
#'
#' @examples
#' # meta plot
#' \dontrun{
#' mt <- jaid::meta(mtcars)
#' metaplot(mt)}
#'
#' @export
metaplot <- function(x, widths = c(5, 5), theme = c("view", "save", "shiny")) {
  jaid::assert_class(x, "meta")
  column <- value <- variable <- NULL

  x$distinct <- as.character(x$distinct)
  x$mode <- as.character(x$mode)

  dt <- data.table::melt(data.table::as.data.table(x), id.vars = c("column"),
                         measure.vars = c("prop", "nzprop"))
  dt$column <- factor(dt$column, levels = unique(dt$column))
  xlvl <- levels(dt$column)
  dt$column <- as.numeric(dt$column)

  ds <- data.table::melt(data.table::as.data.table(x), id.vars = c("column"),
                         measure.vars = c("class", "distinct", "mode"))
  ds$column <- factor(ds$column, levels = xlvl)
  ds$info <- "info"

  theme <- match.arg(theme)
  p <- gridExtra::arrangeGrob(
    ggtable(ds, x = variable, y = column, label = value,
            xlab_position = "bottom", linetype = "solid") +
      facet_wrap(~ info) +
      xlab("") +
      ylab("") +
      match_theme(theme = theme),
    ggbar(dt, x = column, y = value, ymax = 1.5,
          label = sprintf("%.3f", value), label_hjust = -.1) +
      geom_vline(xintercept = seq(1, 1 + length(xlvl)) - 0.5, linetype = "solid") +
      scale_x_reverse(breaks = seq(1, length(xlvl)), labels = xlvl) +
      scale_y_continuous(breaks = c(0, .5, 1, 1.5)) +
      coord_flip() +
      facet_wrap(~ variable) +
      xlab("") +
      ylab("") +
      match_theme(theme = theme, y.size = 0),
    ncol = 2L, widths = widths
  )
  gridExtra::grid.arrange(p)
}

#' @method plot meta
#' @export
plot.meta <- function(x, widths = c(5, 5), theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  metaplot(x = x, widths = widths, theme = theme)
}
