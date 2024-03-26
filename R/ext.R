GeomHlineMean <- ggproto(
  "GeomHlineMean", Stat,
  compute_group = function(data, scales) {
    transform(data, yintercept = mean(y))
  },
  required_aes = c("x", "y")
)

#' Geom horizontal mean line
#'
#' Draw a geom horizontal mean line.
#'
#' @inheritParams ggplot2::geom_abline
#' @param geom name of geom to use for annotation
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use
#' position_jitter), or the result of a call to a position adjustment function. Use the latter if you
#' need to change the settings of the adjustment.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combining with them. This is most useful
#' for helper functions that define both data and aesthetics and shouldn't inherit behaviour from
#' the default plot specification, e.g. [borders()].
#' @param colour [colour()]
#' @param linetype,linewidth [linetype()]
#' @return a ggplot object
#'
#' @export
geom_hline_mean <- function(mapping = NULL, data = NULL, geom = "hline",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, colour = "red", linetype = "dashed",
                            linewidth = .25,  ...) {
  layer(
    stat = GeomHlineMean, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(colour = colour, linetype = linetype, linewidth = linewidth,
                  na.rm = na.rm, ...)
  )
}
