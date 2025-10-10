#' View theme
#'
#' A simple theme for data exploration or on-screen viewing.
#'
#' @param family Font family for all text.
#' @param x.size,y.size,t.size,s.size,l.size Font sizes for x-axis, y-axis,
#'   title, strip, and legend text. Defaults: `NULL` (ggplot defaults).
#' @param x.face,y.face,t.face,s.face,l.face Font face ("plain","bold","italic","bold.italic")
#'   for x-axis, y-axis, title, strip, and legend text.
#' @param x.angle,y.angle Angle of axis text (in degrees).
#' @param x.hjust,x.vjust,y.hjust,y.vjust Horizontal and vertical justification
#'   of axis text.
#' @param legend.key.height,legend.key.width Height/width of legend key boxes.
#' @param legend.position Position of legend: `"none"`, `"left"`, `"right"`,
#'   `"bottom"`, `"top"`, `"inside"`.
#' @param legend.justification Anchor point for legend placement. Either a string
#'   (e.g., `"center"`) or numeric vector length 2.
#'
#' @return A [ggplot2::theme()] object.
#'
#' @export
theme_view <- function(family = getOption("ggshort.font"),
                       x.size = NULL, y.size = NULL,
                       t.size = NULL, s.size = NULL, l.size = NULL,
                       x.face = "plain", y.face = "plain", t.face = "plain",
                       s.face = "plain", l.face = "plain", x.angle = 0,
                       y.angle = 0, x.hjust = .5, x.vjust = .5, y.hjust = NULL,
                       y.vjust = NULL, legend.key.height = NULL,
                       legend.key.width = NULL, legend.position = "right",
                       legend.justification = "center") {

  ticks_x <- if (!is.null(x.size) && identical(x.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()
  ticks_y <- if (!is.null(y.size) && identical(y.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()

  ggplot2::theme(
    text  = ggplot2::element_text(family = family),
    title = ggplot2::element_text(
      family = family, size = t.size, face = t.face
    ),
    strip.text.x = ggplot2::element_text(size = s.size, face = s.face),
    axis.text.x  = .element_axis_text(x.size, x.face, x.angle, x.hjust, x.vjust, family),
    axis.text.y  = .element_axis_text(y.size, y.face, y.angle, y.hjust, y.vjust, family),
    axis.ticks.x = ticks_x,
    axis.ticks.y = ticks_y,
    legend.title = ggplot2::element_text(size = l.size, face = l.face),
    legend.text  = ggplot2::element_text(size = l.size, face = l.face),
    legend.key.height = legend.key.height,
    legend.key.width = legend.key.width,
    legend.position = legend.position,
    legend.justification = legend.justification,
    panel.border = ggplot2::element_rect(
      colour = "black", fill = "transparent"
    ),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    strip.background = ggplot2::element_rect(colour = "black")
  )
}

#' Save theme
#'
#' A simple theme for saving ggplots (e.g., to xlsx or image output).
#'
#' @inheritParams theme_view
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_save <- function(family = getOption("ggshort.font"),
                       x.size = 12, y.size = 12,
                       t.size = 14, s.size = 14, l.size = 12, x.face = "plain",
                       y.face = "plain", t.face = "plain", s.face = "plain",
                       l.face = "plain", x.angle = 0, y.angle = 0, x.hjust = .5,
                       x.vjust = .5, y.hjust = NULL, y.vjust = NULL,
                       legend.key.height = NULL, legend.key.width = NULL,
                       legend.position = "right",
                       legend.justification = "center") {
  ticks_x <- if (!is.null(x.size) && identical(x.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()
  ticks_y <- if (!is.null(y.size) && identical(y.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()

  ggplot2::theme(
    text  = ggplot2::element_text(family = family),
    title = ggplot2::element_text(
      family = family, size = t.size, face = t.face
    ),
    strip.text.x = ggplot2::element_text(size = s.size, face = s.face),
    axis.text.x  = ggplot2::element_text(
      size = x.size, face = x.face, angle = x.angle,
      hjust = x.hjust, vjust = x.vjust
    ),
    axis.text.y  = ggplot2::element_text(
      size = y.size, face = y.face, angle = y.angle,
      hjust = y.hjust, vjust = y.vjust
    ),
    legend.title = ggplot2::element_text(size = l.size, face = l.face),
    legend.text  = ggplot2::element_text(size = l.size, face = l.face),
    legend.key.height = legend.key.height,
    legend.key.width = legend.key.width,
    legend.position = legend.position,
    legend.justification = legend.justification,
    panel.border = ggplot2::element_rect(
      colour = "black", fill = "transparent"
    ),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    strip.background = ggplot2::element_rect(colour = "black")
  )
}

#' Shiny theme
#'
#' A simple theme for Shiny apps, with transparent background by default.
#'
#' @inheritParams theme_view
#' @param plot.background.fill Fill color for entire plot background.
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_shiny <- function(family = getOption("ggshort.font"),
                        x.size = 12, y.size = 12,
                        t.size = 14, s.size = 14, l.size = 12, x.face = "plain",
                        y.face = "plain", t.face = "plain", s.face = "plain",
                        l.face = "plain", x.angle = 0, y.angle = 0,
                        x.hjust = .5, x.vjust = .5, y.hjust = NULL,
                        y.vjust = NULL, legend.key.height = NULL,
                        legend.key.width = NULL, legend.position = "right",
                        legend.justification = "center",
                        plot.background.fill = "transparent") { # original default: panel.background = ggplot2::element_rect() #ECF0F5
  ticks_x <- if (!is.null(x.size) && identical(x.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()
  ticks_y <- if (!is.null(y.size) && identical(y.size, 0))
    ggplot2::element_blank() else ggplot2::element_line()

  ggplot2::theme(
    text  = ggplot2::element_text(family = family),
    title = ggplot2::element_text(
      family = family, size = t.size, face = t.face
    ),
    strip.text.x = ggplot2::element_text(size = s.size, face = s.face),
    axis.text.x  = ggplot2::element_text(
      size = x.size, face = x.face, angle = x.angle,
      hjust = x.hjust, vjust = x.vjust
    ),
    axis.text.y  = ggplot2::element_text(
      size = y.size, face = y.face, angle = y.angle,
      hjust = y.hjust, vjust = y.vjust
    ),
    legend.title = ggplot2::element_text(size = l.size, face = l.face),
    legend.text  = ggplot2::element_text(size = l.size, face = l.face),
    legend.key.height = legend.key.height,
    legend.key.width  = legend.key.width,
    legend.position   = legend.position,
    legend.justification = legend.justification,
    panel.border = ggplot2::element_rect(
      colour = "black", fill = "transparent"
    ),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    strip.background = ggplot2::element_rect(colour = "black"),
    plot.background = ggplot2::element_rect(
      fill = plot.background.fill,
      colour = plot.background.fill
    )
  )
}

#' Switch theme
#'
#' Switch between built-in ggshort themes: `"view"`, `"save"`, `"shiny"`.
#'
#' @param theme One of `"view"`, `"save"`, `"shiny"`.
#' @param ... Additional arguments passed to the chosen theme function.
#'
#' @return A [ggplot2::theme()] object.
#'
#' @export
switch_theme <- function(theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  switch(
    theme,
    view = theme_view(...),
    save = theme_save(...),
    shiny = theme_shiny(...)
  )
}

#' Deprecated: match_theme()
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated alias for [switch_theme()].
#' Use [switch_theme()] instead.
#'
#' @inheritParams switch_theme
#'
#' @export
match_theme <- function(theme = c("view", "save", "shiny"), ...) {
  lifecycle::deprecate_warn("0.0.0.9001", "match_theme()", "switch_theme()")
  theme <- match.arg(theme)
  switch(
    theme,
    view = theme_view(...),
    save = theme_save(...),
    shiny = theme_shiny(...)
  )
}

# Internal helper functions -----------------------------------------------

.element_axis_text <- function(size, face, angle, hjust, vjust, family) {
  if (!is.null(size) && identical(size, 0)) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_text(
      size = size, face = face, angle = angle,
      hjust = hjust, vjust = vjust, family = family
    )
  }
}
