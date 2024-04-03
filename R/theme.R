#' View theme
#'
#' Simple view theme for ggplot.
#'
#' @param family font-family
#' @param x.size,y.size,t.size,s.size,l.size x axis, y axis, title, strip,
#'   legend text size
#' @param x.face,y.face,t.face,s.face,l.face x axis, y axis, title, strip,
#'   legend text face
#' @param x.angle,y.angle x axis, y axis text angle
#' @param x.hjust,x.vjust,y.hjust,y.vjust horizontal, vertical adjustment
#' @param legend.key.height,legend.key.width key background height & width
#' @param legend.position the default position of legends ("none", "left",
#'   "right", "bottom", "top", "inside")
#' @param legend.justification anchor point for positioning legend inside plot
#'   ("center" or two-element numeric vector) or the justification according to
#'   the plot area when positioned outside the plot
#' @return A ggplot object
#'
#' @export
theme_view <- function(family = "Comic Sans MS", x.size = NULL, y.size = NULL,
                       t.size = NULL, s.size = NULL, l.size = NULL,
                       x.face = "plain", y.face = "plain", t.face = "plain",
                       s.face = "plain", l.face = "plain", x.angle = 0,
                       y.angle = 0, x.hjust = .5, x.vjust = .5, y.hjust = NULL,
                       y.vjust = NULL, legend.key.height = NULL,
                       legend.key.width = NULL, legend.position = "right",
                       legend.justification = "center") {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle,
                                  hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle,
                                  hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width = legend.key.width,
      legend.position = legend.position,
      legend.justification = legend.justification,
      panel.border = element_rect(colour = "black", fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      strip.background = element_rect(colour = "black")
    )
  )
}

#' @title Shiny theme
#'
#' @description
#' Simple shiny theme for ggplot.
#'
#' @param family font family
#' @param x.size,y.size,t.size,s.size,l.size x axis, y axis, title, strip,
#'   legend text size
#' @param x.face,y.face,t.face,s.face,l.face x axis, y axis, title, strip,
#'   legend text face
#' @param x.angle,y.angle x axis, y axis text angle
#' @param x.hjust,x.vjust,y.hjust,y.vjust horizontal, vertical adjustment
#' @param legend.key.height,legend.key.width key background height & width
#' @param legend.position the default position of legends ("none", "left",
#'   "right", "bottom", "top", "inside")
#' @param legend.justification anchor point for positioning legend inside plot
#'   ("center" or two-element numeric vector) or the justification according to
#'   the plot area when positioned outside the plot
#' @param plot.background.fill fill background of the entire plot
#'
#' @export
theme_shiny <- function(family = "Comic Sans MS", x.size = 12, y.size = 12,
                        t.size = 14, s.size = 14, l.size = 12, x.face = "plain",
                        y.face = "plain", t.face = "plain", s.face = "plain",
                        l.face = "plain", x.angle = 0, y.angle = 0,
                        x.hjust = .5, x.vjust = .5, y.hjust = NULL,
                        y.vjust = NULL, legend.key.height = NULL,
                        legend.key.width = NULL, legend.position = "right",
                        legend.justification = "center",
                        plot.background.fill = "transparent") { # original default: panel.background = element_rect() #ECF0F5
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle,
                                  hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle,
                                  hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width  = legend.key.width,
      legend.position   = legend.position,
      legend.justification = legend.justification,
      panel.border = element_rect(colour = "black", fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      strip.background = element_rect(colour = "black"),
      plot.background = element_rect(
        fill = plot.background.fill,
        colour = plot.background.fill
      )
    )
  )
}

#' @title Save theme
#'
#' @description
#' Simple xlsx save theme for ggplot.
#'
#' @param family font family
#' @param x.size,y.size,t.size,s.size,l.size x axis, y axis, title, strip,
#'   legend text size
#' @param x.face,y.face,t.face,s.face,l.face x axis, y axis, title, strip,
#'   legend text face
#' @param x.angle,y.angle x axis, y axis text angle
#' @param x.hjust,x.vjust,y.hjust,y.vjust horizontal, vertical adjustment
#' @param legend.key.height,legend.key.width key background height & width
#' @param legend.position the default position of legends ("none", "left",
#'   "right", "bottom", "top", "inside")
#' @param legend.justification anchor point for positioning legend inside plot
#'   ("center" or two-element numeric vector) or the justification according to
#'   the plot area when positioned outside the plot
#'
#' @export
theme_save <- function(family = "Comic Sans MS", x.size = 12, y.size = 12,
                       t.size = 14, s.size = 14, l.size = 12, x.face = "plain",
                       y.face = "plain", t.face = "plain", s.face = "plain",
                       l.face = "plain", x.angle = 0, y.angle = 0, x.hjust = .5,
                       x.vjust = .5, y.hjust = NULL, y.vjust = NULL,
                       legend.key.height = NULL, legend.key.width = NULL,
                       legend.position = "right",
                       legend.justification = "center") {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle,
                                  hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle,
                                  hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width = legend.key.width,
      legend.position = legend.position,
      legend.justification = legend.justification,
      panel.border = element_rect(colour = "black", fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      strip.background = element_rect(colour = "black")
    )
  )
}

#' Match theme
#'
#' Match a ggshort theme.
#'
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... arguments passed on to (`theme_view`, `theme_save`, `theme_shiny`)
#'
#' @export
match_theme <- function(theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  switch(theme, view = theme_view(...), save = theme_save(...),
         shiny = theme_shiny(...))
}
