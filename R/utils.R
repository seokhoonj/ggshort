#' Grob to ggplot
#'
#' Change a grob object to a ggplot object.
#'
#' @param plot a grob object
#' @return a ggplot object.
#'
#' @export
grob2ggplot <- function(plot) {
  jaid::assert_class(plot, "grob")
  x <- y <- NULL
  return(
    ggplot(data.frame(x = 0:1, y = 0:1), aes(x = x, y = y)) +
      geom_blank() +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      annotation_custom(plot, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      theme_void()
  )
}

#' Get a legend
#'
#' Get a legend object from the ggplot object.
#'
#' @param plot a ggplot object
#' @return a ggplot object
#'
#' @examples
#' # get a legend.
#' \donttest{set.seed(123)
#' data <- expand.grid(x = c("A", "B", "C"), fill = c("X", "Y", "Z"))
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

#' Add commas to x-axis or y-axis tick labels
#'
#' Add commas to x-axis or y-axis tick labels.
#'
#' @return an ggproto object
#'
#' @export
scale_x_comma <- function() scale_x_continuous(labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function() scale_y_continuous(labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_comma <- function() {
  list(
    scale_x_comma(),
    scale_y_comma()
  )
}

#' Scale limit reverse (x & y)
#'
#' Reverse x & y axis scale limit
#'
#' @param x a vector x or y variable
#' @return a ggproto object
#'
#' @export
scale_x_limit_reverse <- function(x) {
  if (is.factor(x)) {
    return(scale_x_discrete(limits = rev(levels(x))))
  }
  else if (is.character(x)) {
    return(scale_x_discrete(limits = rev(sort(unique(x)))))
  }
  else if (is.numeric(x)) {
    return(scale_x_reverse())
  }
}

#' @rdname scale_x_limit_reverse
#' @export
scale_y_limit_reverse <- function(x) {
  if (is.factor(x)) {
    return(scale_y_discrete(limits = rev(levels(x))))
  }
  else if (is.character(x)) {
    return(scale_y_discrete(limits = rev(sort(unique(x)))))
  }
  else if (is.numeric(x)) {
    return(scale_y_reverse())
  }
}

#' Horizontal line 1
#'
#' Draw a horizontal line 1.
#'
#' @param logscale a logical whether to apply log scale.
#' @param yintercept a numeric that control the position of the horizontal line.
#' @param color a character specifying a color.
#' @param linetype [linetype]
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_hline}}.
#' @return a ggproto object
#'
#' @export
geom_hline1 <- function(logscale = FALSE, yintercept = 1, color = "red",
                        linetype = "dashed", ...) {
  geom_hline(yintercept = if (logscale) log(yintercept) else yintercept,
             color = color, linetype = linetype, ...)
}

#' Create pair fill scales
#'
#' Create consistent pair fill scales
#'
#' @param pair a vector contains data of pair values
#' @param pair_levels Two element vector expressing pair values (default, c("1", "2"))
#' @param color_type a string of color type, `base` and `deep`
#' @param guide a function used to create a guide or its name. See [guides()] for more information.
#' @return a ggplot object
#'
#' @seealso [scale_pair_fill_manual()]
#'
#' @examples
#' # pair values with `ggline()`
#' \donttest{data <- expand.grid(agecat = c(1:10), gender = c("1", "2"))
#' data$n <- c(1:10, 2:11)
#' ggline(data, x = agecat, y = n, color = gender) +
#'   scale_pair_color_manual(data$gender) +
#'   theme_view(family = NULL)}
#'
#' # pair values with `ggbar()`
#' \donttest{data <- expand.grid(agecat = c(1:10), gender = c("1", "2"))
#' data$n <- c(1:10, 2:11)
#' ggbar(data, x = agecat, y = n, fill = gender) +
#'   scale_pair_fill_manual(data$gender, color_type = "deep") +
#'   theme_view(family = NULL)}
#'
#' @export
scale_pair_color_manual <- function(pair, pair_levels = c("1", "2"),
                                    color_type = c("base", "deep"),
                                    guide = "legend") {
  choice <- match.arg(color_type)
  values <- get_two_colors(choice)
  list(if (jaid::unilen(pair) == 2L) {
    scale_color_manual(values = values, guide = guide)
  } else if (unique(pair) == pair_levels[1L]) {
    scale_color_manual(values = values[1L], guide = guide)
  } else if (unique(pair) == pair_levels[2L]) {
    scale_color_manual(values = values[2L], guide = guide)
  } else {
    scale_color_manual(values = "grey50", guide = guide)
  })
}

#' @rdname scale_pair_color_manual
#' @export
scale_pair_fill_manual <- function(pair, pair_levels = c("1", "2"),
                                   color_type = c("base", "deep"),
                                   guide = "legend") {
  jaid::assert_class(pair, c("character", "factor"))
  choice <- match.arg(color_type)
  values <- get_two_colors(choice)
  list(if (jaid::unilen(pair) == 2L) {
    scale_fill_manual(values = values, guide = guide)
  } else if (unique(pair) == pair_levels[1L]) {
    scale_fill_manual(values = values[1L], guide = guide)
  } else if (unique(pair) == pair_levels[2L]) {
    scale_fill_manual(values = values[2L], guide = guide)
  } else {
    scale_fill_manual(values = "grey50", guide = guide)
  })
}

get_two_colors <- function(choice = c("base", "deep")) {
  choice <- match.arg(choice)
  colors <- switch(choice,
                   base = ".TWO_COLORS_BASE",
                   deep = ".TWO_COLORS_DEEP")
  return(get(colors, envir = .GGSHORT_COLORS_ENV))
}

get_twelve_colors <- function() {
  return(local(.TWELVE_COLORS, envir = .GGSHORT_COLORS_ENV))
}
