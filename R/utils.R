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
                        linetype = "longdash", ...) {
  geom_hline(yintercept = if (logscale) log(yintercept) else yintercept,
             color = color, linetype = linetype, ...)
}

#' Create pair fill scales
#'
#' Create consistent pair fill scales
#'
#' @param pair a vector contains data of pair values
#' @param pair_levels Two element vector expressing pair values (default, c("1", "2"))
#' @param color_type a string of color type, `base`, `deep`, `base_inv` and `deep_inv`.
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
scale_pair_color_manual <- function(
    pair, pair_levels = c("1", "2"),
    color_type = c("base", "deep", "base_inv", "deep_inv"),
    guide = "legend"
) {
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
scale_pair_fill_manual <- function(
    pair, pair_levels = c("1", "2"),
    color_type = c("base", "deep", "base_inv", "deep_inv"),
    guide = "legend"
) {
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

#' Continuous Position Scale for X-Axis with Custom Breaks
#'
#' `scale_x_break()` provides a continuous position scale for the x-axis,
#' allowing users to specify the interval between axis breaks.
#' It rounds the axis limits to align with the specified break interval.
#'
#' @param break_interval A numeric value specifying the interval between x-axis breaks.
#' @param ... Other arguments passed to [ggplot2::scale_x_continuous()],
#'   such as `limits`, `labels`, or `expand`.
#'
#' @return A ggplot2 scale object modifying the x-axis breaks.
#'
#' @section Computation:
#' This function dynamically calculates axis breaks based on the range of x values in the data.
#' - If `break_interval` is an integer, the axis breaks are rounded to the nearest whole numbers.
#' - If `break_interval` is a decimal, the scale retains fractional values without rounding.
#'
#' @seealso [ggplot2::scale_x_continuous()]
#'
#' @examples
#' library(ggplot2)
#'
#' # Example using the iris dataset
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   scale_x_break(break_interval = 0.5)
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
#'   geom_point() +
#'   scale_x_break(break_interval = 1, limits = c(4, 8))
#'
#' @export
scale_x_break <- function(break_interval, ...) {
  scale_x_continuous(
    breaks = function(x) seq(
      from = round(min(x, na.rm = TRUE), break_interval-1),
      to   = round(max(x, na.rm = TRUE), break_interval-1),
      by   = break_interval
    ),
    ...
  )
}

#' @rdname scale_x_break
#' @export
scale_y_break <- function(break_interval, ...) {
  scale_y_continuous(
    breaks = function(y) seq(
      from = round(min(y, na.rm = TRUE), break_interval-1),
      to   = round(may(y, na.rm = TRUE), break_interval-1),
      by   = break_interval
    ),
    ...
  )
}



get_two_colors <- function(choice = c("base", "deep", "base_inv", "deep_inv")) {
  choice <- match.arg(choice)
  colors <- switch(choice,
                   base = ".TWO_COLORS_BASE",
                   deep = ".TWO_COLORS_DEEP",
                   base_inv = ".TWO_COLORS_BASE_INV",
                   deep_inv = ".TWO_COLORS_DEEP_INV")
  return(get(colors, envir = .GGSHORT_COLORS_ENV))
}

get_twelve_colors <- function() {
  return(local(.TWELVE_COLORS, envir = .GGSHORT_COLORS_ENV))
}
