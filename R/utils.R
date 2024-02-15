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

#' Create pair fill scales
#'
#' Create consistent pair fill scales
#'
#' @param pair A vector contains data of pair values
#' @param pair_levels Two element vector expressing pair values (default, c("1", "2"))
#' @param color_type A string of color type, `base` and `deep`
#' @param guide A function used to create a guide or its name. See [guides()] for more information.
#' @return A ggplot object
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
                   "base" = ".TWO_COLORS_1",
                   "deep" = ".TWO_COLORS_2")
  return(get(colors, envir = .GGSHORT_COLORS_ENV))
}

match_cols <- function(df, cols) names(df)[match(cols, names(df), 0L)]
