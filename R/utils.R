
# Utilitiy functions ------------------------------------------------------

#' Convert grob-like objects to a ggplot
#'
#' Wrap a grid grob or gtable inside an empty ggplot so it can be displayed
#' or combined consistently with other ggplots.
#'
#' - If `grob` is already a ggplot, it is returned unchanged.
#' - If `grob` is a gtable or grob, it is embedded using
#'   [ggplot2::annotation_custom()].
#'
#' @param grob A ggplot, gtable or grob.
#'
#' @return A ggplot object displaying the input.
#'
#' @examples
#' \donttest{
#' p <- ggplot2::qplot(mpg, hp, data = mtcars)
#' grob <- ggplot2::ggplotGrob(p)
#' grob_to_ggplot(grob) # convert gtable back to ggplot
#' }
#'
#' @export
grob_to_ggplot <- function(grob) {
  if (inherits(grob, "ggplot"))
    return(grob)

  if (inherits(grob, "gtable") || inherits(grob, "grob")) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotation_custom(
          grob = grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        ) +
        ggplot2::coord_cartesian(clip = "off", expand = FALSE)
    )
  }

  stop("plot must be a ggplot, gtable or grob.", call. = FALSE)
}

#' Extract the legend from a ggplot
#'
#' Get the legend grob from a ggplot object.
#'
#' @param plot A ggplot object.
#'
#' @return A legend grob (the `"guide-box"` grob) extracted from plot.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' dat <- expand.grid(x = c("A","B","C"), fill = c("X","Y","Z"))
#' dat$y <- sample(1:10, size = 9, replace = TRUE)
#' g <- ggbar(dat, x = x, y = y, fill = fill, label = y,
#'            label_args = list(vjust = -0.25)) +
#'      theme_view(family = NULL)
#' get_legend(g)
#' }
#'
#' @export
get_legend <- function(plot) {
  gtable <- ggplot2::ggplot_gtable(ggplot_build(plot))
  guide <- which(sapply(gtable$grobs, function(x) x$name) == "guide-box")
  if (length(guide)) {
    return(gtable$grobs[[guide]])
  } else {
    return(
      gtable::gtable(
        widths = grid::unit(1, "null"),
        heights = grid::unit(1, "null")
      )
    )
  }
}

#' Add a title above a plot
#'
#' Place a title-like text grob above a ggplot or gtable, stacking it
#' vertically with the main plot.
#'
#' @param plot A ggplot object or gtable.
#' @param title Character string for the title text.
#' @param heights Numeric vector of relative heights for title vs plot
#'   (default `c(1, 9)`).
#' @param family Font family for the title (default `getOption("ggshort.font")`).
#' @param size Font size for the title (default `14`).
#' @param hjust,vjust Numeric justification values passed to [grid::textGrob()].
#'
#' @return A gtable object with the title stacked on top. Use [grid::grid.draw()]
#'   to render it.
#'
#' @examples
#' \donttest{
#' p <- ggbar(mtcars, x = factor(cyl), y = mpg) + theme_view()
#'
#' add_title(p, "MPG by cylinder") # or
#' p |> add_title(title = "MPG by cylinder")
#' }
#'
#' @export
add_title <- function(plot, title,
                      heights = c(1, 9),
                      family = getOption("ggshort.font"),
                      size = 14,
                      hjust = NULL,
                      vjust = NULL) {
  # make text grob
  title_grob <- grid::textGrob(
    title,
    gp = grid::gpar(fontfamily = family, fontsize = size),
    hjust = hjust,
    vjust = vjust
  )

  # stack top label and plot
  gt <- vstack_plots(title_grob, plot, heights = heights)

  grid::grid.newpage(); grid::grid.draw(gt)
  invisible(gt)
}

#' Deprecated: add_top()
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [add_title()] instead.
#'
#' @inheritParams add_title
#'
#' @return A gtable object (same as [add_title()]).
#'
#' @seealso [add_title()]
#' @export
add_top <- function(plot, title,
                    heights = c(1, 9),
                    family = getOption("ggshort.font"),
                    size = 14,
                    hjust = NULL,
                    vjust = NULL) {
  lifecycle::deprecate_warn("0.0.0.9001", "add_top()", "add_title()")
  add_title(plot = plot, title = title, heights = heights,
            family = family, size = size, hjust = hjust, vjust = vjust)
}

# Geom functions ----------------------------------------------------------

#' Draw a horizontal reference line at 1
#'
#' Add a horizontal line at y = 1 (or `log(1)` if `logscale = TRUE`).
#'
#' @param logscale Logical; if `TRUE`, interpret `yintercept` on the log scale.
#' @param yintercept Numeric position of the reference line. Default: `1`.
#' @param color Line color. Default: `"red"`.
#' @param linetype Line type; see [ggplot2::geom_hline()]. Default: `"longdash"`.
#' @param ... Additional arguments passed to [ggplot2::geom_hline()].
#'
#' @return A `ggplot2` layer.
#'
#' @export
geom_hline1 <- function(logscale = FALSE, yintercept = 1, color = "red",
                        linetype = "longdash", ...) {
  ggplot2::geom_hline(
    yintercept = if (logscale) log(yintercept) else yintercept,
    color = color, linetype = linetype, ...
  )
}

# Scale functions ---------------------------------------------------------

#' Add comma-formatted axis tick labels
#'
#' Convenience scales that format numeric axis labels with commas.
#'
#' @return A ggproto scale object.
#'
#' @seealso [scales::comma()], [ggplot2::scale_x_continuous()], [ggplot2::scale_y_continuous()]
#'
#' @export
scale_x_comma <- function()
  ggplot2::scale_x_continuous(labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function()
  ggplot2::scale_y_continuous(labels = scales::comma)

#' @rdname scale_x_comma
#' @return A list of two scales: `scale_x_comma()` and `scale_y_comma()`.
#' @export
scale_comma <- function() {
  list(
    scale_x_comma(),
    scale_y_comma()
  )
}

#' Reverse axis scale limits based on data type (x & y)
#'
#' Reverse axis direction, choosing a discrete or continuous scale
#' based on the class of the supplied vector.
#'
#' @param x A vector representing the mapped variable (factor/character/numeric).
#'
#' @return A `ggproto` scale object.
#'
#' @examples
#' \donttest{
#' # x-axis examples
#' scale_x_limit_reverse(factor(c("a","b","c")))
#' scale_x_limit_reverse(c("a","c","b"))
#' scale_x_limit_reverse(1:10)
#' }
#'
#' @export
scale_x_limit_reverse <- function(x) {
  if (is.factor(x)) {
    return(ggplot2::scale_x_discrete(limits = rev(levels(x))))
  }
  else if (is.character(x)) {
    return(ggplot2::scale_x_discrete(limits = rev(sort(unique(x)))))
  }
  else if (is.numeric(x)) {
    return(ggplot2::scale_x_reverse())
  }
}

#' @rdname scale_x_limit_reverse
#' @export
scale_y_limit_reverse <- function(x) {
  if (is.factor(x)) {
    return(ggplot2::scale_y_discrete(limits = rev(levels(x))))
  }
  else if (is.character(x)) {
    return(ggplot2::scale_y_discrete(limits = rev(sort(unique(x)))))
  }
  else if (is.numeric(x)) {
    return(ggplot2::scale_y_reverse())
  }
}

#' Pair color/fill scales for two-level groups
#'
#' Create consistent two-color scales for variables that take **exactly two levels**.
#' These are convenience wrappers around [ggplot2::scale_color_manual()] and
#' [ggplot2::scale_fill_manual()] with predefined palettes from `get_two_colors()`.
#'
#' @param pair_levels A length-2 character vector giving the expected level labels.
#'   Default: `c("1","2")`.
#' @param palette One of `"base"`, `"deep"`, `"base_inv"`, or `"deep_inv"`,
#'   passed to `get_two_colors()` to choose the palette.
#' @param guide A guide function or its name; passed to [ggplot2::guides()] via the scale.
#' @param drop Logical. If `TRUE` (default), unused levels are dropped from the legend.
#'   If `FALSE`, both levels are always shown even if only one is present.
#' @param na.value Color used for missing values or values outside `pair_levels`.
#'
#' @return A `ScaleDiscrete` object from [ggplot2::scale_color_manual()] or
#'   [ggplot2::scale_fill_manual()], suitable for use in ggplot.
#'
#' @seealso [ggplot2::scale_color_manual()], [ggplot2::scale_fill_manual()]
#'
#' @examples
#' \donttest{
#' data <- expand.grid(
#'   gender = c("M","F"),
#'   age_band = seq(from = 10 , to = 100, by = 10)
#' )
#' data$n <- c(1:10, 2:11)
#'
#' ggline(data, x = age_band, y = n, color = gender) +
#'   scale_pair_color_manual(pair_levels = c("M","F"), palette = "base") +
#'   theme_view()
#'
#' ggbar(data, x = age_band, y = n, fill = gender) +
#'   scale_pair_fill_manual(pair_levels = c("M","F"), palette = "deep") +
#'   theme_view()
#' }
#'
#' @export
scale_pair_color_manual <- function(
    pair_levels = c("1", "2"),
    palette     = c("base", "deep", "base_inv", "deep_inv"),
    guide       = "legend",
    drop        = TRUE,              # drop unused levels from legend
    na.value    = "grey50"           # fallback color for missing/out-of-range values
) {
  choice <- match.arg(palette)
  values <- get_two_colors(choice)              # returns a vector of two colors
  named  <- stats::setNames(values, pair_levels)  # name colors according to pair_levels

  ggplot2::scale_color_manual(
    values   = named,                # mapping: level -> color
    limits   = pair_levels,          # define allowed levels and their order
    guide    = guide,
    drop     = drop,
    na.value = na.value
  )
}

#' @rdname scale_pair_color_manual
#' @export
scale_pair_fill_manual <- function(
    pair_levels = c("1", "2"),
    palette     = c("base", "deep", "base_inv", "deep_inv"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  choice <- match.arg(palette)
  values <- get_two_colors(choice)
  named  <- stats::setNames(values, pair_levels)

  ggplot2::scale_fill_manual(
    values   = named,
    limits   = pair_levels,
    guide    = guide,
    drop     = drop,
    na.value = na.value
  )
}

#' @rdname scale_pair_color_manual
#' @param gender_levels Length-2 character vector of expected gender labels
#'   (alias of `pair_levels`), e.g. `c("1","2")` or `c("M","F")`.
#' @export
scale_color_gender <- function(
    gender_levels = c("1", "2"),
    palette     = c("base", "deep"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  scale_pair_color_manual(
    pair_levels = gender_levels, palette = palette,
    guide = guide, drop = drop, na.value = na.value
  )
}

#' @rdname scale_pair_color_manual
#' @param gender_levels Length-2 character vector of expected gender labels
#'   (alias of `pair_levels`), e.g. `c("1","2")` or `c("M","F")`.
#' @export
scale_fill_gender <- function(
    gender_levels = c("1", "2"),
    palette     = c("base", "deep"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  scale_pair_fill_manual(
    pair_levels = gender_levels, palette = palette,
    guide = guide, drop = drop, na.value = na.value
  )
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
  ggplot2::scale_x_continuous(
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
  break_interval <-
  ggplot2::scale_y_continuous(
    breaks = function(y) seq(
      from = round(min(y, na.rm = TRUE), break_interval-1),
      to   = round(max(y, na.rm = TRUE), break_interval-1),
      by   = break_interval
    ),
    ...
  )
}

# Color functions ---------------------------------------------------------

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

# Internal helper functions -----------------------------------------------

#' Convert objects to gtable
#'
#' Internal helpers to coerce different plot objects into a [gtable::gtable].
#'
#' - `.as_grob()` accepts a ggplot, gtable, or grob and returns a gtable.
#' - `.as_gtable()` does the same but is slightly stricter about gtable output.
#'
#' @param x A ggplot, gtable, or grob.
#'
#' @return A [gtable::gtable] object.
#'
#' @keywords internal
#' @noRd
.as_grob <- function(x) {
  if (inherits(x, "gtable"))
    return(x)
  if (inherits(x, "grob"))
    return(
      gtable::gtable(unit(1, "null"), unit(1, "null")) |>
        gtable::gtable_add_grob(x, t = 1, l = 1)
    )
  if (inherits(x, "ggplot"))
    return(ggplot2::ggplotGrob(x))
  stop("x must be a ggplot, gtable or grob.", call. = FALSE)
}

#' @rdname .as_grob
#' @keywords internal
#' @noRd
.as_gtable <- function(x) {
  if (inherits(x, "gtable"))
    return(x)
  if (inherits(x, "ggplot"))
    return(ggplot2::ggplotGrob(x))
  if (inherits(x, "grob")) {
    gt <- gtable::gtable(
      widths = grid::unit(1, "null"),
      heights = grid::unit(1, "null")
    )
    return(gtable::gtable_add_grob(gt, x, t = 1, l = 1))
  }
  stop("x must be a ggplot, gtable or grob.", call. = FALSE)
}

#' Suppress PostScript font database warnings
#'
#' Run an expression while ignoring warnings of the form
#' "font family ... not found in PostScript font database".
#' Other warnings are passed through as usual.
#'
#' @param expr Expression to evaluate.
#'
#' @return The result of evaluating `expr`.
#'
#' @keywords internal
#' @noRd
.suppress_ps_font_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("PostScript font database", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
