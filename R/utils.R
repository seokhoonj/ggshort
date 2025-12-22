
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
#'        theme_view(family = NULL)
#' get_legend(g)
#' }
#'
#' @export
get_legend <- function(plot) {
  if (!ggplot2::is_ggplot(plot))
    stop("`plot` must be a ggplot object (class 'gg'/'ggplot').", call. = FALSE)
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  idx <- which(
    vapply(gt$grobs, function(x) x$name, character(1L)) == "guide-box"
  )
  if (length(idx)) {
    return(gt$grobs[[idx]])
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
#'
#' @keywords internal
#' @noRd
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
#' @seealso [ggplot2::scale_x_continuous()], [ggplot2::scale_y_continuous()],
#'   [scales::label_comma()]
#'
#' @export
scale_x_comma <- function()
  ggplot2::scale_x_continuous(
    labels = scales::label_comma(accuracy = 1, trim = TRUE)
  )

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function()
  ggplot2::scale_y_continuous(
    labels = scales::label_comma(accuracy = 1, trim = TRUE)
  )

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
#'   Default: `c("0","1")`.
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
#'   scale_color_pair_manual(pair_levels = c("M","F"), palette = "base") +
#'   theme_view()
#'
#' ggbar(data, x = age_band, y = n, fill = gender) +
#'   scale_fill_pair_manual(pair_levels = c("M","F"), palette = "deep") +
#'   theme_view()
#' }
#'
#' @export
scale_color_pair_manual <- function(
    pair_levels = c("0", "1"),
    palette     = c("base", "deep", "base_inv", "deep_inv"),
    guide       = "legend",
    drop        = TRUE,              # drop unused levels from legend
    na.value    = "grey50"           # fallback color for missing/out-of-range values
) {
  if (is.numeric(pair_levels))
    pair_levels <- as.character(pair_levels)

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

#' @rdname scale_color_pair_manual
#' @export
scale_fill_pair_manual <- function(
    pair_levels = c("0", "1"),
    palette     = c("base", "deep", "base_inv", "deep_inv"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  if (is.numeric(pair_levels))
    pair_levels <- as.character(pair_levels)

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

#' @rdname scale_color_pair_manual
#' @param gender_levels Length-2 character vector of expected gender labels
#'   (alias of `pair_levels`). Default: `c("M","F")`.
#' @export
scale_color_gender <- function(
    gender_levels = c("M", "F"),
    palette     = c("base", "deep"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  palette <- match.arg(palette)
  scale_color_pair_manual(
    pair_levels = gender_levels, palette = palette,
    guide = guide, drop = drop, na.value = na.value
  )
}

#' @rdname scale_color_pair_manual
#' @param gender_levels Length-2 character vector of expected gender labels
#'   (alias of `pair_levels`). Default: `c("M","F")`.
#' @export
scale_fill_gender <- function(
    gender_levels = c("M", "F"),
    palette     = c("base", "deep"),
    guide       = "legend",
    drop        = TRUE,
    na.value    = "grey50"
) {
  palette <- match.arg(palette)
  scale_fill_pair_manual(
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

#' Logarithmic scale with custom base
#'
#' Shortcut for applying a logarithmic transformation to the x- or y-axis.
#' Defaults to the natural log (`base = exp(1)`).
#'
#' @param base Logarithm base. Default is `exp(1)` (natural log).
#' @param ... Additional arguments passed to [ggplot2::scale_x_continuous()]
#'   or [ggplot2::scale_y_continuous()].
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' ggpoint(mtcars, mpg, wt) +
#'   scale_x_log()                 # natural log on x
#'
#' ggpoint(mtcars, mpg, wt) +
#'   scale_y_log(base = 10)        # log10 on y
#'
#' ggpoint(mtcars, mpg, wt) +
#'   scale_x_log(base = 2)         # log base 2 on x
#' }
#'
#' @export
scale_x_log <- function(base = exp(1), ...) {
  ggplot2::scale_x_continuous(
    ...,
    transform = scales::log_trans(base = base)
  )
}

#' @rdname scale_x_log
#' @export
scale_y_log <- function(base = exp(1), ...) {
  ggplot2::scale_y_continuous(
    ...,
    transform = scales::log_trans(base = base)
  )
}

#' Log(1 + x) scale (safe logarithmic transform)
#'
#' Shortcut for applying a `log1p()` (i.e., `log(1 + x)`) transformation
#' to the x- or y-axis. This is numerically stable for small or zero values.
#'
#' @inheritParams scale_x_log
#'
#' @examples
#' \dontrun{
#' ggpoint(mtcars, mpg, wt) +
#'   scale_x_log1p() +
#'   scale_y_log1p()
#' }
#'
#' @export
scale_x_log1p <- function(...) {
  ggplot2::scale_x_continuous(..., transform = scales::log1p_trans())
}

#' @rdname scale_x_log1p
#' @export
scale_y_log1p <- function(...) {
  ggplot2::scale_y_continuous(..., transform = scales::log1p_trans())
}

#' Stay-scale axis transformation (log1p with custom breaks)
#'
#' Convenience scales for plotting length-of-stay or similar count variables
#' on a `log1p` scale (i.e. \eqn{log(1+x)}), while providing more interpretable
#' breaks. By default, breaks are fixed at common stay durations up to 90
#' (0, 1, 2, 3, 5, 7, 10, 14, 21, 28, 42, 60, 90), then continue doubling
#' (180, 360, 720, ...) until the maximum data value.
#'
#' These functions wrap [ggplot2::scale_x_continuous()] and
#' [ggplot2::scale_y_continuous()] with a predefined transform and breaks.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param breaks Break specification. By default, a function from
#'   `.log_stay_breaks()` which returns 0–90 plus doubling thereafter.
#'   Can be replaced by any numeric vector or function as accepted by
#'   ggplot2 scales.
#'
#' @return A ggplot2 scale object.
#'
#' @seealso [ggplot2::scale_x_continuous()], [scales::log1p_trans()]
#'
#' @examples
#' \dontrun{
#' library(tweedie)
#'
#' tw <- tweedie::rtweedie(n = 1000, mu = 7, phi = 2, power = 1.5)
#' set.seed(123)
#' df <- data.frame(x = round(tw))
#' gghistogram(df, x = x, probs = .5) +
#'   scale_x_log_stay() +
#'   theme_view()
#' }
#'
#' @export
scale_x_log_stay <- function(...,
                             breaks = ggplot2::waiver(),
                             labels = scales::label_comma(accuracy = 1, trim = TRUE)) {
  if (ggplot2::is_waiver(breaks)) {
    breaks <- .log_stay_breaks()
  }
  ggplot2::scale_x_continuous(
    ...,
    breaks    = breaks,
    labels    = labels,
    transform = scales::log1p_trans()
  )
}

#' @rdname scale_x_log_stay
#' @export
scale_y_log_stay <- function(...,
                             breaks = ggplot2::waiver(),
                             labels = scales::label_comma(
                               accuracy = 1, trim = TRUE
                             )) {
  if (ggplot2::is_waiver(breaks)) {
    breaks <- .log_stay_breaks()
  }
  ggplot2::scale_y_continuous(
    ...,
    breaks    = breaks,
    labels    = labels,
    transform = scales::log1p_trans()
  )
}

#' Continuous color gradient by month (for Date variables)
#'
#' A ggplot2 continuous color scale for Date variables. It generates
#' colorbar breaks every N months and formats labels as "YYYY-MM".
#'
#' @param by_month Integer; interval in months between colorbar ticks.
#'   Default is `6`.
#' @param palette Either a palette function, a vector of colors, or a preset name
#'   (case-insensitive). Supported presets include:
#'   `"rainbow"`, `"viridis"`, `"spectral"`, `"ylgnbu"`, `"zissou"`, `"roma"`,
#'   `"vik"`, `"cividis"`, `"berlin"`.
#' @param n Integer; number of colors sampled from the palette. Default `256`.
#' @param include_endpoints Logical; include the min/max dates as ticks if `TRUE`.
#'   Default `FALSE` (to avoid label overlap).
#' @param ... Additional args passed to [ggplot2::scale_color_gradientn()].
#'
#' @return A ggplot2 color scale for month-based Date aesthetics.
#'
#' @examples
#' \dontrun{
#' # A/E Ratio
#' ggline(aer_data, x = elpm, y = ratio, color = uym, group = uym) +
#'   scale_color_by_month_gradientn(by_month = 6) +
#'   theme_view()
#' }
#'
#' @export
scale_color_by_month_gradientn <- function(by_month = 6,
                                           palette = "ylgnbu",
                                           n = 256,
                                           include_endpoints = FALSE,
                                           ...) {
  # Palette preset handler
  get_palette <- function(p, n) {
    if (is.function(p)) return(p(n))
    if (is.character(p) && length(p) == 1L) {
      pname <- tolower(p)
      if (pname == "rainbow")
        return(grDevices::rainbow(n, start = .05, end = .65))
      pals <- list(
        viridis  = "Viridis",
        spectral = "Spectral",
        ylgnbu   = "YlGnBu",
        zissou   = "Zissou 1",
        roma     = "Roma",
        vik      = "Vik",
        cividis  = "Cividis",
        berlin   = "Berlin"
      )
      key <- match(pname, names(pals))
      if (!is.na(key)) {
        return(grDevices::hcl.colors(n, pals[[key]]))
      } else {
        stop(sprintf("Unknown palette name '%s'.", p), call. = FALSE)
      }
    }
    if (is.vector(p)) return(p)
    stop("Invalid palette argument. Must be a name, vector, or function.", call. = FALSE)
  }

  cols <- get_palette(palette, n)

  # Compute colorbar breaks from numeric limits (ggplot passes Dates as numbers)
  breaks_from_numeric_date <- function(lims) {
    if (length(lims) != 2L || any(!is.finite(lims))) return(NULL)

    lo_date <- as.Date(lims[1], origin = "1970-01-01")
    hi_date <- as.Date(lims[2], origin = "1970-01-01")

    # Anchor: start at Jan or Jul depending on the first month (keeps labels tidy)
    start_year  <- as.integer(format(lo_date, "%Y"))
    start_month <- as.integer(format(lo_date, "%m"))
    anchor_month <- if (start_month <= 6) 1L else 7L
    start_date <- as.Date(sprintf("%04d-%02d-01", start_year, anchor_month))

    break_dates <- seq(from = start_date, to = hi_date, by = paste(by_month, "months"))
    break_dates <- break_dates[break_dates >= lo_date & break_dates <= hi_date]

    if (include_endpoints)
      break_dates <- unique(sort(c(lo_date, break_dates, hi_date)))

    as.numeric(break_dates)  # gradientn expects numeric breaks
  }

  # Label formatter: restore to Date then format as "YYYY-MM"
  labels_ym <- function(x) format(as.Date(x, origin = "1970-01-01"), "%Y-%m")

  ggplot2::scale_color_gradientn(
    colours = cols,
    breaks  = breaks_from_numeric_date,
    labels  = labels_ym,
    guide   = ggplot2::guide_colorbar(
      nbin = 256, ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE
    ),
    ...
  )
}

#' Continuous fill gradient by month (for Date variables)
#'
#' A ggplot2 continuous fill scale for Date variables.
#' Generates colorbar breaks every N months (default: 6 months),
#' labeled as "YYYY-MM".
#'
#' @inheritParams scale_color_by_month_gradientn
#'
#' @return A ggplot2 fill scale for month-based Date aesthetics.
#'
#' @export
scale_fill_by_month_gradientn <- function(by_month = 6,
                                          palette = "ylgnbu",
                                          n = 256,
                                          include_endpoints = FALSE,
                                          ...) {
  get_palette <- function(p, n) {
    if (is.function(p)) return(p(n))
    if (is.character(p) && length(p) == 1L) {
      pname <- tolower(p)
      if (pname == "rainbow")
        return(grDevices::rainbow(n, start = .05, end = .65))
      pals <- list(
        viridis  = "Viridis",
        spectral = "Spectral",
        ylgnbu   = "YlGnBu",
        zissou   = "Zissou 1",
        roma     = "Roma",
        vik      = "Vik",
        cividis  = "Cividis",
        berlin   = "Berlin"
      )
      key <- match(pname, names(pals))
      if (!is.na(key)) {
        return(grDevices::hcl.colors(n, pals[[key]]))
      } else {
        stop(sprintf("Unknown palette name '%s'.", p), call. = FALSE)
      }
    }
    if (is.vector(p)) return(p)
    stop("Invalid palette argument. Must be a name, vector, or function.", call. = FALSE)
  }

  cols <- get_palette(palette, n)

  breaks_from_numeric_date <- function(lims) {
    if (length(lims) != 2L || any(!is.finite(lims))) return(NULL)

    lo_date <- as.Date(lims[1], origin = "1970-01-01")
    hi_date <- as.Date(lims[2], origin = "1970-01-01")

    start_year  <- as.integer(format(lo_date, "%Y"))
    start_month <- as.integer(format(lo_date, "%m"))
    anchor_month <- if (start_month <= 6) 1L else 7L
    start_date <- as.Date(sprintf("%04d-%02d-01", start_year, anchor_month))

    break_dates <- seq(from = start_date, to = hi_date, by = paste(by_month, "months"))
    break_dates <- break_dates[break_dates >= lo_date & break_dates <= hi_date]

    if (include_endpoints)
      break_dates <- unique(sort(c(lo_date, break_dates, hi_date)))

    as.numeric(break_dates)
  }

  labels_ym <- function(x) format(as.Date(x, origin = "1970-01-01"), "%Y-%m")

  ggplot2::scale_fill_gradientn(
    colours = cols,
    breaks  = breaks_from_numeric_date,
    labels  = labels_ym,
    guide   = ggplot2::guide_colorbar(
      nbin = 256, ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE
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
  return(get(colors, envir = .GGSHORT_ENV))
}

get_twelve_colors <- function() {
  return(local(.TWELVE_COLORS, envir = .GGSHORT_ENV))
}


# Suppress warnings functions ---------------------------------------------

#' Suppress "Removed rows" warnings in ggplot2
#'
#' Evaluates an expression while silencing the common ggplot2 warnings:
#' \itemize{
#'   \item `"Removed n row(s) containing missing values"`
#'   \item `"Removed n row(s) containing missing values or values outside the scale range"`
#' }
#'
#' Such warnings are typically emitted by `geom_*()` layers (e.g.,
#' `geom_point()`, `geom_bar()`, `geom_text()`) when data contain `NA`s
#' or values outside the axis limits. In exploratory plots or expected-data
#' scenarios, these warnings may be considered harmless.
#'
#' @param expr An expression to evaluate, usually a ggplot2 plotting call
#'   (e.g., `print(p)` or a `ggplot` expression).
#'
#' @return The result of evaluating `expr`. If `expr` itself returns
#'   invisibly, the result will also be invisible.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = c("A", "B", NA), y = c(1, 2, NA))
#'
#' p <- ggbar(df, x = x, y = y)
#'
#' # Normally produces warnings about removed rows
#' print(p)
#'
#' # Suppress those warnings
#' suppress_geom_removed_warnings(print(p))
#' }
#'
#' @export
suppress_geom_removed_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^Removed \\d+ row[s]? containing missing values", msg) ||
          grepl("values outside the scale range", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
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

#' Stay scale break generator
#'
#' Generate a sequence of breaks suitable for stay/day variables
#' on a log1p scale. Returns the fixed base breaks up to 90,
#' and then continues doubling (180, 360, 720, ...) until the
#' maximum of the data range.
#'
#' Designed to be used as a `breaks` function inside
#' [ggplot2::scale_x_continuous()] or [ggplot2::scale_y_continuous()].
#'
#' @param breaks Numeric vector of initial break values
#'   to use before the doubling scheme starts. Default is
#'   common stay/day cutoffs (0, 1, 2, 3, 5, 7, 10, 14, 21, 28, 42, 60, 90).
#'
#' @return A function that takes a numeric vector `x`
#'   (the axis data range, provided by ggplot2) and returns
#'   a sorted numeric vector of break positions.
#'
#' @keywords internal
#' @noRd
.log_stay_breaks <- function(breaks = c(0, 1, 2, 3, 5, 7, 10, 14, 21, 30, 45, 60, 90, 180, 365)) {
  force(breaks)
  breaks <- sort(unique(breaks))
  function(x) {
    rng <- range(x, finite = TRUE)
    if (!is.finite(rng[2L])) return(breaks)

    brks <- breaks
    last <- max(breaks)

    # 2 times after 365
    while (last * 2 <= rng[2L]) {
      last <- last * 2
      brks <- c(brks, last)
    }

    sort(unique(brks[brks >= rng[1L] & brks <= rng[2L]]))
  }
}
