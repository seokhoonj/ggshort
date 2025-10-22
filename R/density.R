#' Density plot
#'
#' Draw kernel density curves by group with optional mean/median guides
#' and quantile labels. Built on top of [ggdensity()] with simplified
#' interface for a single `x_var` and grouping/coloring by `color_var`.
#'
#' @param data A data.frame containing the variables to plot.
#' @param x_var Column to use for the x-axis density variable. May be
#'   either a column name (character string) or a single column index (integer).
#' @param color_var Optional column for grouping/coloring densities. May be
#'   either a column name (character string) or a single column index (integer).
#'   If supplied, it must exist in `data`.
#' @param probs Numeric vector of probabilities in \[0, 1] for quantile guides
#'   (used when `show_label = TRUE`). Default `0.95`.
#' @param na.rm Logical; remove missing values silently if `TRUE`. Default `TRUE`.
#' @param y Numeric y position for quantile labels. Default `Inf` (top of panel).
#' @param show_mean Logical; if `TRUE`, add vertical mean line(s).
#'   Default `FALSE`.
#' @param show_median Logical; if `TRUE`, add vertical median line(s).
#'   Default `FALSE`.
#' @param show_vline Logical; draw vertical quantile line(s) at `probs`
#'   (via `stat_density_quantile_vline()`). Default `TRUE`.
#' @param show_label Logical; if `TRUE`, add quantile label(s) at `probs`.
#'   (via `stat_density_quantile_text()`). Default `TRUE`.
#' @param label_digits Integer; number of decimal digits for quantile labels.
#'   Default `1`.
#' @param label_args A named list of text styling options for quantile labels
#'   (`geom_text()`): `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'   Default:
#'   `list(family = getOption("ggshort.font"), size = 4, angle = 90,
#'         hjust = 2, vjust = 0.5, color = "black")`.
#' @param palette Discrete palette name for categorical `color_var`, forwarded to
#'   [ggplot2::scale_color_brewer()] and [ggplot2::scale_fill_brewer()].
#'   Default `"Set1"`. Ignored when `color_var` is numeric.
#'   See [RColorBrewer::display.brewer.all()] for available palettes.
#' @param title,subtitle,caption Plot title, subtitle, and caption. Passed to
#'   [ggplot2::labs()].
#' @param theme A ggshort theme key passed to [switch_theme()]: one of
#'   `"view"`, `"save"`, or `"shiny"`.
#' @param ... Additional arguments forwarded to `switch_theme(theme = ...)`.
#'
#' @details
#' - **Variable selection:** Both `x_var` and `color_var` can be specified by
#'   name (character) or column index (integer). If an integer is given,
#'   it is resolved to the corresponding column name in `data`.
#' - **Coloring logic:** If `color_var` is categorical (factor/character/logical),
#'   a discrete Brewer palette (`palette`) is applied. If numeric, a continuous
#'   gradient (blue -> red) is used, and `show_mean`/`show_median` apply but
#'   quantile labels remain available.
#'
#' @return A ggplot object.
#'
#' @seealso [ggdensity()], [stat_density_quantile_vline()],
#'   [stat_density_quantile_text()],
#'
#' @examples
#' \donttest{
#' # Basic density by species with quantile labels
#' plot_density(iris, x_var = "Sepal.Length", color_var = "Species",
#'              probs = 0.9, show_label = TRUE)
#'
#' # Using column indices instead of names
#' plot_density(iris, x_var = 1, color_var = 5,
#'              show_mean = TRUE, show_median = TRUE)
#' }
#'
#' @export
plot_density <- function(data, x_var, color_var,
                         probs = .95, na.rm = TRUE, y = Inf,
                         show_mean = FALSE,
                         show_median = FALSE,
                         show_vline = TRUE,
                         show_label = TRUE,
                         label_digits = 1,
                         label_args = list(
                           family = getOption("ggshort.font"),
                           size  = 4,
                           angle = 90,
                           hjust = 2,
                           vjust = 0.5,
                           color = "black"
                         ),
                         palette = "Set1",
                         title = NULL, subtitle = NULL, caption = NULL,
                         theme = c("view", "save", "shiny"),
                         ...) {
  if (!inherits(data, "data.frame"))
    stop("`data` must be a data.frame.", call. = FALSE)
  theme <- match.arg(theme)

  x_var     <- instead::capture_names(data, !!rlang::enquo(x_var))

  has_color <- !missing(color_var)
  if (has_color)
    color_var <- instead::capture_names(data, !!rlang::enquo(color_var))

  # resolve x_var
  if (is.numeric(x_var)) {
    if (length(x_var) != 1L || x_var < 1L || x_var > ncol(data))
      stop("`x_var` must be a single valid column index.", call. = FALSE)
    x_var <- names(data)[x_var]
  }
  if (!is.character(x_var) || length(x_var) != 1L)
    stop("`x_var` must be a single column name or index.", call. = FALSE)
  if (!x_var %in% names(data))
    stop("`x_var` not found in `data`: ", x_var, call. = FALSE)

  # resolve color_var (optional)
  if (has_color) {
    if (is.numeric(color_var)) {
      if (length(color_var) != 1L || color_var < 1L || color_var > ncol(data))
        stop("`color_var` must be a single valid column index.", call. = FALSE)
      color_var <- names(data)[color_var]
    }
    if (!is.character(color_var) || length(color_var) != 1L)
      stop("`color_var` must be a single column name or index.", call. = FALSE)
    if (!color_var %in% names(data))
      stop("`color_var` not found in `data`: ", color_var, call. = FALSE)
  }

  # main density plot
  if (has_color) {
    p <- ggdensity(
      data = data,
      x = .data[[x_var]],
      color = .data[[color_var]],
      fill  = .data[[color_var]],
      group = .data[[color_var]],
      probs = probs, na.rm = na.rm, y = y,
      show_mean = show_mean,
      show_median = show_median,
      show_vline = show_vline,
      show_label = show_label,
      label_digits = label_digits,
      label_args = label_args
    ) +
      switch_theme(theme = theme, legend.position = "right", ...)
  } else {
    p <- ggdensity(
      data = data,
      x = .data[[x_var]],
      fill = "",
      probs = probs, na.rm = na.rm, y = y,
      show_mean = show_mean,
      show_median = show_median,
      show_vline = show_vline,
      show_label = show_label,
      label_digits = label_digits,
      label_args = label_args
    ) +
      switch_theme(theme = theme, legend.position = "none", ...)
  }

  # add color/fill scales if grouping is categorical
  if (has_color) {
    if (is.numeric(data[[color_var]])) {
      colors <- get_two_colors("deep")
      low <- colors[[1L]]; high <- colors[[2L]]
      p <- p + ggplot2::scale_color_gradient(low = low, high = high)
    } else {
      p <- p +
        ggplot2::scale_color_brewer(palette = palette) +
        ggplot2::scale_fill_brewer(palette = palette)
    }
  } else {
    p <- p +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::scale_fill_brewer(palette = palette)
  }

  p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
}

# #' Simple density plot
# #'
# #' Show a simple density plot.
# #'
# #' @param data a data frame
# #' @param x a name specifying numeric column.
# #' @param facet a name of columns for applying `facet_wrap()` function.
# #' @param kernel a character string giving the smoothing kernel to be used.
# #' This must partially match one of "gaussian", "rectangular", "triangular",
# #' "epanechnikov", "biweight", "cosine" or "optcosine", with default "gaussian",
# #' and may be abbreviated to a unique prefix (single letter).
# #' "cosine" is smoother than "optcosine", which is the usual ‘cosine’ kernel in
# #' the literature and almost MSE-efficient. However, "cosine" is the version
# #' used by S.
# #' @param probs probability for a density function.
# #' @param logscale a boolean value that determines whether or not to make the x
# #' variable logscale.
# #' @param round_digits rounding digits
# #' @param scales Should scales be fixed ("fixed", the default), free ("free"),
# #' or free in one dimension ("free_x", "free_y")?
# #' @param family label font-family
# #'
# #' @return A ggplot object.
# #'
# #' @examples
# #' \donttest{
# #' set.seed(123)
# #' x <- sample(1:10, size = 1000, replace = TRUE)
# #' facet1 <- sample(c("A", "B", "C"), size = 1000, replace = TRUE)
# #' facet2 <- sample(c("X", "Y", "Z"), size = 1000, replace = TRUE)
# #' data <- data.frame(x, facet1, facet2)
# #' ggdensity(data, x = x, facet = list(facet1, facet2), family = NA) +
# #'  theme_view(family = NA)
# #' }
# #'
# #' @export
# densplot <- function(data, x, facets, kernel = "gaussian", probs = .95,
#                       logscale = F, round_digits = 0, scales = "fixed",
#                       family = "Comic Sans MS") {
#   if (is.vector(data)) {
#     dt <- data.table::data.table(x = data)
#     .x <- "x"
#   } else {
#     .x <- rlang::as_name(rlang::enquo(x))
#     if (missing(facets)) {
#       if (!inherits(data, "data.table")) {
#         dt <- data.table::as.data.table(data[, .x, drop = FALSE])
#       } else {
#         dt <- data[, .SD, .SDcols = .x]
#       }
#     } else {
#      # .facet <- instead::match_cols(data, sapply(rlang::enexpr(facets), rlang::as_name))
#       .facet <- instead::capture_names(data, !!rlang::enquo(facets))
#       if (!inherits(data, "data.table")) {
#         dt <- data.table::as.data.table(data[, c(.x, .facet)])
#       } else {
#         dt <- data[, .SD, .SDcols = c(.x, .facet)]
#       }
#     }
#   }
#
#   # if (logscale)
#   #   data.table::set(dt, j = .x, value = log(1 + dt[[.x]]))
#
#   area <- prob <- y <- NULL
#   from <- min(dt[[.x]])
#   if (missing(facets)) {
#     ds <- dt[, list(x = unlist(lapply(.SD, function(x) stats::density(x, kernel = kernel, from = from)[["x"]])),
#                     y = unlist(lapply(.SD, function(x) stats::density(x, kernel = kernel, from = from)[["y"]]))),
#              .SDcols = .x]
#     cutoff_ds <- ds[, list(
#       prob = probs,
#       cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
#     ), .SDcols = "x"]
#     cutoff_dt <- dt[, list(
#       prob = probs,
#       cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
#     ), .SDcols = .x]
#   } else {
#     ds <- dt[, list(x = unlist(lapply(.SD, function(x) density(x, kernel = kernel, from = from)[["x"]])),
#                     y = unlist(lapply(.SD, function(x) density(x, kernel = kernel, from = from)[["y"]]))),
#              keyby = .facet, .SDcols = .x]
#     cutoff_ds <- ds[, list(
#       prob = probs,
#       cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
#     ), keyby = .facet, .SDcols = "x"]
#     cutoff_dt <- dt[, list(
#       prob = probs,
#       cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
#     ), keyby = .facet, .SDcols = .x]
#   }
#
#   cutoff <- cutoff_dt[prob == probs[length(probs)]]
#   levels <- paste0(c(">", "<"), probs[length(probs)] * 1e2, "%")
#
#   cutoff_dt[, y := Inf]
#   cutoff_dt[, area := levels[1L]]
#
#   if (missing(facets)) {
#     ds[, cutoff := cutoff$cutoff]
#   } else {
#     ds[cutoff, cutoff := cutoff, on = .facet]
#   }
#   ds[, area := factor(ifelse(x >= cutoff, levels[1L], levels[2L]),
#                       levels = levels)]
#
#   g <- ggplot(data = ds[], aes(x = x, ymin = 0, ymax = y, group = area, fill = area)) +
#     geom_ribbon() + geom_line(aes(y = y)) +
#     # list(
#     #   if (logscale) {
#     #     geom_text(data = cutoff_dt, aes(
#     #       x = cutoff, y = y,
#     #       label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1, round_digits)),
#     #       group = area), family = family, hjust = -0.1, vjust = 2)
#     #   } else {
#     #     geom_text(data = cutoff_dt, aes(
#     #       x = cutoff, y = y,
#     #       label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff, round_digits)),
#     #       group = area), family = family, hjust = -0.1, vjust = 2)
#     #   }) +
#     geom_text(data = cutoff_dt, aes(
#       x = cutoff, y = y,
#       label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff, round_digits)),
#       group = area), family = family, hjust = -0.1, vjust = 2) +
#     list(
#       if (logscale) {
#         scale_x_log10()
#       }
#     ) +
#     geom_vline(data = cutoff_dt, aes(xintercept = cutoff), color = "red",
#                linetype = "dashed") +
#     list(
#       if (!missing(facets)) {
#         facet_wrap(formula(paste("~", paste(.facet, collapse = "+"))),
#                    scales = scales)
#       }
#     ) +
#     ylab("density")
#
#   return(g)
# }

# ggdensity <- function(data, x, facets = NULL,
#                       kernel = "gaussian", probs = .95,
#                       logscale = FALSE, round_digits = 0,
#                       scales = "fixed", family = getOption("ggshort.font)) {
#   if (is.vector(data)) {
#     dt <- data.table::data.table(`..x` = data)
#     x_col <- "..x"
#   } else {
#     x_col <- rlang::as_name(rlang::enquo(x))
#     if (!inherits(data, "data.table")) {
#       dt <- data.table::as.data.table(data[, x_col, drop = FALSE])
#       data.table::setnames(dt, x_col, x_col)
#     } else {
#       dt <- data[, .SD, .SDcols = x_col]
#     }
#   }
#
#   # parse facet formula (facets = ~ g1 + g2)
#   facet_vars <- character(0)
#   if (!is.null(facets)) {
#     if (!inherits(facets, "formula"))
#       stop("`facets` must be a formula like ~ group or ~ g1 + g2.", call. = FALSE)
#     facet_vars <- all.vars(facets)
#     if (length(facet_vars)) {
#       if (!inherits(data, "data.table")) {
#         extra <- data.table::as.data.table(data[, facet_vars, drop = FALSE])
#       } else {
#         extra <- data[, .SD, .SDcols = facet_vars]
#       }
#       dt <- cbind(dt, extra)
#     }
#   }
#
#   from <- min(dt[[x_col]], na.rm = TRUE)
#
#   if (!length(facet_vars)) {
#     dens <- stats::density(dt[[x_col]], kernel = kernel, from = from)
#     ds <- data.table::data.table(x = dens$x, y = dens$y)
#
#     cutoff_dt <- data.table::data.table(
#       prob = probs,
#       cutoff = as.numeric(stats::quantile(dt[[x_col]], probs = probs, na.rm = TRUE))
#     )
#   } else {
#     ds <- dt[, {
#       d <- stats::density(get(x_col), kernel = kernel, from = from)
#       data.table::data.table(x = d$x, y = d$y)
#     }, by = facet_vars]
#
#     cutoff_dt <- dt[, {
#       qs <- as.numeric(stats::quantile(get(x_col), probs = probs, na.rm = TRUE))
#       data.table::data.table(prob = probs, cutoff = qs)
#     }, by = facet_vars]
#   }
#
#   # last prob
#   last_prob <- tail(probs, 1L)
#   levels_lbl <- paste0(c(">", "<"), last_prob * 100, "%")
#
#   # label position
#   cutoff_label_dt <- cutoff_dt[prob == last_prob]
#   cutoff_label_dt[, `:=`(y = Inf, area = levels_lbl[1L])]
#
#   # shade
#   if (!length(facet_vars)) {
#     ds[, cutoff := cutoff_label_dt$cutoff]
#   } else {
#     ds <- merge(ds, cutoff_label_dt[, c(facet_vars, "cutoff"), with = FALSE],
#                 by = facet_vars, all.x = TRUE)
#   }
#   ds[, area := factor(ifelse(x >= cutoff, levels_lbl[1L], levels_lbl[2L]),
#                       levels = levels_lbl)]
#
#   p <- ggplot2::ggplot(ds, ggplot2::aes(x = x, ymin = 0, ymax = y, group = area, fill = area)) +
#     ggplot2::geom_ribbon() +
#     ggplot2::geom_line(ggplot2::aes(y = y)) +
#     ggplot2::geom_text(
#       data = cutoff_label_dt,
#       ggplot2::aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)",
#                                                       prob * 100,
#                                                       round(cutoff, round_digits)),
#                    group = area),
#       family = family, hjust = -0.1, vjust = 2
#     ) +
#     ggplot2::geom_vline(data = cutoff_label_dt, ggplot2::aes(xintercept = cutoff),
#                         color = "red", linetype = "dashed") +
#     ggplot2::ylab("density")
#
#   if (isTRUE(logscale)) {
#     p <- p + ggplot2::scale_x_log10()
#   }
#
#   if (length(facet_vars)) {
#     p <- p + ggplot2::facet_wrap(facets, scales = scales)
#   }
#
#   p
# }
