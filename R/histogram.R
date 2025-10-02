#' Histogram plot
#'
#' Draw grouped histograms with optional mean/median guides and quantile
#' labels. Built on top of [gghistogram()] with a simplified interface:
#' select a single x variable (`x_var`) and (optionally) a grouping/coloring
#' variable (`color_var`). Optionally attach a compact density strip above
#' the plot via `show_density = TRUE`.
#'
#' @param data A data.frame containing the variables to plot.
#' @param x_var Column to use for the x-axis. May be either a column name
#'   (character string) or a single column index (integer).
#' @param color_var Optional column for grouping/coloring the bars. May be
#'   either a column name (character string) or a single column index (integer).
#'   If supplied, it must exist in `data`.
#' @param probs Numeric vector of probabilities in \[0, 1] used for quantile
#'   guides/labels when those layers are enabled. Default `0.95`.
#' @param na.rm Logical; silently drop missing values if `TRUE`. Default `TRUE`.
#' @param y Numeric y-position for quantile labels. Default `Inf` (top of panel).
#' @param bins Number of bins for the histogram. Ignored if `binwidth` is
#'   specified. Default `30`.
#' @param binwidth Width of each histogram bin. Overrides `bins` if provided.
#'   Should be a single numeric value.
#' @param show_mean Logical; add vertical mean line(s) if `TRUE`. Default `FALSE`.
#' @param show_median Logical; add vertical median line(s) if `TRUE`. Default `FALSE`.
#' @param show_density Logical; if `TRUE`, add a small density panel of `x_var`
#'   (by `color_var`, when present) above the histogram. Default `FALSE`.
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
#' @param title,subtitle,caption Plot title, subtitle, and caption; passed to
#'   [ggplot2::labs()].
#' @param theme A ggshort theme key passed to [switch_theme()]:
#'   one of `"view"`, `"save"`, or `"shiny"`.
#' @param ... Additional arguments forwarded to `switch_theme(theme = ...)`.
#'
#' @details
#' - **Variable selection:** `x_var` and `color_var` can be given as a name
#'   (character) or single column index (integer). Integer inputs are resolved
#'   to names within `data`.
#' - **Coloring logic:** If `color_var` is categorical (factor/character/logical),
#'   a discrete Brewer palette (`palette`) is applied. If numeric, a continuous
#'   gradient is used (via [ggplot2::scale_color_gradient()]); quantile guides
#'   and mean/median lines remain available.
#' - **Density strip:** When `show_density = TRUE`, a compact density plot is
#'   composed above the main histogram to help compare shapes across groups.
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' # Histogram by group with quantile labels
#' histogram_plot(iris, x_var = "Sepal.Length", color_var = "Species",
#'                probs = 0.9, show_label = TRUE)
#'
#' # Using column indices and adding mean/median guides
#' histogram_plot(iris, x_var = 1, color_var = 5,
#'                show_mean = TRUE, show_median = TRUE,
#'                show_density = TRUE)
#' }
#'
#' @seealso [gghistogram()], [ggdensity()], [stat_density_quantile_vline()],
#'   [stat_density_quantile_text()]
#'
#' @export
histogram_plot <- function(data, x_var, color_var,
                           probs = .95, na.rm = TRUE, y = Inf,
                           bins = 30, binwidth = NULL,
                           show_mean = FALSE,
                           show_median = FALSE,
                           show_density = FALSE,
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

  # main histogram
  if (has_color) {
    p <- gghistogram(
      data = data,
      x = .data[[x_var]],
      color = .data[[color_var]],
      fill  = .data[[color_var]],
      group = .data[[color_var]],
      probs = probs, na.rm = na.rm, y = y,
      bins = bins, binwidth = binwidth,
      show_mean = show_mean,
      show_median = show_median,
      show_vline = show_vline,
      show_label = show_label,
      label_args = label_args
    ) +
      switch_theme(theme = theme, legend.position = "right", ...)
  } else {
    p <- gghistogram(
      data = data,
      x = .data[[x_var]],
      probs = probs, na.rm = na.rm, y = y,
      bins = bins, binwidth = binwidth,
      show_mean = show_mean,
      show_median = show_median,
      show_vline  = show_vline,
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

  p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  if (show_density) {
    args <- .modify_label_args(label_args)
    if (has_color) {
      p_y <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(x = .data[[x_var]], fill = .data[[color_var]])
      )
    } else {
      p_y <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(x = .data[[x_var]])
      )
    }
    p_y <- p_y +
      ggplot2::geom_density(alpha = .7, linewidth = .3) +
      ggplot2::theme_void(base_family = args$family) +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0)) +
      ggplot2::scale_fill_brewer(palette = palette)
    p <- .add_plot_axis(p, p_y, position = "top")
    p <- grob_to_ggplot(p)
  }

  p
}
