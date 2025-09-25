#' Box plot with optional jitter, mean marker and side density
#'
#' Quickly draw grouped box plots with a consistent interface. You pass
#' **column names as strings** for the value and group variables. The function
#' can overlay jittered points, add mean markers, and (optionally) attach a
#' density subplot to the **right** (or to the **top** when `flip = TRUE`).
#'
#' @param data A data frame.
#' @param x_var Character. Numeric column name for the box plot value
#'   (e.g., `"Sepal.Length"`).
#' @param color_var Character. Grouping/color column name (typically a factor;
#'   e.g., `"Species"`).
#' @param width Numeric box width passed to [ggplot2::geom_boxplot()] (default `0.6`).
#' @param varwidth Logical; if `TRUE`, draw boxes with widths proportional to
#'   the square-root of group sample sizes.
#' @param flip Logical; if `TRUE`, flip coordinates to make a horizontal box plot.
#' @param show_point Logical; if `TRUE`, overlay jittered points on the boxes.
#' @param show_mean Logical; if `TRUE`, add mean markers using [stat_mean_point()].
#'   Two layers are added: a black stroked point and a red point grouped by `color_var`.
#' @param show_density Logical; if `TRUE`, attach a density plot of `x_var`
#'   by `color_var` to the main plot (to the **right**; to the **top** when `flip = TRUE`).
#' @param show_label Logical; if `TRUE`, add text labels at each box's median
#'   position using the `color_var` values.
#' @param label_args A named list of `geom_text()` style options for labels.
#'   Supported names: `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param palette Discrete palette name for categorical `color_var`, forwarded to
#'   [ggplot2::scale_color_brewer()] and [ggplot2::scale_fill_brewer()].
#'   Default `"Set1"`.
#' @param title,subtitle,caption Optional text passed to plot labels (via your theme helper).
#' @param theme One of `"view"`, `"save"`, or `"shiny"`, forwarded to [switch_theme()].
#' @param ... Additional arguments forwarded to [switch_theme()].
#'
#' @details
#' When `show_density = TRUE`, the side density is created with minimal
#' styling (no axes/legend margins) and attached using an internal gtable
#' combiner. The result is wrapped back into a regular ggplot via
#' [grob_to_ggplot()], so you can continue adding layers/themes as usual.
#'
#' @return A ggplot object.
#'
#' @seealso [stat_mean_point()], [ggplot2::geom_boxplot()], [grob_to_ggplot()],
#'   [ggdensity()], [switch_theme()]
#'
#' @examples
#' \donttest{
#' # Basic grouped box plot with jittered points
#' box_plot(
#'   iris, x_var = "Sepal.Length", color_var = "Species",
#'   title = "Box plot example"
#' )
#'
#' # Add labels at each box's median and hide mean markers
#' box_plot(
#'   iris, x_var = "Sepal.Length", color_var = "Species",
#'   show_mean = FALSE, show_label = TRUE,
#'   label_args = list(vjust = -0.4),
#'   title = "Box plot example"
#' )
#'
#' # Horizontal box plot with density on top
#' box_plot(
#'   iris, x_var = "Sepal.Length", color_var = "Species",
#'   flip = TRUE, show_density = TRUE,
#'   title = "Box plot example"
#' )
#' }
#'
#' @export
box_plot <- function(data, x_var, color_var,
                     width = 0.6,
                     varwidth = FALSE,
                     flip = FALSE,
                     show_point = TRUE,
                     show_mean = TRUE,
                     show_density = FALSE,
                     show_label = FALSE,
                     label_args = list(
                       family = getOption("ggshort.font"),
                       size   = 4,
                       angle  = 0,
                       hjust  = 0.5,
                       vjust  = 0.5,
                       color  = "black"
                     ),
                     palette = "Set1",
                     title = NULL, subtitle = NULL, caption = NULL,
                     theme = c("view", "save", "shiny"), ...) {
  if (!inherits(data, "data.frame"))
    stop("`data` must be a data.frame.", call. = FALSE)
  theme <- match.arg(theme)

  x_var     <- instead::capture_names(data, !!rlang::enquo(x_var))
  color_var <- instead::capture_names(data, !!rlang::enquo(color_var))

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[[color_var]], y = .data[[x_var]], fill = .data[[color_var]])
    ) +
    ggplot2::geom_boxplot(
      width = width, varwidth = varwidth,
      outlier.shape = if (show_point) NA else 19,
      alpha = .7
    ) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::scale_fill_brewer(palette = palette)

  if (show_point)
    p <- p + ggplot2::geom_jitter(
      ggplot2::aes(group = .data[[color_var]]),
      width = width * .4, height = 0, alpha = .5
    )

  if (show_label) {
    args <- .modify_label_args(label_args)
    # add labels at median
    p <- p + ggplot2::stat_summary(
      ggplot2::aes(label = .data[[color_var]]),
      fun = stats::median, geom = "text",
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  if (show_mean)
    p <- p +
      stat_mean_point(stroke = 1.5) +
      stat_mean_point(
        ggplot2::aes(group = .data[[color_var]]),
        color = "red", stroke = 1
      )

  p <- p +
    labs(title = title, subtitle = subtitle, caption = caption) +
    switch_theme(theme = theme, ...)

  if (flip)
    p <- p + ggplot2::coord_flip()

  if (show_density && !flip) {
    args <- .modify_label_args(label_args)
    p_y <- ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = .data[[x_var]], fill = .data[[color_var]])
    ) +
      ggplot2::geom_density(alpha = .7, linewidth = .3) +
      ggplot2::theme_void(base_family = args$family) +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0)) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_brewer(palette = palette)
    p <- .add_plot_axis(p, p_y, position = "right")
    p <- grob_to_ggplot(p)
  } else if (show_density && flip) {
    args <- .modify_label_args(label_args)
    p_x <- ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = .data[[x_var]], fill = .data[[color_var]])
    ) +
      ggplot2::geom_density(alpha = .7, linewidth = .3) +
      ggplot2::theme_void(base_family = args$family) +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0)) +
      ggplot2::scale_fill_brewer(palette = palette)
    p <- .add_plot_axis(p, p_x, position = "top")
    p <- grob_to_ggplot(p)
  }

  p
}
