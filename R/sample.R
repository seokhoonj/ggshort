#' Plot sampling distributions of estimated proportions
#'
#' Generates a faceted histogram and density plot of simulated sampling
#' distributions for proportion estimates (`phat`), based on a
#' `sample_size` design object.
#'
#' @param x A `sample_size` object created by `compute_sample_size(as_dt = TRUE)`.
#' @param times Integer; number of simulated samples per parameter combination (default: `5000`).
#' @param fmt Function applied to x-axis labels (default: percentage formatter).
#' @param nrow Integer; number of rows for facet layout (default: automatic).
#' @param ncol Integer; number of columns for facet layout (default: automatic).
#' @param scales Character; facet scaling, one of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"`.
#' @param theme One of `"view"`, `"save"`, or `"shiny"`, passed to [ggshort::switch_theme()].
#' @param ... Additional arguments forwarded to [ggshort::switch_theme()].
#'
#' @return A `ggplot` object showing simulated sampling distributions
#' for all combinations of `p`, `r`, and `conf.level`.
#'
#' @details
#' Each facet corresponds to a unique parameter combination, labelled
#' by `p`, `r`, `conf.level`, and `n`.
#'
#' The x-axis shows simulated proportion estimates (`phat`), and the
#' y-axis shows estimated density. This visualization is useful for
#' understanding how precision and uncertainty change with sample size.
#'
#' @seealso [instead::compute_sample_size()], [instead::simulate_sample_size()]
#'
#' @examples
#' \donttest{
#' ss <- instead::compute_sample_size(p = c(0.03, 0.05), r = c(0.1, 0.2),
#'                                    conf.level = 0.95, as_dt = TRUE)
#' plot_sample_size(ss, times = 3000)
#' }
#'
#' @export
plot_sample_size <- function(x, times = 5000,
                             fmt = function(x) sprintf("%.2f", x*100),
                             nrow = NULL, ncol = NULL, scales = "fixed",
                             theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "sample_size")
  theme <- match.arg(theme)

  sim <- instead::simulate_sample_size(x, times = times)

  ggplot2::ggplot(sim, ggplot2::aes(x = .data$phat)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 50, alpha = 0.35) +
    ggplot2::geom_density(linewidth = .9) +
    stat_mean_vline() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$ci_lo),
                        color = "gray40", linetype = "dotdash") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$ci_hi),
                        color = "gray40", linetype = "dotdash") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$band_lo),
                        color = "firebrick") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$band_hi),
                        color = "firebrick") +
    ggplot2::scale_x_continuous(labels = fmt) +
    ggplot2::scale_y_continuous(labels = instead::as_comma) +
    ggplot2::facet_wrap(~ label, nrow = nrow, ncol = ncol, scales = scales) +
    ggplot2::labs(
      title = expression("Sampling distribution of " * hat(p)),
      x = expression(hat(p)),
      y = "Density"
    ) +
    switch_theme(theme = theme, ...)
}

#' Plot method for sample_size objects
#'
#' S3 method for [plot()] applied to `sample_size` objects.
#' Delegates to [plot_sample_size()] to visualize the simulated
#' sampling distributions of estimated proportions.
#'
#' @inheritParams plot_sample_size
#' @param ... Additional arguments passed to [plot_sample_size()].
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \donttest{
#' ss <- instead::compute_sample_size(p = 0.05, r = 0.1, as_dt = TRUE)
#' plot(ss)
#' }
#'
#' @method plot sample_size
#' @export
plot.sample_size <- function(x, times = 5000,
                             fmt = function(x) sprintf("%.2f", x*100),
                             nrow = NULL, ncol = NULL, scales = "fixed",
                             theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  plot_sample_size(x, times, fmt, nrow, ncol, scales, theme, ...)
}
