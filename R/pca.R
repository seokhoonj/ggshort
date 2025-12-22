#' PCA plot
#'
#' Draw a PCA biplot-like figure from a data frame. The function computes
#' [stats::prcomp()] on selected numeric columns (`measure_vars`), plots scores
#' on the requested principal components, and overlays loading (variable) arrows.
#'
#' @param data A data.frame containing the variables to be analyzed with PCA.
#' @param measure_vars Columns to use for PCA. Supports:
#'   - unquoted selection, e.g. `c(x1, x2, x3)` or `.(x1, x2, x3)`,
#'   - a character vector of names, e.g. `c("x1","x2","x3")`,
#'   - or integer indices, e.g. `c(1,2,3)`.
#'   If missing, **all numeric columns** in `data` are used. At least two
#'   numeric columns are required.
#' @param color_var Optional single column used to color points. Supports the
#'   same forms as `measure_vars` but must resolve to **exactly one** column.
#' @param x,y Principal component indices for the x- and y-axes (defaults:
#'   `x = 1`, `y = 2`). They must be different and within the number of PCs.
#' @param scale Scaling exponent applied to score axes; set `0` to disable.
#'   Internally the scores on PCs `x` and `y` are divided by `sdev * sqrt(n)`
#'   and then raised to `scale` (biplot-like scaling). Default: `1`.
#' @param alpha Point transparency in \[0, 1]. Default: `0.3`.
#' @param show_arrow Logical; if `TRUE`, draw loading arrows. Default: `TRUE`.
#' @param show_label Logical; if `TRUE`, draw variable names at arrow tips.
#'   Default: `TRUE`.
#' @param label_args A named list of text styling options for loading labels
#'   (`geom_text()`): `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#'   Default:
#'   `list(family = getOption("ggshort.font"), size = 4, angle = 0,
#'         hjust = 0.5, vjust = 0.5, color = "gray7")`.
#' @param show_mean Logical; if `TRUE` and `color_var` is categorical, add group
#'   mean points (shape: O). Ignored for numeric `color_var`. Default `FALSE`.
#' @param show_median Logical; if `TRUE` and `color_var` is categorical, add group
#'   median points (shape: X). Ignored for numeric `color_var`. Default `FALSE`.
#' @param show_ellipse Logical; if `TRUE` and `color_var` is provided, draw
#'   normal-theory ellipses by group at 0.90 level. Default: `TRUE`.
#' @param show_chull Logical; if `TRUE` and `color_var` is categorical,
#'   draw per-group convex hull outlines (via [stat_chull()]).
#'   Default: `FALSE`.
#' @param show_density Logical; if `TRUE` and `color_var` is categorical, attach
#'   marginal density panels (top/right) colored by group. Default `TRUE`.
#' @param palette Discrete palette name for categorical `color_var`, forwarded to
#'   [ggplot2::scale_color_brewer()] and [ggplot2::scale_fill_brewer()].
#'   Default `"Set1"`. Ignored when `color_var` is numeric.
#' @param title,subtitle,caption Plot title, subtitle, and caption. Passed to
#'   [ggplot2::labs()].
#' @param theme A ggshort theme key passed to [switch_theme()]: one of
#'   `"view"`, `"save"`, or `"shiny"`.
#' @param ... Additional arguments forwarded to `switch_theme(theme = ...)`.
#'
#' @details
#' - **Selection helpers:** `measure_vars`/`color_var` are resolved via
#'   `capture_names()`, so `c(a,b)`, `.(a,b)`, character names, and indices
#'   are all supported.
#' - **Coloring logic:** If `color_var` is categorical (factor/character/logical),
#'   a discrete Brewer palette (`palette`) is applied to points and densities.
#'   If `color_var` is numeric, a continuous gradient (blue -> red) is used and
#'   `show_mean`/`show_ellipse`/`show_density` are ignored.
#' - **Scaling:** With `scale != 0`, scores on the chosen PCs are divided by
#'   `sdev * sqrt(n)` and raised to `scale` to approximate classic biplot scaling.
#' - **Arrows/labels:** Loading vectors are scaled to fit within the score range.
#'   Labels are styled via `label_args`.
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' # Quick start: use all numeric columns (Species excluded automatically)
#' plot_pca(iris, x = 1, y = 2)
#'
#' # Explicit variables (character) + categorical coloring with ellipses & means
#' plot_pca(
#'   iris,
#'   measure_vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   color_var = "Species",
#'   x = 1, y = 2,
#'   show_ellipse = TRUE,
#'   show_mean = TRUE,
#'   palette = "Set2",
#'   title = "Iris PCA (categorical coloring)"
#' )
#'
#' # NSE selection (unquoted)
#' plot_pca(
#'   iris,
#'   measure_vars = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#'   color_var = Species
#' )
#'
#' # Integer indices
#' plot_pca(iris, measure_vars = 1:4, color_var = 5)
#'
#' # Numeric color variable -> continuous gradient; ellipses/means/density off
#' df <- iris
#' df$num_group <- df$Sepal.Length
#' plot_pca(df, measure_vars = 1:4, color_var = num_group)
#' }
#'
#' @export
plot_pca <- function(data, measure_vars, color_var,
                     x = 1, y = 2, scale = 1, alpha = .3,
                     show_arrow = TRUE,
                     show_label = TRUE,
                     label_args = list(
                       family = getOption("ggshort.font"),
                       size   = 4,
                       angle  = 0,
                       hjust  = 0.5,
                       vjust  = 0.5,
                       color  = "black"
                     ),
                     show_mean = TRUE,
                     show_median = TRUE,
                     show_ellipse = TRUE,
                     show_chull = FALSE,
                     show_density = FALSE,
                     palette = "Set1",
                     title = NULL, subtitle = NULL, caption = NULL,
                     theme = c("view", "save", "shiny"), ...) {
  if (!inherits(data, "data.frame"))
    stop("`data` must be a data.frame.", call. = FALSE)
  theme <- match.arg(theme)

  if (inherits(data, "data.table"))
    data <- as.data.frame(data)

  # resolve measure_vars: handle missing first, then resolve
  if (missing(measure_vars)) {
    measure_vars <- names(data)[vapply(data, is.numeric, logical(1L))]
    if (length(measure_vars) < 2L) {
      stop("At least two numeric columns are required for PCA. ",
           "No or only one numeric column detected.", call. = FALSE)
    }
  } else {
    measure_vars <- instead::capture_names(data, !!rlang::enquo(measure_vars))
    if (length(measure_vars) < 2L)
      stop("`measure_vars` must select at least two columns.", call. = FALSE)
    .assert_all_numeric(data[, measure_vars, drop = FALSE])
  }

  # resolve color_var (optional)
  if (missing(color_var)) {
    has_color <- FALSE
    is_color_numeric <- FALSE
  } else {
    color_var <- instead::capture_names(data, !!rlang::enquo(color_var))
    if (length(color_var) != 1L)
      stop("`color_var` must resolve to exactly one column.", call. = FALSE)
    has_color <- TRUE
    is_color_numeric <- is.numeric(data[[color_var]])
  }

  # principal components
  pc <- stats::prcomp(data[, measure_vars, drop = FALSE])
  scaled <- as.data.frame(pc$x)
  pc_names <- names(scaled)
  pc_xy <- paste0("PC", c(x, y))
  col_x <- pc_xy[1L]; col_y <- pc_xy[2L]

  # add a color column if requested
  if (has_color) {
    scaled[[color_var]] <- data[[color_var]]
    is_color_numeric <- is.numeric(data[[color_var]])
    if (is_color_numeric) {
      colors <- get_two_colors("deep")
      low <- colors[[1L]]; high <- colors[[2L]]
      scale_color <- ggplot2::scale_color_gradient(
        low = low, high = high
      )
    } else {
      scale_color <- ggplot2::scale_color_brewer(palette = palette)
      scale_fill  <- ggplot2::scale_fill_brewer(palette = palette)
    }
  }

  # explained variance
  sdev <- pc$sdev
  ve <- sdev^2 / sum(sdev^2)
  labs <- sprintf("%s (%.2f%%)", pc_xy, ve[c(x, y)] * 100)
  xlab <- labs[1L]; ylab <- labs[2L]

  # score scaling
  lam <- sdev[c(x, y)] * sqrt(nrow(data))
  if (scale != 0) {
    lam <- lam^scale
    scaled[, c(col_x, col_y)] <- t(t(scaled[, c(col_x, col_y)])/lam)
  }

  # loading (rotation) for arrows
  rotation <- as.data.frame(pc$rotation)
  rotation <- rotation[, pc_xy, drop = FALSE]
  rotation$variable <- rownames(rotation)
  scaler <- min(
    max(abs(scaled[, 1L])) / max(abs(rotation[, 1L])),
    max(abs(scaled[, 2L])) / max(abs(rotation[, 2L]))
  )
  rotation[, 1L:2L] <- rotation[, 1L:2L] * scaler * .8

  # plot
  p <- ggplot2::ggplot(
    data = scaled,
    ggplot2::aes(x = .data[[col_x]], y = .data[[col_y]])
  )

  if (has_color) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[color_var]]), alpha = alpha
    ) + scale_color
  } else {
    p <- p + geom_point(alpha = alpha)
  }

  if (show_arrow) {
    if (show_ellipse || show_density) {
      p <- p +
        ggplot2::geom_segment(
          data = rotation,
          ggplot2::aes(
            x = 0, y = 0,
            xend = .data[[col_x]], yend = .data[[col_y]]
          ),
          arrow = grid::arrow(length = grid::unit(7, "points")),
          color = "grey50"
        )
    } else {
      p <- p +
        ggplot2::geom_segment(
          data = rotation,
          ggplot2::aes(
            x = 0, y = 0,
            xend = .data[[col_x]], yend = .data[[col_y]],
            color = .data[["variable"]]
          ),
          arrow = grid::arrow(length = grid::unit(7, "points"))
        )
    }
  }

  if (show_arrow && show_label) {
    args <- .modify_label_args(label_args)
    p <- p + ggplot2::geom_text(
      data = rotation,
      ggplot2::aes(label = .data[["variable"]]),
      family = args$family, size = args$size, angle = args$angle,
      hjust  = args$hjust,  vjust = args$vjust, color = args$color
    )
  }

  if (show_mean && has_color && !is_color_numeric) {
    args <- .modify_label_args(label_args)
    p <- p + stat_mean_point(
      ggplot2::aes(group = .data[[color_var]], color = .data[[color_var]]),
      size = args$size, shape = 1
    )
  }

  if (show_median && has_color && !is_color_numeric) {
    args <- .modify_label_args(label_args)
    p <- p + stat_median_point(
      ggplot2::aes(group = .data[[color_var]], color = .data[[color_var]]),
      size = args$size, shape = 4
    )
  }

  if (show_ellipse && has_color && !is_color_numeric) {
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(group = .data[[color_var]], color = .data[[color_var]]),
      type = "norm", level = .9, alpha = .9
    )
  }

  if (show_chull && has_color && !is_color_numeric) {
    p <- p + stat_chull(
      ggplot2::aes(group = .data[[color_var]], color = .data[[color_var]])
    )
  }

  p <- p + xlab(xlab) + ylab(ylab) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    switch_theme(theme = theme, ...)

  if (show_density && has_color && !is_color_numeric) {
    args <- .modify_label_args(label_args)
    p_x <- ggplot2::ggplot(
      data = scaled,
      ggplot2::aes(x = .data[[col_x]], fill = .data[[color_var]])) +
      ggplot2::geom_density(alpha = .7, linewidth = .2) +
      ggplot2::theme_void(base_family = args$family) +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0)) +
      scale_fill
    p_y <- ggplot2::ggplot(
      data = scaled,
      ggplot2::aes(x = .data[[col_y]], fill = .data[[color_var]])) +
      ggplot2::geom_density(alpha = .7, linewidth = .2) +
      ggplot2::theme_void(base_family = args$family) +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0)) +
      ggplot2::coord_flip() +
      scale_fill
    p <- .add_plot_axis(p, p_x, position = "top")
    p <- .add_plot_axis(p, p_y, position = "right")
    p <- grob_to_ggplot(p)
  }

  p
}

# Internal helper functions -----------------------------------------------

#' Check that all columns are numeric
#'
#' @param df A data.frame.
#'
#' @return Invisibly returns TRUE if all columns are numeric; otherwise throws error.
#'
#' @keywords internal
#' @noRd
.assert_all_numeric <- function(df) {
  stopifnot(is.data.frame(df))
  is_num <- vapply(df, is.numeric, logical(1L))
  if (!all(is_num)) {
    not_num_cols <- names(df)[!is_num]
    stop("Non-numeric columns found: ", paste(not_num_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Insert a subplot along a main plot edge
#'
#' Attach a small subplot (e.g., a marginal density / histogram or any grob)
#' to one edge of a main plot. Works for faceted ggplots as well— the subplot
#' spans the full panel area across facets.
#' This helper converts inputs as needed:
#'
#' - `main_plot`: must be a ggplot or gtable (used to locate the panel region).
#' - `sub_plot`: may be a ggplot, gtable or grob. Grobs are
#'   wrapped into a 1×1 gtable cell.
#'
#' @param main_plot A ggplot or gtable The plot to which the subplot
#'   will be attached.
#' @param sub_plot A ggplot, gtable or grob to insert
#'   along the selected edge (e.g., a marginal density plot).
#' @param position Edge to attach the subplot to. One of `"top"`, `"bottom"`,
#'   `"right"`, or `"left"`.
#' @param size A `grid::unit` giving the thickness of the inserted band
#'   (height for top/bottom, width for right/left). If a numeric is supplied,
#'   it is interpreted in centimeters (i.e., `grid::unit(size, "cm")`).
#'
#' @return A gtable object. Draw with `grid::grid.draw()` (or print in an
#'   interactive device). The result is no longer a ggplot object.
#'
#' @details
#' - Axis labels/ticks of the `sub_plot` are not removed automatically. If you
#'   want a “pure shape” marginal, style `sub_plot` with `theme_void()` and
#'   `plot.margin = margin(0,0,0,0)`, and match `scale_*_continuous(limits=..., expand=...)`
#'   to the main plot so the shapes align tightly.
#' - When `position` is `"top"`/`"bottom"`, `size` controls the added row height;
#'   for `"right"`/`"left"`, it controls the added column width. Use absolute units
#'   like `"mm"`/`"cm"` for fixed thickness, or `"null"` to use relative layout space.
#' - Because the return type is a **gtable**, you cannot add further ggplot layers
#'   with `+`. Apply all ggplot styling before calling this function.
#'
#' @keywords internal
#' @noRd
.add_plot_axis <- function(
    main_plot, sub_plot,
    position = c("top", "bottom", "right", "left"),
    size = grid::unit(0.25, "null")) {
  position <- match.arg(position)

  # main_plot: ggplot or gtable
  if (inherits(main_plot, "ggplot")) {
    main_gtable <- ggplot2::ggplotGrob(main_plot)
  } else if (inherits(main_plot, "gtable")) {
    main_gtable <- main_plot
  } else {
    stop("`main_plot` must be a ggplot or gtable.", call. = FALSE)
  }

  # sub_plot: ggplot/gtable/grob
  if (inherits(sub_plot, "ggplot")) {
    sub_gtable <- ggplot2::ggplotGrob(sub_plot)
  } else if (inherits(sub_plot, "gtable")) {
    sub_gtable <- sub_plot
  } else if (inherits(sub_plot, "grob")) {
    sub_gtable <- gtable::gtable(
      widths = grid::unit(1, "null"),
      heights = grid::unit(1, "null")
    )
    sub_gtable <- gtable::gtable_add_grob(
      sub_gtable, grobs = sub_plot,
      t = 1, l = 1
    )
  } else {
    stop("`sub_plot` must be a ggplot, gtable or grob.", call. = FALSE)
  }

  if (is.numeric(size)) size <- grid::unit(size, "cm")

  layout <- main_gtable$layout[main_gtable$layout$name == "panel", , drop = FALSE]
  tmin <- min(layout$t); bmax <- max(layout$b)
  lmin <- min(layout$l); rmax <- max(layout$r)

  switch(
    position,
    top = {
      main_gtable <- gtable::gtable_add_rows(
        main_gtable,
        heights = size, pos = tmin - 1
      )
      new_gtable <- gtable::gtable_add_grob(
        main_gtable, sub_gtable,
        t = tmin, l = lmin, b = tmin, r = rmax,
        name = "insert-top"
      )
    },
    bottom = {
      main_gtable <- gtable::gtable_add_rows(
        main_gtable,
        heights = size, pos = bmax
      )
      new_gtable <- gtable::gtable_add_grob(
        main_gtable, sub_gtable,
        t = bmax + 1, l = lmin, b = bmax + 1, r = rmax,
        name = "insert-bottom"
      )
    },
    right = {
      main_gtable <- gtable::gtable_add_cols(
        main_gtable,
        widths = size, pos = rmax
      )
      new_gtable <- gtable::gtable_add_grob(
        main_gtable, sub_gtable,
        t = tmin, l = rmax + 1, b = bmax, r = rmax + 1,
        name = "insert-right"
      )
    },
    left = {
      main_gtable <- gtable::gtable_add_cols(
        main_gtable,
        widths = size, pos = lmin - 1
      )
      new_gtable <- gtable::gtable_add_grob(
        main_gtable, sub_gtable,
        t = tmin, l = lmin, b = bmax, r = lmin,
        name = "insert-left"
      )
    }
  )

  new_gtable
}
