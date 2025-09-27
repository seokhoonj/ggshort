#' Set or get the default font for ggshort plots
#'
#' These helpers manage the global font family used by `ggshort` themes
#' and label helpers, stored in the option `"ggshort.font"`.
#'
#' - `set_ggshort_font()` updates the option, verifying that the font exists
#'   in [systemfonts::system_fonts()]. If the font is not found, the previous
#'   option value is restored and a warning is issued.
#' - `get_ggshort_font()` retrieves the currently set option.
#'
#' @param family Character or NULL.
#'   - Use `NULL` or `""` to reset to system default (stored as `""`).
#'   - Otherwise must match a family in [systemfonts::system_fonts()].
#'
#' @return
#' - `set_ggshort_font()`: the font family string that was set (or `""` if reset).
#' - `get_ggshort_font()`: the current font family string (default `""`).
#'
#' @examples
#' \dontrun{
#' # Set Comic Sans MS if available
#' set_ggshort_font("Comic Sans MS")
#'
#' # Reset to system default
#' set_ggshort_font("")
#' set_ggshort_font(NULL)
#'
#' # Get current font
#' get_ggshort_font()
#' }
#'
#' @export
set_ggshort_font <- function(family) {
  prev <- getOption("ggshort.font", default = "")

  # Treat both NULL and "" as reset -> store "" as the canonical default
  if (is.null(family) || identical(family, "")) {
    options(ggshort.font = "")
    return("")
  }

  # Validate availability
  sf <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
  has_font <- !is.null(sf) && any(tolower(sf$family) == tolower(family))
  if (!has_font) {
    options(ggshort.font = prev)
    stop(
      sprintf("Font '%s' not found. Reverting to previous option '%s'.",
              family, prev),
      call. = FALSE
    )
  }

  # Best-effort register with sysfonts (if available)
  if (requireNamespace("sysfonts", quietly = TRUE)) {
    row  <- sf[match(tolower(family), tolower(sf$family)), , drop = FALSE]
    path <- row$path[1L]
    if (is.character(path) && nzchar(path) && file.exists(path)) {
      try(sysfonts::font_add(family, path), silent = TRUE)
    }
  }

  options(ggshort.font = family)

  # Optional: enable showtext on Windows if available
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext::showtext_auto(.mean_dpi())
    } else {
      message("showtext package not found -> skipping font auto-activation.")
    }
  }

  family
}

#' @rdname set_ggshort_font
#' @export
get_ggshort_font <- function() {
  getOption("ggshort.font", default = "")
}

# Internal helper function ------------------------------------------------

.mean_dpi <- function() {
  dpi_x <- grDevices::dev.size("px")[1L] / grDevices::dev.size("in")[1L]
  dpi_y <- grDevices::dev.size("px")[2L] / grDevices::dev.size("in")[2L]
  mean(c(dpi_x, dpi_y))
}
