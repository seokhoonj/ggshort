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
#' @param family A character string giving the font family to use.
#'   - Use `""` (empty string) to reset to system default.
#'   - Must match a font available in [systemfonts::system_fonts()].
#'
#' @return
#' - `set_ggshort_font()`: the font family (character scalar) that was set.
#' - `get_ggshort_font()`: the current font family (character scalar).
#'
#' @examples
#' \dontrun{
#' # Set Comic Sans MS if available
#' set_ggshort_font("Comic Sans MS")
#'
#' # Reset to system default
#' set_ggshort_font("")
#'
#' # Get current font
#' get_ggshort_font()
#' }
#'
#' @export
set_ggshort_font <- function(family) {
  pf <- getOption("ggshort.font", default = "")

  if (identical(family, "")) {
    options(ggshort.font = "")
    return("")
  }

  # system fonts
  sf <- tryCatch(
    systemfonts::system_fonts(),
    error = function(e) NULL
  )
  has_font <- !is.null(sf) && any(tolower(sf$family) == tolower(family))
  if (!has_font) {
    options(ggshort.font = pf)
    stop(
      sprintf("Font '%s' not found. Reverting to previous option '%s'.",
              family, pf),
      call. = FALSE
    )
  }

  if (requireNamespace("sysfonts", quietly = TRUE)) {
    row <- sf[match(tolower(family), tolower(sf$family)), , drop = FALSE]
    path <- row$path[1L]
    if (is.character(path) && nzchar(path) && file.exists(path)) {
      try(sysfonts::font_add(family, path), silent = TRUE)
    }
  }

  options(ggshort.font = family)

  os_type <- Sys.info()[["sysname"]]
  if (os_type == "Windows")
    showtext::showtext_auto(.mean_dpi())

  family
}

#' @rdname set_ggshort_font
#' @export
get_ggshort_font <- function()
  getOption("ggshort.font", default = "")

# Internal helper function ------------------------------------------------

.mean_dpi <- function() {
  dpi_x <- grDevices::dev.size("px")[1L] / grDevices::dev.size("in")[1L]
  dpi_y <- grDevices::dev.size("px")[2L] / grDevices::dev.size("in")[2L]
  mean(c(dpi_x, dpi_y))
}
