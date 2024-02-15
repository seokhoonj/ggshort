
.GGSHORT_COLORS_ENV <- NULL
.onLoad <- function(libname, pkgname) {
  .GGSHORT_COLORS_ENV <<- new.env()
  assign(".TWO_COLORS_1", c("#80B1D3", "#FB8072"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_2", c("#377EB8", "#E41A1C"), envir = .GGSHORT_COLORS_ENV)
}
