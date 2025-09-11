
.GGSHORT_COLORS_ENV <- NULL
.onLoad <- function(libname, pkgname) {
  .GGSHORT_COLORS_ENV <<- new.env()
  assign(".TWO_COLORS_BASE", c("#80B1D3", "#FB8072"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_DEEP", c("#377EB8", "#E41A1C"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_BASE_INV", c("#FB8072", "#80B1D3"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_DEEP_INV", c("#E41A1C", "#377EB8"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_BASE_REV", c("#FB8072", "#80B1D3"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWO_COLORS_DEEP_REV", c("#E41A1C", "#377EB8"), envir = .GGSHORT_COLORS_ENV)
  assign(".TWELVE_COLORS",
         c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
           "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"),
         envir = .GGSHORT_COLORS_ENV)
  op <- options()
  op.ggshort <- list(
    ggshort.font = ""
  )
  toset <- !(names(op.ggshort) %in% names(op))
  if (any(toset)) options(op.ggshort[toset])
  invisible()
}
