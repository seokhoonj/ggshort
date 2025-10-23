
.GGSHORT_ENV <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign(".TWO_COLORS_BASE", c("#80B1D3", "#FB8072"), envir = .GGSHORT_ENV)
  assign(".TWO_COLORS_DEEP", c("#377EB8", "#E41A1C"), envir = .GGSHORT_ENV)
  assign(".TWO_COLORS_BASE_INV", c("#FB8072", "#80B1D3"), envir = .GGSHORT_ENV)
  assign(".TWO_COLORS_DEEP_INV", c("#E41A1C", "#377EB8"), envir = .GGSHORT_ENV)
  assign(".TWO_COLORS_BASE_REV", c("#FB8072", "#80B1D3"), envir = .GGSHORT_ENV)
  assign(".TWO_COLORS_DEEP_REV", c("#E41A1C", "#377EB8"), envir = .GGSHORT_ENV)
  assign(".EIGHT_COLORS",
         c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
           "#E5C494", "#B3B3B3"),
         envir = .GGSHORT_ENV)
  assign(".NINE_COLORS",
         c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
           "#A65628", "#F781BF", "#999999"),
         envir = .GGSHORT_ENV)
  assign(".TWELVE_COLORS",
         c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
           "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"),
         envir = .GGSHORT_ENV)

  op <- options()
  op.ggshort <- list(
    ggshort.font = ""
  )
  toset <- setdiff(names(op.ggshort), names(op))
  if (length(toset)) options(op.ggshort[toset])

  invisible()
}
