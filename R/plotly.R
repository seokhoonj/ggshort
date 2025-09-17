#' Plotly treemap
#'
#' Build a Plotly treemap from grouped data (column names as character).
#'
#' @param data A data frame.
#' @param groups Character vector of grouping columns in `data`
#'   (e.g., `c("grp1","grp2")`). Order defines the hierarchy from root to leaves.
#' @param values Single character string: numeric column in `data` to sum for sizes.
#' @param path_labels Logical; if `TRUE`, use full hierarchical path for labels.
#'   Default: `FALSE`.
#' @param path_sep Separator for path labels. Default: `" / "`.
#' @param add_parents Logical; if `TRUE`, prefix each group value with its
#'   column name (parent label hint). Default: `FALSE`.
#' @param textinfo Plotly `textinfo` string. Default:
#'   `"label+value+percent entry+percent parent+percent root"`.
#' @param hoverinfo Plotly `hoverinfo` string. Default:
#'   `"percent entry+percent parent+percent root"`.
#'
#' @return A plotly object. The aggregated hierarchy is also attached as
#'   `attr(x, "tree_data")` (data.frame).
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' x <- c("A", "A", "B", "B", "C", "C")
#' y <- c("X", "Y", "X", "Y", "X", "Y")
#' z <- c(600, 500, 400, 300, 200, 100)
#' df <- data.frame(x = x, y = y, z = z)
#' plotly_treemap(df, groups = c("x","y"), values = "z")
#' }
#'
#' @export
plotly_treemap <- function(data, groups, values, path_labels = FALSE,
                           path_sep = " / ", add_parents = FALSE,
                           textinfo = "label+value+percent entry+percent parent+percent root",
                           hoverinfo = "percent entry+percent parent+percent root") {
  if (!inherits(data, "data.frame"))
    stop("`data` must be a data.frame.", call. = FALSE)

  env <- jaid::ensure_dt_env(data)
  dt  <- env$dt

  groups <- jaid::capture_names(dt, !!rlang::enquo(groups))
  values <- jaid::capture_names(dt, !!rlang::enquo(values))

  # add parents
  if (add_parents)
    dt[, groups] <- lapply(seq_along(groups), function(x)
      paste(rep(groups[x], nrow(dt)), dt[[groups[x]]]))

  # total
  dt_tot <- data.table::data.table(
    parents = "", ids = "", labels = "",
    dt[, lapply(.SD, sum), .SDcols = values]
  )

  # sum
  dt_sum_list <- lapply(seq_along(groups), function(x) {
    label_var <- groups[1:x]
    ds <- dt[, lapply(.SD, sum), by = label_var, .SDcols = values]
    if (x > 1) {
      parent_var <- groups[1:(x-1)]
      parents <- .paste_list(ds[, .SD, .SDcols = parent_var], sep = path_sep)
      ids     <- .paste_list(ds[, .SD, .SDcols = label_var ], sep = path_sep)
      if (path_labels) {
        labels  <- ids
      } else {
        labels  <- ds[[label_var[x]]]
      }
    } else {
      parents <- ""
      ids     <- .paste_list(ds[, .SD, .SDcols = label_var ], sep = path_sep)
      labels  <- .paste_list(ds[, .SD, .SDcols = label_var ], sep = path_sep)
      if (path_labels) {
        labels  <- ids
      } else {
        labels  <- ds[[label_var[x]]]
      }
    }
    data.table(parents = parents, ids = ids, labels = labels,
               ds[, .SD, .SDcols = values])
  })
  dt_sum <- data.table::rbindlist(dt_sum_list)

  # all
  dt_all <- data.table::rbindlist(list(dt_tot, dt_sum), fill = TRUE)
  colors <- c("", rep(get_twelve_colors(), ceiling(nrow(dt_all)/12)))

  p <- plotly::plot_ly(
    type = "treemap",
    branchvalues = "total",
    ids = dt_all$id,
    labels  = dt_all$labels,
    parents = dt_all$parents,
    values  = dt_all[[values]],
    marker = list(colors = colors),
    textinfo = textinfo,
    hoverinfo = hoverinfo,
    domain = list(column = 0)
  )
  attr(p, "tree_data") <- env$restore(dt_all)

  p
}

#' Plotly pie
#'
#' Build a Plotly pie chart (column names as character).
#'
#' @param data A data frame.
#' @param labels Character string: column in `data` used for slice labels.
#' @param values Character string: numeric column in `data` used for slice sizes.
#' @param texttemplate Plotly `texttemplate`. Default:
#'   `"%{label}<br>%{value}<br>(%{percent})"`.
#' @param hovertemplate Plotly `hovertemplate`. Default:
#'   `"%{label}<br>%{value}<br>(%{percent})<extra></extra>"`.
#'
#' @return A plotly object.
#'
#' @examples
#' \donttest{
#' df <- data.frame(labels = c("A", "B", "C"), values = c(300, 150, 50))
#' plotly_pie(df, labels = "labels", values = "values")
#' }
#'
#' @export
plotly_pie <- function(
    data, labels, values,
    texttemplate = "%{label}<br>%{value}<br>(%{percent})",
    hovertemplate = "%{label}<br>%{value}<br>(%{percent})<extra></extra>"
    ) { # <extra></extra> is to remove trace label
  if (!inherits(data, "data.frame"))
    stop("`data` must be a data.frame.", call. = FALSE)

  labels <- jaid::capture_names(data, !!rlang::enquo(labels))
  values <- jaid::capture_names(data, !!rlang::enquo(values))
  labels <- formula(paste0("~", labels))
  values <- formula(paste0("~", values))

  plotly::plot_ly(
    data = data,
    type = "pie",
    labels = labels,
    values = values,
    texttemplate = texttemplate,
    hovertemplate = hovertemplate
  ) |>
    plotly::layout(uniformtext = list(minsize = 12, mode = "hide"))
}

# Internal helper functions -----------------------------------------------

#' Paste vectors of a list
#'
#' Paste vectors of equal length in a list or data.frame
#'
#' @param x A list with same length vectors or data frame column vectors you want to paste.
#' @param sep A character string to separate the terms.
#' @param na.rm A logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#' @return a vector pasted
#'
#' @examples
#' \donttest{
#' iris$size <- paste_list(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
#' head(iris)
#' }
#'
#' @keywords internal
#' @noRd
.paste_list <- function(x, sep = "|", na.rm = FALSE) {
  if (na.rm)
    x[] <- lapply(x, function(s) ifelse(is.na(s), "", s))
  pattern <- sprintf("^\\%s|\\%s\\%s|\\%s$", sep, sep, sep,
                     sep)
  n <- length(x)
  if (n == 1L)
    return(x[[1L]])
  x <- do.call(function(...) paste(..., sep = sep), x)
  x <- gsub(pattern, "", x)
  if (na.rm)
    x <- ifelse(x == "", NA, x)
  return(x)
}
