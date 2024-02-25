#' Plotly treemap
#'
#' Draw a Plotly treemap using grouping data.
#'
#' @param data a grouping data frame
#' @param groups names of group variables like (groups = .(GROUP_A, GROUP_B))
#' @param values a variable name to sum by group variables
#' @param path_labels a boolean value whether to use labels like path type
#' @param path_sep a string to separate the path labels. (default: " / ")
#' @param add_parents a boolean value whether the parents are prefixed to the labels
#' @param textinfo a string to express text information
#' (default: "label+value+percent entry+percent parent+percent root")
#' @param hoverinfo a string to express hover information
#' (default: "percent entry+percent parent+percent root")
#' @return a plotly object
#'
#' @examples
#' # plotly treemap
#' \donttest{set.seed(123)
#' x <- c("A", "A", "B", "B", "C", "C")
#' y <- c("X", "Y", "X", "Y", "X", "Y")
#' z <- c(600, 500, 400, 300, 200, 100)
#' data <- data.frame(x = x, y = y, z = z)
#' plotly_treemap(data, groups = .(x, y), values = z)}
#'
#' @export
plotly_treemap <- function(data, groups, values, path_labels = FALSE,
                           path_sep = " / ", add_parents = FALSE,
                           textinfo = "label+value+percent entry+percent parent+percent root",
                           hoverinfo = "percent entry+percent parent+percent root") {
  old_class <- class(data)
  jaid::set_dt(data)
  groups <- match_cols(data, sapply(rlang::enexpr(groups), rlang::as_name))
  values <- match_cols(data, sapply(rlang::enexpr(values), rlang::as_name))

  # add parents
  if (add_parents)
    data[, groups] <- lapply(seq_along(groups), function(x)
      paste(rep(groups[x], nrow(data)), data[[groups[x]]]))
  # total
  dt_tot <- data.table(parents = "", ids = "", labels = "",
                       data[, lapply(.SD, sum), .SDcols = values])
  # sum
  dt_sum_list <- lapply(seq_along(groups), function(x) {
    label_var <- groups[1:x]
    dt <- data[, lapply(.SD, sum), by = label_var, .SDcols = values]
    if (x > 1) {
      parent_var <- groups[1:(x-1)]
      parents <- jaid::paste_list(dt[, .SD, .SDcols = parent_var], sep = path_sep)
      ids     <- jaid::paste_list(dt[, .SD, .SDcols = label_var ], sep = path_sep)
      if (path_labels) {
        labels  <- ids
      } else {
        labels  <- dt[[label_var[x]]]
      }
    } else {
      parents <- ""
      ids     <- jaid::paste_list(dt[, .SD, .SDcols = label_var ], sep = path_sep)
      labels  <- jaid::paste_list(dt[, .SD, .SDcols = label_var ], sep = path_sep)
      if (path_labels) {
        labels  <- ids
      } else {
        labels  <- dt[[label_var[x]]]
      }
    }
    data.table(parents = parents, ids = ids, labels = labels,
               dt[, .SD, .SDcols = values])
  })
  dt_sum <- data.table::rbindlist(dt_sum_list)
  # all
  dt_all <- data.table::rbindlist(list(dt_tot, dt_sum), fill = TRUE)
  colors <- c("", rep(get_twelve_colors(), ceiling(nrow(dt_all)/12)))
  g <- plotly::plot_ly(
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
  data.table::setattr(data, "class", old_class)
  attr(g, "tree_data") <- dt_all
  return(g)
}

#' Plotly pie
#'
#' Draw a Plotly pie plot using grouping data.
#'
#' @param data a grouping data frame
#' @param labels a name of variable you want to group
#' @param values a name of variable specifying values
#' @param texttemplate a string specifying a text template
#' @param hovertemplate a string specifying a hover template
#'
#' @examples
#' # plotly pie
#' \donttest{set.seed(123)
#' data <- data.frame(labels = c("A", "B", "C"), values = c(300, 150, 50))
#' plotly_pie(data = data, labels = labels, values = values)}
#'
#' @export
plotly_pie <- function(
    data, labels, values,
    texttemplate = "%{label}<br>%{value}<br>(%{percent})",
    hovertemplate = "%{label}<br>%{value}<br>(%{percent})<extra></extra>"
    ) { # <extra></extra> is to remove trace label
  labels <- formula(paste0("~", rlang::as_name(rlang::enquo(labels))))
  values <- formula(paste0("~", rlang::as_name(rlang::enquo(values))))
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
