#' Plotly treemap
#'
#' Draw a Plotly treemap using grouping data
#'
#' @param data a grouping data frame
#' @param group_var names of group variables
#' @param value_var a variable name to sum by group variables
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
#' \dontrun{set.seed(123)
#' x <- c("A", "A", "B", "B", "C", "C")
#' y <- c("X", "Y", "X", "Y", "X", "Y")
#' z <- c(600, 500, 400, 300, 200, 100)
#' data <- data.frame(x = x, y = y, z = z)
#' plotly_treemap(data, group_var = .(x, y), value_var = z)}
#'
#' @export
plotly_treemap <- function(data, group_var, value_var, path_labels = FALSE,
                           path_sep = " / ", add_parents = FALSE,
                           textinfo = "label+value+percent entry+percent parent+percent root",
                           hoverinfo = "percent entry+percent parent+percent root") {
  old_class <- class(data)
  jaid::set_dt(data)
  group_var <- match_cols(data, sapply(rlang::enexpr(group_var), rlang::as_name))
  value_var <- match_cols(data, sapply(rlang::enexpr(value_var), rlang::as_name))

  if (add_parents)
    data[, group_var] <- lapply(seq_along(group_var), function(x)
      paste(rep(group_var[x], nrow(data)), data[[group_var[x]]]))

  # total
  dt_tot <- data.table(parents = "", ids = "", labels = "",
                       data[, lapply(.SD, sum), .SDcols = value_var])
  # sum
  dt_sum_list <- lapply(seq_along(group_var), function(x) {
    label_var <- group_var[1:x]
    dt <- data[, lapply(.SD, sum), by = label_var, .SDcols = value_var]
    if (x > 1) {
      parent_var <- group_var[1:(x-1)]
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
               dt[, .SD, .SDcols = value_var])
  })
  dt_sum <- data.table::rbindlist(dt_sum_list)

  dt_all <- data.table::rbindlist(list(dt_tot, dt_sum), fill = TRUE)
  colors <- c("", rep(get_twelve_colors(), ceiling(nrow(dt_all)/12)))
  g <- plotly::plot_ly(
    type = "treemap",
    branchvalues = "total",
    ids = dt_all$id,
    labels  = dt_all$labels,
    parents = dt_all$parents,
    values  = dt_all[[value_var]],
    marker = list(colors = colors),
    # hoverinfo = "text",
    # text = ~paste("</br> Count: ", dts[[value_var[[1L]]]],
    #               "</br> Stay: ", dts[[value_var[[2L]]]]),
    textinfo = textinfo,
    hoverinfo = hoverinfo,
    domain = list(column = 0)
  )
  data.table::setattr(data, "class", old_class)
  attr(g, "tree_data") <- dt_all
  return(g)
}
