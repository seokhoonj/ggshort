#' @description
#' This package simplify the grammar of ggplot to the functions only with
#' frequently used arguments. Despite its limited flexibility, it helps people
#' draw ggplot easily.
#' @keywords internal
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom data.table `:=` `.SD` as.data.table data.table set setDT
#' @importFrom jaid paste_list set_dt unilen
#' @importFrom plotly ggplotly plot_ly
#' @importFrom rlang enquo enquos quo_is_null
#' @importFrom scales comma
#' @importFrom stats density formula quantile
#' @importFrom utils globalVariables head tail
"_PACKAGE"
