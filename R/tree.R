
# data2treemap <- function(df, group_var, value_var, fig = TRUE,
#                          add_names = FALSE, sep = " / ",
#                          textinfo = "label+value+percent entry+percent parent+percent root",
#                          hoverinfo = "percent entry+percent parent+percent root") {
#   group_cols <- vapply(substitute(group_var), deparse, "character")[-1L]
#   value_cols <- vapply(substitute(value_var), deparse, "character")[-1L]
#   if (add_names)
#     df[, group_cols] <- lapply(seq_along(group_cols), function(x)
#       paste(rep(group_cols[x], nrow(df)), df[[group_cols[x]]]))
#   prop0 <- data.table(parents = "", labels = "Total", df[, lapply(.SD, sum),
#                                                          .SDcols = value_cols])
#   props <- lapply(seq_along(group_cols), function(x) {
#     label_cols <- group_cols[1:x]
#     prop <- df[, lapply(.SD, sum), by = label_cols, .SDcols = value_cols]
#     if (x > 1) {
#       parent_cols <- group_cols[1:(x-1)]
#       parents <- repaste(prop[, ..parent_cols], sep = sep)
#       labels <- repaste(prop[, ..label_cols], sep = sep)
#     } else {
#       parents <- "Total"
#       labels <- repaste(prop[, ..label_cols], sep = sep)
#     }
#     data.table(parents = parents,
#                labels = labels,
#                prop[, ..value_cols])
#   })
#   props <- rbind(prop0, do.call("rbind", props))
#   if (fig) {
#     g <- plot_ly(
#       type = "treemap",
#       branchvalues = "total",
#       labels  = props$labels,
#       parents = props$parents,
#       values  = props[[value_cols]],
#       marker = list(colors = c("", rep(brewer.pal(12, "Set3"),
#                                        ceiling(nrow(props)/12)))),
#       # hoverinfo = "text",
#       # text = ~paste("</br> Count: ", props[[value_cols[[1L]]]],
#       #               "</br> Stay: ", props[[value_cols[[2L]]]]),
#       textinfo = textinfo,
#       hoverinfo = hoverinfo,
#       domain = list(column = 0)
#     )
#     attr(props, "fig") <- g
#     print(g)
#   }
#   return(g)
# }

