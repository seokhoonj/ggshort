#' @description
#' This package simplify the grammar of ggplot to the functions only with
#' frequently used arguments. Despite its limited flexibility, it helps people
#' draw ggplot easily.
#' @importFrom ggplot2 aes annotation_custom coord_flip element_blank facet_wrap
#'   geom_bar geom_blank geom_boxplot geom_density geom_hline geom_jitter
#'   geom_line geom_point geom_ribbon geom_segment geom_text geom_tile
#'   geom_vline ggplot ggplot_build ggplot_gtable ggplotGrob labs layer margin
#'   position_dodge2 position_fill position_identity position_jitter Stat
#'   scale_color_brewer scale_color_manual scale_fill_brewer scale_fill_manual
#'   scale_fill_gradient2 scale_x_continuous scale_x_discrete scale_x_log10
#'   scale_x_reverse scale_y_continuous scale_y_discrete scale_y_reverse
#'   stat_summary theme theme_classic theme_void xlab ylab
#' @importFrom grid arrow grid.draw gpar textGrob unit
#' @importFrom gtable gtable gtable_add_grob gtable_add_rows
#' @importFrom data.table `:=` `.SD` as.data.table data.table melt set setDT
#' @importFrom jaid capture_names
#' @importFrom lifecycle badge deprecate_warn signal_stage
#' @importFrom plotly ggplotly plot_ly
#' @importFrom rlang .data enquo enquos quo_is_missing quo_is_null
#' @importFrom scales comma
#' @importFrom showtext showtext_auto
#' @importFrom stats density formula median prcomp quantile
#' @importFrom systemfonts system_fonts
#' @importFrom utils globalVariables head modifyList tail
#' @keywords internal
"_PACKAGE"
