#' @export
plot_cran_top <- function(when = c("last-month", "last-week", "last-day"),
                          count = 10, viewer_width = 1200, viewer_height = 500) {
  when <- match.arg(when)
  shiny::runGadget(
    shiny::fluidPage(
      plotOutput("cran_download_plot", width = "100%", height = "500px")
    ),
    function(input, output, session) {
      output$cran_download_plot <- renderPlot({
        cran_tops <- cranlogs::cran_top_downloads(when = when, count = count)
        cran_tops$rank <- as.factor(cran_tops$rank)
        ggbar(cran_tops, x = rank, y = count, fill = rank,
              label = package, label_size = 5, label_hjust = -.1) +
          scale_y_comma() +
          coord_flip() +
          scale_x_limit_reverse(cran_tops$rank) +
          labs(title = sprintf("Cran Top Downloads (%s)", when)) +
          theme_shiny(x.angle = 45)
      })
    }, viewer = dialogViewer(sprintf("Cran Top Downloads %s", when),
                             width = viewer_width, height = viewer_height))
}

plot_cran <- function(packages = c("ecos", "kisopenapi", "kosis"),
                      when = c("last-month", "last-week", "last-day"),
                      viewer_width = 1200, viewer_height = 500) {
  when <- match.arg(when)
  shiny::runGadget(
    shiny::fluidPage(
      plotOutput("cran_download_plot", width = "100%", height = "500px")
    ),
    function(input, output, session) {
      output$cran_download_plot <- renderPlot({
        cran_downloads <- cranlogs::cran_downloads(packages = packages, when = when)
        ggline(cran_downloads, x = date, y = count, color = package,
               label_size = 5, label_hjust = -.1) +
          scale_y_comma() +
          labs(title = sprintf("Cran Downloads (%s)", when)) +
          theme_shiny(x.angle = 45)
      })
    }, viewer = dialogViewer(sprintf("Cran Downloads %s", when),
                             width = viewer_width, height = viewer_height))
}
