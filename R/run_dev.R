#' Run dev version of app
#'
#' @return shiny app (dev)
#' @export run_dev
#'
run_dev <- function() {
  shiny::shinyApp(ui = dev_ui, server = dev_server)
}
