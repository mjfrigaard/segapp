#' iso_range UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @export mod_pair_type_ui
#'
#' @importFrom shiny NS tagList
#' @importFrom shiny fluidRow column
mod_iso_range_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_iso_range")))),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::tags$br(),
        shiny::uiOutput(ns("text_iso_range"))
      )
    )
  )
}

#' iso_range Server Functions
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_pair_type_server
#'
#' @importFrom shiny NS moduleServer
#' @importFrom shiny reactive bindEvent validate
mod_iso_range_server <- function(id, module_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_iso_range <- reactable::renderReactable({
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        iso_ranges <- segtools::seg_iso_range_tbl(risk_vars_tbl)
        reactable::reactable(
          data = iso_ranges
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_iso_range <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p(
              shiny::tags$em(
                "ISO range = difference between BGM and REF as percent of REF for REF > 100 mg/dL and in mg/dL for REF <= 100 mg/dL."
              )
            )
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())
  })
}

## To be copied in the UI
# mod_iso_range_ui("iso_range_1")

## To be copied in the server
# mod_iso_range_server("iso_range_1")
