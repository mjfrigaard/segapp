#' risk_grade UI Function
#'
#' @description Risk grade shiny UI module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_risk_grade_ui
#'
#' @importFrom shiny NS tagList column
#' @importFrom shiny fluidRow verbatimTextOutput
mod_risk_grade_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_grade"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' risk_grade Server Functions
#'
#' @description Risk grade table server function
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_risk_grade_server
mod_risk_grade_server <- function(id, module_data) {
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
      output$display_grade <- reactable::renderReactable({
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        risk_grade_tbl <- segtools::seg_risk_grade_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = risk_grade_tbl,
          compact = TRUE,
          rowStyle = function(index) {
            if (risk_grade_tbl[index, "Risk Grade"] == "A") {
              list(
                background = "#33CC33",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "B") {
              list(
                background = "#99FF33",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "C") {
              list(
                background = "#FFFF00",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "D") {
              list(
                background = "#FF9900",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "E") {
              list(
                background = "#FF0000",
                fontWeight = "bold"
              )
            }
          }
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())
  })
}

## To be copied in the UI
# mod_risk_grade_ui("risk_grade_1")

## To be copied in the server
# mod_risk_grade_server("risk_grade_1")
