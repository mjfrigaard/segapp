#' risk_level UI Function
#'
#' @description risk level shiny UI module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_risk_category_ui
#'
#' @importFrom shiny NS tagList column
#' @importFrom shiny fluidRow verbatimTextOutput
mod_risk_category_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_level"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' risk_level Server Functions
#'
#' @description Risk level table server function
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_risk_category_server
mod_risk_category_server <- function(id, module_data) {
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
      output$display_level <- reactable::renderReactable({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        risk_cat_tbl <- segtools::seg_risk_cat_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = risk_cat_tbl,
          compact = TRUE,
          rowStyle = function(index) {
            if (risk_cat_tbl[index, "SEG Risk Category"] == "None") {
              list(
                background = "#00EE00",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Slight, Lower") {
              list(
                background = "#ADFF2F",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Slight, Higher") {
              list(
                background = "#FFFF00",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Moderate, Lower") {
              list(
                background = "#FFD700",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Moderate, Higher") {
              list(
                background = "#FFA500",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Severe, Lower") {
              list(
                background = "#EE7600",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Severe, Upper") {
              list(
                background = "#FF4500",
                fontWeight = "bold"
              )
            } else {
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
# mod_risk_category_ui("risk_level_1")

## To be copied in the server
# mod_risk_category_server("risk_level_1")
