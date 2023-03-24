#' mod_table UI Function
#'
#' @description MARD table UI function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_mard_table_ui
#'
#' @importFrom shiny NS tagList tags
#' @importFrom shiny fluidRow uiOutput column
#' @importFrom reactable reactable reactableOutput renderReactable
mod_mard_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::uiOutput(ns('text_mard'))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_mard"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' mod_mard_table Server Functions
#'
#' @description MARD table server function
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_mard_table_server
#'
#' @importFrom htmltools HTML
#' @importFrom shiny moduleServer reactive validate need
#' @importFrom shiny bindEvent observe renderText
mod_mard_table_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
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
      output$display_mard <- reactable::renderReactable({
        # data for MARD table
          shiny::validate(
            shiny::need(
              expr = module_data$imported_data(),
              message = "please import data"
            )
          )
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        mard <- segtools::seg_mard_tbl(risk_vars = risk_vars_tbl)

        reactable::reactable(
          data = mard
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

      shiny::observe({
       output$text_mard <- shiny::renderText({
          shiny::validate(
            shiny::need(
              expr = module_data$imported_data(),
              message = "please import data"
            )
          )
         htmltools::HTML(
           paste0(
             shiny::tags$p(
               shiny::tags$strong("Bias:"),
               shiny::tags$em("Mean relative difference between BGM and REF"),
               shiny::tags$code(shiny::tags$strong(" (BGM-REF ) / REF "))
             ),
             shiny::tags$p(
               shiny::tags$strong("MARD:"),
               shiny::tags$em("Mean Absolute Relative Difference"),
               shiny::tags$code(shiny::tags$strong(" | BGM-REF | / REF "))
             ),
             shiny::tags$p(
               shiny::tags$strong("CV:"),
               shiny::tags$em("Standard Deviation of Relative Difference between BGM and REF")
             ),
             shiny::tags$p(
               shiny::tags$strong("Lower 95% Limit of Agreement:"),
               shiny::tags$code(shiny::tags$strong("Bias - 1.96 * CV"))
             ),
             shiny::tags$p(
               shiny::tags$strong("Upper 95% Limit of Agreement:"),
               shiny::tags$code(shiny::tags$strong("Bias + 1.96 * CV"))
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
# mod_mard_table_ui("mod_mard_table_1")

## To be copied in the server
# mod_mard_table_server("mod_mard_table_1")
