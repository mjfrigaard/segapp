#' compliant_pairs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_compliant_pairs_ui
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash box
#' @importFrom shiny fluidRow column
mod_compliant_pairs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 12,
        shiny::uiOutput(ns('text_compliant_pairs'))
        )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_compliant_pairs")),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns('text_source'))
      )
    )

  )
}

#' compliant_pairs Server Functions
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_compliant_pairs_server
#'
#' @importFrom shiny NS moduleServer
#' @importFrom shiny reactive bindEvent validate
mod_compliant_pairs_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
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

    shiny::observe({
      output$text_compliant_pairs <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p(
              "Models indicate that a device with ≥ 97% pairs inside the SEG no-risk 'green' zone would meet the requirements of ≤ 5% data pairs outside the 15 mg/dL (0.83 mmol/L) / 15% standard limits, while higher percentages outside the SEG no-risk zone would indicate noncompliance with the standard."
            )
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # use observe for the side-effect here
    shiny::observe({
      output$display_compliant_pairs <- reactable::renderReactable({
            risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
            binom_table <-  segtools::seg_binom_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = binom_table
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_source <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p("The Diabetes Technology Society Blood Glucose Monitor System (BGMS) Surveillance Program confirmed these ranges on 18 blood glucose monitoring systems using pre-determined analytical accuracy criteria agreed upon by the DTS-BGMS Surveillance Committee."),
        shiny::tags$a("Source: J Diabetes Sci Technol 8: 673-684, 2014. PMID: 25562887",
          href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4764239/")
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

  })
}

## To be copied in the UI
# mod_compliant_pairs_ui("compliant_pairs_1")

## To be copied in the server
# mod_compliant_pairs_server("compliant_pairs_1")
