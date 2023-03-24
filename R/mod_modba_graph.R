#' modba_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_modba_graph_ui
#'
#' @importFrom shiny NS tagList column numericInput
#' @importFrom shiny fluidRow sliderInput
#' @importFrom shiny plotOutput verbatimTextOutput
mod_modba_graph_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10, offset = 1,
        shiny::plotOutput(ns("display_modba"),
          width = "100%")
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' modba_graph Server Functions
#'
#' @description Mod Bland-Altman graph server function
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_seg_graph_server
mod_modba_graph_server <- function(id, module_data){

  moduleServer( id, function(input, output, session){
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
      output$display_modba <- shiny::renderPlot({
        seg_modba <- segtools::seg_modba_graph(
          data = imported()
        )
        seg_modba
      }, res = 108,
        width = 1000,
        height = 600
        )
    }) |>
      # bind this to import
      shiny::bindEvent(imported())


  })
}

## To be copied in the UI
# mod_modba_graph_ui("modba_graph_1")

## To be copied in the server
# mod_modba_graph_server("modba_graph_1")
