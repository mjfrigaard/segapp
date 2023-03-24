#' seg_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export  mod_seg_graph_ui
#'
#' @importFrom shiny NS tagList column numericInput
#' @importFrom shiny fluidRow sliderInput
#' @importFrom shiny plotOutput verbatimTextOutput
mod_seg_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::sliderInput(
          width = "220px",
          ns("alpha"),
          label = "Point opacity",
          min = 0.1,
          max = 1,
          step = 0.1,
          value = 0.60
        ),
        shiny::numericInput(
          width = "220px",
          ns("size"),
          label = "Point Size",
          value = 2.4,
          step = 0.1,
          min = 1,
          max = 5
        ),
        shiny::selectInput(
          width = "220px",
          inputId = ns("color"),
          choices = c("#000000", "#222222", "#2F2F2F", "#444444", "#565656"),
          selected = "#000000",
          label = "Point outline color"
        ),
        shiny::selectInput(
          width = "220px",
          inputId = ns("fill"),
          choices = c("#FFFFFF", "#EEEEEE", "#F5F5F5", "#E7F5F7", "#F0F8FF", "#E5E5E5"),
          selected = "#FFFFFF",
          label = "Point fill color"
        )
      ),
    # shiny::fluidRow(
    #   shiny::column(
    #     width = 12,
    # textInput(
    #   inputId = "plot_title",
    #   label = "Plot title",
    #   placeholder = "Enter text to be used as plot title"
    # )))
      shiny::column(
        width = 8, offset = 1,
        shiny::plotOutput(ns("display_grid"), width = "100%")
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' seg_graph Server Functions
#'
#' @description SEG grid server function
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_seg_graph_server
mod_seg_graph_server <- function(id, module_data) {
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
      output$display_grid <- shiny::renderPlot({
        seg_grid <- segtools::seg_graph(
          data = imported(),
          alpha_var = input$alpha,
          size_var = input$size,
          color_var = input$color,
          fill_var = input$fill
        )
        seg_grid
      }, res = 108,
        width = 900,
        height = 450
        )
      }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # reactives (dev only)
    # output$values <- shiny::renderPrint({
    #   input$color
      #   shiny::reactiveValuesToList(
      #     x = input,
      #     all.names = TRUE
      #   )
      # remove reactable ids
      # react_ids <- stringr::str_detect(names(vals()),
      #   "__reactable__",
      #   negate = TRUE
      # )
      # vals()[react_ids]
    # })

  })
}

## To be copied in the UI
# mod_seg_graph_ui("seg_graph_1")

## To be copied in the server
# mod_seg_graph_server("seg_graph_1")
