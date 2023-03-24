#' mod_pair_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_pair_type_ui
#'
#' @importFrom shiny NS tagList
#' @importFrom shiny fluidRow column
mod_pair_type_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        # BGM text ----
        shiny::tags$p(
          shiny::tags$strong(
            shiny::tags$code("BGM")
          ), " = Blood Glucose Monitor"
        ),
        # REF text ----
        shiny::tags$p(
          shiny::tags$strong(
            shiny::tags$code("REF")
          ), " = Reference"
        ),
        shiny::uiOutput(ns("text_pairs"))
      ),
      shiny::column(
        width = 6,
        reactable::reactableOutput(ns("display_pairs"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}



#' mod_pair_type Server Functions
#'
#' @param id module id
#' @param module_data data from import module
#'
#' @export mod_pair_type_server
#'
#' @importFrom shiny NS moduleServer
#' @importFrom shiny reactive bindEvent validate need
#' @importFrom segtools seg_pair_type_tbl
mod_pair_type_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "import a .csv file"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_pairs <- reactable::renderReactable({
        shiny::validate(
          shiny::need(imported(), "import a .csv file")
        )
        pairs_tbl <- segtools::seg_pair_type_tbl(imported())
        reactable::reactable(
          data = pairs_tbl
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_pairs <- shiny::renderText({
        # Table description text ----
        HTML(paste0(shiny::tags$p("This contains the number of ", shiny::tags$code("BGM"), "values that were 1) less than the ", shiny::tags$code("REF"), "values, 2) equal to the ", shiny::tags$code("REF"), "values, and 3) greater than the ", shiny::tags$code("REF"), "values. Note that ", shiny::tags$code("REF"), "values >600 mg/dL will not be plotted on the SEG.")))
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # dev purposes
    # output$values <- shiny::renderPrint({
    #   str(
    #     imported()
    #     )
    # })
  })
}

## To be copied in the UI
# mod_mod_pair_type_ui("mod_pair_type_1")

## To be copied in the server
# mod_mod_pair_type_server("mod_pair_type_1")
